{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------
--
-- Makefile Dependency Generation
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module GHC.Driver.MakeFile
   ( doMkDependHS
   )
where

import GHC.Prelude

import qualified GHC
import GHC.Data.Maybe
import GHC.Driver.Monad
import GHC.Driver.DynFlags
import GHC.Driver.Ppr
import GHC.Driver.MakeFile.JSON
import GHC.Utils.Misc
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Pipeline (runPipeline, TPhase (T_Unlit, T_FileArgs), use, mkPipeEnv)
import GHC.Driver.Phases (StopPhase (StopPreprocess), startPhase, Phase (Unlit))
import GHC.Driver.Pipeline.Monad (PipelineOutput (NoOutputFile))
import GHC.Driver.Session (pgm_F)
import qualified GHC.SysTools as SysTools
import GHC.Data.Graph.Directed ( SCC(..) )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.SourceError
import GHC.Types.SrcLoc
import GHC.Types.PkgQual
import Data.List (partition)
import GHC.Utils.TmpFs

import GHC.Iface.Load (cannotFindModule)
import GHC.Iface.Errors.Types

import GHC.Unit.Module
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Graph
import GHC.Unit.Finder
import GHC.Unit.State (lookupUnitId)

import GHC.Utils.Exception
import GHC.Utils.Error
import GHC.Utils.Logger

import System.Directory
import System.FilePath
import System.IO
import System.IO.Error  ( isEOFError )
import Control.Monad    ( when )
import Data.Foldable (traverse_)
import Data.IORef
import qualified Data.Set as Set

-----------------------------------------------------------------
--
--              The main function
--
-----------------------------------------------------------------

doMkDependHS :: GhcMonad m => [FilePath] -> m ()
doMkDependHS srcs = do
    logger <- getLogger

    -- Initialisation
    dflags0 <- GHC.getSessionDynFlags

    -- We kludge things a bit for dependency generation. Rather than
    -- generating dependencies for each way separately, we generate
    -- them once and then duplicate them for each way's osuf/hisuf.
    -- We therefore do the initial dependency generation with an empty
    -- way and .o/.hi extensions, regardless of any flags that might
    -- be specified.
    let dflags1 = dflags0
            { targetWays_ = Set.empty
            , hiSuf_      = "hi"
            , objectSuf_  = "o"
            }
    GHC.setSessionDynFlags dflags1

    -- If no suffix is provided, use the default -- the empty one
    let dflags = if null (depSuffixes dflags1)
                 then dflags1 { depSuffixes = [""] }
                 else dflags1

    tmpfs <- hsc_tmpfs <$> getSession
    files <- liftIO $ beginMkDependHS logger tmpfs dflags

    -- Do the downsweep to find all the modules
    targets <- mapM (\s -> GHC.guessTarget s Nothing Nothing) srcs
    GHC.setTargets targets
    let excl_mods = depExcludeMods dflags
    module_graph <- GHC.depanal excl_mods True {- Allow dup roots -}
    -- Sort into dependency order
    -- There should be no cycles
    let sorted = GHC.topSortModuleGraph False module_graph Nothing

    -- Print out the dependencies if wanted
    liftIO $ debugTraceMsg logger 2 (text "Module dependencies" $$ ppr sorted)

    -- Process them one by one, dumping results into makefile
    -- and complaining about cycles
    hsc_env <- getSession
    root <- liftIO getCurrentDirectory
    let excl_mods = depExcludeMods dflags
    mapM_ (liftIO . processDeps dflags hsc_env excl_mods root (mkd_tmp_hdl files) (mkd_dep_json files)) sorted

    -- If -ddump-mod-cycles, show cycles in the module graph
    liftIO $ dumpModCycles logger module_graph

    -- Tidy up
    liftIO $ endMkDependHS logger files

-----------------------------------------------------------------
--
--              beginMkDependHs
--      Create a temporary file,
--      find the Makefile,
--      slurp through it, etc
--
-----------------------------------------------------------------

data MkDepFiles
  = MkDep { mkd_make_file :: FilePath,          -- Name of the makefile
            mkd_make_hdl  :: Maybe Handle,      -- Handle for the open makefile
             -- | Output interface for the -dep-json file
            mkd_dep_json  :: !(Maybe (JsonOutput DepJSON)),
            mkd_tmp_file  :: FilePath,          -- Name of the temporary file
            mkd_tmp_hdl   :: Handle }           -- Handle of the open temporary file

beginMkDependHS :: Logger -> TmpFs -> DynFlags -> IO MkDepFiles
beginMkDependHS logger tmpfs dflags = do
        -- open a new temp file in which to stuff the dependency info
        -- as we go along.
  tmp_file <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "dep"
  tmp_hdl <- openFile tmp_file WriteMode

        -- open the makefile
  let makefile = depMakefile dflags
  exists <- doesFileExist makefile
  mb_make_hdl <-
        if not exists
        then return Nothing
        else do
           makefile_hdl <- openFile makefile ReadMode

                -- slurp through until we get the magic start string,
                -- copying the contents into dep_makefile
           let slurp = do
                l <- hGetLine makefile_hdl
                if (l == depStartMarker)
                        then return ()
                        else do hPutStrLn tmp_hdl l; slurp

                -- slurp through until we get the magic end marker,
                -- throwing away the contents
           let chuck = do
                l <- hGetLine makefile_hdl
                if (l == depEndMarker)
                        then return ()
                        else chuck

           catchIO slurp
                (\e -> if isEOFError e then return () else ioError e)
           catchIO chuck
                (\e -> if isEOFError e then return () else ioError e)

           return (Just makefile_hdl)

  dep_json_ref <- mkJsonOutput initDepJSON (depJSON dflags)

        -- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depStartMarker

  return (MkDep { mkd_make_file = makefile, mkd_make_hdl = mb_make_hdl,
                  mkd_dep_json = dep_json_ref,
                  mkd_tmp_file  = tmp_file, mkd_tmp_hdl  = tmp_hdl})

-----------------------------------------------------------------
--
--              processDeps
--
-----------------------------------------------------------------

processDeps :: DynFlags
            -> HscEnv
            -> [ModuleName]
            -> FilePath
            -> Handle           -- Write dependencies to here
            -> Maybe (JsonOutput DepJSON)
            -> SCC ModuleGraphNode
            -> IO ()
-- Write suitable dependencies to handle
-- Always:
--                      this.o : this.hs
--
-- If the dependency is on something other than a .hi file:
--                      this.o this.p_o ... : dep
-- otherwise
--                      this.o ...   : dep.hi
--                      this.p_o ... : dep.p_hi
--                      ...
-- (where .o is $osuf, and the other suffixes come from
-- the cmdline -s options).
--
-- For {-# SOURCE #-} imports the "hi" will be "hi-boot".

processDeps dflags _ _ _ _ _ (CyclicSCC nodes)
  =     -- There shouldn't be any cycles; report them
    throwGhcExceptionIO $ ProgramError $
      showSDoc dflags $ GHC.cyclicModuleErr nodes

processDeps dflags _ _ _ _ _ (AcyclicSCC (InstantiationNode _uid node))
  =     -- There shouldn't be any backpack instantiations; report them as well
    throwGhcExceptionIO $ ProgramError $
      showSDoc dflags $
        vcat [ text "Unexpected backpack instantiation in dependency graph while constructing Makefile:"
             , nest 2 $ ppr node ]
processDeps _dflags_ _ _ _ _ _ (AcyclicSCC (LinkNode {})) = return ()

processDeps dflags hsc_env excl_mods root hdl m_dep_json (AcyclicSCC (ModuleNode _ node)) = do
  pp <- preprocessor
  deps <- fmap concat $ sequence $
    [cpp_deps | depIncludeCppDeps dflags] ++ [
      import_deps IsBoot (ms_srcimps node),
      import_deps NotBoot (ms_imps node)
    ]
  updateJson m_dep_json (updateDepJSON include_pkg_deps pp dep_node deps)
  writeDependencies include_pkg_deps root hdl extra_suffixes dep_node deps
  where
    extra_suffixes = depSuffixes dflags
    include_pkg_deps = depIncludePkgDeps dflags
    src_file = msHsFilePath node
    dep_node =
      DepNode {
        dn_mod = ms_mod node,
        dn_src = src_file,
        dn_obj = msObjFilePath node,
        dn_hi = msHiFilePath node,
        dn_boot = isBootSummary node,
        dn_options = Set.fromList (ms_opts node)
      }

    preprocessor
      | Just src <- ml_hs_file (ms_location node)
      = runPipeline (hsc_hooks hsc_env) $ do
        let (_, suffix) = splitExtension src
            lit | Unlit _ <- startPhase suffix = True
                | otherwise = False
            pipe_env = mkPipeEnv StopPreprocess src Nothing NoOutputFile
        unlit_fn <- if lit then use (T_Unlit pipe_env hsc_env src) else pure src
        (dflags1, _, _) <- use (T_FileArgs hsc_env unlit_fn)
        let pp = pgm_F dflags1
        pure (if null pp then global_preprocessor else Just pp)
      | otherwise
      = pure global_preprocessor

    global_preprocessor
      | let pp = pgm_F dflags
      , not (null pp)
      = Just pp
      | otherwise
      = Nothing

    -- Emit a dependency for each CPP import
    -- CPP deps are discovered in the module parsing phase by parsing
    -- comment lines left by the preprocessor.
    -- Note that GHC.parseModule may throw an exception if the module
    -- fails to parse, which may not be desirable (see #16616).
    cpp_deps = do
      session <- Session <$> newIORef hsc_env
      parsedMod <- reflectGhc (GHC.parseModule node) session
      pure (DepCpp <$> GHC.pm_extra_src_files parsedMod)

    -- Emit a dependency for each import
    import_deps is_boot idecls =
      sequence [
        findDependency hsc_env loc mb_pkg mod is_boot
        | (mb_pkg, L loc mod) <- idecls
        , mod `notElem` excl_mods
        ]


findDependency  :: HscEnv
                -> SrcSpan
                -> PkgQual              -- package qualifier, if any
                -> ModuleName           -- Imported module
                -> IsBootInterface      -- Source import
                -> IO Dep
findDependency hsc_env srcloc pkg imp dep_boot = do
  -- Find the module; this will be fast because
  -- we've done it once during downsweep
  findImportedModule hsc_env imp pkg >>= \case
    Found loc dep_mod ->
      pure DepHi {
        dep_mod,
        dep_path = ml_hi_file loc,
        dep_unit = lookupUnitId (hsc_units hsc_env) (moduleUnitId dep_mod),
        dep_local,
        dep_boot
      }
      where
        dep_local = isJust (ml_hs_file loc)

    fail ->
      throwOneError $
      mkPlainErrorMsgEnvelope srcloc $
      GhcDriverMessage $
      DriverInterfaceError $
      Can'tFindInterface (cannotFindModule hsc_env imp fail) $
      LookingForModule imp dep_boot

writeDependencies ::
  Bool ->
  FilePath ->
  Handle ->
  [FilePath] ->
  DepNode ->
  [Dep] ->
  IO ()
writeDependencies include_pkgs root hdl suffixes node deps =
  traverse_ write tasks
  where
    tasks = source_dep : boot_dep ++ concatMap import_dep deps

    -- Emit std dependency of the object(s) on the source file
    -- Something like       A.o : A.hs
    source_dep = (obj_files, dn_src)

    -- add dependency between objects and their corresponding .hi-boot
    -- files if the module has a corresponding .hs-boot file (#14482)
    boot_dep
      | IsBoot <- dn_boot
      = [([obj], hi) | (obj, hi) <- zip (suffixed (removeBootSuffix dn_obj)) (suffixed dn_hi)]
      | otherwise
      = []

    -- Add one dependency for each suffix;
    -- e.g.         A.o   : B.hi
    --              A.x_o : B.x_hi
    import_dep = \case
      DepHi {dep_path, dep_boot, dep_unit}
        | isNothing dep_unit || include_pkgs
        , let path = addBootSuffix_maybe dep_boot dep_path
        -> [([obj], hi) | (obj, hi) <- zip obj_files (suffixed path)]

        | otherwise
        -> []

      DepCpp {dep_path} -> [(obj_files, dep_path)]

    write (from, to) = writeDependency root hdl from to

    obj_files = suffixed dn_obj

    suffixed f = insertSuffixes f suffixes

    DepNode {dn_src, dn_obj, dn_hi, dn_boot} = node

-----------------------------
writeDependency :: FilePath -> Handle -> [FilePath] -> FilePath -> IO ()
-- (writeDependency r h [t1,t2] dep) writes to handle h the dependency
--      t1 t2 : dep
writeDependency root hdl targets dep
  = do let -- We need to avoid making deps on
           --     c:/foo/...
           -- on cygwin as make gets confused by the :
           -- Making relative deps avoids some instances of this.
           dep' = makeRelative root dep
           forOutput = escapeSpaces . reslash Forwards . normalise
           output = unwords (map forOutput targets) ++ " : " ++ forOutput dep'
       hPutStrLn hdl output

-----------------------------
insertSuffixes
        :: FilePath     -- Original filename;   e.g. "foo.o"
        -> [String]     -- Suffix prefixes      e.g. ["x_", "y_"]
        -> [FilePath]   -- Zapped filenames     e.g. ["foo.x_o", "foo.y_o"]
        -- Note that the extra bit gets inserted *before* the old suffix
        -- We assume the old suffix contains no dots, so we know where to
        -- split it
insertSuffixes file_name extras
  = [ basename <.> (extra ++ suffix) | extra <- extras ]
  where
    (basename, suffix) = case splitExtension file_name of
                         -- Drop the "." from the extension
                         (b, s) -> (b, drop 1 s)


-----------------------------------------------------------------
--
--              endMkDependHs
--      Complete the makefile, close the tmp file etc
--
-----------------------------------------------------------------

endMkDependHS :: Logger -> MkDepFiles -> IO ()

endMkDependHS logger
   (MkDep { mkd_make_file = makefile, mkd_make_hdl = makefile_hdl,
            mkd_dep_json,
            mkd_tmp_file = tmp_file, mkd_tmp_hdl = tmp_hdl })
  = do
  -- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depEndMarker

  case makefile_hdl of
     Nothing  -> return ()
     Just hdl -> do
        -- slurp the rest of the original makefile and copy it into the output
        SysTools.copyHandle hdl tmp_hdl
        hClose hdl

  hClose tmp_hdl  -- make sure it's flushed

        -- Create a backup of the original makefile
  when (isJust makefile_hdl) $ do
    showPass logger ("Backing up " ++ makefile)
    SysTools.copyFile makefile (makefile++".bak")

        -- Copy the new makefile in place
  showPass logger "Installing new makefile"
  SysTools.copyFile tmp_file makefile

  -- Write the dependency and option data to a json file if the corresponding
  -- flags were specified.
  writeJsonOutput mkd_dep_json


-----------------------------------------------------------------
--              Module cycles
-----------------------------------------------------------------

dumpModCycles :: Logger -> ModuleGraph -> IO ()
dumpModCycles logger module_graph
  | not (logHasDumpFlag logger Opt_D_dump_mod_cycles)
  = return ()

  | null cycles
  = putMsg logger (text "No module cycles")

  | otherwise
  = putMsg logger (hang (text "Module cycles found:") 2 pp_cycles)
  where
    topoSort = GHC.topSortModuleGraph True module_graph Nothing

    cycles :: [[ModuleGraphNode]]
    cycles =
      [ c | CyclicSCC c <- topoSort ]

    pp_cycles = vcat [ (text "---------- Cycle" <+> int n <+> text "----------")
                        $$ pprCycle c $$ blankLine
                     | (n,c) <- [1..] `zip` cycles ]

pprCycle :: [ModuleGraphNode] -> SDoc
-- Print a cycle, but show only the imports within the cycle
pprCycle summaries = pp_group (CyclicSCC summaries)
  where
    cycle_mods :: [ModuleName]  -- The modules in this cycle
    cycle_mods = map (moduleName . ms_mod) [ms | ModuleNode _ ms <- summaries]

    pp_group :: SCC ModuleGraphNode -> SDoc
    pp_group (AcyclicSCC (ModuleNode _ ms)) = pp_ms ms
    pp_group (AcyclicSCC _) = empty
    pp_group (CyclicSCC mss)
        = assert (not (null boot_only)) $
                -- The boot-only list must be non-empty, else there would
                -- be an infinite chain of non-boot imports, and we've
                -- already checked for that in processModDeps
          pp_ms loop_breaker $$ vcat (map pp_group groups)
        where
          (boot_only, others) = partition is_boot_only mss
          is_boot_only (ModuleNode _ ms) = not (any in_group (map snd (ms_imps ms)))
          is_boot_only  _ = False
          in_group (L _ m) = m `elem` group_mods
          group_mods = map (moduleName . ms_mod) [ms | ModuleNode _ ms <- mss]

          loop_breaker = head ([ms | ModuleNode _ ms  <- boot_only])
          all_others   = tail boot_only ++ others
          groups =
            GHC.topSortModuleGraph True (mkModuleGraph all_others) Nothing

    pp_ms summary = text mod_str <> text (take (20 - length mod_str) (repeat ' '))
                       <+> (pp_imps empty (map snd (ms_imps summary)) $$
                            pp_imps (text "{-# SOURCE #-}") (map snd (ms_srcimps summary)))
        where
          mod_str = moduleNameString (moduleName (ms_mod summary))

    pp_imps :: SDoc -> [Located ModuleName] -> SDoc
    pp_imps _    [] = empty
    pp_imps what lms
        = case [m | L _ m <- lms, m `elem` cycle_mods] of
            [] -> empty
            ms -> what <+> text "imports" <+>
                                pprWithCommas ppr ms

-----------------------------------------------------------------
--
--              Flags
--
-----------------------------------------------------------------

depStartMarker, depEndMarker :: String
depStartMarker = "# DO NOT DELETE: Beginning of Haskell dependencies"
depEndMarker   = "# DO NOT DELETE: End of Haskell dependencies"
