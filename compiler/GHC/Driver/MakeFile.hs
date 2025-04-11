

-----------------------------------------------------------------------------
--
-- Makefile Dependency Generation
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module GHC.Driver.MakeFile
   ( doMkDependHS
   , doMkDependModuleGraph
   )
where

import GHC.Prelude

import qualified GHC
import GHC.Driver.Make
import GHC.Driver.Monad
import GHC.Driver.DynFlags
import GHC.Utils.Misc
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import qualified GHC.SysTools as SysTools
import GHC.Data.Graph.Directed ( SCC(..) )
import GHC.Data.OsPath (unsafeDecodeUtf)
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.SourceError
import GHC.Types.SrcLoc
import GHC.Types.PkgQual
import Data.List (partition)
import GHC.Utils.TmpFs

import GHC.Iface.Load (cannotFindModule)

import GHC.Unit.Module
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Graph
import GHC.Unit.Finder

import GHC.Utils.Exception
import GHC.Utils.Error
import GHC.Utils.Logger

import System.Directory
import System.FilePath
import System.IO
import System.IO.Error  ( isEOFError )
import Control.Monad    ( when, forM_ )
import Data.Maybe       ( isJust )
import Data.IORef
import qualified Data.Set as Set
import GHC.Iface.Errors.Types
import Data.Either

-----------------------------------------------------------------
--
--              The main function
--
-----------------------------------------------------------------

doMkDependHS :: GhcMonad m => [FilePath] -> m ()
doMkDependHS srcs = do
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

    -- Do the downsweep to find all the modules
    targets <- mapM (\s -> GHC.guessTarget s Nothing Nothing) srcs
    GHC.setTargets targets
    let excl_mods = depExcludeMods dflags
    module_graph <- GHC.depanal excl_mods True {- Allow dup roots -}
    doMkDependModuleGraph dflags module_graph



doMkDependModuleGraph :: GhcMonad m =>  DynFlags -> ModuleGraph -> m ()
doMkDependModuleGraph dflags module_graph = do
    logger <- getLogger
    tmpfs <- hsc_tmpfs <$> getSession
    let excl_mods = depExcludeMods dflags

    files <- liftIO $ beginMkDependHS logger tmpfs dflags
    let sorted = GHC.topSortModuleGraph False module_graph Nothing

    -- Print out the dependencies if wanted
    liftIO $ debugTraceMsg logger 2 (text "Module dependencies" $$ ppr sorted)

    -- Process them one by one, dumping results into makefile
    -- and complaining about cycles
    hsc_env <- getSession
    root <- liftIO getCurrentDirectory
    mapM_ (liftIO . processDeps dflags hsc_env excl_mods root (mkd_tmp_hdl files)) sorted

    -- If -ddump-mod-cycles, show cycles in the module graph
    liftIO $ dumpModCycles logger module_graph

    -- Tidy up
    liftIO $ endMkDependHS logger files

    -- Unconditional exiting is a bad idea.  If an error occurs we'll get an
    --exception; if that is not caught it's fine, but at least we have a
    --chance to find out exactly what went wrong.  Uncomment the following
    --line if you disagree.

    --`GHC.ghcCatch` \_ -> io $ exitWith (ExitFailure 1)

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


        -- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depStartMarker

  return (MkDep { mkd_make_file = makefile, mkd_make_hdl = mb_make_hdl,
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

processDeps _ _ _ _ _ (CyclicSCC nodes)
  =     -- There shouldn't be any cycles; report them
    throwOneError $ cyclicModuleErr nodes

processDeps _ _ _ _ _ (AcyclicSCC (InstantiationNode _uid node))
  =     -- There shouldn't be any backpack instantiations; report them as well
    throwOneError $
      mkPlainErrorMsgEnvelope noSrcSpan $
      GhcDriverMessage $ DriverInstantiationNodeInDependencyGeneration node

processDeps _dflags _ _ _ _ (AcyclicSCC (LinkNode {})) = return ()
processDeps _dflags _ _ _ _ (AcyclicSCC (UnitNode {})) = return ()
processDeps _ _ _ _ _ (AcyclicSCC (ModuleNode _ (ModuleNodeFixed {})))
  -- No dependencies needed for fixed modules (already compiled)
  = return ()
processDeps dflags hsc_env excl_mods root hdl (AcyclicSCC (ModuleNode _ (ModuleNodeCompile node)))
  = do  { let extra_suffixes = depSuffixes dflags
              include_pkg_deps = depIncludePkgDeps dflags
              src_file  = msHsFilePath node
              obj_file  = msObjFilePath node
              obj_files = insertSuffixes obj_file extra_suffixes

              do_imp loc is_boot pkg_qual imp_mod
                = do { mb_hi <- findDependency hsc_env loc pkg_qual imp_mod
                                               is_boot include_pkg_deps
                     ; case mb_hi of {
                           Nothing      -> return () ;
                           Just hi_file -> do
                     { let hi_files = insertSuffixes hi_file extra_suffixes
                           write_dep (obj,hi) = writeDependency root hdl [obj] hi

                        -- Add one dependency for each suffix;
                        -- e.g.         A.o   : B.hi
                        --              A.x_o : B.x_hi
                     ; mapM_ write_dep (obj_files `zip` hi_files) }}}


                -- Emit std dependency of the object(s) on the source file
                -- Something like       A.o : A.hs
        ; writeDependency root hdl obj_files src_file

          -- add dependency between objects and their corresponding .hi-boot
          -- files if the module has a corresponding .hs-boot file (#14482)
        ; when (isBootSummary node == IsBoot) $ do
            let hi_boot = msHiFilePath node
            let obj     = unsafeDecodeUtf $ removeBootSuffix (msObjFileOsPath node)
            forM_ extra_suffixes $ \suff -> do
               let way_obj     = insertSuffixes obj     [suff]
               let way_hi_boot = insertSuffixes hi_boot [suff]
               mapM_ (writeDependency root hdl way_obj) way_hi_boot

                -- Emit a dependency for each CPP import
        ; when (depIncludeCppDeps dflags) $ do
            -- CPP deps are discovered in the module parsing phase by parsing
            -- comment lines left by the preprocessor.
            -- Note that GHC.parseModule may throw an exception if the module
            -- fails to parse, which may not be desirable (see #16616).
          { session <- Session <$> newIORef hsc_env
          ; parsedMod <- reflectGhc (GHC.parseModule node) session
          ; mapM_ (writeDependency root hdl obj_files)
                  (GHC.pm_extra_src_files parsedMod)
          }

                -- Emit a dependency for each import

        ; let do_imps is_boot idecls = sequence_
                    [ do_imp loc is_boot mb_pkg mod
                    | (mb_pkg, L loc mod) <- idecls,
                      mod `notElem` excl_mods ]

        ; do_imps IsBoot (ms_srcimps node)
        ; do_imps NotBoot (ms_imps node)
        }


findDependency  :: HscEnv
                -> SrcSpan
                -> PkgQual              -- package qualifier, if any
                -> ModuleName           -- Imported module
                -> IsBootInterface      -- Source import
                -> Bool                 -- Record dependency on package modules
                -> IO (Maybe FilePath)  -- Interface file
findDependency hsc_env srcloc pkg imp is_boot include_pkg_deps = do
  -- Find the module; this will be fast because
  -- we've done it once during downsweep
  r <- findImportedModuleWithIsBoot hsc_env imp is_boot pkg
  case r of
    Found loc _
        -- Home package: just depend on the .hi or hi-boot file
        | isJust (ml_hs_file loc) || include_pkg_deps
        -> return (Just (ml_hi_file loc))

        -- Not in this package: we don't need a dependency
        | otherwise
        -> return Nothing

    fail ->
        throwOneError $
          mkPlainErrorMsgEnvelope srcloc $
          GhcDriverMessage $ DriverInterfaceError $
             (Can'tFindInterface (cannotFindModule hsc_env imp fail) (LookingForModule imp is_boot))

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
   (MkDep { mkd_make_file = makefile, mkd_make_hdl =  makefile_hdl,
            mkd_tmp_file  = tmp_file, mkd_tmp_hdl  =  tmp_hdl })
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
    cycle_keys :: [NodeKey]  -- The modules in this cycle
    cycle_keys = map mkNodeKey summaries

    pp_group :: SCC ModuleGraphNode -> SDoc
    pp_group (AcyclicSCC (ModuleNode deps m)) = pp_mod deps m
    pp_group (AcyclicSCC _) = empty
    pp_group (CyclicSCC mss)
        = assert (not (null boot_only)) $
                -- The boot-only list must be non-empty, else there would
                -- be an infinite chain of non-boot imports, and we've
                -- already checked for that in processModDeps
          pp_mod loop_deps loop_breaker $$ vcat (map pp_group groups)
        where
          (boot_only, others) = partitionEithers (map is_boot_only mss)
          is_boot_key (NodeKey_Module (ModNodeKeyWithUid (GWIB _ IsBoot) _)) = True
          is_boot_key _ = False
          is_boot_only n@(ModuleNode deps ms) =
            let non_boot_deps = filter (not . is_boot_key) deps
            in if not (any in_group non_boot_deps)
                then Left (deps, ms)
                else Right n
          is_boot_only n = Right n
          in_group m = m `elem` group_mods
          group_mods = map mkNodeKey mss

          (loop_deps, loop_breaker) =  head boot_only
          all_others   = tail (map (uncurry ModuleNode) boot_only) ++ others
          groups =
            GHC.topSortModuleGraph True (mkModuleGraph all_others) Nothing

    pp_mod :: [NodeKey] -> ModuleNodeInfo -> SDoc
    pp_mod deps mn =
      text mod_str <> text (take (20 - length mod_str) (repeat ' ')) <> ppr_deps deps
      where
        mod_str = moduleNameString (moduleNodeInfoModuleName mn)

    ppr_deps :: [NodeKey] -> SDoc
    ppr_deps [] = empty
    ppr_deps deps =
      let is_mod_dep (NodeKey_Module {}) = True
          is_mod_dep _ = False

          is_boot_dep (NodeKey_Module (ModNodeKeyWithUid (GWIB _ IsBoot) _)) = True
          is_boot_dep _ = False

          cycle_deps = filter (`elem` cycle_keys) deps
          (mod_deps, other_deps) = partition is_mod_dep cycle_deps
          (boot_deps, normal_deps) = partition is_boot_dep mod_deps
      in vcat [
           if null normal_deps then empty
           else text "imports" <+> pprWithCommas ppr normal_deps,
           if null boot_deps then empty
           else text "{-# SOURCE #-} imports" <+> pprWithCommas ppr boot_deps,
           if null other_deps then empty
           else text "depends on" <+> pprWithCommas ppr other_deps
         ]

-----------------------------------------------------------------
--
--              Flags
--
-----------------------------------------------------------------

depStartMarker, depEndMarker :: String
depStartMarker = "# DO NOT DELETE: Beginning of Haskell dependencies"
depEndMarker   = "# DO NOT DELETE: End of Haskell dependencies"
