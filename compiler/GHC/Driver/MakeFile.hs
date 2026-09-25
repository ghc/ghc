

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

import GHC qualified

import GHC.Data.Bag (listToBag)
import GHC.Data.Graph.Directed (SCC (..))
import GHC.Data.OsPath (unsafeDecodeUtf, unsafeEncodeUtf)

import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Make
import GHC.Driver.Monad
import GHC.Driver.Phases
import GHC.Driver.Pipeline
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Session

import GHC.Iface.Errors.Types
import GHC.Iface.Load (cannotFindModule)

import GHC.SysTools qualified as SysTools
import GHC.Types.PkgQual
import GHC.Types.SourceError
import GHC.Types.SrcLoc
import GHC.Types.Unique.Set (UniqSet)
import GHC.Types.Unique.Set qualified as UniqSet
import GHC.Types.UnresolvedImport

import GHC.Unit.Finder
import GHC.Unit.Info
import GHC.Unit.Module
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModSummary
import GHC.Unit.State (lookupUnitId)

import GHC.Utils.Error
import GHC.Utils.Exception
import GHC.Utils.Logger
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.TmpFs

import Control.Monad (when)
import Data.Either
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (partition)
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error (isEOFError)

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
    mapM_ (liftIO . processDeps dflags hsc_env (UniqSet.mkUniqSet excl_mods) root (mkd_tmp_hdl files)) sorted

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

--------------------------------------------------------------------------------
-- Types abstracting over the output
--------------------------------------------------------------------------------

data DepNode =
  DepNode
    { dn_mod :: Module
    , dn_src :: FilePath
    , dn_obj :: FilePath
    , dn_hi :: FilePath
    , dn_boot :: IsBootInterface
    , dn_preprocessing :: PreprocessingNode
    }

data PreprocessingNode = PreprocessingNode
  { pn_preprocessor :: Maybe String
  , pn_options :: [String]
  }

data Dep
  = DepHi
    { dep_mod :: Module
    , dep_path :: FilePath
    , dep_unit :: Maybe UnitInfo
    , dep_local :: Bool
    , dep_boot :: IsBootInterface
    }
  | DepCpp
    { dep_path :: FilePath
    }


-----------------------------------------------------------------
--
--              processDeps
--
-----------------------------------------------------------------

processDeps :: DynFlags
            -> HscEnv
            -> UniqSet ModuleName -- ^ Excludes
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

processDeps _ hsc_env _ _ _ (CyclicSCC nodes)
  =     -- There shouldn't be any cycles; report them
    throwOneError (initSourceErrorContext (hsc_dflags hsc_env)) $ cyclicModuleErr nodes

processDeps _ hsc_env _ _ _ (AcyclicSCC (InstantiationNode _uid node))
  =     -- There shouldn't be any backpack instantiations; report them as well
    throwOneError (initSourceErrorContext (hsc_dflags hsc_env)) $
      mkPlainErrorMsgEnvelope noSrcSpan $
      GhcDriverMessage $ DriverInstantiationNodeInDependencyGeneration node

processDeps _dflags _ _ _ _ (AcyclicSCC (LinkNode {})) = return ()
processDeps _dflags _ _ _ _ (AcyclicSCC (UnitNode {})) = return ()
processDeps _ _ _ _ _ (AcyclicSCC (ModuleNode _ (ModuleNodeFixed {})))
  -- No dependencies needed for fixed modules (already compiled)
  = return ()

processDeps dflags hsc_env excl_mods root hdl (AcyclicSCC (ModuleNode _ (ModuleNodeCompile node))) = do
  pp <- preprocessor
  let
    dep_node = mkDepNode pp
    find_deps imps = do
      cpp_deps <- find_cpp_deps
      import_deps <- find_import_deps imps
      pure $ map Right cpp_deps ++ import_deps
  (missing_dep_errs, deps) <- partitionEithers <$> find_deps (ms_imps node)

  if null missing_dep_errs
    then do
      writeDependencies include_pkg_deps root hdl extra_suffixes dep_node deps
    else do
      let sec = initSourceErrorContext (hsc_dflags hsc_env)
      throwErrors sec (mkMessages (listToBag missing_dep_errs))
  where
    extra_suffixes = depSuffixes dflags
    include_pkg_deps = depIncludePkgDeps dflags
    src_file = msHsFilePath node
    mkDepNode opts =
      DepNode {
        dn_mod = ms_mod node,
        dn_src = src_file,
        dn_obj = msObjFilePath node,
        dn_hi = msHiFilePath node,
        dn_boot = isBootSummary node,
        dn_preprocessing = opts
      }

    preprocessor :: IO PreprocessingNode
    preprocessor
      | Just src <- ml_hs_file (ms_location node)
      = runPipeline (hsc_hooks hsc_env) $ do
        let (_, suffix) = splitExtension src
            lit | Unlit _ <- startPhase suffix = True
                | otherwise = False
            pipe_env = mkPipeEnv StopPreprocess src Nothing NoOutputFile
        unlit_fn <- if lit then use (T_Unlit pipe_env hsc_env src) else pure src
        (dflags1, opts, _, _) <- use (T_FileArgs (hsc_logger hsc_env) (hsc_dflags hsc_env) unlit_fn)
        let pp = pgm_F dflags1
        pure PreprocessingNode
          { pn_preprocessor = if null pp then global_preprocessor else Just pp
          , pn_options = opts
          }
      | otherwise
      = pure PreprocessingNode
          { pn_preprocessor = global_preprocessor
          , pn_options = []
          }

    global_preprocessor :: Maybe String
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
    find_cpp_deps :: IO [Dep]
    find_cpp_deps = do
      session <- Session <$> newIORef hsc_env
      parsedMod <- reflectGhc (GHC.parseModule node) session
      pure (DepCpp <$> GHC.pm_extra_src_files parsedMod)

    -- Emit a dependency for each import
    find_import_deps :: [UnresolvedImport PkgQual] -> IO [Either (MsgEnvelope GhcMessage) Dep]
    find_import_deps idecls =
      sequence
        [ findDependency hsc_env decl
        | decl <- idecls
        , let L _loc mod = ui_mod_name decl
        , not $ mod `UniqSet.elementOfUniqSet` excl_mods
        ]


findDependency  :: HscEnv
                -> UnresolvedImport PkgQual   -- The import to find
                -> IO (Either (MsgEnvelope GhcMessage) Dep)  -- Interface file
findDependency hsc_env imp = do
  -- Find the module; this will be fast because
  -- we've done it once during downsweep.
  r <- resolveImport hsc_env imp
  case r of
    Found loc dep_mod ->
      pure $ Right
        DepHi
          { dep_mod = dep_mod
          , dep_path = ml_hi_file loc
          , dep_unit = lookupUnitId (hsc_units hsc_env) (moduleUnitId dep_mod)
          , dep_local = isJust (ml_hs_file loc)
          , dep_boot = is_boot
          }

    fail ->
      return $
        Left $
          mkPlainErrorMsgEnvelope srcloc $
          GhcDriverMessage $ DriverInterfaceError $
             (Can'tFindInterface (cannotFindModule hsc_env mod_name fail) (LookingForModule mod_name is_boot))
  where
    L srcloc mod_name = ui_mod_name imp
    is_boot           = ui_boot imp

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
      = [([obj], hi) | (obj, hi) <- zip (suffixed (viaOsPath removeBootSuffix dn_obj)) (suffixed dn_hi)]
      | otherwise
      = []

    -- Add one dependency for each suffix;
    -- e.g.         A.o   : B.hi
    --              A.x_o : B.x_hi
    import_dep = \case
      DepHi {dep_path, dep_unit}
        | isNothing dep_unit || include_pkgs
        -> [([obj], hi) | (obj, hi) <- zip obj_files (suffixed dep_path)]

        | otherwise
        -> []

      DepCpp {dep_path} -> [(obj_files, dep_path)]

    write (from, to) = writeDependency root hdl from to

    obj_files = suffixed dn_obj

    suffixed f = insertSuffixes f suffixes

    DepNode {dn_src, dn_obj, dn_hi, dn_boot} = node

    viaOsPath f a = unsafeDecodeUtf (f (unsafeEncodeUtf a))

-----------------------------
writeDependency :: FilePath -> Handle -> [FilePath] -> FilePath -> IO ()
-- (writeDependency r h [t1,t2] dep) writes to handle h the dependency
--      t1 t2 : dep
writeDependency root hdl targets dep
  = do let -- We need to avoid making deps on
           --     c:/foo/...
           -- on Windows as make gets confused by the :
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
            let dep_mods = map edgeTargetKey deps
                non_boot_deps = filter (not . is_boot_key) dep_mods
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

    pp_mod :: [ModuleNodeEdge] -> ModuleNodeInfo -> SDoc
    pp_mod deps mn =
      text mod_str <> text (take (20 - length mod_str) (repeat ' ')) <> ppr_deps (map edgeTargetKey deps)
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
