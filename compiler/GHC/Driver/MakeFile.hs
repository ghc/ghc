

-----------------------------------------------------------------------------
--
-- Makefile Dependency Generation
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Driver.MakeFile
   ( doMkDepend
   , doMkDependHS
   , doMkDependModuleGraph
   )
where

import GHC.Prelude

import GHC qualified

import GHC.Data.Bag (listToBag)
import GHC.Data.FastString (lexicalCompareFS, unpackFS)
import GHC.Data.Graph.Directed (SCC (..))
import GHC.Data.OsPath

import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Make
import GHC.Driver.Monad
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

import GHC.Unit.Env qualified as UnitEnv
import GHC.Unit.Finder
import GHC.Unit.Home.Graph (HomeUnitEnv (homeUnitEnv_dflags))
import GHC.Unit.Info
import GHC.Unit.Info qualified as UnitInfo
import GHC.Unit.Module
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModSummary
import GHC.Unit.State (lookupUnitId)

import GHC.Utils.Error
import GHC.Utils.Exception
import GHC.Utils.Json
import GHC.Utils.Logger
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.TmpFs

import Control.Monad (forM_, guard, when)
import Data.Either
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Monoid qualified as Monoid
import Data.Set qualified as Set
import System.Directory
import System.Directory.OsPath qualified as Directory
import System.IO
import System.IO.Error (isEOFError)
import System.OsPath as OsPath
import System.OsString qualified as OsString

-----------------------------------------------------------------
--
--              The main function
--
-----------------------------------------------------------------

doMkDependHS :: GhcMonad m => [FilePath] -> m ()
{-# DEPRECATED doMkDependHS "Supports only a single home unit.\n \
    \Use 'GHC.Driver.Session.Units.initMkDepend', 'GHC.setTargets' and 'doMkDepend' to run with multiple home unit support."
#-}
doMkDependHS srcs = do
    -- Initialisation
    targets <- mapM (\s -> GHC.guessTarget s Nothing Nothing) srcs
    GHC.setTargets targets
    doMkDepend

-- | Generate the dependency information for the current GHC session.
--
-- Uses 'GHC.Target's set by 'GHC.setTargets' to compute the module graph
-- and outputs the dependency information topologically sorted.
doMkDepend :: GhcMonad m => m ()
doMkDepend = do
    hug_ <- hsc_HUG <$> getSession

    let hug =
          fmap (\ hue ->
            let
              -- We kludge things a bit for dependency generation. Rather than
              -- generating dependencies for each way separately, we generate
              -- them once and then duplicate them for each way's osuf/hisuf.
              -- We therefore do the initial dependency generation with an empty
              -- way and .o/.hi extensions, regardless of any flags that might
              -- be specified.
              dflags1 = (homeUnitEnv_dflags hue)
                { targetWays_ = Set.empty
                , hiSuf_      = "hi"
                , objectSuf_  = "o"
                }
            in
              hue {homeUnitEnv_dflags = dflags1}
              ) hug_

    _ <- GHC.setProgramHUG hug

    dflagsGlobal <- GHC.getSessionDynFlags
    -- If no suffix is provided, use the default -- the empty one
    let dflags = if null (depSuffixes dflagsGlobal)
                 then dflagsGlobal { depSuffixes = [""] }
                 else dflagsGlobal

    let excl_mods = depExcludeMods dflags
    module_graph <- GHC.depanal excl_mods True {- Allow dup roots -}
    doMkDependModuleGraph dflags module_graph

doMkDependModuleGraph :: GhcMonad m => DynFlags -> ModuleGraph -> m ()
doMkDependModuleGraph dflags module_graph = do
    hsc_env <- getSession
    logger <- getLogger
    let
      tmpfs = hsc_tmpfs hsc_env
      excl_mods = depExcludeMods dflags

    root <- liftIO Directory.getCurrentDirectory

    let
      suffixes = fmap unsafeEncodeUtf (depSuffixes dflags)
      tmpdir = tmpDir dflags
      backends = concat
        [ [ initFileDepWriter logger root tmpfs tmpdir suffixes makefile_output
          | Just makefile_output <- [initDepOutput (depMakefile dflags)]
          ]
        , [ initJsonDepWriter logger tmpfs tmpdir json_output
          | Just json_output <- [initDepOutput (depJson dflags)]
          ]
        ]


    let sorted = GHC.topSortModuleGraph False module_graph Nothing
    -- Print out the dependencies if wanted
    liftIO $ debugTraceMsg logger 2 (text "Module dependencies" $$ ppr sorted)

    liftIO $ do

      -- Setup the writers
      withWriters backends $ \ write_dep -> do
        -- Do the actual work
        -- Process them one by one, dumping results and complaining about cycles
        forM_
          sorted
          (\ mod_graph_node ->
            processDeps hsc_env (UniqSet.mkUniqSet excl_mods) mod_graph_node >>= \ case
              Nothing -> pure ()
              Just dep_node -> write_dep dep_node
          )

        -- If -ddump-mod-cycles, show cycles in the module graph
        liftIO $ dumpModCycles logger module_graph

    -- Unconditional exiting is a bad idea.  If an error occurs we'll get an
    --exception; if that is not caught it's fine, but at least we have a
    --chance to find out exactly what went wrong.  Uncomment the following
    --line if you disagree.

    --`GHC.ghcCatch` \_ -> io $ exitWith (ExitFailure 1)

--------------------------------------------------------------------------------
-- Types abstracting over the output
--------------------------------------------------------------------------------

-- | A 'DepNode' is a direct translation of a 'ModuleGraphNode' that
-- can be serialised to a Makefile or json lines format.
data DepNode
  = ModuleDepNode HomeModuleNode [Dep]
  -- ^ A dependency node of a home module with its dependencies
  | LinkDepNode DepLink [DepLinkNode]

-- | A home module node.
-- Everything we know about home modules that can be serialised.
data HomeModuleNode =
  HomeModuleNode
    { hmn_unitId :: UnitId
    -- ^ 'UnitId' of the 'HomeUnitEnv' this home module belongs to.
    , hmn_packageName :: Maybe String
    -- ^ Optional package name, used for @-XPackageImports@.
    , hmn_mod :: ModuleWithIsBoot
    -- ^ 'Module' of this home module with 'IsBootInterface' information.
    , hmn_src :: OsPath
    -- ^ Where is the source for this home module located.
    , hmn_obj :: OsPath
    -- ^ Where is the object file for this home module located if it exists.
    , hmn_hi :: OsPath
    -- ^ Where is the interface file for this home module located if it exists.
    , hmn_preprocessing :: Maybe PreprocessingNode
    -- ^ Is there a preprocessor that needs to be run for this module before compilation.
    , hmn_includePkgDeps :: Bool
    -- ^ When serialising this home module, should we include external package dependencies
    -- (such as importing @Data.List@ from @base@) in the output?
    }

-- | Preprocessor that needs to be run before a module can be compiled.
data PreprocessingNode = PreprocessingNode
  { pn_preprocessor :: String
  -- ^ The preprocessor.
  , pn_arguments :: [String]
  }

-- | A dependency edge on another module, or cpp path.
data Dep
  = DepHi
    -- ^ A dependency on a Haskell interface.
    { dep_mod :: Module
    -- ^ The 'Module' a dependency node is depending on.
    , dep_path :: OsPath
    -- ^ Location of the module
    , dep_unit :: Maybe UnitInfo
    -- ^ To which external unit does this dependency belong to?
    -- If this is 'Nothing', then this should mean this is a home module.
    , dep_local :: Bool
    -- ^ Is this dependency a home module?
    , dep_level :: ImportLevel
    -- ^ Is this dependency used at a specific 'ImportLevel'?
    , dep_boot :: IsBootInterface
    -- ^ Is this dependency imported using @{- SOURCE -}@.
    -- May only be 'IsBoot' if it is a home module from the same home unit
    -- as the 'DepModuleNode'.
    }
  | DepCpp
    -- ^ A dependency on a CPP @#include@ in the Haskell source file, identified by the preprocessor.
    { dep_path :: OsPath
    -- ^ Path to the @#include@d path.
    }

data DepLink
  = DepLink
    { link_output :: !UnitId
    , link_type :: !GhcLink
    }

data DepLinkNode
  = DepLink_Unit !UnitId
  | DepLink_Module !ModuleNameWithIsBoot !UnitId
  | DepLink_HomeUnit {-# UNPACK #-} !UnitId
  | DepLink_ExtUnit {-# UNPACK #-} !UnitId


-- | 'DependencyWriter' is a backend for outputting 'DepNode' in a specific
-- output format.
data DependencyWriter = DependencyWriter
  { dw_writeDependency :: DepNode -> IO ()
  -- ^ Serialise a 'DepNode' and write it out.
  , dw_endWriter :: IO ()
  -- ^ We finished consuming the dependency information, perform final
  -- tidy up operations. This may be a no-op, but could also
  -- copy temporary files, close open handlers, etc...
  --
  -- If 'dw_endWriter' was called, calling 'dw_onError' is unnecessary,
  -- but it must be safe to call 'dw_onError' any way.
  , dw_onError :: IO ()
  -- ^ Something crashed! Clean up any resources opened by this 'DependencyWriter'.
  }

-- | Initialise multiple backends and combine them into one output function.
--
-- Combines the 'DepSink' of each 'DependencyWriter' into one single consumer.
withWriters :: [IO DependencyWriter] -> ((DepNode -> IO ()) -> IO a) -> IO a
withWriters [] k = k (\ _ -> pure ())
withWriters (dw : dws) k =
  withWriter
    dw
    $ \s -> withWriters dws (\s' -> k (s `mappendWriter` s'))
  where
    mappendWriter w1 w2 = \ node -> w1 node >> w2 node

-- | Initialise a single 'DependencyWriter', making sure any allocated resource
-- get's tidied up again.
--
-- Accepts 'IO' 'DependencyWriter' since the initialisation may perform setup operations,
-- such as opening file handles, or intermediate data types.
withWriter :: IO DependencyWriter -> ((DepNode -> IO ()) -> IO r) -> IO r
withWriter dwIO k =
  bracketOnError dwIO dw_onError $ \ dw -> do
    r <- k (dw_writeDependency dw)
    -- Clean up open resources
    dw_endWriter dw
    pure r

-- | Where should the dependency output end up?
data DepOutput
  = DepStreamHandle Handle
  -- ^ The dependency information should be written to this 'Handle'.
  | DepFileOutput FilePath
  -- ^ The dependency information should be written to this 'FilePath'.

-- | A 'DepStream' is an abstraction over how to output single elements
-- of the dependency generation, e.g., a json object or one makefile rule.
data DepStream = DepStream
  { depStream_write :: String -> IO ()
  , depStream_close :: IO ()
  , depStream_onError :: IO ()
  }

initDepOutput :: MkDependOutput -> Maybe DepOutput
initDepOutput = \ case
  MkDepend_None -> Nothing
  MkDepend_Default fp -> Just $ DepFileOutput fp
  MkDepend_Explicit fp -> Just $ DepFileOutput fp
  MkDepend_StdOut -> Just $ DepStreamHandle stdout

initMakefileDepStream :: Logger -> TmpFs -> TempDir -> DepOutput -> IO DepStream
initMakefileDepStream _ _ _ (DepStreamHandle h) =
  pure $ depStreamHandle h
initMakefileDepStream logger tmpfs tmpdir (DepFileOutput fp) = do
  files <- beginMkDependHS logger tmpfs tmpdir fp
  pure $
    DepStream
      { depStream_write = hPutStrLn (mkd_tmp_hdl files)
      , depStream_onError = do
          hClose $ mkd_tmp_hdl files
          case mkd_make_hdl files of
            Nothing -> return ()
            Just hdl -> hClose hdl
      , depStream_close = endMkDependHS logger files
      }

initJsonDepStream :: Logger -> TmpFs -> TempDir -> DepOutput -> IO DepStream
initJsonDepStream _ _ _ (DepStreamHandle h) =
  pure $ depStreamHandle h
initJsonDepStream logger tmpfs tmpdir (DepFileOutput fp) = do
  files <- initJsonDepFiles logger tmpfs tmpdir fp
  pure $
    DepStream
      { depStream_write = hPutStrLn (json_tmp_hdl files)
      , depStream_onError = hClose (json_tmp_hdl files)
      , depStream_close = endJsonDepFiles logger files
      }

depStreamHandle :: Handle -> DepStream
depStreamHandle h =
    DepStream
      { depStream_write =
          \output -> do
            hPutStrLn h output
            hFlush h -- We flush to ensure the output is streamed per line.
      , depStream_close = pure ()
      , depStream_onError = pure ()
      }

-----------------------------------------------------------------
--
--              processDeps
--
-----------------------------------------------------------------

processDeps :: HscEnv
            -> UniqSet ModuleName -- ^ Excludes
            -> SCC ModuleGraphNode
            -> IO (Maybe DepNode)
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

processDeps hsc_env _ (CyclicSCC nodes)
  =     -- There shouldn't be any cycles; report them
    throwOneError (initSourceErrorContext (hsc_dflags hsc_env)) $ cyclicModuleErr nodes

processDeps hsc_env _ (AcyclicSCC (InstantiationNode _uid node))
  =     -- There shouldn't be any backpack instantiations; report them as well
    throwOneError (initSourceErrorContext (hsc_dflags hsc_env)) $
      mkPlainErrorMsgEnvelope noSrcSpan $
      GhcDriverMessage $ DriverInstantiationNodeInDependencyGeneration node

processDeps hsc_env _ (AcyclicSCC (LinkNode keys unitId)) = do
  let
    toLinkNodeDep = \ case
      NodeKey_Unit iu -> DepLink_Unit $ virtualUnitId iu
      NodeKey_Module node -> DepLink_Module (mnkModuleName node) (mnkUnitId node)
      NodeKey_Link uid -> DepLink_HomeUnit uid
      NodeKey_ExternalUnit uid -> DepLink_ExtUnit uid

    linkNode = DepLink
      { link_output = unitId
      , link_type = ghcLink $ UnitEnv.ue_unitFlags unitId (hsc_unit_env hsc_env)
      }

  pure $ Just $ LinkDepNode linkNode (map toLinkNodeDep keys)

processDeps _ _ (AcyclicSCC (UnitNode {})) = return Nothing
processDeps _ _ (AcyclicSCC (ModuleNode _ (ModuleNodeFixed {})))
  -- No dependencies needed for fixed modules (already compiled)
  = return Nothing

processDeps hsc_env0 excl_mods (AcyclicSCC (ModuleNode _ (ModuleNodeCompile node))) = do
  let
    find_deps imps = do
      cpp_deps <- find_cpp_deps
      import_deps <- find_import_deps imps
      pure $ map Right cpp_deps ++ import_deps

  (missing_dep_errs, deps) <- partitionEithers <$> find_deps (ms_imps node)

  if null missing_dep_errs
    then do
      pure $ Just $ ModuleDepNode mkDepNode deps
    else do
      let sec = initSourceErrorContext dflags
      throwErrors sec (mkMessages (listToBag missing_dep_errs))
  where
    -- Operations such import resolution depend on the currently active home unit id.
    -- With multiple home units, each module may be from a different home unit with
    -- separate dependencies and options.
    -- Thus, we need to set the active unit id to the one of the module we are processing.
    hsc_env = hscSetActiveUnitId (ms_unitid node) hsc_env0
    dflags = ms_hspp_opts node
    src_file = msHsFileOsPath node
    mkDepNode =
      HomeModuleNode
        { hmn_unitId = ms_unitid node
        , hmn_packageName = thisPackageName dflags
        , hmn_mod = GWIB (ms_mod node) (isBootSummary node)
        , hmn_src = src_file
        , hmn_obj = msObjFileOsPath node
        , hmn_hi = msHiFileOsPath node
        , hmn_preprocessing = preprocessor
        , hmn_includePkgDeps = depIncludePkgDeps dflags
        }

    preprocessor :: Maybe PreprocessingNode
    preprocessor = do
      guard (gopt Opt_Pp dflags)
      let pp = pgm_F dflags
      guard (not $ null pp)
      let args = opt_F dflags
      pure $ PreprocessingNode
        { pn_preprocessor = pp
        , pn_arguments = args
        }

    -- Emit a dependency for each CPP import
    -- CPP deps are discovered in the module parsing phase by parsing
    -- comment lines left by the preprocessor.
    -- Note that GHC.parseModule may throw an exception if the module
    -- fails to parse, which may not be desirable (see #16616).
    find_cpp_deps :: IO [Dep]
    find_cpp_deps =
      if depIncludeCppDeps dflags
        then do
          session <- Session <$> newIORef hsc_env
          parsedMod <- reflectGhc (GHC.parseModule node) session
          pure (DepCpp . unsafeEncodeUtf <$> GHC.pm_extra_src_files parsedMod)
        else
          pure []

    -- Emit a dependency for each import
    find_import_deps :: [Located (UnresolvedImport PkgQual)] -> IO [Either (MsgEnvelope GhcMessage) Dep]
    find_import_deps idecls =
      sequence
        [ findDependency hsc_env decl
        | decl <- idecls
        , not $ ui_mod_name (unLoc decl) `UniqSet.elementOfUniqSet` excl_mods
        ]

findDependency  :: HscEnv
                -> Located (UnresolvedImport PkgQual)   -- The import to find
                -> IO (Either (MsgEnvelope GhcMessage) Dep)  -- Interface file
findDependency hsc_env (L srcloc imp) = do
  -- Find the module; this will be fast because
  -- we've done it once during downsweep.
  r <- runFinderM $ resolveImport hsc_env imp
  case r of
    Found loc dep_mod ->
      pure $ Right
        DepHi
          { dep_mod = dep_mod
          , dep_path = ml_hi_file_ospath loc
          , dep_unit = lookupUnitId (hsc_units hsc_env) (moduleUnitId dep_mod)
          , dep_local = isJust (ml_hs_file loc)
          , dep_boot = is_boot
          , dep_level = ui_level imp
          }

    fail ->
      return $
        Left $
          mkPlainErrorMsgEnvelope srcloc $
          GhcDriverMessage $ DriverInterfaceError $
             (Can'tFindInterface (cannotFindModule hsc_env mod_name fail) (LookingForModule mod_name is_boot))
  where
    mod_name = ui_mod_name imp
    is_boot  = ui_boot imp

-----------------------------------------------------------------
--
--              beginMkDependHs
--      Create a temporary file,
--      find the Makefile,
--      slurp through it, etc
--
-----------------------------------------------------------------

initFileDepWriter :: Logger -> OsPath -> TmpFs -> TempDir -> [OsPath] -> DepOutput -> IO DependencyWriter
initFileDepWriter logger root tmpfs tmpdir suffixes output = do
  stream <- initMakefileDepStream logger tmpfs tmpdir output
  pure DependencyWriter
    { dw_writeDependency = \ case
        ModuleDepNode node deps -> do
          traverse_ (depStream_write stream) (dependencyRules root suffixes node deps)
        LinkDepNode _node _deps -> do
          -- Makefile can't handle 'LinkDepNode'
          pure ()
    , dw_endWriter = depStream_close stream
    , dw_onError = depStream_onError stream
    }

data MkDepFiles
  = MkDep { mkd_make_file :: FilePath,          -- Name of the makefile
            mkd_make_hdl  :: Maybe Handle,      -- Handle for the open makefile
            mkd_tmp_file  :: FilePath,          -- Name of the temporary file
            mkd_tmp_hdl   :: Handle }           -- Handle of the open temporary file

beginMkDependHS :: Logger -> TmpFs -> TempDir -> FilePath -> IO MkDepFiles
beginMkDependHS logger tmpfs tmpdir makefile = do
        -- open a new temp file in which to stuff the dependency info
        -- as we go along.
  tmp_file <- newTempName logger tmpfs tmpdir TFL_CurrentModule "dep"
  tmp_hdl <- openFile tmp_file WriteMode

        -- open the makefile
  exists <- System.Directory.doesFileExist makefile
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

dependencyRules ::
  OsPath ->
  [OsString] ->
  -- ^ Suffixes
  HomeModuleNode ->
  [Dep] ->
  [String]
dependencyRules root suffixes node deps =
  map rule tasks
  where
    tasks = source_dep : boot_dep ++ concatMap import_dep deps

    -- Emit std dependency of the object(s) on the source file
    -- Something like       A.o : A.hs
    source_dep = (obj_files, hmn_src)

    -- add dependency between objects and their corresponding .hi-boot
    -- files if the module has a corresponding .hs-boot file (#14482)
    boot_dep = case gwib_isBoot hmn_mod of
      IsBoot -> [([obj], hi) | (obj, hi) <- zip (suffixed (removeBootSuffix hmn_obj)) (suffixed hmn_hi)]
      NotBoot -> []

    -- Add one dependency for each suffix;
    -- e.g.         A.o   : B.hi
    --              A.x_o : B.x_hi
    import_dep = \case
      DepHi {dep_path, dep_local}
        | dep_local || hmn_includePkgDeps
        -> [([obj], hi) | (obj, hi) <- zip obj_files (suffixed dep_path)]

        | otherwise
        -> []

      DepCpp {dep_path} -> [(obj_files, dep_path)]

    rule (from, to) = dependencyRule root from to

    obj_files = suffixed hmn_obj

    suffixed f = insertSuffixes f suffixes

    HomeModuleNode {hmn_src, hmn_obj, hmn_hi, hmn_mod, hmn_includePkgDeps} = node

-----------------------------
dependencyRule :: OsPath -> [OsPath] -> OsPath -> String
-- (dependencyRule r [t1,t2] dep) renders the dependency
--      t1 t2 : dep
dependencyRule root targets dep
  = let -- We need to avoid making deps on
        --     c:/foo/...
        -- on Windows as make gets confused by the :
        -- Making relative deps avoids some instances of this.
        dep' = OsPath.makeRelative root dep
        forOutput = escapeSpaces . reslash Forwards . unsafeDecodeUtf . OsPath.normalise
    in
        unwords (map forOutput targets) ++ " : " ++ forOutput dep'

-----------------------------
insertSuffixes
        :: OsPath     -- Original filename;   e.g. "foo.o"
        -> [OsString]     -- Suffix prefixes      e.g. ["x_", "y_"]
        -> [OsPath]   -- Zapped filenames     e.g. ["foo.x_o", "foo.y_o"]
        -- Note that the extra bit gets inserted *before* the old suffix
        -- We assume the old suffix contains no dots, so we know where to
        -- split it
insertSuffixes file_name extras
  = [ basename <.> (extra Monoid.<> suffix) | extra <- extras ]
  where
    (basename, suffix) = case OsPath.splitExtension file_name of
                         -- Drop the "." from the extension
                         (b, s) -> (b, OsString.drop 1 s)


-----------------------------------------------------------------
-- endMkDependHs
-- Complete the makefile, close the tmp file etc

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
--
--              JSON MkDepends output
--
-----------------------------------------------------------------

initJsonDepWriter :: Logger -> TmpFs -> TempDir -> DepOutput -> IO DependencyWriter
initJsonDepWriter logger tmpfs tmpdir output = do
  stream <- initJsonDepStream logger tmpfs tmpdir output
  json_var <- mkJsonOutput
  pure
    DependencyWriter
      { dw_writeDependency = \ case
          ModuleDepNode node deps -> do
            extUnits <- readJson json_var
            let
              (extUnitsDeps, payload) = homeUnitDeps node deps
              newExtUnits = Set.difference extUnitsDeps (externalDeps extUnits)
            -- Write new external unit deps before the module that uses them
            for_ newExtUnits $ \extUnit -> do
              depStream_write stream (renderJsonl extUnit)
            depStream_write stream (renderJsonl payload)
            -- Cache external deps we already reported in the past
            updateJson json_var $ cacheExternalDepsJson newExtUnits
          LinkDepNode node deps -> do
            depStream_write stream $ renderJsonl $ linkNodeJson node deps
      , dw_endWriter = depStream_close stream
      , dw_onError = depStream_onError stream
      }

--------------------------------------------------------------------------------
-- Output interface for json dumps

-- | State to keep track of during `-dep-json` generation.
newtype JsonState
  = JsonState
  { json_ref :: IORef DepJson
  -- ^ This ref is updated in @processDeps@ incrementally, using a
  -- flag-specific type.
  }

data JsonDepFiles = JsonDepFiles
  { json_path :: FilePath
    -- ^ The output file path specified as argument to the flag.
  , json_tmp_file  :: FilePath
    -- ^ Name of the temporary file
  , json_tmp_hdl   :: Handle
    -- ^ Handle of the open temporary file
  }

initJsonDepFiles :: Logger -> TmpFs -> TempDir -> FilePath -> IO JsonDepFiles
initJsonDepFiles logger tmpfs tmpdir path = do
  -- open a new temp file in which to stuff the dependency info into
  -- as we go along.
  json_tmp_file <- newTempName logger tmpfs tmpdir TFL_CurrentModule "dep-json"
  json_tmp_hdl <- openFile json_tmp_file WriteMode
  pure $ JsonDepFiles
    { json_path = path
    , json_tmp_file = json_tmp_file
    , json_tmp_hdl = json_tmp_hdl
    }

-- | Finalise the json generation by copying the temporary file to the final destination.
-- Avoids partial writes in the case of an error.
endJsonDepFiles :: Logger -> JsonDepFiles -> IO ()
endJsonDepFiles logger JsonDepFiles {json_path, json_tmp_file, json_tmp_hdl} = do
  hClose json_tmp_hdl
  -- Copy the new jsonl in place
  showPass logger "Installing new json output"
  SysTools.copyFile json_tmp_file json_path

-- | Set up json generation for `ghc -M`.
mkJsonOutput :: IO JsonState
mkJsonOutput =
  JsonState <$> newIORef (DepJson Set.empty)

updateJson :: JsonState -> (DepJson -> DepJson) -> IO ()
updateJson JsonState {json_ref} f = modifyIORef' json_ref f

readJson :: JsonState -> IO DepJson
readJson JsonState {json_ref} = readIORef json_ref

--------------------------------------------------------------------------------
-- Output helpers

renderJsonl :: ToJson a => a -> String
renderJsonl output =
  showSDocUnsafe $ renderJSON $ json output

--------------------------------------------------------------------------------
-- Payload for -dep-json

-- | State of the @-dep-json@ dependency generation.
data DepJson = DepJson
  { externalDeps :: Set.Set ExtUnitDep
  -- ^ The external units we have already published/generated.
  -- We show each external unit exactly once, before the first time they are referenced.
  }

-- | The json representation of a home module node.
--
-- In the json representation, we omit empty lists and null values from the object, or
-- when a sensible default can be assumed.
-- For example, the @boot@ is only present in the final json object, if it is 'IsBoot'.
data HomeUnitDep = HomeUnitDep
  { unitId :: UnitId
  , packageName :: Maybe String
  , modWithIsBoot :: ModuleWithIsBoot
  , source :: OsPath
  , imports :: ImportDeps
  , cpp :: Set.Set OsPath
  , preprocessor :: Maybe PreprocessingNode
  }

addImport :: ImportDep -> HomeUnitDep -> HomeUnitDep
addImport import_dep mod_node_deps =
  mod_node_deps
    { imports = ImportDeps $ Map.insertWith Set.union uid (Set.singleton import_dep) (getImports $ imports mod_node_deps)
    }
  where
    uid = toUnitId $ moduleUnit $ imp_mod import_dep

addCpp :: OsPath -> HomeUnitDep -> HomeUnitDep
addCpp p mod_node_deps =
  mod_node_deps
    { cpp = Set.insert p (cpp mod_node_deps)
    }

-- | Import object of a home module node.
-- We group imports by 'UnitId' to avoid repetition when many modules are imported from the
-- same unit.
newtype ImportDeps = ImportDeps
  { getImports :: Map.Map UnitId (Set.Set ImportDep)
  }

-- | A full import specification.
-- Note, the json representation of 'ImportDep' omits keys if they are the default value.
-- In particular, we don't serialise the 'ImportLevel', if it is 'NormalLevel', and we are also omitting
-- 'IsBootInterface' if it is 'IsBoot'. This allows us to express the default case (a normal, non-source import)
-- as simply the module name.
data ImportDep = ImportDep
  { imp_mod :: Module
  , imp_level :: ImportLevel
  , imp_isBoot :: IsBootInterface
  } deriving (Eq, Ord)

mkImportDep :: Module -> ImportLevel -> IsBootInterface -> ImportDep
mkImportDep modl level isBoot = ImportDep
  { imp_mod = modl
  , imp_level = level
  , imp_isBoot = isBoot
  }

data ExtUnitDep = ExtUnitDep
  { extUnitId :: UnitId
  -- ^ The 'UnitId' of a unit. This is assumed to be globally unique.
  , extUnitName :: String
  , extUnitPackageId :: DPackageId
  } deriving (Eq, Ord)

newtype DPackageId = DPackageId PackageId
  deriving newtype (Eq)

instance Ord DPackageId where
  DPackageId (PackageId d1) `compare` DPackageId (PackageId d2) = d1 `lexicalCompareFS` d2

-- | Json representation of a 'LinkNode'.
data LinkNodeDep = LinkNodeDep
  { linkNodeUid :: !UnitId
  -- ^ The 'UnitId' of the home unit we are producing a link node for.
  , linkNodeType :: !GhcLink
  , linkNodeModuleDeps :: !(Map.Map UnitId (Set.Set ModuleNameWithIsBoot))
  -- ^ Module dependencies grouped by 'UnitId'.
  , linkNodeUnitDeps :: !(Set.Set UnitId)
  -- ^ Backpack unit instantiation.
  , linkNodeHomeUnitDeps :: !(Set.Set UnitId)
  -- ^ Dependencies on other home unit 'LinkNode's.
  , linkNodeExtUnitDeps :: !(Set.Set UnitId)
  -- ^ Dependencies on external 'UnitId's.
  }

{- Note [ghc -M -dep-json output format]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The following instances must conform to the JSON schema:

* 'ToJson' 'HomeUnitDep'
* 'ToJson' 'ExtUnitDep'
* 'ToJson' 'LinkNodeDep'

The instances must conform to docs/users_guide/make-depends-json-schema-1_0.json.
When the schema is altered, please bump the version.
If the content is altered in a backwards compatible way,
update the minor version (e.g. 1.3 ~> 1.4).
If the content is breaking, update the major version (e.g. 1.3 ~> 2.0).
When updating the schema, replace the above file and name it appropriately with
the version appended, and change the documentation of the -dep-json
flag to reflect the new schema.
To learn more about JSON schemas, check out the below link:
https://json-schema.org
-}


-- See Note [ghc -M -dep-json output format]
instance ToJson HomeUnitDep where
  json HomeUnitDep{unitId, packageName, modWithIsBoot, source, imports, cpp, preprocessor} =
    JSObject $
      concat $
        [ mkPair      "module"       (JSString $ moduleKey (gwib_mod modWithIsBoot))
        , mkPair      "unit-id"      (JSString $ unitIdString unitId)
        , isBootKey   "boot"         (gwib_isBoot modWithIsBoot)
        , mkPair      "imports"      (importsObj imports)
        , omitIfNull  "package-name" (maybe JSNull JSString packageName)
        , mkPair      "source"       (JSString . showOsPath $ normalise source)
        , omitIfEmpty "includes"     (strArray (Set.toList cpp) showOsPath)
        , omitIfNull  "preprocessor" (maybe JSNull preprocessorObj preprocessor)
        ]
   where
    moduleKey :: Module -> String
    moduleKey t = moduleNameString (moduleName t)

    isBootKey :: String -> IsBootInterface -> [(String, JsonDoc)]
    isBootKey k b =
      omitIfDefault k b (== NotBoot) (JSBool . (== IsBoot))

    importsObj :: ImportDeps -> JsonDoc
    importsObj importDeps = JSObject $ map importObj (Map.toList $ getImports importDeps)

    importObj :: (UnitId, Set.Set ImportDep) -> (String, JsonDoc)
    importObj (uid, importSet) =
      (unitIdString uid, array importSet importDepObj)

    importDepObj :: ImportDep -> JsonDoc
    importDepObj ImportDep{imp_mod, imp_level, imp_isBoot}
      | NormalLevel <- imp_level, NotBoot <- imp_isBoot =
          JSString (moduleKey imp_mod)
      | otherwise =
          JSObject $
            concat
              [ mkPair        "module" (JSString (moduleKey imp_mod))
              , omitIfDefault "level"  imp_level (== NormalLevel) importLevel
              , isBootKey     "boot"   imp_isBoot
              ]

    importLevel :: ImportLevel -> JsonDoc
    importLevel lvl =
      JSString $ case lvl of
        NormalLevel -> "normal"
        SpliceLevel -> "splice"
        QuoteLevel -> "quote"

    preprocessorObj :: PreprocessingNode -> JsonDoc
    preprocessorObj pp =
      JSObject $
        concat
          [ mkPair      "command"   (JSString $ pn_preprocessor pp)
          , omitIfEmpty "arguments" (strArray (pn_arguments pp) id)
          ]

    array values render = JSArray (fmap render (Set.toList values))
    strArray values render = fmap (JSString . render) values

    omitIfDefault :: String -> a -> (a -> Bool) -> (a -> JsonDoc) -> [(String, JsonDoc)]
    omitIfDefault k v p toJson
      | p v = []
      | otherwise = [(k, toJson v)]

    omitIfNull :: String -> JsonDoc -> [(String, JsonDoc)]
    omitIfNull _ JSNull = []
    omitIfNull k v      = [(k, v)]

    omitIfEmpty :: String -> [JsonDoc] -> [(String, JsonDoc)]
    omitIfEmpty k vs =
      omitIfDefault k vs null JSArray

    showOsPath = unsafeDecodeUtf
    mkPair k v = [(k, v)]

-- See Note [ghc -M -dep-json output format]
instance ToJson ExtUnitDep where
  json ExtUnitDep {extUnitId, extUnitName, extUnitPackageId} =
    JSObject
      [ ("unit-id", JSString $ unitIdString extUnitId)
      , ("package-name", JSString extUnitName)
      , ("package-id", JSString $ showPackageId extUnitPackageId)
      ]
    where
      showPackageId (DPackageId (PackageId pid)) = unpackFS pid

-- See Note [ghc -M -dep-json output format]
instance ToJson LinkNodeDep where
  json LinkNodeDep{linkNodeUid, linkNodeType, linkNodeModuleDeps, linkNodeUnitDeps, linkNodeHomeUnitDeps, linkNodeExtUnitDeps} =
    JSObject
      [ ("unit-id", JSString $ unitIdString linkNodeUid)
      , ("link-type", linkTypeJson linkNodeType)
      , ("links", JSArray $ concat
        [ map renderMods (Map.toList linkNodeModuleDeps)
        , map (JSString . unitIdString) (Set.toList linkNodeUnitDeps)
        , map (JSString . unitIdString) (Set.toList linkNodeHomeUnitDeps)
        , map (JSString . unitIdString) (Set.toList linkNodeExtUnitDeps)
        ])
      ]
    where
      linkTypeJson :: GhcLink -> JsonDoc
      linkTypeJson = JSString . \ case
        NoLink{} -> panic "Received -no-link, but a link node must not exist with -no-link."
        LinkExecutable{} -> "executable"
        LinkInMemory{} -> "in-memory"
        LinkDynLib{} -> "dynamic-library"
        LinkBytecodeLib{} -> "bytecode-library"
        LinkStaticLib{} -> "static-library"
        LinkMergedObj{} -> "merged-object"

      renderMods (u, ms) =
        JSObject
          [ ("unit-id", JSString $ unitIdString u)
          , ("modules", JSArray $ map moduleKey (Set.toList ms))
          ]


      moduleNameJson m = JSString $ moduleNameString m

      moduleKey :: ModuleNameWithIsBoot -> JsonDoc
      moduleKey t =
        case gwib_isBoot t of
          NotBoot -> moduleNameJson (gwib_mod t)
          IsBoot -> JSObject
            [ ("module", moduleNameJson (gwib_mod t))
            -- Redundant check, but the compiler will fix it and
            -- this feels like it might avoid bugs in the future.
            , ("boot", JSBool $ gwib_isBoot t == IsBoot)
            ]

cacheExternalDepsJson :: Set.Set ExtUnitDep -> DepJson -> DepJson
cacheExternalDepsJson extUnits (DepJson e0) =
  DepJson
    { externalDeps =
        Set.union e0 extUnits
    }

homeUnitDeps :: Foldable t => HomeModuleNode -> t Dep -> (Set.Set ExtUnitDep, HomeUnitDep)
homeUnitDeps HomeModuleNode {..} deps =
  foldl' go (Set.empty, initial_node_data) deps
  where
    initial_node_data =
      HomeUnitDep
        { unitId = hmn_unitId
        , packageName = hmn_packageName
        , modWithIsBoot = hmn_mod
        , source = hmn_src
        , preprocessor = hmn_preprocessing
        , cpp = Set.empty
        , imports = ImportDeps Map.empty
        }

    go (extDeps, node_data) = \ case
      DepHi {dep_mod, dep_local, dep_unit, dep_boot, dep_level}
        | dep_local
        -> (extDeps, addImport (mkImportDep dep_mod dep_level dep_boot) node_data)

        | hmn_includePkgDeps
        , Just unit <- dep_unit
        , let PackageName nameFS = unitPackageName unit
              name = unpackFS nameFS
              withLibName (PackageName c) = name ++ ":" ++ unpackFS c
              lname = maybe name withLibName (unitComponentName unit)
              newExtDep = ExtUnitDep
                { extUnitId = UnitInfo.unitId unit
                , extUnitName = lname
                , extUnitPackageId = DPackageId $ unitPackageId unit
                }
        ->
          ( Set.insert newExtDep extDeps
          , addImport (mkImportDep dep_mod dep_level dep_boot) node_data
          )

        | otherwise
        -> (extDeps, node_data)

      DepCpp {dep_path} ->
        (extDeps, addCpp dep_path node_data)

linkNodeJson :: DepLink -> [DepLinkNode] -> LinkNodeDep
linkNodeJson (DepLink uid linkType) deps =
  LinkNodeDep
    { linkNodeUid = uid
    , linkNodeType = linkType
    , linkNodeModuleDeps = allMods
    , linkNodeUnitDeps = units
    , linkNodeHomeUnitDeps = homeUids
    , linkNodeExtUnitDeps = extUids
    }
  where
    (extUids, homeUids, units, allMods) = foldl' go (Set.empty, Set.empty, Set.empty, Map.empty) deps

    go (ext, homeUid, unit, mods) = \ case
      DepLink_Module modName u ->
        let
          newMods = Map.insertWith (Set.union) u (Set.singleton modName) mods
        in
          (ext, homeUid, unit, newMods)
      DepLink_Unit uid ->
        (ext, homeUid, Set.insert uid unit, mods)
      DepLink_HomeUnit uid ->
        (ext, Set.insert uid homeUid, unit, mods)
      DepLink_ExtUnit uid ->
        (Set.insert uid ext, homeUid, unit, mods)

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
