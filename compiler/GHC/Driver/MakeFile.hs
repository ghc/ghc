

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
import GHC.Unit.Home.Graph (HomeUnitEnv (homeUnitEnv_dflags))
import GHC.Unit.Info
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

import Control.Applicative ((<|>))
import Control.Monad (guard, when)
import Data.Either
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Monoid qualified as Monoid
import Data.Semigroup qualified as Semigroup
import Data.Set qualified as Set
import GHC.Generics (Generic, Generically (..))
import System.Directory
import System.Directory qualified as Directory
import System.FilePath qualified as FilePath
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
doMkDependHS srcs = do
    -- Initialisation
    targets <- mapM (\s -> GHC.guessTarget s Nothing Nothing) srcs
    GHC.setTargets targets
    doMkDepend

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

doMkDependModuleGraph :: GhcMonad m =>  DynFlags -> ModuleGraph -> m ()
doMkDependModuleGraph dflags module_graph = do
    hsc_env <- getSession
    logger <- getLogger
    let
      tmpfs = hsc_tmpfs hsc_env
      excl_mods = depExcludeMods dflags

    root_ <- liftIO getCurrentDirectory
    root <- encodeUtf root_

    let
      backends = concat
        [ [ initFileDepWriter logger root tmpfs dflags makefile_output
          | Just makefile_output <- [depMakefile dflags]
          ]
        , [ initJsonDepWriter json_output dflags
          | Just json_output <- [depJson dflags]
          ]
        ]


    let sorted = GHC.topSortModuleGraph False module_graph Nothing
    -- Print out the dependencies if wanted
    liftIO $ debugTraceMsg logger 2 (text "Module dependencies" $$ ppr sorted)

    liftIO $ do

      -- Setup the writers
      writers <- sequenceA backends
      sinks <- traverse dw_beginWriter writers

      -- Do the actual work
      do
        -- Process them one by one, dumping results and complaining about cycles
        mapM_ (processDeps hsc_env (UniqSet.mkUniqSet excl_mods) sinks) sorted

        -- If -ddump-mod-cycles, show cycles in the module graph
        liftIO $ dumpModCycles logger module_graph

      -- Tidy up
      do
        traverse_ dw_endWriter writers


    -- Unconditional exiting is a bad idea.  If an error occurs we'll get an
    --exception; if that is not caught it's fine, but at least we have a
    --chance to find out exactly what went wrong.  Uncomment the following
    --line if you disagree.

    --`GHC.ghcCatch` \_ -> io $ exitWith (ExitFailure 1)

--------------------------------------------------------------------------------
-- Types abstracting over the output
--------------------------------------------------------------------------------

data DepNode =
  DepNode
    { dn_mod :: ModuleWithIsBoot
    , dn_src :: OsPath
    , dn_obj :: OsPath
    , dn_hi :: OsPath
    , dn_preprocessing :: PreprocessingNode
    }

data PreprocessingNode = PreprocessingNode
  { pn_preprocessor :: Maybe String
  , pn_options :: [String]
  }

data Dep
  = DepHi
    { dep_mod :: Module
    , dep_path :: OsPath
    , dep_unit :: Maybe UnitInfo
    , dep_local :: Bool
    , dep_level :: ImportLevel
    , dep_boot :: IsBootInterface
    }
  | DepCpp
    { dep_path :: OsPath
    }

data DependencyWriter = DependencyWriter
  { dw_beginWriter :: IO DepSink
  , dw_endWriter :: IO ()
  }

data DepSink = DepSink
  { ds_writeDependency :: DepNode -> [Dep] -> IO ()
  }

-----------------------------------------------------------------
--
--              processDeps
--
-----------------------------------------------------------------

processDeps :: HscEnv
            -> UniqSet ModuleName -- ^ Excludes
            -> [DepSink]
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

processDeps hsc_env _ _ (CyclicSCC nodes)
  =     -- There shouldn't be any cycles; report them
    throwOneError (initSourceErrorContext (hsc_dflags hsc_env)) $ cyclicModuleErr nodes

processDeps hsc_env _ _ (AcyclicSCC (InstantiationNode _uid node))
  =     -- There shouldn't be any backpack instantiations; report them as well
    throwOneError (initSourceErrorContext (hsc_dflags hsc_env)) $
      mkPlainErrorMsgEnvelope noSrcSpan $
      GhcDriverMessage $ DriverInstantiationNodeInDependencyGeneration node

processDeps _ _ _ (AcyclicSCC (LinkNode {})) = return ()
processDeps _ _ _ (AcyclicSCC (UnitNode {})) = return ()
processDeps _ _ _ (AcyclicSCC (ModuleNode _ (ModuleNodeFixed {})))
  -- No dependencies needed for fixed modules (already compiled)
  = return ()

processDeps hsc_env0 excl_mods sinks (AcyclicSCC (ModuleNode _ (ModuleNodeCompile node))) = do
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
      traverse_ (\ sink -> ds_writeDependency sink dep_node deps) sinks
    else do
      let sec = initSourceErrorContext (hsc_dflags local_hsc_env)
      throwErrors sec (mkMessages (listToBag missing_dep_errs))
  where
    -- Operations such import resolution depend on the currently active home unit id.
    -- With multiple home units, each module may be from a different home unit with
    -- separate dependencies and options.
    -- Thus, we need to set the active unit id to the one of the module we are processing.
    local_hsc_env = hscSetActiveUnitId (ms_unitid node) hsc_env0
    unit_dflags = hsc_dflags local_hsc_env
    dflags = ms_hspp_opts node
    src_file = msHsFileOsPath node
    mkDepNode preproc =
      DepNode
        { dn_mod = GWIB (ms_mod node) (isBootSummary node)
        , dn_src = src_file
        , dn_obj = msObjFileOsPath node
        , dn_hi = msHiFileOsPath node
        , dn_preprocessing = preproc
        }

    preprocessor :: IO PreprocessingNode
    preprocessor
      | Just src <- ml_hs_file (ms_location node)
      = runPipeline (hsc_hooks local_hsc_env) $ do
        let
          (_, suffix) = FilePath.splitExtension src
          lit | Unlit _ <- startPhase suffix = True
              | otherwise = False
          pipe_env = mkPipeEnv StopPreprocess src Nothing NoOutputFile
        unlit_fn <- if lit then use (T_Unlit pipe_env local_hsc_env src) else pure src
        (dflags1, opts, _, _) <- use (T_FileArgs (hsc_logger local_hsc_env) (ms_hspp_opts node) unlit_fn)
        let pp = find_preprocessor dflags1
        pure PreprocessingNode
          { pn_preprocessor = pp <|> find_preprocessor dflags
          , pn_options = opts
          }
      | otherwise
      = pure PreprocessingNode
          { pn_preprocessor = find_preprocessor dflags
          , pn_options = []
          }

    -- Emit a dependency for each CPP import
    -- CPP deps are discovered in the module parsing phase by parsing
    -- comment lines left by the preprocessor.
    -- Note that GHC.parseModule may throw an exception if the module
    -- fails to parse, which may not be desirable (see #16616).
    find_cpp_deps :: IO [Dep]
    find_cpp_deps =
      if depIncludeCppDeps unit_dflags
        then do
          session <- Session <$> newIORef local_hsc_env
          parsedMod <- reflectGhc (GHC.parseModule node) session
          pure (DepCpp . unsafeEncodeUtf <$> GHC.pm_extra_src_files parsedMod)
        else
          pure []

    -- Emit a dependency for each import
    find_import_deps :: [UnresolvedImport PkgQual] -> IO [Either (MsgEnvelope GhcMessage) Dep]
    find_import_deps idecls =
      sequence
        [ findDependency local_hsc_env decl
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
    L srcloc mod_name = ui_mod_name imp
    is_boot           = ui_boot imp

find_preprocessor :: DynFlags -> Maybe String
find_preprocessor d = do
  guard (gopt Opt_Pp d)
  let pp = pgm_F d
  guard (not $ null pp)
  Just pp

-----------------------------------------------------------------
--
--              beginMkDependHs
--      Create a temporary file,
--      find the Makefile,
--      slurp through it, etc
--
-----------------------------------------------------------------

initFileDepWriter :: Logger -> OsPath -> TmpFs -> DynFlags -> FilePath -> IO DependencyWriter
initFileDepWriter logger root tmpfs dflags makefile = do
  files <- beginMkDependHS logger tmpfs dflags makefile
  pure DependencyWriter
    { dw_beginWriter = do
        pure $ DepSink $ \ node deps -> do
          writeDependencies (depIncludePkgDeps dflags) root (mkd_tmp_hdl files) suffixes node deps
    , dw_endWriter = endMkDependHS logger files
    }
  where
    suffixes = map unsafeEncodeUtf (depSuffixes dflags)

data MkDepFiles
  = MkDep { mkd_make_file :: FilePath,          -- Name of the makefile
            mkd_make_hdl  :: Maybe Handle,      -- Handle for the open makefile
            mkd_tmp_file  :: FilePath,          -- Name of the temporary file
            mkd_tmp_hdl   :: Handle }           -- Handle of the open temporary file

beginMkDependHS :: Logger -> TmpFs -> DynFlags -> FilePath -> IO MkDepFiles
beginMkDependHS logger tmpfs dflags makefile = do
        -- open a new temp file in which to stuff the dependency info
        -- as we go along.
  tmp_file <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "dep"
  tmp_hdl <- openFile tmp_file WriteMode

        -- open the makefile
  exists <- Directory.doesFileExist makefile
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

writeDependencies ::
  Bool ->
  OsPath ->
  Handle ->
  [OsString] ->
  -- ^ Suffixes
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
    boot_dep = case gwib_isBoot dn_mod of
      IsBoot -> [([obj], hi) | (obj, hi) <- zip (suffixed (removeBootSuffix dn_obj)) (suffixed dn_hi)]
      NotBoot -> []

    -- Add one dependency for each suffix;
    -- e.g.         A.o   : B.hi
    --              A.x_o : B.x_hi
    import_dep = \case
      DepHi {dep_path, dep_local}
        | dep_local || include_pkgs
        -> [([obj], hi) | (obj, hi) <- zip obj_files (suffixed dep_path)]

        | otherwise
        -> []

      DepCpp {dep_path} -> [(obj_files, dep_path)]

    write (from, to) = writeDependency root hdl from to

    obj_files = suffixed dn_obj

    suffixed f = insertSuffixes f suffixes

    DepNode {dn_src, dn_obj, dn_hi, dn_mod} = node

-----------------------------
writeDependency :: OsPath -> Handle -> [OsPath] -> OsPath -> IO ()
-- (writeDependency r h [t1,t2] dep) writes to handle h the dependency
--      t1 t2 : dep
writeDependency root hdl targets dep
  = do let -- We need to avoid making deps on
           --     c:/foo/...
           -- on Windows as make gets confused by the :
           -- Making relative deps avoids some instances of this.
           dep' = OsPath.makeRelative root dep
           forOutput = escapeSpaces . reslash Forwards . unsafeDecodeUtf . OsPath.normalise
           output = unwords (map forOutput targets) ++ " : " ++ forOutput dep'
       hPutStrLn hdl output

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

initJsonDepWriter :: FilePath -> DynFlags -> IO DependencyWriter
initJsonDepWriter output dflags = do
  json_var <- mkJsonOutput output initDepJson
  pure DependencyWriter
    { dw_beginWriter =
        pure $ DepSink $ \ node deps ->
          updateJson json_var (updateDepJson (depIncludePkgDeps dflags) node deps)
    , dw_endWriter =
        writeJsonOutput json_var
    }

--------------------------------------------------------------------------------
-- Output interface for json dumps

-- | Resources for a json dump option, used in "GHC.Driver.MakeFile".
-- The flag @-dep-json@ add an additional output target for dependency
-- diagnostics.
data JsonOutput a =
  JsonOutput {
    -- | This ref is updated in @processDeps@ incrementally, using a
    -- flag-specific type.
    json_ref :: IORef a,

    -- | The output file path specified as argument to the flag.
    json_path :: FilePath
  }

-- | TODO: @fendor
mkJsonOutput ::
  FilePath ->
  IO (IORef a) ->
  IO (JsonOutput a)
mkJsonOutput json_path mk_ref = do
  json_ref <- mk_ref
  pure JsonOutput {json_ref, json_path}

-- | Update the dump data in 'json_ref' if the output target is present.
updateJson :: JsonOutput a -> (a -> a) -> IO ()
updateJson JsonOutput {json_ref} f = modifyIORef' json_ref f

-- | Write a json object to the flag-dependent file if the output target is
-- present.
writeJsonOutput ::
  ToJson a =>
  JsonOutput a ->
  IO ()
writeJsonOutput JsonOutput {json_ref, json_path} = do
  payload <- readIORef json_ref
  writeJsonFile payload json_path

--------------------------------------------------------------------------------
-- Output helpers

writeJsonFile :: ToJson a => a -> FilePath -> IO ()
writeJsonFile doc p = do
  withAtomicRename p
    $ \tmp -> writeFile tmp $ showSDocUnsafe $ renderJSON $ json doc

--------------------------------------------------------------------------------
-- Payload for -dep-json

newtype DPackageId = DPackageId PackageId
  deriving newtype (Eq)

instance Ord DPackageId where
  DPackageId (PackageId d1) `compare` DPackageId (PackageId d2) = d1 `lexicalCompareFS` d2

data ModuleNodeDeps = ModuleNodeDeps
  { source :: OsPath
  , imports :: ImportDeps
  , cpp :: Set.Set OsPath
  , options :: [String]
  , preprocessor :: Maybe FilePath
  }
  deriving stock (Generic)

addImport :: ImportDep -> ModuleNodeDeps -> ModuleNodeDeps
addImport import_dep mod_node_deps =
  mod_node_deps
    { imports = ImportDeps $ Map.insertWith Set.union uid (Set.singleton import_dep) (getImports $ imports mod_node_deps)
    }
  where
    uid = toUnitId $ moduleUnit $ imp_mod import_dep

addCpp :: OsPath -> ModuleNodeDeps -> ModuleNodeDeps
addCpp p mod_node_deps =
  mod_node_deps
    { cpp = Set.insert p (cpp mod_node_deps)
    }

newtype ImportDeps = ImportDeps
  { getImports :: Map.Map UnitId (Set.Set ImportDep)
  }
  deriving stock (Generic)

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

data HomeUnitDeps = HomeUnitDeps
  { homeModules :: Map.Map ModuleWithIsBoot ModuleNodeDeps
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (Generically HomeUnitDeps)

data ExtUnitDep = ExtUnitDep
  { extUnitId :: UnitId
  -- ^ The 'UnitId' of a unit. This is assumed to be globally unique.
  , extUnitName :: String
  , extUnitPackageId :: DPackageId
  } deriving (Eq, Ord)

data DepJson = DepJson
  { homeUnitDeps :: Map.Map UnitId HomeUnitDeps
  , externalDeps :: Set.Set ExtUnitDep
  }

{- Note [ghc -M -dep-json output format]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The instance of 'ToJson' 'DepJson' must conform to the JSON schema
specified in docs/users_guide/make-depends-json-schema-1_0.json.
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
instance ToJson DepJson where
  json (DepJson homeUnits extDeps) =
    JSObject $
      [ homeUnit huid hud
      | (huid, hud) <- Map.toList homeUnits
      ] ++
      [ mkExtDepObj dep
      | dep <- Set.toList extDeps
      ]
    where
      homeUnit uid hud =
        ( unitIdString uid
        , JSObject
            [
              ( "modules"
              , JSObject
                  [ (homeUnitModuleKey modl, homeUnitModule deps)
                  | (modl, deps) <- Map.toList $ homeModules hud
                  ]
              )
            ]
        )

      homeUnitModuleKey :: ModuleWithIsBoot -> String
      homeUnitModuleKey t = moduleKey (gwib_mod t) (gwib_isBoot t)

      moduleKey :: Module -> IsBootInterface -> String
      moduleKey t b = moduleNameString (moduleName t) ++ case b of
        IsBoot -> "[boot]"
        NotBoot -> ""

      homeUnitModule :: ModuleNodeDeps -> JsonDoc
      homeUnitModule ModuleNodeDeps {source, imports, cpp, options, preprocessor} =
        JSObject
          [ ("source", JSString . showOsPath $ normalise source)
          , ("imports", importsObj imports)
          , ("includes", strArray cpp showOsPath)
          , ("options", JSArray $ map JSString options)
          , ("preprocessor", maybe JSNull JSString preprocessor)
          ]

      importsObj :: ImportDeps -> JsonDoc
      importsObj importDeps = JSObject $ map importObj (Map.toList $ getImports importDeps)

      importObj :: (UnitId, Set.Set ImportDep) -> (String, JsonDoc)
      importObj (uid, importSet) =
        (unitIdString uid, array importSet importDepObj)

      importDepObj :: ImportDep -> JsonDoc
      importDepObj ImportDep {imp_mod, imp_level, imp_isBoot}
        | NormalLevel <- imp_level = JSString (moduleKey imp_mod imp_isBoot)
        | otherwise = JSObject
          [ ("module", JSString (moduleKey imp_mod imp_isBoot))
          , ("level" , importLevel imp_level)
          ]

      importLevel :: ImportLevel -> JsonDoc
      importLevel lvl =
        JSString $ case lvl of
          NormalLevel -> "normal"
          SpliceLevel -> "splice"
          QuoteLevel -> "quote"

      mkExtDepObj :: ExtUnitDep -> (String, JsonDoc)
      mkExtDepObj ExtUnitDep{..} =
        ( unitIdString extUnitId
        , JSObject
            [ ("package-name", JSString extUnitName)
            , ("package-id", JSString $ showPackageId extUnitPackageId)
            ]
        )

      array values render = JSArray (fmap render (Set.toList values))
      strArray values render = JSArray (fmap (JSString . render) (Set.toList values))

      showOsPath = unsafeDecodeUtf
      showPackageId (DPackageId (PackageId pid)) = unpackFS pid

initDepJson :: IO (IORef DepJson)
initDepJson = newIORef $ DepJson Map.empty Set.empty

insertDepJson :: ModuleWithIsBoot -> ModuleNodeDeps -> Set.Set ExtUnitDep -> DepJson -> DepJson
insertDepJson target modNode extUnits (DepJson m0 e0) =
  DepJson
    { homeUnitDeps =
        Map.insertWith
          (Semigroup.<>)
          modUnitId
          (HomeUnitDeps $ Map.singleton target modNode)
          m0
    , externalDeps =
        Set.union e0 extUnits
    }
  where
    modUnitId = toUnitId $ moduleUnit $ gwib_mod target

updateDepJson :: Bool -> DepNode -> [Dep] -> DepJson -> DepJson
updateDepJson include_pkgs DepNode {..} deps =
  insertDepJson dn_mod payload externalDeps
  where
    (externalDeps, payload) = foldl' go (Set.empty, initial_node_data) deps

    initial_node_data =
      ModuleNodeDeps
        { source = dn_src
        , preprocessor = pn_preprocessor dn_preprocessing
        , options = pn_options dn_preprocessing
        , cpp = Set.empty
        , imports = ImportDeps Map.empty
        }

    go (extDeps, node_data) = \ case
      DepHi {dep_mod, dep_local, dep_unit, dep_boot, dep_level}
        | dep_local
        -> (extDeps, addImport (mkImportDep dep_mod dep_level dep_boot) node_data)

        | include_pkgs
        , Just unit <- dep_unit
        , let PackageName nameFS = unitPackageName unit
              name = unpackFS nameFS
              withLibName (PackageName c) = name ++ ":" ++ unpackFS c
              lname = maybe name withLibName (unitComponentName unit)
              newExtDep = ExtUnitDep
                { extUnitId = unitId unit
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
