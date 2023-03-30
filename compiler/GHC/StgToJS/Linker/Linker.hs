{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Linker
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- GHCJS linker, collects dependencies from the object files
-- which contain linkable units with dependency information
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Linker
  ( jsLinkBinary
  , embedJsFile
  )
where

import Prelude

import GHC.Platform.Host (hostPlatformArchOS)

import GHC.JS.Make
import GHC.JS.Unsat.Syntax
import qualified GHC.JS.Syntax as Sat
import GHC.JS.Transform

import GHC.Driver.Session (DynFlags(..))
import Language.Haskell.Syntax.Module.Name
import GHC.SysTools.Cpp
import GHC.SysTools

import GHC.Linker.Static.Utils (exeFileName)

import GHC.StgToJS.Linker.Types
import GHC.StgToJS.Linker.Utils
import GHC.StgToJS.Rts.Rts
import GHC.StgToJS.Object
import GHC.StgToJS.Types hiding (LinkableUnit)
import GHC.StgToJS.Symbols
import GHC.StgToJS.Printer
import GHC.StgToJS.Arg
import GHC.StgToJS.Closure

import GHC.Unit.State
import GHC.Unit.Env
import GHC.Unit.Home
import GHC.Unit.Types
import GHC.Unit.Module (moduleStableString)

import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Logger (Logger, logVerbAtLeast)
import GHC.Utils.Binary
import qualified GHC.Utils.Ppr as Ppr
import GHC.Utils.Monad
import GHC.Utils.TmpFs

import GHC.Types.Unique.Set

import qualified GHC.SysTools.Ar          as Ar

import qualified GHC.Data.ShortText as ST
import GHC.Data.FastString

import Control.Concurrent.MVar
import Control.Monad

import Data.Array
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString          as BS
import Data.Function            (on)
import Data.IntSet              (IntSet)
import qualified Data.IntSet              as IS
import Data.IORef
import Data.List  ( partition, nub, intercalate, sort
                  , groupBy, intersperse,
                  )
import qualified Data.List.NonEmpty       as NE
import Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import Data.Maybe
import Data.Set                 (Set)
import qualified Data.Set                 as S
import Data.Word

import System.IO
import System.FilePath ((<.>), (</>), dropExtension, takeDirectory)
import System.Directory ( createDirectoryIfMissing
                        , doesFileExist
                        , getCurrentDirectory
                        , Permissions(..)
                        , setPermissions
                        , getPermissions
                        )

data LinkerStats = LinkerStats
  { bytesPerModule     :: !(Map Module Word64) -- ^ number of bytes linked per module
  , packedMetaDataSize :: !Word64              -- ^ number of bytes for metadata
  }

newtype ArchiveState = ArchiveState { loadedArchives :: IORef (Map FilePath Ar.Archive) }

emptyArchiveState :: IO ArchiveState
emptyArchiveState = ArchiveState <$> newIORef M.empty

jsLinkBinary
  :: JSLinkConfig
  -> StgToJSConfig
  -> [FilePath]
  -> Logger
  -> DynFlags
  -> UnitEnv
  -> [FilePath]
  -> [UnitId]
  -> IO ()
jsLinkBinary lc_cfg cfg js_srcs logger dflags u_env objs dep_pkgs
  | lcNoJSExecutables lc_cfg = return ()
  | otherwise = do
    -- additional objects to link are passed as FileOption ldInputs...
    let cmdline_objs = [ f | FileOption _ f <- ldInputs dflags ]
    -- discriminate JavaScript sources from real object files.
    (cmdline_js_srcs, cmdline_js_objs) <- partitionM isJsFile cmdline_objs
    let
        objs'    = map ObjFile (objs ++ cmdline_js_objs)
        js_srcs' = js_srcs ++ cmdline_js_srcs
        isRoot _ = True
        exe      = jsExeFileName dflags

    void $ link lc_cfg cfg logger u_env exe mempty dep_pkgs objs' js_srcs' isRoot mempty

-- | link and write result to disk (jsexe directory)
link :: JSLinkConfig
     -> StgToJSConfig
     -> Logger
     -> UnitEnv
     -> FilePath               -- ^ output file/directory
     -> [FilePath]             -- ^ include path for home package
     -> [UnitId]               -- ^ packages to link
     -> [LinkedObj]            -- ^ the object files we're linking
     -> [FilePath]             -- ^ extra js files to include
     -> (ExportedFun -> Bool)  -- ^ functions from the objects to use as roots (include all their deps)
     -> Set ExportedFun        -- ^ extra symbols to link in
     -> IO ()
link lc_cfg cfg logger unit_env out _include units objFiles jsFiles isRootFun extraStaticDeps = do

      -- create output directory
      createDirectoryIfMissing False out

      -------------------------------------------------------------
      -- link all Haskell code (program + dependencies) into out.js

      -- compute dependencies
      (dep_map, dep_units, all_deps, _rts_wired_functions, dep_archives)
        <- computeLinkDependencies cfg logger out unit_env units objFiles extraStaticDeps isRootFun

      -- retrieve code for dependencies
      mods <- collectDeps dep_map dep_units all_deps

      -- LTO + rendering of JS code
      link_stats <- withBinaryFile (out </> "out.js") WriteMode $ \h ->
        renderLinker h mods jsFiles

      -------------------------------------------------------------

      -- dump foreign references file (.frefs)
      unless (lcOnlyOut lc_cfg) $ do
        let frefsFile  = "out.frefs"
            -- frefs      = concatMap mc_frefs mods
            jsonFrefs  = mempty -- FIXME: toJson frefs

        BL.writeFile (out </> frefsFile <.> "json") jsonFrefs
        BL.writeFile (out </> frefsFile <.> "js")
                     ("h$checkForeignRefs(" <> jsonFrefs <> ");")

      -- dump stats
      unless (lcNoStats lc_cfg) $ do
        let statsFile = "out.stats"
        writeFile (out </> statsFile) (renderLinkerStats link_stats)

      -- link generated RTS parts into rts.js
      unless (lcNoRts lc_cfg) $ do
        BL.writeFile (out </> "rts.js") ( BLC.pack rtsDeclsText
                                         <> BLC.pack (rtsText cfg))

      -- link dependencies' JS files into lib.js
      withBinaryFile (out </> "lib.js") WriteMode $ \h -> do
        forM_ dep_archives $ \archive_file -> do
          Ar.Archive entries <- Ar.loadAr archive_file
          forM_ entries $ \entry -> do
            case getJsArchiveEntry entry of
              Nothing -> return ()
              Just bs -> do
                B.hPut   h bs
                hPutChar h '\n'

      -- link everything together into all.js
      when (generateAllJs lc_cfg) $ do
        _ <- combineFiles lc_cfg out
        writeHtml    out
        writeRunMain out
        writeRunner lc_cfg out
        writeExterns out


computeLinkDependencies
  :: StgToJSConfig
  -> Logger
  -> String
  -> UnitEnv
  -> [UnitId]
  -> [LinkedObj]
  -> Set ExportedFun
  -> (ExportedFun -> Bool)
  -> IO (Map Module (Deps, DepsLocation), [UnitId], Set LinkableUnit, Set ExportedFun, [FilePath])
computeLinkDependencies cfg logger target unit_env units objFiles extraStaticDeps isRootFun = do

  (objDepsMap, objRequiredUnits) <- loadObjDeps objFiles

  let roots    = S.fromList . filter isRootFun $ concatMap (M.keys . depsHaskellExported . fst) (M.elems objDepsMap)
      rootMods = map (moduleNameString . moduleName . NE.head) . NE.group . sort . map funModule . S.toList $ roots
      objPkgs  = map moduleUnitId $ nub (M.keys objDepsMap)

  when (logVerbAtLeast logger 2) $ void $ do
    compilationProgressMsg logger $ hcat
      [ text "Linking ", text target, text " (", text (intercalate "," rootMods), char ')' ]
    compilationProgressMsg logger $ hcat
      [ text "objDepsMap ", ppr objDepsMap ]
    compilationProgressMsg logger $ hcat
      [ text "objFiles ", ppr objFiles ]

  let (rts_wired_units, rts_wired_functions) = rtsDeps units

  -- all the units we want to link together, without their dependencies
  let root_units = filter (/= mainUnitId)
                   $ nub
                   $ rts_wired_units ++ reverse objPkgs ++ reverse units

  -- all the units we want to link together, including their dependencies,
  -- preload units, and backpack instantiations
  all_units_infos <- mayThrowUnitErr (preloadUnitsInfo' unit_env root_units)

  let all_units = fmap unitId all_units_infos

  dep_archives <- getPackageArchives cfg unit_env all_units
  env <- newGhcjsEnv
  (archsDepsMap, archsRequiredUnits) <- loadArchiveDeps env dep_archives

  when (logVerbAtLeast logger 2) $
    logInfo logger $ hang (text "Linking with archives:") 2 (vcat (fmap text dep_archives))

  -- compute dependencies
  let dep_units      = all_units ++ [homeUnitId (ue_unsafeHomeUnit $ unit_env)]
      dep_map        = objDepsMap `M.union` archsDepsMap
      excluded_units = S.empty
      dep_fun_roots  = roots `S.union` rts_wired_functions `S.union` extraStaticDeps
      dep_unit_roots = archsRequiredUnits ++ objRequiredUnits

  all_deps <- getDeps (fmap fst dep_map) excluded_units dep_fun_roots dep_unit_roots

  when (logVerbAtLeast logger 2) $
    logInfo logger $ hang (text "Units to link:") 2 (vcat (fmap ppr dep_units))
    -- logInfo logger $ hang (text "All deps:") 2 (vcat (fmap ppr (S.toList all_deps)))

  return (dep_map, dep_units, all_deps, rts_wired_functions, dep_archives)


-- | Compiled module
data ModuleCode = ModuleCode
  { mc_module   :: !Module
  , mc_js_code  :: !Sat.JStat
  , mc_exports  :: !B.ByteString        -- ^ rendered exports
  , mc_closures :: ![ClosureInfo]
  , mc_statics  :: ![StaticInfo]
  , mc_frefs    :: ![ForeignJSRef]
  }

-- | ModuleCode after link with other modules.
--
-- It contains less information than ModuleCode because they have been commoned
-- up into global "metadata" for the whole link.
data CompactedModuleCode = CompactedModuleCode
  { cmc_module  :: !Module
  , cmc_js_code :: !Sat.JStat
  , cmc_exports :: !B.ByteString        -- ^ rendered exports
  }

-- | Link modules and pretty-print them into the given Handle
renderLinker
  :: Handle
  -> [ModuleCode] -- ^ linked code per module
  -> [FilePath]   -- ^ additional JS files
  -> IO LinkerStats
renderLinker h mods jsFiles = do

  -- link modules
  let (compacted_mods, meta) = linkModules mods

  let
    putBS   = B.hPut h
    putJS x = do
      before <- hTell h
      Ppr.printLeftRender h (pretty x)
      hPutChar h '\n'
      after <- hTell h
      pure $! (after - before)

  ---------------------------------------------------------
  -- Pretty-print JavaScript code for all the dependencies.
  --
  -- We have to pretty-print at link time because we want to be able to perform
  -- global link-time optimisations (e.g. renamings) on the whole generated JS
  -- file.

  -- modules themselves
  mod_sizes <- forM compacted_mods $ \m -> do
    !mod_size <- fromIntegral <$> putJS (cmc_js_code m)
    let !mod_mod  = cmc_module m
    pure (mod_mod, mod_size)

  -- commoned up metadata
  !meta_length <- fromIntegral <$> putJS (satJStat meta)

  -- module exports
  mapM_ (putBS . cmc_exports) compacted_mods

  -- explicit additional JS files
  mapM_ (\i -> B.readFile i >>= putBS) jsFiles

  -- stats
  let link_stats = LinkerStats
        { bytesPerModule     = M.fromList mod_sizes
        , packedMetaDataSize = meta_length
        }

  pure link_stats

-- | Render linker stats
renderLinkerStats :: LinkerStats -> String
renderLinkerStats s =
  intercalate "\n\n" [meta_stats, package_stats, module_stats] <> "\n\n"
  where
    meta = packedMetaDataSize s
    meta_stats = "number of modules: " <> show (length bytes_per_mod)
                 <> "\npacked metadata:   " <> show meta

    bytes_per_mod = M.toList $ bytesPerModule s

    show_unit (UnitId fs) = unpackFS fs

    ps :: Map UnitId Word64
    ps = M.fromListWith (+) . map (\(m,s) -> (moduleUnitId m,s)) $ bytes_per_mod

    pad :: Int -> String -> String
    pad n t = let l = length t
              in  if l < n then t <> replicate (n-l) ' ' else t

    pkgMods :: [[(Module,Word64)]]
    pkgMods = groupBy ((==) `on` (moduleUnitId . fst)) bytes_per_mod

    showMod :: (Module, Word64) -> String
    showMod (m,s) = pad 40 ("    " <> moduleStableString m <> ":") <> show s <> "\n"

    package_stats :: String
    package_stats = "code size summary per package (in bytes):\n\n"
                     <> concatMap (\(p,s) -> pad 25 (show_unit p <> ":") <> show s <> "\n") (M.toList ps)

    module_stats :: String
    module_stats = "code size per module (in bytes):\n\n" <> unlines (map (concatMap showMod) pkgMods)


getPackageArchives :: StgToJSConfig -> UnitEnv -> [UnitId] -> IO [FilePath]
getPackageArchives cfg unit_env units =
  filterM doesFileExist [ ST.unpack p </> "lib" ++ ST.unpack l ++ profSuff <.> "a"
                        | u <- units
                        , p <- getInstalledPackageLibDirs ue_state u
                        , l <- getInstalledPackageHsLibs  ue_state u
                        ]
  where
    ue_state = ue_units unit_env

    -- XXX the profiling library name is probably wrong now
    profSuff | csProf cfg = "_p"
             | otherwise  = ""


-- | Combine rts.js, lib.js, out.js to all.js that can be run
-- directly with node.js or SpiderMonkey jsshell
combineFiles :: JSLinkConfig
             -> FilePath
             -> IO ()
combineFiles cfg fp = do
  let files = map (fp </>) ["rts.js", "lib.js", "out.js"]
  withBinaryFile (fp </> "all.js") WriteMode $ \h -> do
    let cpy i = B.readFile i >>= B.hPut h
    mapM_ cpy files
    unless (lcNoHsMain cfg) $ do
      B.hPut h runMainJS

-- | write the index.html file that loads the program if it does not exit
writeHtml
  :: FilePath -- ^ output directory
  -> IO ()
writeHtml out = do
  let htmlFile = out </> "index.html"
  e <- doesFileExist htmlFile
  unless e $
    B.writeFile htmlFile templateHtml


templateHtml :: B.ByteString
templateHtml =
  "<!DOCTYPE html>\n\
  \<html>\n\
  \  <head>\n\
  \  </head>\n\
  \  <body>\n\
  \  </body>\n\
  \  <script language=\"javascript\" src=\"all.js\" defer></script>\n\
  \</html>"

-- | write the runmain.js file that will be run with defer so that it runs after
-- index.html is loaded
writeRunMain
  :: FilePath -- ^ output directory
  -> IO ()
writeRunMain out = do
  let runMainFile = out </> "runmain.js"
  e <- doesFileExist runMainFile
  unless e $
    B.writeFile runMainFile runMainJS

runMainJS :: B.ByteString
runMainJS = "h$main(h$mainZCZCMainzimain);\n"

writeRunner :: JSLinkConfig -- ^ Settings
            -> FilePath     -- ^ Output directory
            -> IO ()
writeRunner _settings out = do
  cd    <- getCurrentDirectory
  let arch_os = hostPlatformArchOS
  let runner  = cd </> exeFileName arch_os False (Just (dropExtension out))
      srcFile = out </> "all" <.> "js"
      nodePgm :: B.ByteString
      nodePgm = "node"
  src <- B.readFile (cd </> srcFile)
  B.writeFile runner ("#!/usr/bin/env " <> nodePgm <> "\n" <> src)
  perms <- getPermissions runner
  setPermissions runner (perms {executable = True})

rtsExterns :: FastString
rtsExterns =
  "// GHCJS RTS externs for closure compiler ADVANCED_OPTIMIZATIONS\n\n" <>
  mconcat (map (\x -> "/** @type {*} */\nObject.d" <> mkFastString (show x) <> ";\n")
               [(7::Int)..16384])

writeExterns :: FilePath -> IO ()
writeExterns out = writeFile (out </> "all.js.externs")
  $ unpackFS rtsExterns

-- | get all dependencies for a given set of roots
getDeps :: Map Module Deps  -- ^ loaded deps
        -> Set LinkableUnit -- ^ don't link these blocks
        -> Set ExportedFun  -- ^ start here
        -> [LinkableUnit]   -- ^ and also link these
        -> IO (Set LinkableUnit)
getDeps loaded_deps base fun startlu = go' S.empty (S.fromList startlu) (S.toList fun)
  where
    go :: Set LinkableUnit
       -> Set LinkableUnit
       -> IO (Set LinkableUnit)
    go result open = case S.minView open of
      Nothing -> return result
      Just (lu@(lmod,n), open') ->
          case M.lookup lmod loaded_deps of
            Nothing -> pprPanic "getDeps.go: object file not loaded for:  " (pprModule lmod)
            Just (Deps _ _ _ b) ->
              let block = b!n
                  result' = S.insert lu result
              in go' result'
                 (addOpen result' open' $
                   map (lmod,) (blockBlockDeps block)) (blockFunDeps block)

    go' :: Set LinkableUnit
        -> Set LinkableUnit
        -> [ExportedFun]
        -> IO (Set LinkableUnit)
    go' result open [] = go result open
    go' result open (f:fs) =
        let key = funModule f
        in  case M.lookup key loaded_deps of
              Nothing -> pprPanic "getDeps.go': object file not loaded for:  " $ pprModule key
              Just (Deps _m _r e _b) ->
                 let lun :: Int
                     lun = fromMaybe (pprPanic "exported function not found: " $ ppr f)
                                     (M.lookup f e)
                     lu  = (key, lun)
                 in  go' result (addOpen result open [lu]) fs

    addOpen :: Set LinkableUnit -> Set LinkableUnit -> [LinkableUnit]
            -> Set LinkableUnit
    addOpen result open newUnits =
      let alreadyLinked s = S.member s result ||
                            S.member s open   ||
                            S.member s base
      in  open `S.union` S.fromList (filter (not . alreadyLinked) newUnits)

-- | collect dependencies for a set of roots
collectDeps :: Map Module (Deps, DepsLocation) -- ^ Dependency map
            -> [UnitId]                        -- ^ packages, code linked in this order
            -> Set LinkableUnit                -- ^ All dependencides
            -> IO [ModuleCode]
collectDeps mod_deps packages all_deps = do

  -- read ghc-prim first, since we depend on that for static initialization
  let packages' = uncurry (++) $ partition (== primUnitId) (nub packages)

      units_by_module :: Map Module IntSet
      units_by_module = M.fromListWith IS.union $
                      map (\(m,n) -> (m, IS.singleton n)) (S.toList all_deps)

      mod_deps_bypkg :: Map UnitId [(Deps, DepsLocation)]
      mod_deps_bypkg = M.fromListWith (++)
                        (map (\(m,v) -> (moduleUnitId m,[v])) (M.toList mod_deps))

  ar_state <- emptyArchiveState
  fmap (catMaybes . concat) . forM packages' $ \pkg ->
    mapM (uncurry $ extractDeps ar_state units_by_module)
         (fromMaybe [] $ M.lookup pkg mod_deps_bypkg)

extractDeps :: ArchiveState
            -> Map Module IntSet
            -> Deps
            -> DepsLocation
            -> IO (Maybe ModuleCode)
extractDeps ar_state units deps loc =
  case M.lookup mod units of
    Nothing       -> return Nothing
    Just mod_units -> Just <$> do
      let selector n _  = fromIntegral n `IS.member` mod_units || isGlobalUnit (fromIntegral n)
      case loc of
        ObjectFile fp -> do
          us <- readObjectUnits fp selector
          pure (collectCode us)
        ArchiveFile a -> do
          obj <- readArObject ar_state mod a
          us <- getObjectUnits obj selector
          pure (collectCode us)
        InMemory _n obj -> do
          us <- getObjectUnits obj selector
          pure (collectCode us)
  where
    mod           = depsModule deps
    newline       = BC.pack "\n"
    mk_exports    = mconcat . intersperse newline . filter (not . BS.null) . map oiRaw
    mk_js_code    = mconcat . map oiStat
    collectCode l = ModuleCode
                      { mc_module   = mod
                      , mc_js_code  = mk_js_code l
                      , mc_exports  = mk_exports l
                      , mc_closures = concatMap oiClInfo l
                      , mc_statics  = concatMap oiStatic l
                      , mc_frefs    = concatMap oiFImports l
                      }

readArObject :: ArchiveState -> Module -> FilePath -> IO Object
readArObject ar_state mod ar_file = do
  loaded_ars <- readIORef (loadedArchives ar_state)
  (Ar.Archive entries) <- case M.lookup ar_file loaded_ars of
    Just a -> pure a
    Nothing -> do
      a <- Ar.loadAr ar_file
      modifyIORef (loadedArchives ar_state) (M.insert ar_file a)
      pure a

  -- look for the right object in archive
  let go_entries = \case
        -- XXX this shouldn't be an exception probably
        [] -> panic $ "could not find object for module "
                      ++ moduleNameString (moduleName mod)
                      ++ " in "
                      ++ ar_file

        (e:es) -> do
          let bs = Ar.filedata e
          bh <- unsafeUnpackBinBuffer bs
          getObjectHeader bh >>= \case
            Left _         -> go_entries es -- not a valid object entry
            Right mod_name
              | mod_name /= moduleName mod
              -> go_entries es -- not the module we're looking for
              | otherwise
              -> getObjectBody bh mod_name -- found it

  go_entries entries


-- | A helper function to read system dependencies that are hardcoded
diffDeps
  :: [UnitId]                    -- ^ Packages that are already Linked
  -> ([UnitId], Set ExportedFun) -- ^ New units and functions to link
  -> ([UnitId], Set ExportedFun) -- ^ Diff
diffDeps pkgs (deps_pkgs,deps_funs) =
  ( filter   linked_pkg deps_pkgs
  , S.filter linked_fun deps_funs
  )
  where
    linked_fun f = moduleUnitId (funModule f) `S.member` linked_pkgs
    linked_pkg p = S.member p linked_pkgs
    linked_pkgs  = S.fromList pkgs

-- | dependencies for the RTS, these need to be always linked
rtsDeps :: [UnitId] -> ([UnitId], Set ExportedFun)
rtsDeps pkgs = diffDeps pkgs $
  ( [baseUnitId, primUnitId]
  , S.fromList $ concat
      [ mkBaseFuns "GHC.Conc.Sync"
          ["reportError"]
      , mkBaseFuns "Control.Exception.Base"
          ["nonTermination"]
      , mkBaseFuns "GHC.Exception.Type"
          [ "SomeException"
          , "underflowException"
          , "overflowException"
          , "divZeroException"
          ]
      , mkBaseFuns "GHC.TopHandler"
          [ "runMainIO"
          , "topHandler"
          ]
      , mkBaseFuns "GHC.Base"
          ["$fMonadIO"]
      , mkBaseFuns "GHC.Maybe"
          [ "Nothing"
          , "Just"
          ]
      , mkBaseFuns "GHC.Ptr"
          ["Ptr"]
      , mkBaseFuns "GHC.JS.Prim"
          [ "JSVal"
          , "JSException"
          , "$fShowJSException"
          , "$fExceptionJSException"
          , "resolve"
          , "resolveIO"
          , "toIO"
          ]
      , mkBaseFuns "GHC.JS.Prim.Internal"
          [ "wouldBlock"
          , "blockedIndefinitelyOnMVar"
          , "blockedIndefinitelyOnSTM"
          , "ignoreException"
          , "setCurrentThreadResultException"
          , "setCurrentThreadResultValue"
          ]
      , mkPrimFuns "GHC.Types"
          [ ":"
          , "[]"
          ]
      , mkPrimFuns "GHC.Tuple.Prim"
          [ "(,)"
          , "(,,)"
          , "(,,,)"
          , "(,,,,)"
          , "(,,,,,)"
          , "(,,,,,,)"
          , "(,,,,,,,)"
          , "(,,,,,,,,)"
          , "(,,,,,,,,,)"
          ]
      ]
  )

-- | Export the functions in base
mkBaseFuns :: FastString -> [FastString] -> [ExportedFun]
mkBaseFuns = mkExportedFuns baseUnitId

-- | Export the Prim functions
mkPrimFuns :: FastString -> [FastString] -> [ExportedFun]
mkPrimFuns = mkExportedFuns primUnitId

-- | Given a @UnitId@, a module name, and a set of symbols in the module,
-- package these into an @ExportedFun@.
mkExportedFuns :: UnitId -> FastString -> [FastString] -> [ExportedFun]
mkExportedFuns uid mod_name symbols = map mk_fun symbols
  where
    mod        = mkModule (RealUnit (Definite uid)) (mkModuleNameFS mod_name)
    mk_fun sym = ExportedFun mod (LexicalFastString (mkJsSymbol True mod sym))

-- | read all dependency data from the to-be-linked files
loadObjDeps :: [LinkedObj] -- ^ object files to link
            -> IO (Map Module (Deps, DepsLocation), [LinkableUnit])
loadObjDeps objs = (prepareLoadedDeps . catMaybes) <$> mapM readDepsFromObj objs

-- | Load dependencies for the Linker from Ar
loadArchiveDeps :: GhcjsEnv
                -> [FilePath]
                -> IO ( Map Module (Deps, DepsLocation)
                      , [LinkableUnit]
                      )
loadArchiveDeps env archives = modifyMVar (linkerArchiveDeps env) $ \m ->
  case M.lookup archives' m of
    Just r  -> return (m, r)
    Nothing -> loadArchiveDeps' archives >>= \r -> return (M.insert archives' r m, r)
  where
     archives' = S.fromList archives

loadArchiveDeps' :: [FilePath]
                 -> IO ( Map Module (Deps, DepsLocation)
                       , [LinkableUnit]
                       )
loadArchiveDeps' archives = do
  archDeps <- forM archives $ \file -> do
    (Ar.Archive entries) <- Ar.loadAr file
    catMaybes <$> mapM (readEntry file) entries
  return (prepareLoadedDeps $ concat archDeps)
    where
      readEntry :: FilePath -> Ar.ArchiveEntry -> IO (Maybe (Deps, DepsLocation))
      readEntry ar_file ar_entry = do
          let bs = Ar.filedata ar_entry
          bh <- unsafeUnpackBinBuffer bs
          getObjectHeader bh >>= \case
            Left _         -> pure Nothing -- not a valid object entry
            Right mod_name -> do
              obj <- getObjectBody bh mod_name
              let !deps = objDeps obj
              pure $ Just (deps, ArchiveFile ar_file)

-- | Predicate to check that an entry in Ar is a JS source
-- and to return it without its header
getJsArchiveEntry :: Ar.ArchiveEntry -> Maybe B.ByteString
getJsArchiveEntry entry = getJsBS (Ar.filedata entry)

-- | Predicate to check that a file is a JS source
isJsFile :: FilePath -> IO Bool
isJsFile fp = withBinaryFile fp ReadMode $ \h -> do
  bs <- B.hGet h jsHeaderLength
  pure (isJsBS bs)

isJsBS :: B.ByteString -> Bool
isJsBS bs = isJust (getJsBS bs)

-- | Get JS source with its header (if it's one)
getJsBS :: B.ByteString -> Maybe B.ByteString
getJsBS bs = B.stripPrefix jsHeader bs

-- Header added to JS sources to discriminate them from other object files.
-- They all have .o extension but JS sources have this header.
jsHeader :: B.ByteString
jsHeader = "//JavaScript"

jsHeaderLength :: Int
jsHeaderLength = B.length jsHeader



prepareLoadedDeps :: [(Deps, DepsLocation)]
                  -> ( Map Module (Deps, DepsLocation)
                     , [LinkableUnit]
                     )
prepareLoadedDeps deps =
  let req     = concatMap (requiredUnits . fst) deps
      depsMap = M.fromList $ map (\d -> (depsModule (fst d), d)) deps
  in  (depsMap, req)

requiredUnits :: Deps -> [LinkableUnit]
requiredUnits d = map (depsModule d,) (IS.toList $ depsRequired d)

-- | read dependencies from an object that might have already been into memory
-- pulls in all Deps from an archive
readDepsFromObj :: LinkedObj -> IO (Maybe (Deps, DepsLocation))
readDepsFromObj = \case
  ObjLoaded name obj -> do
    let !deps = objDeps obj
    pure $ Just (deps,InMemory name obj)
  ObjFile file -> do
    readObjectDeps file >>= \case
      Nothing   -> pure Nothing
      Just deps -> pure $ Just (deps,ObjectFile file)


-- | Embed a JS file into a .o file
--
-- The JS file is merely copied into a .o file with an additional header
-- ("//Javascript") in order to be recognized later on.
--
-- JS files may contain option pragmas of the form: //#OPTIONS:
-- For now, only the CPP option is supported. If the CPP option is set, we
-- append some common CPP definitions to the file and call cpp on it.
embedJsFile :: Logger -> DynFlags -> TmpFs -> UnitEnv -> FilePath -> FilePath -> IO ()
embedJsFile logger dflags tmpfs unit_env input_fn output_fn = do
  let profiling  = False -- FIXME: add support for profiling way

  createDirectoryIfMissing True (takeDirectory output_fn)

  -- the header lets the linker recognize processed JavaScript files
  -- But don't add JavaScript header to object files!

  -- header appended to JS files stored as .o to recognize them.
  let header = "//JavaScript\n"
  jsFileNeedsCpp input_fn >>= \case
    False -> copyWithHeader header input_fn output_fn
    True  -> do

      -- append common CPP definitions to the .js file.
      -- They define macros that avoid directly wiring zencoded names
      -- in RTS JS files
      pp_fn <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "js"
      payload <- B.readFile input_fn
      B.writeFile pp_fn (commonCppDefs profiling <> payload)

      -- run CPP on the input JS file
      js_fn <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule "js"
      let
        cpp_opts = CppOpts
          { cppUseCc       = True
          , cppLinePragmas = False -- LINE pragmas aren't JS compatible
          }
      doCpp logger
              tmpfs
              dflags
              unit_env
              cpp_opts
              pp_fn
              js_fn
      -- add header to recognize the object as a JS file
      copyWithHeader header js_fn output_fn

jsFileNeedsCpp :: FilePath -> IO Bool
jsFileNeedsCpp fn = do
  opts <- getOptionsFromJsFile fn
  pure (CPP `elem` opts)

-- | Link module codes.
--
-- Performs link time optimizations and produces one JStat per module plus some
-- commoned up initialization code.
linkModules :: [ModuleCode] -> ([CompactedModuleCode], JStat)
linkModules mods = (compact_mods, meta)
  where
    compact_mods = map compact mods

    -- here GHCJS used to:
    --  - deduplicate declarations
    --  - rename local variables into shorter ones
    --  - compress initialization data
    -- but we haven't ported it (yet).
    compact m = CompactedModuleCode
      { cmc_js_code = mc_js_code m
      , cmc_module  = mc_module m
      , cmc_exports = mc_exports m
      }

    -- common up statics: different bindings may reference the same statics, we
    -- filter them here to initialize them once
    statics = nubStaticInfo (concatMap mc_statics mods)

    infos   = concatMap mc_closures mods
    debug   = False -- TODO: this could be enabled in a debug build.
                    -- It adds debug info to heap objects
    meta = mconcat
            -- render metadata as individual statements
            [ mconcat (map staticDeclStat statics)
            , mconcat (map staticInitStat statics)
            , mconcat (map (closureInfoStat debug) infos)
            ]

-- | Only keep a single StaticInfo with a given name
nubStaticInfo :: [StaticInfo] -> [StaticInfo]
nubStaticInfo = go emptyUniqSet
  where
    go us = \case
      []     -> []
      (x:xs) ->
        -- only match on siVar. There is no reason for the initializing value to
        -- be different for the same global name.
        let name = siVar x
        in if elementOfUniqSet name us
          then go us xs
          else x : go (addOneToUniqSet us name) xs

-- | Initialize a global object.
--
-- All global objects have to be declared (staticInfoDecl) first.
staticInitStat :: StaticInfo -> JStat
staticInitStat (StaticInfo i sv mcc) =
  case sv of
    StaticData con args         -> appS "h$sti" $ add_cc_arg
                                    [ var i
                                    , var con
                                    , jsStaticArgs args
                                    ]
    StaticFun  f   args         -> appS "h$sti" $ add_cc_arg
                                    [ var i
                                    , var f
                                    , jsStaticArgs args
                                    ]
    StaticList args mt          -> appS "h$stl" $ add_cc_arg
                                    [ var i
                                    , jsStaticArgs args
                                    , toJExpr $ maybe null_ (toJExpr . TxtI) mt
                                    ]
    StaticThunk (Just (f,args)) -> appS "h$stc" $ add_cc_arg
                                    [ var i
                                    , var f
                                    , jsStaticArgs args
                                    ]
    _                           -> mempty
  where
    -- add optional cost-center argument
    add_cc_arg as = case mcc of
      Nothing -> as
      Just cc -> as ++ [toJExpr cc]

-- | declare and do first-pass init of a global object (create JS object for heap objects)
staticDeclStat :: StaticInfo -> JStat
staticDeclStat (StaticInfo global_name static_value _) = decl
  where
    global_ident = TxtI global_name
    decl_init v  = global_ident ||= v
    decl_no_init = appS "h$di" [toJExpr global_ident]

    decl = case static_value of
      StaticUnboxed u     -> decl_init (unboxed_expr u)
      StaticThunk Nothing -> decl_no_init -- CAF initialized in an alternative way
      _                   -> decl_init (app "h$d" [])

    unboxed_expr = \case
      StaticUnboxedBool b          -> app "h$p" [toJExpr b]
      StaticUnboxedInt i           -> app "h$p" [toJExpr i]
      StaticUnboxedDouble d        -> app "h$p" [toJExpr (unSaneDouble d)]
      StaticUnboxedString str      -> app "h$rawStringData" [ValExpr (to_byte_list str)]
      StaticUnboxedStringOffset {} -> 0

    to_byte_list = JList . map (Int . fromIntegral) . BS.unpack
