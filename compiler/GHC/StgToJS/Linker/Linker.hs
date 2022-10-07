{-# LANGUAGE DeriveGeneric     #-}
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
  )
where

import Prelude

import GHC.Platform.Host (hostPlatformArchOS)

import GHC.JS.Syntax

import GHC.Driver.Session (DynFlags(..))
import Language.Haskell.Syntax.Module.Name

import GHC.Linker.Static.Utils (exeFileName)

import GHC.StgToJS.Linker.Types
import GHC.StgToJS.Linker.Utils
import GHC.StgToJS.Linker.Compactor
import GHC.StgToJS.Rts.Rts
import GHC.StgToJS.Object
import GHC.StgToJS.Types hiding (LinkableUnit)
import GHC.StgToJS.UnitUtils
import GHC.StgToJS.Printer

import GHC.Unit.State
import GHC.Unit.Env
import GHC.Unit.Home
import GHC.Unit.Types
import GHC.Unit.Module (moduleStableString)

import GHC.Utils.Encoding
import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Logger (Logger, logVerbAtLeast)
import GHC.Utils.Binary
import GHC.Utils.Ppr (Style(..), renderStyle, Mode(..))
import qualified GHC.Utils.Ppr as Ppr
import GHC.Utils.CliOption
import GHC.Utils.Monad

import qualified GHC.SysTools.Ar          as Ar

import GHC.Data.FastString

import Control.Concurrent.MVar
import Control.Monad

import Data.Array
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy     as BL
import Data.Function            (on)
import Data.Int
import Data.IntSet              (IntSet)
import qualified Data.IntSet              as IS
import Data.IORef
import Data.List  ( partition, nub, intercalate, group, sort
                  , groupBy, intersperse
                  )
import Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import Data.Maybe
import Data.Set                 (Set)
import qualified Data.Set                 as S
import Data.Word

import System.IO
import System.FilePath ((<.>), (</>), dropExtension)
import System.Directory ( createDirectoryIfMissing
                        , doesFileExist
                        , getCurrentDirectory
                        , Permissions(..)
                        , setPermissions
                        , getPermissions
                        )

newtype LinkerStats = LinkerStats
  { bytesPerModule :: Map Module Word64 -- ^ number of bytes linked per module
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
      (dep_map, dep_units, all_deps, rts_wired_functions, dep_archives)
        <- computeLinkDependencies cfg logger out unit_env units objFiles extraStaticDeps isRootFun

      -- retrieve code for dependencies
      mods <- collectDeps dep_map dep_units all_deps

      -- LTO + rendering of JS code
      link_stats <- withBinaryFile (out </> "out.js") WriteMode $ \h -> do
        (metaSize, _compactorState, stats) <- renderLinker lc_cfg cfg h emptyCompactorState rts_wired_functions mods jsFiles
        pure $ linkerStats metaSize stats

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
        writeFile (out </> statsFile) link_stats

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
  env <- newGhcjsEnv
  (objDepsMap, objRequiredUnits) <- loadObjDeps objFiles

  let roots    = S.fromList . filter isRootFun $ concatMap (M.keys . depsHaskellExported . fst) (M.elems objDepsMap)
      rootMods = map (moduleNameString . moduleName . head) . group . sort . map funModule . S.toList $ roots
      objPkgs  = map moduleUnitId $ nub (M.keys objDepsMap)

  when (logVerbAtLeast logger 2) $ void $
    compilationProgressMsg logger $ hcat
      [ text "Linking ", text target, text " (", text (intercalate "," rootMods), char ')' ]

  let (rts_wired_units, rts_wired_functions) = rtsDeps units

  let ue_state = ue_units $ unit_env
  let preload_units = preloadUnits (ue_units unit_env)

  -- all the units we want to link together, without their dependencies
  let root_units = nub (preload_units ++ rts_wired_units ++ reverse objPkgs ++ reverse units)

  -- all the units we want to link together, including their dependencies
  let all_units = transitive_units root_units

      -- compute transitive unit dependencies
      transitive_units = reverse . transitive_units_ []
      transitive_units_ xs = \case
        []     -> xs
        (u:us)
          | u == mainUnitId -> transitive_units_ (u:xs) us
          | otherwise       -> case lookupUnitId ue_state u of
              Nothing -> unit_not_found u
              Just d  ->
                let deps         = unitDepends d
                    is_new_dep x = x `notElem` xs
                    new_deps     = filter is_new_dep deps
                in case new_deps of
                  []
                    | u `elem` xs -> transitive_units_ xs us
                    | otherwise   -> transitive_units_ (u:xs) us
                  ds -> transitive_units_ xs     (ds ++ (u:us))

      unit_not_found u = throwGhcException (CmdLineError ("unknown unit: " ++ unpackFS (unitIdFS u)))

      mkPkgLibPaths :: UnitState -> [UnitId] -> [(UnitId, ([FilePath],[String]))]
      mkPkgLibPaths u_st
        = map (\k -> ( k
                     , (getInstalledPackageLibDirs u_st k
                     , getInstalledPackageHsLibs u_st k)
                     ))

  dep_archives <- getPackageArchives cfg (map snd $ mkPkgLibPaths ue_state all_units)
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


data ModuleCode = ModuleCode
  { mc_module   :: !Module
  , mc_js_code  :: JStat
  , mc_exports  :: !B.ByteString        -- ^ rendered exports
  , mc_closures :: ![ClosureInfo]
  , mc_statics  :: ![StaticInfo]
  , mc_frefs    :: ![ForeignJSRef]
  }

renderLinker
  :: JSLinkConfig
  -> StgToJSConfig
  -> Handle
  -> CompactorState
  -> Set ExportedFun
  -> [ModuleCode] -- ^ linked code per module
  -> [FilePath]   -- ^ additional JS files
  -> IO (Int64, CompactorState, LinkerStats)
renderLinker settings cfg h renamer_state rtsDeps mods jsFiles = do

  -- extract ModuleCode fields required to make a LinkedUnit
  let code_to_linked_unit c = LinkedUnit
        { lu_js_code  = mc_js_code c
        , lu_closures = mc_closures c
        , lu_statics  = mc_statics c
        }

  -- call the compactor
  let (renamer_state', compacted, meta) = compact settings cfg renamer_state
                                            (map ((\(LexicalFastString f) -> f) . funSymbol) $ S.toList rtsDeps)
                                            (map code_to_linked_unit mods)

  let
    doc_str          = renderStyle (Style
                          { lineLength = 100
                          , ribbonsPerLine = 1.5
                          , mode = LeftMode
                            -- Faster to write but uglier code.
                            -- Use "PageMode False" to enable nicer code instead
                          })
    putBS       = B.hPut h
    render_js x = BC.pack (doc_str (pretty x Ppr.<> Ppr.char '\n'))

  ---------------------------------------------------------
  -- Pretty-print JavaScript code for all the dependencies.
  --
  -- We have to pretty-print at link time because we want to be able to perform
  -- global link-time optimisations (e.g. renamings) on the whole generated JS
  -- file.

  -- modules themselves
  mod_sizes <- forM (mods `zip` compacted) $ \(mod,compacted_mod) -> do
    let !bs = render_js compacted_mod
    putBS bs
    let !mod_size = fromIntegral (B.length bs)
    let !mod_mod  = mc_module mod
    pure $! (mod_mod, mod_size)

  -- metadata
  let meta_bs = render_js meta
  let !meta_length = fromIntegral (B.length meta_bs)
  putBS meta_bs

  -- exports
  mapM_ (putBS . mc_exports) mods

  -- explicit additional JS files
  mapM_ (\i -> B.readFile i >>= putBS) jsFiles

  -- stats
  let link_stats = LinkerStats
        { bytesPerModule = M.fromList mod_sizes
        }

  pure
    ( meta_length
    , renamer_state'
    , link_stats
    )

-- | Render linker stats
linkerStats :: Int64         -- ^ code size of packed metadata
            -> LinkerStats   -- ^ code size per module
            -> String
linkerStats meta s =
  intercalate "\n\n" [meta_stats, package_stats, module_stats] <> "\n\n"
  where
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


getPackageArchives :: StgToJSConfig -> [([FilePath],[String])] -> IO [FilePath]
getPackageArchives cfg pkgs =
  filterM doesFileExist [ p </> "lib" ++ l ++ profSuff <.> "a"
                        | (paths, libs) <- pkgs, p <- paths, l <- libs ]
  where
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
  "<!DOCTYPE html>\
  \<html>\
  \  <head>\
  \    <script language=\"javascript\" src=\"rts.js\"></script>\
  \    <script language=\"javascript\" src=\"lib.js\"></script>\
  \    <script language=\"javascript\" src=\"out.js\"></script>\
  \  </head>\
  \  <body>\
  \  </body>\
  \  <script language=\"javascript\" src=\"runmain.js\" defer></script>\
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
  code <- fmap (catMaybes . concat) . forM packages' $ \pkg ->
    mapM (uncurry $ extractDeps ar_state units_by_module)
         (fromMaybe [] $ M.lookup pkg mod_deps_bypkg)
  return code

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
    unlines'      = intersperse newline . map oiRaw
    collectCode l = ModuleCode
                      { mc_module   = mod
                      , mc_js_code  = mconcat (map oiStat l)
                      , mc_exports  = mconcat (unlines' l)
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
    mk_fun sym = ExportedFun mod (LexicalFastString (mkJsSymbol mod sym))

-- | Make JS symbol corresponding to the given Haskell symbol in the given
-- module
mkJsSymbol :: Module -> FastString -> FastString
mkJsSymbol mod s = mkFastString $ mconcat
  [ "h$"
  , zEncodeString (unitModuleString mod <> ".")
  , zString (zEncodeFS s)
  ]

-- | read all dependency data from the to-be-linked files
loadObjDeps :: [LinkedObj] -- ^ object files to link
            -> IO (Map Module (Deps, DepsLocation), [LinkableUnit])
loadObjDeps objs = prepareLoadedDeps <$> mapM readDepsFile' objs

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
readDepsFile' :: LinkedObj -> IO (Deps, DepsLocation)
readDepsFile' = \case
  ObjLoaded name obj -> do
    let !deps = objDeps obj
    pure (deps,InMemory name obj)
  ObjFile file -> do
    deps <- readObjectDeps file
    pure (deps,ObjectFile file)
