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
-- GHCJS linker, collects dependencies from the object files (.js_o, js_p_o),
-- which contain linkable units with dependency information
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Linker where

import Prelude

import GHC.Platform.Ways
import GHC.Platform.Host (hostPlatformArchOS)

import           GHC.StgToJS.Linker.Types
import           GHC.StgToJS.Linker.Utils
import           GHC.StgToJS.Linker.Compactor
import           GHC.StgToJS.Linker.Shims

import           GHC.StgToJS.Rts.Rts

import           GHC.JS.Syntax

import           GHC.StgToJS.Object
import           GHC.StgToJS.Types hiding (LinkableUnit)
import           GHC.StgToJS.UnitUtils
import           GHC.StgToJS.Printer

import qualified GHC.SysTools.Ar          as Ar
import           GHC.Utils.Encoding
import           GHC.Utils.Outputable hiding ((<>))
import           GHC.Utils.Panic
import           GHC.Unit.State
import           GHC.Unit.Env
import           GHC.Unit.Home
import           GHC.Unit.Types
import           GHC.Utils.Error
import           GHC.Data.FastString

import           Control.Concurrent.MVar
import           Control.Monad

import           Data.Array
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy     as BL
import           Data.Function            (on)
import           Data.Int
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IS
import           Data.IORef
import           Data.List  ( partition, nub, foldl', intercalate, group, sort
                            , groupBy, isSuffixOf, find, intersperse
                            )
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Word

import           GHC.Generics (Generic)

import           System.FilePath (splitPath, (<.>), (</>), dropExtension, isExtensionOf)
import           System.Environment (lookupEnv)
import           System.Directory ( createDirectoryIfMissing
                                  , doesFileExist
                                  , getCurrentDirectory
                                  , Permissions(..)
                                  , setPermissions
                                  , getPermissions
                                  , listDirectory
                                  )

import GHC.Driver.Session (targetWays_, DynFlags(..))
import Language.Haskell.Syntax.Module.Name
import GHC.Unit.Module (moduleStableString)
import GHC.Utils.Logger (Logger, logVerbAtLeast)
import GHC.Utils.TmpFs (TmpFs)

import GHC.Linker.Static.Utils (exeFileName)

newtype LinkerStats = LinkerStats
  { bytesPerModule :: Map Module Word64 -- ^ number of bytes linked per module
  }

-- | result of a link pass
data LinkResult = LinkResult
  { linkOut         :: B.ByteString   -- ^ compiled Haskell code
  , linkOutStats    :: LinkerStats    -- ^ statistics about generated code
  , linkOutMetaSize :: Int64          -- ^ size of packed metadata in generated code
  , linkForeignRefs :: [ForeignJSRef] -- ^ foreign code references in compiled haskell code
  , linkLibRTS      :: [FilePath]     -- ^ library code to load with the RTS
  , linkLibA        :: [FilePath]     -- ^ library code to load after RTS
  , linkLibAArch    :: [FilePath]     -- ^ library code to load from archives after RTS
  , linkBase        :: Base           -- ^ base metadata to use if we want to link incrementally against this result
  } deriving (Generic)

newtype ArchiveState = ArchiveState { loadedArchives :: IORef (Map FilePath Ar.Archive) }

emptyArchiveState :: IO ArchiveState
emptyArchiveState = ArchiveState <$> newIORef M.empty

-- | link and write result to disk (jsexe directory)
link :: GhcjsEnv
     -> JSLinkConfig
     -> StgToJSConfig
     -> Logger
     -> TmpFs
     -> DynFlags
     -> UnitEnv
     -> FilePath               -- ^ output file/directory
     -> [FilePath]             -- ^ include path for home package
     -> [UnitId]               -- ^ packages to link
     -> [LinkedObj]            -- ^ the object files we're linking
     -> [FilePath]             -- ^ extra js files to include
     -> (ExportedFun -> Bool)  -- ^ functions from the objects to use as roots (include all their deps)
     -> Set ExportedFun        -- ^ extra symbols to link in
     -> IO ()
link env lc_cfg cfg logger tmpfs dflags unit_env out include pkgs objFiles jsFiles isRootFun extraStaticDeps
  | lcNoJSExecutables lc_cfg = return ()
  | otherwise = do
      link_res <- link' env lc_cfg cfg dflags logger unit_env out include pkgs objFiles jsFiles
                    isRootFun extraStaticDeps

      let genBase = isJust (lcGenBase lc_cfg)
          jsExt | genBase   = "base.js"
                | otherwise = "js"
      createDirectoryIfMissing False out
      B.writeFile (out </> "out" <.> jsExt) (linkOut link_res)

      -- dump foreign references file (.frefs)
      unless (lcOnlyOut lc_cfg) $ do
        let frefsFile   = if genBase then "out.base.frefs" else "out.frefs"
            jsonFrefs  = mempty

        BL.writeFile (out </> frefsFile <.> "json") jsonFrefs
        BL.writeFile (out </> frefsFile <.> "js")
                     ("h$checkForeignRefs(" <> jsonFrefs <> ");")

        -- dump stats
        unless (lcNoStats lc_cfg) $ do
          let statsFile = if genBase then "out.base.stats" else "out.stats"
          let stats = linkerStats (linkOutMetaSize link_res) (linkOutStats link_res)
          writeFile (out </> statsFile) stats

        -- Sylvain (2022-06): find RTS js files (shims) via an environment variable...
        -- Remove when all files are located via Cabal's js-sources
        let is_js_file f = "js" `isExtensionOf` f || "pp" `isExtensionOf` f
        let find_env_shims env_var = do
              lookupEnv env_var >>= \case
                Nothing  -> error (env_var ++ " env var not set!")
                Just dir -> do
                  (fmap (dir </>) . filter is_js_file) <$> listDirectory dir

        -- link with the RTS
        unless (lcNoRts lc_cfg) $ do
          static_rts_files <- find_env_shims "JS_RTS_PATH"
          let all_rts_js = linkLibRTS link_res ++ static_rts_files

          rts_js_bss <- streamShims <$> readShimFiles logger tmpfs dflags unit_env all_rts_js
          BL.writeFile (out </> "rts.js") (BLC.pack rtsDeclsText
                                           <> BL.fromChunks rts_js_bss
                                           <> BLC.pack (rtsText cfg))

        static_base_files <- find_env_shims "JS_BASE_PATH"
        let all_lib_js = linkLibA link_res ++ static_base_files
        lla'    <- streamShims <$> readShimFiles logger tmpfs dflags unit_env all_lib_js
        -- llarch' <- mapM (readShimsArchive dflags) (linkLibArch link_res)
        -- let lib_js = BL.fromChunks $ llarch' ++ lla'
        let lib_js = BL.fromChunks $! lla'
        BL.writeFile (out </> "lib" <.> jsExt) lib_js

        if genBase
          then generateBase out (linkBase link_res)
          else when (    not (lcOnlyOut lc_cfg)
                      && not (lcNoRts   lc_cfg)
                      && not (usingBase lc_cfg)
                    )
               $ do
                 _ <- combineFiles lc_cfg out
                 writeHtml    out
                 writeRunMain out
                 writeRunner lc_cfg out
                 writeExterns out

-- | link in memory
link' :: GhcjsEnv
      -> JSLinkConfig
      -> StgToJSConfig
      -> DynFlags
      -> Logger
      -> UnitEnv
      -> String                     -- ^ target (for progress message)
      -> [FilePath]                 -- ^ include path for home package
      -> [UnitId]                   -- ^ packages to link
      -> [LinkedObj]                -- ^ the object files we're linking
      -> [FilePath]                 -- ^ extra js files to include
      -> (ExportedFun -> Bool)      -- ^ functions from the objects to use as roots (include all their deps)
      -> Set ExportedFun            -- ^ extra symbols to link in
      -> IO LinkResult
link' env lc_cfg cfg dflags logger unit_env target _include pkgs objFiles _jsFiles isRootFun extraStaticDeps
  = do
      (objDepsMap, objRequiredUnits) <- loadObjDeps objFiles

      let rootSelector | Just baseMod <- lcGenBase lc_cfg =
                           \(ExportedFun  m _s) -> m == baseMod
                       | otherwise = isRootFun
          roots    = S.fromList . filter rootSelector $ concatMap (M.keys . depsHaskellExported . fst) (M.elems objDepsMap)
          rootMods = map (moduleNameString . moduleName . head) . group . sort . map funModule . S.toList $ roots
          objPkgs  = map moduleUnitId $ nub (M.keys objDepsMap)

      when (logVerbAtLeast logger 2) $ void $
        compilationProgressMsg logger . text $
          case lcGenBase lc_cfg of
            Just baseMod -> "Linking base bundle " ++ target ++ " (" ++ moduleNameString (moduleName baseMod) ++ ")"
            _            -> "Linking " ++ target ++ " (" ++ intercalate "," rootMods ++ ")"

      base <- case lcUseBase lc_cfg of
        NoBase        -> return emptyBase
        BaseFile file -> loadBase file
        BaseState b   -> return b

      (rdPkgs, rds) <- rtsDeps pkgs

      -- c   <- newMVar M.empty
      let preload_units = preloadUnits (ue_units unit_env)

      let rtsPkgs     =  map stringToUnitId ["@rts", "@rts_" ++ waysTag (targetWays_ $ dflags)]
          pkgs' :: [UnitId]
          pkgs'       = nub (rtsPkgs ++ preload_units ++ rdPkgs ++ reverse objPkgs ++ reverse pkgs)
          pkgs''      = filter (not . isAlreadyLinked base) pkgs'
          ue_state    = ue_units $ unit_env
          -- pkgLibPaths = mkPkgLibPaths pkgs'
          -- getPkgLibPaths :: UnitId -> ([FilePath],[String])
          -- getPkgLibPaths k = fromMaybe ([],[]) (lookup k pkgLibPaths)
      (archsDepsMap, archsRequiredUnits) <- loadArchiveDeps env =<< getPackageArchives cfg (map snd $ mkPkgLibPaths ue_state pkgs')
      pkgArchs <- getPackageArchives cfg (map snd $ mkPkgLibPaths ue_state pkgs'')

      when (logVerbAtLeast logger 2) $
        logInfo logger $ hang (text "Linking with archives:") 2 (vcat (fmap text pkgArchs))

      -- compute dependencies
      let dep_units      = pkgs' ++ [homeUnitId (ue_unsafeHomeUnit $ unit_env)]
          dep_map        = objDepsMap `M.union` archsDepsMap
          excluded_units = baseUnits base -- already linked units
          dep_fun_roots  = roots `S.union` rds `S.union` extraStaticDeps
          dep_unit_roots = archsRequiredUnits ++ objRequiredUnits

      all_deps <- getDeps (fmap fst dep_map) excluded_units dep_fun_roots dep_unit_roots

      when (logVerbAtLeast logger 2) $
        logInfo logger $ hang (text "Units to link:") 2 (vcat (fmap ppr dep_units))
        -- logInfo logger $ hang (text "All deps:") 2 (vcat (fmap ppr (S.toList all_deps)))

      -- retrieve code for dependencies
      code <- collectDeps dep_map dep_units all_deps

      let (outJs, metaSize, compactorState, stats) =
             renderLinker lc_cfg cfg (baseCompactorState base) rds code
          base'  = Base compactorState (nub $ basePkgs base ++ pkgs'')
                         (all_deps `S.union` baseUnits base)

      return $ LinkResult
        { linkOut         = outJs
        , linkOutStats    = stats
        , linkOutMetaSize = metaSize
        , linkForeignRefs = concatMap mc_frefs code
        , linkLibRTS      = [] -- (filter (`notElem` alreadyLinkedBefore) shimsBefore)
        , linkLibA        = [] -- (filter (`notElem` alreadyLinkedAfter)  shimsAfter)
        , linkLibAArch    = pkgArchs
        , linkBase        = base'
        }
  where
    isAlreadyLinked :: Base -> UnitId -> Bool
    isAlreadyLinked b uid = uid `elem` basePkgs b

    mkPkgLibPaths :: UnitState -> [UnitId] -> [(UnitId, ([FilePath],[String]))]
    mkPkgLibPaths u_st
      = map (\k -> ( k
                   , (getInstalledPackageLibDirs u_st k
                   , getInstalledPackageHsLibs u_st k)
                   ))


data ModuleCode = ModuleCode
  { mc_module   :: !Module
  , mc_js_code  :: !JStat
  , mc_exports  :: !FastString        -- ^ rendered exports
  , mc_closures :: ![ClosureInfo]
  , mc_statics  :: ![StaticInfo]
  , mc_frefs    :: ![ForeignJSRef]
  }

renderLinker
  :: JSLinkConfig
  -> StgToJSConfig
  -> CompactorState
  -> Set ExportedFun
  -> [ModuleCode] -- ^ linked code per module
  -> (B.ByteString, Int64, CompactorState, LinkerStats)
renderLinker settings cfg renamer_state rtsDeps code =
  ( rendered_all
  , meta_length
  , renamer_state'
  , stats
  )
  where
    -- extract ModuleCode fields required to make a LinkedUnit
    code_to_linked_unit c = LinkedUnit
      { lu_js_code  = mc_js_code c
      , lu_closures = mc_closures c
      , lu_statics  = mc_statics c
      }
    -- call the compactor
    (renamer_state', compacted, meta) = compact settings cfg renamer_state
                                          (map ((\(LexicalFastString f) -> f) . funSymbol) $ S.toList rtsDeps)
                                          (map code_to_linked_unit code)
    -- render result into JS code
    rendered_all     = mconcat [mconcat rendered_mods, rendered_meta, rendered_exports]
    rendered_mods    = fmap render_js compacted
    rendered_meta    = render_js meta
    render_js        = BC.pack . (<>"\n") . show . pretty
    rendered_exports = BC.concat . map bytesFS . filter (not . nullFS) $ map mc_exports code
    meta_length      = fromIntegral (BC.length rendered_meta)
    -- make LinkerStats entry for the given ModuleCode.
    -- For now, only associate generated code size in bytes to each module
    mk_stat c b = (mc_module c, fromIntegral . BC.length $ b)
    stats = LinkerStats $ M.fromList $ zipWith mk_stat code rendered_mods

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


splitPath' :: FilePath -> [FilePath]
splitPath' = map (filter (`notElem` ("/\\"::String))) . splitPath

getPackageArchives :: StgToJSConfig -> [([FilePath],[String])] -> IO [FilePath]
getPackageArchives cfg pkgs =
  filterM doesFileExist [ p </> "lib" ++ l ++ profSuff <.> "a"
                        | (paths, libs) <- pkgs, p <- paths, l <- libs ]
  where
    -- XXX the profiling library name is probably wrong now
    profSuff | csProf cfg = "_p"
             | otherwise  = ""

-- fixme the wired-in package id's we get from GHC we have no version
getShims :: [FilePath] -> [UnitId] -> IO ([FilePath], [FilePath])
getShims = panic "Panic from getShims: Shims not implemented! no to shims!"
-- getShims dflags extraFiles pkgDeps = do
--   (w,a) <- collectShims (getLibDir dflags </> "shims")
--                         (map (convertPkg dflags) pkgDeps)
--   extraFiles' <- mapM canonicalizePath extraFiles
--   return (w, a++extraFiles')

{- | convenience: combine rts.js, lib.js, out.js to all.js that can be run
     directly with node.js or SpiderMonkey jsshell
 -}
combineFiles :: JSLinkConfig
             -> FilePath
             -> IO ()
combineFiles cfg fp = do
  files   <- mapM (B.readFile.(fp</>)) ["rts.js", "lib.js", "out.js"]
  let runMain
        | lcNoHsMain cfg = mempty
        | otherwise      = runMainJS
  writeBinaryFile (fp</>"all.js") (mconcat (files ++ [runMain]))

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

-- | write the manifest.webapp file that for firefox os
writeWebAppManifest :: FilePath -- ^ top directory
                    -> FilePath -- ^ output directory
                    -> IO ()
writeWebAppManifest top out = do
  e <- doesFileExist manifestFile
  unless e $ B.readFile (top </> "manifest.webapp") >>= B.writeFile manifestFile
  where
    manifestFile = out </> "manifest.webapp"

rtsExterns :: FastString
rtsExterns =
  "// GHCJS RTS externs for closure compiler ADVANCED_OPTIMIZATIONS\n\n" <>
  mconcat (map (\x -> "/** @type {*} */\nObject.d" <> mkFastString (show x) <> ";\n")
               [(7::Int)..16384])

writeExterns :: FilePath -> IO ()
writeExterns out = writeFile (out </> "all.js.externs")
  $ unpackFS rtsExterns

-- | get all functions in a module
modFuns :: Deps -> [ExportedFun]
modFuns (Deps _m _r e _b) = M.keys e

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
    Just modUnits -> do
      let selector n _  = n `IS.member` modUnits || isGlobalUnit n
      x <- case loc of
        ObjectFile o  -> collectCode =<< readObjectFileKeys selector o
        ArchiveFile a -> (collectCode
                        <=< readObjectKeys (a ++ ':':moduleNameString (moduleName mod)) selector)
                        =<< readArObject ar_state mod a
        InMemory n b  -> collectCode =<< readObjectKeys n selector b
      return x
  where
    mod           = depsModule deps
    newline       = mkFastString "\n"
    unlines'      = intersperse newline . map oiRaw
    collectCode l = let x = ModuleCode
                              { mc_module   = mod
                              , mc_js_code  = mconcat (map oiStat l)
                              , mc_exports  = mconcat (unlines' l)
                              , mc_closures = concatMap oiClInfo l
                              , mc_statics  = concatMap oiStatic l
                              , mc_frefs    = concatMap oiFImports l
                              }
                    in return (Just x)

readArObject :: ArchiveState -> Module -> FilePath -> IO BL.ByteString
readArObject ar_state mod ar_file = do
  loaded_ars <- readIORef (loadedArchives ar_state)
  (Ar.Archive entries) <- case M.lookup ar_file loaded_ars of
    Just a -> pure a
    Nothing -> do
      a <- Ar.loadAr ar_file
      modifyIORef (loadedArchives ar_state) (M.insert ar_file a)
      pure a
  let tag = moduleNameTag $ moduleName mod
      matchTag entry
        | Right hdr <- getHeader (BL.fromStrict $ Ar.filedata entry)
        = hdrModuleName hdr == tag
        | otherwise
        = False

  -- XXX this shouldn't be an exception probably
  pure $! maybe (error $ "could not find object for module "
                ++ moduleNameString (moduleName mod)
                ++ " in "
                ++ ar_file)
                (BL.fromStrict . Ar.filedata) (find matchTag entries)

{- | Static dependencies are symbols that need to be linked regardless
     of whether the linked program refers to them. For example
     dependencies that the RTS uses or symbols that the user program
     refers to directly
 -}
newtype StaticDeps =
  StaticDeps { unStaticDeps :: [(FastString, FastString)] -- module/symbol
             }

noStaticDeps :: StaticDeps
noStaticDeps = StaticDeps []


-- | dependencies for the RTS, these need to be always linked
rtsDeps :: [UnitId] -> IO ([UnitId], Set ExportedFun)
rtsDeps pkgs = readSystemDeps pkgs "rtsdeps.yaml"

-- | dependencies for the Template Haskell, these need to be linked when running
--   Template Haskell (in addition to the RTS deps)
thDeps :: [UnitId] -> IO ([UnitId], Set ExportedFun)
thDeps pkgs = readSystemDeps pkgs "thdeps.yaml"

-- | A helper function to read system dependencies that are hardcoded via a file
-- path.
readSystemDeps :: [UnitId]    -- ^ Packages that are already Linked
               -> FilePath    -- ^ File to read
               -> IO ([UnitId], Set ExportedFun)
readSystemDeps pkgs file = do
  (deps_pkgs, deps_funs) <- readSystemDeps' file
  pure ( filter (`S.member` linked_pkgs) deps_pkgs
       , S.filter (\fun ->
                     moduleUnitId (funModule fun) `S.member` linked_pkgs) deps_funs
       )

  where
    linked_pkgs     = S.fromList pkgs


readSystemDeps' :: FilePath -> IO ([UnitId], Set ExportedFun)
readSystemDeps' file
  -- hardcode contents to get rid of yaml dep
  -- XXX move runTHServer to some suitable wired-in package
  | file == "thdeps.yaml" = pure ( [ baseUnitId ]
                                 , S.fromList $ d baseUnitId "GHC.JS.Prim.TH.Eval" ["runTHServer"])
  | file == "rtsdeps.yaml" = pure ( [ baseUnitId
                                    , primUnitId
                                    , bignumUnitId
                                    ]
                                  , S.fromList $ concat
                                  [ d baseUnitId "GHC.Conc.Sync" ["reportError"]
                                  , d baseUnitId "Control.Exception.Base" ["nonTermination"]
                                  , d baseUnitId "GHC.Exception.Type" ["SomeException"]
                                  , d baseUnitId "GHC.TopHandler" ["runMainIO", "topHandler"]
                                  , d baseUnitId "GHC.Base" ["$fMonadIO"]
                                  , d baseUnitId "GHC.Maybe" ["Nothing", "Just"]
                                  , d baseUnitId "GHC.Ptr" ["Ptr"]
                                  , d primUnitId "GHC.Types" [":", "[]"]
                                  , d primUnitId "GHC.Tuple" ["(,)", "(,,)", "(,,,)", "(,,,,)", "(,,,,,)","(,,,,,,)", "(,,,,,,,)", "(,,,,,,,,)", "(,,,,,,,,,)"]
                                  , d baseUnitId "GHC.JS.Prim" ["JSVal", "JSException", "$fShowJSException", "$fExceptionJSException", "resolve", "resolveIO", "toIO"]
                                  , d baseUnitId "GHC.JS.Prim.Internal" ["wouldBlock", "blockedIndefinitelyOnMVar", "blockedIndefinitelyOnSTM", "ignoreException", "setCurrentThreadResultException", "setCurrentThreadResultValue"]
                                  ]
                                  )
  | otherwise = pure (mempty, mempty)
  where

    d :: UnitId -> FastString -> [FastString] -> [ExportedFun]
    d uid mod symbols =
      let pkg_module = mkJsModule uid mod
      in map (ExportedFun pkg_module
              . LexicalFastString
              . mkJsSymbol pkg_module
             )
             symbols

    mkJsModule :: UnitId -> FastString -> Module
    mkJsModule uid mod = mkModule (RealUnit (Definite uid)) (mkModuleNameFS mod)

-- | Make JS symbol corresponding to the given Haskell symbol in the given
-- module
mkJsSymbol :: Module -> FastString -> FastString
mkJsSymbol mod s = mkFastString $ mconcat
  [ "h$"
  , zEncodeString (unitModuleString mod <> ".")
  , zString (zEncodeFS s)
  ]

{- | read a static dependencies specification and give the roots

     if dependencies come from a versioned (non-hardwired) package
     that is linked multiple times, then the returned dependencies
     will all come from the same version, but it's undefined which one.
 -}

type SDep = (FastString, FastString) -- ^ module/symbol

staticDeps :: UnitEnv
           -> [(FastString, Module)]   -- ^ wired-in package names / keys
           -> StaticDeps              -- ^ deps from yaml file
           -> (StaticDeps, Set UnitId, Set ExportedFun)
                                      -- ^ the StaticDeps contains the symbols
                                      --   for which no package could be found
staticDeps unit_env wiredin sdeps = mkDeps sdeps
  where
    u_st  = ue_units unit_env
    mkDeps (StaticDeps ds) =
      let (u, p, r) = foldl' resolveDep ([], S.empty, S.empty) ds
      in  (StaticDeps u, closePackageDeps u_st p, r)
    resolveDep :: ([SDep], Set UnitId, Set ExportedFun)
               -> SDep
               -> ([SDep], Set UnitId, Set ExportedFun)
    resolveDep (unresolved, pkgs, resolved) dep@(mod_name, s) =
      -- lookup our module in wiredin names
      case lookup mod_name wiredin of
             -- we didn't find the module in wiredin so add to unresolved
             Nothing -> ( dep : unresolved, pkgs, resolved)
             -- this is a wired in module
             Just mod  ->
               let mod_uid = moduleUnitId mod
               in case lookupUnitId u_st mod_uid of
                 -- couldn't find the uid for this wired in package so explode
                 Nothing -> pprPanic ("Package key for wired-in dependency could not be found.`"
                                     ++ "I looked for: "
                                     ++ unpackFS mod_name
                                     ++ " received " ++ moduleNameString (moduleName mod)
                                     ++ " but could not find: " ++ unitString mod_uid
                                     ++ " in the UnitState."
                                     ++ " Here is too much info for you: ")
                            $ pprWithUnitState u_st (ppr mod)
                 -- we are all good, add the uid to the package set, construct
                 -- its symbols on the fly and add the module to exported symbol
                 -- set
                 Just _ -> ( unresolved
                           , S.insert mod_uid pkgs
                           , S.insert (ExportedFun mod
                                       . LexicalFastString $ mkJsSymbol mod s) resolved
                           )

closePackageDeps :: UnitState -> Set UnitId -> Set UnitId
closePackageDeps u_st pkgs
  | S.size pkgs == S.size pkgs' = pkgs
  | otherwise                   = closePackageDeps u_st pkgs'
  where
    pkgs' = pkgs `S.union` S.fromList (concatMap deps $ S.toList pkgs)
    notFound = error "closePackageDeps: package not found"
    deps :: UnitId -> [UnitId]
    deps = unitDepends
         . fromMaybe notFound
         . lookupUnitId u_st

-- read all dependency data from the to-be-linked files
loadObjDeps :: [LinkedObj] -- ^ object files to link
            -> IO (Map Module (Deps, DepsLocation), [LinkableUnit])
loadObjDeps objs = prepareLoadedDeps <$> mapM readDepsFile' objs

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
      readEntry ar_file ar_entry
        | isObjFile (Ar.filename ar_entry) =
            fmap (,ArchiveFile ar_file) <$>
                 (readDepsMaybe (ar_file ++ ':':Ar.filename ar_entry) (BL.fromStrict $ Ar.filedata ar_entry))
        | otherwise = return Nothing


isObjFile :: FilePath -> Bool
isObjFile file = ".o" `isSuffixOf` file || -- vanilla
                 "_o" `isSuffixOf` file    -- some "Way", like .p_o

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

-- read dependencies from an object that might have already been into memory
-- pulls in all Deps from an archive
readDepsFile' :: LinkedObj -> IO (Deps, DepsLocation)
readDepsFile' (ObjLoaded name bs) = (,InMemory name bs) <$>
                                    readDeps name bs
readDepsFile' (ObjFile file)      =
  (,ObjectFile file) <$> readDepsFile file

generateBase :: FilePath -> Base -> IO ()
generateBase outDir b =
  BL.writeFile (outDir </> "out.base.symbs") (renderBase b)

