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
  ( link
  )
where

import Prelude

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
import           Data.List  ( partition, nub, intercalate, group, sort
                            , groupBy, intersperse
                            )
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Word

import           GHC.Generics (Generic)

import           System.FilePath ((<.>), (</>), dropExtension)
import           System.Directory ( createDirectoryIfMissing
                                  , doesFileExist
                                  , getCurrentDirectory
                                  , Permissions(..)
                                  , setPermissions
                                  , getPermissions
                                  )

import GHC.Driver.Session (DynFlags(..))
import Language.Haskell.Syntax.Module.Name
import GHC.Unit.Module (moduleStableString)
import GHC.Utils.Logger (Logger, logVerbAtLeast)
import GHC.Utils.TmpFs (TmpFs)
import GHC.Utils.Binary
import GHC.Utils.Ppr (Style(..), renderStyle, Mode(..))

import GHC.Linker.Static.Utils (exeFileName)

newtype LinkerStats = LinkerStats
  { bytesPerModule :: Map Module Word64 -- ^ number of bytes linked per module
  }

-- | result of a link pass
data LinkResult = LinkResult
  { linkOut         :: FilePath -> IO () -- ^ compiled Haskell code
  , linkOutStats    :: LinkerStats       -- ^ statistics about generated code
  , linkOutMetaSize :: Int64             -- ^ size of packed metadata in generated code
  , linkForeignRefs :: [ForeignJSRef]    -- ^ foreign code references in compiled haskell code
  , linkLibRTS      :: [FilePath]        -- ^ library code to load with the RTS
  , linkLibA        :: [FilePath]        -- ^ library code to load after RTS
  , linkLibAArch    :: [FilePath]        -- ^ library code to load from archives after RTS
  , linkBase        :: Base              -- ^ base metadata to use if we want to link incrementally against this result
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
      link_res <- link' env lc_cfg cfg logger unit_env out include pkgs objFiles jsFiles
                    isRootFun extraStaticDeps

      let genBase = isJust (lcGenBase lc_cfg)
          jsExt | genBase   = "base.js"
                | otherwise = "js"
      createDirectoryIfMissing False out
      linkOut link_res (out </> "out" <.> jsExt)

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

        -- link with the RTS
        unless (lcNoRts lc_cfg) $ do
          BL.writeFile (out </> "rts.js") ( BLC.pack rtsDeclsText
                                           <> BLC.pack (rtsText cfg))

        let all_lib_js = linkLibA link_res
        lla'    <- streamShims <$> readShimFiles logger tmpfs dflags unit_env all_lib_js
        llarch' <- mapM readShimsArchive (linkLibAArch link_res)
        let lib_js = BL.fromChunks $! llarch' ++ lla'
        BL.writeFile (out </> "lib" <.> jsExt) lib_js

        if genBase
          then panic "support for base bundle not implemented"
            -- generateBase out (linkBase link_res)
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

readShimsArchive :: FilePath -> IO B.ByteString
readShimsArchive ar_file = do
  (Ar.Archive entries) <- Ar.loadAr ar_file
  jsdata <- catMaybes <$> mapM readEntry entries
  return (B.intercalate "\n" jsdata)
    where
      readEntry :: Ar.ArchiveEntry -> IO (Maybe B.ByteString)
      readEntry ar_entry
        | isJsFile ar_entry = pure $ Just (Ar.filedata ar_entry)
        | otherwise = pure Nothing



-- | link in memory
link' :: GhcjsEnv
      -> JSLinkConfig
      -> StgToJSConfig
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
link' env lc_cfg cfg logger unit_env target _include pkgs objFiles _jsFiles isRootFun extraStaticDeps
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
        BaseFile _file -> panic "support for base bundle not implemented" -- loadBase file
        BaseState b   -> return b

      let (rdPkgs, rds) = rtsDeps pkgs

      -- c   <- newMVar M.empty
      let preload_units = preloadUnits (ue_units unit_env)

      let pkgs' :: [UnitId]
          pkgs'       = nub (preload_units ++ rdPkgs ++ reverse objPkgs ++ reverse pkgs)
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

      (outJs, metaSize, compactorState, stats) <- renderLinker lc_cfg cfg (baseCompactorState base) rds code
      let base'  = Base compactorState (nub $ basePkgs base ++ pkgs'')
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
  , mc_exports  :: !B.ByteString        -- ^ rendered exports
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
  -> IO (FilePath -> IO (), Int64, CompactorState, LinkerStats)
renderLinker settings cfg renamer_state rtsDeps code = do

  -- extract ModuleCode fields required to make a LinkedUnit
  let code_to_linked_unit c = LinkedUnit
        { lu_js_code  = mc_js_code c
        , lu_closures = mc_closures c
        , lu_statics  = mc_statics c
        }

  -- call the compactor
  let (renamer_state', compacted, meta) = compact settings cfg renamer_state
                                            (map ((\(LexicalFastString f) -> f) . funSymbol) $ S.toList rtsDeps)
                                            (map code_to_linked_unit code)
  let
    render_all fp = do
      BL.writeFile fp rendered_all

    -- render result into JS code
    rendered_all     = mconcat [mconcat rendered_mods, rendered_meta, rendered_exports]
    rendered_mods    = fmap render_js compacted
    rendered_meta    = render_js meta
    doc_str          = renderStyle (Style
                          { lineLength = 100
                          , ribbonsPerLine = 1.5
                          , mode = LeftMode
                            -- Faster to write but uglier code.
                            -- Use "PageMode False" to enable nicer code instead
                          })
    render_js x      = BL.fromChunks [BC.pack (doc_str (pretty x)), BC.pack "\n"]
    rendered_exports = BL.fromChunks (map mc_exports code)
    meta_length      = fromIntegral (BL.length rendered_meta)
    -- make LinkerStats entry for the given ModuleCode.
    -- For now, only associate generated code size in bytes to each module
    mk_stat c b = (mc_module c, fromIntegral . BL.length $ b)
    stats = LinkerStats $ M.fromList $ zipWith mk_stat code rendered_mods

  pure
    ( render_all
    , meta_length
    , renamer_state'
    , stats
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


-- | A helper function to read system dependencies that are hardcoded via a file
-- path.
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
      , mkPrimFuns "GHC.Tuple"
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

{- | read a static dependencies specification and give the roots

     if dependencies come from a versioned (non-hardwired) package
     that is linked multiple times, then the returned dependencies
     will all come from the same version, but it's undefined which one.
 -}

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

-- | Predicate to check that an entry in Ar is a JS payload
isJsFile :: Ar.ArchiveEntry -> Bool
isJsFile = checkEntryHeader "//JavaScript"

-- | Ensure that the entry header to the Archive object is sound.
checkEntryHeader :: B.ByteString -> Ar.ArchiveEntry -> Bool
checkEntryHeader header entry =
  B.take (B.length header) (Ar.filedata entry) == header


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
