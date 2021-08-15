{-# LANGUAGE OverloadedStrings,
             TupleSections,
             DeriveGeneric

  #-}
{- |
  GHCJS linker, collects dependencies from
    the object files (.js_o, js_p_o), which contain linkable
    units with dependency information
-}

module Gen2.Linker where

import           DynFlags
import           Encoding
import           Panic
import           Module ( InstalledUnitId
                        , stringToInstalledUnitId
                        , installedUnitIdString
                        , toInstalledUnitId
                        , mkModuleName, wiredInUnitIds
                        , moduleNameString
                        , primUnitId )
import           PackageConfig ({-sourcePackageId, -}unitId)
-- import           Outputable (ppr, showSDoc)
import qualified Packages

import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception        (evaluate)
import           Control.Monad
import           Control.Parallel.Strategies
import Prelude

import           Data.Array
import qualified Data.Aeson               as Aeson
import           Data.Binary
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Function            (on)
import qualified Data.HashMap.Strict      as HM
import           Data.Int
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IS
import           Data.IORef
import           Data.List
  (partition, nub, foldl', intercalate, group, sort, groupBy, isSuffixOf, find)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe
-- import           Data.Monoid
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.IO        as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Data.Vector              as V

-- import           Data.Yaml                (FromJSON(..), Value(..))
-- import qualified Data.Yaml                as Yaml
import           Data.Aeson                (FromJSON(..), Value(..))

import qualified Distribution.Simple.Utils as Cabal

import           GHC.Generics

import           System.FilePath
  (splitPath, (<.>), (</>), dropExtension)

import System.IO

import           System.Directory
  ( createDirectoryIfMissing, canonicalizePath
  , doesFileExist, getCurrentDirectory, copyFile)
import           Text.PrettyPrint.Leijen.Text (displayT, renderPretty)

import           Compiler.Compat
import           Compiler.Info
import           Compiler.JMacro
import           Compiler.Settings
import           Compiler.Utils

-- import qualified Gen2.Archive             as Ar
import qualified Ar
import           Gen2.Base
import           Gen2.ClosureInfo         hiding (Fun)
import qualified Gen2.Compactor           as Compactor
import           Gen2.Object
import qualified Gen2.Object              as Object
import           Gen2.Printer             (pretty)
import           Gen2.Rts                 (rtsText, rtsDeclsText)
import           Gen2.RtsTypes
import           Gen2.Shim

import qualified Data.ByteString.Char8 as CS8 (pack, unpack)
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Base16 as BS16


import qualified Compiler.Platform as Platform

type LinkableUnit = (Package, Module, Int) -- ^ module and the index of the block in the object file
type Module       = Text

-- number of bytes linked per module
type LinkerStats  = Map (Package, Module) Int64

-- | result of a link pass
data LinkResult = LinkResult
  { linkOut         :: BL.ByteString -- ^ compiled Haskell code
  , linkOutStats    :: LinkerStats   -- ^ statistics about generated code
  , linkOutMetaSize :: Int64         -- ^ size of packed metadata in generated code
  , linkForeignRefs :: [ForeignRef]  -- ^ foreign code references in compiled haskell code
  , linkLibRTS      :: [FilePath]    -- ^ library code to load with the RTS
  , linkLibA        :: [FilePath]    -- ^ library code to load after RTS
  , linkLibAArch    :: [FilePath]    -- ^ library code to load from archives after RTS
  , linkBase        :: Base          -- ^ base metadata to use if we want to link incrementally against this result
  } deriving (Generic)

instance Binary LinkResult


newtype ArchiveState = ArchiveState { loadedArchives :: IORef (Map FilePath Ar.Archive) }

emptyArchiveState :: IO ArchiveState
emptyArchiveState = ArchiveState <$> newIORef M.empty

-- | link and write result to disk (jsexe directory)
link :: DynFlags
     -> GhcjsEnv
     -> GhcjsSettings
     -> FilePath                   -- ^ output file/directory
     -> [FilePath]                 -- ^ include path for home package
     -> [InstalledUnitId]          -- ^ packages to link
     -> [LinkedObj]                -- ^ the object files we're linking
     -> [FilePath]                 -- ^ extra js files to include
     -> (Fun -> Bool)              -- ^ functions from the objects to use as roots (include all their deps)
     -> Set Fun                    -- ^ extra symbols to link in
     -> IO ()
link dflags env settings out include pkgs objFiles jsFiles isRootFun extraStaticDeps
  | gsNoJSExecutables settings = return ()
  | otherwise = do
      LinkResult lo lstats lmetasize lfrefs llW lla llarch lbase <-
        link' dflags env settings out include pkgs objFiles jsFiles
              isRootFun extraStaticDeps
      let genBase = isJust (gsGenBase settings)
          jsExt | genBase   = "base.js"
                | otherwise = "js"
      createDirectoryIfMissing False out
      BL.writeFile (out </> "out" <.> jsExt) lo
      unless (gsOnlyOut settings) $ do
        let frefsFile   = if genBase then "out.base.frefs" else "out.frefs"
            jsonFrefs  = Aeson.encode lfrefs
        BL.writeFile (out </> frefsFile <.> "json") jsonFrefs
        BL.writeFile (out </> frefsFile <.> "js")
                     ("h$checkForeignRefs(" <> jsonFrefs <> ");")
        unless (gsNoStats settings) $ do
          let statsFile = if genBase then "out.base.stats" else "out.stats"
          TL.writeFile (out </> statsFile) (linkerStats lmetasize lstats)
        unless (gsNoRts settings) $ do
          withRts <- mapM (tryReadShimFile dflags) llW
          BL.writeFile (out </> "rts.js")
            (TLE.encodeUtf8 rtsDeclsText <>
             BL.fromChunks withRts <>
             TLE.encodeUtf8 (rtsText' dflags $ dfCgSettings dflags))
        lla'    <- mapM (tryReadShimFile dflags) lla
        llarch' <- mapM (readShimsArchive dflags) llarch
        BL.writeFile (out </> "lib" <.> jsExt)
                     (BL.fromChunks $ llarch' ++ lla')
        if genBase
          then generateBase out lbase
          else when (not (gsOnlyOut settings) &&
                     not (gsNoRts settings) &&
                     not (usingBase settings)) $ do
                 combineFiles dflags out
                 writeHtml dflags out
                 writeRunMain dflags out
                 writeRunner settings dflags out
                 writeWebAppManifest dflags out
                 writeExterns out

-- | link in memory
link' :: DynFlags
      -> GhcjsEnv
      -> GhcjsSettings
      -> String                     -- ^ target (for progress message)
      -> [FilePath]                 -- ^ include path for home package
      -> [InstalledUnitId]          -- ^ packages to link
      -> [LinkedObj]                -- ^ the object files we're linking
      -> [FilePath]                 -- ^ extra js files to include
      -> (Fun -> Bool)              -- ^ functions from the objects to use as roots (include all their deps)
      -> Set Fun                    -- ^ extra symbols to link in
      -> IO LinkResult
link' dflags env settings target _include pkgs objFiles jsFiles isRootFun extraStaticDeps = do
      (objDepsMap, objRequiredUnits) <- loadObjDeps objFiles
      let rootSelector | Just baseMod <- gsGenBase settings =
                           \(Fun _p m _s) -> m == T.pack baseMod
                       | otherwise = isRootFun
          roots = S.fromList . filter rootSelector $
            concatMap (M.keys . depsHaskellExported . fst) (M.elems objDepsMap)
          rootMods = map (T.unpack . head) . group . sort . map funModule . S.toList $ roots
          objPkgs = map toPackageKey $ nub (map fst $ M.keys objDepsMap)
      compilationProgressMsg dflags $
        case gsGenBase settings of
          Just baseMod -> "Linking base bundle " ++ target ++ " (" ++ baseMod ++ ")"
          _            -> "Linking " ++ target ++ " (" ++ intercalate "," rootMods ++ ")"
      base <- case gsUseBase settings of
        NoBase        -> return emptyBase
        BaseFile file -> Compactor.loadBase file
        BaseState b   -> return b
      (rdPkgs, rds) <- rtsDeps dflags
      -- c   <- newMVar M.empty
      let rtsPkgs     =  map stringToInstalledUnitId
                             ["@rts", "@rts_" ++ buildTag dflags]
          pkgs' :: [InstalledUnitId]
          pkgs'       = nub (rtsPkgs ++ rdPkgs ++ reverse objPkgs ++ reverse pkgs)
          pkgs''      = filter (not . (isAlreadyLinked base)) pkgs'
          -- pkgLibPaths = mkPkgLibPaths pkgs'
          -- getPkgLibPaths :: InstalledUnitId -> ([FilePath],[String])
          -- getPkgLibPaths k = fromMaybe ([],[]) (lookup k pkgLibPaths)
      (archsDepsMap, archsRequiredUnits) <- loadArchiveDeps env =<<
          getPackageArchives dflags (map snd $ mkPkgLibPaths pkgs')
      pkgArchs <- getPackageArchives dflags (map snd $ mkPkgLibPaths pkgs'')
      (allDeps, code) <-
        collectDeps dflags
                    (objDepsMap `M.union` archsDepsMap)
                    (pkgs' ++ [thisInstalledUnitId dflags])
                    (baseUnits base)
                    (roots `S.union` rds `S.union` extraStaticDeps)
                    (archsRequiredUnits ++ objRequiredUnits)
      let (outJs, metaSize, compactorState, stats) =
             renderLinker settings dflags (baseCompactorState base) rds code
          base'  = Base compactorState (nub $ basePkgs base ++ map mkPackage pkgs'')
                         (allDeps `S.union` baseUnits base)
      (alreadyLinkedBefore, alreadyLinkedAfter) <- getShims dflags [] (filter (isAlreadyLinked base) pkgs')
      (shimsBefore, shimsAfter) <- getShims dflags jsFiles pkgs''
      return $ LinkResult outJs stats metaSize
                 (concatMap (\(_,_,_,_,_,_,r) -> r) code)
                 (filter (`notElem` alreadyLinkedBefore) shimsBefore)
                 (filter (`notElem` alreadyLinkedAfter)  shimsAfter)
                 pkgArchs base'
  where
    isAlreadyLinked :: Base -> InstalledUnitId -> Bool
    isAlreadyLinked b pkg = mkPackage pkg `elem` basePkgs b

    mkPkgLibPaths :: [InstalledUnitId] -> [(InstalledUnitId, ([FilePath],[String]))]
    mkPkgLibPaths
      = map (\k -> ( k
                   , (getInstalledPackageLibDirs dflags k
                     , getInstalledPackageHsLibs dflags k)
                   ))

renderLinker :: GhcjsSettings
             -> DynFlags
             -> CompactorState
             -> Set Fun
             -> [(Package, Module, JStat, Text, [ClosureInfo], [StaticInfo], [ForeignRef])] -- ^ linked code per module
             -> (BL.ByteString, Int64, CompactorState, LinkerStats)
renderLinker settings dflags renamerState rtsDeps code =
  let (renamerState', compacted, meta) = Compactor.compact settings dflags renamerState (map funSymbol $ S.toList rtsDeps) (map (\(_,_,s,_,ci,si,_) -> (s,ci,si)) code)
      pe = TLE.encodeUtf8 . (<>"\n") . displayT . renderPretty 0.8 150 . pretty
      rendered  = parMap rdeepseq pe compacted
      renderedMeta = pe meta
      renderedExports = TLE.encodeUtf8 . TL.fromStrict . T.unlines . filter (not . T.null) $ map (\(_,_,_,rs,_,_,_) -> rs) code
      mkStat (p,m,_,_,_,_,_) b = ((p,m), BL.length b)
  in ( mconcat rendered <> renderedMeta <> renderedExports
     , BL.length renderedMeta
     , renamerState'
     , M.fromList $ zipWith mkStat code rendered
     )

linkerStats :: Int64         -- ^ code size of packed metadata
            -> LinkerStats   -- ^ code size per module
            -> TL.Text
linkerStats meta s =
  TL.intercalate "\n\n" [packageStats, moduleStats, metaStats] <> "\n\n"
  where
    ps = M.fromListWith (+) . map (\((p,_),s) -> (p,s)) . M.toList $ s
    pad n t = let l = TL.length t
              in  if l < n then t <> TL.replicate (n-l) " " else t
    pkgMods = groupBy ((==) `on` (fst . fst)) (M.toList s)
    showMod ((_,m),s) = pad 40 ("    " <> TL.fromStrict m <> ":") <> TL.pack (show s)
    packageStats = "code size summary per package:\n\n" <>
      TL.unlines (map (\(p,s) -> pad 25 (showPkg p <> ":") <> TL.pack (show s)) $ M.toList ps)
    moduleStats = "code size per module:\n\n" <> TL.unlines
      (map (\xs@(((p,_),_):_) -> showPkg p <> "\n" <> TL.unlines (map showMod xs)) pkgMods)
    metaStats = "packed metadata: " <> TL.pack (show meta)

rtsText' :: DynFlags -> CgSettings -> TL.Text
rtsText' = rtsText
{- prerender RTS for faster linking (fixme this results in a build error, why?)
rtsText' debug = if debug
                   then TL.pack $ $(runQ $ litE (StringL . TL.unpack . rtsText $ True))
                   else TL.pack $ $(runQ $ litE (StringL . TL.unpack . rtsText $ False))
-}

splitPath' :: FilePath -> [FilePath]
splitPath' = map (filter (`notElem` ("/\\"::String))) . splitPath

getPackageArchives :: DynFlags -> [([FilePath],[String])] -> IO [FilePath]
getPackageArchives dflags pkgs =
  filterM doesFileExist [ p </> "lib" ++ l ++ profSuff <.> "a"
                        | (paths, libs) <- pkgs, p <- paths, l <- libs ]
  where
    -- XXX the profiling library name is probably wrong now
    profSuff | WayProf `elem` ways dflags = "_p"
             | otherwise                  = ""

-- fixme the wired-in package id's we get from GHC we have no version
getShims :: DynFlags -> [FilePath] -> [InstalledUnitId] -> IO ([FilePath], [FilePath])
getShims dflags extraFiles pkgDeps = do
  (w,a) <- collectShims (getLibDir dflags </> "shims")
                        (map (convertPkg dflags) pkgDeps)
  extraFiles' <- mapM canonicalizePath extraFiles
  return (w, a++extraFiles')

convertPkg :: DynFlags -> InstalledUnitId -> (Text, Version)
convertPkg dflags p
  = case getInstalledPackageVersion dflags p of
      Just v -> (T.pack (getInstalledPackageName dflags p), v)
      -- special or wired-in
      Nothing -> (T.pack (installedUnitIdString p), Version [])

{- | convenience: combine rts.js, lib.js, out.js to all.js that can be run
     directly with node.js or SpiderMonkey jsshell
 -}
combineFiles :: DynFlags -> FilePath -> IO ()
combineFiles dflags fp = do
  files   <- mapM (B.readFile.(fp</>)) ["rts.js", "lib.js", "out.js"]
  runMain <- if   gopt Opt_NoHsMain dflags
             then pure mempty
             else B.readFile (getLibDir dflags </> "runmain.js")
  writeBinaryFile (fp</>"all.js") (mconcat (files ++ [runMain]))

-- | write the index.html file that loads the program if it does not exit
writeHtml :: DynFlags -> FilePath -> IO ()
writeHtml df out = do
  e <- doesFileExist htmlFile
  unless e $
    B.readFile (getLibDir df </>"template.html") >>= B.writeFile htmlFile
  where
    htmlFile = out </> "index.html"

-- | write the runmain.js file that will be run with defer so that it runs after index.html is loaded
writeRunMain :: DynFlags -> FilePath -> IO ()
writeRunMain df out = do
  e <- doesFileExist runMainFile
  unless e $
    B.readFile (getLibDir df </> "runmain.js") >>= B.writeFile runMainFile
  where
    runMainFile = out </> "runmain.js"

writeRunner :: GhcjsSettings -> DynFlags -> FilePath -> IO ()
writeRunner settings dflags out = 
  {-when (gsBuildRunner settings) $ -} do
  cd    <- getCurrentDirectory
  let runner = cd </> addExeExtension (dropExtension out)
      srcFile = out </> "all" <.> "js"
  -- nodeSettings <- readNodeSettings dflags
  let nodePgm = "node" -- XXX we don't read nodeSettings.json anymore, we should somehow know how to find node?
  if Platform.isWindows
  then do
    copyFile (topDir dflags </> "bin" </> "wrapper" <.> "exe")
             runner
    T.writeFile (runner <.> "options") $ T.unlines
                [ T.pack nodePgm -- T.pack (nodeProgram nodeSettings)
                , T.pack ("{{EXEPATH}}" </> out </> "all" <.> "js")
                ]
  else do
    src <- readBinaryFile (cd </> srcFile)
    -- let pgm = TE.encodeUtf8 (T.pack $ nodeProgram nodeSettings)
    B.writeFile runner ("#!" <> TE.encodeUtf8 (T.pack nodePgm) <> "\n" <> src)
    Cabal.setFileExecutable runner

-- | write the manifest.webapp file that for firefox os
writeWebAppManifest :: DynFlags -> FilePath -> IO ()
writeWebAppManifest df out = do
  e <- doesFileExist manifestFile
  unless e $
    B.readFile (getLibDir df </> "manifest.webapp") >>= B.writeFile manifestFile
  where
    manifestFile = out </> "manifest.webapp"

rtsExterns :: Text
rtsExterns =
  "// GHCJS RTS externs for closure compiler ADVANCED_OPTIMIZATIONS\n\n" <>
  mconcat (map (\x -> "/** @type {*} */\nObject.d" <> T.pack (show x) <> ";\n")
               [(7::Int)..16384])

writeExterns :: FilePath -> IO ()
writeExterns out = T.writeFile (out </> "all.js.externs") rtsExterns

-- | get all functions in a module
modFuns :: Deps -> [Fun]
modFuns (Deps _p _m _r e _b) = M.keys e

-- | get all dependencies for a given set of roots
getDeps :: Map (Package,Module) Deps -- ^ loaded deps
        -> Set LinkableUnit -- ^ don't link these blocks
        -> Set Fun          -- ^ start here
        -> [LinkableUnit]   -- ^ and also link these
        -> IO (Set LinkableUnit)
getDeps lookup base fun startlu = go' S.empty (S.fromList startlu) (S.toList fun)
  where
    go :: Set LinkableUnit
       -> Set LinkableUnit
       -> IO (Set LinkableUnit)
    go result open = case S.minView open of
      Nothing -> return result
      Just (lu@(lpkg,lmod,n), open') ->
          let key = (lpkg, lmod)
          in  case M.lookup (lpkg,lmod) lookup of
                Nothing -> error ("getDeps.go: object file not loaded for:  " ++ show key)
                Just (Deps _ _ _ _ b) ->
                  let block = b!n
                      result' = S.insert lu result
                  in go' result'
                         (addOpen result' open' $ map (lpkg,lmod,) (blockBlockDeps block))
                         (blockFunDeps block)

    go' :: Set LinkableUnit
        -> Set LinkableUnit
        -> [Fun]
        -> IO (Set LinkableUnit)
    go' result open [] = go result open
    go' result open (f:fs) =
        let key = (funPackage f, funModule f)
        in  case M.lookup key lookup of
              Nothing -> error ("getDeps.go': object file not loaded for:  " ++ show key)
              Just (Deps _p _m _r e _b) ->
                 let lun :: Int
                     lun = fromMaybe (error $ "exported function not found: " ++ show f)
                                     (M.lookup f e)
                     lu  = (funPackage f, funModule f, lun)
                 in  go' result (addOpen result open [lu]) fs

    addOpen :: Set LinkableUnit -> Set LinkableUnit -> [LinkableUnit]
            -> Set LinkableUnit
    addOpen result open newUnits =
      let alreadyLinked s = S.member s result ||
                            S.member s open   ||
                            S.member s base
      in  open `S.union` (S.fromList $ filter (not . alreadyLinked) newUnits)

-- | collect dependencies for a set of roots
collectDeps :: DynFlags
            -> Map (Package, Module) (Deps, DepsLocation)
            -> [InstalledUnitId]     -- ^ packages, code linked in this order
            -> Set LinkableUnit -- ^ do not include these
            -> Set Fun -- ^ roots
            -> [LinkableUnit] -- ^ more roots
            -> IO ( Set LinkableUnit
                  , [(Package, Module, JStat, Text, [ClosureInfo], [StaticInfo], [ForeignRef])]
                  )
collectDeps _dflags lookup packages base roots units = do
  allDeps <- getDeps (fmap fst lookup) base roots units
  -- read ghc-prim first, since we depend on that for static initialization
  let packages' = uncurry (++) $ partition (==(toInstalledUnitId primUnitId)) (nub packages)
      unitsByModule :: Map (Package, Module) IntSet
      unitsByModule = M.fromListWith IS.union $
                      map (\(p,m,n) -> ((p,m),IS.singleton n)) (S.toList allDeps)
      lookupByPkg :: Map Package [(Deps, DepsLocation)]
      lookupByPkg = M.fromListWith (++) (map (\((p,_m),v) -> (p,[v])) (M.toList lookup))
  ar_state <- emptyArchiveState
  code <- fmap (catMaybes . concat) . forM packages' $ \pkg ->
    mapM (uncurry $ extractDeps ar_state unitsByModule)
         (fromMaybe [] $ M.lookup (mkPackage pkg) lookupByPkg)
  return (allDeps, code)

extractDeps :: ArchiveState
            -> Map (Package, Module) IntSet
            -> Deps
            -> DepsLocation
            -> IO (Maybe (Package, Module, JStat, Text, [ClosureInfo], [StaticInfo], [ForeignRef]))
extractDeps ar_state units deps loc =
  case M.lookup (pkg, mod) units of
    Nothing       -> return Nothing
    Just modUnits -> do
      let selector n _  = n `IS.member` modUnits || isGlobalUnit n
      x <- case loc of
        ObjectFile o  -> collectCode =<< readObjectFileKeys selector o
        ArchiveFile a -> collectCode =<<
                            (readObjectKeys (a ++ ':':T.unpack mod) selector <$>
                              readArObject ar_state mod a)
                            --  error ("Ar.readObject: " ++ a ++ ':' : T.unpack mod))
                            -- Ar.readObject (mkModuleName $ T.unpack mod) a)
        InMemory n b  -> collectCode $
                            readObjectKeys n selector (BL.fromStrict b)
      evaluate (rnf x)
      return x
  where
    pkg           = depsPackage deps
    mod           = depsModule deps
    collectCode l = let x = ( pkg
                            , mod
                            , mconcat (map oiStat l)
                            , T.unlines (map oiRaw l)
                            , concatMap oiClInfo l
                            , concatMap oiStatic l
                            , concatMap oiFImports l)
                    in evaluate (rnf x) >> return (Just x)

readArObject :: ArchiveState -> Text -> FilePath -> IO BL.ByteString
readArObject ar_state mod_name ar_file = do
  loaded_ars <- readIORef (loadedArchives ar_state)
  ar@(Ar.Archive entries) <- case M.lookup ar_file loaded_ars of
    Just a -> pure a
    Nothing -> do
      a <- Ar.loadAr ar_file
      modifyIORef (loadedArchives ar_state) (M.insert ar_file a)
      pure a
  let tag = Object.moduleNameTag mod_name
      matchTag entry
        | Right hdr <- Object.getHeader (BL.fromStrict $ Ar.filedata entry)
        = Object.hdrModuleName hdr == tag
        | otherwise = False

  -- XXX this shouldn't be an exception probably    
  pure $ fromMaybe (error $ "could not find object for module " ++ T.unpack mod_name ++ " in " ++ ar_file)
                   (fmap (BL.fromStrict . Ar.filedata) (find matchTag entries))
  -- mapM_ (\e -> putStrLn ("found file: " ++ Ar.filename e)) entries

mkPackage :: InstalledUnitId -> Package
mkPackage pk = Package (T.pack $ installedUnitIdString pk)

toPackageKey :: Package -> InstalledUnitId
toPackageKey = stringToInstalledUnitId . T.unpack . unPackage

{- | Static dependencies are symbols that need to be linked regardless
     of whether the linked program refers to them. For example
     depenencies that the RTS uses or symbols that the user program
     refers to directly
 -}
newtype StaticDeps =
  StaticDeps { unStaticDeps :: [(Text, Text, Text)] -- package/module/symbol
             }

noStaticDeps :: StaticDeps
noStaticDeps = StaticDeps []

{- | The input file format for static deps is a yaml document with a
     package/module/symbol tree where symbols can be either a list or
     just a single string, for example:

     base:
       GHC.Conc.Sync:          reportError
       Control.Exception.Base: nonTermination
     ghcjs-prim:
       GHCJS.Prim:
         - JSVal
         - JSException
 -}
instance FromJSON StaticDeps where
  parseJSON (Object v) = StaticDeps . concat <$> mapM (uncurry parseMod) (HM.toList v)
    where
      parseMod p (Object v) = concat <$> mapM (uncurry (parseSymb p)) (HM.toList v)
      parseMod _ _          = mempty
      parseSymb p m (String s) = pure [(p,m,s)]
      parseSymb p m (Array v)  = mapM (parseSingleSymb p m) (V.toList v)
      parseSymb _ _ _          = mempty
      parseSingleSymb p m (String s) = pure (p,m,s)
      parseSingleSymb _ _ _          = mempty
  parseJSON _          = mempty

-- | dependencies for the RTS, these need to be always linked
rtsDeps :: DynFlags -> IO ([InstalledUnitId], Set Fun)
rtsDeps dflags = readSystemDeps dflags
                                "RTS"
                                "linking"
                                "rtsdeps.yaml"

-- | dependencies for the Template Haskell, these need to be linked when running
--   Template Haskell (in addition to the RTS deps)
thDeps :: DynFlags -> IO ([InstalledUnitId], Set Fun)
thDeps dflags = readSystemDeps dflags
                               "Template Haskell"
                               "running Template Haskell"
                               "thdeps.yaml"

readSystemDeps :: DynFlags
               -> String
               -> String
               -> FilePath
               -> IO ([InstalledUnitId], Set Fun)
readSystemDeps dflags depsName requiredFor file
  -- hardcode contents to get rid of yaml dep
  -- XXX move runTHServer to some suitable wired-in package
  -- | True = pure ([], mempty) -- XXX to fix booting
  | file == "thdeps.yaml" = pure ( [stringToInstalledUnitId "base"]
                                 , S.fromList $ d "base" "GHCJS.Prim.TH.Eval" ["runTHServer"])
  | file == "rtsdeps.yaml" = pure ( [stringToInstalledUnitId "base"
                                    , stringToInstalledUnitId "ghc-prim"
                                    , stringToInstalledUnitId "integer-wired-in"
                                    ]
                                  , S.fromList $ concat
                                  [ d "base" "GHC.Conc.Sync" ["reportError"]
                                  , d "base" "Control.Exception.Base" ["nonTermination"]
                                  , d "base" "GHC.Exception.Type" ["SomeException"]
                                  , d "base" "GHC.TopHandler" ["runMainIO", "topHandler"]
                                  , d "base" "GHC.Base" ["$fMonadIO"]
                                  , d "base" "GHC.Maybe" ["Nothing", "Just"]
                                  , d "base" "GHC.Ptr" ["Ptr"]
                                  , d "ghc-prim" "GHC.Types" [":", "[]"]
                                  , d "ghc-prim" "GHC.Tuple" ["(,)", "(,,)", "(,,,)", "(,,,,)", "(,,,,,)","(,,,,,,)", "(,,,,,,,)", "(,,,,,,,,)", "(,,,,,,,,,)"]
                                  , d "integer-wired-in" "GHC.Integer.Type" ["S#", "Jp#", "Jn#"]
                                  , d "ghc-prim" "GHC.Types" [ "JSVal" ]
                                  , d "base" "GHCJS.Prim" ["JSException", "$fShowJSException", "$fExceptionJSException", "resolve", "resolveIO", "toIO"]
                                  , d "base" "GHCJS.Prim.Internal" ["wouldBlock", "blockedIndefinitelyOnMVar", "blockedIndefinitelyOnSTM", "ignoreException", "setCurrentThreadResultException", "setCurrentThreadResultValue"]
                                  ]
                                  )
  | otherwise = pure ([], mempty)
  where
    d pkg m syms = map (\s -> (Fun (Package $ T.pack pkg) m) (mkHaskellSym (stringToInstalledUnitId pkg) m s)) syms
    zenc  = T.pack . zEncodeString . T.unpack
    mkHaskellSym :: InstalledUnitId -> Text -> Text -> Text
    mkHaskellSym p m s = "h$" <> zenc (T.pack (encodeInstalledUnitId dflags p) <> ":" <> m <> "." <> s)

{-
  b  <- readBinaryFile (getLibDir dflags </> file)
  wi <- readSystemWiredIn dflags
  case Yaml.decodeEither b of
    Left err -> panic $ "could not read " ++ depsName ++
                        " dependencies from " ++ file ++ ":\n" ++ err
    Right sdeps ->
      let (StaticDeps unresolved, pkgs, funs) = staticDeps dflags wi sdeps
      in  case unresolved of
            ((p,_,_):_) ->
                  panic $ "Package `" ++ T.unpack p ++ "' is required for " ++
                          requiredFor ++ ", but was not found"
            _ ->
              -- putStrLn "system dependencies:"
              -- print (map installedUnitIdString pkgs, funs)
              return (pkgs, funs)

-}

readSystemWiredIn :: DynFlags -> IO [(Text, InstalledUnitId)]
readSystemWiredIn _ = pure [] -- XXX
{-
readSystemWiredIn dflags = do
  b <- B.readFile filename
  case Yaml.decodeEither b of
     Left _err -> error $ "could not read wired-in package keys from " ++ filename
     Right m  -> return . M.toList
                        . M.union ghcWiredIn -- GHC wired-in package keys override those in the file
                        . fmap stringToInstalledUnitId $ m
  where
    filename = getLibDir dflags </> "wiredinkeys" <.> "yaml"
    ghcWiredIn :: Map Text InstalledUnitId
    ghcWiredIn = M.fromList $ map (\k -> (T.pack (installedUnitIdString k), k))
                                  (map toInstalledUnitId wiredInUnitIds)
                                  -}
{- | read a static dependencies specification and give the roots

     if dependencies come from a versioned (non-hardwired) package
     that is linked multiple times, then the returned dependencies
     will all come from the same version, but it's undefined which one.
 -}

type SDep = (Text, Text, Text)

staticDeps :: DynFlags
           -> [(Text, InstalledUnitId)]    -- ^ wired-in package names / keys
           -> StaticDeps              -- ^ deps from yaml file
           -> (StaticDeps, [InstalledUnitId], Set Fun)
                                      -- ^ the StaticDeps contains the symbols
                                      --   for which no package could be found
staticDeps dflags wiredin sdeps = mkDeps sdeps
  where
    zenc  = T.pack . zEncodeString . T.unpack
    mkDeps (StaticDeps ds) =
      let (u, p, r) = foldl' resolveDep ([], S.empty, S.empty) ds
      in  (StaticDeps u, S.toList (closePackageDeps dflags p), r)
    resolveDep :: ([SDep], Set InstalledUnitId, Set Fun)
               -> SDep
               -> ([SDep], Set InstalledUnitId, Set Fun)
    resolveDep (unresolved, pkgs, resolved) dep@(p, m, s) =
      case lookup p wiredin of
             Nothing -> ( dep : unresolved, pkgs, resolved)
             Just k  -> case Packages.lookupInstalledPackage dflags k of
               Nothing -> error $ "Package key for wired-in dependency `" ++
                                  T.unpack p ++ "' could not be found: "  ++
                                  installedUnitIdString k
               Just conf ->
                 let k' = unitId conf
                 in  ( unresolved
                     , S.insert k' pkgs
                     , S.insert (Fun (mkPackage k') m $ mkSymb k' m s)
                                resolved
                     )
    mkSymb :: InstalledUnitId -> Text -> Text -> Text
    mkSymb p m s  =
      "h$" <> zenc (T.pack (encodeInstalledUnitId dflags p) <> ":" <> m <> "." <> s)

closePackageDeps :: DynFlags -> Set InstalledUnitId -> Set InstalledUnitId
closePackageDeps dflags pkgs
  | S.size pkgs == S.size pkgs' = pkgs
  | otherwise                   = closePackageDeps dflags pkgs'
  where
    pkgs' = pkgs `S.union` S.fromList (concatMap deps $ S.toList pkgs)
    notFound = error "closePackageDeps: package not found"
    deps :: InstalledUnitId -> [InstalledUnitId]
    deps =
--           map (Packages.resolveInstalledPackageId dflags)
           Packages.depends
         . fromMaybe notFound
         . Packages.lookupInstalledPackage dflags

-- read all dependency data from the to-be-linked files
loadObjDeps :: [LinkedObj] -- ^ object files to link
            -> IO (Map (Package, Module) (Deps, DepsLocation), [LinkableUnit])
loadObjDeps objs = prepareLoadedDeps <$> mapM readDepsFile' objs

loadArchiveDeps :: GhcjsEnv
                -> [FilePath]
                -> IO ( Map (Package, Module) (Deps, DepsLocation)
                      , [LinkableUnit]
                      )
loadArchiveDeps env archives = modifyMVar (linkerArchiveDeps env) $ \m ->
  case M.lookup archives' m of
    Just r  -> return (m, r)
    Nothing -> loadArchiveDeps' archives >>= \r -> return (M.insert archives' r m, r)
  where
     archives' = S.fromList archives

loadArchiveDeps' :: [FilePath]
                 -> IO ( Map (Package, Module) (Deps, DepsLocation)
                       , [LinkableUnit]
                       )
loadArchiveDeps' archives = do
  archDeps <- forM archives $ \file -> do
    ar@(Ar.Archive entries) <- Ar.loadAr file
    pure (catMaybes $ map (readEntry file) entries)
  return (prepareLoadedDeps $ concat archDeps)
    where
      readEntry :: FilePath -> Ar.ArchiveEntry -> Maybe (Deps, DepsLocation)
      readEntry ar_file ar_entry
        | isObjFile (Ar.filename ar_entry) =
            fmap (,ArchiveFile ar_file)
                 (readDepsMaybe (ar_file ++ ':':Ar.filename ar_entry) (BL.fromStrict $ Ar.filedata ar_entry))
        | otherwise = Nothing


isObjFile :: FilePath -> Bool
isObjFile file = ".o" `isSuffixOf` file || -- vanilla
                 "_o" `isSuffixOf` file    -- some "Way", like .p_o

prepareLoadedDeps :: [(Deps, DepsLocation)]
                  -> ( Map (Package, Module) (Deps, DepsLocation)
                     , [LinkableUnit]
                     )
prepareLoadedDeps deps =
  let req     = concatMap (requiredUnits . fst) deps
      depsMap = M.fromList $ map (\d -> ((depsPackage (fst d)
                                         ,depsModule (fst d)), d))
                                 deps
  in  (depsMap, req)

requiredUnits :: Deps -> [LinkableUnit]
requiredUnits d = map (depsPackage d, depsModule d,)
                      (IS.toList $ depsRequired d)

-- read dependencies from an object that might have already been into memory
-- pulls in all Deps from an archive
readDepsFile' :: LinkedObj -> IO (Deps, DepsLocation)
readDepsFile' (ObjLoaded name bs) = pure . (,InMemory name bs) $
                                    readDeps name (BL.fromStrict bs)
readDepsFile' (ObjFile file)      =
  (,ObjectFile file) <$> readDepsFile file

generateBase :: FilePath -> Base -> IO ()
generateBase outDir b =
  BL.writeFile (outDir </> "out.base.symbs") (Compactor.renderBase b)
