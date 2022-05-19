{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
----------------------------- FIXMEs -------------------------------------------
-- FIXME: Jeff (2022,03): Finish module description. Specifically:
-- 1. What are the important modules this module uses
-- 2. Who is the consumer for this module (hint: DynamicLinking)
-- 3. What features are missing due to the implementation in this module? For
-- example, Are we blocked from linking foreign imports due to some code in this
-- module?
--
--  - add ForeignRefs imports in @link@
--  - factor out helper functions in @link'@
--  - remove @head@ function in @link'@
--  - remove @ue_unsafeHomeUnit@ function in @link'@
--  - use newtypes instead of strings for output directories in @writeRunner@
--  - add support for windows in @writeRunner@
--  - resolve strange unpack call in @writeExterns@ the right thing to do here
--      might be to just remove it
--  - fix: @collectDeps@ inputs a [UnitId], but [] is unordered yet comments in
--      @collectDeps@ indicate a specific ordering is needed. This ordering
--      should be enforced in some data structure other than [] which is
--      obviously ordered but in an undefined and ad-hoc way
--  - fix: For most of the Linker I pass around UnitIds, I (Jeff) am unsure if
--      these should really be modules. Or to say this another way is UnitId the
--      right abstraction level? Or is Module? Or some other unit thing?
--  - fix: Gen2.GHCJS used NFData instances over a lot of types. Replicating
--      these instances would mean adding a Generic and NFData instance to some
--      internal GHC types. I (Jeff) do not think we want to do that. Instead, we
--      should use strict data structures as a default and then implement lazy
--      ones where it makes sense and only if it makes sense. IMHO Gen2.GHCJS was
--      overly lazy and we should avoid repeating that here. Let profiling be our
--      guide during our performance refactoring.
--  - Employ the type system more effectively for @readSystemDeps'@, in
--      particular get rid of the string literals
--  - fix foldl' memory leak in @staticDeps@
--  - move @mkSymb@
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Linker where

import           GHC.StgToJS.Linker.Types
import           GHC.StgToJS.Linker.Utils
import           GHC.StgToJS.Linker.Compactor

import           GHC.StgToJS.Rts.Rts

import           GHC.JS.Syntax

import           GHC.StgToJS.Object
import           GHC.StgToJS.Types hiding (LinkableUnit)
import           GHC.StgToJS.UnitUtils
import           GHC.StgToJS.Printer

import qualified GHC.SysTools.Ar          as Ar
import           GHC.Utils.Encoding
import           GHC.Utils.Outputable (ppr, text)
import           GHC.Utils.Panic
import           GHC.Unit.State
import           GHC.Unit.Env
import           GHC.Unit.Home
import           GHC.Unit.Types
import           GHC.Utils.Error
import           GHC.Platform.Ways
import           GHC.Driver.Env.Types
import           GHC.Data.ShortText       (ShortText)
import qualified GHC.Data.ShortText       as T

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

import           GHC.Generics (Generic)

import           System.FilePath (splitPath, (<.>), (</>), dropExtension)
import           System.IO
import           System.Directory ( createDirectoryIfMissing
                                  , doesFileExist
                                  , getCurrentDirectory
                                  , Permissions(..)
                                  , setPermissions
                                  , getPermissions
                                  )

import Prelude
import GHC.Driver.Session (targetWays_, settings, DynFlags)
import GHC.Settings (sTopDir)
import GHC.Unit.Module.Name
import GHC.Unit.Module (moduleStableString)
import GHC.Utils.Logger (Logger)

-- number of bytes linked per module
type LinkerStats  = Map Module Int64

-- | result of a link pass
data LinkResult = LinkResult
  { linkOut         :: BL.ByteString -- ^ compiled Haskell code
  , linkOutStats    :: LinkerStats   -- ^ statistics about generated code
  , linkOutMetaSize :: Int64         -- ^ size of packed metadata in generated code
  , linkForeignRefs :: [ForeignJSRef]  -- ^ foreign code references in compiled haskell code
  , linkLibRTS      :: [FilePath]    -- ^ library code to load with the RTS
  , linkLibA        :: [FilePath]    -- ^ library code to load after RTS
  , linkLibAArch    :: [FilePath]    -- ^ library code to load from archives after RTS
  , linkBase        :: Base          -- ^ base metadata to use if we want to link incrementally against this result
  } deriving (Generic)

newtype ArchiveState = ArchiveState { loadedArchives :: IORef (Map FilePath Ar.Archive) }

emptyArchiveState :: IO ArchiveState
emptyArchiveState = ArchiveState <$> newIORef M.empty

-- | link and write result to disk (jsexe directory)
link :: GhcjsEnv
     -> JSLinkConfig
     -> StgToJSConfig
     -> Logger
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
link env lc_cfg cfg logger dflags u_env out include pkgs objFiles jsFiles isRootFun extraStaticDeps
  | lcNoJSExecutables lc_cfg = return ()
  | otherwise = do
      LinkResult lo lstats lmetasize _lfrefs llW lla llarch lbase <-
        link' env lc_cfg cfg dflags logger u_env out include pkgs objFiles jsFiles
              isRootFun extraStaticDeps
      let genBase = isJust (lcGenBase lc_cfg)
          jsExt | genBase   = "base.js"
                | otherwise = "js"
      createDirectoryIfMissing False out
      BL.writeFile (out </> "out" <.> jsExt) lo
      unless (lcOnlyOut lc_cfg) $ do
        let frefsFile   = if genBase then "out.base.frefs" else "out.frefs"
            -- FIXME: Jeff (2022,03): GHCJS used Aeson to encode Foreign
            -- references as StaticDeps to a Bytestring and then write these out
            -- to a tmp file for linking. We do not have access to Aeson so
            -- we'll need to find an alternative coding strategy to write these
            -- out. See the commented instance for FromJSON StaticDeps below.
            -- - this line called out to the FromJSon Instance
            -- jsonFrefs  = Aeson.encode lfrefs
            jsonFrefs  = mempty

        BL.writeFile (out </> frefsFile <.> "json") jsonFrefs
        BL.writeFile (out </> frefsFile <.> "js")
                     ("h$checkForeignRefs(" <> jsonFrefs <> ");")
        unless (lcNoStats lc_cfg) $ do
          let statsFile = if genBase then "out.base.stats" else "out.stats"
          writeFile (out </> statsFile) (linkerStats lmetasize lstats)
        unless (lcNoRts lc_cfg) $ do
          withRts <- mapM (tryReadShimFile dflags) llW
          BL.writeFile (out </> "rts.js") (BLC.pack (T.unpack rtsDeclsText)
                                           <> BL.fromChunks withRts
                                           <> BLC.pack (T.unpack $ rtsText cfg))
        lla'    <- mapM (tryReadShimFile dflags) lla
        llarch' <- mapM (readShimsArchive dflags) llarch
        BL.writeFile (out </> "lib" <.> jsExt)
                     (BL.fromChunks $ llarch' ++ lla')
        if genBase
          then generateBase out lbase
          else when (    not (lcOnlyOut lc_cfg)
                      && not (lcNoRts   lc_cfg)
                      && not (usingBase lc_cfg)
                    )
               $ do
                 let top = sTopDir . settings $ dflags
                 _ <- combineFiles lc_cfg top out
                 writeHtml    top out
                 writeRunMain top out
                 writeRunner lc_cfg out
                 writeWebAppManifest top out
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
link' env lc_cfg cfg dflags logger u_env target _include pkgs objFiles jsFiles isRootFun extraStaticDeps
  = do
  -- FIXME: Jeff (2022,04): This function has several helpers that should be
  -- factored out. In its current condition it is hard to read exactly whats
  -- going on and why.
      (objDepsMap, objRequiredUnits) <- loadObjDeps objFiles

      let rootSelector | Just baseMod <- lcGenBase lc_cfg =
                           \(ExportedFun  m _s) -> m == baseMod
                       | otherwise = isRootFun
          roots    = S.fromList . filter rootSelector $ concatMap (M.keys . depsHaskellExported . fst) (M.elems objDepsMap)
          -- FIXME: Jeff (2022,03): Remove head. opt for NonEmptyList. Every
          -- head is a time bomb waiting to explode
          rootMods = map (moduleNameString . moduleName . head) . group . sort . map funModule . S.toList $ roots
          objPkgs  = map moduleUnitId $ nub (M.keys objDepsMap)

      _ <- compilationProgressMsg logger . text $
                case lcGenBase lc_cfg of
                  Just baseMod -> "Linking base bundle " ++ target ++ " (" ++ moduleNameString (moduleName baseMod) ++ ")"
                  _            -> "Linking " ++ target ++ " (" ++ intercalate "," rootMods ++ ")"
      base <- case lcUseBase lc_cfg of
        NoBase        -> return emptyBase
        BaseFile file -> loadBase file
        BaseState b   -> return b
      (rdPkgs, rds) <- rtsDeps u_env dflags pkgs
      -- c   <- newMVar M.empty
      let rtsPkgs     =  map stringToUnitId ["@rts", "@rts_" ++ waysTag (targetWays_ $ dflags)]
          pkgs' :: [UnitId]
          pkgs'       = nub (rtsPkgs ++ rdPkgs ++ reverse objPkgs ++ reverse pkgs)
          pkgs''      = filter (not . isAlreadyLinked base) pkgs'
          ue_state    = ue_units $ u_env
          -- pkgLibPaths = mkPkgLibPaths pkgs'
          -- getPkgLibPaths :: UnitId -> ([FilePath],[String])
          -- getPkgLibPaths k = fromMaybe ([],[]) (lookup k pkgLibPaths)
      (archsDepsMap, archsRequiredUnits) <- loadArchiveDeps env =<< getPackageArchives cfg (map snd $ mkPkgLibPaths ue_state pkgs')
      pkgArchs <- getPackageArchives cfg (map snd $ mkPkgLibPaths ue_state pkgs'')
      (allDeps, code) <-
        collectDeps (objDepsMap `M.union` archsDepsMap)
                    (pkgs' ++ [homeUnitId (ue_unsafeHomeUnit $ u_env)]) -- FIXME: dont use unsafe
                    (baseUnits base)
                    (roots `S.union` rds `S.union` extraStaticDeps)
                    (archsRequiredUnits ++ objRequiredUnits)
      let (outJs, metaSize, compactorState, stats) =
             renderLinker lc_cfg cfg (baseCompactorState base) rds code
          base'  = Base compactorState (nub $ basePkgs base ++ pkgs'')
                         (allDeps `S.union` baseUnits base)
      (alreadyLinkedBefore, alreadyLinkedAfter) <- getShims [] (filter (isAlreadyLinked base) pkgs')
      (shimsBefore, shimsAfter) <- getShims jsFiles pkgs''
      return $ LinkResult outJs stats metaSize
                 (concatMap (\(_,_,_,_,_,r) -> r) code)
                 (filter (`notElem` alreadyLinkedBefore) shimsBefore)
                 (filter (`notElem` alreadyLinkedAfter)  shimsAfter)
                 pkgArchs base'
  where
    isAlreadyLinked :: Base -> UnitId -> Bool
    isAlreadyLinked b uid = uid `elem` basePkgs b

    mkPkgLibPaths :: UnitState -> [UnitId] -> [(UnitId, ([FilePath],[String]))]
    mkPkgLibPaths u_st
      = map (\k -> ( k
                   , (getInstalledPackageLibDirs u_st k
                   , getInstalledPackageHsLibs u_st k)
                   ))

renderLinker :: JSLinkConfig
             -> StgToJSConfig
             -> CompactorState
             -> Set ExportedFun
             -> [(Module, JStat, ShortText, [ClosureInfo], [StaticInfo], [ForeignJSRef])] -- ^ linked code per module
             -> (BL.ByteString, Int64, CompactorState, LinkerStats)
renderLinker settings cfg renamerState rtsDeps code =
  let
      (_renamerState', compacted, meta) = compact settings cfg renamerState (map funSymbol $ S.toList rtsDeps) (map (\(_,s,_,ci,si,_) -> (s,ci,si)) code)
      pe = (<>"\n") . show . pretty
      rendered  = fmap pe compacted
      renderedMeta = pe meta
      renderedExports = concatMap T.unpack . filter (not . T.null) $ map (\(_,_,rs,_,_,_) -> rs) code
      mkStat (m,_,_,_,_,_) b = (m, BL.length . BLC.pack $ b)
  in ( BL.fromStrict $ BC.pack $ mconcat [mconcat rendered, renderedMeta, renderedExports]
     , BL.length $ BL.fromStrict $ BC.pack renderedMeta
     , renamerState
     , M.fromList $ zipWith mkStat code rendered
     )

linkerStats :: Int64         -- ^ code size of packed metadata
            -> LinkerStats   -- ^ code size per module
            -> String
linkerStats meta s =
  intercalate "\n\n" [packageStats, moduleStats, metaStats] <> "\n\n"
  where
    ps = M.fromListWith (+) . map (\(m,s) -> (moduleName m,s)) . M.toList $ s
    pad :: Int -> String -> String
    pad n t = let l = length t
              in  if l < n then t <> replicate (n-l) ' ' else t

    pkgMods :: [[(Module,Int64)]]
    pkgMods = groupBy ((==) `on` fst) (M.toList s)

    showMod :: (Module, Int64) -> String
    showMod (m,s) = pad 40 ("    " <> moduleStableString m <> ":") <> show s

    packageStats :: String
    packageStats = "code size summary per package:\n\n"
                   <> concatMap (\(p,s) -> pad 25 (show p <> ":") <> show s) (M.toList ps)

    moduleStats :: String
    moduleStats = "code size per module:\n\n" <> unlines (map (concatMap showMod) pkgMods)

    metaStats :: String
    metaStats = "packed metadata: " <> show meta

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
             -> FilePath      -- ^ top level dir
             -> FilePath
             -> IO ()
combineFiles cfg top fp = do
  files   <- mapM (B.readFile.(fp</>)) ["rts.js", "lib.js", "out.js"]
  runMain <- if   lcNoHsMain cfg
             then pure mempty
             else B.readFile (top </> "runmain.js")
  writeBinaryFile (fp</>"all.js") (mconcat (files ++ [runMain]))

-- | write the index.html file that loads the program if it does not exit
writeHtml :: FilePath -- ^ top level library directory
          -> FilePath -- ^ output directory
          -> IO ()
writeHtml top out = do
  e <- doesFileExist htmlFile
  unless e $
    B.readFile (top </>"template.html") >>= B.writeFile htmlFile
  where
    htmlFile = out </> "index.html"

-- | write the runmain.js file that will be run with defer so that it runs after
-- index.html is loaded
writeRunMain :: FilePath -- ^ top level library directory
             -> FilePath -- ^ output directory
             -> IO ()
writeRunMain top out = do
  e <- doesFileExist runMainFile
  unless e $
    B.readFile (top </> "runmain.js") >>= B.writeFile runMainFile
  where
    runMainFile = out </> "runmain.js"

-- FIXME: Jeff (2022,03): Use Newtypes instead of Strings for these directories
writeRunner :: JSLinkConfig -- ^ Settings
            -> FilePath     -- ^ Output directory
            -> IO ()
writeRunner _settings out =
  -- FIXME: Jeff (2022,03): why was the buildRunner check removed? If we don't
  -- need to check then does the flag need to exist?
  {-when (lcBuildRunner _settings) $ -} do
  cd    <- getCurrentDirectory
  let runner  = cd </> addExeExtension (dropExtension out)
      srcFile = out </> "all" <.> "js"
  -- nodeSettings <- readNodeSettings dflags
      nodePgm :: B.ByteString
      nodePgm = "node" -- XXX we don't read nodeSettings.json anymore, we should somehow know how to find node?

  ---------------------------------------------
  -- FIXME: Jeff (2022,03): Add support for windows. Detect it and act on it here:
  -- if Platform.isWindows
  -- then do
  --   copyFile (topDir dflags </> "bin" </> "wrapper" <.> "exe")
  --            runner
  --   writeFile (runner <.> "options") $ unlines
  --               [ T.pack nodePgm -- T.pack (nodeProgram nodeSettings)
  --               , T.pack ("{{EXEPATH}}" </> out </> "all" <.> "js")
  --               ]
  -- else do
  ---------------------------------------------
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

rtsExterns :: ShortText
rtsExterns =
  "// GHCJS RTS externs for closure compiler ADVANCED_OPTIMIZATIONS\n\n" <>
  mconcat (map (\x -> "/** @type {*} */\nObject.d" <> T.pack (show x) <> ";\n")
               [(7::Int)..16384])

writeExterns :: FilePath -> IO ()
writeExterns out = writeFile (out </> "all.js.externs")
  $ T.unpack rtsExterns -- FIXME: Jeff (2022,03): Why write rtsExterns as
                        -- ShortText just to unpack?

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
                     lun = fromMaybe (pprPanic "exported function not found: " $ pprModule key)
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

-- FIXME: Jeff: (2022,03): if the order of the [UnitId] list matters after
-- ghc-prim then we should be using an Ordered Set or something
-- similar since the implementation of this function uses a lot of
-- expensive operations on this list and a lot of
-- serialization/deserialization
-- FIXME: Jeff (2022,03): Should [UnitId] be [Module]?
-- | collect dependencies for a set of roots
collectDeps :: Map Module (Deps, DepsLocation) -- ^ Dependency map
            -> [UnitId]                        -- ^ packages, code linked in this order
            -> Set LinkableUnit                -- ^ do not include these
            -> Set ExportedFun                 -- ^ roots
            -> [LinkableUnit]                  -- ^ more roots
            -> IO ( Set LinkableUnit
                  , [(Module, JStat, ShortText, [ClosureInfo], [StaticInfo], [ForeignJSRef])]
                  )
collectDeps mod_deps packages base roots units = do
  allDeps <- getDeps (fmap fst mod_deps) base roots units
  -- read ghc-prim first, since we depend on that for static initialization
  let packages' = uncurry (++) $ partition (== primUnitId) (nub packages)

      units_by_module :: Map Module IntSet
      units_by_module = M.fromListWith IS.union $
                      map (\(m,n) -> (m, IS.singleton n)) (S.toList allDeps)

      mod_deps_bypkg :: Map UnitId [(Deps, DepsLocation)]
      mod_deps_bypkg = M.mapKeys moduleUnitId
                       $ M.fromListWith (++)
                        (map (\(m,v) -> (m,[v])) (M.toList mod_deps))

  ar_state <- emptyArchiveState
  code <- fmap (catMaybes . concat) . forM packages' $ \pkg ->
    mapM (uncurry $ extractDeps ar_state units_by_module)
         (fromMaybe [] $ M.lookup pkg mod_deps_bypkg)
  return (allDeps, code)

extractDeps :: ArchiveState
            -> Map Module IntSet
            -> Deps
            -> DepsLocation
            -> IO (Maybe (Module, JStat, ShortText, [ClosureInfo], [StaticInfo], [ForeignJSRef]))
extractDeps ar_state units deps loc =
  case M.lookup mod units of
    Nothing       -> return Nothing
    Just modUnits -> do
      let selector n _  = n `IS.member` modUnits || isGlobalUnit n
      x <- case loc of
        ObjectFile o  -> collectCode =<< readObjectFileKeys selector o
        ArchiveFile a -> collectCode
                        . readObjectKeys (a ++ ':':moduleNameString (moduleName mod)) selector
                        =<< readArObject ar_state mod a
                            --  error ("Ar.readObject: " ++ a ++ ':' : T.unpack mod))
                            -- Ar.readObject (mkModuleName $ T.unpack mod) a)
        InMemory n b  -> collectCode $ readObjectKeys n selector b
      -- evaluate (rnf x) -- See FIXME Re: NFData instance on Safety and
                          -- ForeignJSRefs below
      return x
  where
    mod           = depsModule deps
    -- FIXME: Jeff (2022,03): remove this hacky reimplementation of unlines
    newline       = T.pack "\n"
    unlines'      = intersperse newline . map oiRaw
    collectCode l = let x = ( mod
                            , mconcat (map oiStat l)
                            , mconcat (unlines' l)
                            , concatMap oiClInfo l
                            , concatMap oiStatic l
                            , concatMap oiFImports l)
                -- FIXME: (2022,04): this evaluate and rnf require an NFData
                -- instance on ForeignJSRef which in turn requries a NFData
                -- instance on Safety. Does this even make sense? We'll skip
                -- this for now.

                --  in evaluate (rnf x) >> return (Just x)

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
  pure $ maybe (error $ "could not find object for module "
                ++ moduleNameString (moduleName mod)
                ++ " in "
                ++ ar_file)
                (BL.fromStrict . Ar.filedata) (find matchTag entries)
  -- mapM_ (\e -> putStrLn ("found file: " ++ Ar.filename e)) entries

{- | Static dependencies are symbols that need to be linked regardless
     of whether the linked program refers to them. For example
     dependencies that the RTS uses or symbols that the user program
     refers to directly
 -}
newtype StaticDeps =
  StaticDeps { unStaticDeps :: [(ShortText, ShortText)] -- module/symbol
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
-- instance FromJSON StaticDeps where
--   parseJSON (Object v) = StaticDeps . concat <$> mapM (uncurry parseMod) (HM.toList v)
--     where
--       parseMod p (Object v) = concat <$> mapM (uncurry (parseSymb p)) (HM.toList v)
--       parseMod _ _          = mempty
--       parseSymb p m (String s) = pure [(p,m,s)]
--       parseSymb p m (Array v)  = mapM (parseSingleSymb p m) (V.toList v)
--       parseSymb _ _ _          = mempty
--       parseSingleSymb p m (String s) = pure (p,m,s)
--       parseSingleSymb _ _ _          = mempty
--   parseJSON _          = mempty

-- | dependencies for the RTS, these need to be always linked
rtsDeps :: UnitEnv -> DynFlags -> [UnitId] -> IO ([UnitId], Set ExportedFun)
rtsDeps u_env dflags pkgs = readSystemDeps u_env dflags pkgs "rtsdeps.yaml"

-- | dependencies for the Template Haskell, these need to be linked when running
--   Template Haskell (in addition to the RTS deps)
thDeps :: UnitEnv -> DynFlags -> [UnitId] -> IO ([UnitId], Set ExportedFun)
thDeps u_env dflags pkgs = readSystemDeps u_env dflags pkgs "thdeps.yaml"

-- FIXME: Jeff (2022,03): fill in the ?
-- | A helper function to read system dependencies that are hardcoded via a file
-- path.
readSystemDeps :: UnitEnv     -- ^ The unit envrionment
               -> DynFlags
               -> [UnitId]    -- ^ Packages to ??
               -> FilePath    -- ^ File to read
               -> IO ([UnitId], Set ExportedFun)
readSystemDeps u_env dflags pkgs file = do
  (deps_pkgs, deps_funs) <- readSystemDeps' u_env dflags file
  pure ( filter (`S.member` linked_pkgs) deps_pkgs
       , S.filter (\fun ->
                     moduleUnitId (funModule fun) `S.member` linked_pkgs) deps_funs
       )

  where
    -- FIXME: Jeff (2022,03): Each time we _do not_ use a list like a stack we
    -- gain evidence that we should be using a different data structure. @pkgs@
    -- is the list in question here
    linked_pkgs     = S.fromList pkgs


readSystemDeps' :: UnitEnv
               -> DynFlags
               -> FilePath
               -> IO ([UnitId], Set ExportedFun)
readSystemDeps' u_env dflags file
  -- hardcode contents to get rid of yaml dep
  -- XXX move runTHServer to some suitable wired-in package
  -- FIXME: Jeff (2022,03): Use types not string matches, These should be
  -- wired-in just like in GHC and thus we should make them top level
  -- definitions
  | file == "thdeps.yaml" = pure ( [stringToUnitId "base"]
                                 , S.fromList $ d "base" "GHCJS.Prim.TH.Eval" ["runTHServer"])
  | file == "rtsdeps.yaml" = pure ( [stringToUnitId "base"
                                    , stringToUnitId "ghc-prim"
                                    , stringToUnitId "integer-wired-in"
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
  | otherwise = pure (mempty, mempty)
  where

    d :: String -> String -> [String] -> [ExportedFun]
    d pkg mod symbols = map (let pkg_module = mkJsModule pkg mod
                              in ExportedFun pkg_module
                                 . mkHaskellSym pkg_module (T.pack mod)
                                 . T.pack)
                        symbols
    zenc  = T.pack . zEncodeString . T.unpack

    mkHaskellSym :: Module -> ShortText -> ShortText -> ShortText
    mkHaskellSym mod m s = "h$" <> zenc (T.pack (encodeModule u_env dflags mod)
                                       <> ":"
                                       <> m
                                       <> "."
                                       <> s)
    mkJsModule :: String -> String -> GenModule Unit
    mkJsModule pkg mod = mkModule (RealUnit (Definite (stringToUnitId pkg))) (mkModuleName mod)

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

readSystemWiredIn :: HscEnv -> IO [(ShortText, UnitId)]
readSystemWiredIn _ = pure [] -- XXX
{-
readSystemWiredIn dflags = do
  b <- B.readFile filename
  case Yaml.decodeEither b of
     Left _err -> error $ "could not read wired-in package keys from " ++ filename
     Right m  -> return . M.toList
                        . M.union ghcWiredIn -- GHC wired-in package keys override those in the file
                        . fmap stringToUnitId $ m
  where
    filename = getLibDir dflags </> "wiredinkeys" <.> "yaml"
    ghcWiredIn :: Map Text UnitId
    ghcWiredIn = M.fromList $ map (\k -> (T.pack (installedUnitIdString k), k))
                                  (map toUnitId wiredInUnitIds)
                                  -}
{- | read a static dependencies specification and give the roots

     if dependencies come from a versioned (non-hardwired) package
     that is linked multiple times, then the returned dependencies
     will all come from the same version, but it's undefined which one.
 -}

type SDep = (ShortText, ShortText) -- ^ module/symbol

staticDeps :: UnitEnv
           -> DynFlags
           -> [(ShortText, Module)]   -- ^ wired-in package names / keys
           -> StaticDeps              -- ^ deps from yaml file
           -> (StaticDeps, Set UnitId, Set ExportedFun)
                                      -- ^ the StaticDeps contains the symbols
                                      --   for which no package could be found
staticDeps u_env dflags wiredin sdeps = mkDeps sdeps
  where
    zenc  = T.pack . zEncodeString . T.unpack
    u_st  = ue_units u_env
    mkDeps (StaticDeps ds) =
      -- FIXME: Jeff (2022,03): this foldl' will leak memory due to the tuple
      -- and in the list in the fst position because the list is neither spine
      -- nor value strict. So the WHNF computed by foldl' will by a 3-tuple with
      -- 3 thunks and the WHNF for the list will be a cons cell
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
                                     ++ T.unpack mod_name
                                     ++ " receieved " ++ moduleNameString (moduleName mod)
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
                                       $ mkSymb mod mod_name s) resolved
                           )
    -- confusingly with the new ghc api we now use Module where we formerly had
    -- Package, so this becomes Module -> Module -> Symbol where the first
    -- Module is GHC's module type and the second is the SDep Moudle read as a
    -- ShortText
    -- FIXME: Jeff (2022,03): should mkSymb be in the UnitUtils?
    mkSymb :: Module -> ShortText -> ShortText -> ShortText
    mkSymb p m s  =
      "h$" <> zenc (T.pack (encodeModule u_env dflags p) <> ":" <> m <> "." <> s)

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
    pure (mapMaybe (readEntry file) entries)
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
readDepsFile' (ObjLoaded name bs) = pure . (,InMemory name bs) $
                                    readDeps name bs
readDepsFile' (ObjFile file)      =
  (,ObjectFile file) <$> readDepsFile file

generateBase :: FilePath -> Base -> IO ()
generateBase outDir b =
  BL.writeFile (outDir </> "out.base.symbs") (renderBase b)

