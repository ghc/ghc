{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Control.Monad.State
import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.FilePath

import BuildInfo
import FilenameDescr
import Change
import Utils
import Tar

-- TODO:
-- * Check installed trees too
-- * Check hashbangs

sizeChangeThresholds :: [(Integer,  -- Theshold only applies if one of
                                    -- the files is at least this big
                          Integer)] -- Size changed if the larger file's
                                    -- size is at least this %age of the
                                    -- smaller file's size
sizeChangeThresholds = [(     1000, 150),
                        (50 * 1000, 110)]

main :: IO ()
main = do args <- getArgs
          (ignoreSizeChanges, p1, p2) <-
              case args of
              [p1, p2]                          -> return (False, p1, p2)
              ["--ignore-size-changes", p1, p2] -> return (True,  p1, p2)
              _ -> die ["Bad args. Need 2 filepaths."]
          doFileOrDirectory ignoreSizeChanges p1 p2

doFileOrDirectory :: Bool -> FilePath -> FilePath -> IO ()
doFileOrDirectory ignoreSizeChanges p1 p2
 = do b <- doesDirectoryExist p1
      let doit = if b then doDirectory else doFile
      doit ignoreSizeChanges p1 p2

doDirectory :: Bool -> FilePath -> FilePath -> IO ()
doDirectory ignoreSizeChanges p1 p2
 = do fs1 <- getDirectoryContents p1
      fs2 <- getDirectoryContents p2
      let isVersionChar c = isDigit c || c == '.'
          mkFileInfo "." = return []
          mkFileInfo ".." = return []
          mkFileInfo fp@('g':'h':'c':'-':x:xs)
           | isDigit x = return [(("ghc-", "VERSION", dropWhile isVersionChar xs), fp)]
           | otherwise = die ["No version number in " ++ show fp]
          mkFileInfo fp = die ["Unrecognised filename " ++ show fp]
      fss1' <- mapM mkFileInfo fs1
      fss2' <- mapM mkFileInfo fs2
      let fs1' = sort $ concat fss1'
          fs2' = sort $ concat fss2'

          putBreak = putStrLn "=========="
          extraFile d fp = do putBreak
                              putStrLn ("Extra file in " ++ show d
                                     ++ ": " ++ show fp)
          doFiles [] [] = return ()
          doFiles ((_, fp) : xs) [] = do extraFile p1 fp
                                         doFiles xs []
          doFiles [] ((_, fp) : ys) = do extraFile p2 fp
                                         doFiles [] ys
          doFiles xs@((fpc1, fp1) : xs') ys@((fpc2, fp2) : ys')
              = do case fpc1 `compare` fpc2 of
                       EQ ->
                           do putBreak
                              putStrLn $ unwords ["Doing", show fp1, show fp2]
                              doFile ignoreSizeChanges (p1 </> fp1)
                                                       (p2 </> fp2)
                              doFiles xs' ys'
                       LT -> do extraFile p1 fp1
                                doFiles xs' ys
                       GT -> do extraFile p2 fp2
                                doFiles xs ys'
      doFiles fs1' fs2'

doFile :: Bool -> FilePath -> FilePath -> IO ()
doFile ignoreSizeChanges bd1 bd2
 = do tls1 <- readTarLines bd1
      tls2 <- readTarLines bd2
      let mWays1 = findWays tls1
          mWays2 = findWays tls2
      wayDifferences <- case (mWays1, mWays2) of
                        (Nothing, Nothing) ->
                            return []
                        (Just ways1, Just ways2) ->
                            return $ diffWays ways1 ways2
                        _ ->
                            die ["One input has ways, but the other doesn't"]
      (content1, tvm1) <- dieOnErrors $ mkContents mWays1 tls1
      (content2, tvm2) <- dieOnErrors $ mkContents mWays2 tls2
      let sortedContent1 = sortByFst content1
          sortedContent2 = sortByFst content2
          (nubProbs1, nubbedContent1) = nubContents sortedContent1
          (nubProbs2, nubbedContent2) = nubContents sortedContent2
          differences = compareContent mWays1 nubbedContent1
                                       mWays2 nubbedContent2
          allProbs = map First nubProbs1 ++ map Second nubProbs2
                  ++ diffThingVersionMap tvm1 tvm2
                  ++ wayDifferences
                  ++ differences
          wantedProbs = if ignoreSizeChanges
                        then filter (not . isSizeChange) allProbs
                        else allProbs
      mapM_ (putStrLn . pprFileChange) wantedProbs

-- *nix bindists have ways.
-- Windows "bindists", install trees, and testsuites don't.
findWays :: [TarLine] -> Maybe Ways
findWays tls = msum $ map f tls
    where f tl = case re regex (tlFileName tl) of
                 Just [dashedWays] -> Just (unSepList '-' dashedWays)
                 _                 -> Nothing
          regex = "/libraries/base/dist-install/build/\\.depend-(.*)\\.haskell"

diffWays :: Ways -> Ways -> [FileChange]
diffWays ws1 ws2 = f (sort ws1) (sort ws2)
    where f [] [] = []
          f xs [] = map (First  . ExtraWay) xs
          f [] ys = map (Second . ExtraWay) ys
          f xs@(x : xs') ys@(y : ys')
              = case x `compare` y of
                LT -> First  (ExtraWay x) : f xs' ys
                GT -> Second (ExtraWay y) : f xs  ys'
                EQ ->                       f xs' ys'

diffThingVersionMap :: ThingVersionMap -> ThingVersionMap -> [FileChange]
diffThingVersionMap tvm1 tvm2 = f (sortByFst tvm1) (sortByFst tvm2)
    where f [] [] = []
          f xs [] = map (First  . ExtraThing . fst) xs
          f [] ys = map (Second . ExtraThing . fst) ys
          f xs@((xt, xv) : xs') ys@((yt, yv) : ys')
              = case xt `compare` yt of
                LT -> First  (ExtraThing xt) : f xs' ys
                GT -> Second (ExtraThing yt) : f xs  ys'
                EQ -> let this = if xv == yv
                                 then []
                                 else [Change (ThingVersionChanged xt xv yv)]
                      in this ++ f xs' ys'

mkContents :: Maybe Ways -> [TarLine]
           -> Either Errors ([(FilenameDescr, TarLine)], ThingVersionMap)
mkContents mWays tls
    = case runStateT (mapM f tls) (emptyBuildInfo mWays) of
      Nothing -> Left ["Can't happen: mkContents: Nothing"]
      Just (xs, finalBuildInfo) ->
          case concat $ map (checkContent finalBuildInfo) xs of
          []   -> Right (xs, biThingVersionMap finalBuildInfo)
          errs -> Left errs
    where f tl = do fnd <- mkFilePathDescr (tlFileName tl)
                    return (fnd, tl)

nubContents :: [(FilenameDescr, TarLine)]
            -> ([Change], [(FilenameDescr, TarLine)])
nubContents [] = ([], [])
nubContents [x] = ([], [x])
nubContents (x1@(fd1, tl1) : xs@((fd2, _) : _))
 | fd1 == fd2 = (DuplicateFile (tlFileName tl1) : ps, xs')
 | otherwise  = (ps, x1 : xs')
    where (ps, xs') = nubContents xs

mkFilePathDescr :: FilePath -> BIMonad FilenameDescr
mkFilePathDescr fp
 | Just [ghcVersion, _, middle, filename]
     <- re ("^ghc-" ++ versionRE ++ "(/.*)?/([^/]*)$") fp
    = do haveThingVersion "ghc" ghcVersion
         middle' <- mkMiddleDescr middle
         filename' <- mkFileNameDescr filename
         let fd = FP "ghc-" : VersionOf "ghc" : middle' ++ FP "/" : filename'
         return $ normaliseDescr fd
 | otherwise = return [FP fp]

mkMiddleDescr :: FilePath -> BIMonad FilenameDescr
mkMiddleDescr middle
 -- haddock docs in a Windows installed tree
 | Just [thing, thingVersion, _, src]
       <- re ("^/doc/html/libraries/([^/]*)-" ++ versionRE ++ "(/src)?$")
             middle
    = do haveThingVersion thing thingVersion
         return [FP "/doc/html/libraries/",
                 FP thing, FP "-", VersionOf thing, FP src]
      `mplus` unchanged
 -- libraries in a Windows installed tree
 | Just [thing, thingVersion, _, rest]
       <- re ("^/lib/([^/]*)-" ++ versionRE ++ "(/.*)?$")
             middle
    = do haveThingVersion thing thingVersion
         return [FP "/lib/", FP thing, FP "-", VersionOf thing, FP rest]
      `mplus` unchanged
 -- Windows in-tree gcc
 | Just [prefix, _, _, gccVersion, _, rest]
       <- re ("^(/mingw/(lib(exec)?/gcc/mingw32/|share/gcc-))" ++ versionRE ++ "(/.*)?$")
             middle
    = do haveThingVersion "gcc" gccVersion
         return [FP prefix, VersionOf "gcc", FP rest]
      `mplus` unchanged
 | otherwise = unchanged
    where unchanged = return [FP middle]

mkFileNameDescr :: FilePath -> BIMonad FilenameDescr
mkFileNameDescr filename
 | Just [prog, ghcVersion, _, exe]
       <- re ("^(ghc|ghci|ghcii|haddock)-" ++ versionRE ++ "(\\.exe|\\.sh|)$")
             filename
    = do haveThingVersion "ghc" ghcVersion
         return [FP prog, FP "-", VersionOf "ghc", FP exe]
      `mplus` unchanged
 | Just [thing, thingVersion, _, ghcVersion, _, soDll]
       <- re ("^libHS(.*)-" ++ versionRE ++ "-ghc" ++ versionRE ++ "\\.(so|dll|dylib)$")
             filename
    = do haveThingVersion "ghc" ghcVersion
         haveThingVersion thing thingVersion
         return [FP "libHS", FP thing, FP "-", VersionOf thing,
                 FP "-ghc", VersionOf "ghc", FP ".", FP soDll]
      `mplus` unchanged
 | Just [way, thingVersion, _, soDll]
       <- re ("^libHSrts(_.*)?-ghc" ++ versionRE ++ "\\.(so|dll|dylib)$")
             filename
    = do haveThingVersion "ghc" thingVersion
         return [FP "libHSrts", FP way, FP "-ghc", VersionOf "ghc",
                 FP ".", FP soDll]
      `mplus` unchanged
 | Just [thingVersion, _, soDll]
       <- re ("^libHSffi-ghc" ++ versionRE ++ "\\.(so|dll|dylib)$")
             filename
    = do haveThingVersion "ghc" thingVersion
         return [FP "libHSffi-ghc", VersionOf "ghc", FP ".", FP soDll]
      `mplus` unchanged
 | Just [thing, thingVersion, _, way]
       <- re ("^libHS(.*)-" ++ versionRE ++ "(_.*)?\\.a$")
             filename
    = do haveThingVersion thing thingVersion
         return [FP "libHS", FP thing, FP "-", VersionOf thing,
                 FP way, FP ".a"]
      `mplus` unchanged
 | Just [thing, thingVersion, _]
       <- re ("^HS(.*)-" ++ versionRE ++ "\\.o$")
             filename
    = do haveThingVersion thing thingVersion
         return [FP "HS", FP thing, FP "-", VersionOf thing, FP ".o"]
      `mplus` unchanged
 | Just [thing, thingVersion, _, thingHash]
       <- re ("^(.*)-" ++ versionRE ++ "-([0-9a-f]{32})\\.conf$")
             filename
    = do haveThingVersion thing thingVersion
         haveThingHash    thing thingHash
         return [FP thing, FP "-", VersionOf thing, FP "-", HashOf thing,
                 FP ".conf"]
      `mplus` unchanged
 | Just [thingVersion, _]
       <- re ("^mingw32-gcc-" ++ versionRE ++ "\\.exe$")
             filename
    = do haveThingVersion "gcc" thingVersion
         return [FP "mingw32-gcc-", VersionOf "gcc", FP ".exe"]
      `mplus` unchanged
 | Just [dashedWays, depType]
       <- re "^\\.depend-(.*)\\.(haskell|c_asm)"
             filename
    = do mWays <- getMaybeWays
         if Just (unSepList '-' dashedWays) == mWays
             then return [FP ".depend-", Ways, FP ".", FP depType]
             else unchanged
 | otherwise = unchanged
    where unchanged = return [FP filename]

compareContent :: Maybe Ways -> [(FilenameDescr, TarLine)]
               -> Maybe Ways -> [(FilenameDescr, TarLine)]
               -> [FileChange]
compareContent mWays1 xs1all mWays2 xs2all
 = f xs1all xs2all
    where f [] [] = []
          f xs [] = concatMap (mkExtraFile mWays1 mWays2 First  . tlFileName . snd) xs
          f [] ys = concatMap (mkExtraFile mWays2 mWays1 Second . tlFileName . snd) ys
          f xs1@((fd1, tl1) : xs1') xs2@((fd2, tl2) : xs2')
           = case fd1 `compare` fd2 of
             EQ -> map Change (compareTarLine tl1 tl2)
                ++ f xs1' xs2'
             LT -> mkExtraFile mWays1 mWays2 First  (tlFileName tl1)
                ++ f xs1' xs2
             GT -> mkExtraFile mWays2 mWays1 Second (tlFileName tl2)
                ++ f xs1 xs2'
          mkExtraFile mWaysMe mWaysThem mkFileChange filename
              = case (findFileWay filename, mWaysMe, mWaysThem) of
                (Just way, Just waysMe, Just waysThem)
                 | (way `elem` waysMe) && not (way `elem` waysThem) -> []
                _                  -> [mkFileChange (ExtraFile filename)]

findFileWay :: FilePath -> Maybe String
findFileWay fp
 | Just [way] <- re "\\.([a-z_]+)_hi$" fp
    = Just way
 | Just [_, _, way] <- re ("libHS.*-" ++ versionRE ++ "_([a-z_]+).a$") fp
    = Just way
 | otherwise = Nothing

compareTarLine :: TarLine -> TarLine -> [Change]
compareTarLine tl1 tl2
    = [ PermissionsChanged fn1 fn2 perms1 perms2 | perms1 /= perms2 ]
   ++ [ FileSizeChanged    fn1 fn2 size1  size2  | sizeChanged ]
    where fn1 = tlFileName tl1
          fn2 = tlFileName tl2
          perms1 = tlPermissions tl1
          perms2 = tlPermissions tl2
          size1 = tlSize tl1
          size2 = tlSize tl2
          sizeMin = size1 `min` size2
          sizeMax = size1 `max` size2
          sizeChanged = any sizeChangeThresholdReached sizeChangeThresholds
          sizeChangeThresholdReached (reqSize, percentage)
              = (sizeMax >= reqSize)
             && (((100 * sizeMax) `div` sizeMin) >= percentage)

versionRE :: String
versionRE = "([0-9]+(\\.[0-9]+)*)"

