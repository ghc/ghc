{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Control.Monad.State
import Data.List
import System.Environment

import BuildInfo
import FilenameDescr
import Change
import Utils
import Tar

-- TODO:
-- * Check installed trees too
-- * Check hashbangs

-- Only size changes > sizeAbs are considered an issue
sizeAbs :: Integer
sizeAbs = 1000

-- Only a size change of sizePercentage% or more is considered an issue
sizePercentage :: Integer
sizePercentage = 150

main :: IO ()
main = do args <- getArgs
          case args of
              [bd1, bd2]                          -> doit False bd1 bd2
              ["--ignore-size-changes", bd1, bd2] -> doit True  bd1 bd2
              _ -> die ["Bad args. Need 2 bindists."]

doit :: Bool -> FilePath -> FilePath -> IO ()
doit ignoreSizeChanges bd1 bd2
 = do let windows = any ("mingw" `isPrefixOf`) (tails bd1)
      tls1 <- readTarLines bd1
      tls2 <- readTarLines bd2
      -- If it looks like we have a Windows "bindist" then just
      -- set ways to [] for now.
      ways1 <- if windows then return []
                          else dieOnErrors $ findWays tls1
      ways2 <- if windows then return []
                          else dieOnErrors $ findWays tls2
      (content1, tvm1) <- dieOnErrors $ mkContents ways1 tls1
      (content2, tvm2) <- dieOnErrors $ mkContents ways2 tls2
      let sortedContent1 = sortByFst content1
          sortedContent2 = sortByFst content2
          (nubProbs1, nubbedContent1) = nubContents sortedContent1
          (nubProbs2, nubbedContent2) = nubContents sortedContent2
          differences = compareContent ways1 nubbedContent1
                                       ways2 nubbedContent2
          allProbs = map First nubProbs1 ++ map Second nubProbs2
                  ++ diffThingVersionMap tvm1 tvm2
                  ++ diffWays ways1 ways2
                  ++ differences
          wantedProbs = if ignoreSizeChanges
                        then filter (not . isSizeChange) allProbs
                        else allProbs
      mapM_ (putStrLn . pprFileChange) wantedProbs

findWays :: [TarLine] -> Either Errors Ways
findWays = foldr f (Left ["Couldn't find ways"])
    where f tl res = case re regex (tlFileName tl) of
                     Just [dashedWays] ->
                         Right (unSepList '-' dashedWays)
                     _ ->
                         res
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

mkContents :: Ways -> [TarLine]
           -> Either Errors ([(FilenameDescr, TarLine)], ThingVersionMap)
mkContents ways tls
    = case runStateT (mapM f tls) (emptyBuildInfo ways) of
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
         return $ normalise fd
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
    = do ways <- getWays
         if unSepList '-' dashedWays == ways
             then return [FP ".depend-", Ways, FP ".", FP depType]
             else unchanged
 | otherwise = unchanged
    where unchanged = return [FP filename]

compareContent :: Ways -> [(FilenameDescr, TarLine)]
               -> Ways -> [(FilenameDescr, TarLine)]
               -> [FileChange]
compareContent _ [] _ [] = []
compareContent _ xs _ [] = map (First  . ExtraFile . tlFileName . snd) xs
compareContent _ [] _ ys = map (Second . ExtraFile . tlFileName . snd) ys
compareContent ways1 xs1 ways2 xs2
    = case (xs1, xs2) of
      ([], []) -> []
      (xs, []) -> concatMap (mkExtraFile ways1 First  . tlFileName . snd) xs
      ([], ys) -> concatMap (mkExtraFile ways2 Second . tlFileName . snd) ys
      ((fd1, tl1) : xs1', (fd2, tl2) : xs2') ->
          case fd1 `compare` fd2 of
          EQ -> map Change (compareTarLine tl1 tl2)
             ++ compareContent ways1 xs1' ways2 xs2'
          LT -> mkExtraFile ways1 First  (tlFileName tl1)
             ++ compareContent ways1 xs1' ways2 xs2
          GT -> mkExtraFile ways2 Second (tlFileName tl2)
             ++ compareContent ways1 xs1 ways2 xs2'
    where mkExtraFile ways mkFileChange filename
              = case findFileWay filename of
                Just way
                 | way `elem` ways -> []
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
          sizeChanged = abs (size1 - size2) > sizeAbs
                     && (((100 * size1) `div` size2) > sizePercentage ||
                         ((100 * size2) `div` size1) > sizePercentage)

versionRE :: String
versionRE = "([0-9]+(\\.[0-9]+)*)"

