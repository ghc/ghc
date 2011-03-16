{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Control.Monad.State
import Data.Function
import Data.List
import System.Environment

import BuildInfo
import FilenameDescr
import Problem
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
              [bd1, bd2] -> doit bd1 bd2
              _ -> die ["Bad args. Need 2 bindists."]

doit :: FilePath -> FilePath -> IO ()
doit bd1 bd2
 = do tls1 <- readTarLines bd1
      tls2 <- readTarLines bd2
      ways1 <- dieOnErrors $ findWays tls1
      ways2 <- dieOnErrors $ findWays tls2
      content1 <- dieOnErrors $ mkContents ways1 tls1
      content2 <- dieOnErrors $ mkContents ways2 tls2
      let mySort = sortBy (compare `on` fst)
          sortedContent1 = mySort content1
          sortedContent2 = mySort content2
          (nubProbs1, nubbedContent1) = nubContents sortedContent1
          (nubProbs2, nubbedContent2) = nubContents sortedContent2
          differences = compareContent ways1 nubbedContent1
                                       ways2 nubbedContent2
          allProbs = map First nubProbs1 ++ map Second nubProbs2
                  ++ diffWays ways1 ways2
                  ++ differences
      mapM_ (putStrLn . pprFileProblem) allProbs

findWays :: [TarLine] -> Either Errors Ways
findWays = foldr f (Left ["Couldn't find ways"])
    where f tl res = case re regex (tlFileName tl) of
                     Just [dashedWays] ->
                         Right (unSepList '-' dashedWays)
                     _ ->
                         res
          regex = "/libraries/base/dist-install/build/\\.depend-(.*)\\.haskell"

diffWays :: Ways -> Ways -> [FileProblem]
diffWays ws1 ws2 = f (sort ws1) (sort ws2)
    where f [] [] = []
          f xs [] = map (First . ExtraWay) xs
          f [] ys = map (First . ExtraWay) ys
          f xs@(x : xs') ys@(y : ys')
              = case x `compare` y of
                LT -> First  (ExtraWay x) : f xs' ys
                GT -> Second (ExtraWay y) : f xs  ys'
                EQ ->                       f xs' ys'

mkContents :: Ways -> [TarLine] -> Either Errors [(FilenameDescr, TarLine)]
mkContents ways tls
    = case runState (mapM f tls) initialBuildInfo of
      (xs, finalBuildInfo) ->
          case concat $ map (checkContent finalBuildInfo) xs of
          []   -> Right xs
          errs -> Left errs
    where f tl = do fnd <- mkFilePathDescr (tlFileName tl)
                    return (fnd, tl)
          initialBuildInfo = BuildInfo {
                                 biThingVersionMap = [],
                                 biWays = ways
                             }

nubContents :: [(FilenameDescr, TarLine)]
            -> ([Problem], [(FilenameDescr, TarLine)])
nubContents [] = ([], [])
nubContents [x] = ([], [x])
nubContents (x1@(fd1, tl1) : xs@((fd2, _) : _))
 | fd1 == fd2 = (DuplicateFile (tlFileName tl1) : ps, xs')
 | otherwise  = (ps, x1 : xs')
    where (ps, xs') = nubContents xs

mkFilePathDescr :: FilePath -> State BuildInfo FilenameDescr
mkFilePathDescr fp
 | Just [ghcVersion, _, middle, filename]
     <- re ("^ghc-" ++ versionRE ++ "(/.*)?/([^/]*)$") fp
    = do ghcVersionDescr <- do mapping <- getThingVersionMap
                               case addThingVersion mapping "ghc" ghcVersion of
                                   Just mapping' ->
                                       do putThingVersionMap mapping'
                                          return (VersionOf "ghc")
                                   Nothing ->
                                       return (FP ghcVersion)
         filename' <- mkFileNameDescr filename
         let fd = FP "ghc-" : ghcVersionDescr : FP middle : FP "/" : filename'
         return $ normalise fd
 | otherwise = return [FP fp]

mkFileNameDescr :: FilePath -> State BuildInfo FilenameDescr
mkFileNameDescr filename
 | Just [thing, thingVersion, _, ghcVersion, _]
       <- re ("^libHS(.*)-" ++ versionRE ++ "-ghc" ++ versionRE ++ "\\.so$")
             filename
    = do mapping <- getThingVersionMap
         case addThingVersion mapping "ghc" ghcVersion of
             Just m ->
                 case addThingVersion m thing thingVersion of
                 Just m' ->
                     do putThingVersionMap m'
                        return [FP "libHS", FP thing, FP "-", VersionOf thing,
                                FP "-ghc", VersionOf "ghc", FP ".so"]
                 _ -> unchanged
             _ -> unchanged
 | Just [way, thingVersion, _]
       <- re ("^libHSrts(_.*)?-ghc" ++ versionRE ++ "\\.so$")
             filename
    = do mapping <- getThingVersionMap
         case addThingVersion mapping "ghc" thingVersion of
             Just mapping' ->
                 do putThingVersionMap mapping'
                    return [FP "libHSrts", FP way, FP "-ghc", VersionOf "ghc",
                            FP ".so"]
             _ -> unchanged
 | Just [thing, thingVersion, _, way]
       <- re ("^libHS(.*)-" ++ versionRE ++ "(_.*)?\\.a$")
             filename
    = do mapping <- getThingVersionMap
         case addThingVersion mapping thing thingVersion of
             Just mapping' ->
                 do putThingVersionMap mapping'
                    return [FP "libHS", FP thing, FP "-", VersionOf thing,
                            FP way, FP ".a"]
             _ -> unchanged
 | Just [thing, thingVersion, _]
       <- re ("^HS(.*)-" ++ versionRE ++ "\\.o$")
             filename
    = do mapping <- getThingVersionMap
         case addThingVersion mapping thing thingVersion of
             Just mapping' ->
                 do putThingVersionMap mapping'
                    return [FP "HS", FP thing, FP "-", VersionOf thing,
                            FP ".o"]
             _ -> unchanged
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
               -> [FileProblem]
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
    where mkExtraFile ways mkFileProblem filename
              = case findFileWay filename of
                Just way
                 | way `elem` ways -> []
                _                  -> [mkFileProblem (ExtraFile filename)]

findFileWay :: FilePath -> Maybe String
findFileWay fp
 | Just [way] <- re "\\.([a-z_]+)_hi$" fp
    = Just way
 | otherwise = Nothing

compareTarLine :: TarLine -> TarLine -> [Problem]
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

