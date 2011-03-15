{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Control.Monad.State
import Data.Function
import Data.List
import System.Environment

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
      content1 <- dieOnErrors $ mkContents tls1
      content2 <- dieOnErrors $ mkContents tls2
      let mySort = sortBy (compare `on` fst)
          sortedContent1 = mySort content1
          sortedContent2 = mySort content2
          (nubProbs1, nubbedContent1) = nubContents sortedContent1
          (nubProbs2, nubbedContent2) = nubContents sortedContent2
          differences = compareContent nubbedContent1
                                       nubbedContent2
          allProbs = map First nubProbs1 ++ map Second nubProbs2
                  ++ differences
      mapM_ (putStrLn . pprFileProblem) allProbs

mkContents :: [TarLine] -> Either Errors [(FilenameDescr, TarLine)]
mkContents tls = case runState (mapM f tls) [] of
                 (xs, mapping) ->
                     case concat $ map (checkContent mapping) xs of
                     []   -> Right xs
                     errs -> Left errs
    where f tl = do fnd <- mkFilePathDescr (tlFileName tl)
                    return (fnd, tl)

nubContents :: [(FilenameDescr, TarLine)]
            -> ([Problem], [(FilenameDescr, TarLine)])
nubContents [] = ([], [])
nubContents [x] = ([], [x])
nubContents (x1@(fd1, tl1) : xs@((fd2, _) : _))
 | fd1 == fd2 = (DuplicateFile (tlFileName tl1) : ps, xs')
 | otherwise  = (ps, x1 : xs')
    where (ps, xs') = nubContents xs

mkFilePathDescr :: FilePath -> State ThingVersionMap FilenameDescr
mkFilePathDescr fp
 | Just [ghcVersion, _, middle, filename]
     <- re ("^ghc-" ++ versionRE ++ "(/.*)?/([^/]*)$") fp
    = do ghcVersionDescr <- do mapping <- get
                               case addThingVersion mapping "ghc" ghcVersion of
                                   Just mapping' ->
                                       do put mapping'
                                          return (VersionOf "ghc")
                                   Nothing ->
                                       return (FP ghcVersion)
         filename' <- mkFileNameDescr filename
         let fd = FP "ghc-" : ghcVersionDescr : FP middle : FP "/" : filename'
         return $ normalise fd
 | otherwise = return [FP fp]

mkFileNameDescr :: FilePath -> State ThingVersionMap FilenameDescr
mkFileNameDescr filename
 | Just [thing, thingVersion, _, ghcVersion, _]
       <- re ("^libHS(.*)-" ++ versionRE ++ "-ghc" ++ versionRE ++ "\\.so$")
             filename
    = do mapping <- get
         case addThingVersion mapping "ghc" ghcVersion of
             Just m ->
                 case addThingVersion m thing thingVersion of
                 Just m' ->
                     do put m'
                        return [FP "libHS", FP thing, FP "-", VersionOf thing,
                                FP "-ghc", VersionOf "ghc", FP ".so"]
                 _ -> unchanged
             _ -> unchanged
 | Just [way, thingVersion, _]
       <- re ("^libHSrts(_.*)?-ghc" ++ versionRE ++ "\\.so$")
             filename
    = do mapping <- get
         case addThingVersion mapping "ghc" thingVersion of
             Just mapping' ->
                 do put mapping'
                    return [FP "libHSrts", FP way, FP "-ghc", VersionOf "ghc",
                            FP ".so"]
             _ -> unchanged
 | Just [thing, thingVersion, _, way]
       <- re ("^libHS(.*)-" ++ versionRE ++ "(_.*)?\\.a$")
             filename
    = do mapping <- get
         case addThingVersion mapping thing thingVersion of
             Just mapping' ->
                 do put mapping'
                    return [FP "libHS", FP thing, FP "-", VersionOf thing,
                            FP way, FP ".a"]
             _ -> unchanged
 | Just [thing, thingVersion, _]
       <- re ("^HS(.*)-" ++ versionRE ++ "\\.o$")
             filename
    = do mapping <- get
         case addThingVersion mapping thing thingVersion of
             Just mapping' ->
                 do put mapping'
                    return [FP "HS", FP thing, FP "-", VersionOf thing,
                            FP ".o"]
             _ -> unchanged
 | otherwise = unchanged
    where unchanged = return [FP filename]

compareContent :: [(FilenameDescr, TarLine)] -> [(FilenameDescr, TarLine)]
               -> [FileProblem]
compareContent [] [] = []
compareContent xs [] = map (First  . ExtraFile . tlFileName . snd) xs
compareContent [] ys = map (Second . ExtraFile . tlFileName . snd) ys
compareContent xs1@((fd1, tl1) : xs1') xs2@((fd2, tl2) : xs2')
 = case fd1 `compare` fd2 of
   EQ -> map Change (compareTarLine tl1 tl2) ++ compareContent xs1' xs2'
   LT -> First  (ExtraFile (tlFileName tl1)) : compareContent xs1' xs2
   GT -> Second (ExtraFile (tlFileName tl2)) : compareContent xs1  xs2'

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

