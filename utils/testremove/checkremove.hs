
module Main (main) where

import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import System.Environment
import System.FilePath

data CleanWhat = CleanFile FilePath
               | CleanRec  FilePath
    deriving (Read, Show)

newtype FilePathFragment = FilePathFragment BSC.ByteString
    deriving (Show, Eq, Ord)

toFilePathFragments :: FilePath -> [FilePathFragment]
toFilePathFragments
    = map (FilePathFragment . BSC.pack) . splitDirectories . normalise

fromFilePathFragments :: [FilePathFragment] -> FilePath
fromFilePathFragments xs = joinPath $ map f $ reverse xs
    where f (FilePathFragment frag) = BSC.unpack frag

data Tree = Node !FileInfo !(Map FilePathFragment Tree)
data FileInfo = FileInfo {
                    fiBefore :: !Bool,
                    fiAfter :: !Bool,
                    fiDeleted :: !Bool
                }

beforeFileInfo :: FileInfo
beforeFileInfo = noFileInfo { fiBefore  = True }

afterFileInfo :: FileInfo
afterFileInfo = noFileInfo { fiAfter   = True }

noFileInfo :: FileInfo
noFileInfo = FileInfo {
                 fiBefore  = False,
                 fiAfter   = False,
                 fiDeleted = False
             }

readTree :: FileInfo -> FilePath -> IO (Tree)
readTree fi fp = do xs <- readFile fp
                    return $ mkTree fi $ lines xs

mkTree :: FileInfo -> [FilePath] -> Tree
mkTree fi fps = f (sort fragss)
    where fragss = map toFilePathFragments fps
          f xs = let xs' = g $ groupBy ((==) `on` head)
                             $ filter (not . null) xs
                 in Node fi xs'
          g xss = mapFromList' [ (head (head xs), f (map tail xs))
                               | xs <- xss ]

mapFromList' :: Ord a => [(a, b)] -> Map a b
mapFromList' xs = seqAll xs `seq` Map.fromList xs
    where seqAll [] = ()
          seqAll ((x, y) : xys) = x `seq` y `seq` seqAll xys

{-
... = OK: will happen if a file in a non-existent directory is rm'd [1]
..D = OK: will happen if a non-existent file is rm'd [1]
.A. = suspicious: Why wasn't this file cleaned?
.AD = OK: This is what object files look like
B.. = suspicious: Where did the file go?
B.D = suspicious: Why are we removing a file that existed before?
BA. = OK: This is what source files look like
BAD = suspicious: Why are we removing a file that existed before?

[1] some files may only be created on certain platforms, or in certain
    build-system configurations, but the cleaning code is deliberately
    simple so it will always clean them regardless
-}
pprSuspicious :: Tree -> [String]
pprSuspicious t = f [] t
    where f ps (Node fi m) = suspicious (fromFilePathFragments ps) fi
                          ++ concat [ f (p : ps) m' | (p, m') <- Map.toList m ]
          suspicious fp (FileInfo False True  False) = ["File not deleted:    " ++ show fp]
          suspicious fp (FileInfo True  False False) = ["File disappeared:    " ++ show fp]
          suspicious fp (FileInfo True  False True)  = ["Deleted before file: " ++ show fp]
          suspicious fp (FileInfo True  True  True)  = ["Deleted before file: " ++ show fp]
          suspicious _  _                            = []

pprTree :: Tree -> [String]
pprTree t = f [] t
    where f ps (Node fi m) = (pprInfo fi ++ " " ++ fromFilePathFragments ps)
                           : concat [ f (p : ps) m' | (p, m') <- Map.toList m ]

pprInfo :: FileInfo -> String
pprInfo (FileInfo before after deleted) = [if before  then 'B' else '.',
                                           if after   then 'A' else '.',
                                           if deleted then 'D' else '.']

mergeTree :: Tree -> Tree -> Tree
mergeTree (Node fi1 m1) (Node fi2 m2)
    = Node (mergeFileInfo fi1 fi2)
           (Map.unionWith mergeTree m1 m2)

mergeFileInfo :: FileInfo -> FileInfo -> FileInfo
mergeFileInfo (FileInfo before1 after1 deleted1)
              (FileInfo before2 after2 deleted2)
    = FileInfo (before1 || before2) (after1 || after2) (deleted1 || deleted2)

main :: IO ()
main = do args <- getArgs
          case args of
              [contentsBeforeFile, contentsAfterFile, wouldBeCleanedFile] ->
                  doit contentsBeforeFile contentsAfterFile wouldBeCleanedFile
              _ ->
                  error "Bad args"

doit :: FilePath -> FilePath -> FilePath -> IO ()
doit contentsBeforeFile contentsAfterFile wouldBeCleanedFile
 = do contentsBefore <- readTree beforeFileInfo contentsBeforeFile
      contentsAfter  <- readTree afterFileInfo  contentsAfterFile
      let contentsMerged = mergeTree contentsBefore contentsAfter
      wouldBeCleaned <- liftM (map read . lines) $ readFile wouldBeCleanedFile
      let contentsCleaned = simulateCleans contentsMerged wouldBeCleaned
      mapM_ putStrLn $ pprSuspicious contentsCleaned

simulateCleans :: Tree -> [CleanWhat] -> Tree
simulateCleans = foldl' simulateClean

simulateClean :: Tree -> CleanWhat -> Tree
simulateClean t (CleanFile fp) = at t fp markDeleted
simulateClean t (CleanRec  fp) = at t fp markSubtreeDeleted

markDeleted :: Tree -> Tree
markDeleted (Node fi m) = Node (fi { fiDeleted = True }) m

markSubtreeDeleted :: Tree -> Tree
markSubtreeDeleted (Node fi m) = Node fi' (Map.map markSubtreeDeleted m)
    where fi' = -- "rm -r" will only delete things that are there afterwards
                if fiAfter fi then fi { fiDeleted = True } else fi

at :: Tree -> FilePath -> (Tree -> Tree) -> Tree
at t fp f = at' t (toFilePathFragments fp) f

at' :: Tree -> [FilePathFragment] -> (Tree -> Tree) -> Tree
at' t           []       f = f t
at' (Node fi m) (p : ps) f = Node fi m'
    where m' = Map.insert p (at' t ps f) m
          t = Map.findWithDefault (Node noFileInfo Map.empty) p m

