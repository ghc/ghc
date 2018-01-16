-- This program compares the sizes of corresponding files in two trees

--   $ ./compareSizes --hi ~/ghc/darcs/ghc ~/ghc/6.12-branch/ghc
--        Size | Change | Filename
--      25644 | -0.99% | compiler/stage1/build/Demand.hi
--      21103 | -0.98% | compiler/stage2/build/Demand.hi
--     180044 | -0.98% | libraries/base/dist-install/build/GHC/Classes.hi
--       6415 | -0.58% | .../Data/Array/Parallel/Prelude/Base/Tuple.hi
--       6507 | -0.57% | .../Data/Array/Parallel/Prelude/Base/Tuple.hi
--   [...]
--       3264 |  3.16% | .../Parallel/Unlifted/Sequential/Flat/Enum.hi
--      51389 |  3.30% | .../build/Language/Haskell/Extension.hi
--       1415 | 72.18% | libraries/base/dist-install/build/Data/Tuple.hi
--   28752162 | -0.00% | TOTAL

-- Flags:
--    --o to compare object files.
--    --hi to compare interface files [DEFAULT]

-- There's a hack to avoid descending into '*_split' directories


module Main (main) where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Numeric
import System.Directory
import System.Environment
import System.FilePath
import System.IO

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          args <- getArgs
          case args of
              ["--hi", dir1, dir2] -> doit isHiFile dir1 dir2
              ["--o", dir1, dir2]  -> doit isOFile  dir1 dir2
              [dir1, dir2]         -> doit isHiFile dir1 dir2
              _ -> error "Bad arguments"

isHiFile :: FilePath -> Bool
isHiFile = (".hi" `isSuffixOf`)

isOFile :: FilePath -> Bool
isOFile = (".o" `isSuffixOf`)

doit :: (FilePath -> Bool) -> FilePath -> FilePath -> IO ()
doit isFileInteresting dir1 dir2
    = do when verbose $ putStrLn "Reading tree 1"
         tree1 <- getTree isFileInteresting dir1 "." "."
         when verbose $ putStrLn "Reading tree 2"
         tree2 <- getTree isFileInteresting dir2 "." "."
         when verbose $ putStrLn "Comparing trees"
         let ds = compareTree tree1 tree2
             ds' = sortBy comparingPercentage ds
             total = mkTotalDifference ds'
         mapM_ putStrLn $ showDifferences (ds' ++ [total])

verbose :: Bool
verbose = False

----------------------------------------------------------------------
-- Reading the trees

data Tree = Directory { nodeName :: FilePath, _subTrees :: [Tree] }
          | File      { nodeName :: FilePath, _filePath :: FilePath,
                                              _size :: Size }
    deriving Show

type Size = Integer
type Percentage = Double

getTree :: (FilePath -> Bool) -> FilePath -> FilePath -> FilePath -> IO Tree
getTree isFileInteresting root dir subdir
    = do entries <- getDirectoryContents (root </> dir </> subdir)
         mSubtrees <- mapM doEntry $ sort $ filter interesting entries
         return $ Directory subdir $ catMaybes mSubtrees
    where interesting "."  = False
          interesting ".." = False
          -- We don't want to descend into object-splitting directories,
          -- and compare the hundreds of split object files. Instead we
          -- just compare the combined object file outside of the _split
          -- directory.
          interesting d    = not ("_split" `isSuffixOf` d)
          dir' = dir <//> subdir
          doEntry :: FilePath -> IO (Maybe Tree)
          doEntry e = liftM Just (getTree isFileInteresting root dir' e)
            `catchIO` \_ -> -- XXX We ought to check this is a
                            -- "not a directory" exception really
                      if isFileInteresting e
                      then do let fn = dir' <//> e
                              h <- openFile (root </> fn) ReadMode
                              size <- hFileSize h
                              hClose h
                              return $ Just $ File e fn size
                      else return Nothing

catchIO :: IO a -> (IOError -> IO a) -> IO a
catchIO = catch

----------------------------------------------------------------------
-- Comparing the trees

data Difference = Difference FilePath Size Size Percentage
    deriving Show

compareTree :: Tree -> Tree -> [Difference]
compareTree (Directory _ ts1) (Directory _ ts2) = compareTrees ts1 ts2
compareTree (File _ fn s1) (File _ _ s2)
    = [Difference fn s1 s2 (mkPercentage s1 s2)]
compareTree _ _ = []

mkPercentage :: Size -> Size -> Percentage
mkPercentage s1 s2 = fromIntegral (s2 - s1) / fromIntegral s1

compareTrees :: [Tree] -> [Tree] -> [Difference]
compareTrees t1s@(t1 : t1s') t2s@(t2 : t2s')
    = case nodeName t1 `compare` nodeName t2 of
      LT ->                      compareTrees t1s' t2s
      EQ -> compareTree t1 t2 ++ compareTrees t1s' t2s'
      GT ->                      compareTrees t1s  t2s'
compareTrees _ _ = []

showDifferences :: [Difference] -> [String]
showDifferences ds = showTable [lpad, lpad, rpad]
                     (["Size", "Change", "Filename"] :
                      map showDifference ds)

showDifference :: Difference -> [String]
showDifference (Difference fp s1 _ percentage)
    = [show s1, showFFloat (Just 2) percentage "%", shorten fp]

shorten :: FilePath -> FilePath
shorten fp = let xs = map joinPath $ tails $ splitDirectories fp
             in case xs of
                x : _
                 | length x <= allowed ->
                    x
                _ -> case dropWhile ((> allowed - 4) . length) xs of
                     x : _ ->
                         "..." </> x
                     [] ->
                         take (allowed - 3) (takeFileName fp) ++ "..."
    where allowed = 50

comparingPercentage :: Difference -> Difference -> Ordering
comparingPercentage (Difference _ _ _ p1) (Difference _ _ _ p2)
    = compare p1 p2

mkTotalDifference :: [Difference] -> Difference
mkTotalDifference ds = let s1 = sum [ x | Difference _ x _ _ <- ds ]
                           s2 = sum [ x | Difference _ _ x _ <- ds ]
                           percentage = mkPercentage s1 s2
                       in Difference "TOTAL" s1 s2 percentage

----------------------------------------------------------------------
-- Utils

(<//>) :: FilePath -> FilePath -> FilePath
"." <//> fp = fp
dir <//> fn = dir </> fn

showTable :: [Int -> String -> String] -> [[String]] -> [String]
showTable padders xss
    = let lengths = map (maximum . map length) $ transpose xss
      in map (concat . intersperse " | " . zipWith3 id padders lengths) xss

lpad :: Int -> String -> String
lpad n s = replicate (n - length s) ' ' ++ s

rpad :: Int -> String -> String
rpad n s = s ++ replicate (n - length s) ' '
