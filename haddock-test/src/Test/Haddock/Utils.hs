module Test.Haddock.Utils where


import Control.Monad

import Data.Maybe

import System.Directory
import System.FilePath


mlast :: [a] -> Maybe a
mlast = listToMaybe . reverse


partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM p (x:xs) = do
    (ss, fs) <- partitionM p xs
    b <- p x
    pure $ if b then (x:ss, fs) else (ss, x:fs)


whenM :: Monad m => m Bool -> m () -> m ()
whenM mb action = mb >>= \b -> when b action


getDirectoryTree :: FilePath -> IO [FilePath]
getDirectoryTree path = do
    (dirs, files) <- partitionM isDirectory =<< contents
    subfiles <- fmap concat . forM dirs $ \dir ->
        map (dir </>) <$> getDirectoryTree (path </> dir)
    pure $ files ++ subfiles
  where
    contents = filter realEntry <$> getDirectoryContents path
    isDirectory entry = doesDirectoryExist $ path </> entry
    realEntry entry = not $ entry == "." || entry == ".."


createEmptyDirectory :: FilePath -> IO ()
createEmptyDirectory path = do
    whenM (doesDirectoryExist path) $ removeDirectoryRecursive path
    createDirectory path
