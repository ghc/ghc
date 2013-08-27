#!/usr/bin/env runhaskell
\begin{code}
{-# LANGUAGE CPP #-}
import System.Environment
import System.FilePath
import System.Directory
import Data.List
import Control.Applicative
import Control.Monad

baseDir :: FilePath
baseDir = takeDirectory __FILE__

main :: IO ()
main = do
  contents <- filter (not . ignore) <$> getDirectoryContents (baseDir </> "out")
  args <- getArgs
  mapM_ copyDir $ if not (null args)
                  then filter ((`elem` args) . takeBaseName) contents
                  else contents
  where
    ignore =
      foldr (liftA2 (||)) (const False) [
        (== ".")
      , (== "..")
      , isPrefixOf "index"
      , isPrefixOf "doc-index"
      ]

-- | Copy a directory to ref, one level deep.
copyDir :: FilePath -> IO ()
copyDir dir = do
  let old = baseDir </> "out" </> dir
      new = baseDir </> "ref" </> dir
  alreadyExists <- doesDirectoryExist new
  unless alreadyExists $ do
    putStrLn (old ++ " -> " ++ new)
    createDirectoryIfMissing True new
    files <- getDirectoryContents old >>= filterM (liftM not . doesDirectoryExist)
    let files' = filter (\x -> x /= "." && x /= "..") files
    mapM_ (\f -> copyFile' (old </> f) (new </> f)) files'
      where
        copyFile' o n = do
          putStrLn $ o ++ " -> " ++ n
          copyFile o n
\end{code}
