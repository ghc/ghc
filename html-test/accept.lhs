#!/usr/bin/env runhaskell
\begin{code}
{-# LANGUAGE CPP #-}
import System.Cmd
import System.Environment
import System.FilePath
import System.Directory
import Data.List
import Control.Applicative

baseDir = takeDirectory __FILE__

main :: IO ()
main = do
  contents <- filter (`notElem` ignore) <$> getDirectoryContents (baseDir </> "output")
  args <- getArgs
  if not $ null args then
    mapM_ copy [ baseDir </> "output" </> file | file <- contents, ".html" `isSuffixOf` file, takeBaseName file `elem` args  ]
  else
    mapM_ copy [ baseDir </> "output" </> file | file <- contents]
  where
    ignore = [
        "."
      , ".."
      , "doc-index.html"
      , "index-frames.html"
      , "index.html"
      ]

copy :: FilePath -> IO ()
copy file = do
  let new = baseDir </> "ref" </> takeFileName file
  if ".html" `isSuffixOf` file then do
    putStrLn (file ++ " -> " ++ new)
    stripLinks <$> readFile file >>= writeFile new
  else do
    -- copy css, images, etc.
    copyFile file new

stripLinks :: String -> String
stripLinks str =
  let prefix = "<a href=\"" in
  case stripPrefix prefix str of
    Just str' -> prefix ++ stripLinks (dropWhile (/= '"') str')
    Nothing ->
      case str of
        [] -> []
        x : xs -> x : stripLinks xs
\end{code}
