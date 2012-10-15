import System.Cmd
import System.Environment
import System.FilePath
import System.Directory
import Data.List
import Control.Applicative

main :: IO ()
main = do
  args <- getArgs
  dir <- getCurrentDirectory
  contents <- filter (`notElem` ignore) <$> getDirectoryContents (dir </> "output")
  if not $ null args then
    mapM_ copy [ "output" </> file  | file <- contents, ".html" `isSuffixOf` file, takeBaseName file `elem` args  ]
  else
    mapM_ copy [ "output" </> file | file <- contents]
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
  let new = "ref" </> takeFileName file
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
