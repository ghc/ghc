import System.Cmd
import System.Environment
import System.FilePath
import System.Exit
import System.Directory
import Data.List
import Control.Monad


main :: IO ()
main = do
  args <- getArgs
  dir <- getCurrentDirectory
  contents <- getDirectoryContents (dir </> "output")
  if not $ null args
    then
      mapM_ copy [ "output" </> file  | file <- contents, ".html" `isSuffixOf` file, takeBaseName file `elem` args  ]
    else
      mapM_ copy [ "output" </> file | file <- contents, ".html" `isSuffixOf` file ]


copy file = do
  let new = "tests" </> takeFileName file <.> ".ref"
  print file
  print new
  contents <- readFile file
  writeFile new (stripLinks contents)


stripLinks str =
  let prefix = "<a href=\"" in
  case stripPrefix prefix str of
    Just str' -> prefix ++ stripLinks (dropWhile (/= '"') str')
    Nothing ->
      case str of
        [] -> []
        x : xs -> x : stripLinks xs
