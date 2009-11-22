import System.Cmd
import System.Environment
import System.FilePath
import System.Exit
import System.Directory
import Data.List
import Control.Monad
import Text.Regex


main = do
  args <- getArgs
  dir <- getCurrentDirectory
  contents <- getDirectoryContents (dir </> "output")
  if not $ null args
    then
      mapM copy [ "output" </> file  | file <- contents, ".html" `isSuffixOf` file, takeBaseName file `elem` args  ]
    else
      mapM copy [ "output" </> file | file <- contents, ".html" `isSuffixOf` file ]


copy file = do
  let new = "tests" </> takeFileName file <.> ".ref"
  print file
  print new
  contents <- readFile file
  writeFile new (stripLinks contents)


stripLinks f = subRegex (mkRegexWithOpts "<A HREF=[^>]*>" False False) f "<A HREF=\"\">"
