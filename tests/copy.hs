import System.Cmd
import System.Environment
import System.FilePath
import System.Exit
import System.Directory
import Data.List
import Control.Monad

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
  copyFile file ("tests" </> takeFileName file <.> ".ref") 
