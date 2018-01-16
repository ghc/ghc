import System.Environment (getArgs)
import qualified Data.Set as Set
import System.IO

main = do
  [file1,file2] <- getArgs
  dict <- readFileLatin1 file1
  input <- readFileLatin1 file2
  let set = Set.fromList (words dict)
  let tocheck = words input
  print (filter (`Set.notMember` set) tocheck)

readFileLatin1 f = do
  h <- openFile f ReadMode
  hSetEncoding h latin1
  hGetContents h
