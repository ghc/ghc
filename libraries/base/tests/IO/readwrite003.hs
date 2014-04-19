import System.IO

file = "readwrite003.txt"

main = do
  writeFile file "ab\ncd\nef\ngh"
  h <- openFile file ReadWriteMode
  hGetLine h
  hPutStrLn h "yz"
  hClose h
  h <- openBinaryFile file ReadMode
  hGetContents h >>= putStr
