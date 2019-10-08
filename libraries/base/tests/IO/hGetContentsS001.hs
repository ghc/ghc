import System.IO

file = "hGetContentsS001.txt"

main = do
  writeFile file "ab\ncd\nef\ngh\n"
  h <- openFile file ReadMode
  hGetContents' h >>= putStr
