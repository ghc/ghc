import System.IO
import System.IO.Error

main = do
  h <- openFile "hClose001.tmp" WriteMode
  hPutStr h "junk" 
  hClose h
  hPutStr h "junk" `catchIOError` \ err -> if isIllegalOperation err then putStr "Okay\n" else error "Not okay\n"
