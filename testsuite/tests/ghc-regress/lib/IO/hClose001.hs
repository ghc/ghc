import IO

main = do
  h <- openFile "hClose001.tmp" WriteMode
  hPutStr h "junk" 
  hClose h
  hPutStr h "junk" `catch` \ err -> if isIllegalOperation err then putStr "Okay\n" else error "Not okay\n"
