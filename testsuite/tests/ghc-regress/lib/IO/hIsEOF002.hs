-- !!! test hIsEOF in various buffering situations

import System.IO

main = do
  h <- openFile "hIsEOF002.hs" ReadMode
  hSetBuffering h NoBuffering
  hSeek h SeekFromEnd 0
  hIsEOF h >>= print
  hSeek h SeekFromEnd (-1)
  hIsEOF h >>= print
  hGetChar h >>= print 

  hSetBuffering h LineBuffering
  hSeek h SeekFromEnd 0
  hIsEOF h >>= print
  hSeek h SeekFromEnd (-1)
  hIsEOF h >>= print
  hGetChar h >>= print  

  hSetBuffering h (BlockBuffering (Just 1))
  hSeek h SeekFromEnd 0
  hIsEOF h >>= print
  hSeek h SeekFromEnd (-1)
  hIsEOF h >>= print
  hGetChar h >>= print  

  hSetBuffering h (BlockBuffering Nothing)
  hSeek h SeekFromEnd 0
  hIsEOF h >>= print
  hSeek h SeekFromEnd (-1)
  hIsEOF h >>= print
  hGetChar h >>= print  
  hClose h

  h <- openFile "hIsEOF002.out" WriteMode
  hPutStrLn h "hello, world"
  hClose h

  h <- openFile "hIsEOF002.out" ReadWriteMode
  hSetBuffering h NoBuffering
  hSeek h SeekFromEnd 0
  hIsEOF h >>= print
  hPutChar h 'x'
  hIsEOF h >>= print
  hSeek h SeekFromEnd (-1)
  hIsEOF h >>= print
  hGetChar h >>= print 
