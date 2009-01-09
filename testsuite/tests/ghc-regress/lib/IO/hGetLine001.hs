-- !!! testing hGetLine

import IO
#ifdef mingw32_HOST_OS
import GHC.Handle(hSetBinaryMode)
#endif

-- one version of 'cat'
main = do
  let loop h = do b <- hIsEOF h
        	  if b then return ()
		       else do l <- hGetLine h; putStrLn l; loop h
  loop stdin 

  h <- openFile "hGetLine001.hs" ReadMode

#ifdef mingw32_HOST_OS
  hSetBinaryMode stdout True
  hSetBinaryMode h True
#endif

  hSetBuffering h NoBuffering
  loop h

  hSeek h AbsoluteSeek 0
  hSetBuffering h LineBuffering
  loop h

  hSeek h AbsoluteSeek 0
  hSetBuffering h (BlockBuffering (Just 83))
  loop h
