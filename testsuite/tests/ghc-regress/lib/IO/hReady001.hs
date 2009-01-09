-- !!! hReady test

 -- hReady should throw and EOF exception at the end of a file. Trac #1063.

import IO
#ifdef mingw32_HOST_OS
import GHC.Handle(hSetBinaryMode)
#endif

main = do
 h <- openFile "hReady001.hs" ReadMode
#ifdef mingw32_HOST_OS
 hSetBinaryMode h True
#endif
 hReady h >>= print
 hSeek h SeekFromEnd 0
 (hReady h >> return ()) `catch` print

