-- !!! hReady test

 -- hReady should throw and EOF exception at the end of a file. Trac #1063.

import IO
#ifdef i386_unknown_mingw32
import GHC.Handle(hSetBinaryMode)
#endif

main = do
 h <- openFile "hReady001.hs" ReadMode
#ifdef i386_unknown_mingw32
 hSetBinaryMode h True
#endif
 hReady h >>= print
 hSeek h SeekFromEnd 0
 (hReady h >> return ()) `catch` print

