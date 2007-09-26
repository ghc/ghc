-- !!! hReady test

 -- hReady should probably return False at the end of a file,
 -- but in GHC it returns True (known bug).

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
 hReady h >>= print
