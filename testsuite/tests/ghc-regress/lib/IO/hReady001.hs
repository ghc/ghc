-- !!! hReady test

 -- hReady should probably return False at the end of a file,
 -- but in GHC it returns True (known bug).

import IO
#ifdef i386_unknown_mingw32
import PrelHandle(hSetBinaryMode)
#endif

main = do
 h <- openFile "hReady001.hs" ReadMode
#ifdef i386_unknown_mingw32
 hSetBinaryMode h True
#endif
 hSeek h SeekFromEnd 0
 hReady h >>= print
