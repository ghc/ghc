-- !!! hReady test

 -- hReady should probably return False at the end of a file,
 -- but in GHC it returns True (known bug).

import IO
#if defined(__MINGW32__)
import PrelHandle(hSetBinaryMode)
#endif

main = do
 h <- openFile "hReady001.hs" ReadMode
#if defined(__MINGW32__)
 hSetBinaryMode h True
#endif
 hSeek h SeekFromEnd 0
 hReady h >>= print
