-- !!! can't seek an AppendMode handle

import IO
#if defined(__MINGW32__)
import PrelHandle(hSetBinaryMode)
#endif

main = do
  h <- openFile "hSeek004.out" AppendMode
# if defined(__MINGW32__)
  hSetBinaryMode h True
# endif
  try (hSeek h AbsoluteSeek 0) >>= print
