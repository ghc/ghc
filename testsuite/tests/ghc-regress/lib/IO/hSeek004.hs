-- !!! can't seek an AppendMode handle

import IO
#ifdef i386_unknown_mingw32
import GHC.Handle(hSetBinaryMode)
#endif

main = do
  h <- openFile "hSeek004.out" AppendMode
#ifdef i386_unknown_mingw32
  hSetBinaryMode h True
#endif
  try (hSeek h AbsoluteSeek 0) >>= print
