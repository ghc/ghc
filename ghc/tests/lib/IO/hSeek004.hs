-- !!! can't seek an AppendMode handle

import IO

main = do
  h <- openFile "hSeek004.out" AppendMode
  hSetBinaryMode h True
  try (hSeek h AbsoluteSeek 0) >>= print
