-- !!! can't seek an AppendMode handle

import System.IO
import System.IO.Error

main = do
  h <- openFile "hSeek004.out" AppendMode
  tryIOError (hSeek h AbsoluteSeek 0) >>= print
