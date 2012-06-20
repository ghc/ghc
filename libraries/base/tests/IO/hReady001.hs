-- !!! hReady test

 -- hReady should throw and EOF exception at the end of a file. Trac #1063.

import System.IO
import System.IO.Error

main = do
 h <- openFile "hReady001.hs" ReadMode
 hReady h >>= print
 hSeek h SeekFromEnd 0
 (hReady h >> return ()) `catchIOError` print
