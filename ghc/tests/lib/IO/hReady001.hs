-- !!! hReady test

 -- hReady should probably return False at the end of a file,
 -- but in GHC it returns True (known bug).

import IO

main = do
 h <- openFile "hReady001.hs" ReadMode
 hSeek h SeekFromEnd 0
 hReady h >>= print
