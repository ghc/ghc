-- !!! Test seeking

import IO

main = do
    h  <- openFile "io013.in" ReadMode
    sz <- hFileSize h
    print sz
    hSeek h SeekFromEnd (-3)
    x <- hGetChar h
    putStr (x:"\n")
    hSeek h RelativeSeek (-2)
    w <- hGetChar h
    putStr (w:"\n")
    True <- hIsSeekable h
    hClose h

