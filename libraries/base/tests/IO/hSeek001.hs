-- !!! Test seeking

import System.IO

main = do
    h  <- openFile "hSeek001.in" ReadMode
    True <- hIsSeekable h
    hSeek h SeekFromEnd (-1)
    z <- hGetChar h
    putStr (z:"\n")
    hSeek h SeekFromEnd (-3)
    x <- hGetChar h
    putStr (x:"\n")
    hSeek h RelativeSeek (-2)
    w <- hGetChar h
    putStr (w:"\n")
    hSeek h RelativeSeek 2
    z <- hGetChar h
    putStr (z:"\n")
    hSeek h AbsoluteSeek (0)
    a <- hGetChar h
    putStr (a:"\n")
    hSeek h AbsoluteSeek (10)
    k <- hGetChar h
    putStr (k:"\n")
    hSeek h AbsoluteSeek (25)
    z <- hGetChar h
    putStr (z:"\n")
    hClose h
