import IO -- 1.3

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
    ~True <- hIsSeekable h
    hClose h
    h <- openFile "/dev/null" ReadMode
    ~False <- hIsSeekable h
    hClose h

