main = 
    openFile "io013.in" ReadMode >>= \ h ->
    hFileSize h >>= \ sz -> 
    putText sz >>
    putChar '\n' >>
    hSeek h SeekFromEnd (-3) >>
    hGetChar h >>= \ x ->
    putStr (x:"\n") >>
    hSeek h RelativeSeek (-2) >>
    hGetChar h >>= \ w ->
    putStr (w:"\n") >>
    hIsSeekable h >>= \ True ->
    hClose h >>
    openFile "/dev/null" ReadMode >>= \ h ->
    hIsSeekable h >>= \ False ->
    hClose h
    