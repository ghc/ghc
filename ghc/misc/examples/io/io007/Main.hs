main =
    openFile "io007.in" ReadMode >>= \ hIn ->
    hPutStr hIn "test" `handle`
    \ (IllegalOperation _) -> 
        hGetContents hIn >>= \ stuff ->
        hPutStr stdout stuff
