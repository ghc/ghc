import IO -- 1.3

main =
    openFile "io007.in" ReadMode >>= \ hIn ->
    hPutStr hIn "test" `catch`
    \ err ->
        if isIllegalOperation err then
        hGetContents hIn >>= \ stuff ->
        hPutStr stdout stuff
	else
	    error "Oh dear\n"
