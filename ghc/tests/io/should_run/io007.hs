import IO

main =
    openFile "io007.hs" ReadMode >>= \ hIn ->
    hPutStr hIn "test" `catch`
    \ err ->
        if isIllegalOperation err then
        hGetContents hIn >>= \ stuff ->
        hPutStr stdout stuff
	else
	    error "Oh dear\n"
