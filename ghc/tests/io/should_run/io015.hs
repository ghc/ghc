import IO

main =
    isEOF >>= \ eof ->
    if eof then 
	return ()
    else
	getChar >>= \ c ->
        putChar c >>
        main
