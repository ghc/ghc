import IO -- 1.3

main =
    isEOF >>= \ eof ->
    if eof then 
	return ()
    else
	getChar >>= \ c ->
        putChar c >>
        main
