import MiniPrel

main = (ccall getchar)    `thenU` ( \ ch ->
	   case ch of
	     -1# -> (ccall exit 0#)
	     _   -> (ccall putchar ch) `thenU` ( \ _ ->
		    main )
	 )
