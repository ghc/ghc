import LibSystem (system, ExitCode(..), exitWith)

main = 
    system "cat dog 1>/dev/null 2>&1" >>= \ ec ->
    case ec of
        ExitSuccess   -> putStr "What?!?\n" >> fail "dog succeeded"
        ExitFailure _ ->
            system "cat Main.hs 2>/dev/null" >>= \ ec ->
	    case ec of
	        ExitSuccess   -> exitWith ExitSuccess
	        ExitFailure _ -> putStr "What?!?\n" >> fail "cat failed"
