-- Not run on mingw, because of /dev/null use

import System.Cmd (system)
import System.Exit (ExitCode(..), exitWith)

main = 
    system "cat dog 1>/dev/null 2>&1" >>= \ ec ->
    case ec of
        ExitSuccess   -> putStr "What?!?\n" >> ioError (userError "dog succeeded")
        ExitFailure _ ->
            system "cat system001.hs 2>/dev/null" >>= \ ec ->
	    case ec of
	        ExitSuccess   -> exitWith ExitSuccess
	        ExitFailure _ -> putStr "What?!?\n" >> ioError (userError "cat failed")
