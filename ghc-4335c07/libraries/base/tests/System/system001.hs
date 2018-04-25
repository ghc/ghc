-- Not run on mingw, because of /dev/null use

import System.Cmd (system)
import System.Exit (ExitCode(..), exitWith)

main = do ec <- system "cat dog 1>/dev/null 2>&1"
          case ec of
              ExitSuccess ->
                  do putStr "What?!?\n"
                     ioError (userError "dog succeeded")
              ExitFailure _ ->
                  do ec <- system "cat system001.hs 2>/dev/null"
                     case ec of
                         ExitSuccess ->
                             exitWith ExitSuccess
                         ExitFailure _ ->
                             do putStr "What?!?\n"
                                ioError (userError "cat failed")
