import Control.Exception as CE

exception_uncaught = ioError (userError "error")
exception_caught   = CE.try exception_uncaught :: IO (Either CE.IOException ())
