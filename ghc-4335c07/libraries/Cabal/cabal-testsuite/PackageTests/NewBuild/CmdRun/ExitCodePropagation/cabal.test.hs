import Test.Cabal.Prelude
import Control.Monad ( (>=>) )
import System.Exit (ExitCode(ExitFailure))
main = cabalTest $
    fails (cabal' "new-run" ["foo"]) >>= assertExitCode (ExitFailure 42)

