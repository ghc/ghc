import Test.Cabal.Prelude
import Control.Monad ( (>=>) )
main = cabalTest $ do
    -- the exe
    cabal' "new-run" ["foo"] >>= assertOutputContains "Hello World"
    -- the lib
    fails (cabal' "new-run" ["ExeAndLib"]) >>= assertOutputDoesNotContain "Hello World"

