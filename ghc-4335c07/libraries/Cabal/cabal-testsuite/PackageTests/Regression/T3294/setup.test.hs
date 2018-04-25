import Test.Cabal.Prelude
import Control.Monad.IO.Class
-- Test that executable recompilation works
-- https://github.com/haskell/setup/issues/3294
main = setupAndCabalTest $ do
    withSourceCopy . withDelay $ do
        writeSourceFile "Main.hs" "main = putStrLn \"aaa\""
        setup "configure" []
        setup "build" []
        runExe' "T3294" [] >>= assertOutputContains "aaa"
        delay
        writeSourceFile "Main.hs" "main = putStrLn \"bbb\""
        setup "build" []
        runExe' "T3294" [] >>= assertOutputContains "bbb"
