import Test.Cabal.Prelude
import Data.Maybe
main = cabalTest $ do
    withPackageDb $ do
        withSandbox $ do
            fails $ cabal "exec" ["my-executable"]
            cabal "install" []
            -- The library should not be available outside the sandbox
            ghcPkg' "list" [] >>= assertOutputDoesNotContain "my-0.1"
            -- Execute ghc-pkg inside the sandbox; it should find my-0.1
            cabal' "exec" ["ghc-pkg", "list"]
                >>= assertOutputContains "my-0.1"
