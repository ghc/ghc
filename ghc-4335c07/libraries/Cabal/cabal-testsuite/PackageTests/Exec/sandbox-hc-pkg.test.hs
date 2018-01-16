import Test.Cabal.Prelude
import Data.Maybe
import System.Directory
import Control.Monad.IO.Class
main = cabalTest $ do
    withPackageDb $ do
        withSandbox $ do
            fails $ cabal "exec" ["my-executable"]
            cabal "install" []
            -- The library should not be available outside the sandbox
            ghcPkg' "list" [] >>= assertOutputDoesNotContain "my-0.1"
            -- When run inside 'cabal-exec' the 'sandbox hc-pkg list' sub-command
            -- should find the library.
            env <- getTestEnv
            -- NB: cabal_path might be relative, so we have to
            -- turn it absolute
            rel_cabal_path <- programPathM cabalProgram
            cabal_path <- liftIO $ makeAbsolute rel_cabal_path
            cabal' "exec" ["sh", "--", "-c"
                          , "cd subdir && " ++ show cabal_path ++
                            -- TODO: Ugh. Test abstractions leaking
                            -- through
                            " --sandbox-config-file " ++ show (testSandboxConfigFile env) ++
                            " sandbox hc-pkg list"]
                >>= assertOutputContains "my-0.1"
