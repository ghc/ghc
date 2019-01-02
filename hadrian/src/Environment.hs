module Environment (setupEnvironment) where

import System.Environment

-- | The build system invokes many external builders whose behaviour is
-- influenced by the environment variables. We need to modify some of them
-- for better robustness of the build system.
setupEnvironment :: IO ()
setupEnvironment = do
    -- Cabal refuses to work when GHC_PACKAGE_PATH is set (e.g. by Stack)
    unsetEnv "GHC_PACKAGE_PATH"

    -- cabal new-exec sets GHC_ENVIRONMENT, it needs to be unset for GHC
    -- invocations to work properly
    unsetEnv "GHC_ENVIRONMENT"

    -- in MinGW if PWD is set to a Windows "C:\\" style path then configure
    -- `pwd` will return the Windows path, and then modifying $PATH will fail.
    -- See https://github.com/snowleopard/hadrian/issues/189 for details.
    unsetEnv "PWD"
