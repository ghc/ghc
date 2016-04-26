module Environment (setupEnvironment) where

import Base
import System.Environment

-- | The build system invokes many external builders whose behaviour is
-- influenced by the environment variables. We need to modify some of them
-- for better robustness of the build system.
setupEnvironment :: IO ()
setupEnvironment = do
    -- ghc-cabal refuses to work when GHC_PACKAGE_PATH is set (e.g. by Stack)
    unsetEnv "GHC_PACKAGE_PATH"

    -- in MinGW if PWD is set to a Windows "C:\\" style path then configure
    -- `pwd` will return the Windows path, and then modifying $PATH will fail.
    -- See https://github.com/snowleopard/hadrian/issues/189 for details.
    unsetEnv "PWD"

    -- On Windows, some path variables start a prefix like "C:\\" which may
    -- lead to failures of scripts such as autoreconf. One particular variable
    -- which causes issues is ACLOCAL_PATH. At the moment we simply reset it
    -- if it contains a problematic Windows path.
    -- TODO: Handle Windows paths in ACLOCAL_PATH more gracefully.
    aclocal <- lookupEnv "ACLOCAL_PATH"
    case aclocal of
        Nothing -> return ()
        Just s  -> when (":\\" `isPrefixOf` drop 1 s) $ unsetEnv "ACLOCAL_PATH"
