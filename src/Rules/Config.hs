module Rules.Config (configRules) where

import Base

-- We add the following line to 'configure.ac' in order to produce configuration
-- file "system.config" from "system.config.in" by running 'configure' script.
configCommand :: String
configCommand = "AC_CONFIG_FILES([" ++ configPath ++ "system.config])"

configRules :: Rules ()
configRules = do
    configPath -/- "system.config" %> \_ -> do
        need [configPath -/- "system.config.in", "configure"]
        putBuild "Running configure..."
        cmd "bash configure" -- TODO: get rid of 'bash'

    -- TODO: this rule won't rebuild if configure.ac is changed. Worth fixing?
    "configure" %> \_ -> do
        -- Make sure 'configure.ac' script contains a line with configCommand
        script <- fmap lines . liftIO $ readFile "configure.ac"
        when (configCommand `notElem` script) $ do
            putBuild $ "Adding '" ++ configCommand ++ "' to configure.ac..."
            let (before, rest) = break ("AC_CONFIG_FILES" `isPrefixOf`) script
            when (null rest) $ do
                putError "No AC_CONFIG_FILES command in configure.ac!"
            let newScript = unlines $ before ++ [configCommand] ++ rest
            length newScript `seq` liftIO (writeFile "configure.ac" newScript)
        putBuild "Running autoconf..."
        cmd "bash autoconf" -- TODO: get rid of 'bash'
