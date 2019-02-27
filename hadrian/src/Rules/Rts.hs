module Rules.Rts (rtsRules) where

import Hadrian.Utilities
import Settings.Builders.Common

-- | Dynamic RTS library files need syslinks without the dummy version number.
-- This is for backwards compatability (the old make buid system omitted the
-- dummy version number).
rtsRules :: Rules ()
rtsRules = priority 2 $ do
    root <- buildRootRules
    [ root -/- "//libHSrts_*-ghc*.so",
      root -/- "//libHSrts_*-ghc*.dylib",
      root -/- "//libHSrts-ghc*.so",
      root -/- "//libHSrts-ghc*.dylib"]
      |%> linkRts

linkRts :: FilePath -> Action ()
linkRts rtsLibFilePath' = do
    -- Add the dummy version number
    let versionlessPrefix = "libHSrts"
        prefix = versionlessPrefix ++ "-1.0"
        rtsLibFile' = takeFileName rtsLibFilePath'
        rtsLibFile = maybe
            (error $ "Expected RTS library file to start with "
                    ++ versionlessPrefix)
            (prefix ++)
            (stripPrefix versionlessPrefix rtsLibFile')

    createFileLinkUntracked rtsLibFile rtsLibFilePath'