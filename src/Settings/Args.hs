module Settings.Args (
    args
    ) where

import Settings.User
import Settings.GhcPkg
import Settings.GhcCabal
import Expression

args :: Args
args = defaultArgs <> userArgs

-- TODO: add all other settings
defaultArgs :: Args
defaultArgs = mconcat
    [ cabalArgs
    , ghcPkgArgs
    , customPackageArgs ]
