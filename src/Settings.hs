module Settings (
    args
    ) where

import Base hiding (arg, args, Args)
import Settings.GhcPkg
import Settings.GhcCabal
import Settings.User
import Expression hiding (when, liftIO)

args :: Args
args = defaultArgs <> userArgs

-- TODO: add all other settings
defaultArgs :: Args
defaultArgs = mconcat
    [ cabalArgs
    , ghcPkgArgs
    , customPackageArgs ]
