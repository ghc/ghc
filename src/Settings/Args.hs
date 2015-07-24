module Settings.Args (
    args
    ) where

import Settings.User
import Settings.GhcM
import Settings.GhcPkg
import Settings.GhcCabal
import Expression

args :: Args
args = defaultArgs <> userArgs

-- TODO: add all other settings
-- TODO: add src-hc-args = -H32m -O
defaultArgs :: Args
defaultArgs = mconcat
    [ cabalArgs
    , ghcPkgArgs
    , ghcMArgs
    , customPackageArgs ]
