module Settings.Flavours.GhcInGhci (ghcInGhciFlavour) where

import Expression
import Flavour
import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Common

-- Please update doc/flavours.md when changing this file.
ghcInGhciFlavour :: Flavour
ghcInGhciFlavour = defaultFlavour
    { name        = "ghc-in-ghci"
    , args        = defaultBuilderArgs <> ghciArgs <> defaultPackageArgs
    -- We can't build DLLs on Windows (yet). Actually we should only
    -- include the dynamic way when we have a dynamic host GHC, but just
    -- checking for Windows seems simpler for now.
    , libraryWays = pure [vanilla] <> pure [ dynamic | not windowsHost ]
    , rtsWays     = pure [vanilla, threaded] <> pure [ dynamic | not windowsHost ]
    , dynamicGhcPrograms = return False }

ghciArgs :: Args
ghciArgs = sourceArgs SourceArgs
    { hsDefault  = mconcat $
        [ pure ["-O0", "-H64m"]
        , naturalInBaseFixArgs
        ]
    , hsLibrary  = mempty
    , hsCompiler = mempty
    , hsGhc = mempty }
