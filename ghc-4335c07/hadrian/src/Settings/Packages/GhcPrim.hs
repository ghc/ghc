module Settings.Packages.GhcPrim (ghcPrimPackageArgs) where

import Oracles.Flag
import Expression

ghcPrimPackageArgs :: Args
ghcPrimPackageArgs = package ghcPrim ? mconcat
    [ builder GhcCabal ? arg "--flag=include-ghc-prim"

    , builder (Cc CompileC)     ?
      (not <$> flag GccIsClang) ?
      input "//cbits/atomic.c"  ? arg "-Wno-sync-nand" ]
