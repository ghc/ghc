module Settings.Packages.GhcPrim (ghcPrimPackageArgs) where

import GHC
import Oracles.Config.Flag
import Predicate

ghcPrimPackageArgs :: Args
ghcPrimPackageArgs = package ghcPrim ? mconcat
    [ builder GhcCabal ? arg "--flag=include-ghc-prim"

    , builder (Cc CompileC)    ?
      (not <$> flag GccLt44)   ?
      (not <$> flag GccIsClang) ?
      input "//cbits/atomic.c" ? arg "-Wno-sync-nand" ]
