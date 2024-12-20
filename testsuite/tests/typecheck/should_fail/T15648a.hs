module T15648a where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

ueqT :: Q Type
ueqT = conT $ mkNameG_tc "ghc-internal" "GHC.Internal.Prim" "~#"
