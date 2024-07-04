{-# language PackageImports #-}

module Local where

import GHC.Prim
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (lift)
-- just to be sure that the file isn't accidentally picked up locally
import "dep" DepApi (dep, A (A))
import {-# source #-} Num (num)

splc :: ExpQ
splc = lift @_ @Int (num + d)
  where
    A d = dep
