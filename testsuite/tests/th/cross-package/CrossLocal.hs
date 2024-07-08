{-# language PackageImports #-}

module CrossLocal where

import GHC.Prim
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (lift)
-- just to be sure that the file isn't accidentally picked up locally
import "dep" CrossDepApi (dep, A (A))
import {-# source #-} CrossNum (num)
import CrossObj (numo)

splc :: ExpQ
splc = lift @_ @Int (num + d + numo)
  where
    A d = dep
