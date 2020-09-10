-- Source: https://github.com/ghc-proposals/ghc-proposals/pull/111#issuecomment-438125526
{-# LANGUAGE LinearTypes, GADTs, KindSignatures, DataKinds, TypeFamilies, PolyKinds #-}
module LinearPolyType where

import GHC.Types
data SBool :: Bool -> Type where
  STrue :: SBool True
  SFalse :: SBool False

type family If b t f where
  If True t _ = t
  If False _ f = f

dep :: SBool b -> Int %(If b One Many) -> Int
dep STrue x = x
dep SFalse _ = 0
