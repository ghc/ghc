{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
module T12144_1 where

import Data.Kind (Type)

class C (a :: Type -> Type)
data T a = MkT (a -> Int) deriving C
