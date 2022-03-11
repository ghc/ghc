{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RepPolyCase2 where

import Data.Kind
import GHC.Exts

type Rep :: Type -> RuntimeRep
type family Rep a

class Unboxable a where
  type Unbox a :: TYPE (Rep a)
  unbox :: a -> Unbox a

type instance Rep Int = IntRep
instance Unboxable Int where
  type Unbox Int = Int#
  unbox (I# i#) = i#

type instance Rep Double = DoubleRep
instance Unboxable Double where
  type Unbox Double = Double#
  unbox (D# d#) = d#

x :: () -> ()
x _ = case unbox (3 :: Int) of { _ -> () }
