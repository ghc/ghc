{-# LANGUAGE DataKinds, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleContexts #-}
module T2 where

import GHC.TypeLits

type family IntRep a where
  IntRep Int      = Integer
  IntRep Integer  = Integer
  IntRep Bool     = Integer
  IntRep a        = TypeError (Text "The type '" :<>: ShowType a :<>:
                               Text "' cannot be represented as an integer.")

convert :: Num (IntRep a) => a -> IntRep a
convert _ = 5

err = convert id


