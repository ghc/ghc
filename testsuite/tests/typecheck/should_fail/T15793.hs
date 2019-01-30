{-# Language RankNTypes       #-}
{-# Language TypeFamilies     #-}
{-# Language TypeApplications #-}
{-# Language PolyKinds        #-}

module T15793 where
import Data.Kind

type family
  F1 (a :: Type) :: Type where
  F1 a = Maybe a

f1 :: F1 a
f1 = Nothing

type family
  F2 :: forall (a :: Type). Type where
  F2 @a = Maybe a
