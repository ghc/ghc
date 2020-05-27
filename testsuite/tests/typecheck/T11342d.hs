{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module T11342d where

import GHC.TypeLits
import Data.Type.Equality

f1 :: CmpChar 'x' 'x' :~: EQ
f1 = Refl

f2 :: CmpChar 'x' 'y' :~: LT
f2 = Refl

f3 :: forall (a :: Char). CmpChar a a :~: EQ
f3 = Refl
