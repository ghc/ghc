{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE GADTs, KindSignatures #-}
module Bug85 where

import Data.Kind (Type)

-- explicitly stated non-trivial kind
data Foo :: (Type -> Type) -> Type -> Type where
  Bar :: f x -> Foo f (f x)

-- Just kind * but explicitly written
data Baz :: Type where
  Baz' :: Baz

-- No kind signature written down at all
data Qux where
  Quux :: Qux
