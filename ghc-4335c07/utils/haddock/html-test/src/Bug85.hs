{-# LANGUAGE GADTs, KindSignatures #-}
module Bug85 where

-- explicitly stated non-trivial kind
data Foo :: (* -> *) -> * -> * where
  Bar :: f x -> Foo f (f x)

-- Just kind * but explicitly written
data Baz :: * where
  Baz' :: Baz

-- No kind signature written down at all
data Qux where
  Quux :: Qux
