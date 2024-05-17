{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}

module TypeFamilies3 where

-- | A closed type family
type family Foo a where
  Foo () = Int
  Foo _ = ()

-- | An open family
type family Bar a

type instance Bar Int = ()
type instance Bar () = Int

-- | A data family
data family Baz a

data instance Baz () = Baz1
data instance Baz Int = Baz2 Bool
newtype instance Baz Double = Baz3 Float
