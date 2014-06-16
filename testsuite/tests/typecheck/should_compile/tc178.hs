{-# LANGUAGE FlexibleInstances #-}

-- This one tickled the kind-check in TcType.matchTys,
-- which should use sub-kinding

module ShouldCompile where

type TypeRep = ()

class Typeable2 t where
  typeOf2 :: t a b -> TypeRep
 
class Typeable1 t where
  typeOf1 :: t a -> TypeRep

class Typeable0 a where
  typeOf0 :: a -> TypeRep

instance Typeable2 (->) where
  typeOf2 = undefined

instance (Typeable2 t, Typeable0 a) => Typeable1 (t a) where
  typeOf1 = undefined

instance (Typeable1 t, Typeable0 a) => Typeable0 (t a) where
  typeOf0 = undefined

class Typeable0 a => Data0 a where
  dataTypeOf0 :: a -> Bool

instance (Data0 a, Data0 b) => Data0 (a -> b) where
  dataTypeOf0 = undefined

foo :: (Typeable0 a, Typeable0 b) => (a -> b) -> TypeRep
foo f = typeOf0 f
