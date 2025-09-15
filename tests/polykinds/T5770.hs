{-# LANGUAGE TypeFamilies,
             PolyKinds,
             ScopedTypeVariables
 #-}

module T5770 where

convert :: a -> b
convert = convert

type family Foo a 
type instance Foo Int = Bool

barT5770 :: forall a b c dummya. (b -> c) -> Foo a -> a
barT5770 f = (convert f :: Foo a -> a)

barT5769 :: forall b a. b -> (a, Foo a)
barT5769 f = (convert f :: (a, Foo a))

barT5768 :: forall a b. b -> (a, Foo a)
barT5768 f = (convert f :: (a, Foo a))
