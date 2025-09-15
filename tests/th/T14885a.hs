{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module T14885a where

class Foo1 a where
  bar1 :: forall b. a -> b -> b
  bar1 _ x = (x :: b)

$([d| class Foo2 a where
        bar2 :: forall b. a -> b -> b
        bar2 _ x = (x :: b)

      instance Foo2 Int where
        bar2 :: forall b. Int -> b -> b
        bar2 _ x = (x :: b)
    |])
