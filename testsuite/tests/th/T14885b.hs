{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module T14885b where

class Foo1 a where
  foo1         :: forall b. a -> b -> b
  default foo1 :: forall b. a -> b -> b
  foo1 _ x = (x :: b)

$([d| class Foo2 a where
        foo2         :: forall b. a -> b -> b
        default foo2 :: forall b. a -> b -> b
        foo2 _ x = (x :: b)
    |])
