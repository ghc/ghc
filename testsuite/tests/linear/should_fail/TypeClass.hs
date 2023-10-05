{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LinearTypes #-}
module Foo where

data Either a b = Left a | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left a) = f a
either f g (Right b) = g b

class Iden p where
  iden :: p a a

instance Iden (->) where
  iden x = x

class Cat p where
  comp :: p b c -> p a b -> p a c

instance Cat (->) where
  comp f g = \x -> f (g x)

class ArrowChoice a where
    (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
    (|||) :: a b d -> a c d -> a (Either b c) d

instance ArrowChoice (->) where
--    This doesn't work as |p| is inferred to be |-o| because of |Left|.
--    Then GHC complains that |f| is not the same type before it realises
--    that the overall type must be (->)
--    f +++ g = (Left `comp` f) ||| (Right `comp` g)
    f +++ g = (comp @(->) Left f) ||| (comp @(->) Right g)
    (|||) = either


-- This shouldn't work
foo :: a ⊸ a
foo = iden
