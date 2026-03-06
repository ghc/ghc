{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T11141 where

data F a = F a
instance Show a => Show (F a) where
    show :: forall a. Show a => F a -> String
    show (F x) = show x

{- Previously emitted error:

    Could not deduce (Show a0)
    from the context (Show a)
      bound by the type signature for show :: Show a => F a -> String
      at A.hs:8:13-45
    The type variable ‘a0’ is ambiguous
    When checking that:
        forall a. Show a => forall a1. Show a1 => F a1 -> String
      is more polymorphic than: forall a. Show a => F a -> String
    When checking that instance signature for ‘show’
      is more general than its signature in the class
      Instance sig: forall a.
                    Show a =>
                    forall a1. Show a1 => F a1 -> String
         Class sig: forall a. Show a => F a -> String
    In the instance declaration for ‘Show (F a)’
-}
