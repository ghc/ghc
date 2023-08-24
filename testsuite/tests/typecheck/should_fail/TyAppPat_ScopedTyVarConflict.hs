{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror=name-shadowing #-}

module Main where

-- Shouldn't work because we don't accept multiple occurrences of a binding variable.
foo :: forall a. Monoid a => Maybe a -> a
foo (Nothing @a) = (mempty :: a)
foo (Just @a x) = (x :: a)

main = do
  print (foo @String Nothing)
