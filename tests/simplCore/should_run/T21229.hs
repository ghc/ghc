{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Main where

data B = T | F deriving Show

class Sing (b :: B) where sing :: B
instance Sing 'T where sing = T
instance Sing 'F where sing = F

f :: forall a. Sing a => Int -> (Int -> B -> B) -> B
f 0 g = g 0 (sing @a)
f n g = f @a (n-1) g

h :: forall a. Sing a => Int -> (Int -> B -> B) -> B
h = case sing @a of
      T -> f @'T
      F -> f @a
{-# NOINLINE h #-}

main = print $ h @'T 0 (\ _ a -> a)
