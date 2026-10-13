{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}

-- See Note [Let-bound coercions] in GHC.Core.
-- The big coercion should appear just once in the optimised code,
-- let-bound, rather than once in each branch of the case.
module LetBoundCoercion1 where

import Data.Coerce

type family F a where
  F Int  = Bool
  F Bool = Char
  F Char = Int

type family G a where
  G (Maybe a) = Either (F a) (G [a])
  G [a]       = (a, F (F a))

type T = Either (Maybe (G (Maybe Int))) [F (F Int)]

-- Case-of-case pushes the outer case, and the cast, into each branch.
f :: Int -> Maybe Int -> Int
f n x = case (coerce (case x of
                         Nothing -> g1 n
                         Just 0  -> g2 n
                         Just 1  -> g1 (n+1)
                         Just _  -> g2 (n-1)) :: T) of
          Left _  -> h1 n
          Right _ -> h2 n

g1, g2 :: Int -> Either (Maybe (Either Bool (Int,Char))) [Char]
g1 n = if n > 0 then Right ['c'] else Left Nothing
g2 n = Left (Just (Left (n > 0)))
h1, h2 :: Int -> Int
h1 n = n+1
h2 n = n*2
{-# NOINLINE g1 #-}
{-# NOINLINE g2 #-}
{-# NOINLINE h1 #-}
{-# NOINLINE h2 #-}
