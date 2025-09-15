{-# OPTIONS_GHC -Wredundant-constraints -dsuppress-uniques #-}
{-# LANGUAGE DefaultSignatures, InstanceSigs #-}
module M ( f ) where

-- Redundant constraint
f :: Eq a => a -> ()
f _ = ()

-- Redundant constraint in expression signature
g _ = (\x -> ()) :: Eq a => a -> ()

-- GHC highlights more than necessary
h :: (Eq a, Ord b) => a -> b -> b
h _ b
    | b <= b = b
    | otherwise = b

-- Redundant constraint in specialize pragma.
-- Also generates an unrelated warning:
-- > Forall'd constraint ‘Eq a’ is not bound in RULE lhs
{-# SPECIALISE spec :: Eq a => a -> Int -> Int #-}

spec :: Ord b => a -> b -> b
spec _ b
    | b <= b = b
    | otherwise = b

class Foo a where
    foo :: [a]
    -- Redundant constraint in default method
    default foo :: Show a => [a]
    foo = []

class Bar a where
    bar :: Ord b => a -> b -> a

instance Bar Int where
    -- Redundant Constraint in Instance Signature
    bar :: (Eq b, Ord b) => Int -> b -> Int
    bar n _ = n
