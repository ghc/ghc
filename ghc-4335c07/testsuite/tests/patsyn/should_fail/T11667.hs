{-# LANGUAGE PatternSynonyms, GADTs #-}
module T11667 where

{-
The following four pattern synonym declarations should give valid
error messages
-}

-- Check if we mention no explicit type signature (or the correct
-- signature "Eq a => Maybe a")
pattern Pat1 :: Eq a => Maybe a
pattern Pat1 <- Just 42

-- Check if we mention no explicit type signature (or the correct
-- signature "forall b. () => forall a. b ~ Bool => a -> b -> (b, T)")
data T where MkT :: a -> T
pattern Pat2 :: () => b ~ Bool => a -> b -> (b, T)
pattern Pat2 x y = (y, MkT x)

-- Check if we do not tell the user that we could not deduce (Show a)
-- from the "required" context. Also, check if we do not give the
-- possible fix that suggests to add (Show a) to the "required" context.
pattern Pat3 :: Eq a => Show a => a -> Maybe a
pattern Pat3 x <- Just x

-- Check if we return a valid error message concerning the missing
-- constraint (Num a) when the bidirectional pattern synonym is used
-- in an expression context
data S a where MkS :: (Num a, Show a) => a -> S a
pattern Pat4 :: (Eq a) => (Show a) => S a
pattern Pat4 = MkS 42
