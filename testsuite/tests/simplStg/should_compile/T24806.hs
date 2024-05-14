module T24806 ( go ) where

data List a = Nil | Cons a !(List a) -- deriving Show

data Tup2 a b = Tup2 !a !b

-- All branches of go return either two properly tagged values *or* are bottom.
-- This means we should see something like:
--
--      (T24806.$wgo, <TagTuple[TagProper, TagProper]>) =
--
-- in the dump output.
-- See Note [Bottom functions are TagTagged] for details why.
go :: List a1 -> List a2 -> Tup2 (List a2) (List a2)
go Nil ys = Tup2 ys Nil
go (Cons _ xs) ys = case ys of
    Nil -> undefined
    Cons y ys' -> case go xs ys' of
        Tup2 s zs -> Tup2 s (Cons y zs)