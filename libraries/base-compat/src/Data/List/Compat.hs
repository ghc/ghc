{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.List.Compat (
  module Base
#if !(MIN_VERSION_base(4,15,0))
, singleton
#endif

#if !(MIN_VERSION_base(4,11,0))
, iterate'
#endif

#if !(MIN_VERSION_base(4,8,0))
, all
, and
, any
, concat
, concatMap
, elem
, find
, foldl
, foldl'
, foldl1
, foldr
, foldr1
, length
, maximum
, maximumBy
, minimum
, minimumBy
, notElem
, nub
, nubBy
, null
, or
, product
, sum
, union
, unionBy
, mapAccumL
, mapAccumR

, isSubsequenceOf
, sortOn
, uncons
, scanl'
#endif

#if !(MIN_VERSION_base(4,5,0))
, dropWhileEnd
#endif
) where

#if MIN_VERSION_base(4,8,0)
import Data.List as Base
#else
import Data.List as Base hiding (
    all
  , and
  , any
  , concat
  , concatMap
  , elem
  , find
  , foldl
  , foldl'
  , foldl1
  , foldr
  , foldr1
  , length
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , notElem
  , nub
  , nubBy
  , null
  , or
  , product
  , sum
  , union
  , unionBy
  , mapAccumL
  , mapAccumR
  )
import Data.Foldable.Compat
import Data.Traversable
import Data.Ord (comparing)
#endif

#if !(MIN_VERSION_base(4,11,0))
import GHC.Exts (build)
import Prelude.Compat hiding (foldr, null)
#endif

#if !(MIN_VERSION_base(4,5,0))
-- | The 'dropWhileEnd' function drops the largest suffix of a list
-- in which the given predicate holds for all elements.  For example:
--
-- > dropWhileEnd isSpace "foo\n" == "foo"
-- > dropWhileEnd isSpace "foo bar" == "foo bar"
-- > dropWhileEnd isSpace ("foo\n" ++ undefined) == "foo" ++ undefined
--
-- /Since: 4.5.0.0/
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

#endif

#if !(MIN_VERSION_base(4,8,0))
-- | The 'isSubsequenceOf' function takes two lists and returns 'True' if the
-- first list is a subsequence of the second list.
--
-- @'isSubsequenceOf' x y@ is equivalent to @'elem' x ('subsequences' y)@.
--
-- /Since: 4.8.0.0/
--
-- ==== __Examples__
--
-- >>> isSubsequenceOf "GHC" "The Glorious Haskell Compiler"
-- True
-- >>> isSubsequenceOf ['a','d'..'z'] ['a'..'z']
-- True
-- >>> isSubsequenceOf [1..10] [10,9..0]
-- False
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []    _                    = True
isSubsequenceOf _     []                   = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b

-- | Sort a list by comparing the results of a key function applied to each
-- element.  @sortOn f@ is equivalent to @sortBy . comparing f@, but has the
-- performance advantage of only evaluating @f@ once for each element in the
-- input list.  This is called the decorate-sort-undecorate paradigm, or
-- Schwartzian transform.
--
-- /Since: 4.8.0.0/
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

-- | Decompose a list into its head and tail. If the list is empty,
-- returns 'Nothing'. If the list is non-empty, returns @'Just' (x, xs)@,
-- where @x@ is the head of the list and @xs@ its tail.
--
-- /Since: 4.8.0.0/
uncons                  :: [a] -> Maybe (a, [a])
uncons []               = Nothing
uncons (x:xs)           = Just (x, xs)

-- | A strictly accumulating version of 'scanl'
{-# NOINLINE [1] scanl' #-}
scanl'           :: (b -> a -> b) -> b -> [a] -> [b]
-- This peculiar form is needed to prevent scanl' from being rewritten
-- in its own right hand side.
scanl' = scanlGo'
  where
    scanlGo'           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo' f !q ls    = q : (case ls of
                            []   -> []
                            x:xs -> scanlGo' f (f q x) xs)

-- | /O(n^2)/. The 'nub' function removes duplicate elements from a list.
-- In particular, it keeps only the first occurrence of each element.
-- (The name 'nub' means \`essence\'.)
-- It is a special case of 'nubBy', which allows the programmer to supply
-- their own equality test.
nub                     :: (Eq a) => [a] -> [a]
nub                     =  nubBy (==)


-- | The 'nubBy' function behaves just like 'nub', except it uses a
-- user-supplied equality predicate instead of the overloaded '=='
-- function.
nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
-- stolen from HBC
nubBy eq l              = nubBy' l []
  where
    nubBy' [] _         = []
    nubBy' (y:ys) xs
       | elem_by eq y xs = nubBy' ys xs
       | otherwise       = y : nubBy' ys (y:xs)

-- Not exported:
-- Note that we keep the call to `eq` with arguments in the
-- same order as in the reference (prelude) implementation,
-- and that this order is different from how `elem` calls (==).
-- See #2528, #3280 and #7913.
-- 'xs' is the list of things we've seen so far,
-- 'y' is the potential new element
elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
elem_by _  _ []         =  False
elem_by eq y (x:xs)     =  x `eq` y || elem_by eq y xs

-- | The 'union' function returns the list union of the two lists.
-- For example,
--
-- > "dog" `union` "cow" == "dogcw"
--
-- Duplicates, and elements of the first list, are removed from the
-- the second list, but if the first list contains duplicates, so will
-- the result.
-- It is a special case of 'unionBy', which allows the programmer to supply
-- their own equality test.

union                   :: (Eq a) => [a] -> [a] -> [a]
union                   = unionBy (==)

-- | The 'unionBy' function is the non-overloaded version of 'union'.
unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs
#endif

#if !(MIN_VERSION_base(4,11,0))
-- | 'iterate\'' is the strict version of 'iterate'.
--
-- It ensures that the result of each application of force to weak head normal
-- form before proceeding.
{-# NOINLINE [1] iterate' #-}
iterate' :: (a -> a) -> a -> [a]
iterate' f x =
    let x' = f x
    in x' `seq` (x : iterate' f x')

{-# INLINE [0] iterate'FB #-} -- See Note [Inline FB functions]
iterate'FB :: (a -> b -> b) -> (a -> a) -> a -> b
iterate'FB c f x0 = go x0
  where go x =
            let x' = f x
            in x' `seq` (x `c` go x')

{-# RULES
"iterate'"    [~1] forall f x.   iterate' f x = build (\c _n -> iterate'FB c f x)
"iterate'FB"  [1]                iterate'FB (:) = iterate'
 #-}
#endif

#if !(MIN_VERSION_base(4,15,0))
-- | Produce singleton list.
--
-- >>> singleton True
-- [True]
--
-- /Since: 4.14.0.0/
--
singleton :: a -> [a]
singleton x = [x]
#endif
