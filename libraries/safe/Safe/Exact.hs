{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TupleSections    #-}
{- |
Provides functions that raise errors in corner cases instead of returning \"best effort\"
results, then provides wrappers like the "Safe" module. For example:

* @'takeExact' 3 [1,2]@ raises an error, in contrast to 'take' which would return
  just two elements.

* @'takeExact' (-1) [1,2]@ raises an error, in contrast to 'take' which would return
  no elements.

* @'zip' [1,2] [1]@ raises an error, in contrast to 'zip' which would only pair up the
  first element.

Note that the @May@ variants of these functions are /strict/ in at least the bit of the prefix
of the list required to spot errors. The standard and @Note@ versions are lazy, but throw
errors later in the process - they do not check upfront.
-}
module Safe.Exact(
    -- * New functions
    takeExact, dropExact, splitAtExact,
    zipExact, zipWithExact,
    zip3Exact, zipWith3Exact,
    -- * Safe wrappers
    takeExactMay, takeExactNote, takeExactDef,
    dropExactMay, dropExactNote, dropExactDef,
    splitAtExactMay, splitAtExactNote, splitAtExactDef,
    zipExactMay, zipExactNote, zipExactDef,
    zipWithExactMay, zipWithExactNote, zipWithExactDef,
    zip3ExactMay, zip3ExactNote, zip3ExactDef,
    zipWith3ExactMay, zipWith3ExactNote, zipWith3ExactDef,
    ) where

import Control.Arrow
import Data.Maybe
import Safe.Util
import Safe.Partial

---------------------------------------------------------------------
-- HELPERS

addNote :: Partial => String -> String -> String -> a
addNote note fun msg = error $
    "Safe.Exact." ++ fun ++ ", " ++ msg ++ (if null note then "" else ", " ++ note)


---------------------------------------------------------------------
-- IMPLEMENTATIONS

{-# INLINE splitAtExact_ #-}
splitAtExact_ :: Partial => (String -> r) -> ([a] -> r) -> (a -> r -> r) -> Int -> [a] -> r
splitAtExact_ err nil cons o xs
    | o < 0 = err $ "index must not be negative, index=" ++ show o
    | otherwise = f o xs
    where
        f 0 xs = nil xs
        f i (x:xs) = x `cons` f (i-1) xs
        f i [] = err $ "index too large, index=" ++ show o ++ ", length=" ++ show (o-i)


{-# INLINE zipWithExact_ #-}
zipWithExact_ :: Partial => (String -> r) -> r -> (a -> b -> r -> r) -> [a] -> [b] -> r
zipWithExact_ err nil cons = f
    where
        f (x:xs) (y:ys) = cons x y $ f xs ys
        f [] [] = nil
        f [] _ = err "second list is longer than the first"
        f _ [] = err "first list is longer than the second"


{-# INLINE zipWith3Exact_ #-}
zipWith3Exact_ :: Partial => (String -> r) -> r -> (a -> b -> c -> r -> r) -> [a] -> [b] -> [c] -> r
zipWith3Exact_ err nil cons = f
    where
        f (x:xs) (y:ys) (z:zs) = cons x y z $ f xs ys zs
        f [] [] [] = nil
        f [] _ _ = err "first list is shorter than the others"
        f _ [] _ = err "second list is shorter than the others"
        f _ _ [] = err "third list is shorter than the others"


---------------------------------------------------------------------
-- TAKE/DROP/SPLIT

-- |
-- > takeExact n xs =
-- >   | n >= 0 && n <= length xs = take n xs
-- >   | otherwise                = error "some message"
takeExact :: Partial => Int -> [a] -> [a]
takeExact i xs = withFrozenCallStack $ splitAtExact_ (addNote "" "takeExact") (const []) (:) i xs

-- |
-- > dropExact n xs =
-- >   | n >= 0 && n <= length xs = drop n xs
-- >   | otherwise                = error "some message"
dropExact :: Partial => Int -> [a] -> [a]
dropExact i xs = withFrozenCallStack $ splitAtExact_ (addNote "" "dropExact") id (flip const) i xs

-- |
-- > splitAtExact n xs =
-- >   | n >= 0 && n <= length xs = splitAt n xs
-- >   | otherwise                = error "some message"
splitAtExact :: Partial => Int -> [a] -> ([a], [a])
splitAtExact i xs = withFrozenCallStack $ splitAtExact_ (addNote "" "splitAtExact")
    ([],) (\a b -> first (a:) b) i xs

takeExactNote :: Partial => String -> Int -> [a] -> [a]
takeExactNote note i xs = withFrozenCallStack $ splitAtExact_ (addNote note "takeExactNote") (const []) (:) i xs

takeExactMay :: Int -> [a] -> Maybe [a]
takeExactMay = splitAtExact_ (const Nothing) (const $ Just []) (\a -> fmap (a:))

takeExactDef :: [a] -> Int -> [a] -> [a]
takeExactDef def = fromMaybe def .^ takeExactMay

dropExactNote :: Partial => String -> Int -> [a] -> [a]
dropExactNote note i xs = withFrozenCallStack $ splitAtExact_ (addNote note "dropExactNote") id (flip const) i xs

dropExactMay :: Int -> [a] -> Maybe [a]
dropExactMay = splitAtExact_ (const Nothing) Just (flip const)

dropExactDef :: [a] -> Int -> [a] -> [a]
dropExactDef def = fromMaybe def .^ dropExactMay

splitAtExactNote :: Partial => String -> Int -> [a] -> ([a], [a])
splitAtExactNote note i xs = withFrozenCallStack $ splitAtExact_ (addNote note "splitAtExactNote")
    ([],) (\a b -> first (a:) b) i xs

splitAtExactMay :: Int -> [a] -> Maybe ([a], [a])
splitAtExactMay = splitAtExact_ (const Nothing)
    (\x -> Just ([], x)) (\a b -> fmap (first (a:)) b)

splitAtExactDef :: ([a], [a]) -> Int -> [a] -> ([a], [a])
splitAtExactDef def = fromMaybe def .^ splitAtExactMay

---------------------------------------------------------------------
-- ZIP

-- |
-- > zipExact xs ys =
-- >   | length xs == length ys = zip xs ys
-- >   | otherwise              = error "some message"
zipExact :: Partial => [a] -> [b] -> [(a,b)]
zipExact xs ys = withFrozenCallStack $ zipWithExact_ (addNote "" "zipExact") []  (\a b xs -> (a,b) : xs) xs ys

-- |
-- > zipWithExact f xs ys =
-- >   | length xs == length ys = zipWith f xs ys
-- >   | otherwise              = error "some message"
zipWithExact :: Partial => (a -> b -> c) -> [a] -> [b] -> [c]
zipWithExact f xs ys = withFrozenCallStack $ zipWithExact_ (addNote "" "zipWithExact") [] (\a b xs -> f a b : xs) xs ys


zipExactNote :: Partial => String -> [a] -> [b] -> [(a,b)]
zipExactNote note xs ys = withFrozenCallStack $ zipWithExact_ (addNote note "zipExactNote") []  (\a b xs -> (a,b) : xs) xs ys

zipExactMay :: [a] -> [b] -> Maybe [(a,b)]
zipExactMay = zipWithExact_ (const Nothing) (Just [])  (\a b xs -> fmap ((a,b) :) xs)

zipExactDef :: [(a,b)] -> [a] -> [b] -> [(a,b)]
zipExactDef def = fromMaybe def .^ zipExactMay

zipWithExactNote :: Partial => String -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithExactNote note f xs ys = withFrozenCallStack $ zipWithExact_ (addNote note "zipWithExactNote") []  (\a b xs -> f a b : xs) xs ys

zipWithExactMay :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWithExactMay f = zipWithExact_ (const Nothing) (Just [])  (\a b xs -> fmap (f a b :) xs)

zipWithExactDef :: [c] -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithExactDef def = fromMaybe def .^^ zipWithExactMay


-- |
-- > zip3Exact xs ys zs =
-- >   | length xs == length ys && length xs == length zs = zip3 xs ys zs
-- >   | otherwise                                        = error "some message"
zip3Exact :: Partial => [a] -> [b] -> [c] -> [(a,b,c)]
zip3Exact xs ys zs = withFrozenCallStack $ zipWith3Exact_ (addNote "" "zip3Exact") [] (\a b c xs -> (a, b, c) : xs) xs ys zs

-- |
-- > zipWith3Exact f xs ys zs =
-- >   | length xs == length ys && length xs == length zs = zipWith3 f xs ys zs
-- >   | otherwise                                        = error "some message"
zipWith3Exact :: Partial => (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3Exact f xs ys zs = withFrozenCallStack $ zipWith3Exact_ (addNote "" "zipWith3Exact") [] (\a b c xs -> f a b c : xs) xs ys zs


zip3ExactNote :: Partial => String -> [a] -> [b] -> [c]-> [(a,b,c)]
zip3ExactNote note xs ys zs = withFrozenCallStack $ zipWith3Exact_ (addNote note "zip3ExactNote") [] (\a b c xs -> (a,b,c) : xs) xs ys zs

zip3ExactMay :: [a] -> [b] -> [c] -> Maybe [(a,b,c)]
zip3ExactMay = zipWith3Exact_ (const Nothing) (Just [])  (\a b c xs -> fmap ((a,b,c) :) xs)

zip3ExactDef :: [(a,b,c)] -> [a] -> [b] -> [c] -> [(a,b,c)]
zip3ExactDef def = fromMaybe def .^^ zip3ExactMay

zipWith3ExactNote :: Partial => String -> (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3ExactNote note f xs ys zs = withFrozenCallStack $ zipWith3Exact_ (addNote note "zipWith3ExactNote") []  (\a b c xs -> f a b c : xs) xs ys zs

zipWith3ExactMay :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> Maybe [d]
zipWith3ExactMay f = zipWith3Exact_ (const Nothing) (Just [])  (\a b c xs -> fmap (f a b c :) xs)

zipWith3ExactDef :: [d] -> (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3ExactDef def = fromMaybe def .^^^ zipWith3ExactMay
