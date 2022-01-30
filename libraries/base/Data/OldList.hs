{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ScopedTypeVariables,
             MagicHash, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Operations on lists.
--
-----------------------------------------------------------------------------

module Data.OldList
   (
   -- * Basic functions

     (++)
   , head
   , last
   , tail
   , init
   , uncons
   , singleton
   , null
   , length

   -- * List transformations
   , map
   , reverse

   , intersperse
   , intercalate
   , transpose

   , subsequences
   , permutations

   -- * Reducing lists (folds)

   , foldl
   , foldl'
   , foldl1
   , foldl1'
   , foldr
   , foldr1

   -- ** Special folds

   , concat
   , concatMap
   , and
   , or
   , any
   , all
   , sum
   , product
   , maximum
   , minimum

   -- * Building lists

   -- ** Scans
   , scanl
   , scanl'
   , scanl1
   , scanr
   , scanr1

   -- ** Accumulating maps
   , mapAccumL
   , mapAccumR

   -- ** Infinite lists
   , iterate
   , iterate'
   , repeat
   , replicate
   , cycle

   -- ** Unfolding
   , unfoldr

   -- * Sublists

   -- ** Extracting sublists
   , take
   , drop
   , splitAt

   , takeWhile
   , dropWhile
   , dropWhileEnd
   , span
   , break

   , stripPrefix

   , group

   , inits
   , tails

   -- ** Predicates
   , isPrefixOf
   , isSuffixOf
   , isInfixOf

   -- * Searching lists

   -- ** Searching by equality
   , elem
   , notElem
   , lookup

   -- ** Searching with a predicate
   , find
   , filter
   , partition

   -- * Indexing lists
   -- | These functions treat a list @xs@ as a indexed collection,
   -- with indices ranging from 0 to @'length' xs - 1@.

   , (!!)

   , elemIndex
   , elemIndices

   , findIndex
   , findIndices

   -- * Zipping and unzipping lists

   , zip
   , zip3
   , zip4, zip5, zip6, zip7

   , zipWith
   , zipWith3
   , zipWith4, zipWith5, zipWith6, zipWith7

   , unzip
   , unzip3
   , unzip4, unzip5, unzip6, unzip7

   -- * Special lists

   -- ** Functions on strings
   , lines
   , words
   , unlines
   , unwords

   -- ** \"Set\" operations

   , nub

   , delete
   , (\\)

   , union
   , intersect

   -- ** Ordered lists
   , sort
   , sortOn
   , insert

   -- * Generalized functions

   -- ** The \"@By@\" operations
   -- | By convention, overloaded functions have a non-overloaded
   -- counterpart whose name is suffixed with \`@By@\'.
   --
   -- It is often convenient to use these functions together with
   -- 'Data.Function.on', for instance @'sortBy' ('compare'
   -- \`on\` 'fst')@.

   -- *** User-supplied equality (replacing an @Eq@ context)
   -- | The predicate is assumed to define an equivalence.
   , nubBy
   , deleteBy
   , deleteFirstsBy
   , unionBy
   , intersectBy
   , groupBy

   -- *** User-supplied comparison (replacing an @Ord@ context)
   -- | The function is assumed to define a total ordering.
   , sortBy
   , insertBy
   , maximumBy
   , minimumBy

   -- ** The \"@generic@\" operations
   -- | The prefix \`@generic@\' indicates an overloaded function that
   -- is a generalized version of a "Prelude" function.

   , genericLength
   , genericTake
   , genericDrop
   , genericSplitAt
   , genericIndex
   , genericReplicate

   ) where

import Data.Maybe
import Data.Bits        ( (.&.) )
import Data.Char        ( isSpace )
import Data.Ord         ( comparing )
import Data.Tuple       ( fst, snd )

import GHC.Num
import GHC.Real
import GHC.List
import GHC.Base

infix 5 \\ -- comment to fool cpp: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#cpp-and-string-gaps

-- -----------------------------------------------------------------------------
-- List functions

-- | The 'dropWhileEnd' function drops the largest suffix of a list
-- in which the given predicate holds for all elements.  For example:
--
-- >>> dropWhileEnd isSpace "foo\n"
-- "foo"
--
-- >>> dropWhileEnd isSpace "foo bar"
-- "foo bar"
--
-- > dropWhileEnd isSpace ("foo\n" ++ undefined) == "foo" ++ undefined
--
-- @since 4.5.0.0
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

-- | \(\mathcal{O}(\min(m,n))\). The 'stripPrefix' function drops the given
-- prefix from a list. It returns 'Nothing' if the list did not start with the
-- prefix given, or 'Just' the list after the prefix, if it does.
--
-- >>> stripPrefix "foo" "foobar"
-- Just "bar"
--
-- >>> stripPrefix "foo" "foo"
-- Just ""
--
-- >>> stripPrefix "foo" "barfoo"
-- Nothing
--
-- >>> stripPrefix "foo" "barfoobaz"
-- Nothing
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
 | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

-- | The 'elemIndex' function returns the index of the first element
-- in the given list which is equal (by '==') to the query element,
-- or 'Nothing' if there is no such element.
--
-- >>> elemIndex 4 [0..]
-- Just 4
elemIndex       :: Eq a => a -> [a] -> Maybe Int
elemIndex x     = findIndex (x==)

-- | The 'elemIndices' function extends 'elemIndex', by returning the
-- indices of all elements equal to the query element, in ascending order.
--
-- >>> elemIndices 'o' "Hello World"
-- [4,7]
elemIndices     :: Eq a => a -> [a] -> [Int]
elemIndices x   = findIndices (x==)

-- | The 'find' function takes a predicate and a list and returns the
-- first element in the list matching the predicate, or 'Nothing' if
-- there is no such element.
--
-- >>> find (> 4) [1..]
-- Just 5
--
-- >>> find (< 0) [1..10]
-- Nothing
find            :: (a -> Bool) -> [a] -> Maybe a
find p          = listToMaybe . filter p

-- | The 'findIndex' function takes a predicate and a list and returns
-- the index of the first element in the list satisfying the predicate,
-- or 'Nothing' if there is no such element.
--
-- >>> findIndex isSpace "Hello World!"
-- Just 5
findIndex       :: (a -> Bool) -> [a] -> Maybe Int
findIndex p     = listToMaybe . findIndices p

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
--
-- >>> findIndices (`elem` "aeiou") "Hello World!"
-- [1,4,7]
findIndices      :: (a -> Bool) -> [a] -> [Int]
#if defined(USE_REPORT_PRELUDE)
findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]
#else
-- Efficient definition, adapted from Data.Sequence
-- (Note that making this INLINABLE instead of INLINE allows
-- 'findIndex' to fuse, fixing #15426.)
{-# INLINABLE findIndices #-}
findIndices p ls = build $ \c n ->
  let go x r k | p x       = I# k `c` r (k +# 1#)
               | otherwise = r (k +# 1#)
  in foldr go (\_ -> n) ls 0#
#endif  /* USE_REPORT_PRELUDE */

-- | \(\mathcal{O}(\min(m,n))\). The 'isPrefixOf' function takes two lists and
-- returns 'True' iff the first list is a prefix of the second.
--
-- >>> "Hello" `isPrefixOf` "Hello World!"
-- True
--
-- >>> "Hello" `isPrefixOf` "Wello Horld!"
-- False
isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _         =  True
isPrefixOf _  []        =  False
isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys

-- | The 'isSuffixOf' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
--
-- >>> "ld!" `isSuffixOf` "Hello World!"
-- True
--
-- >>> "World" `isSuffixOf` "Hello World!"
-- False
isSuffixOf              :: (Eq a) => [a] -> [a] -> Bool
ns `isSuffixOf` hs      = maybe False id $ do
      delta <- dropLengthMaybe ns hs
      return $ ns == dropLength delta hs
      -- Since dropLengthMaybe ns hs succeeded, we know that (if hs is finite)
      -- length ns + length delta = length hs
      -- so dropping the length of delta from hs will yield a suffix exactly
      -- the length of ns.

-- A version of drop that drops the length of the first argument from the
-- second argument. If xs is longer than ys, xs will not be traversed in its
-- entirety.  dropLength is also generally faster than (drop . length)
-- Both this and dropLengthMaybe could be written as folds over their first
-- arguments, but this reduces clarity with no benefit to isSuffixOf.
--
-- >>> dropLength "Hello" "Holla world"
-- " world"
--
-- >>> dropLength [1..] [1,2,3]
-- []
dropLength :: [a] -> [b] -> [b]
dropLength [] y = y
dropLength _ [] = []
dropLength (_:x') (_:y') = dropLength x' y'

-- A version of dropLength that returns Nothing if the second list runs out of
-- elements before the first.
--
-- >>> dropLengthMaybe [1..] [1,2,3]
-- Nothing
dropLengthMaybe :: [a] -> [b] -> Maybe [b]
dropLengthMaybe [] y = Just y
dropLengthMaybe _ [] = Nothing
dropLengthMaybe (_:x') (_:y') = dropLengthMaybe x' y'

-- | The 'isInfixOf' function takes two lists and returns 'True'
-- iff the first list is contained, wholly and intact,
-- anywhere within the second.
--
-- >>> isInfixOf "Haskell" "I really like Haskell."
-- True
--
-- >>> isInfixOf "Ial" "I really like Haskell."
-- False
isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

-- | \(\mathcal{O}(n^2)\). The 'nub' function removes duplicate elements from a
-- list. In particular, it keeps only the first occurrence of each element. (The
-- name 'nub' means \`essence\'.) It is a special case of 'nubBy', which allows
-- the programmer to supply their own equality test.
--
-- >>> nub [1,2,3,4,3,2,1,2,4,3,5]
-- [1,2,3,4,5]
nub                     :: (Eq a) => [a] -> [a]
nub                     =  nubBy (==)

-- | The 'nubBy' function behaves just like 'nub', except it uses a
-- user-supplied equality predicate instead of the overloaded '=='
-- function.
--
-- >>> nubBy (\x y -> mod x 3 == mod y 3) [1,2,4,5,6]
-- [1,2,6]
nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
#if defined(USE_REPORT_PRELUDE)
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)
#else
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
#endif


-- | \(\mathcal{O}(n)\). 'delete' @x@ removes the first occurrence of @x@ from
-- its list argument. For example,
--
-- >>> delete 'a' "banana"
-- "bnana"
--
-- It is a special case of 'deleteBy', which allows the programmer to
-- supply their own equality test.
delete                  :: (Eq a) => a -> [a] -> [a]
delete                  =  deleteBy (==)

-- | \(\mathcal{O}(n)\). The 'deleteBy' function behaves like 'delete', but
-- takes a user-supplied equality predicate.
--
-- >>> deleteBy (<=) 4 [1..10]
-- [1,2,3,5,6,7,8,9,10]
deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _  _ []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys

-- | The '\\' function is list difference (non-associative).
-- In the result of @xs@ '\\' @ys@, the first occurrence of each element of
-- @ys@ in turn (if any) has been removed from @xs@.  Thus
--
-- > (xs ++ ys) \\ xs == ys.
--
-- >>> "Hello World!" \\ "ell W"
-- "Hoorld!"
--
-- It is a special case of 'deleteFirstsBy', which allows the programmer
-- to supply their own equality test.

(\\)                    :: (Eq a) => [a] -> [a] -> [a]
(\\)                    =  foldl (flip delete)

-- | The 'union' function returns the list union of the two lists.
-- For example,
--
-- >>> "dog" `union` "cow"
-- "dogcw"
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

-- | The 'intersect' function takes the list intersection of two lists.
-- For example,
--
-- >>> [1,2,3,4] `intersect` [2,4,6,8]
-- [2,4]
--
-- If the first list contains duplicates, so will the result.
--
-- >>> [1,2,2,3,4] `intersect` [6,4,4,2]
-- [2,2,4]
--
-- It is a special case of 'intersectBy', which allows the programmer to
-- supply their own equality test. If the element is found in both the first
-- and the second list, the element from the first list will be used.

intersect               :: (Eq a) => [a] -> [a] -> [a]
intersect               =  intersectBy (==)

-- | The 'intersectBy' function is the non-overloaded version of 'intersect'.
intersectBy             :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy _  [] _     =  []
intersectBy _  _  []    =  []
intersectBy eq xs ys    =  [x | x <- xs, any (eq x) ys]

-- | \(\mathcal{O}(n)\). The 'intersperse' function takes an element and a list
-- and \`intersperses\' that element between the elements of the list. For
-- example,
--
-- >>> intersperse ',' "abcde"
-- "a,b,c,d,e"
intersperse             :: a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs


-- Not exported:
-- We want to make every element in the 'intersperse'd list available
-- as soon as possible to avoid space leaks. Experiments suggested that
-- a separate top-level helper is more efficient than a local worker.
prependToAll            :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

-- | 'intercalate' @xs xss@ is equivalent to @('concat' ('intersperse' xs xss))@.
-- It inserts the list @xs@ in between the lists in @xss@ and concatenates the
-- result.
--
-- >>> intercalate ", " ["Lorem", "ipsum", "dolor"]
-- "Lorem, ipsum, dolor"
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

-- | The 'transpose' function transposes the rows and columns of its argument.
-- For example,
--
-- >>> transpose [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
--
-- If some of the rows are shorter than the following rows, their elements are skipped:
--
-- >>> transpose [[10,11],[20],[],[30,31,32]]
-- [[10,20,30],[11,31],[32]]
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x : xs) : xss) = combine x hds xs tls
  where
    -- We tie the calculations of heads and tails together
    -- to prevent heads from leaking into tails and vice versa.
    -- unzip makes the selector thunk arrangements we need to
    -- ensure everything gets cleaned up properly.
    (hds, tls) = unzip [(hd, tl) | hd : tl <- xss]
    combine y h ys t = (y:h) : transpose (ys:t)
    {-# NOINLINE combine #-}
  {- Implementation note:
  If the bottom part of the function was written as such:

  ```
  transpose ((x : xs) : xss) = (x:hds) : transpose (xs:tls)
  where
    (hds,tls) = hdstls
    hdstls = unzip [(hd, tl) | hd : tl <- xss]
    {-# NOINLINE hdstls #-}
  ```
  Here are the steps that would take place:

  1. We allocate a thunk, `hdstls`, representing the result of unzipping.
  2. We allocate selector thunks, `hds` and `tls`, that deconstruct `hdstls`.
  3. Install `hds` as the tail of the result head and pass `xs:tls` to
     the recursive call in the result tail.

  Once optimised, this code would amount to:

  ```
  transpose ((x : xs) : xss) = (x:hds) : (let tls = snd hdstls in transpose (xs:tls))
  where
    hds = fst hdstls
    hdstls = unzip [(hd, tl) | hd : tl <- xss]
    {-# NOINLINE hdstls #-}
  ```

  In particular, GHC does not produce the `tls` selector thunk immediately;
  rather, it waits to do so until the tail of the result is actually demanded.
  So when `hds` is demanded, that does not resolve `snd hdstls`; the tail of the
  result keeps `hdstls` alive.

  By writing `combine` and making it NOINLINE, we prevent GHC from delaying
  the selector thunk allocation, requiring that `hds` and `tls` are actually
  allocated to be passed to `combine`.
  -}


-- | The 'partition' function takes a predicate and a list, and returns
-- the pair of lists of elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p xs == (filter p xs, filter (not . p) xs)
--
-- >>> partition (`elem` "aeiou") "Hello World!"
-- ("eoo","Hll Wrld!")
partition               :: (a -> Bool) -> [a] -> ([a],[a])
{-# INLINE partition #-}
partition p xs = foldr (select p) ([],[]) xs

select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x ~(ts,fs) | p x       = (x:ts,fs)
                    | otherwise = (ts, x:fs)

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a list, passing
-- an accumulating parameter from left to right, and returning a final
-- value of this accumulator together with the new list.
mapAccumL :: (acc -> x -> (acc, y)) -- Function of elt of input list
                                    -- and accumulator, returning new
                                    -- accumulator and elt of result list
          -> acc            -- Initial accumulator
          -> [x]            -- Input list
          -> (acc, [y])     -- Final accumulator and result list
{-# NOINLINE [1] mapAccumL #-}
mapAccumL _ s []        =  (s, [])
mapAccumL f s (x:xs)    =  (s'',y:ys)
                           where (s', y ) = f s x
                                 (s'',ys) = mapAccumL f s' xs

{-# RULES
"mapAccumL" [~1] forall f s xs . mapAccumL f s xs = foldr (mapAccumLF f) pairWithNil xs s
"mapAccumLList" [1] forall f s xs . foldr (mapAccumLF f) pairWithNil xs s = mapAccumL f s xs
 #-}

pairWithNil :: acc -> (acc, [y])
{-# INLINE [0] pairWithNil #-}
pairWithNil x = (x, [])

mapAccumLF :: (acc -> x -> (acc, y)) -> x -> (acc -> (acc, [y])) -> acc -> (acc, [y])
{-# INLINE [0] mapAccumLF #-}
mapAccumLF f = \x r -> oneShot (\s ->
                         let (s', y)   = f s x
                             (s'', ys) = r s'
                         in (s'', y:ys))
  -- See Note [Left folds via right fold]


-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a list, passing
-- an accumulating parameter from right to left, and returning a final
-- value of this accumulator together with the new list.
mapAccumR :: (acc -> x -> (acc, y))     -- Function of elt of input list
                                        -- and accumulator, returning new
                                        -- accumulator and elt of result list
            -> acc              -- Initial accumulator
            -> [x]              -- Input list
            -> (acc, [y])               -- Final accumulator and result list
mapAccumR _ s []        =  (s, [])
mapAccumR f s (x:xs)    =  (s'', y:ys)
                           where (s'',y ) = f s' x
                                 (s', ys) = mapAccumR f s xs

-- | \(\mathcal{O}(n)\). The 'insert' function takes an element and a list and
-- inserts the element into the list at the first position where it is less than
-- or equal to the next element. In particular, if the list is sorted before the
-- call, the result will also be sorted. It is a special case of 'insertBy',
-- which allows the programmer to supply their own comparison function.
--
-- >>> insert 4 [1,2,3,5,6,7]
-- [1,2,3,4,5,6,7]
insert :: Ord a => a -> [a] -> [a]
insert e ls = insertBy (compare) e ls

-- | \(\mathcal{O}(n)\). The non-overloaded version of 'insert'.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _   x [] = [x]
insertBy cmp x ys@(y:ys')
 = case cmp x y of
     GT -> y : insertBy cmp x ys'
     _  -> x : ys

-- | The 'maximumBy' function takes a comparison function and a list
-- and returns the greatest element of the list by the comparison function.
-- The list must be finite and non-empty.
--
-- We can use this to find the longest entry of a list:
--
-- >>> maximumBy (\x y -> compare (length x) (length y)) ["Hello", "World", "!", "Longest", "bar"]
-- "Longest"
maximumBy               :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ []          =  errorWithoutStackTrace "List.maximumBy: empty list"
maximumBy cmp xs        =  foldl1 maxBy xs
                        where
                           maxBy x y = case cmp x y of
                                       GT -> x
                                       _  -> y

-- | The 'minimumBy' function takes a comparison function and a list
-- and returns the least element of the list by the comparison function.
-- The list must be finite and non-empty.
--
-- We can use this to find the shortest entry of a list:
--
-- >>> minimumBy (\x y -> compare (length x) (length y)) ["Hello", "World", "!", "Longest", "bar"]
-- "!"
minimumBy               :: (a -> a -> Ordering) -> [a] -> a
minimumBy _ []          =  errorWithoutStackTrace "List.minimumBy: empty list"
minimumBy cmp xs        =  foldl1 minBy xs
                        where
                           minBy x y = case cmp x y of
                                       GT -> y
                                       _  -> x

-- | \(\mathcal{O}(n)\). The 'genericLength' function is an overloaded version
-- of 'length'. In particular, instead of returning an 'Int', it returns any
-- type which is an instance of 'Num'. It is, however, less efficient than
-- 'length'.
--
-- >>> genericLength [1, 2, 3] :: Int
-- 3
-- >>> genericLength [1, 2, 3] :: Float
-- 3.0
--
-- Users should take care to pick a return type that is wide enough to contain
-- the full length of the list. If the width is insufficient, the overflow
-- behaviour will depend on the @(+)@ implementation in the selected 'Num'
-- instance. The following example overflows because the actual list length
-- of 200 lies outside of the 'Int8' range of @-128..127@.
--
-- >>> genericLength [1..200] :: Int8
-- -56
genericLength           :: (Num i) => [a] -> i
{-# NOINLINE [2] genericLength #-}
    -- Give time for the RULEs for (++) to fire in InitialPhase
    -- It's recursive, so won't inline anyway,
    -- but saying so is more explicit
genericLength []        =  0
genericLength (_:l)     =  1 + genericLength l

{-# RULES
  "genericLengthInt"     genericLength = (strictGenericLength :: [a] -> Int);
  "genericLengthInteger" genericLength = (strictGenericLength :: [a] -> Integer);
 #-}

strictGenericLength     :: (Num i) => [b] -> i
strictGenericLength l   =  gl l 0
                        where
                           gl [] a     = a
                           gl (_:xs) a = let a' = a + 1 in a' `seq` gl xs a'

-- | The 'genericTake' function is an overloaded version of 'take', which
-- accepts any 'Integral' value as the number of elements to take.
genericTake             :: (Integral i) => i -> [a] -> [a]
genericTake n _ | n <= 0 = []
genericTake _ []        =  []
genericTake n (x:xs)    =  x : genericTake (n-1) xs

-- | The 'genericDrop' function is an overloaded version of 'drop', which
-- accepts any 'Integral' value as the number of elements to drop.
genericDrop             :: (Integral i) => i -> [a] -> [a]
genericDrop n xs | n <= 0 = xs
genericDrop _ []        =  []
genericDrop n (_:xs)    =  genericDrop (n-1) xs


-- | The 'genericSplitAt' function is an overloaded version of 'splitAt', which
-- accepts any 'Integral' value as the position at which to split.
genericSplitAt          :: (Integral i) => i -> [a] -> ([a], [a])
genericSplitAt n xs | n <= 0 =  ([],xs)
genericSplitAt _ []     =  ([],[])
genericSplitAt n (x:xs) =  (x:xs',xs'') where
    (xs',xs'') = genericSplitAt (n-1) xs

-- | The 'genericIndex' function is an overloaded version of '!!', which
-- accepts any 'Integral' value as the index.
genericIndex :: (Integral i) => [a] -> i -> a
genericIndex (x:_)  0 = x
genericIndex (_:xs) n
 | n > 0     = genericIndex xs (n-1)
 | otherwise = errorWithoutStackTrace "List.genericIndex: negative argument."
genericIndex _ _      = errorWithoutStackTrace "List.genericIndex: index too large."

-- | The 'genericReplicate' function is an overloaded version of 'replicate',
-- which accepts any 'Integral' value as the number of repetitions to make.
genericReplicate        :: (Integral i) => i -> a -> [a]
genericReplicate n x    =  genericTake n (repeat x)

-- | The 'zip4' function takes four lists and returns a list of
-- quadruples, analogous to 'zip'.
-- It is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
{-# INLINE zip4 #-}
zip4                    :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4                    =  zipWith4 (,,,)

-- | The 'zip5' function takes five lists and returns a list of
-- five-tuples, analogous to 'zip'.
-- It is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
{-# INLINE zip5 #-}
zip5                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5                    =  zipWith5 (,,,,)

-- | The 'zip6' function takes six lists and returns a list of six-tuples,
-- analogous to 'zip'.
-- It is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
{-# INLINE zip6 #-}
zip6                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
                              [(a,b,c,d,e,f)]
zip6                    =  zipWith6 (,,,,,)

-- | The 'zip7' function takes seven lists and returns a list of
-- seven-tuples, analogous to 'zip'.
-- It is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
{-# INLINE zip7 #-}
zip7                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
                              [g] -> [(a,b,c,d,e,f,g)]
zip7                    =  zipWith7 (,,,,,,)

-- | The 'zipWith4' function takes a function which combines four
-- elements, as well as four lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
-- It is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
{-# NOINLINE [1] zipWith4 #-}
zipWith4                :: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
                        =  z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _      =  []

-- | The 'zipWith5' function takes a function which combines five
-- elements, as well as five lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
-- It is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
{-# NOINLINE [1] zipWith5 #-}
zipWith5                :: (a->b->c->d->e->f) ->
                           [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
                        =  z a b c d e : zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _    = []

-- | The 'zipWith6' function takes a function which combines six
-- elements, as well as six lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
-- It is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
{-# NOINLINE [1] zipWith6 #-}
zipWith6                :: (a->b->c->d->e->f->g) ->
                           [a]->[b]->[c]->[d]->[e]->[f]->[g]
zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
                        =  z a b c d e f : zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _  = []

-- | The 'zipWith7' function takes a function which combines seven
-- elements, as well as seven lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
-- It is capable of list fusion, but it is restricted to its
-- first list argument and its resulting list.
{-# NOINLINE [1] zipWith7 #-}
zipWith7                :: (a->b->c->d->e->f->g->h) ->
                           [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
                   =  z a b c d e f g : zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ = []

{-
Functions and rules for fusion of zipWith4, zipWith5, zipWith6 and zipWith7.
The principle is the same as for zip and zipWith in GHC.List:
Turn zipWithX into a version in which the first argument and the result
can be fused. Turn it back into the original function if no fusion happens.
-}

{-# INLINE [0] zipWith4FB #-} -- See Note [Inline FB functions]
zipWith4FB :: (e->xs->xs') -> (a->b->c->d->e) ->
              a->b->c->d->xs->xs'
zipWith4FB cons func = \a b c d r -> (func a b c d) `cons` r

{-# INLINE [0] zipWith5FB #-} -- See Note [Inline FB functions]
zipWith5FB :: (f->xs->xs') -> (a->b->c->d->e->f) ->
              a->b->c->d->e->xs->xs'
zipWith5FB cons func = \a b c d e r -> (func a b c d e) `cons` r

{-# INLINE [0] zipWith6FB #-} -- See Note [Inline FB functions]
zipWith6FB :: (g->xs->xs') -> (a->b->c->d->e->f->g) ->
              a->b->c->d->e->f->xs->xs'
zipWith6FB cons func = \a b c d e f r -> (func a b c d e f) `cons` r

{-# INLINE [0] zipWith7FB #-} -- See Note [Inline FB functions]
zipWith7FB :: (h->xs->xs') -> (a->b->c->d->e->f->g->h) ->
              a->b->c->d->e->f->g->xs->xs'
zipWith7FB cons func = \a b c d e f g r -> (func a b c d e f g) `cons` r

{-# INLINE [0] foldr4 #-}
foldr4 :: (a->b->c->d->e->e) ->
          e->[a]->[b]->[c]->[d]->e
foldr4 k z = go
  where
    go (a:as) (b:bs) (c:cs) (d:ds) = k a b c d (go as bs cs ds)
    go _      _      _      _      = z

{-# INLINE [0] foldr5 #-}
foldr5 :: (a->b->c->d->e->f->f) ->
          f->[a]->[b]->[c]->[d]->[e]->f
foldr5 k z = go
  where
    go (a:as) (b:bs) (c:cs) (d:ds) (e:es) = k a b c d e (go as bs cs ds es)
    go _      _      _      _      _      = z

{-# INLINE [0] foldr6 #-}
foldr6 :: (a->b->c->d->e->f->g->g) ->
          g->[a]->[b]->[c]->[d]->[e]->[f]->g
foldr6 k z = go
  where
    go (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) = k a b c d e f (
        go as bs cs ds es fs)
    go _      _      _      _      _      _      = z

{-# INLINE [0] foldr7 #-}
foldr7 :: (a->b->c->d->e->f->g->h->h) ->
          h->[a]->[b]->[c]->[d]->[e]->[f]->[g]->h
foldr7 k z = go
  where
    go (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) = k a b c d e f g (
        go as bs cs ds es fs gs)
    go _      _      _      _      _      _      _      = z

foldr4_left :: (a->b->c->d->e->f)->
               f->a->([b]->[c]->[d]->e)->
               [b]->[c]->[d]->f
foldr4_left k _z a r (b:bs) (c:cs) (d:ds) = k a b c d (r bs cs ds)
foldr4_left _  z _ _ _      _      _      = z

foldr5_left :: (a->b->c->d->e->f->g)->
               g->a->([b]->[c]->[d]->[e]->f)->
               [b]->[c]->[d]->[e]->g
foldr5_left k _z a r (b:bs) (c:cs) (d:ds) (e:es) = k a b c d e (r bs cs ds es)
foldr5_left _  z _ _ _      _      _      _      = z

foldr6_left :: (a->b->c->d->e->f->g->h)->
               h->a->([b]->[c]->[d]->[e]->[f]->g)->
               [b]->[c]->[d]->[e]->[f]->h
foldr6_left k _z a r (b:bs) (c:cs) (d:ds) (e:es) (f:fs) =
    k a b c d e f (r bs cs ds es fs)
foldr6_left _  z _ _ _      _      _      _      _      = z

foldr7_left :: (a->b->c->d->e->f->g->h->i)->
               i->a->([b]->[c]->[d]->[e]->[f]->[g]->h)->
               [b]->[c]->[d]->[e]->[f]->[g]->i
foldr7_left k _z a r (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) =
    k a b c d e f g (r bs cs ds es fs gs)
foldr7_left _  z _ _ _      _      _      _      _      _      = z

{-# RULES

"foldr4/left"   forall k z (g::forall b.(a->b->b)->b->b).
                  foldr4 k z (build g) = g (foldr4_left k z) (\_ _ _ -> z)
"foldr5/left"   forall k z (g::forall b.(a->b->b)->b->b).
                  foldr5 k z (build g) = g (foldr5_left k z) (\_ _ _ _ -> z)
"foldr6/left"   forall k z (g::forall b.(a->b->b)->b->b).
                  foldr6 k z (build g) = g (foldr6_left k z) (\_ _ _ _ _ -> z)
"foldr7/left"   forall k z (g::forall b.(a->b->b)->b->b).
                  foldr7 k z (build g) = g (foldr7_left k z) (\_ _ _ _ _ _ -> z)

"zipWith4" [~1] forall f as bs cs ds.
                  zipWith4 f as bs cs ds = build (\c n ->
                        foldr4 (zipWith4FB c f) n as bs cs ds)
"zipWith5" [~1] forall f as bs cs ds es.
                  zipWith5 f as bs cs ds es = build (\c n ->
                        foldr5 (zipWith5FB c f) n as bs cs ds es)
"zipWith6" [~1] forall f as bs cs ds es fs.
                  zipWith6 f as bs cs ds es fs = build (\c n ->
                        foldr6 (zipWith6FB c f) n as bs cs ds es fs)
"zipWith7" [~1] forall f as bs cs ds es fs gs.
                  zipWith7 f as bs cs ds es fs gs = build (\c n ->
                        foldr7 (zipWith7FB c f) n as bs cs ds es fs gs)

"zipWith4List"  [1]  forall f.   foldr4 (zipWith4FB (:) f) [] = zipWith4 f
"zipWith5List"  [1]  forall f.   foldr5 (zipWith5FB (:) f) [] = zipWith5 f
"zipWith6List"  [1]  forall f.   foldr6 (zipWith6FB (:) f) [] = zipWith6 f
"zipWith7List"  [1]  forall f.   foldr7 (zipWith7FB (:) f) [] = zipWith7 f

 #-}

{-

Note [Inline @unzipN@ functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The inline principle for @unzip{4,5,6,7}@ is the same as 'unzip'/'unzip3' in
"GHC.List".
The 'unzip'/'unzip3' functions are inlined so that the `foldr` with which they
are defined has an opportunity to fuse.

As such, since there are not any differences between 2/3-ary 'unzip' and its
n-ary counterparts below aside from the number of arguments, the `INLINE`
pragma should be replicated in the @unzipN@ functions below as well.

-}

-- | The 'unzip4' function takes a list of quadruples and returns four
-- lists, analogous to 'unzip'.
{-# INLINE unzip4 #-}
-- Inline so that fusion with `foldr` has an opportunity to fire.
-- See Note [Inline @unzipN@ functions] above.
unzip4                  :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4                  =  foldr (\(a,b,c,d) ~(as,bs,cs,ds) ->
                                        (a:as,b:bs,c:cs,d:ds))
                                 ([],[],[],[])

-- | The 'unzip5' function takes a list of five-tuples and returns five
-- lists, analogous to 'unzip'.
{-# INLINE unzip5 #-}
-- Inline so that fusion with `foldr` has an opportunity to fire.
-- See Note [Inline @unzipN@ functions] above.
unzip5                  :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5                  =  foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) ->
                                        (a:as,b:bs,c:cs,d:ds,e:es))
                                 ([],[],[],[],[])

-- | The 'unzip6' function takes a list of six-tuples and returns six
-- lists, analogous to 'unzip'.
{-# INLINE unzip6 #-}
-- Inline so that fusion with `foldr` has an opportunity to fire.
-- See Note [Inline @unzipN@ functions] above.
unzip6                  :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip6                  =  foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) ->
                                        (a:as,b:bs,c:cs,d:ds,e:es,f:fs))
                                 ([],[],[],[],[],[])

-- | The 'unzip7' function takes a list of seven-tuples and returns
-- seven lists, analogous to 'unzip'.
{-# INLINE unzip7 #-}
-- Inline so that fusion with `foldr` has an opportunity to fire.
-- See Note [Inline @unzipN@ functions] above.
unzip7          :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
unzip7          =  foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
                                (a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
                         ([],[],[],[],[],[],[])


-- | The 'deleteFirstsBy' function takes a predicate and two lists and
-- returns the first list with the first occurrence of each element of
-- the second list removed.
deleteFirstsBy          :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))

-- | The 'group' function takes a list and returns a list of lists such
-- that the concatenation of the result is equal to the argument.  Moreover,
-- each sublist in the result contains only equal elements.  For example,
--
-- >>> group "Mississippi"
-- ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to supply
-- their own equality test.
group                   :: Eq a => [a] -> [[a]]
group                   =  groupBy (==)

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []           =  []
groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                           where (ys,zs) = span (eq x) xs

-- | The 'inits' function returns all initial segments of the argument,
-- shortest first.  For example,
--
-- >>> inits "abc"
-- ["","a","ab","abc"]
--
-- Note that 'inits' has the following strictness property:
-- @inits (xs ++ _|_) = inits xs ++ _|_@
--
-- In particular,
-- @inits _|_ = [] : _|_@
inits                   :: [a] -> [[a]]
inits                   = map toListSB . scanl' snocSB emptySB
{-# NOINLINE inits #-}

-- We do not allow inits to inline, because it plays havoc with Call Arity
-- if it fuses with a consumer, and it would generally lead to serious
-- loss of sharing if allowed to fuse with a producer.

-- | \(\mathcal{O}(n)\). The 'tails' function returns all final segments of the
-- argument, longest first. For example,
--
-- >>> tails "abc"
-- ["abc","bc","c",""]
--
-- Note that 'tails' has the following strictness property:
-- @tails _|_ = _|_ : _|_@
tails                   :: [a] -> [[a]]
{-# INLINABLE tails #-}
tails lst               =  build (\c n ->
  let tailsGo xs = xs `c` case xs of
                             []      -> n
                             _ : xs' -> tailsGo xs'
  in tailsGo lst)

-- | The 'subsequences' function returns the list of all subsequences of the argument.
--
-- >>> subsequences "abc"
-- ["","a","b","ab","c","ac","bc","abc"]
subsequences            :: [a] -> [[a]]
subsequences xs         =  [] : nonEmptySubsequences xs

-- | The 'nonEmptySubsequences' function returns the list of all subsequences of the argument,
--   except for the empty list.
--
-- >>> nonEmptySubsequences "abc"
-- ["a","b","ab","c","ac","bc","abc"]
nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r


-- | The 'permutations' function returns the list of all permutations of the argument.
--
-- >>> permutations "abc"
-- ["abc","bac","cba","bca","cab","acb"]
permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)


------------------------------------------------------------------------------
-- Quick Sort algorithm taken from HBC's QSort library.

-- | The 'sort' function implements a stable sorting algorithm.
-- It is a special case of 'sortBy', which allows the programmer to supply
-- their own comparison function.
--
-- Elements are arranged from lowest to highest, keeping duplicates in
-- the order they appeared in the input.
--
-- >>> sort [1,6,4,3,2,5]
-- [1,2,3,4,5,6]
sort :: (Ord a) => [a] -> [a]

-- | The 'sortBy' function is the non-overloaded version of 'sort'.
--
-- >>> sortBy (\(a,_) (b,_) -> compare a b) [(2, "world"), (4, "!"), (1, "Hello")]
-- [(1,"Hello"),(2,"world"),(4,"!")]
sortBy :: (a -> a -> Ordering) -> [a] -> [a]

#if defined(USE_REPORT_PRELUDE)
sort = sortBy compare
sortBy cmp = foldr (insertBy cmp) []
#else

{-
GHC's mergesort replaced by a better implementation, 24/12/2009.
This code originally contributed to the nhc12 compiler by Thomas Nordin
in 2002.  Rumoured to have been based on code by Lennart Augustsson, e.g.
    http://www.mail-archive.com/haskell@haskell.org/msg01822.html
and possibly to bear similarities to a 1982 paper by Richard O'Keefe:
"A smooth applicative merge sort".

Benchmarks show it to be often 2x the speed of the previous implementation.
Fixes ticket https://gitlab.haskell.org/ghc/ghc/issues/2143
-}

sort = sortBy compare
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = let !x = as [a]
                          in x : sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as

{-
sortBy cmp l = mergesort cmp l
sort l = mergesort compare l

Quicksort replaced by mergesort, 14/5/2002.

From: Ian Lynagh <igloo@earth.li>

I am curious as to why the List.sort implementation in GHC is a
quicksort algorithm rather than an algorithm that guarantees n log n
time in the worst case? I have attached a mergesort implementation along
with a few scripts to time it's performance, the results of which are
shown below (* means it didn't finish successfully - in all cases this
was due to a stack overflow).

If I heap profile the random_list case with only 10000 then I see
random_list peaks at using about 2.5M of memory, whereas in the same
program using List.sort it uses only 100k.

Input style     Input length     Sort data     Sort alg    User time
stdin           10000            random_list   sort        2.82
stdin           10000            random_list   mergesort   2.96
stdin           10000            sorted        sort        31.37
stdin           10000            sorted        mergesort   1.90
stdin           10000            revsorted     sort        31.21
stdin           10000            revsorted     mergesort   1.88
stdin           100000           random_list   sort        *
stdin           100000           random_list   mergesort   *
stdin           100000           sorted        sort        *
stdin           100000           sorted        mergesort   *
stdin           100000           revsorted     sort        *
stdin           100000           revsorted     mergesort   *
func            10000            random_list   sort        0.31
func            10000            random_list   mergesort   0.91
func            10000            sorted        sort        19.09
func            10000            sorted        mergesort   0.15
func            10000            revsorted     sort        19.17
func            10000            revsorted     mergesort   0.16
func            100000           random_list   sort        3.85
func            100000           random_list   mergesort   *
func            100000           sorted        sort        5831.47
func            100000           sorted        mergesort   2.23
func            100000           revsorted     sort        5872.34
func            100000           revsorted     mergesort   2.24

mergesort :: (a -> a -> Ordering) -> [a] -> [a]
mergesort cmp = mergesort' cmp . map wrap

mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
mergesort' _   [] = []
mergesort' _   [xs] = xs
mergesort' cmp xss = mergesort' cmp (merge_pairs cmp xss)

merge_pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
merge_pairs _   [] = []
merge_pairs _   [xs] = [xs]
merge_pairs cmp (xs:ys:xss) = merge cmp xs ys : merge_pairs cmp xss

merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge _   [] ys = ys
merge _   xs [] = xs
merge cmp (x:xs) (y:ys)
 = case x `cmp` y of
        GT -> y : merge cmp (x:xs)   ys
        _  -> x : merge cmp    xs (y:ys)

wrap :: a -> [a]
wrap x = [x]



OLDER: qsort version

-- qsort is stable and does not concatenate.
qsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
qsort _   []     r = r
qsort _   [x]    r = x:r
qsort cmp (x:xs) r = qpart cmp x xs [] [] r

-- qpart partitions and sorts the sublists
qpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
qpart cmp x [] rlt rge r =
    -- rlt and rge are in reverse order and must be sorted with an
    -- anti-stable sorting
    rqsort cmp rlt (x:rqsort cmp rge r)
qpart cmp x (y:ys) rlt rge r =
    case cmp x y of
        GT -> qpart cmp x ys (y:rlt) rge r
        _  -> qpart cmp x ys rlt (y:rge) r

-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
rqsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
rqsort _   []     r = r
rqsort _   [x]    r = x:r
rqsort cmp (x:xs) r = rqpart cmp x xs [] [] r

rqpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
rqpart cmp x [] rle rgt r =
    qsort cmp rle (x:qsort cmp rgt r)
rqpart cmp x (y:ys) rle rgt r =
    case cmp y x of
        GT -> rqpart cmp x ys rle (y:rgt) r
        _  -> rqpart cmp x ys (y:rle) rgt r
-}

#endif /* USE_REPORT_PRELUDE */

-- | Sort a list by comparing the results of a key function applied to each
-- element.  @sortOn f@ is equivalent to @sortBy (comparing f)@, but has the
-- performance advantage of only evaluating @f@ once for each element in the
-- input list.  This is called the decorate-sort-undecorate paradigm, or
-- Schwartzian transform.
--
-- Elements are arranged from lowest to highest, keeping duplicates in
-- the order they appeared in the input.
--
-- >>> sortOn fst [(2, "world"), (4, "!"), (1, "Hello")]
-- [(1,"Hello"),(2,"world"),(4,"!")]
--
-- @since 4.8.0.0
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

-- | Produce singleton list.
--
-- >>> singleton True
-- [True]
--
-- @since 4.15.0.0
--
singleton :: a -> [a]
singleton x = [x]

-- | The 'unfoldr' function is a \`dual\' to 'foldr': while 'foldr'
-- reduces a list to a summary value, 'unfoldr' builds a list from
-- a seed value.  The function takes the element and returns 'Nothing'
-- if it is done producing the list or returns 'Just' @(a,b)@, in which
-- case, @a@ is a prepended to the list and @b@ is used as the next
-- element in a recursive call.  For example,
--
-- > iterate f == unfoldr (\x -> Just (x, f x))
--
-- In some cases, 'unfoldr' can undo a 'foldr' operation:
--
-- > unfoldr f' (foldr f z xs) == xs
--
-- if the following holds:
--
-- > f' (f x y) = Just (x,y)
-- > f' z       = Nothing
--
-- A simple use of unfoldr:
--
-- >>> unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
-- [10,9,8,7,6,5,4,3,2,1]
--

-- Note [INLINE unfoldr]
-- ~~~~~~~~~~~~~~~~~~~~~
-- We treat unfoldr a little differently from some other forms for list fusion
-- for two reasons:
--
-- 1. We don't want to use a rule to rewrite a basic form to a fusible
-- form because this would inline before constant floating. As Simon Peyton-
-- Jones and others have pointed out, this could reduce sharing in some cases
-- where sharing is beneficial. Thus we simply INLINE it, which is, for
-- example, how enumFromTo::Int becomes eftInt. Unfortunately, we don't seem
-- to get enough of an inlining discount to get a version of eftInt based on
-- unfoldr to inline as readily as the usual one. We know that all the Maybe
-- nonsense will go away, but the compiler does not.
--
-- 2. The benefit of inlining unfoldr is likely to be huge in many common cases,
-- even apart from list fusion. In particular, inlining unfoldr often
-- allows GHC to erase all the Maybes. This appears to be critical if unfoldr
-- is to be used in high-performance code. A small increase in code size
-- in the relatively rare cases when this does not happen looks like a very
-- small price to pay.
--
-- Doing a back-and-forth dance doesn't seem to accomplish anything if the
-- final form has to be inlined in any case.

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

{-# INLINE unfoldr #-} -- See Note [INLINE unfoldr]
unfoldr f b0 = build (\c n ->
  let go b = case f b of
               Just (a, new_b) -> a `c` go new_b
               Nothing         -> n
  in go b0)

-- -----------------------------------------------------------------------------
-- Functions on strings

-- | Splits the argument into a list of /lines/ stripped of their terminating
-- @\n@ characters.  The @\n@ terminator is optional in a final non-empty
-- line of the argument string.
--
-- For example:
--
-- >>> lines ""           -- empty input contains no lines
-- []
--
-- >>> lines "\n"         -- single empty line
-- [""]
--
-- >>> lines "one"        -- single unterminated line
-- ["one"]
--
-- >>> lines "one\n"      -- single non-empty line
-- ["one"]
--
-- >>> lines "one\n\n"    -- second line is empty
-- ["one",""]
--
-- >>> lines "one\ntwo"   -- second line is unterminated
-- ["one","two"]
--
-- >>> lines "one\ntwo\n" -- two non-empty lines
-- ["one","two"]
--
-- When the argument string is empty, or ends in a @\n@ character, it can be
-- recovered by passing the result of 'lines' to the 'unlines' function.
-- Otherwise, 'unlines' appends the missing terminating @\n@.  This makes
-- @unlines . lines@ /idempotent/:
--
-- > (unlines . lines) . (unlines . lines) = (unlines . lines)
--
lines                   :: String -> [String]
lines ""                =  []
-- Somehow GHC doesn't detect the selector thunks in the below code,
-- so s' keeps a reference to the first line via the pair and we have
-- a space leak (cf. #4334).
-- So we need to make GHC see the selector thunks with a trick.
lines s                 =  cons (case break (== '\n') s of
                                    (l, s') -> (l, case s' of
                                                    []      -> []
                                                    _:s''   -> lines s''))
  where
    cons ~(h, t)        =  h : t

-- | Appends a @\n@ character to each input string, then concatenates the
-- results. Equivalent to @'foldMap' (\s -> s '++' "\n")@.
--
-- >>> unlines ["Hello", "World", "!"]
-- "Hello\nWorld\n!\n"
--
-- = Note
--
-- @'unlines' '.' 'lines' '/=' 'id'@ when the input is not @\n@-terminated:
--
-- >>> unlines . lines $ "foo\nbar"
-- "foo\nbar\n"
unlines                 :: [String] -> String
#if defined(USE_REPORT_PRELUDE)
unlines                 =  concatMap (++ "\n")
#else
-- HBC version (stolen)
-- here's a more efficient version
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls
#endif

-- | 'words' breaks a string up into a list of words, which were delimited
-- by white space.
--
-- >>> words "Lorem ipsum\ndolor"
-- ["Lorem","ipsum","dolor"]
words                   :: String -> [String]
{-# NOINLINE [1] words #-}
words s                 =  case dropWhile {-partain:Char.-}isSpace s of
                                "" -> []
                                s' -> w : words s''
                                      where (w, s'') =
                                             break {-partain:Char.-}isSpace s'

{-# RULES
"words" [~1] forall s . words s = build (\c n -> wordsFB c n s)
"wordsList" [1] wordsFB (:) [] = words
 #-}
wordsFB :: ([Char] -> b -> b) -> b -> String -> b
{-# INLINE [0] wordsFB #-} -- See Note [Inline FB functions] in GHC.List
wordsFB c n = go
  where
    go s = case dropWhile isSpace s of
             "" -> n
             s' -> w `c` go s''
                   where (w, s'') = break isSpace s'

-- | 'unwords' is an inverse operation to 'words'.
-- It joins words with separating spaces.
--
-- >>> unwords ["Lorem", "ipsum", "dolor"]
-- "Lorem ipsum dolor"
unwords                 :: [String] -> String
#if defined(USE_REPORT_PRELUDE)
unwords []              =  ""
unwords ws              =  foldr1 (\w s -> w ++ ' ':s) ws
#else
-- Here's a lazier version that can get the last element of a
-- _|_-terminated list.
{-# NOINLINE [1] unwords #-}
unwords []              =  ""
unwords (w:ws)          = w ++ go ws
  where
    go []     = ""
    go (v:vs) = ' ' : (v ++ go vs)

-- In general, the foldr-based version is probably slightly worse
-- than the HBC version, because it adds an extra space and then takes
-- it back off again. But when it fuses, it reduces allocation. How much
-- depends entirely on the average word length--it's most effective when
-- the words are on the short side.
{-# RULES
"unwords" [~1] forall ws .
   unwords ws = tailUnwords (foldr unwordsFB "" ws)
"unwordsList" [1] forall ws .
   tailUnwords (foldr unwordsFB "" ws) = unwords ws
 #-}

{-# INLINE [0] tailUnwords #-}
tailUnwords           :: String -> String
tailUnwords []        = []
tailUnwords (_:xs)    = xs

{-# INLINE [0] unwordsFB #-}
unwordsFB               :: String -> String -> String
unwordsFB w r           = ' ' : w ++ r
#endif

{- A "SnocBuilder" is a version of Chris Okasaki's banker's queue that supports
toListSB instead of uncons. In single-threaded use, its performance
characteristics are similar to John Hughes's functional difference lists, but
likely somewhat worse. In heavily persistent settings, however, it does much
better, because it takes advantage of sharing. The banker's queue guarantees
(amortized) O(1) snoc and O(1) uncons, meaning that we can think of toListSB as
an O(1) conversion to a list-like structure a constant factor slower than
normal lists--we pay the O(n) cost incrementally as we consume the list. Using
functional difference lists, on the other hand, we would have to pay the whole
cost up front for each output list. -}

{- We store a front list, a rear list, and the length of the queue.  Because we
only snoc onto the queue and never uncons, we know it's time to rotate when the
length of the queue plus 1 is a power of 2. Note that we rely on the value of
the length field only for performance.  In the unlikely event of overflow, the
performance will suffer but the semantics will remain correct.  -}

data SnocBuilder a = SnocBuilder {-# UNPACK #-} !Word [a] [a]

{- Smart constructor that rotates the builder when lp is one minus a power of
2. Does not rotate very small builders because doing so is not worth the
trouble. The lp < 255 test goes first because the power-of-2 test gives awful
branch prediction for very small n (there are 5 powers of 2 between 1 and
16). Putting the well-predicted lp < 255 test first avoids branching on the
power-of-2 test until powers of 2 have become sufficiently rare to be predicted
well. -}

{-# INLINE sb #-}
sb :: Word -> [a] -> [a] -> SnocBuilder a
sb lp f r
  | lp < 255 || (lp .&. (lp + 1)) /= 0 = SnocBuilder lp f r
  | otherwise                          = SnocBuilder lp (f ++ reverse r) []

-- The empty builder

emptySB :: SnocBuilder a
emptySB = SnocBuilder 0 [] []

-- Add an element to the end of a queue.

snocSB :: SnocBuilder a -> a -> SnocBuilder a
snocSB (SnocBuilder lp f r) x = sb (lp + 1) f (x:r)

-- Convert a builder to a list

toListSB :: SnocBuilder a -> [a]
toListSB (SnocBuilder _ f r) = f ++ reverse r
