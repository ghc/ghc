{-# LANGUAGE Trustworthy #-} -- can't use Safe due to IsList instance
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.NonEmpty
-- Copyright   :  (C) 2011-2015 Edward Kmett,
--                (C) 2010 Tony Morris, Oliver Taylor, Eelis van der Weegen
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'NonEmpty' list is one which always has at least one element, but
-- is otherwise identical to the traditional list type in complexity
-- and in terms of API. You will almost certainly want to import this
-- module @qualified@.
--
-- @since 4.9.0.0
----------------------------------------------------------------------------

-- Function implementations in this module adhere to the following principle:
--
-- For every NonEmpty function that is different from a corresponding
-- List function only in the presence of NonEmpty in its type, both
-- the List and NonEmpty functions should have the same strictness
-- properties. Same applies to the class instances.

module Data.List.NonEmpty (
   -- * The type of non-empty streams
     NonEmpty(..)

   -- * Non-empty stream transformations
   , map         -- :: (a -> b) -> NonEmpty a -> NonEmpty b
   , intersperse -- :: a -> NonEmpty a -> NonEmpty a
   , scanl       -- :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b
   , scanr       -- :: Foldable f => (a -> b -> b) -> b -> f a -> NonEmpty b
   , scanl1      -- :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
   , scanr1      -- :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
   , transpose   -- :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
   , sortBy      -- :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
   , sortWith      -- :: Ord o => (a -> o) -> NonEmpty a -> NonEmpty a
   -- * Basic functions
   , length      -- :: NonEmpty a -> Int
   , compareLength -- :: NonEmpty a -> Int -> Ordering
   , head        -- :: NonEmpty a -> a
   , tail        -- :: NonEmpty a -> [a]
   , last        -- :: NonEmpty a -> a
   , init        -- :: NonEmpty a -> [a]
   , singleton   -- :: a -> NonEmpty a
   , (<|), cons  -- :: a -> NonEmpty a -> NonEmpty a
   , uncons      -- :: NonEmpty a -> (a, Maybe (NonEmpty a))
   , unfoldr     -- :: (a -> (b, Maybe a)) -> a -> NonEmpty b
   , sort        -- :: Ord a => NonEmpty a -> NonEmpty a
   , sortOn      -- :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
   , reverse     -- :: NonEmpty a -> NonEmpty a
   , inits       -- :: Foldable f => f a -> NonEmpty [a]
   , inits1      -- :: NonEmpty a -> NonEmpty (NonEmpty a)
   , tails       -- :: Foldable f => f a -> NonEmpty [a]
   , tails1      -- :: NonEmpty a -> NonEmpty (NonEmpty a)
   , append      -- :: NonEmpty a -> NonEmpty a -> NonEmpty a
   , appendList  -- :: NonEmpty a -> [a] -> NonEmpty a
   , prependList -- :: [a] -> NonEmpty a -> NonEmpty a
   -- * Building streams
   , iterate     -- :: (a -> a) -> a -> NonEmpty a
   , repeat      -- :: a -> NonEmpty a
   , cycle       -- :: NonEmpty a -> NonEmpty a
   , unfold      -- :: (a -> (b, Maybe a)) -> a -> NonEmpty b
   , insert      -- :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
   , some1       -- :: Alternative f => f a -> f (NonEmpty a)
   -- * Extracting sublists
   , take        -- :: Int -> NonEmpty a -> [a]
   , drop        -- :: Int -> NonEmpty a -> [a]
   , splitAt     -- :: Int -> NonEmpty a -> ([a], [a])
   , takeWhile   -- :: (a -> Bool) -> NonEmpty a -> [a]
   , dropWhile   -- :: (a -> Bool) -> NonEmpty a -> [a]
   , span        -- :: (a -> Bool) -> NonEmpty a -> ([a], [a])
   , break       -- :: (a -> Bool) -> NonEmpty a -> ([a], [a])
   , filter      -- :: (a -> Bool) -> NonEmpty a -> [a]
   , partition   -- :: (a -> Bool) -> NonEmpty a -> ([a],[a])
   , group       -- :: (Foldable f, Eq a) => f a -> [NonEmpty a]
   , groupBy     -- :: Foldable f => (a -> a -> Bool) -> f a -> [NonEmpty a]
   , groupWith     -- :: (Foldable f, Eq b) => (a -> b) -> f a -> [NonEmpty a]
   , groupAllWith  -- :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
   , group1      -- :: Eq a => NonEmpty a -> NonEmpty (NonEmpty a)
   , groupBy1    -- :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
   , groupWith1     -- :: Eq b => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
   , groupAllWith1  -- :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
   , permutations   -- :: [a] -> NonEmpty [a]
   , permutations1  -- :: NonEmpty a -> NonEmpty (NonEmpty a)
   -- * Sublist predicates
   , isPrefixOf  -- :: Eq a => [a] -> NonEmpty a -> Bool
   -- * \"Set\" operations
   , nub         -- :: Eq a => NonEmpty a -> NonEmpty a
   , nubBy       -- :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a
   -- * Indexing streams
   , (!!)        -- :: NonEmpty a -> Int -> a
   -- * Zipping and unzipping streams
   , zip         -- :: NonEmpty a -> NonEmpty b -> NonEmpty (a,b)
   , zipWith     -- :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
   , unzip       -- :: Functor f => f (a,b) -> (f a, f b)
   -- * Converting to and from a list
   , fromList    -- :: [a] -> NonEmpty a
   , toList      -- :: NonEmpty a -> [a]
   , nonEmpty    -- :: [a] -> Maybe (NonEmpty a)
   , xor         -- :: NonEmpty Bool -> Bool
   ) where


import           Prelude             hiding (break, cycle, drop, dropWhile,
                                      filter, foldl, foldr, head, init, iterate,
                                      last, length, map, repeat, reverse,
                                      scanl, scanl1, scanr, scanr1, span,
                                      splitAt, tail, take, takeWhile,
                                      unzip, zip, zipWith, (!!), Applicative(..))
import qualified Prelude

import           Control.Applicative (Applicative (..), Alternative (many))
import qualified Data.List                        as List
import           GHC.Internal.Data.Foldable       hiding (length, toList)
import qualified GHC.Internal.Data.Foldable       as Foldable
import           GHC.Internal.Data.Function       (on)
import           GHC.Internal.Data.Ord            (comparing)
import           GHC.Internal.Stack.Types     (HasCallStack)
import           GHC.Internal.Data.List.NonEmpty

infixr 5 <|

-- $setup
-- >>> import Prelude
-- >>> import qualified Data.List as List
-- >>> import Data.Ord (comparing)

-- | Number of elements in 'NonEmpty' list.
length :: NonEmpty a -> Int
length (_ :| xs) = 1 + Prelude.length xs

-- | Use 'compareLength' @xs@ @n@ as a safer and faster alternative
-- to 'compare' ('length' @xs@) @n@. Similarly, it's better
-- to write @compareLength xs 10 == LT@ instead of @length xs < 10@.
--
-- While 'length' would force and traverse
-- the entire spine of @xs@ (which could even diverge if @xs@ is infinite),
-- 'compareLength' traverses at most @n@ elements to determine its result.
--
-- >>> compareLength ('a' :| []) 1
-- EQ
-- >>> compareLength ('a' :| ['b']) 3
-- LT
-- >>> compareLength (0 :| [1..]) 100
-- GT
-- >>> compareLength undefined 0
-- GT
-- >>> compareLength ('a' :| 'b' : undefined) 1
-- GT
--
-- @since 4.21.0.0
--
compareLength :: NonEmpty a -> Int -> Ordering
compareLength xs n
  | n < 1 = GT
  | otherwise = foldr
    (\_ f m -> if m > 0 then f (m - 1) else GT)
    (\m -> if m > 0 then LT else EQ)
    xs
    n

-- | Compute n-ary logic exclusive OR operation on 'NonEmpty' list.
xor :: NonEmpty Bool -> Bool
xor (x :| xs)   = foldr xor' x xs
  where xor' True y  = not y
        xor' False y = y

-- | 'unfold' produces a new stream by repeatedly applying the unfolding
-- function to the seed value to produce an element of type @b@ and a new
-- seed value.  When the unfolding function returns 'Nothing' instead of
-- a new seed value, the stream ends.
unfold :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfold f a = case f a of
  (b, Nothing) -> b :| []
  (b, Just c)  -> b <| unfold f c
{-# DEPRECATED unfold "Use unfoldr" #-}
-- Deprecated in 8.2.1, remove in 8.4

-- | 'nonEmpty' efficiently turns a normal list into a 'NonEmpty' stream,
-- producing 'Nothing' if the input is empty.
nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty []     = Nothing
nonEmpty (a:as) = Just (a :| as)

-- | 'uncons' produces the first element of the stream, and a stream of the
-- remaining elements, if any.
uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
uncons (a :| as) = (a, nonEmpty as)

-- | The 'unfoldr' function is analogous to "Data.List"'s
-- 'GHC.Internal.Data.List.unfoldr' operation.
unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfoldr f a = case f a of
  (b, mc) -> b :| maybe [] go mc
 where
    go c = case f c of
      (d, me) -> d : maybe [] go me

-- | Extract the first element of the stream.
head :: NonEmpty a -> a
head (a :| _) = a

-- | Extract the possibly-empty tail of the stream.
tail :: NonEmpty a -> [a]
tail (_ :| as) = as

-- | Extract the last element of the stream.
last :: NonEmpty a -> a
last (a :| []) = a
last (_ :| (a : as)) = last (a :| as)

-- | Extract everything except the last element of the stream.
init :: NonEmpty a -> [a]
init (_ :| []) = []
init (a1 :| (a2 : as)) = a1 : init (a2 :| as)

-- | Construct a 'NonEmpty' list from a single element.
--
-- @since 4.15
singleton :: a -> NonEmpty a
singleton a = a :| []

-- | Prepend an element to the stream.
(<|) :: a -> NonEmpty a -> NonEmpty a
a <| bs = a :| toList bs

-- | Synonym for '<|'.
cons :: a -> NonEmpty a -> NonEmpty a
cons = (<|)

-- | Sort a stream.
sort :: Ord a => NonEmpty a -> NonEmpty a
sort = lift List.sort

-- | Sort a 'NonEmpty' on a user-supplied projection of its elements.
-- See 'List.sortOn' for more detailed information.
--
-- ==== __Examples__
--
-- >>> sortOn fst $ (2, "world") :| [(4, "!"), (1, "Hello")]
-- (1,"Hello") :| [(2,"world"),(4,"!")]
--
-- >>> sortOn List.length ("jim" :| ["creed", "pam", "michael", "dwight", "kevin"])
-- "jim" :| ["pam","creed","kevin","dwight","michael"]
--
-- ==== __Performance notes__
--
-- This function minimises the projections performed, by materialising
-- the projections in an intermediate list.
--
-- For trivial projections, you should prefer using 'sortBy' with
-- 'comparing', for example:
--
-- >>> sortBy (comparing fst) $ (3, 1) :| [(2, 2), (1, 3)]
-- (1,3) :| [(2,2),(3,1)]
--
-- Or, for the exact same API as 'sortOn', you can use `sortBy . comparing`:
--
-- >>> (sortBy . comparing) fst $ (3, 1) :| [(2, 2), (1, 3)]
-- (1,3) :| [(2,2),(3,1)]
--
-- 'sortWith' is an alias for `sortBy . comparing`.
--
-- @since 4.20.0.0
sortOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortOn f = lift (List.sortOn f)

-- | Converts a normal list to a 'NonEmpty' stream.
--
-- Raises an error if given an empty list.
fromList :: HasCallStack => [a] -> NonEmpty a
fromList (a:as) = a :| as
fromList [] = error "NonEmpty.fromList: empty list"

-- | Convert a stream to a normal list efficiently.
toList :: NonEmpty a -> [a]
toList (a :| as) = a : as

-- | Lift list operations to work on a 'NonEmpty' stream.
--
-- /Beware/: If the provided function returns an empty list,
-- this will raise an error.
lift :: Foldable f => ([a] -> [b]) -> f a -> NonEmpty b
lift f = fromList . f . Foldable.toList

-- | Map a function over a 'NonEmpty' stream.
map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f (a :| as) = f a :| fmap f as

-- | The 'inits' function takes a stream @xs@ and returns all the
-- finite prefixes of @xs@, starting with the shortest. The result is
-- 'NonEmpty' because the result always contains the empty list as the first
-- element.
--
-- > inits [1,2,3] == [] :| [[1], [1,2], [1,2,3]]
-- > inits [1] == [] :| [[1]]
-- > inits [] == [] :| []
inits :: Foldable f => f a -> NonEmpty [a]
inits = fromList . List.inits . Foldable.toList

-- | The 'inits1' function takes a 'NonEmpty' stream @xs@ and returns all the
-- 'NonEmpty' finite prefixes of @xs@, starting with the shortest.
--
-- > inits1 (1 :| [2,3]) == (1 :| []) :| [1 :| [2], 1 :| [2,3]]
-- > inits1 (1 :| []) == (1 :| []) :| []
--
-- @since 4.18
inits1 :: NonEmpty a -> NonEmpty (NonEmpty a)
inits1 = fromList . List.inits1 . Foldable.toList

-- | The 'tails' function takes a stream @xs@ and returns all the
-- suffixes of @xs@, starting with the longest. The result is 'NonEmpty'
-- because the result always contains the empty list as the last element.
--
-- > tails [1,2,3] == [1,2,3] :| [[2,3], [3], []]
-- > tails [1] == [1] :| [[]]
-- > tails [] == [] :| []
tails   :: Foldable f => f a -> NonEmpty [a]
tails = fromList . List.tails . Foldable.toList

-- | The 'tails1' function takes a 'NonEmpty' stream @xs@ and returns all the
-- non-empty suffixes of @xs@, starting with the longest.
--
-- > tails1 (1 :| [2,3]) == (1 :| [2,3]) :| [2 :| [3], 3 :| []]
-- > tails1 (1 :| []) == (1 :| []) :| []
--
-- @since 4.18
tails1 :: NonEmpty a -> NonEmpty (NonEmpty a)
tails1 xs = xs :| List.tails1 (tail xs)

-- | @'insert' x xs@ inserts @x@ into the last position in @xs@ where it
-- is still less than or equal to the next element. In particular, if the
-- list is sorted beforehand, the result will also be sorted.
insert  :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
insert a = fromList . List.insert a . Foldable.toList

-- | @'some1' x@ sequences @x@ one or more times.
some1 :: Alternative f => f a -> f (NonEmpty a)
some1 x = liftA2 (:|) x (many x)

-- | 'scanl' is similar to 'foldl', but returns a stream of successive
-- reduced values from the left:
--
-- > scanl f z [x1, x2, ...] == z :| [z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl   :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b
scanl f z = fromList . List.scanl f z . Foldable.toList

-- | 'scanr' is the right-to-left dual of 'scanl'.
-- Note that
--
-- > head (scanr f z xs) == foldr f z xs.
scanr   :: Foldable f => (a -> b -> b) -> b -> f a -> NonEmpty b
scanr f z = fromList . List.scanr f z . Foldable.toList

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f [x1, x2, ...] == x1 :| [x1 `f` x2, x1 `f` (x2 `f` x3), ...]
scanl1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanl1 f (a :| as) = fromList (List.scanl f a as)

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanr1 f (a :| as) = fromList (List.scanr1 f (a:as))

-- | 'intersperse x xs' alternates elements of the list with copies of @x@.
--
-- > intersperse 0 (1 :| [2,3]) == 1 :| [0,2,0,3]
intersperse :: a -> NonEmpty a -> NonEmpty a
intersperse a (b :| bs) = b :| case bs of
    [] -> []
    _ -> a : List.intersperse a bs

-- | @'iterate' f x@ produces the infinite sequence
-- of repeated applications of @f@ to @x@.
--
-- > iterate f x = x :| [f x, f (f x), ..]
iterate :: (a -> a) -> a -> NonEmpty a
iterate f a = a :| List.iterate f (f a)

-- | @'cycle' xs@ returns the infinite repetition of @xs@:
--
-- > cycle (1 :| [2,3]) = 1 :| [2,3,1,2,3,...]
cycle :: NonEmpty a -> NonEmpty a
cycle = fromList . List.cycle . toList

-- | 'reverse' a finite NonEmpty stream.
reverse :: NonEmpty a -> NonEmpty a
reverse = lift List.reverse

-- | @'repeat' x@ returns a constant stream, where all elements are
-- equal to @x@.
repeat :: a -> NonEmpty a
repeat a = a :| List.repeat a

-- | @'take' n xs@ returns the first @n@ elements of @xs@.
take :: Int -> NonEmpty a -> [a]
take n = List.take n . toList

-- | @'drop' n xs@ drops the first @n@ elements off the front of
-- the sequence @xs@.
drop :: Int -> NonEmpty a -> [a]
drop n = List.drop n . toList

-- | @'splitAt' n xs@ returns a pair consisting of the prefix of @xs@
-- of length @n@ and the remaining stream immediately following this prefix.
--
-- > 'splitAt' n xs == ('take' n xs, 'drop' n xs)
-- > xs == ys ++ zs where (ys, zs) = 'splitAt' n xs
splitAt :: Int -> NonEmpty a -> ([a],[a])
splitAt n = List.splitAt n . toList

-- | @'takeWhile' p xs@ returns the longest prefix of the stream
-- @xs@ for which the predicate @p@ holds.
takeWhile :: (a -> Bool) -> NonEmpty a -> [a]
takeWhile p = List.takeWhile p . toList

-- | @'dropWhile' p xs@ returns the suffix remaining after
-- @'takeWhile' p xs@.
dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
dropWhile p = List.dropWhile p . toList

-- | @'span' p xs@ returns the longest prefix of @xs@ that satisfies
-- @p@, together with the remainder of the stream.
--
-- > 'span' p xs == ('takeWhile' p xs, 'dropWhile' p xs)
-- > xs == ys ++ zs where (ys, zs) = 'span' p xs
span :: (a -> Bool) -> NonEmpty a -> ([a], [a])
span p = List.span p . toList

-- | The @'break' p@ function is equivalent to @'span' (not . p)@.
break :: (a -> Bool) -> NonEmpty a -> ([a], [a])
break p = span (not . p)

-- | @'filter' p xs@ removes any elements from @xs@ that do not satisfy @p@.
filter :: (a -> Bool) -> NonEmpty a -> [a]
filter p = List.filter p . toList

-- | The 'partition' function takes a predicate @p@ and a stream
-- @xs@, and returns a pair of lists. The first list corresponds to the
-- elements of @xs@ for which @p@ holds; the second corresponds to the
-- elements of @xs@ for which @p@ does not hold.
--
-- > 'partition' p xs = ('filter' p xs, 'filter' (not . p) xs)
partition :: (a -> Bool) -> NonEmpty a -> ([a], [a])
partition p = List.partition p . toList

-- | The 'group' function takes a stream and returns a list of
-- streams such that flattening the resulting list is equal to the
-- argument.  Moreover, each stream in the resulting list
-- contains only equal elements, and consecutive equal elements
-- of the input end up in the same stream of the output list.
-- For example, in list notation:
--
-- >>> group "Mississippi"
-- ['M' :| "",'i' :| "",'s' :| "s",'i' :| "",'s' :| "s",'i' :| "",'p' :| "p",'i' :| ""]
group :: (Foldable f, Eq a) => f a -> [NonEmpty a]
group = groupBy (==)

-- | 'groupBy' operates like 'group', but uses the provided equality
-- predicate instead of `==`.
groupBy :: Foldable f => (a -> a -> Bool) -> f a -> [NonEmpty a]
groupBy eq0 = go eq0 . Foldable.toList
  where
    go _  [] = []
    go eq (x : xs) = (x :| ys) : groupBy eq zs
      where (ys, zs) = List.span (eq x) xs

-- | 'groupWith' operates like 'group', but uses the provided projection when
-- comparing for equality
groupWith :: (Foldable f, Eq b) => (a -> b) -> f a -> [NonEmpty a]
groupWith f = groupBy ((==) `on` f)

-- | 'groupAllWith' operates like 'groupWith', but sorts the list
-- first so that each equivalence class has, at most, one list in the
-- output
groupAllWith :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
groupAllWith f = groupWith f . List.sortBy (compare `on` f)

-- | 'group1' operates like 'group', but uses the knowledge that its
-- input is non-empty to produce guaranteed non-empty output.
group1 :: Eq a => NonEmpty a -> NonEmpty (NonEmpty a)
group1 = groupBy1 (==)

-- | 'groupBy1' is to 'group1' as 'groupBy' is to 'group'.
groupBy1 :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupBy1 eq (x :| xs) = (x :| ys) :| groupBy eq zs
  where (ys, zs) = List.span (eq x) xs

-- | 'groupWith1' is to 'group1' as 'groupWith' is to 'group'
groupWith1 :: (Eq b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupWith1 f = groupBy1 ((==) `on` f)

-- | 'groupAllWith1' is to 'groupWith1' as 'groupAllWith' is to 'groupWith'
groupAllWith1 :: (Ord b) => (a -> b) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupAllWith1 f = groupWith1 f . sortWith f

-- | The 'permutations' function returns the list of all permutations of the argument.
--
-- @since 4.20.0.0
permutations            :: [a] -> NonEmpty [a]
permutations xs0        =  xs0 :| perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)
-- The implementation of 'permutations' is adopted from 'GHC.Internal.Data.List.permutations',
-- see there for discussion and explanations.

-- | 'permutations1' operates like 'permutations', but uses the knowledge that its input is
-- non-empty to produce output where every element is non-empty.
--
-- > permutations1 = fmap fromList . permutations . toList
--
-- @since 4.20.0.0
permutations1 :: NonEmpty a -> NonEmpty (NonEmpty a)
permutations1 xs = fromList <$> permutations (toList xs)

-- | The 'isPrefixOf' function returns 'True' if the first argument is
-- a prefix of the second.
isPrefixOf :: Eq a => [a] -> NonEmpty a -> Bool
isPrefixOf [] _ = True
isPrefixOf (y:ys) (x :| xs) = (y == x) && List.isPrefixOf ys xs

-- | @xs !! n@ returns the element of the stream @xs@ at index
-- @n@. Note that the head of the stream has index 0.
--
-- /Beware/: a negative or out-of-bounds index will cause an error.
(!!) :: HasCallStack => NonEmpty a -> Int -> a
(!!) (x :| xs) n
  | n == 0 = x
  | n > 0  = xs List.!! (n - 1)
  | otherwise = error "NonEmpty.!! negative index"
infixl 9 !!

-- | The 'unzip' function is the inverse of the 'zip' function.
unzip :: NonEmpty (a, b) -> (NonEmpty a, NonEmpty b)
unzip xs = (fst <$> xs, snd <$> xs)

-- | The 'nub' function removes duplicate elements from a list. In
-- particular, it keeps only the first occurrence of each element.
-- (The name 'nub' means \'essence\'.)
-- It is a special case of 'nubBy', which allows the programmer to
-- supply their own inequality test.
nub :: Eq a => NonEmpty a -> NonEmpty a
nub = nubBy (==)

-- | The 'nubBy' function behaves just like 'nub', except it uses a
-- user-supplied equality predicate instead of the overloaded '=='
-- function.
nubBy :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a
nubBy eq (a :| as) = a :| List.nubBy eq (List.filter (\b -> not (eq a b)) as)

-- | 'transpose' for 'NonEmpty', behaves the same as 'GHC.Internal.Data.List.transpose'
-- The rows/columns need not be the same length, in which case
-- > transpose . transpose /= id
transpose :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
transpose = fmap fromList
          . fromList . List.transpose . toList
          . fmap toList

-- | 'sortBy' for 'NonEmpty', behaves the same as 'GHC.Internal.Data.List.sortBy'
sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortBy f = lift (List.sortBy f)

-- | 'sortWith' for 'NonEmpty', behaves the same as:
--
-- > sortBy . comparing
sortWith :: Ord o => (a -> o) -> NonEmpty a -> NonEmpty a
sortWith = sortBy . comparing

-- | A monomorphic version of '<>' for 'NonEmpty'.
--
-- >>> append (1 :| []) (2 :| [3])
-- 1 :| [2,3]
--
-- @since 4.16
append :: NonEmpty a -> NonEmpty a -> NonEmpty a
append = (<>)

-- | Attach a list at the end of a 'NonEmpty'.
--
-- >>> appendList (1 :| [2,3]) []
-- 1 :| [2,3]
--
-- >>> appendList (1 :| [2,3]) [4,5]
-- 1 :| [2,3,4,5]
--
-- @since 4.16
appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (x :| xs) ys = x :| xs <> ys

-- | Attach a list at the beginning of a 'NonEmpty'.
--
-- >>> prependList [] (1 :| [2,3])
-- 1 :| [2,3]
--
-- >>> prependList [negate 1, 0] (1 :| [2, 3])
-- -1 :| [0,1,2,3]
--
-- @since 4.16
prependList :: [a] -> NonEmpty a -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  (x : xs) -> x :| xs <> toList ne
