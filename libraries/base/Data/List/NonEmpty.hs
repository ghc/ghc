{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
   , head        -- :: NonEmpty a -> a
   , tail        -- :: NonEmpty a -> [a]
   , last        -- :: NonEmpty a -> a
   , init        -- :: NonEmpty a -> [a]
   , (<|), cons  -- :: a -> NonEmpty a -> NonEmpty a
   , uncons      -- :: NonEmpty a -> (a, Maybe (NonEmpty a))
   , unfoldr     -- :: (a -> (b, Maybe a)) -> a -> NonEmpty b
   , sort        -- :: NonEmpty a -> NonEmpty a
   , reverse     -- :: NonEmpty a -> NonEmpty a
   , inits       -- :: Foldable f => f a -> NonEmpty a
   , tails       -- :: Foldable f => f a -> NonEmpty a
   -- * Building streams
   , iterate     -- :: (a -> a) -> a -> NonEmpty a
   , repeat      -- :: a -> NonEmpty a
   , cycle       -- :: NonEmpty a -> NonEmpty a
   , unfold      -- :: (a -> (b, Maybe a) -> a -> NonEmpty b
   , insert      -- :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
   , some1       -- :: Alternative f => f a -> f (NonEmpty a)
   -- * Extracting sublists
   , take        -- :: Int -> NonEmpty a -> [a]
   , drop        -- :: Int -> NonEmpty a -> [a]
   , splitAt     -- :: Int -> NonEmpty a -> ([a], [a])
   , takeWhile   -- :: Int -> NonEmpty a -> [a]
   , dropWhile   -- :: Int -> NonEmpty a -> [a]
   , span        -- :: Int -> NonEmpty a -> ([a],[a])
   , break       -- :: Int -> NonEmpty a -> ([a],[a])
   , filter      -- :: (a -> Bool) -> NonEmpty a -> [a]
   , partition   -- :: (a -> Bool) -> NonEmpty a -> ([a],[a])
   , group       -- :: Foldable f => Eq a => f a -> [NonEmpty a]
   , groupBy     -- :: Foldable f => (a -> a -> Bool) -> f a -> [NonEmpty a]
   , groupWith     -- :: (Foldable f, Eq b) => (a -> b) -> f a -> [NonEmpty a]
   , groupAllWith  -- :: (Foldable f, Ord b) => (a -> b) -> f a -> [NonEmpty a]
   , group1      -- :: Eq a => NonEmpty a -> NonEmpty (NonEmpty a)
   , groupBy1    -- :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
   , groupWith1     -- :: (Foldable f, Eq b) => (a -> b) -> f a -> NonEmpty (NonEmpty a)
   , groupAllWith1  -- :: (Foldable f, Ord b) => (a -> b) -> f a -> NonEmpty (NonEmpty a)
   -- * Sublist predicates
   , isPrefixOf  -- :: Foldable f => f a -> NonEmpty a -> Bool
   -- * \"Set\" operations
   , nub         -- :: Eq a => NonEmpty a -> NonEmpty a
   , nubBy       -- :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a
   -- * Indexing streams
   , (!!)        -- :: NonEmpty a -> Int -> a
   -- * Zipping and unzipping streams
   , zip         -- :: NonEmpty a -> NonEmpty b -> NonEmpty (a,b)
   , zipWith     -- :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
   , unzip       -- :: NonEmpty (a, b) -> (NonEmpty a, NonEmpty b)
   -- * Converting to and from a list
   , fromList    -- :: [a] -> NonEmpty a
   , toList      -- :: NonEmpty a -> [a]
   , nonEmpty    -- :: [a] -> Maybe (NonEmpty a)
   , xor         -- :: NonEmpty a -> Bool
   ) where


import           Prelude             hiding (break, cycle, drop, dropWhile,
                                      filter, foldl, foldr, head, init, iterate,
                                      last, length, map, repeat, reverse,
                                      scanl, scanl1, scanr, scanr1, span,
                                      splitAt, tail, take, takeWhile,
                                      unzip, zip, zipWith, (!!))
import qualified Prelude

import           Control.Applicative (Alternative, many)
import           Control.Monad       (ap)
import           Control.Monad.Fix
import           Control.Monad.Zip   (MonadZip(..))
import           Data.Data           (Data)
import           Data.Foldable       hiding (length, toList)
import qualified Data.Foldable       as Foldable
import           Data.Function       (on)
import qualified Data.List           as List
import           Data.Ord            (comparing)
import qualified GHC.Exts            as Exts (IsList(..))
import           GHC.Generics        (Generic, Generic1)

infixr 5 :|, <|

-- | Non-empty (and non-strict) list type.
--
-- @since 4.9.0.0
data NonEmpty a = a :| [a]
  deriving ( Eq, Ord, Show, Read, Data, Generic, Generic1 )

instance Exts.IsList (NonEmpty a) where
  type Item (NonEmpty a) = a
  fromList               = fromList
  toList                 = toList

instance MonadFix NonEmpty where
  mfix f = case fix (f . head) of
             ~(x :| _) -> x :| mfix (tail . f)

instance MonadZip NonEmpty where
  mzip     = zip
  mzipWith = zipWith
  munzip   = unzip

-- | Number of elements in 'NonEmpty' list.
length :: NonEmpty a -> Int
length (_ :| xs) = 1 + Prelude.length xs

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

-- | 'nonEmpty' efficiently turns a normal list into a 'NonEmpty' stream,
-- producing 'Nothing' if the input is empty.
nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty []     = Nothing
nonEmpty (a:as) = Just (a :| as)

-- | 'uncons' produces the first element of the stream, and a stream of the
-- remaining elements, if any.
uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
uncons ~(a :| as) = (a, nonEmpty as)

-- | The 'unfoldr' function is analogous to "Data.List"'s
-- 'Data.List.unfoldr' operation.
unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfoldr f a = case f a of
  (b, mc) -> b :| maybe [] go mc
 where
    go c = case f c of
      (d, me) -> d : maybe [] go me

instance Functor NonEmpty where
  fmap f ~(a :| as) = f a :| fmap f as
  b <$ ~(_ :| as)   = b   :| (b <$ as)

instance Applicative NonEmpty where
  pure a = a :| []
  (<*>) = ap

instance Monad NonEmpty where
  ~(a :| as) >>= f = b :| (bs ++ bs')
    where b :| bs = f a
          bs' = as >>= toList . f

instance Traversable NonEmpty where
  traverse f ~(a :| as) = (:|) <$> f a <*> traverse f as

instance Foldable NonEmpty where
  foldr f z ~(a :| as) = f a (foldr f z as)
  foldl f z ~(a :| as) = foldl f (f z a) as
  foldl1 f ~(a :| as) = foldl f a as
  foldMap f ~(a :| as) = f a `mappend` foldMap f as
  fold ~(m :| ms) = m `mappend` fold ms

-- | Extract the first element of the stream.
head :: NonEmpty a -> a
head ~(a :| _) = a

-- | Extract the possibly-empty tail of the stream.
tail :: NonEmpty a -> [a]
tail ~(_ :| as) = as

-- | Extract the last element of the stream.
last :: NonEmpty a -> a
last ~(a :| as) = List.last (a : as)

-- | Extract everything except the last element of the stream.
init :: NonEmpty a -> [a]
init ~(a :| as) = List.init (a : as)

-- | Prepend an element to the stream.
(<|) :: a -> NonEmpty a -> NonEmpty a
a <| ~(b :| bs) = a :| b : bs

-- | Synonym for '<|'.
cons :: a -> NonEmpty a -> NonEmpty a
cons = (<|)

-- | Sort a stream.
sort :: Ord a => NonEmpty a -> NonEmpty a
sort = lift List.sort

-- | Converts a normal list to a 'NonEmpty' stream.
--
-- Raises an error if given an empty list.
fromList :: [a] -> NonEmpty a
fromList (a:as) = a :| as
fromList [] = errorWithoutStackTrace "NonEmpty.fromList: empty list"

-- | Convert a stream to a normal list efficiently.
toList :: NonEmpty a -> [a]
toList ~(a :| as) = a : as

-- | Lift list operations to work on a 'NonEmpty' stream.
--
-- /Beware/: If the provided function returns an empty list,
-- this will raise an error.
lift :: Foldable f => ([a] -> [b]) -> f a -> NonEmpty b
lift f = fromList . f . Foldable.toList

-- | Map a function over a 'NonEmpty' stream.
map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f ~(a :| as) = f a :| fmap f as

-- | The 'inits' function takes a stream @xs@ and returns all the
-- finite prefixes of @xs@.
inits :: Foldable f => f a -> NonEmpty [a]
inits = fromList . List.inits . Foldable.toList

-- | The 'tails' function takes a stream @xs@ and returns all the
-- suffixes of @xs@.
tails   :: Foldable f => f a -> NonEmpty [a]
tails = fromList . List.tails . Foldable.toList

-- | @'insert' x xs@ inserts @x@ into the last position in @xs@ where it
-- is still less than or equal to the next element. In particular, if the
-- list is sorted beforehand, the result will also be sorted.
insert  :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
insert a = fromList . List.insert a . Foldable.toList

-- | @'some1' x@ sequences @x@ one or more times.
some1 :: Alternative f => f a -> f (NonEmpty a)
some1 x = (:|) <$> x <*> many x

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
scanl1 f ~(a :| as) = fromList (List.scanl f a as)

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanr1 f ~(a :| as) = fromList (List.scanr1 f (a:as))

-- | 'intersperse x xs' alternates elements of the list with copies of @x@.
--
-- > intersperse 0 (1 :| [2,3]) == 1 :| [0,2,0,3]
intersperse :: a -> NonEmpty a -> NonEmpty a
intersperse a ~(b :| bs) = b :| case bs of
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
-- contains only equal elements.  For example, in list notation:
--
-- > 'group' $ 'cycle' "Mississippi"
-- >   = "M" : "i" : "ss" : "i" : "ss" : "i" : "pp" : "i" : "M" : "i" : ...
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

-- | The 'isPrefix' function returns @True@ if the first argument is
-- a prefix of the second.
isPrefixOf :: Eq a => [a] -> NonEmpty a -> Bool
isPrefixOf [] _ = True
isPrefixOf (y:ys) (x :| xs) = (y == x) && List.isPrefixOf ys xs

-- | @xs !! n@ returns the element of the stream @xs@ at index
-- @n@. Note that the head of the stream has index 0.
--
-- /Beware/: a negative or out-of-bounds index will cause an error.
(!!) :: NonEmpty a -> Int -> a
(!!) ~(x :| xs) n
  | n == 0 = x
  | n > 0  = xs List.!! (n - 1)
  | otherwise = errorWithoutStackTrace "NonEmpty.!! negative argument"

-- | The 'zip' function takes two streams and returns a stream of
-- corresponding pairs.
zip :: NonEmpty a -> NonEmpty b -> NonEmpty (a,b)
zip ~(x :| xs) ~(y :| ys) = (x, y) :| List.zip xs ys

-- | The 'zipWith' function generalizes 'zip'. Rather than tupling
-- the elements, the elements are combined using the function
-- passed as the first argument.
zipWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipWith f ~(x :| xs) ~(y :| ys) = f x y :| List.zipWith f xs ys

-- | The 'unzip' function is the inverse of the 'zip' function.
unzip :: Functor f => f (a,b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)

-- | The 'nub' function removes duplicate elements from a list. In
-- particular, it keeps only the first occurence of each element.
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

-- | 'transpose' for 'NonEmpty', behaves the same as 'Data.List.transpose'
-- The rows/columns need not be the same length, in which case
-- > transpose . transpose /= id
transpose :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
transpose = fmap fromList
          . fromList . List.transpose . Foldable.toList
          . fmap Foldable.toList

-- | 'sortBy' for 'NonEmpty', behaves the same as 'Data.List.sortBy'
sortBy :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a
sortBy f = lift (List.sortBy f)

-- | 'sortWith' for 'NonEmpty', behaves the same as:
--
-- > sortBy . comparing
sortWith :: Ord o => (a -> o) -> NonEmpty a -> NonEmpty a
sortWith = sortBy . comparing
