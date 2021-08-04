-- |
-- Module      : Basement.Sized.List
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A Nat-sized list abstraction
--
-- Using this module is limited to GHC 7.10 and above.
--
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
module Basement.Sized.List
    ( ListN
    , toListN
    , toListN_
    , unListN
    , length
    , create
    , createFrom
    , empty
    , singleton
    , uncons
    , cons
    , unsnoc
    , snoc
    , index
    , indexStatic
    , updateAt
    , map
    , mapi
    , elem
    , foldl
    , foldl'
    , foldl1'
    , scanl'
    , scanl1'
    , foldr
    , foldr1
    , reverse
    , append
    , minimum
    , maximum
    , head
    , tail
    , init
    , take
    , drop
    , splitAt
    , zip, zip3, zip4, zip5
    , unzip
    , zipWith, zipWith3, zipWith4, zipWith5
    , replicate
    -- * Applicative And Monadic
    , replicateM
    , sequence
    , sequence_
    , mapM
    , mapM_
    ) where

import           Data.Proxy
import qualified Data.List
import           Basement.Compat.Base
import           Basement.Compat.CallStack
import           Basement.Compat.Natural
import           Basement.Nat
import           Basement.NormalForm
import           Basement.Numerical.Additive
import           Basement.Numerical.Subtractive
import           Basement.Types.OffsetSize
import           Basement.Compat.ExtList ((!!))
import qualified Prelude
import qualified Control.Monad as M (replicateM, mapM, mapM_, sequence, sequence_)

impossible :: HasCallStack => a
impossible = error "ListN: internal error: the impossible happened"

-- | A Typed-level sized List equivalent to [a]
newtype ListN (n :: Nat) a = ListN { unListN :: [a] }
    deriving (Eq,Ord,Typeable,Generic)

instance Show a => Show (ListN n a) where
    show (ListN l) = show l

instance NormalForm a => NormalForm (ListN n a) where
    toNormalForm (ListN l) = toNormalForm l

-- | Try to create a ListN from a List, succeeding if the length is correct
toListN :: forall (n :: Nat) a . (KnownNat n, NatWithinBound Int n) => [a] -> Maybe (ListN n a)
toListN l
    | expected == Prelude.fromIntegral (Prelude.length l) = Just (ListN l)
    | otherwise                                           = Nothing
  where
    expected = natValInt (Proxy :: Proxy n)

-- | Create a ListN from a List, expecting a given length
--
-- If this list contains more or less than the expected length of the resulting type,
-- then an asynchronous error is raised. use 'toListN' for a more friendly functions
toListN_ :: forall n a . (HasCallStack, NatWithinBound Int n, KnownNat n) => [a] -> ListN n a
toListN_ l
    | expected == got = ListN l
    | otherwise       = error ("toListN_: expecting list of " <> show expected <> " elements, got " <> show got <> " elements")
  where
    expected = natValInt (Proxy :: Proxy n)
    got      = Prelude.length l

-- | performs a monadic action n times, gathering the results in a List of size n.
replicateM :: forall (n :: Nat) m a . (NatWithinBound Int n, Monad m, KnownNat n) => m a -> m (ListN n a)
replicateM action = ListN <$> M.replicateM (Prelude.fromIntegral $ natVal (Proxy :: Proxy n)) action

-- | Evaluate each monadic action in the list sequentially, and collect the results.
sequence :: Monad m => ListN n (m a) -> m (ListN n a)
sequence (ListN l) = ListN <$> M.sequence l

-- | Evaluate each monadic action in the list sequentially, and ignore the results.
sequence_ :: Monad m => ListN n (m a) -> m ()
sequence_ (ListN l) = M.sequence_ l

-- | Map each element of a List to a monadic action, evaluate these
-- actions sequentially and collect the results
mapM :: Monad m => (a -> m b) -> ListN n a -> m (ListN n b)
mapM f (ListN l) = ListN <$> M.mapM f l

-- | Map each element of a List to a monadic action, evaluate these
-- actions sequentially and ignore the results
mapM_ :: Monad m => (a -> m b) -> ListN n a -> m ()
mapM_ f (ListN l) = M.mapM_ f l

-- | Create a list of n elements where each element is the element in argument
replicate :: forall (n :: Nat) a . (NatWithinBound Int n, KnownNat n) => a -> ListN n a
replicate a = ListN $ Prelude.replicate (Prelude.fromIntegral $ natVal (Proxy :: Proxy n)) a

-- | Decompose a list into its head and tail.
uncons :: (1 <= n) => ListN n a -> (a, ListN (n-1) a)
uncons (ListN (x:xs)) = (x, ListN xs)
uncons _ = impossible

-- | prepend an element to the list
cons :: a -> ListN n a -> ListN (n+1) a
cons a (ListN l) = ListN (a : l)

-- | Decompose a list into its first elements and the last.
unsnoc :: (1 <= n) => ListN n a -> (ListN (n-1) a, a)
unsnoc (ListN l) = (ListN $ Data.List.init l, Data.List.last l)

-- | append an element to the list
snoc :: ListN n a -> a -> ListN (n+1) a
snoc (ListN l) a = ListN (l Prelude.++ [a])

-- | Create an empty list of a
empty :: ListN 0 a
empty = ListN []

-- | Get the length of a list
length :: forall a (n :: Nat) . (KnownNat n, NatWithinBound Int n) => ListN n a -> CountOf a
length _ = CountOf $ natValInt (Proxy :: Proxy n)

-- | Create a new list of size n, repeately calling f from 0 to n-1
create :: forall a (n :: Nat) . KnownNat n => (Natural -> a) -> ListN n a
create f = ListN $ Prelude.map (f . Prelude.fromIntegral) [0..(len-1)]
  where
    len = natVal (Proxy :: Proxy n)

-- | Same as create but apply an offset
createFrom :: forall a (n :: Nat) (start :: Nat) . (KnownNat n, KnownNat start)
           => Proxy start -> (Natural -> a) -> ListN n a
createFrom p f = ListN $ Prelude.map (f . Prelude.fromIntegral) [idx..(idx+len-1)]
  where
    len = natVal (Proxy :: Proxy n)
    idx = natVal p

-- | create a list of 1 element
singleton :: a -> ListN 1 a
singleton a = ListN [a]

-- | Check if a list contains the element a
elem :: Eq a => a -> ListN n a -> Bool
elem a (ListN l) = Prelude.elem a l

-- | Append 2 list together returning the new list
append :: ListN n a -> ListN m a -> ListN (n+m) a
append (ListN l1) (ListN l2) = ListN (l1 <> l2)

-- | Get the maximum element of a list
maximum :: (Ord a, 1 <= n) => ListN n a -> a
maximum (ListN l) = Prelude.maximum l

-- | Get the minimum element of a list
minimum :: (Ord a, 1 <= n) => ListN n a -> a
minimum (ListN l) = Prelude.minimum l

-- | Get the head element of a list
head :: (1 <= n) => ListN n a -> a
head (ListN (x:_)) = x
head _ = impossible

-- | Get the tail of a list
tail :: (1 <= n) => ListN n a -> ListN (n-1) a
tail (ListN (_:xs)) = ListN xs
tail _ = impossible

-- | Get the list with the last element missing
init :: (1 <= n) => ListN n a -> ListN (n-1) a
init (ListN l) = ListN $ Data.List.init l

-- | Take m elements from the beggining of the list.
--
-- The number of elements need to be less or equal to the list in argument
take :: forall a (m :: Nat) (n :: Nat) . (KnownNat m, NatWithinBound Int m, m <= n) => ListN n a -> ListN m a
take (ListN l) = ListN (Prelude.take n l)
  where n = natValInt (Proxy :: Proxy m)

-- | Drop elements from a list keeping the m remaining elements
drop :: forall a d (m :: Nat) (n :: Nat) . (KnownNat d, NatWithinBound Int d, (n - m) ~ d, m <= n) => ListN n a -> ListN m a
drop (ListN l) = ListN (Prelude.drop n l)
  where n = natValInt (Proxy :: Proxy d)

-- | Split a list into two, returning 2 lists
splitAt :: forall a d (m :: Nat) (n :: Nat) . (KnownNat d, NatWithinBound Int d, (n - m) ~ d, m <= n) => ListN n a -> (ListN m a, ListN (n-m) a)
splitAt (ListN l) = let (l1, l2) = Prelude.splitAt n l in (ListN l1, ListN l2)
  where n = natValInt (Proxy :: Proxy d)

-- | Get the i'th elements
--
-- This only works with TypeApplication:
--
-- > indexStatic @1 (toListN_ [1,2,3] :: ListN 3 Int)
indexStatic :: forall i n a . (KnownNat i, CmpNat i n ~ 'LT, Offsetable a i) => ListN n a -> a
indexStatic (ListN l) = l !! (natValOffset (Proxy :: Proxy i))

-- | Get the i'the element
index :: ListN n ty -> Offset ty -> ty
index (ListN l) ofs = l !! ofs

-- | Update the value in a list at a specific location
updateAt :: forall n a
         .  Offset a
         -> (a -> a)
         -> ListN n a
         -> ListN n a
updateAt o f (ListN l) = ListN (doUpdate 0 l)
  where doUpdate _ []     = []
        doUpdate i (x:xs)
            | i == o      = f x : xs
            | otherwise   = x : doUpdate (i+1) xs

-- | Map all elements in a list
map :: (a -> b) -> ListN n a -> ListN n b
map f (ListN l) = ListN (Prelude.map f l)

-- | Map all elements in a list with an additional index
mapi :: (Natural -> a -> b) -> ListN n a -> ListN n b
mapi f (ListN l) = ListN . loop 0 $ l
  where loop _ []     = []
        loop i (x:xs) = f i x : loop (i+1) xs

-- | Fold all elements from left
foldl :: (b -> a -> b) -> b -> ListN n a -> b
foldl f acc (ListN l) = Prelude.foldl f acc l

-- | Fold all elements from left strictly
foldl' :: (b -> a -> b) -> b -> ListN n a -> b
foldl' f acc (ListN l) = Data.List.foldl' f acc l

-- | Fold all elements from left strictly with a first element
-- as the accumulator
foldl1' :: (1 <= n) => (a -> a -> a) -> ListN n a -> a
foldl1' f (ListN l) = Data.List.foldl1' f l

-- | Fold all elements from right
foldr :: (a -> b -> b) -> b -> ListN n a -> b
foldr f acc (ListN l) = Prelude.foldr f acc l

-- | Fold all elements from right assuming at least one element is in the list.
foldr1 :: (1 <= n) => (a -> a -> a) -> ListN n a -> a
foldr1 f (ListN l) = Prelude.foldr1 f l

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
scanl' :: (b -> a -> b) -> b -> ListN n a -> ListN (n+1) b
scanl' f initialAcc (ListN start) = ListN (go initialAcc start)
  where
    go !acc l = acc : case l of
                        []     -> []
                        (x:xs) -> go (f acc x) xs

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1' :: (a -> a -> a) -> ListN n a -> ListN n a
scanl1' f (ListN l) = case l of
                        []     -> ListN []
                        (x:xs) -> ListN $ Data.List.scanl' f x xs

-- | Reverse a list
reverse :: ListN n a -> ListN n a
reverse (ListN l) = ListN (Prelude.reverse l)

-- | Zip 2 lists of the same size, returning a new list of
-- the tuple of each elements
zip :: ListN n a -> ListN n b -> ListN n (a,b)
zip (ListN l1) (ListN l2) = ListN (Prelude.zip l1 l2)

-- | Unzip a list of tuple, to 2 List of the deconstructed tuples
unzip :: ListN n (a,b) -> (ListN n a, ListN n b)
unzip l = (map fst l, map snd l)

-- | Zip 3 lists of the same size
zip3 :: ListN n a -> ListN n b -> ListN n c -> ListN n (a,b,c)
zip3 (ListN x1) (ListN x2) (ListN x3) = ListN (loop x1 x2 x3)
  where loop (l1:l1s) (l2:l2s) (l3:l3s) = (l1,l2,l3) : loop l1s l2s l3s
        loop []       _        _        = []
        loop _        _        _        = impossible

-- | Zip 4 lists of the same size
zip4 :: ListN n a -> ListN n b -> ListN n c -> ListN n d -> ListN n (a,b,c,d)
zip4 (ListN x1) (ListN x2) (ListN x3) (ListN x4) = ListN (loop x1 x2 x3 x4)
  where loop (l1:l1s) (l2:l2s) (l3:l3s) (l4:l4s) = (l1,l2,l3,l4) : loop l1s l2s l3s l4s
        loop []       _        _        _        = []
        loop _        _        _        _        = impossible

-- | Zip 5 lists of the same size
zip5 :: ListN n a -> ListN n b -> ListN n c -> ListN n d -> ListN n e -> ListN n (a,b,c,d,e)
zip5 (ListN x1) (ListN x2) (ListN x3) (ListN x4) (ListN x5) = ListN (loop x1 x2 x3 x4 x5)
  where loop (l1:l1s) (l2:l2s) (l3:l3s) (l4:l4s) (l5:l5s) = (l1,l2,l3,l4,l5) : loop l1s l2s l3s l4s l5s
        loop []       _        _        _        _        = []
        loop _        _        _        _        _        = impossible

-- | Zip 2 lists using a function
zipWith :: (a -> b -> x) -> ListN n a -> ListN n b -> ListN n x
zipWith f (ListN (v1:vs)) (ListN (w1:ws)) = ListN (f v1 w1 : unListN (zipWith f (ListN vs) (ListN ws)))
zipWith _ (ListN [])       _ = ListN []
zipWith _ _                _ = impossible

-- | Zip 3 lists using a function
zipWith3 :: (a -> b -> c -> x)
         -> ListN n a
         -> ListN n b
         -> ListN n c
         -> ListN n x
zipWith3 f (ListN (v1:vs)) (ListN (w1:ws)) (ListN (x1:xs)) =
    ListN (f v1 w1 x1 : unListN (zipWith3 f (ListN vs) (ListN ws) (ListN xs)))
zipWith3 _ (ListN []) _       _ = ListN []
zipWith3 _ _          _       _ = impossible

-- | Zip 4 lists using a function
zipWith4 :: (a -> b -> c -> d -> x)
         -> ListN n a
         -> ListN n b
         -> ListN n c
         -> ListN n d
         -> ListN n x
zipWith4 f (ListN (v1:vs)) (ListN (w1:ws)) (ListN (x1:xs)) (ListN (y1:ys)) =
    ListN (f v1 w1 x1 y1 : unListN (zipWith4 f (ListN vs) (ListN ws) (ListN xs) (ListN ys)))
zipWith4 _ (ListN []) _       _       _ = ListN []
zipWith4 _ _          _       _       _ = impossible

-- | Zip 5 lists using a function
zipWith5 :: (a -> b -> c -> d -> e -> x)
         -> ListN n a
         -> ListN n b
         -> ListN n c
         -> ListN n d
         -> ListN n e
         -> ListN n x
zipWith5 f (ListN (v1:vs)) (ListN (w1:ws)) (ListN (x1:xs)) (ListN (y1:ys)) (ListN (z1:zs)) =
    ListN (f v1 w1 x1 y1 z1 : unListN (zipWith5 f (ListN vs) (ListN ws) (ListN xs) (ListN ys) (ListN zs)))
zipWith5 _ (ListN []) _       _       _       _ = ListN []
zipWith5 _ _          _       _       _       _ = impossible
