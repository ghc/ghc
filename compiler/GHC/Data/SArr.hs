{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrapper around 'SmallArray#'
module GHC.Data.SArr (
      SArr,
      -- * Indexing
      get, unsafeGet, (!),
      -- * Construction
      take, drop, replicate,
      -- * Slicing
      Slice(Empty,(:<|),(:|>)), slice, toSArr, takeS, dropS,
      -- * Other operations
      all, map, zipWith
  ) where

import Prelude hiding (replicate, drop, take, head, init, map, all, zipWith)
import qualified Prelude

import qualified Data.List as List
import qualified GHC.Exts as Exts
import GHC.ST
import GHC.Stack

-- | A general-purpose array type that is strict in its elements and the
-- combinators of which enjoy list fusion.
-- It should be used whenever we'd normally use lists, except when we
-- concatenate them a lot or use the list in a persistent way (e.g., as a
-- stack).
data SArr a = SArr (Exts.SmallArray# a)

get :: Int -> SArr a -> Maybe a
get n arr
  | n < length arr = Just $! unsafeGet n arr
  | otherwise      = Nothing

unsafeGet :: Int -> SArr a -> a
unsafeGet (Exts.I# n) (SArr arr) =
  case Exts.indexSmallArray# arr n of (# a #) -> a

(!) :: SArr a -> Int -> a
(!) = flip unsafeGet

instance Exts.IsList (SArr a) where
  type Item (SArr a) = a
  fromList = fromList
  {-# INLINE fromList #-}
  fromListN = fromListN
  {-# INLINE fromListN #-}
  toList = toList
  {-# INLINE toList #-}

instance Show a => Show (SArr a) where
  showsPrec p arr = showParen (p >= 10) $
    showString "fromList " . shows (toList arr)

die :: String -> String -> a
die fun problem = error (fun ++ ": " ++ problem)
{-# NOINLINE die #-}

--
-- * Some boring ST-like type synonyms
--

-- | Like 'STRep', but representation polymorphic in `a`.
type USTRep  s (a :: Exts.TYPE Exts.UnliftedRep) = (# Exts.State# s, a #)
-- | Like 'USTRep', but with an additional field for the (L)ength.
type USTRepL s (a :: Exts.TYPE Exts.UnliftedRep) = (# Exts.State# s, a, Exts.Int# #)
-- | Like 'ST', but representation polymorphic in `a`.
type UST  s (a :: Exts.TYPE Exts.UnliftedRep) = Exts.State# s -> USTRep s a
-- | Like 'UST', but with an additional field for the (L)ength.
type USTL s (a :: Exts.TYPE Exts.UnliftedRep) = Exts.State# s -> USTRepL s a

-- And now the same, but as a monad transformer and thus boxed (so that we can
-- return it in a monadic context)

-- | A box around 'USTRep'.
-- data BoxedUSTRep  s (a :: Exts.TYPE Exts.UnliftedRep) = BoxedUSTRep  (USTRep s a)
-- | A box around 'USTRepL'.
data BoxedUSTRepL s (a :: Exts.TYPE Exts.UnliftedRep) = BoxedUSTRepL (USTRepL s a)
-- | Like 'UST', but with a box around the rep.
-- type BoxedUST  f s (a :: Exts.TYPE Exts.UnliftedRep) = Exts.State# s -> f (BoxedUSTRep s a)
-- | Like 'USTL', but with a box around the rep.
type BoxedUSTL f s (a :: Exts.TYPE Exts.UnliftedRep) = Exts.State# s -> f (BoxedUSTRepL s a)

-- | The initial size of an array when we don't know the number of elements
-- when initialising from a list ('fromList').
iNIT_SIZE :: Int
iNIT_SIZE = 4

-- | Most C++ vector impls use a growth factor of 1.5, because it's near the
-- golden ratio. So we do that, too.
gROWTH_FACTOR :: Exts.Int# -> Exts.Int#
gROWTH_FACTOR n = Exts.uncheckedIShiftRA# (n Exts.*# 3#) 1#
{-# INLINE gROWTH_FACTOR #-}

createSmallArrayN
  :: Int
  -> a
  -> (forall s. Exts.SmallMutableArray# s a -> UST s (Exts.SmallMutableArray# s a))
  -> SArr a
createSmallArrayN (Exts.I# n) x f = runST $ ST $ \s ->
  let !(# s1, sma  #) = Exts.newSmallArray# n x s
      !(# s2, sma' #) = f sma s1
      !(# s3, sa   #) = Exts.unsafeFreezeSmallArray# sma' s2
      -- !_              = trace ("createN: " ++ show (Exts.I# n)) ()
  in (# s3, SArr sa #)
{-# INLINE createSmallArrayN #-}

createSmallArray
  :: a
  -> (forall s. Exts.SmallMutableArray# s a -> USTL s (Exts.SmallMutableArray# s a))
  -> SArr a
createSmallArray x f = runST $ ST $ \s ->
  let !(Exts.I# init_size)  = iNIT_SIZE
      !(# s1, sma  #)       = Exts.newSmallArray# init_size x s
      !(# s2, sma', n #)    = f sma s1
      -- !(# s2', m #)         = Exts.getSizeofSmallMutableArray# sma' s2
      !s3                   = Exts.shrinkSmallMutableArray# sma' n s2
      !(# s4, sa   #)       = Exts.unsafeFreezeSmallArray# sma' s3
      -- !_                    = trace ("create: " ++ show (Exts.I# n) ++ show (Exts.I# m)) ()
  in (# s4, SArr sa #)
{-# INLINE createSmallArray #-}

createSmallArrayF
  :: Functor f
  => a
  -> (forall s. Exts.SmallMutableArray# s a -> BoxedUSTL f s (Exts.SmallMutableArray# s a))
  -> f (SArr a)
createSmallArrayF x f =
  let !(Exts.I# n)    = iNIT_SIZE
      !(# s1, sma  #) = Exts.newSmallArray# n x Exts.realWorld#
      (<&>)           = flip (<$>)
  in f sma s1 <&> (\(BoxedUSTRepL (# s2, sma', n #)) ->
  let s3             = Exts.shrinkSmallMutableArray# sma' n s2
      !(# _s4, sa #) = Exts.unsafeFreezeSmallArray# sma' s3
  -- Since SArr is strict in sa, discarding the RealWorld# token should be fine
  in SArr sa)
{-# INLINABLE[0] createSmallArrayF #-}

ifoldr :: (Int -> a -> b -> b) -> (Int -> b) -> [a] -> b
ifoldr f acc xs = List.foldr c z xs 0
  where
    c a g = Exts.oneShot (\i -> f i a $ g (i+1))
    z = Exts.oneShot $ \(!i) -> acc i
{-# INLINE ifoldr #-}

ifoldrUST :: forall s a (b :: Exts.TYPE Exts.UnliftedRep). (Int -> a -> b -> UST s b) -> b -> [a] -> USTL s b
ifoldrUST f b as s = ifoldr c z as b s
  where
    c :: Int -> a -> (b -> USTL s b) -> b -> USTL s b
    c i a k = Exts.oneShot $ \b s -> case f i a b s of (# s', b' #) -> k b' s'
    z :: Int -> b -> USTL s b
    z = Exts.oneShot $ \(Exts.I# i) -> Exts.oneShot $ \b s -> (# s, b, i #)
{-# INLINE ifoldrUST #-}

dieFromList :: a
dieFromList = die "fromList" "uninitialized element"
{-# NOINLINE dieFromList #-}

fromList :: [a] -> SArr a
fromList l =
  createSmallArray dieFromList $ Exts.oneShot $ \sma ->
    ifoldrUST (\(Exts.I# i) a sma' s -> a `seq` writeResize i a sma' s) sma l
{-# INLINE [2] fromList #-}
-- we need [2], otherwise FB's (and their builds) will be rewritten back to
-- list producing functions and we can't fuse away the ifoldr

dieWriteResize :: a
dieWriteResize = die "writeResize" "uninitialized element"
{-# NOINLINE dieWriteResize #-}

writeResize :: Exts.Int# -> a -> Exts.SmallMutableArray# s a -> UST s (Exts.SmallMutableArray# s a)
writeResize i a sma s =
  let !(# s1, n #) = Exts.getSizeofSmallMutableArray# sma s -- TODO: cache this
      !(# s2, sma' #)
        | Exts.isTrue# (i Exts.<# n) = (# s1, sma #)
        | otherwise                  = Exts.resizeSmallMutableArray# sma (gROWTH_FACTOR n) dieWriteResize s1
      -- !_                             = trace (printf "writeResize: %d %d %d\n" (Exts.I# i) (Exts.I# n) (Exts.I# (gROWTH_FACTOR n))) ()
      s3                             = Exts.writeSmallArray# sma' i a s2
  in (# s3, sma' #)
{-# NOINLINE writeResize #-}

dieFromListN :: HasCallStack => a
dieFromListN = die "fromListN" "uninitialized element"
{-# NOINLINE dieFromListN #-}

fromListN :: HasCallStack => Int -> [a] -> SArr a
fromListN n l =
  createSmallArrayN n dieFromListN $ Exts.oneShot $ \sma s ->
    case ifoldrUST (\(Exts.I# i) a _sma s -> a `seq` (# Exts.writeSmallArray# sma i a s, sma #)) sma l s of
      (# s', _sma, _n #) -> (# s', sma #)
{-# INLINE [2] fromListN #-}

toList :: SArr a -> [a]
toList arr = Prelude.map (\i -> unsafeGet i arr) [0..length arr-1]
{-# INLINE [2] toList #-}

len :: SArr a -> Int
len (SArr sa) = Exts.I# (Exts.sizeofSmallArray# sa)
{-# NOINLINE[0] len #-}

all :: (a -> Bool) -> SArr a -> Bool
all cond = List.all cond . toList
{-# INLINE [2] all #-}

map :: (a -> b) -> SArr a -> SArr b
map f a = fromList $ Prelude.map f (toList a)
{-# INLINE [2] map #-}

zipWith :: (a -> b -> c) -> SArr a -> SArr b -> SArr c
zipWith f a b = fromListN (min (len a) (len b)) $ Prelude.zipWith f (toList a) (toList b)
{-# INLINE [2] zipWith #-}

instance Eq a => Eq (SArr a) where
  a1 == a2 = len a1 == len a2 && and (Prelude.map (\i -> a1 ! i == a2 ! i) [0..len a1])

-- | 'sequence' from list to 'SArr' that fuses with the incoming list.
-- It's like an applicative 'build', but for arrays.
fromListA :: Applicative f => [f a] -> f (SArr a)
fromListA l =
  createSmallArrayF dieFromList $ \sma s ->
    ifoldr (\i fa fr -> c i <$> fa <*> fr) (\(Exts.I# n) -> pure (BoxedUSTRepL (# s, sma, n #))) l
      where
        c (Exts.I# i) a (BoxedUSTRepL (# s, sma', n #)) =
          case a `seq` writeResize i a sma' s of
            (# s1, sma2 #) -> BoxedUSTRepL (# s1, sma2, n #)
{-# INLINE [2] fromListA #-}

{-# RULES
"toList/fromList"  forall xs.    toList (fromList xs)     = xs
"toList/fromListN" forall n xs.  toList (fromListN n xs)  = xs
"fromList/toList"  forall arr.   fromList (toList arr)    = arr
"fromListN/toList" forall n arr. fromListN n (toList arr) = arr
"len/fromListN"    forall n arr. len (fromListN n arr)    = n
#-}

instance Functor SArr where
  fmap = map
  {-# INLINE fmap #-}

instance Foldable SArr where
  foldr f z = foldr f z . toList
  {-# INLINE foldr #-}
  foldl f z = foldl f z . toList
  {-# INLINE foldl #-}
  foldr1 f = foldr1 f . toList
  {-# INLINE foldr1 #-}
  foldl1 f = foldl1 f . toList
  {-# INLINE foldl1 #-}
  foldMap f = foldMap f . toList
  {-# INLINE foldMap #-}
  sum = sum . toList
  {-# INLINE sum #-}
  product = product . toList
  {-# INLINE product #-}
  length = len
  {-# INLINE length #-}
  null arr = length arr == 0
  {-# INLINE null #-}

instance Traversable SArr where
  traverse f arr = fromListA . Prelude.map f . toList $ arr
  {-# INLINE traverse #-}

replicate :: Int -> a -> SArr a
replicate n a = fromListN n (List.replicate n a)
{-# INLINE replicate #-}

take :: Int -> SArr a -> SArr a
take n = fromList . List.take n . toList
{-# INLINE take #-}

drp :: Int -> [a] -> [a]
drp !n xs = Exts.build $ \c z ->
  let !fb | n <= 0    = Exts.oneShot $ \_i a b -> a `c` b
          | otherwise = Exts.oneShot $ \ i a b -> if i < n then b else a `c` b
  in ifoldr fb (const z) xs
{-# INLINE drp #-}

tk :: Int -> [a] -> [a]
tk !n xs = Exts.build $ \c z -> ifoldr (\i a b -> if i < n then a `c` b else z) (const z) xs
{-# INLINE tk #-}

drop :: Int -> SArr a -> SArr a
drop n = fromList . drp n . toList
{-# INLINE drop #-}

data Slice a
  = Slice
  { s_arr   :: !(SArr a)
  , s_begin :: !Int
  , s_end   :: !Int
  } deriving Eq

slice :: SArr a -> Slice a
slice arr = Slice arr 0 (len arr)
{-# INLINE[1] slice #-}

toSArr :: Slice a -> SArr a
toSArr = \sl -> fromList . drp (s_begin sl) . tk (s_end sl) . toList $ s_arr sl
{-# INLINE[1] toSArr #-}

{-# RULES
"slice/toSArr" forall sl.  slice (toSArr sl)  = sl
"toSArr/slice" forall arr. toSArr (slice arr) = arr
#-}

instance Exts.IsList (Slice a) where
  type Item (Slice a) = a
  fromList = slice . fromList
  {-# INLINE fromList #-}
  fromListN n = slice . fromListN n
  {-# INLINE fromListN #-}
  toList = \sl -> List.drop (s_begin sl) . List.take (s_end sl) . toList $ s_arr sl
  {-# INLINE toList #-}

instance Functor Slice where
  fmap f sl = sl { s_arr = fmap f (s_arr sl) }
  {-# INLINE fmap #-}

instance Foldable Slice where
  foldr f z = foldr f z . Exts.toList
  {-# INLINE foldr #-}
  foldl f z = foldl f z . Exts.toList
  {-# INLINE foldl #-}
  foldr1 f = foldr1 f . Exts.toList
  {-# INLINE foldr1 #-}
  foldl1 f = foldl1 f . Exts.toList
  {-# INLINE foldl1 #-}
  foldMap f = foldMap f . Exts.toList
  {-# INLINE foldMap #-}
  sum = sum . Exts.toList
  {-# INLINE sum #-}
  product = product . Exts.toList
  {-# INLINE product #-}
  length s = max (s_end s - s_begin s) 0
  {-# INLINE length #-}
  null s = length s == 0
  {-# INLINE null #-}

head :: Slice a -> Maybe (a, Slice a)
head (null -> True)                     = Nothing
head sl@Slice{s_arr = arr, s_begin = b} = Just (a, sl')
  where
    !a  = arr ! b
    sl' = sl { s_begin = b+1 }
{-# INLINE head #-}

init :: Slice a -> Maybe (Slice a, a)
init (null -> True)                   = Nothing
init sl@Slice{s_arr = arr, s_end = e} = Just (sl', a)
  where
    !a  = arr ! (e-1)
    sl' = sl { s_end = e-1 }
{-# INLINE init #-}

takeS :: Int -> Slice a -> Slice a
takeS n sl = sl{ s_end = s_begin sl + n }

dropS :: Int -> Slice a -> Slice a
dropS n sl = sl{ s_begin = s_begin sl + n }

-- | An unidirectional pattern synonym matching an empty slice.
pattern Empty :: Slice a
pattern Empty <- (null -> True)

-- | An unidirectional pattern synonym viewing the front of a non-empty slice.
pattern (:<|) :: a -> Slice a -> Slice a
pattern a :<| rest <- (head -> Just (a, rest))
{-# COMPLETE Empty, (:<|) #-}

-- | An unidirectional pattern synonym viewing the rear of a non-empty slice.
pattern (:|>) :: Slice a -> a -> Slice a
pattern rest :|> a <- (init -> Just (rest, a))
{-# COMPLETE Empty, (:|>) #-}
