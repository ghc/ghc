{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module : Data.Primitive.SmallArray
-- Copyright: (c) 2015 Dan Doel
-- License: BSD3
--
-- Maintainer: libraries@haskell.org
-- Portability: non-portable
--
-- Small arrays are boxed (im)mutable arrays.
--
-- The underlying structure of the 'Array' type contains a card table, allowing
-- segments of the array to be marked as having been mutated. This allows the
-- garbage collector to only re-traverse segments of the array that have been
-- marked during certain phases, rather than having to traverse the entire
-- array.
--
-- 'SmallArray' lacks this table. This means that it takes up less memory and
-- has slightly faster writes. It is also more efficient during garbage
-- collection so long as the card table would have a single entry covering the
-- entire array. These advantages make them suitable for use as arrays that are
-- known to be small.
--
-- The card size is 128, so for uses much larger than that, 'Array' would likely
-- be superior.
--
-- The underlying type, 'SmallArray#', was introduced in GHC 7.10, so prior to
-- that version, this module simply implements small arrays as 'Array'.

module Data.Primitive.SmallArray
  ( SmallArray(..)
  , SmallMutableArray(..)
  , newSmallArray
  , readSmallArray
  , writeSmallArray
  , copySmallArray
  , copySmallMutableArray
  , indexSmallArray
  , indexSmallArrayM
  , cloneSmallArray
  , cloneSmallMutableArray
  , freezeSmallArray
  , unsafeFreezeSmallArray
  , thawSmallArray
  , unsafeThawSmallArray
  , sizeofSmallArray
  , sizeofSmallMutableArray
  ) where


#if (__GLASGOW_HASKELL__ >= 710)
#define HAVE_SMALL_ARRAY 1
#endif

#if MIN_VERSION_base(4,7,0)
import GHC.Exts hiding (toList)
import qualified GHC.Exts
#endif

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip
#endif
import Data.Data
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import qualified Data.Semigroup as Sem
#endif
import Text.ParserCombinators.ReadPrec
import Text.Read
import Text.Read.Lex

#if !(HAVE_SMALL_ARRAY)
import Data.Primitive.Array
import Data.Traversable
#endif

#if HAVE_SMALL_ARRAY
data SmallArray a = SmallArray (SmallArray# a)
  deriving Typeable
#else
newtype SmallArray a = SmallArray (Array a) deriving
  ( Eq
  , Ord
  , Show
  , Read
  , Foldable
  , Traversable
  , Functor
  , Applicative
  , Alternative
  , Monad
  , MonadPlus
#if MIN_VERSION_base(4,4,0)
  , MonadZip
#endif
  , MonadFix
  , Monoid
  , Typeable
  )

#if MIN_VERSION_base(4,7,0)
instance IsList (SmallArray a) where
  type Item (SmallArray a) = a
  fromListN n l = SmallArray (fromListN n l)
  fromList l = SmallArray (fromList l)
  toList (SmallArray a) = toList a
#endif
#endif

#if HAVE_SMALL_ARRAY
data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)
  deriving Typeable
#else
newtype SmallMutableArray s a = SmallMutableArray (MutableArray s a)
  deriving (Eq, Typeable)
#endif

-- | Create a new small mutable array.
newSmallArray
  :: PrimMonad m
  => Int -- ^ size
  -> a   -- ^ initial contents
  -> m (SmallMutableArray (PrimState m) a)
#if HAVE_SMALL_ARRAY
newSmallArray (I# i#) x = primitive $ \s ->
  case newSmallArray# i# x s of
    (# s', sma# #) -> (# s', SmallMutableArray sma# #)
#else
newSmallArray n e = SmallMutableArray `liftM` newArray n e
#endif
{-# INLINE newSmallArray #-}

-- | Read the element at a given index in a mutable array.
readSmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ array
  -> Int                               -- ^ index
  -> m a
#if HAVE_SMALL_ARRAY
readSmallArray (SmallMutableArray sma#) (I# i#) =
  primitive $ readSmallArray# sma# i#
#else
readSmallArray (SmallMutableArray a) = readArray a
#endif
{-# INLINE readSmallArray #-}

-- | Write an element at the given idex in a mutable array.
writeSmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ array
  -> Int                               -- ^ index
  -> a                                 -- ^ new element
  -> m ()
#if HAVE_SMALL_ARRAY
writeSmallArray (SmallMutableArray sma#) (I# i#) x =
  primitive_ $ writeSmallArray# sma# i# x
#else
writeSmallArray (SmallMutableArray a) = writeArray a
#endif
{-# INLINE writeSmallArray #-}

-- | Look up an element in an immutable array.
--
-- The purpose of returning a result using a monad is to allow the caller to
-- avoid retaining references to the array. Evaluating the return value will
-- cause the array lookup to be performed, even though it may not require the
-- element of the array to be evaluated (which could throw an exception). For
-- instance:
--
-- > data Box a = Box a
-- > ...
-- >
-- > f sa = case indexSmallArrayM sa 0 of
-- >   Box x -> ...
--
-- 'x' is not a closure that references 'sa' as it would be if we instead
-- wrote:
--
-- > let x = indexSmallArray sa 0
--
-- And does not prevent 'sa' from being garbage collected.
--
-- Note that 'Identity' is not adequate for this use, as it is a newtype, and
-- cannot be evaluated without evaluating the element.
indexSmallArrayM
  :: Monad m
  => SmallArray a -- ^ array
  -> Int          -- ^ index
  -> m a
#if HAVE_SMALL_ARRAY
indexSmallArrayM (SmallArray sa#) (I# i#) =
  case indexSmallArray# sa# i# of
    (# x #) -> pure x
#else
indexSmallArrayM (SmallArray a) = indexArrayM a
#endif
{-# INLINE indexSmallArrayM #-}

-- | Look up an element in an immutable array.
indexSmallArray
  :: SmallArray a -- ^ array
  -> Int          -- ^ index
  -> a
#if HAVE_SMALL_ARRAY
indexSmallArray sa i = runIdentity $ indexSmallArrayM sa i
#else
indexSmallArray (SmallArray a) = indexArray a
#endif
{-# INLINE indexSmallArray #-}

-- | Create a copy of a slice of an immutable array.
cloneSmallArray
  :: SmallArray a -- ^ source
  -> Int          -- ^ offset
  -> Int          -- ^ length
  -> SmallArray a
#if HAVE_SMALL_ARRAY
cloneSmallArray (SmallArray sa#) (I# i#) (I# j#) =
  SmallArray (cloneSmallArray# sa# i# j#)
#else
cloneSmallArray (SmallArray a) i j = SmallArray $ cloneArray a i j
#endif
{-# INLINE cloneSmallArray #-}

-- | Create a copy of a slice of a mutable array.
cloneSmallMutableArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ source
  -> Int                               -- ^ offset
  -> Int                               -- ^ length
  -> m (SmallMutableArray (PrimState m) a)
#if HAVE_SMALL_ARRAY
cloneSmallMutableArray (SmallMutableArray sma#) (I# o#) (I# l#) =
  primitive $ \s -> case cloneSmallMutableArray# sma# o# l# s of
    (# s', smb# #) -> (# s', SmallMutableArray smb# #)
#else
cloneSmallMutableArray (SmallMutableArray ma) i j =
  SmallMutableArray `liftM` cloneMutableArray ma i j
#endif
{-# INLINE cloneSmallMutableArray #-}

-- | Create an immutable array corresponding to a slice of a mutable array.
--
-- This operation copies the portion of the array to be frozen.
freezeSmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ source
  -> Int                               -- ^ offset
  -> Int                               -- ^ length
  -> m (SmallArray a)
#if HAVE_SMALL_ARRAY
freezeSmallArray (SmallMutableArray sma#) (I# i#) (I# j#) =
  primitive $ \s -> case freezeSmallArray# sma# i# j# s of
    (# s', sa# #) -> (# s', SmallArray sa# #)
#else
freezeSmallArray (SmallMutableArray ma) i j =
  SmallArray `liftM` freezeArray ma i j
#endif
{-# INLINE freezeSmallArray #-}

-- | Render a mutable array immutable.
--
-- This operation performs no copying, so care must be taken not to modify the
-- input array after freezing.
unsafeFreezeSmallArray
  :: PrimMonad m => SmallMutableArray (PrimState m) a -> m (SmallArray a)
#if HAVE_SMALL_ARRAY
unsafeFreezeSmallArray (SmallMutableArray sma#) =
  primitive $ \s -> case unsafeFreezeSmallArray# sma# s of
    (# s', sa# #) -> (# s', SmallArray sa# #)
#else
unsafeFreezeSmallArray (SmallMutableArray ma) =
  SmallArray `liftM` unsafeFreezeArray ma
#endif
{-# INLINE unsafeFreezeSmallArray #-}

-- | Create a mutable array corresponding to a slice of an immutable array.
--
-- This operation copies the portion of the array to be thawed.
thawSmallArray
  :: PrimMonad m
  => SmallArray a -- ^ source
  -> Int          -- ^ offset
  -> Int          -- ^ length
  -> m (SmallMutableArray (PrimState m) a)
#if HAVE_SMALL_ARRAY
thawSmallArray (SmallArray sa#) (I# o#) (I# l#) =
  primitive $ \s -> case thawSmallArray# sa# o# l# s of
    (# s', sma# #) -> (# s', SmallMutableArray sma# #)
#else
thawSmallArray (SmallArray a) off len =
  SmallMutableArray `liftM` thawArray a off len
#endif
{-# INLINE thawSmallArray #-}

-- | Render an immutable array mutable.
--
-- This operation performs no copying, so care must be taken with its use.
unsafeThawSmallArray
  :: PrimMonad m => SmallArray a -> m (SmallMutableArray (PrimState m) a)
#if HAVE_SMALL_ARRAY
unsafeThawSmallArray (SmallArray sa#) =
  primitive $ \s -> case unsafeThawSmallArray# sa# s of
    (# s', sma# #) -> (# s', SmallMutableArray sma# #)
#else
unsafeThawSmallArray (SmallArray a) = SmallMutableArray `liftM` unsafeThawArray a
#endif
{-# INLINE unsafeThawSmallArray #-}

-- | Copy a slice of an immutable array into a mutable array.
copySmallArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ destination
  -> Int                               -- ^ destination offset
  -> SmallArray a                      -- ^ source
  -> Int                               -- ^ source offset
  -> Int                               -- ^ length
  -> m ()
#if HAVE_SMALL_ARRAY
copySmallArray
  (SmallMutableArray dst#) (I# do#) (SmallArray src#) (I# so#) (I# l#) =
    primitive_ $ copySmallArray# src# so# dst# do# l#
#else
copySmallArray (SmallMutableArray dst) i (SmallArray src) = copyArray dst i src
#endif
{-# INLINE copySmallArray #-}

-- | Copy a slice of one mutable array into another.
copySmallMutableArray
  :: PrimMonad m
  => SmallMutableArray (PrimState m) a -- ^ destination
  -> Int                               -- ^ destination offset
  -> SmallMutableArray (PrimState m) a -- ^ source
  -> Int                               -- ^ source offset
  -> Int                               -- ^ length
  -> m ()
#if HAVE_SMALL_ARRAY
copySmallMutableArray
  (SmallMutableArray dst#) (I# do#)
  (SmallMutableArray src#) (I# so#)
  (I# l#) =
    primitive_ $ copySmallMutableArray# src# so# dst# do# l#
#else
copySmallMutableArray (SmallMutableArray dst) i (SmallMutableArray src) =
  copyMutableArray dst i src
#endif
{-# INLINE copySmallMutableArray #-}

sizeofSmallArray :: SmallArray a -> Int
#if HAVE_SMALL_ARRAY
sizeofSmallArray (SmallArray sa#) = I# (sizeofSmallArray# sa#)
#else
sizeofSmallArray (SmallArray a) = sizeofArray a
#endif
{-# INLINE sizeofSmallArray #-}

sizeofSmallMutableArray :: SmallMutableArray s a -> Int
#if HAVE_SMALL_ARRAY
sizeofSmallMutableArray (SmallMutableArray sa#) =
  I# (sizeofSmallMutableArray# sa#)
#else
sizeofSmallMutableArray (SmallMutableArray ma) = sizeofMutableArray ma
#endif
{-# INLINE sizeofSmallMutableArray #-}

#if HAVE_SMALL_ARRAY
die :: String -> String -> a
die fun problem = error $ "Data.Primitive.SmallArray." ++ fun ++ ": " ++ problem

emptySmallArray :: SmallArray a
emptySmallArray =
  runST $ newSmallArray 0 (die "emptySmallArray" "impossible")
            >>= unsafeFreezeSmallArray
{-# NOINLINE emptySmallArray #-}

createSmallArray
  :: Int -> a -> (forall s. SmallMutableArray s a -> ST s ()) -> SmallArray a
createSmallArray 0 _ _ = emptySmallArray
createSmallArray i x k =
  runST $ newSmallArray i x >>= \sa -> k sa *> unsafeFreezeSmallArray sa
{-# INLINE createSmallArray #-}

infixl 1 ?
(?) :: (a -> b -> c) -> (b -> a -> c)
(?) = flip
{-# INLINE (?) #-}

noOp :: a -> ST s ()
noOp = const $ pure ()

instance Eq a => Eq (SmallArray a) where
  sa1 == sa2 = length sa1 == length sa2 && loop (length sa1 - 1)
   where
   loop i
     | i < 0     = True
     | otherwise = indexSmallArray sa1 i == indexSmallArray sa2 i && loop (i-1)

instance Eq (SmallMutableArray s a) where
  SmallMutableArray sma1# == SmallMutableArray sma2# =
    isTrue# (sameSmallMutableArray# sma1# sma2#)

instance Ord a => Ord (SmallArray a) where
  compare sl sr = fix ? 0 $ \go i ->
    if i < l
      then compare (indexSmallArray sl i) (indexSmallArray sr i) <> go (i+1)
      else compare (length sl) (length sr)
   where l = length sl `min` length sr

instance Foldable SmallArray where
  foldr f z sa = fix ? 0 $ \go i ->
    if i < length sa
      then f (indexSmallArray sa i) (go $ i+1)
      else z
  {-# INLINE foldr #-}

  foldr' f z sa = fix ? z ? length sa - 1 $ \go acc i ->
    if i < 0
      then acc
      else go (f (indexSmallArray sa i) acc) (i-1)
  {-# INLINE foldr' #-}

  foldl f z sa = fix ? length sa - 1 $ \go i ->
    if i < 0
      then z
      else f (go $ i-1) $ indexSmallArray sa i
  {-# INLINE foldl #-}

  foldl' f z sa = fix ? z ? 0 $ \go acc i ->
    if i < length sa
      then go (f acc $ indexSmallArray sa i) (i+1)
      else acc
  {-# INLINE foldl' #-}

  foldr1 f sa
    | sz == 0   = die "foldr1" "empty list"
    | otherwise = fix ? 0 $ \go i ->
        if i < sz-1
          then f (indexSmallArray sa i) (go $ i+1)
          else indexSmallArray sa $ sz-1
   where sz = sizeofSmallArray sa
  {-# INLINE foldr1 #-}

  foldl1 f sa
    | sz == 0   = die "foldl1" "empty list"
    | otherwise = fix ? sz-1 $ \go i ->
        if i < 1
        then indexSmallArray sa 0
        else f (go $ i-1) (indexSmallArray sa i)
   where sz = sizeofSmallArray sa
  {-# INLINE foldl1 #-}

  null sa = sizeofSmallArray sa == 0
  {-# INLINE null #-}

  length = sizeofSmallArray
  {-# INLINE length #-}

instance Traversable SmallArray where
  traverse f sa = fromListN l <$> traverse (f . indexSmallArray sa) [0..l-1]
   where l = length sa

instance Functor SmallArray where
  fmap f sa = createSmallArray (length sa) (die "fmap" "impossible") $ \smb ->
    fix ? 0 $ \go i ->
      when (i < length sa) $
        writeSmallArray smb i (f $ indexSmallArray sa i) *> go (i+1)
  {-# INLINE fmap #-}

  x <$ sa = createSmallArray (length sa) x noOp

instance Applicative SmallArray where
  pure x = createSmallArray 1 x noOp

  sa *> sb = createSmallArray (la*lb) (die "*>" "impossible") $ \smb ->
    fix ? 0 $ \go i ->
      when (i < la) $
        copySmallArray smb 0 sb 0 lb *> go (i+1)
   where
   la = length sa ; lb = length sb

  sa <* sb = createSmallArray (la*lb) (indexSmallArray sa $ la-1) $ \sma ->
    fix ? 0 $ \outer i -> when (i < la-1) $ do
      let a = indexSmallArray sa i
      fix ? 0 $ \inner j ->
        when (j < lb) $
          writeSmallArray sma (la*i + j) a *> inner (j+1)
      outer $ i+1
   where
   la = length sa ; lb = length sb

  sf <*> sx = createSmallArray (lf*lx) (die "<*>" "impossible") $ \smb ->
    fix ? 0 $ \outer i -> when (i < lf) $ do
      let f = indexSmallArray sf i
      fix ? 0 $ \inner j ->
        when (j < lx) $
          writeSmallArray smb (lf*i + j) (f $ indexSmallArray sx j)
            *> inner (j+1)
      outer $ i+1
   where
   lf = length sf ; lx = length sx

instance Alternative SmallArray where
  empty = emptySmallArray

  sl <|> sr =
    createSmallArray (length sl + length sr) (die "<|>" "impossible") $ \sma ->
      copySmallArray sma 0 sl 0 (length sl)
        *> copySmallArray sma (length sl) sr 0 (length sr)

  many sa | null sa   = pure []
          | otherwise = die "many" "infinite arrays are not well defined"

  some sa | null sa   = emptySmallArray
          | otherwise = die "some" "infinite arrays are not well defined"

instance Monad SmallArray where
  return = pure
  (>>) = (*>)

  sa >>= f = collect 0 [] (la-1)
   where
   la = length sa
   collect sz stk i
     | i < 0 = createSmallArray sz (die ">>=" "impossible") $ fill 0 stk
     | otherwise = let sb = f $ indexSmallArray sa i in
         collect (sz + length sb) (sb:stk) (i-1)

   fill _   [      ] _   = return ()
   fill off (sb:sbs) smb =
     copySmallArray smb off sb 0 (length sb)
       *> fill (off + length sb) sbs smb

  fail _ = emptySmallArray

instance MonadPlus SmallArray where
  mzero = empty
  mplus = (<|>)

zipW :: String -> (a -> b -> c) -> SmallArray a -> SmallArray b -> SmallArray c
zipW nm = \f sa sb -> let mn = length sa `min` length sb in
  createSmallArray mn (die nm "impossible") $ \mc ->
    fix ? 0 $ \go i -> when (i < mn) $
      writeSmallArray mc i (f (indexSmallArray sa i) (indexSmallArray sb i))
        *> go (i+1)
{-# INLINE zipW #-}

instance MonadZip SmallArray where
  mzip = zipW "mzip" (,)
  mzipWith = zipW "mzipWith"
  {-# INLINE mzipWith #-}
  munzip sab = runST $ do
    let sz = length sab
    sma <- newSmallArray sz $ die "munzip" "impossible"
    smb <- newSmallArray sz $ die "munzip" "impossible"
    fix ? 0 $ \go i ->
      when (i < sz) $ case indexSmallArray sab i of
        (x, y) -> do writeSmallArray sma i x
                     writeSmallArray smb i y
                     go $ i+1
    (,) <$> unsafeFreezeSmallArray sma
        <*> unsafeFreezeSmallArray smb

instance MonadFix SmallArray where
  mfix f = fromList . mfix $ toList . f

#if MIN_VERSION_base(4,9,0)
instance Sem.Semigroup (SmallArray a) where
  (<>) = (<|>)
  sconcat = mconcat . toList
#endif

instance Monoid (SmallArray a) where
  mempty = empty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<|>)
#endif
  mconcat sas = createSmallArray n (die "mconcat" "impossible") $ \sma ->
    fix ? 0 ? sas $ \go off l -> case l of
      [] -> return ()
      sa:stk -> copySmallArray sma off sa 0 (length sa) *> go (off+1) stk
   where n = sum . fmap length $ sas

instance IsList (SmallArray a) where
  type Item (SmallArray a) = a
  fromListN n l =
    createSmallArray n (die "fromListN" "mismatched size and list") $ \sma ->
      fix ? 0 ? l $ \go i li -> case li of
        [] -> pure ()
        x:xs -> writeSmallArray sma i x *> go (i+1) xs
  fromList l = fromListN (length l) l
  toList sa = indexSmallArray sa <$> [0 .. length sa - 1]

instance Show a => Show (SmallArray a) where
  showsPrec p sa = showParen (p > 10) $
    showString "fromListN " . shows (length sa) . showString " "
      . shows (toList sa)

instance Read a => Read (SmallArray a) where
  readPrec = parens . prec 10 $ do
    Symbol "fromListN" <- lexP
    Number nu <- lexP
    n <- maybe empty pure $ numberToInteger nu
    fromListN (fromIntegral n) <$> readPrec

smallArrayDataType :: DataType
smallArrayDataType =
  mkDataType "Data.Primitive.SmallArray.SmallArray" [fromListConstr]

fromListConstr :: Constr
fromListConstr = mkConstr smallArrayDataType "fromList" [] Prefix

instance Data a => Data (SmallArray a) where
  toConstr _ = fromListConstr
  dataTypeOf _ = smallArrayDataType
  gunfold k z c = case constrIndex c of
    1 -> k (z fromList)
    _ -> die "gunfold" "SmallArray"
  gfoldl f z m = z fromList `f` toList m

instance (Typeable s, Typeable a) => Data (SmallMutableArray s a) where
  toConstr _ = die "toConstr" "SmallMutableArray"
  gunfold _ _ = die "gunfold" "SmallMutableArray"
  dataTypeOf _ = mkNoRepType "Data.Primitive.SmallArray.SmallMutableArray"
#endif
