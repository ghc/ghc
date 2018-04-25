{-# LANGUAGE CPP, MagicHash, UnboxedTuples, DeriveDataTypeable, BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Primitive.Array
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Primitive arrays of boxed values.
--

module Data.Primitive.Array (
  Array(..), MutableArray(..),

  newArray, readArray, writeArray, indexArray, indexArrayM,
  freezeArray, thawArray,
  unsafeFreezeArray, unsafeThawArray, sameMutableArray,
  copyArray, copyMutableArray,
  cloneArray, cloneMutableArray,
  sizeofArray, sizeofMutableArray,
  fromListN, fromList
) where

import Control.Monad.Primitive

import GHC.Base  ( Int(..) )
import GHC.Prim
import qualified GHC.Exts as Exts
#if (MIN_VERSION_base(4,7,0))
import GHC.Exts (fromListN, fromList)
#endif

import Data.Typeable ( Typeable )
import Data.Data
  (Data(..), DataType, mkDataType, Constr, mkConstr, Fixity(..), constrIndex)
import Data.Primitive.Internal.Compat ( isTrue#, mkNoRepType )

import Control.Monad.ST(ST,runST)

import Control.Applicative
import Control.Monad (MonadPlus(..))
import Control.Monad.Fix
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip
#endif
import Data.Foldable (Foldable(..), toList)
#if !(MIN_VERSION_base(4,8,0))
import Data.Traversable (Traversable(..))
import Data.Monoid
#endif
#if MIN_VERSION_base(4,9,0)
import qualified Data.Foldable as F
import Data.Semigroup
#endif

import Text.ParserCombinators.ReadP

-- | Boxed arrays
data Array a = Array
             { array# :: Array# a
#if (__GLASGOW_HASKELL__ < 702)
             , sizeofArray :: {-# UNPACK #-} !Int
#endif
             }
  deriving ( Typeable )

-- | Mutable boxed arrays associated with a primitive state token.
data MutableArray s a = MutableArray
                      { marray# :: MutableArray# s a
#if (__GLASGOW_HASKELL__ < 702)
                      , sizeofMutableArray :: {-# UNPACK #-} !Int
#endif
                      }
  deriving ( Typeable )

#if (__GLASGOW_HASKELL__ >= 702)
sizeofArray :: Array a -> Int
sizeofArray a = I# (sizeofArray# (array# a))
{-# INLINE sizeofArray #-}

sizeofMutableArray :: MutableArray s a -> Int
sizeofMutableArray a = I# (sizeofMutableArray# (marray# a))
{-# INLINE sizeofMutableArray #-}
#endif

-- | Create a new mutable array of the specified size and initialise all
-- elements with the given value.
newArray :: PrimMonad m => Int -> a -> m (MutableArray (PrimState m) a)
{-# INLINE newArray #-}
newArray (I# n#) x = primitive
   (\s# -> case newArray# n# x s# of
             (# s'#, arr# #) ->
               let ma = MutableArray arr#
#if (__GLASGOW_HASKELL__ < 702)
                          (I# n#)
#endif
               in (# s'# , ma #))

-- | Read a value from the array at the given index.
readArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> m a
{-# INLINE readArray #-}
readArray arr (I# i#) = primitive (readArray# (marray# arr) i#)

-- | Write a value to the array at the given index.
writeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> a -> m ()
{-# INLINE writeArray #-}
writeArray arr (I# i#) x = primitive_ (writeArray# (marray# arr) i# x)

-- | Read a value from the immutable array at the given index.
indexArray :: Array a -> Int -> a
{-# INLINE indexArray #-}
indexArray arr (I# i#) = case indexArray# (array# arr) i# of (# x #) -> x

-- | Monadically read a value from the immutable array at the given index.
-- This allows us to be strict in the array while remaining lazy in the read
-- element which is very useful for collective operations. Suppose we want to
-- copy an array. We could do something like this:
--
-- > copy marr arr ... = do ...
-- >                        writeArray marr i (indexArray arr i) ...
-- >                        ...
--
-- But since primitive arrays are lazy, the calls to 'indexArray' will not be
-- evaluated. Rather, @marr@ will be filled with thunks each of which would
-- retain a reference to @arr@. This is definitely not what we want!
--
-- With 'indexArrayM', we can instead write
--
-- > copy marr arr ... = do ...
-- >                        x <- indexArrayM arr i
-- >                        writeArray marr i x
-- >                        ...
--
-- Now, indexing is executed immediately although the returned element is
-- still not evaluated.
--
indexArrayM :: Monad m => Array a -> Int -> m a
{-# INLINE indexArrayM #-}
indexArrayM arr (I# i#)
  = case indexArray# (array# arr) i# of (# x #) -> return x

-- | Create an immutable copy of a slice of an array.
--
-- This operation makes a copy of the specified section, so it is safe to
-- continue using the mutable array afterward.
freezeArray
  :: PrimMonad m
  => MutableArray (PrimState m) a -- ^ source
  -> Int                          -- ^ offset
  -> Int                          -- ^ length
  -> m (Array a)
{-# INLINE freezeArray #-}
#if (__GLASGOW_HASKELL__ >= 702)
freezeArray (MutableArray ma#) (I# off#) (I# len#) =
  primitive $ \s -> case freezeArray# ma# off# len# s of
    (# s', a# #) -> (# s', Array a# #)
#else
freezeArray src off len = do
  dst <- newArray len (die "freezeArray" "impossible")
  copyMutableArray dst 0 src off len
  unsafeFreezeArray dst
#endif

-- | Convert a mutable array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeArray :: PrimMonad m => MutableArray (PrimState m) a -> m (Array a)
{-# INLINE unsafeFreezeArray #-}
unsafeFreezeArray arr
  = primitive (\s# -> case unsafeFreezeArray# (marray# arr) s# of
                        (# s'#, arr'# #) ->
                          let a = Array arr'#
#if (__GLASGOW_HASKELL__ < 702)
                                    (sizeofMutableArray arr)
#endif
                          in (# s'#, a #))

-- | Create a mutable array from a slice of an immutable array.
--
-- This operation makes a copy of the specified slice, so it is safe to use the
-- immutable array afterward.
thawArray
  :: PrimMonad m
  => Array a -- ^ source
  -> Int     -- ^ offset
  -> Int     -- ^ length
  -> m (MutableArray (PrimState m) a)
{-# INLINE thawArray #-}
#if (__GLASGOW_HASKELL__ >= 702)
thawArray (Array a#) (I# off#) (I# len#) =
  primitive $ \s -> case thawArray# a# off# len# s of
    (# s', ma# #) -> (# s', MutableArray ma# #)
#else
thawArray src off len = do
  dst <- newArray len (die "thawArray" "impossible")
  copyArray dst 0 src off len
  return dst
#endif

-- | Convert an immutable array to an mutable one without copying. The
-- immutable array should not be used after the conversion.
unsafeThawArray :: PrimMonad m => Array a -> m (MutableArray (PrimState m) a)
{-# INLINE unsafeThawArray #-}
unsafeThawArray a
  = primitive (\s# -> case unsafeThawArray# (array# a) s# of
                        (# s'#, arr'# #) ->
                          let ma = MutableArray arr'#
#if (__GLASGOW_HASKELL__ < 702)
                                     (sizeofArray a)
#endif
                          in (# s'#, ma #))

-- | Check whether the two arrays refer to the same memory block.
sameMutableArray :: MutableArray s a -> MutableArray s a -> Bool
{-# INLINE sameMutableArray #-}
sameMutableArray arr brr
  = isTrue# (sameMutableArray# (marray# arr) (marray# brr))

-- | Copy a slice of an immutable array to a mutable array.
copyArray :: PrimMonad m
          => MutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> Array a                         -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copyArray #-}
#if __GLASGOW_HASKELL__ > 706
-- NOTE: copyArray# and copyMutableArray# are slightly broken in GHC 7.6.* and earlier
copyArray (MutableArray dst#) (I# doff#) (Array src#) (I# soff#) (I# len#)
  = primitive_ (copyArray# src# soff# dst# doff# len#)
#else
copyArray !dst !doff !src !soff !len = go 0
  where
    go i | i < len = do
                       x <- indexArrayM src (soff+i)
                       writeArray dst (doff+i) x
                       go (i+1)
         | otherwise = return ()
#endif

-- | Copy a slice of a mutable array to another array. The two arrays may
-- not be the same.
copyMutableArray :: PrimMonad m
          => MutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> MutableArray (PrimState m) a    -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copyMutableArray #-}
#if __GLASGOW_HASKELL__ >= 706
-- NOTE: copyArray# and copyMutableArray# are slightly broken in GHC 7.6.* and earlier
copyMutableArray (MutableArray dst#) (I# doff#)
                 (MutableArray src#) (I# soff#) (I# len#)
  = primitive_ (copyMutableArray# src# soff# dst# doff# len#)
#else
copyMutableArray !dst !doff !src !soff !len = go 0
  where
    go i | i < len = do
                       x <- readArray src (soff+i)
                       writeArray dst (doff+i) x
                       go (i+1)
         | otherwise = return ()
#endif

-- | Return a newly allocated Array with the specified subrange of the
-- provided Array. The provided Array should contain the full subrange
-- specified by the two Ints, but this is not checked.
cloneArray :: Array a -- ^ source array
           -> Int     -- ^ offset into destination array
           -> Int     -- ^ number of elements to copy
           -> Array a
{-# INLINE cloneArray #-}
#if __GLASGOW_HASKELL__ >= 702
cloneArray (Array arr#) (I# off#) (I# len#)
  = case cloneArray# arr# off# len# of arr'# -> Array arr'#
#else
cloneArray arr off len = runST $ do
    marr2 <- newArray len $ die "cloneArray" "impossible"
    copyArray marr2 0 arr off len
    unsafeFreezeArray marr2
#endif

-- | Return a newly allocated MutableArray. with the specified subrange of
-- the provided MutableArray. The provided MutableArray should contain the
-- full subrange specified by the two Ints, but this is not checked.
cloneMutableArray :: PrimMonad m
        => MutableArray (PrimState m) a -- ^ source array
        -> Int                          -- ^ offset into destination array
        -> Int                          -- ^ number of elements to copy
        -> m (MutableArray (PrimState m) a)
{-# INLINE cloneMutableArray #-}
#if __GLASGOW_HASKELL__ >= 702
cloneMutableArray (MutableArray arr#) (I# off#) (I# len#) = primitive
   (\s# -> case cloneMutableArray# arr# off# len# s# of
             (# s'#, arr'# #) -> (# s'#, MutableArray arr'# #))
#else
cloneMutableArray marr off len = do
        marr2 <- newArray len $ die "cloneMutableArray" "impossible"
        let go !i !j c
                | c >= len = return marr2
                | otherwise = do
                    b <- readArray marr i
                    writeArray marr2 j b
                    go (i+1) (j+1) (c+1)
        go off 0 0
#endif

emptyArray :: Array a
emptyArray =
  runST $ newArray 0 (die "emptyArray" "impossible") >>= unsafeFreezeArray
{-# NOINLINE emptyArray #-}

createArray
  :: Int
  -> a
  -> (forall s. MutableArray s a -> ST s ())
  -> Array a
createArray 0 _ _ = emptyArray
createArray n x f = runST $ do
  ma <- newArray n x
  f ma
  unsafeFreezeArray ma

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.Array." ++ fun ++ ": " ++ problem

instance Eq a => Eq (Array a) where
  a1 == a2 = sizeofArray a1 == sizeofArray a2 && loop (sizeofArray a1 - 1)
   where loop i | i < 0     = True
                | otherwise = indexArray a1 i == indexArray a2 i && loop (i-1)

instance Eq (MutableArray s a) where
  ma1 == ma2 = isTrue# (sameMutableArray# (marray# ma1) (marray# ma2))

instance Ord a => Ord (Array a) where
  compare a1 a2 = loop 0
   where
   mn = sizeofArray a1 `min` sizeofArray a2
   loop i
     | i < mn    = compare (indexArray a1 i) (indexArray a2 i) `mappend` loop (i+1)
     | otherwise = compare (sizeofArray a1) (sizeofArray a2)

instance Foldable Array where
  foldr f z a = go 0
   where go i | i < sizeofArray a = f (indexArray a i) (go $ i+1)
              | otherwise         = z
  {-# INLINE foldr #-}
  foldl f z a = go (sizeofArray a - 1)
   where go i | i < 0     = z
              | otherwise = f (go $ i-1) (indexArray a i)
  {-# INLINE foldl #-}
  foldr1 f a | sz < 0    = die "foldr1" "empty array"
             | otherwise = go 0
   where sz = sizeofArray a - 1
         z = indexArray a sz
         go i | i < sz    = f (indexArray a i) (go $ i+1)
              | otherwise = z
  {-# INLINE foldr1 #-}
  foldl1 f a | sz == 0   = die "foldl1" "empty array"
             | otherwise = go $ sz-1
   where sz = sizeofArray a
         z = indexArray a 0
         go i | i < 1     = f (go $ i-1) (indexArray a i)
              | otherwise = z
  {-# INLINE foldl1 #-}
#if MIN_VERSION_base(4,6,0)
  foldr' f z a = go (sizeofArray a - 1) z
   where go i !acc | i < 0     = acc
                   | otherwise = go (i-1) (f (indexArray a i) acc)
  {-# INLINE foldr' #-}
  foldl' f z a = go 0 z
   where go i !acc | i < sizeofArray a = go (i+1) (f acc $ indexArray a i)
                   | otherwise         = acc
  {-# INLINE foldl' #-}
#endif
#if MIN_VERSION_base(4,8,0)
  toList a = Exts.build $ \c z -> let
      sz = sizeofArray a
      go i | i < sz    = c (indexArray a i) (go $ i+1)
           | otherwise = z
    in go 0
  {-# INLINE toList #-}
  null a = sizeofArray a == 0
  {-# INLINE null #-}
  length = sizeofArray
  {-# INLINE length #-}
  maximum a | sz == 0   = die "maximum" "empty array"
            | otherwise = go 1 (indexArray a 0)
   where sz = sizeofArray a
         go i !e | i < sz    = go (i+1) (max e $ indexArray a i)
                 | otherwise = e
  {-# INLINE maximum #-}
  minimum a | sz == 0   = die "minimum" "empty array"
            | otherwise = go 1 (indexArray a 0)
   where sz = sizeofArray a
         go i !e | i < sz    = go (i+1) (min e $ indexArray a i)
                 | otherwise = e
  {-# INLINE minimum #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}
#endif

instance Traversable Array where
  traverse f a =
    fromListN (sizeofArray a)
      <$> traverse (f . indexArray a) [0 .. sizeofArray a - 1]

#if MIN_VERSION_base(4,7,0)
instance Exts.IsList (Array a) where
  type Item (Array a) = a
  fromListN n l =
    createArray n (die "fromListN" "mismatched size and list") $ \mi ->
      let go i (x:xs) = writeArray mi i x >> go (i+1) xs
          go _ [    ] = return ()
       in go 0 l
  fromList l = Exts.fromListN (length l) l
  toList = toList
#else
fromListN :: Int -> [a] -> Array a
fromListN n l =
  createArray n (die "fromListN" "mismatched size and list") $ \mi ->
    let go i (x:xs) = writeArray mi i x >> go (i+1) xs
        go _ [    ] = return ()
     in go 0 l

fromList :: [a] -> Array a
fromList l = fromListN (length l) l
#endif

instance Functor Array where
  fmap f a =
    createArray (sizeofArray a) (die "fmap" "impossible") $ \mb ->
      let go i | i < sizeofArray a = return ()
               | otherwise         = writeArray mb i (f $ indexArray a i)
                                  >> go (i+1)
       in go 0
#if MIN_VERSION_base(4,8,0)
  e <$ a = runST $ newArray (sizeofArray a) e >>= unsafeFreezeArray
#endif

instance Applicative Array where
  pure x = runST $ newArray 1 x >>= unsafeFreezeArray
  ab <*> a = runST $ do
    mb <- newArray (szab*sza) $ die "<*>" "impossible"
    let go1 i
          | i < szab  = go2 (i*sza) (indexArray ab i) 0 >> go1 (i+1)
          | otherwise = return ()
        go2 off f j
          | j < sza   = writeArray mb (off + j) (f $ indexArray a j)
          | otherwise = return ()
    go1 0
    unsafeFreezeArray mb
   where szab = sizeofArray ab ; sza = sizeofArray a
  a *> b = createArray (sza*szb) (die "*>" "impossible") $ \mb ->
    let go i | i < sza   = copyArray mb (i * szb) b 0 szb
             | otherwise = return ()
     in go 0
   where sza = sizeofArray a ; szb = sizeofArray b
  a <* b = createArray (sza*szb) (die "<*" "impossible") $ \ma ->
    let fill off i e | i < szb   = writeArray ma (off+i) e >> fill off (i+1) e
                     | otherwise = return ()
        go i | i < sza   = fill (i*szb) 0 (indexArray a i) >> go (i+1)
             | otherwise = return ()
     in go 0
   where sza = sizeofArray a ; szb = sizeofArray b

instance Alternative Array where
  empty = emptyArray
  a1 <|> a2 = createArray (sza1 + sza2) (die "<|>" "impossible") $ \ma ->
    copyArray ma 0 a1 0 sza1 >> copyArray ma sza1 a2 0 sza2
   where sza1 = sizeofArray a1 ; sza2 = sizeofArray a2
  some a | sizeofArray a == 0 = emptyArray
         | otherwise = die "some" "infinite arrays are not well defined"
  many a | sizeofArray a == 0 = pure []
         | otherwise = die "many" "infinite arrays are not well defined"

instance Monad Array where
  return = pure
  (>>) = (*>)
  a >>= f = push 0 [] (sizeofArray a - 1)
   where
   push !sz bs i
     | i < 0 = build sz bs
     | otherwise = let b = f $ indexArray a i
                    in push (sz + sizeofArray b) (b:bs) (i+1)

   build sz stk = createArray sz (die ">>=" "impossible") $ \mb ->
     let go off (b:bs) = copyArray mb off b 0 (sizeofArray b) >> go (off + sizeofArray b) bs
         go _   [    ] = return ()
      in go 0 stk
  fail _ = empty

instance MonadPlus Array where
  mzero = empty
  mplus = (<|>)

zipW :: String -> (a -> b -> c) -> Array a -> Array b -> Array c
zipW s f aa ab = createArray mn (die s "impossible") $ \mc ->
  let go i
        | i < mn    = writeArray mc i (f (indexArray aa i) (indexArray ab i))
                   >> go (i+1)
        | otherwise = return ()
   in go 0
 where mn = sizeofArray aa `min` sizeofArray ab
{-# INLINE zipW #-}

#if MIN_VERSION_base(4,4,0)
instance MonadZip Array where
  mzip aa ab = zipW "mzip" (,) aa ab
  mzipWith f aa ab = zipW "mzipWith" f aa ab
  munzip aab = runST $ do
    let sz = sizeofArray aab
    ma <- newArray sz (die "munzip" "impossible")
    mb <- newArray sz (die "munzip" "impossible")
    let go i | i < sz = do
          let (a, b) = indexArray aab i
          writeArray ma i a
          writeArray mb i b
          go (i+1)
        go _ = return ()
    go 0
    (,) <$> unsafeFreezeArray ma <*> unsafeFreezeArray mb
#endif

instance MonadFix Array where
  mfix f = let l = mfix (toList . f) in fromListN (length l) l

#if MIN_VERSION_base(4,9,0)
instance Semigroup (Array a) where
  (<>) = (<|>)
  sconcat = mconcat . F.toList
#endif

instance Monoid (Array a) where
  mempty = empty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<|>)
#endif
  mconcat l = createArray sz (die "mconcat" "impossible") $ \ma ->
    let go !_  [    ] = return ()
        go off (a:as) =
          copyArray ma off a 0 (sizeofArray a) >> go (off + sizeofArray a) as
     in go 0 l
   where sz = sum . fmap sizeofArray $ l

instance Show a => Show (Array a) where
  showsPrec p a = showParen (p > 10) $
    showString "fromListN " . shows (sizeofArray a) . showString " "
      . shows (toList a)

instance Read a => Read (Array a) where
  readsPrec p = readParen (p > 10) . readP_to_S $ do
    () <$ string "fromListN"
    skipSpaces
    n <- readS_to_P reads
    skipSpaces
    l <- readS_to_P reads
    return $ fromListN n l

arrayDataType :: DataType
arrayDataType = mkDataType "Data.Primitive.Array.Array" [fromListConstr]

fromListConstr :: Constr
fromListConstr = mkConstr arrayDataType "fromList" [] Prefix

instance Data a => Data (Array a) where
  toConstr _ = fromListConstr
  dataTypeOf _ = arrayDataType
  gunfold k z c = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  gfoldl f z m = z fromList `f` toList m

instance (Typeable s, Typeable a) => Data (MutableArray s a) where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Primitive.Array.MutableArray"
