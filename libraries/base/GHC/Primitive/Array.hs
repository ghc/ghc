{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Primitive.Array
  ( Array(..)
  , MutableArray(..)
  , newArray
  , readArray
  , writeArray
  , indexArray
  , indexArray##
  , indexArrayM
  , unsafeFreezeArray
  , replicateArrayP
  , sizeofArray
  , sizeofMutableArray
  , traverseArrayP
  , traverseMutableArray
  , emptyArray
  , arrayFromList
  , arrayFromListN
  , mutableArrayFromList
  , singletonArray
  , consArray
  , consMutableArray
  , copyArray
  , copyMutableArray
  , deleteIndexMutableArray
  , appendArray
  , filterArray
  , partitionArray
  , mapArray'
  ) where

import Data.Foldable (Foldable(..))

import GHC.Num ((+),(*),(-))
import GHC.ST (ST,runST)
import GHC.Base
import GHC.Primitive.Monad (PrimMonad(..),primitive_)

import qualified GHC.ST as GHCST

-- Most the code in this module is copied directly from
-- the primitive library. The replicateArrayP function
-- has been added.

-- | Boxed arrays
data Array a = Array
  { array# :: Array# a }

-- | Mutable boxed arrays associated with a primitive state token.
data MutableArray s a = MutableArray
  { marray# :: MutableArray# s a }

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

-- | Execute the monadic action the given number of times and store the
-- results in a primitive array.
{-# INLINE replicateArrayP #-}
replicateArrayP :: PrimMonad m
  => Int
  -> m a
  -> m (Array a)
replicateArrayP sz f = do
  marr <- newArray sz uninitializedElement
  let go !ix = if ix < sz
        then do
          b <- f
          writeArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezeArray marr

-- | Convert a mutable array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeArray :: PrimMonad m => MutableArray (PrimState m) a -> m (Array a)
{-# INLINE unsafeFreezeArray #-}
unsafeFreezeArray arr
  = primitive (\s# -> case unsafeFreezeArray# (marray# arr) s# of
                        (# s'#, arr'# #) ->
                          let a = Array arr'#
                          in (# s'#, a #))

-- | Create a new mutable array of the specified size and initialise all
-- elements with the given value.
newArray :: PrimMonad m => Int -> a -> m (MutableArray (PrimState m) a)
{-# INLINE newArray #-}
newArray (I# n#) x = primitive
   (\s# -> case newArray# n# x s# of
             (# s'#, arr# #) ->
               let ma = MutableArray arr#
               in (# s'# , ma #))


{-# NOINLINE uninitializedElement #-}
uninitializedElement :: a
uninitializedElement = error "GHC.Primitive.Array: element not initialized"

sizeofArray :: Array a -> Int
sizeofArray a = I# (sizeofArray# (array# a))
{-# INLINE sizeofArray #-}

sizeofMutableArray :: MutableArray s a -> Int
sizeofMutableArray a = I# (sizeofMutableArray# (marray# a))
{-# INLINE sizeofMutableArray #-}

emptyArray :: Array a
emptyArray = runST $ newArray 0 undefined >>= unsafeFreezeArray
{-# NOINLINE emptyArray #-}


-- | This is the fastest, most straightforward way to traverse
-- an array, but it only works correctly with a sufficiently
-- "affine" 'PrimMonad' instance. In particular, it must only produce
-- *one* result array. 'Control.Monad.Trans.List.ListT'-transformed
-- monads, for example, will not work right at all.
traverseArrayP
  :: PrimMonad m
  => (a -> m b)
  -> Array a
  -> m (Array b)
traverseArrayP f = \ !ary ->
  let
    !sz = sizeofArray ary
    go !i !mary
      | i == sz
      = unsafeFreezeArray mary
      | otherwise
      = do
          a <- indexArrayM ary i
          b <- f a
          writeArray mary i b
          go (i + 1) mary
  in do
    mary <- newArray sz uninitializedElement
    go 0 mary
{-# INLINE traverseArrayP #-}

traverseMutableArray
  :: PrimMonad m
  => (a -> m b)
  -> MutableArray (PrimState m) a
  -> m (MutableArray (PrimState m) b)
{-# INLINE traverseMutableArray #-}
traverseMutableArray f = \ !ary ->
  let
    !sz = sizeofMutableArray ary
    go !i !mary
      | i == sz = pure mary
      | otherwise = do
          a <- readArray ary i
          b <- f a
          writeArray mary i b
          go (i + 1) mary
  in do
    mary <- newArray sz uninitializedElement
    go 0 mary

mutableArrayFromListN :: PrimMonad m => Int -> [a] -> m (MutableArray (PrimState m) a)
{-# INLINE mutableArrayFromListN #-}
mutableArrayFromListN n xs = do
  sma <- newArray n (die "mutableArrayFromListN" "uninitialized element")
  mutableArrayFromListWorker sma n xs
  pure sma

mutableArrayFromListWorker :: PrimMonad m => MutableArray (PrimState m) a -> Int -> [a] -> m ()
{-# INLINE mutableArrayFromListWorker #-}
mutableArrayFromListWorker sma n e = do
  let go !ix [] = if ix == n
        then pure ()
        else die "mutableArrayFromListWorker" "list length less than specified size"
      go !ix (x : xs) = if ix < n
        then do
          writeArray sma ix x
          go (ix + 1) xs
        else die "mutableArrayFromListWorker" "list length greater than specified size"
  go 0 e

arrayFromListN :: Int -> [a] -> Array a
{-# INLINE arrayFromListN #-}
arrayFromListN n l =
  createArray n (die "arrayFromListN" "uninitialized element") $ \sma ->
    mutableArrayFromListWorker sma n l

arrayFromList :: [a] -> Array a
{-# INLINE arrayFromList #-}
arrayFromList l = arrayFromListN (length l) l

mutableArrayFromList :: PrimMonad m => [a] -> m (MutableArray (PrimState m) a)
{-# INLINE mutableArrayFromList #-}
mutableArrayFromList l = mutableArrayFromListN (length l) l


die :: String -> String -> a
die fun problem = error $ "GHC.Primitive.Array." ++ fun ++ ": " ++ problem

-- This low-level business is designed to work with GHC's worker-wrapper
-- transformation. A lot of the time, we don't actually need an Array
-- constructor. By putting it on the outside, and being careful about
-- how we special-case the empty array, we can make GHC smarter about this.
createArray
  :: Int
  -> a
  -> (forall s. MutableArray s a -> ST s b)
  -> Array a
{-# INLINE createArray #-}
createArray n x f = runArray $ do
  mary <- newArray n x
  _ <- f mary
  pure mary


runArray
  :: (forall s. ST s (MutableArray s a))
  -> Array a
runArray m = Array (runArray# m)

runArray2
  :: (forall s. ST s (MutableArray s a, MutableArray s b))
  -> (Array a, Array b)
runArray2 m = case runArray2# m of
  (# x, y #) -> (Array x, Array y)

runArray#
  :: (forall s. ST s (MutableArray s a))
  -> Array# a
runArray# m = case runRW# $ \s ->
  case unST m s of { (# s', MutableArray mary# #) ->
  unsafeFreezeArray# mary# s'} of (# _, ary# #) -> ary#

runArray2# ::
     (forall s. ST s (MutableArray s a,MutableArray s b))
  -> (# Array# a, Array# b #)
runArray2# m = case
  ( runRW# $ \s0 ->
    case unST m s0 of
      (# s1, (MutableArray maryA#, MutableArray maryB# ) #) ->
        case unsafeFreezeArray# maryA# s1 of
          (# s2, aryA# #) -> case unsafeFreezeArray# maryB# s2 of
            (# s3, aryB# #) -> (# s3, (# aryA#, aryB# #) #)
  ) of { (# _, pair #) -> pair }

unST :: ST s a -> State# s -> (# State# s, a #)
unST (GHCST.ST f) = f

singletonArray :: a -> Array a
{-# INLINE singletonArray #-}
singletonArray a = createArray 1 a (\_ -> pure ())

consArray :: a -> Array a -> Array a
{-# INLINE consArray #-}
consArray a xs = createArray (len + 1) a $ \m -> do
  copyArray m 1 xs 0 len
  where
  len = sizeofArray xs

consMutableArray :: PrimMonad m
  => a
  -> MutableArray (PrimState m) a
  -> m (MutableArray (PrimState m) a)
{-# INLINE consMutableArray #-}
consMutableArray a xs = do
  m <- newArray (len + 1) a
  copyMutableArray m 1 xs 0 len
  pure m
  where
  len = sizeofMutableArray xs

deleteIndexMutableArray :: PrimMonad m
  => MutableArray (PrimState m) a
  -> Int
  -> m (MutableArray (PrimState m) a)
{-# INLINE deleteIndexMutableArray #-}
deleteIndexMutableArray !src !ix = do
  let len = sizeofMutableArray src
  dst <- newArray (len - 1) uninitializedElement
  copyMutableArray dst 0 src 0 ix
  copyMutableArray dst ix src (ix + 1) (len - (ix + 1))
  pure dst

-- | Copy a slice of an immutable array to a mutable array.
copyArray :: PrimMonad m
          => MutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> Array a                         -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copyArray #-}
copyArray (MutableArray dst#) (I# doff#) (Array src#) (I# soff#) (I# len#)
  = primitive_ (copyArray# src# soff# dst# doff# len#)

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
copyMutableArray (MutableArray dst#) (I# doff#)
                 (MutableArray src#) (I# soff#) (I# len#)
  = primitive_ (copyMutableArray# src# soff# dst# doff# len#)


appendArray :: Array a -> Array a -> Array a
{-# INLINE appendArray #-}
appendArray !a !b
  | sizeofArray a == 0 = b
  | sizeofArray b == 0 = a
  | otherwise =
      let !sza = sizeofArray a
          !szb = sizeofArray b
      in createArray (sza + szb) (die "<|>" "impossible") $ \ma -> do
           copyArray ma 0 a 0 sza
           copyArray ma sza b 0 szb

-- | Filter elements of an array according to a predicate.
-- filterArray ::
--      (a -> Bool)
--   -> Array a
--   -> Array a
-- {-# INLINE filterPrimArray #-}
-- filterArray p !arr = if sizeofArray arr > 0
--   then case f (indexArray arr 0) of
--     True -> 
--     False -> 
--   else emptyArray

-- | Discard elements that do not satisfy the predicate.
filterArray :: (a -> Bool) -> Array a -> Array a
{-# INLINE filterArray #-}
filterArray p !arr = runArray $ do
  let !sz = sizeofArray arr
  marr <- newArray sz (die "filterArray" "impossible")
  let go !ixSrc !ixDst = if ixSrc < sz
        then do
          a <- indexArrayM arr ixSrc
          if p a
            then do
              writeArray marr ixDst a
              go (ixSrc + 1) (ixDst + 1)
            else go (ixSrc + 1) ixDst
        else return ixDst
  dstLen <- go 0 0
  internalResizeMutableArray marr dstLen

partitionArray :: (a -> Bool) -> Array a -> (Array a, Array a)
{-# INLINE partitionArray #-}
partitionArray p !arr = runArray2 $ do
  let !sz = sizeofArray arr
  marrA <- newArray sz (die "partitionArray" "impossible")
  marrB <- newArray sz (die "partitionArray" "impossible")
  let go !ixSrc !ixDstA !ixDstB = if ixSrc < sz
        then do
          a <- indexArrayM arr ixSrc
          if p a
            then do
              writeArray marrA ixDstA a
              go (ixSrc + 1) (ixDstA + 1) ixDstB
            else do
              writeArray marrB ixDstB a
              go (ixSrc + 1) ixDstA (ixDstB + 1)
        else return (ixDstA,ixDstB)
  (dstLenA,dstLenB) <- go 0 0 0
  arrA <- internalResizeMutableArray marrA dstLenA
  arrB <- internalResizeMutableArray marrB dstLenB
  pure (arrA,arrB)

-- Copy elements of an array into a new smaller array. The length
-- must be less than or equal to the length of the original array but
-- this precondition is not checked. The original mutable array must
-- not be reused after being passed to this function, since it may
-- be aliased. This function is not exported.
internalResizeMutableArray :: MutableArray s a -> Int -> ST s (MutableArray s a)
internalResizeMutableArray !a !len = if len == sizeofMutableArray a
  then return a
  else do
    b <- newArray len (die "internalResizeMutableArray" "impossible")
    copyMutableArray b 0 a 0 len
    return b
{-# INLINE internalResizeMutableArray #-}

instance Foldable Array where
  -- Note: we perform the array lookups eagerly so we won't
  -- create thunks to perform lookups even if GHC can't see
  -- that the folding function is strict.
  foldr f = \z !ary ->
    let
      !sz = sizeofArray ary
      go i
        | i == sz = z
        | (# x #) <- indexArray## ary i
        = f x (go (i+1))
    in go 0
  {-# INLINE foldr #-}
  foldl f = \z !ary ->
    let
      go i
        | i < 0 = z
        | (# x #) <- indexArray## ary i
        = f (go (i-1)) x
    in go (sizeofArray ary - 1)
  {-# INLINE foldl #-}
  foldr1 f = \ !ary ->
    let
      !sz = sizeofArray ary - 1
      go i =
        case indexArray## ary i of
          (# x #) | i == sz -> x
                  | otherwise -> f x (go (i+1))
    in if sz < 0
       then die "foldr1" "empty array"
       else go 0
  {-# INLINE foldr1 #-}
  foldl1 f = \ !ary ->
    let
      !sz = sizeofArray ary - 1
      go i =
        case indexArray## ary i of
          (# x #) | i == 0 -> x
                  | otherwise -> f (go (i - 1)) x
    in if sz < 0
       then die "foldl1" "empty array"
       else go sz
  {-# INLINE foldl1 #-}
  foldr' f = \z !ary ->
    let
      go i !acc
        | i == -1 = acc
        | (# x #) <- indexArray## ary i
        = go (i-1) (f x acc)
    in go (sizeofArray ary - 1) z
  {-# INLINE foldr' #-}
  foldl' f = \z !ary ->
    let
      !sz = sizeofArray ary
      go i !acc
        | i == sz = acc
        | (# x #) <- indexArray## ary i
        = go (i+1) (f acc x)
    in go 0 z
  {-# INLINE foldl' #-}
  null a = sizeofArray a == 0
  {-# INLINE null #-}
  length = sizeofArray
  {-# INLINE length #-}
  maximum ary | sz == 0   = die "maximum" "empty array"
              | (# frst #) <- indexArray## ary 0
              = go 1 frst
   where
     sz = sizeofArray ary
     go i !e
       | i == sz = e
       | (# x #) <- indexArray## ary i
       = go (i+1) (max e x)
  {-# INLINE maximum #-}
  minimum ary | sz == 0   = die "minimum" "empty array"
              | (# frst #) <- indexArray## ary 0
              = go 1 frst
   where sz = sizeofArray ary
         go i !e
           | i == sz = e
           | (# x #) <- indexArray## ary i
           = go (i+1) (min e x)
  {-# INLINE minimum #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}


-- | Read a value from the immutable array at the given index, returning
-- the result in an unboxed unary tuple. This is currently used to implement
-- folds.
indexArray## :: Array a -> Int -> (# a #)
indexArray## arr (I# i) = indexArray# (array# arr) i
{-# INLINE indexArray## #-}

-- | Strict map over the elements of the array.
mapArray' :: (a -> b) -> Array a -> Array b
mapArray' f a =
  createArray (sizeofArray a) (die "mapArray'" "impossible") $ \mb ->
    let go i | i == sizeofArray a
             = return ()
             | otherwise
             = do x <- indexArrayM a i
                  -- We use indexArrayM here so that we will perform the
                  -- indexing eagerly even if f is lazy.
                  let !y = f x
                  writeArray mb i y >> go (i+1)
     in go 0
{-# INLINE mapArray' #-}
