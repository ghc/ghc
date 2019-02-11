{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Primitive.SmallArray
  ( SmallArray(..)
  , SmallMutableArray(..)
  , newSmallArray
  , readSmallArray
  , writeSmallArray
  , indexSmallArray
  , indexSmallArray##
  , indexSmallArrayM
  , unsafeFreezeSmallArray
  , replicateSmallArrayP
  , sizeofSmallArray
  , sizeofSmallMutableArray
  , traverseSmallArrayP
  , traverseSmallMutableArray
  , emptySmallArray
  , smallArrayFromList
  , smallArrayFromListN
  , smallMutableArrayFromList
  , singletonSmallArray
  , consSmallArray
  , consSmallMutableArray
  , copySmallArray
  , copySmallMutableArray
  , deleteIndexSmallMutableArray
  , appendSmallArray
  , filterSmallArray
  , partitionSmallArray
  , mapSmallArray'
  , foldlFilterSmallArray'
  ) where

import Data.Foldable (Foldable(..))

import GHC.Num ((+),(*),(-))
import GHC.ST (ST,runST)
import GHC.Base
import GHC.Primitive.Monad (PrimMonad(..),primitive_)

import qualified GHC.ST as GHCST

-- Most the code in this module is copied directly from
-- the primitive library. The replicateSmallArrayP function
-- has been added.

-- | Boxed arrays
data SmallArray a = SmallArray
  { smallArray# :: SmallArray# a }

-- | Mutable boxed arrays associated with a primitive state token.
data SmallMutableArray s a = SmallMutableArray
  { msmallArray# :: SmallMutableArray# s a }

-- | Read a value from the array at the given index.
readSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> m a
{-# INLINE readSmallArray #-}
readSmallArray arr (I# i#) = primitive (readSmallArray# (msmallArray# arr) i#)

-- | Write a value to the array at the given index.
writeSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> a -> m ()
{-# INLINE writeSmallArray #-}
writeSmallArray arr (I# i#) x = primitive_ (writeSmallArray# (msmallArray# arr) i# x)

-- | Read a value from the immutable array at the given index.
indexSmallArray :: SmallArray a -> Int -> a
{-# INLINE indexSmallArray #-}
indexSmallArray arr (I# i#) = case indexSmallArray# (smallArray# arr) i# of (# x #) -> x


-- | Monadically read a value from the immutable array at the given index.
-- This allows us to be strict in the array while remaining lazy in the read
-- element which is very useful for collective operations. Suppose we want to
-- copy an array. We could do something like this:
--
-- > copy marr arr ... = do ...
-- >                        writeSmallArray marr i (indexSmallArray arr i) ...
-- >                        ...
--
-- But since primitive arrays are lazy, the calls to 'indexArray' will not be
-- evaluated. Rather, @marr@ will be filled with thunks each of which would
-- retain a reference to @arr@. This is definitely not what we want!
--
-- With 'indexArrayM', we can instead write
--
-- > copy marr arr ... = do ...
-- >                        x <- indexSmallArrayM arr i
-- >                        writeSmallArray marr i x
-- >                        ...
--
-- Now, indexing is executed immediately although the returned element is
-- still not evaluated.
--
indexSmallArrayM :: Monad m => SmallArray a -> Int -> m a
{-# INLINE indexSmallArrayM #-}
indexSmallArrayM arr (I# i#)
  = case indexSmallArray# (smallArray# arr) i# of (# x #) -> return x

-- | Execute the monadic action the given number of times and store the
-- results in a primitive array.
{-# INLINE replicateSmallArrayP #-}
replicateSmallArrayP :: PrimMonad m
  => Int
  -> m a
  -> m (SmallArray a)
replicateSmallArrayP sz f = do
  marr <- newSmallArray sz uninitializedElement
  let go !ix = if ix < sz
        then do
          b <- f
          writeSmallArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  unsafeFreezeSmallArray marr

-- | Convert a mutable array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeSmallArray :: PrimMonad m => SmallMutableArray (PrimState m) a -> m (SmallArray a)
{-# INLINE unsafeFreezeSmallArray #-}
unsafeFreezeSmallArray arr
  = primitive (\s# -> case unsafeFreezeSmallArray# (msmallArray# arr) s# of
                        (# s'#, arr'# #) ->
                          let a = SmallArray arr'#
                          in (# s'#, a #))

-- | Create a new mutable array of the specified size and initialise all
-- elements with the given value.
newSmallArray :: PrimMonad m => Int -> a -> m (SmallMutableArray (PrimState m) a)
{-# INLINE newSmallArray #-}
newSmallArray (I# n#) x = primitive
   (\s# -> case newSmallArray# n# x s# of
             (# s'#, arr# #) ->
               let ma = SmallMutableArray arr#
               in (# s'# , ma #))


{-# NOINLINE uninitializedElement #-}
uninitializedElement :: a
uninitializedElement = error "GHC.Primitive.SmallArray: element not initialized"

sizeofSmallArray :: SmallArray a -> Int
sizeofSmallArray a = I# (sizeofSmallArray# (smallArray# a))
{-# INLINE sizeofSmallArray #-}

sizeofSmallMutableArray :: SmallMutableArray s a -> Int
sizeofSmallMutableArray a = I# (sizeofSmallMutableArray# (msmallArray# a))
{-# INLINE sizeofSmallMutableArray #-}

emptySmallArray :: SmallArray a
emptySmallArray = runST $ newSmallArray 0 undefined >>= unsafeFreezeSmallArray
{-# NOINLINE emptySmallArray #-}

-- | This is the fastest, most straightforward way to traverse
-- an array, but it only works correctly with a sufficiently
-- "affine" 'PrimMonad' instance. In particular, it must only produce
-- *one* result array. 'Control.Monad.Trans.List.ListT'-transformed
-- monads, for example, will not work right at all.
traverseSmallArrayP
  :: PrimMonad m
  => (a -> m b)
  -> SmallArray a
  -> m (SmallArray b)
traverseSmallArrayP f = \ !ary ->
  let
    !sz = sizeofSmallArray ary
    go !i !mary
      | i == sz
      = unsafeFreezeSmallArray mary
      | otherwise
      = do
          a <- indexSmallArrayM ary i
          b <- f a
          writeSmallArray mary i b
          go (i + 1) mary
  in do
    mary <- newSmallArray sz uninitializedElement
    go 0 mary
{-# INLINE traverseSmallArrayP #-}

traverseSmallMutableArray
  :: PrimMonad m
  => (a -> m b)
  -> SmallMutableArray (PrimState m) a
  -> m (SmallMutableArray (PrimState m) b)
{-# INLINE traverseSmallMutableArray #-}
traverseSmallMutableArray f = \ !ary ->
  let
    !sz = sizeofSmallMutableArray ary
    go !i !mary
      | i == sz = pure mary
      | otherwise = do
          a <- readSmallArray ary i
          b <- f a
          writeSmallArray mary i b
          go (i + 1) mary
  in do
    mary <- newSmallArray sz uninitializedElement
    go 0 mary

smallMutableArrayFromListN :: PrimMonad m => Int -> [a] -> m (SmallMutableArray (PrimState m) a)
{-# INLINE smallMutableArrayFromListN #-}
smallMutableArrayFromListN n xs = do
  sma <- newSmallArray n (die "smallMutableArrayFromListN" "uninitialized element")
  smallMutableArrayFromListWorker sma n xs
  pure sma

smallMutableArrayFromListWorker :: PrimMonad m => SmallMutableArray (PrimState m) a -> Int -> [a] -> m ()
{-# INLINE smallMutableArrayFromListWorker #-}
smallMutableArrayFromListWorker sma n e = do
  let go !ix [] = if ix == n
        then pure ()
        else die "smallMutableArrayFromListWorker" "list length less than specified size"
      go !ix (x : xs) = if ix < n
        then do
          writeSmallArray sma ix x
          go (ix + 1) xs
        else die "smallMutableArrayFromListWorker" "list length greater than specified size"
  go 0 e

smallArrayFromListN :: Int -> [a] -> SmallArray a
{-# INLINE smallArrayFromListN #-}
smallArrayFromListN n l =
  createArray n (die "smallArrayFromListN" "uninitialized element") $ \sma ->
    smallMutableArrayFromListWorker sma n l

smallArrayFromList :: [a] -> SmallArray a
{-# INLINE smallArrayFromList #-}
smallArrayFromList l = smallArrayFromListN (length l) l

smallMutableArrayFromList :: PrimMonad m => [a] -> m (SmallMutableArray (PrimState m) a)
{-# INLINE smallMutableArrayFromList #-}
smallMutableArrayFromList l = smallMutableArrayFromListN (length l) l


die :: String -> String -> a
die fun problem = error $ "GHC.Primitive.SmallArray." ++ fun ++ ": " ++ problem

-- This low-level business is designed to work with GHC's worker-wrapper
-- transformation. A lot of the time, we don't actually need an Array
-- constructor. By putting it on the outside, and being careful about
-- how we special-case the empty array, we can make GHC smarter about this.
createArray
  :: Int
  -> a
  -> (forall s. SmallMutableArray s a -> ST s b)
  -> SmallArray a
{-# INLINE createArray #-}
createArray n x f = runArray $ do
  mary <- newSmallArray n x
  _ <- f mary
  pure mary

runArrayWriter ::
     (forall s. ST s (a, SmallMutableArray s b))
  -> (a, SmallArray b)
runArrayWriter m = case runArrayWriter# m of
  (# x, y #) -> (x, SmallArray y)

runArray
  :: (forall s. ST s (SmallMutableArray s a))
  -> SmallArray a
runArray m = SmallArray (runArray# m)

runArray2
  :: (forall s. ST s (SmallMutableArray s a, SmallMutableArray s b))
  -> (SmallArray a, SmallArray b)
runArray2 m = case runArray2# m of
  (# x, y #) -> (SmallArray x, SmallArray y)

runArray#
  :: (forall s. ST s (SmallMutableArray s a))
  -> SmallArray# a
{-# INLINE runArray# #-}
runArray# m = case runRW# $ \s ->
  case unST m s of { (# s', SmallMutableArray mary# #) ->
  unsafeFreezeSmallArray# mary# s'} of (# _, ary# #) -> ary#

runArray2# ::
     (forall s. ST s (SmallMutableArray s a,SmallMutableArray s b))
  -> (# SmallArray# a, SmallArray# b #)
{-# INLINE runArray2# #-}
runArray2# m = case
  ( runRW# $ \s0 ->
    case unST m s0 of
      (# s1, (SmallMutableArray maryA#, SmallMutableArray maryB# ) #) ->
        case unsafeFreezeSmallArray# maryA# s1 of
          (# s2, aryA# #) -> case unsafeFreezeSmallArray# maryB# s2 of
            (# s3, aryB# #) -> (# s3, (# aryA#, aryB# #) #)
  ) of { (# _, pair #) -> pair }

runArrayWriter# ::
     (forall s. ST s (a,SmallMutableArray s b))
  -> (# a, SmallArray# b #)
{-# INLINE runArrayWriter# #-}
runArrayWriter# m = case
  ( runRW# $ \s0 ->
    case unST m s0 of
      (# s1, (a, SmallMutableArray maryB# ) #) ->
        case unsafeFreezeSmallArray# maryB# s1 of
          (# s2, aryB# #) -> (# s2, (# a, aryB# #) #)
  ) of { (# _, pair #) -> pair }

unST :: ST s a -> State# s -> (# State# s, a #)
unST (GHCST.ST f) = f

singletonSmallArray :: a -> SmallArray a
{-# INLINE singletonSmallArray #-}
singletonSmallArray a = createArray 1 a (\_ -> pure ())

consSmallArray :: a -> SmallArray a -> SmallArray a
{-# INLINE consSmallArray #-}
consSmallArray a xs = createArray (len + 1) a $ \m -> do
  copySmallArray m 1 xs 0 len
  where
  len = sizeofSmallArray xs

consSmallMutableArray :: PrimMonad m
  => a
  -> SmallMutableArray (PrimState m) a
  -> m (SmallMutableArray (PrimState m) a)
{-# INLINE consSmallMutableArray #-}
consSmallMutableArray a xs = do
  m <- newSmallArray (len + 1) a
  copySmallMutableArray m 1 xs 0 len
  pure m
  where
  len = sizeofSmallMutableArray xs

deleteIndexSmallMutableArray :: PrimMonad m
  => SmallMutableArray (PrimState m) a
  -> Int
  -> m (SmallMutableArray (PrimState m) a)
{-# INLINE deleteIndexSmallMutableArray #-}
deleteIndexSmallMutableArray !src !ix = do
  let len = sizeofSmallMutableArray src
  dst <- newSmallArray (len - 1) uninitializedElement
  copySmallMutableArray dst 0 src 0 ix
  copySmallMutableArray dst ix src (ix + 1) (len - (ix + 1))
  pure dst

-- | Copy a slice of an immutable array to a mutable array.
copySmallArray :: PrimMonad m
          => SmallMutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> SmallArray a                         -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copySmallArray #-}
copySmallArray (SmallMutableArray dst#) (I# doff#) (SmallArray src#) (I# soff#) (I# len#)
  = primitive_ (copySmallArray# src# soff# dst# doff# len#)

-- | Copy a slice of a mutable array to another array. The two arrays may
-- not be the same.
copySmallMutableArray :: PrimMonad m
          => SmallMutableArray (PrimState m) a    -- ^ destination array
          -> Int                             -- ^ offset into destination array
          -> SmallMutableArray (PrimState m) a    -- ^ source array
          -> Int                             -- ^ offset into source array
          -> Int                             -- ^ number of elements to copy
          -> m ()
{-# INLINE copySmallMutableArray #-}
copySmallMutableArray (SmallMutableArray dst#) (I# doff#)
                 (SmallMutableArray src#) (I# soff#) (I# len#)
  = primitive_ (copySmallMutableArray# src# soff# dst# doff# len#)


appendSmallArray :: SmallArray a -> SmallArray a -> SmallArray a
{-# INLINE appendSmallArray #-}
appendSmallArray !a !b
  | sizeofSmallArray a == 0 = b
  | sizeofSmallArray b == 0 = a
  | otherwise =
      let !sza = sizeofSmallArray a
          !szb = sizeofSmallArray b
      in createArray (sza + szb) (die "<|>" "impossible") $ \ma -> do
           copySmallArray ma 0 a 0 sza
           copySmallArray ma sza b 0 szb

-- | Filter elements of an array according to a predicate.
-- filterSmallArray ::
--      (a -> Bool)
--   -> SmallArray a
--   -> Array a
-- {-# INLINE filterPrimArray #-}
-- filterSmallArray p !arr = if sizeofSmallArray arr > 0
--   then case f (indexSmallArray arr 0) of
--     True -> 
--     False -> 
--   else emptyArray

-- | Discard elements that do not satisfy the predicate.
filterSmallArray :: (a -> Bool) -> SmallArray a -> SmallArray a
{-# INLINE filterSmallArray #-}
filterSmallArray p !arr = runArray $ do
  let !sz = sizeofSmallArray arr
  marr <- newSmallArray sz (die "filterSmallArray" "impossible")
  let go !ixSrc !ixDst = if ixSrc < sz
        then do
          a <- indexSmallArrayM arr ixSrc
          if p a
            then do
              writeSmallArray marr ixDst a
              go (ixSrc + 1) (ixDst + 1)
            else go (ixSrc + 1) ixDst
        else return ixDst
  dstLen <- go 0 0
  internalResizeMutableArray marr dstLen

-- | Strict left fold over an array. At each step, the callback
-- folds an element into the accumulator and decides if the
-- element should be preserved. 
--
-- Note: this function could be improved to avoid allocating
-- when the resulting array ends up empty. 
foldlFilterSmallArray' ::
     (b -> a -> (Bool,b))
  -> b
  -> SmallArray a
  -> (b, SmallArray a)
foldlFilterSmallArray' p !b0 !arr = runArrayWriter $ do
  let !sz = sizeofSmallArray arr
  marr <- newSmallArray sz (die "foldlFilterSmallArray'" "impossible")
  let go !ixSrc !ixDst !b = if ixSrc < sz
        then do
          a <- indexSmallArrayM arr ixSrc
          let (preserve,b') = p b a
          if preserve
            then do
              writeSmallArray marr ixDst a
              go (ixSrc + 1) (ixDst + 1) b'
            else go (ixSrc + 1) ixDst b'
        else return (ixDst,b)
  (dstLen,r) <- go 0 0 b0
  finalArr <- internalResizeMutableArray marr dstLen
  pure (r,finalArr)

partitionSmallArray :: (a -> Bool) -> SmallArray a -> (SmallArray a, SmallArray a)
{-# INLINE partitionSmallArray #-}
partitionSmallArray p !arr = runArray2 $ do
  let !sz = sizeofSmallArray arr
  marrA <- newSmallArray sz (die "partitionSmallArray" "impossible")
  marrB <- newSmallArray sz (die "partitionSmallArray" "impossible")
  let go !ixSrc !ixDstA !ixDstB = if ixSrc < sz
        then do
          a <- indexSmallArrayM arr ixSrc
          if p a
            then do
              writeSmallArray marrA ixDstA a
              go (ixSrc + 1) (ixDstA + 1) ixDstB
            else do
              writeSmallArray marrB ixDstB a
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
internalResizeMutableArray :: SmallMutableArray s a -> Int -> ST s (SmallMutableArray s a)
internalResizeMutableArray !a !len = if len == sizeofSmallMutableArray a
  then return a
  else do
    b <- newSmallArray len (die "internalResizeMutableArray" "impossible")
    copySmallMutableArray b 0 a 0 len
    return b
{-# INLINE internalResizeMutableArray #-}

instance Foldable SmallArray where
  -- Note: we perform the array lookups eagerly so we won't
  -- create thunks to perform lookups even if GHC can't see
  -- that the folding function is strict.
  foldr f = \z !ary ->
    let
      !sz = sizeofSmallArray ary
      go i
        | i == sz = z
        | (# x #) <- indexSmallArray## ary i
        = f x (go (i+1))
    in go 0
  {-# INLINE foldr #-}
  foldl f = \z !ary ->
    let
      go i
        | i < 0 = z
        | (# x #) <- indexSmallArray## ary i
        = f (go (i-1)) x
    in go (sizeofSmallArray ary - 1)
  {-# INLINE foldl #-}
  foldr1 f = \ !ary ->
    let
      !sz = sizeofSmallArray ary - 1
      go i =
        case indexSmallArray## ary i of
          (# x #) | i == sz -> x
                  | otherwise -> f x (go (i+1))
    in if sz < 0
       then die "foldr1" "empty array"
       else go 0
  {-# INLINE foldr1 #-}
  foldl1 f = \ !ary ->
    let
      !sz = sizeofSmallArray ary - 1
      go i =
        case indexSmallArray## ary i of
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
        | (# x #) <- indexSmallArray## ary i
        = go (i-1) (f x acc)
    in go (sizeofSmallArray ary - 1) z
  {-# INLINE foldr' #-}
  foldl' f = \z !ary ->
    let
      !sz = sizeofSmallArray ary
      go i !acc
        | i == sz = acc
        | (# x #) <- indexSmallArray## ary i
        = go (i+1) (f acc x)
    in go 0 z
  {-# INLINE foldl' #-}
  null a = sizeofSmallArray a == 0
  {-# INLINE null #-}
  length = sizeofSmallArray
  {-# INLINE length #-}
  maximum ary | sz == 0   = die "maximum" "empty array"
              | (# frst #) <- indexSmallArray## ary 0
              = go 1 frst
   where
     sz = sizeofSmallArray ary
     go i !e
       | i == sz = e
       | (# x #) <- indexSmallArray## ary i
       = go (i+1) (max e x)
  {-# INLINE maximum #-}
  minimum ary | sz == 0   = die "minimum" "empty array"
              | (# frst #) <- indexSmallArray## ary 0
              = go 1 frst
   where sz = sizeofSmallArray ary
         go i !e
           | i == sz = e
           | (# x #) <- indexSmallArray## ary i
           = go (i+1) (min e x)
  {-# INLINE minimum #-}
  sum = foldl' (+) 0
  {-# INLINE sum #-}
  product = foldl' (*) 1
  {-# INLINE product #-}


-- | Read a value from the immutable array at the given index, returning
-- the result in an unboxed unary tuple. This is currently used to implement
-- folds.
indexSmallArray## :: SmallArray a -> Int -> (# a #)
indexSmallArray## arr (I# i) = indexSmallArray# (smallArray# arr) i
{-# INLINE indexSmallArray## #-}

-- | Strict map over the elements of the array.
mapSmallArray' :: (a -> b) -> SmallArray a -> SmallArray b
mapSmallArray' f a =
  createArray (sizeofSmallArray a) (die "mapSmallArray'" "impossible") $ \mb ->
    let go i | i == sizeofSmallArray a
             = return ()
             | otherwise
             = do x <- indexSmallArrayM a i
                  -- We use indexSmallArrayM here so that we will perform the
                  -- indexing eagerly even if f is lazy.
                  let !y = f x
                  writeSmallArray mb i y >> go (i+1)
     in go 0
{-# INLINE mapSmallArray' #-}

