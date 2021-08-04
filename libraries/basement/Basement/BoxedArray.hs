-- |
-- Module      : Basement.BoxedArray
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Simple boxed array abstraction
--
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Basement.BoxedArray
    ( Array
    , MArray
    , empty
    , length
    , mutableLength
    , copy
    , unsafeCopyAtRO
    , thaw
    , new
    , create
    , unsafeFreeze
    , unsafeThaw
    , freeze
    , unsafeWrite
    , unsafeRead
    , unsafeIndex
    , write
    , read
    , index
    , singleton
    , replicate
    , null
    , take
    , drop
    , splitAt
    , revTake
    , revDrop
    , revSplitAt
    , splitOn
    , sub
    , intersperse
    , span
    , spanEnd
    , break
    , breakEnd
    , mapFromUnboxed
    , mapToUnboxed
    , cons
    , snoc
    , uncons
    , unsnoc
    -- , findIndex
    , sortBy
    , filter
    , reverse
    , elem
    , find
    , foldl'
    , foldr
    , foldl1'
    , foldr1
    , all
    , any
    , isPrefixOf
    , isSuffixOf
    , builderAppend
    , builderBuild
    , builderBuild_
    ) where

import           GHC.Prim
import           GHC.Types
import           GHC.ST
import           Data.Proxy
import           Basement.Numerical.Additive
import           Basement.Numerical.Subtractive
import           Basement.NonEmpty
import           Basement.Compat.Base
import qualified Basement.Alg.Class as Alg
import qualified Basement.Alg.Mutable as Alg
import           Basement.Compat.MonadTrans
import           Basement.Compat.Semigroup
import           Basement.Compat.Primitive
import           Basement.Types.OffsetSize
import           Basement.PrimType
import           Basement.NormalForm
import           Basement.Monad
import           Basement.UArray.Base (UArray)
import qualified Basement.UArray.Base as UArray
import           Basement.Exception
import           Basement.MutableBuilder
import qualified Basement.Compat.ExtList as List

-- | Array of a
data Array a = Array {-# UNPACK #-} !(Offset a)
                     {-# UNPACK #-} !(CountOf a)
                                    (Array# a)
    deriving (Typeable)

instance Data ty => Data (Array ty) where
    dataTypeOf _ = arrayType
    toConstr _   = error "toConstr"
    gunfold _ _  = error "gunfold"

arrayType :: DataType
arrayType = mkNoRepType "Foundation.Array"

instance NormalForm a => NormalForm (Array a) where
    toNormalForm arr = loop 0
      where
        !sz = length arr
        loop !i
            | i .==# sz = ()
            | otherwise = unsafeIndex arr i `seq` loop (i+1)

-- | Mutable Array of a
data MArray a st = MArray {-# UNPACK #-} !(Offset a)
                          {-# UNPACK #-} !(CountOf a)
                                         (MutableArray# st a)
    deriving (Typeable)

instance Functor Array where
    fmap = map

instance Semigroup (Array a) where
    (<>) = append
instance Monoid (Array a) where
    mempty  = empty
    mappend = append
    mconcat = concat

instance Show a => Show (Array a) where
    show v = show (toList v)

instance Eq a => Eq (Array a) where
    (==) = equal
instance Ord a => Ord (Array a) where
    compare = vCompare

instance IsList (Array ty) where
    type Item (Array ty) = ty
    fromList = vFromList
    fromListN len = vFromListN (CountOf len)
    toList = vToList

-- | return the numbers of elements in a mutable array
mutableLength :: MArray ty st -> Int
mutableLength (MArray _ (CountOf len) _) = len
{-# INLINE mutableLength #-}

-- | return the numbers of elements in a mutable array
mutableLengthSize :: MArray ty st -> CountOf ty
mutableLengthSize (MArray _ size _) = size
{-# INLINE mutableLengthSize #-}

-- | Return the element at a specific index from an array.
--
-- If the index @n is out of bounds, an error is raised.
index :: Array ty -> Offset ty -> ty
index array n
    | isOutOfBound n len = outOfBound OOB_Index n len
    | otherwise          = unsafeIndex array n
  where len = length array
{-# INLINE index #-}

-- | Return the element at a specific index from an array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'index' if unsure.
unsafeIndex :: Array ty -> Offset ty -> ty
unsafeIndex (Array start _ a) ofs = primArrayIndex a (start+ofs)
{-# INLINE unsafeIndex #-}

-- | read a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
read :: PrimMonad prim => MArray ty (PrimState prim) -> Offset ty -> prim ty
read array n
    | isOutOfBound n len = primOutOfBound OOB_Read n len
    | otherwise          = unsafeRead array n
  where len = mutableLengthSize array
{-# INLINE read #-}

-- | read from a cell in a mutable array without bounds checking.
--
-- Reading from invalid memory can return unpredictable and invalid values.
-- use 'read' if unsure.
unsafeRead :: PrimMonad prim => MArray ty (PrimState prim) -> Offset ty -> prim ty
unsafeRead (MArray start _ ma) i = primMutableArrayRead ma (start + i)
{-# INLINE unsafeRead #-}

-- | Write to a cell in a mutable array.
--
-- If the index is out of bounds, an error is raised.
write :: PrimMonad prim => MArray ty (PrimState prim) -> Offset ty -> ty -> prim ()
write array n val
    | isOutOfBound n len = primOutOfBound OOB_Write n len
    | otherwise          = unsafeWrite array n val
  where len = mutableLengthSize array
{-# INLINE write #-}

-- | write to a cell in a mutable array without bounds checking.
--
-- Writing with invalid bounds will corrupt memory and your program will
-- become unreliable. use 'write' if unsure.
unsafeWrite :: PrimMonad prim => MArray ty (PrimState prim) -> Offset ty -> ty -> prim ()
unsafeWrite (MArray start _ ma) ofs v =
    primMutableArrayWrite ma (start + ofs) v
{-# INLINE unsafeWrite #-}

-- | Freeze a mutable array into an array.
--
-- the MArray must not be changed after freezing.
unsafeFreeze :: PrimMonad prim => MArray ty (PrimState prim) -> prim (Array ty)
unsafeFreeze (MArray ofs sz ma) = primitive $ \s1 ->
    case unsafeFreezeArray# ma s1 of
        (# s2, a #) -> (# s2, Array ofs sz a #)
{-# INLINE unsafeFreeze #-}

-- | Thaw an immutable array.
--
-- The Array must not be used after thawing.
unsafeThaw :: PrimMonad prim => Array ty -> prim (MArray ty (PrimState prim))
unsafeThaw (Array ofs sz a) = primitive $ \st -> (# st, MArray ofs sz (unsafeCoerce# a) #)
{-# INLINE unsafeThaw #-}

-- | Thaw an array to a mutable array.
--
-- the array is not modified, instead a new mutable array is created
-- and every values is copied, before returning the mutable array.
thaw :: PrimMonad prim => Array ty -> prim (MArray ty (PrimState prim))
thaw array = do
    m <- new (length array)
    unsafeCopyAtRO m (Offset 0) array (Offset 0) (length array)
    pure m
{-# INLINE thaw #-}

freeze :: PrimMonad prim => MArray ty (PrimState prim) -> prim (Array ty)
freeze marray = do
    m <- new sz
    copyAt m (Offset 0) marray (Offset 0) sz
    unsafeFreeze m
  where
    sz = mutableLengthSize marray

-- | Copy the element to a new element array
copy :: Array ty -> Array ty
copy a = runST (unsafeThaw a >>= freeze)

-- | Copy a number of elements from an array to another array with offsets
copyAt :: PrimMonad prim
       => MArray ty (PrimState prim) -- ^ destination array
       -> Offset ty                  -- ^ offset at destination
       -> MArray ty (PrimState prim) -- ^ source array
       -> Offset ty                  -- ^ offset at source
       -> CountOf ty                 -- ^ number of elements to copy
       -> prim ()
copyAt dst od src os n = loop od os
  where -- !endIndex = os `offsetPlusE` n
        loop d s
            | s .==# n  = pure ()
            | otherwise = unsafeRead src s >>= unsafeWrite dst d >> loop (d+1) (s+1)

-- | Copy @n@ sequential elements from the specified offset in a source array
--   to the specified position in a destination array.
--
--   This function does not check bounds. Accessing invalid memory can return
--   unpredictable and invalid values.
unsafeCopyAtRO :: PrimMonad prim
               => MArray ty (PrimState prim) -- ^ destination array
               -> Offset ty                  -- ^ offset at destination
               -> Array ty                   -- ^ source array
               -> Offset ty                  -- ^ offset at source
               -> CountOf ty                    -- ^ number of elements to copy
               -> prim ()
unsafeCopyAtRO (MArray (Offset (I# dstart)) _ da) (Offset (I# dofs))
               (Array  (Offset (I# sstart)) _ sa) (Offset (I# sofs))
               (CountOf (I# n)) =
    primitive $ \st ->
        (# copyArray# sa (sstart +# sofs) da (dstart +# dofs) n st, () #)

-- | Allocate a new array with a fill function that has access to the elements of
--   the source array.
unsafeCopyFrom :: Array ty -- ^ Source array
               -> CountOf ty  -- ^ Length of the destination array
               -> (Array ty -> Offset ty -> MArray ty s -> ST s ())
               -- ^ Function called for each element in the source array
               -> ST s (Array ty) -- ^ Returns the filled new array
unsafeCopyFrom v' newLen f = new newLen >>= fill (Offset 0) f >>= unsafeFreeze
  where len = length v'
        endIdx = Offset 0 `offsetPlusE` len
        fill i f' r'
            | i == endIdx = pure r'
            | otherwise   = do f' v' i r'
                               fill (i + Offset 1) f' r'

-- | Create a new mutable array of size @n.
--
-- all the cells are uninitialized and could contains invalid values.
--
-- All mutable arrays are allocated on a 64 bits aligned addresses
-- and always contains a number of bytes multiples of 64 bits.
new :: PrimMonad prim => CountOf ty -> prim (MArray ty (PrimState prim))
new sz@(CountOf (I# n)) = primitive $ \s1 ->
    case newArray# n (error "vector: internal error uninitialized vector") s1 of
        (# s2, ma #) -> (# s2, MArray (Offset 0) sz ma #)

-- | Create a new array of size @n by settings each cells through the
-- function @f.
create :: forall ty . CountOf ty -- ^ the size of the array
       -> (Offset ty -> ty)   -- ^ the function that set the value at the index
       -> Array ty            -- ^ the array created
create n initializer = runST (new n >>= iter initializer)
  where
    iter :: PrimMonad prim => (Offset ty -> ty) -> MArray ty (PrimState prim) -> prim (Array ty)
    iter f ma = loop 0
      where
        loop s
            | s .==# n  = unsafeFreeze ma
            | otherwise = unsafeWrite ma s (f s) >> loop (s+1)
        {-# INLINE loop #-}
    {-# INLINE iter #-}

-----------------------------------------------------------------------
-- higher level collection implementation
-----------------------------------------------------------------------
equal :: Eq a => Array a -> Array a -> Bool
equal a b = (len == length b) && eachEqual 0
  where
    len = length a
    eachEqual !i
        | i .==# len                         = True
        | unsafeIndex a i /= unsafeIndex b i = False
        | otherwise                          = eachEqual (i+1)

vCompare :: Ord a => Array a -> Array a -> Ordering
vCompare a b = loop 0
  where
    !la = length a
    !lb = length b
    loop n
        | n .==# la = if la == lb then EQ else LT
        | n .==# lb = GT
        | otherwise =
            case unsafeIndex a n `compare` unsafeIndex b n of
                EQ -> loop (n+1)
                r  -> r

empty :: Array a
empty = runST $ onNewArray 0 (\_ s -> s)

length :: Array a -> CountOf a
length (Array _ sz _) = sz

vFromList :: [a] -> Array a
vFromList l = runST (new len >>= loop 0 l)
  where
    len = List.length l
    loop _ []     ma = unsafeFreeze ma
    loop i (x:xs) ma = unsafeWrite ma i x >> loop (i+1) xs ma

-- | just like vFromList but with a length hint.
--
-- The resulting array is guarantee to have been allocated to the length
-- specified, but the slice might point to the initialized cells only in
-- case the length is bigger than the list.
--
-- If the length is too small, then the list is truncated.
--
vFromListN :: forall a . CountOf a -> [a] -> Array a
vFromListN len l = runST $ do
    ma <- new len
    sz <- loop 0 l ma
    unsafeFreezeShrink ma sz
  where
    -- TODO rewrite without ma as parameter
    loop :: Offset a -> [a] -> MArray a s -> ST s (CountOf a)
    loop i []     _  = return (offsetAsSize i)
    loop i (x:xs) ma
        | i .==# len = return (offsetAsSize i)
        | otherwise  = unsafeWrite ma i x >> loop (i+1) xs ma

vToList :: Array a -> [a]
vToList v
    | len == 0  = []
    | otherwise = fmap (unsafeIndex v) [0..sizeLastOffset len]
  where !len = length v

-- | Append 2 arrays together by creating a new bigger array
append :: Array ty -> Array ty -> Array ty
append a b = runST $ do
    r  <- new (la+lb)
    unsafeCopyAtRO r (Offset 0) a (Offset 0) la
    unsafeCopyAtRO r (sizeAsOffset la) b (Offset 0) lb
    unsafeFreeze r
  where la = length a
        lb = length b

concat :: [Array ty] -> Array ty
concat l = runST $ do
    r <- new (mconcat $ fmap length l)
    loop r (Offset 0) l
    unsafeFreeze r
  where loop _ _ []     = pure ()
        loop r i (x:xs) = do
            unsafeCopyAtRO r i x (Offset 0) lx
            loop r (i `offsetPlusE` lx) xs
          where lx = length x

{-
modify :: PrimMonad m
       => Array a
       -> (MArray (PrimState m) a -> m ())
       -> m (Array a)
modify (Array a) f = primitive $ \st -> do
    case thawArray# a 0# (sizeofArray# a) st of
        (# st2, mv #) ->
            case internal_ (f $ MArray mv) st2 of
                st3 ->
                    case unsafeFreezeArray# mv st3 of
                        (# st4, a' #) -> (# st4, Array a' #)
-}

-----------------------------------------------------------------------
-- helpers

onNewArray :: PrimMonad m
           => Int
           -> (MutableArray# (PrimState m) a -> State# (PrimState m) -> State# (PrimState m))
           -> m (Array a)
onNewArray len@(I# len#) f = primitive $ \st -> do
    case newArray# len# (error "onArray") st of { (# st2, mv #) ->
    case f mv st2                            of { st3           ->
    case unsafeFreezeArray# mv st3           of { (# st4, a #)  ->
        (# st4, Array (Offset 0) (CountOf len) a #) }}}

-----------------------------------------------------------------------


null :: Array ty -> Bool
null = (==) 0 . length

take :: CountOf ty -> Array ty -> Array ty
take nbElems a@(Array start len arr)
    | nbElems <= 0 = empty
    | n == len     = a
    | otherwise    = Array start n arr
  where
    n = min nbElems len

drop :: CountOf ty -> Array ty -> Array ty
drop nbElems a@(Array start len arr)
    | nbElems <= 0                               = a
    | Just nbTails <- len - nbElems, nbTails > 0 = Array (start `offsetPlusE` nbElems) nbTails arr
    | otherwise                                  = empty

splitAt :: CountOf ty -> Array ty -> (Array ty, Array ty)
splitAt nbElems a@(Array start len arr)
    | nbElems <= 0 = (empty, a)
    | Just nbTails <- len - nbElems, nbTails > 0 = ( Array start                         nbElems arr
                                                   , Array (start `offsetPlusE` nbElems) nbTails arr)
    | otherwise = (a, empty)

-- inverse a CountOf that is specified from the end (e.g. take n elements from the end)
countFromStart :: Array ty -> CountOf ty -> CountOf ty
countFromStart v sz@(CountOf sz')
    | sz >= len = CountOf 0
    | otherwise = CountOf (len' - sz')
  where len@(CountOf len') = length v

revTake :: CountOf ty -> Array ty -> Array ty
revTake n v = drop (countFromStart v n) v

revDrop :: CountOf ty -> Array ty -> Array ty
revDrop n v = take (countFromStart v n) v

revSplitAt :: CountOf ty -> Array ty -> (Array ty, Array ty)
revSplitAt n v = (drop idx v, take idx v) where idx = countFromStart v n

splitOn ::  (ty -> Bool) -> Array ty -> [Array ty]
splitOn predicate vec
    | len == CountOf 0 = [mempty]
    | otherwise     = loop (Offset 0) (Offset 0)
  where
    !len = length vec
    !endIdx = Offset 0 `offsetPlusE` len
    loop prevIdx idx
        | idx == endIdx = [sub vec prevIdx idx]
        | otherwise     =
            let e = unsafeIndex vec idx
                idx' = idx + 1
             in if predicate e
                    then sub vec prevIdx idx : loop idx' idx'
                    else loop prevIdx idx'

sub :: Array ty -> Offset ty -> Offset ty -> Array ty
sub (Array start len a) startIdx expectedEndIdx
    | startIdx == endIdx           = empty
    | otherwise                    = Array (start + startIdx) newLen a
  where
    newLen = endIdx - startIdx
    endIdx = min expectedEndIdx (sizeAsOffset len)

break ::  (ty -> Bool) -> Array ty -> (Array ty, Array ty)
break predicate v = findBreak 0
  where
    !len = length v
    findBreak i
        | i .==# len  = (v, empty)
        | otherwise   =
            if predicate (unsafeIndex v i)
                then splitAt (offsetAsSize i) v
                else findBreak (i+1)

breakEnd ::  (ty -> Bool) -> Array ty -> (Array ty, Array ty)
breakEnd predicate v = findBreak (sizeAsOffset len)
  where
    !len = length v
    findBreak !i
        | i == 0      = (v, empty)
        | predicate e = splitAt (offsetAsSize i) v
        | otherwise   = findBreak i'
      where
        e = unsafeIndex v i'
        i' = i `offsetSub` 1

intersperse :: ty -> Array ty -> Array ty
intersperse sep v = case len - 1 of
    Nothing -> v
    Just 0 -> v
    Just more -> runST $ unsafeCopyFrom v (len + more) (go (Offset 0 `offsetPlusE` more) sep)
  where len = length v
        -- terminate 1 before the end

        go :: Offset ty -> ty -> Array ty -> Offset ty -> MArray ty s -> ST s ()
        go endI sep' oldV oldI newV
            | oldI == endI = unsafeWrite newV dst e
            | otherwise    = do
                unsafeWrite newV dst e
                unsafeWrite newV (dst + 1) sep'
          where
            e = unsafeIndex oldV oldI
            dst = oldI + oldI

span ::  (ty -> Bool) -> Array ty -> (Array ty, Array ty)
span p = break (not . p)

spanEnd ::  (ty -> Bool) -> Array ty -> (Array ty, Array ty)
spanEnd p = breakEnd (not . p)

map :: (a -> b) -> Array a -> Array b
map f a = create (sizeCast Proxy $ length a) (\i -> f $ unsafeIndex a (offsetCast Proxy i))

mapFromUnboxed :: PrimType a => (a -> b) -> UArray a -> Array b
mapFromUnboxed f arr = vFromListN (sizeCast Proxy $ UArray.length arr) . fmap f . toList $ arr

mapToUnboxed :: PrimType b => (a -> b) -> Array a -> UArray b
mapToUnboxed f arr = UArray.vFromListN (sizeCast Proxy $ length arr) . fmap f . toList $ arr

{-
mapIndex :: (Int -> a -> b) -> Array a -> Array b
mapIndex f a = create (length a) (\i -> f i $ unsafeIndex a i)
-}

singleton :: ty -> Array ty
singleton e = runST $ do
    a <- new 1
    unsafeWrite a 0 e
    unsafeFreeze a

replicate :: CountOf ty -> ty -> Array ty
replicate sz ty = create sz (const ty)

cons :: ty -> Array ty -> Array ty
cons e vec
    | len == CountOf 0 = singleton e
    | otherwise     = runST $ do
        mv <- new (len + CountOf 1)
        unsafeWrite mv 0 e
        unsafeCopyAtRO mv (Offset 1) vec (Offset 0) len
        unsafeFreeze mv
  where
    !len = length vec

snoc ::  Array ty -> ty -> Array ty
snoc vec e
    | len == 0  = singleton e
    | otherwise = runST $ do
        mv <- new (len + 1)
        unsafeCopyAtRO mv 0 vec 0 len
        unsafeWrite mv (sizeAsOffset len) e
        unsafeFreeze mv
  where
    !len = length vec

uncons :: Array ty -> Maybe (ty, Array ty)
uncons vec
    | len == 0  = Nothing
    | otherwise = Just (unsafeIndex vec 0, drop 1 vec)
  where
    !len = length vec

unsnoc :: Array ty -> Maybe (Array ty, ty)
unsnoc vec = case len - 1 of
    Nothing -> Nothing
    Just newLen -> Just (take newLen vec, unsafeIndex vec (sizeLastOffset len))
  where
    !len = length vec

elem :: Eq ty => ty -> Array ty -> Bool
elem !ty arr = loop 0
  where
    !sz = length arr
    loop !i | i .==# sz = False
            | t == ty   = True
            | otherwise = loop (i+1)
      where t = unsafeIndex arr i

find :: (ty -> Bool) -> Array ty -> Maybe ty
find predicate vec = loop 0
  where
    !len = length vec
    loop i
        | i .==# len = Nothing
        | otherwise  =
            let e = unsafeIndex vec i
             in if predicate e then Just e else loop (i+1)

instance (PrimMonad prim, st ~ PrimState prim) 
         => Alg.RandomAccess (MArray ty st) prim ty where
    read (MArray _ _ mba) = primMutableArrayRead mba
    write (MArray _ _ mba) = primMutableArrayWrite mba

sortBy :: forall ty . (ty -> ty -> Ordering) -> Array ty -> Array ty
sortBy xford vec
    | len == 0  = empty
    | otherwise = runST (thaw vec >>= doSort xford)
  where
    len = length vec
    doSort :: PrimMonad prim => (ty -> ty -> Ordering) -> MArray ty (PrimState prim) -> prim (Array ty)
    doSort ford ma = Alg.inplaceSortBy ford 0 len ma >> unsafeFreeze ma

filter :: forall ty . (ty -> Bool) -> Array ty -> Array ty
filter predicate vec = runST (new len >>= copyFilterFreeze predicate (unsafeIndex vec))
  where
    !len = length vec
    copyFilterFreeze :: PrimMonad prim => (ty -> Bool) -> (Offset ty -> ty) -> MArray ty (PrimState prim) -> prim (Array ty)
    copyFilterFreeze predi getVec mvec = loop (Offset 0) (Offset 0) >>= freezeUntilIndex mvec
      where
        loop d s
            | s .==# len  = pure d
            | predi v     = unsafeWrite mvec d v >> loop (d+1) (s+1)
            | otherwise   = loop d (s+1)
          where
            v = getVec s

freezeUntilIndex :: PrimMonad prim => MArray ty (PrimState prim) -> Offset ty -> prim (Array ty)
freezeUntilIndex mvec d = do
    m <- new (offsetAsSize d)
    copyAt m (Offset 0) mvec (Offset 0) (offsetAsSize d)
    unsafeFreeze m

unsafeFreezeShrink :: PrimMonad prim => MArray ty (PrimState prim) -> CountOf ty -> prim (Array ty)
unsafeFreezeShrink (MArray start _ ma) n = unsafeFreeze (MArray start n ma)

reverse :: Array ty -> Array ty
reverse a = create len toEnd
  where
    len@(CountOf s) = length a
    toEnd (Offset i) = unsafeIndex a (Offset (s - 1 - i))

foldr :: (ty -> a -> a) -> a -> Array ty -> a
foldr f initialAcc vec = loop 0
  where
    len = length vec
    loop !i
        | i .==# len = initialAcc
        | otherwise  = unsafeIndex vec i `f` loop (i+1)

foldl' :: (a -> ty -> a) -> a -> Array ty -> a
foldl' f initialAcc vec = loop 0 initialAcc
  where
    len = length vec
    loop !i !acc
        | i .==# len = acc
        | otherwise  = loop (i+1) (f acc (unsafeIndex vec i))

foldl1' :: (ty -> ty -> ty) -> NonEmpty (Array ty) -> ty
foldl1' f arr = let (initialAcc, rest) = splitAt 1 $ getNonEmpty arr
               in foldl' f (unsafeIndex initialAcc 0) rest

foldr1 :: (ty -> ty -> ty) -> NonEmpty (Array ty) -> ty
foldr1 f arr = let (initialAcc, rest) = revSplitAt 1 $ getNonEmpty arr
               in foldr f (unsafeIndex initialAcc 0) rest

all :: (ty -> Bool) -> Array ty -> Bool
all p ba = loop 0
  where
    len = length ba
    loop !i
      | i .==# len = True
      | not $ p (unsafeIndex ba i) = False
      | otherwise = loop (i + 1)

any :: (ty -> Bool) -> Array ty -> Bool
any p ba = loop 0
  where
    len = length ba
    loop !i
      | i .==# len = False
      | p (unsafeIndex ba i) = True
      | otherwise = loop (i + 1)

isPrefixOf :: Eq ty => Array ty -> Array ty -> Bool
isPrefixOf pre arr
    | pLen > pArr = False
    | otherwise   = pre == take pLen arr
  where
    !pLen = length pre
    !pArr = length arr

isSuffixOf :: Eq ty => Array ty -> Array ty -> Bool
isSuffixOf suffix arr
    | pLen > pArr = False
    | otherwise   = suffix == revTake pLen arr
  where
    !pLen = length suffix
    !pArr = length arr

builderAppend :: PrimMonad state => ty -> Builder (Array ty) (MArray ty) ty state err ()
builderAppend v = Builder $ State $ \(i, st, e) ->
    if i .==# chunkSize st
        then do
            cur      <- unsafeFreeze (curChunk st)
            newChunk <- new (chunkSize st)
            unsafeWrite newChunk 0 v
            pure ((), (Offset 1, st { prevChunks     = cur : prevChunks st
                                      , prevChunksSize = chunkSize st + prevChunksSize st
                                      , curChunk       = newChunk
                                      }, e))
        else do
            unsafeWrite (curChunk st) i v
            pure ((), (i+1, st, e))

builderBuild :: PrimMonad m => Int -> Builder (Array ty) (MArray ty) ty m err () -> m (Either err (Array ty))
builderBuild sizeChunksI ab
    | sizeChunksI <= 0 = builderBuild 64 ab
    | otherwise        = do
        first      <- new sizeChunks
        (i, st, e) <- snd <$> runState (runBuilder ab) (Offset 0, BuildingState [] (CountOf 0) first sizeChunks, Nothing)
        case e of
          Just err -> pure (Left err)
          Nothing -> do
            cur <- unsafeFreezeShrink (curChunk st) (offsetAsSize i)
            -- Build final array
            let totalSize = prevChunksSize st + offsetAsSize i
            bytes <- new totalSize >>= fillFromEnd totalSize (cur : prevChunks st) >>= unsafeFreeze
            pure (Right bytes)
  where
    sizeChunks = CountOf sizeChunksI

    fillFromEnd _    []     mua = pure mua
    fillFromEnd !end (x:xs) mua = do
        let sz = length x
        let start = end `sizeSub` sz
        unsafeCopyAtRO mua (sizeAsOffset start) x (Offset 0) sz
        fillFromEnd start xs mua

builderBuild_ :: PrimMonad m => Int -> Builder (Array ty) (MArray ty) ty m () () -> m (Array ty)
builderBuild_ sizeChunksI ab = either (\() -> internalError "impossible output") id <$> builderBuild sizeChunksI ab
