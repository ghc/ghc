{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
module T20598a where

import Control.Monad (liftM)
import Control.Monad.ST (runST)
import GHC.Exts
import GHC.ST (ST(..))
import GHC.Types (SPEC(..))
import Data.Kind

data Value = Unit Double | Number Int
  deriving (Show)

mkArray :: [Value] -> Value
mkArray = mkVector . vfromList

mkVector :: Vector Value -> Value
mkVector vs = case vtraverse extractNumber vs of
  Nothing -> Unit 43.5
  Just{}  -> Number 42

extractNumber :: Value -> Maybe Int
extractNumber (Number i) = Just i
extractNumber _ = Nothing

-----
-- Data.Vector
-----

data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(Array a)

type instance Mutable Vector = MVector

instance GVector Vector a where
  {-# INLINE gbasicUnsafeFreeze #-}
  gbasicUnsafeFreeze (MVector i n marr)
    = Vector i n `liftM` unsafeFreezeArray marr

  {-# INLINE gbasicLength #-}
  gbasicLength (Vector _ n _) = n

  {-# INLINE gbasicUnsafeIndexM #-}
  gbasicUnsafeIndexM (Vector i _ arr) j = indexArrayM arr (i+j)

  {-# INLINE gbasicUnsafeCopy #-}
  gbasicUnsafeCopy (MVector i n dst) (Vector j _ src)
    = copyArray dst i src j n

vfromList :: [a] -> Vector a
{-# INLINE vfromList #-}
vfromList = gfromList

vfromListN :: Int -> [a] -> Vector a
{-# INLINE vfromListN #-}
vfromListN = gfromListN

vtoList :: Vector a -> [a]
{-# INLINE vtoList #-}
vtoList = gtoList

vtraverse :: Applicative f => (a -> f b) -> Vector a -> f (Vector b)
{-# INLINE vtraverse #-}
vtraverse f xs =
  -- Get the length of the vector in /O(1)/ time
  let !n = glength xs
  -- Use fromListN to be more efficient in construction of resulting vector
  -- Also behaves better with compact regions, preventing runtime exceptions
  in vfromListN n <$> traverse f (vtoList xs)

-----
-- Data.Vector.Mutable
-----

data MVector s a = MVector { _offset :: {-# UNPACK #-} !Int
                           , _size   :: {-# UNPACK #-} !Int
                           , _array  :: {-# UNPACK #-} !(MutableArray s a)
                           }

instance GMVector MVector a where
  {-# INLINE gmbasicLength #-}
  gmbasicLength (MVector _ n _) = n

  {-# INLINE gmbasicUnsafeSlice #-}
  gmbasicUnsafeSlice j m (MVector i _ arr) = MVector (i+j) m arr

  {-# INLINE gmbasicUnsafeNew #-}
  gmbasicUnsafeNew n
    = do
        arr <- newArray n uninitialised
        return (MVector 0 n arr)

  {-# INLINE gmbasicUnsafeWrite #-}
  gmbasicUnsafeWrite (MVector i _ arr) j x = writeArray arr (i+j) x

  {-# INLINE gmbasicUnsafeCopy #-}
  gmbasicUnsafeCopy (MVector i n dst) (MVector j _ src)
    = copyMutableArray dst i src j n

uninitialised :: a
uninitialised = error "Data.Vector.Mutable: uninitialised element. If you are trying to compact a vector, use the 'Data.Vector.force' function to remove uninitialised elements from the underlying array."

-----
-- Data.Vector.Fusion.Bundle
----

type Bundle = MBundle Id

bfromList :: [a] -> Bundle v a
{-# INLINE bfromList #-}
bfromList = mbfromList

bfromListN :: Int -> [a] -> Bundle v a
{-# INLINE bfromListN #-}
bfromListN = mbfromListN

btoList :: Bundle v a -> [a]
{-# INLINE btoList #-}
-- btoList s = unId (mbtoList s)
btoList s = build (\c n -> btoListFB c n s)

btoListFB :: (a -> b -> b) -> b -> Bundle v a -> b
{-# INLINE [0] btoListFB #-}
btoListFB c n Bundle{sElems = Stream step t} = go t
  where
    go s = case unId (step s) of
             Yield x s' -> x `c` go s'
             Skip    s' -> go s'
             Done       -> n

bfromVector :: GVector v a => v a -> Bundle v a
{-# INLINE bfromVector #-}
bfromVector = mbfromVector

blength :: Bundle v a -> Int
{-# INLINE blength #-}
blength = unId . mblength

-----
-- Data.Vector.Fusion.Bundle.Monadic
----

data MBundle m v a = Bundle { sElems  :: Stream m a
                            , sChunks :: Stream m (Chunk v a)
                            , sVector :: Maybe (v a)
                            , sSize   :: Size
                            }

data Chunk v a = Chunk Int (forall m. (PrimMonad m, GVector v a) => Mutable v (PrimState m) a -> m ())

mbfromList :: Monad m => [a] -> MBundle m v a
{-# INLINE mbfromList #-}
mbfromList xs = mbunsafeFromList Unknown xs

mbfromListN :: Monad m => Int -> [a] -> MBundle m v a
{-# INLINE [1] mbfromListN #-}
mbfromListN n xs = mbfromStream (sfromListN n xs) (Max (delay_inline max n 0))

mbunsafeFromList :: Monad m => Size -> [a] -> MBundle m v a
{-# INLINE [1] mbunsafeFromList #-}
mbunsafeFromList sz xs = mbfromStream (sfromList xs) sz

mbfromStream :: Monad m => Stream m a -> Size -> MBundle m v a
{-# INLINE mbfromStream #-}
mbfromStream (Stream step t) sz = Bundle (Stream step t) (Stream step' t) Nothing sz
  where
    step' s = do r <- step s
                 return $ fmap (\x -> Chunk 1 (\v -> gmbasicUnsafeWrite v 0 x)) r

mbchunks :: MBundle m v a -> Stream m (Chunk v a)
{-# INLINE mbchunks #-}
mbchunks = sChunks

mbsize :: MBundle m v a -> Size
{-# INLINE mbsize #-}
mbsize = sSize

mblift :: Monad m => MBundle Id v a -> MBundle m v a
{-# INLINE [1] mblift #-}
mblift (Bundle (Stream step s) (Stream vstep t) v sz)
    = Bundle (Stream (return . unId . step) s)
             (Stream (return . unId . vstep) t) v sz

mbfromVector :: (Monad m, GVector v a) => v a -> MBundle m v a
{-# INLINE [1] mbfromVector #-}
mbfromVector v = v `seq` n `seq` Bundle (Stream step 0)
                                        (Stream vstep True)
                                        (Just v)
                                        (Exact n)
  where
    n = gbasicLength v

    {-# INLINE step #-}
    step i | i >= n = return Done
           | otherwise = case gbasicUnsafeIndexM v i of
                           Box x -> return $ Yield x (i+1)


    {-# INLINE vstep #-}
    vstep True  = return (Yield (Chunk (gbasicLength v) (\mv -> gbasicUnsafeCopy mv v)) False)
    vstep False = return Done

mblength :: Monad m => MBundle m v a -> m Int
{-# INLINE [1] mblength #-}
mblength Bundle{sSize = Exact n}  = return n
mblength Bundle{sChunks = s} = sfoldl' (\n (Chunk k _) -> n+k) 0 s

-----
-- Data.Vector.Fusion.Bundle.Size
----

data Size = Exact Int
          | Max   Int
          | Unknown

upperBound :: Size -> Maybe Int
upperBound (Exact n) = Just n
upperBound (Max   n) = Just n
upperBound Unknown   = Nothing

-----
-- Data.Vector.Fusion.Stream.Monadic
----

data Stream m a = forall s. Stream (s -> m (Step s a)) s

data Step s a where
  Yield :: a -> s -> Step s a
  Skip  :: s -> Step s a
  Done  :: Step s a

instance Functor (Step s) where
  {-# INLINE fmap #-}
  fmap f (Yield x s) = Yield (f x) s
  fmap _ (Skip s) = Skip s
  fmap _ Done = Done
  {-# INLINE (<$) #-}
  (<$) = fmap . const

sfromList :: Monad m => [a] -> Stream m a
{-# INLINE sfromList #-}
sfromList zs = Stream step zs
  where
    step (x:xs) = return (Yield x xs)
    step []     = return Done

sfromListN :: Monad m => Int -> [a] -> Stream m a
{-# INLINE [1] sfromListN #-}
sfromListN m zs = Stream step (zs,m)
  where
    {-# INLINE [0] step #-}
    step (_, n) | n <= 0 = return Done
    step (x:xs,n)        = return (Yield x (xs,n-1))
    step ([],_)          = return Done

sfoldlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE [1] sfoldlM #-}
sfoldlM m w (Stream step t) = foldlM_loop SPEC w t
  where
    foldlM_loop !_ z s
      = do
          r <- step s
          case r of
            Yield x s' -> do { z' <- m z x; foldlM_loop SPEC z' s' }
            Skip    s' -> foldlM_loop SPEC z s'
            Done       -> return z

sfoldl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> m a
{-# INLINE sfoldl' #-}
sfoldl' f = sfoldlM' (\a b -> return (f a b))

sfoldlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE [1] sfoldlM' #-}
sfoldlM' m w (Stream step t) = foldlM'_loop SPEC w t
  where
    foldlM'_loop !_ z s
      = z `seq`
        do
          r <- step s
          case r of
            Yield x s' -> do { z' <- m z x; foldlM'_loop SPEC z' s' }
            Skip    s' -> foldlM'_loop SPEC z s'
            Done       -> return z

-----
-- Data.Vector.Fusion.Util
----

newtype Id a = Id { unId :: a }

instance Functor Id where
  fmap f (Id x) = Id (f x)

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)

instance Monad Id where
  return = pure
  Id x >>= f = f x

data Box a = Box { unBox :: a }

instance Functor Box where
  fmap f (Box x) = Box (f x)

instance Applicative Box where
  pure = Box
  Box f <*> Box x = Box (f x)

instance Monad Box where
  return = pure
  Box x >>= f = f x

delay_inline :: (a -> b) -> a -> b
{-# INLINE [0] delay_inline #-}
delay_inline f = f

-----
-- Data.Vector.Generic
-----

gfromList :: GVector v a => [a] -> v a
{-# INLINE gfromList #-}
gfromList = gunstream . bfromList

gfromListN :: GVector v a => Int -> [a] -> v a
{-# INLINE gfromListN #-}
gfromListN n = gunstream . bfromListN n

gtoList :: GVector v a => v a -> [a]
{-# INLINE gtoList #-}
gtoList = btoList . gstream

gstream :: GVector v a => v a -> Bundle v a
{-# INLINE [1] gstream #-}
gstream v = bfromVector v

gunstream :: GVector v a => Bundle v a -> v a
{-# INLINE gunstream #-}
gunstream s = gnew (newunstream s)

gnew :: GVector v a => New v a -> v a
{-# INLINE [1] gnew #-}
gnew m = m `seq` runST (gunsafeFreeze =<< newrun m)

gunsafeFreeze
  :: (PrimMonad m, GVector v a) => Mutable v (PrimState m) a -> m (v a)
{-# INLINE gunsafeFreeze #-}
gunsafeFreeze = gbasicUnsafeFreeze

glength :: GVector v a => v a -> Int
{-# INLINE glength #-}
glength = blength . gstream

-----
-- Data.Vector.Generic.Base
-----

type family Mutable (v :: Type -> Type) :: Type -> Type -> Type

class GMVector (Mutable v) a => GVector v a where
  gbasicUnsafeFreeze :: PrimMonad m => Mutable v (PrimState m) a -> m (v a)
  gbasicLength      :: v a -> Int
  gbasicUnsafeIndexM  :: Monad m => v a -> Int -> m a
  gbasicUnsafeCopy :: PrimMonad m => Mutable v (PrimState m) a -> v a -> m ()

-----
-- Data.Vector.Generic.Mutable
-----

gmvunstream :: (PrimMonad m, GVector v a)
            => Bundle v a -> m (Mutable v (PrimState m) a)
{-# INLINE [1] gmvunstream #-}
gmvunstream s = gmvmunstream (mblift s)

gmvmunstream :: (PrimMonad m, GVector v a)
             => MBundle m v a -> m (Mutable v (PrimState m) a)
{-# INLINE [1] gmvmunstream #-}
gmvmunstream s = case upperBound (mbsize s) of
               Just n  -> gmvmunstreamMax     s n
               Nothing -> gmvmunstreamUnknown s


gmvmunstreamMax :: (PrimMonad m, GVector v a)
                => MBundle m v a -> Int -> m (Mutable v (PrimState m) a)
{-# INLINE gmvmunstreamMax #-}
gmvmunstreamMax s n
  = do
      v <- {- INTERNAL_CHECK(checkLength) "munstreamMax" n
           $ -} gmunsafeNew n
      let {-# INLINE [0] copyChunk #-}
          copyChunk i (Chunk m f) =
            {- INTERNAL_CHECK(checkSlice) "munstreamMax.copyChunk" i m (length v) $ -} do
              f (gmbasicUnsafeSlice i m v)
              return (i+m)

      n' <- sfoldlM' copyChunk 0 (mbchunks s)
      return -- $ INTERNAL_CHECK(checkSlice) "munstreamMax" 0 n' n
             $ gmunsafeSlice 0 n' v

gmvmunstreamUnknown :: (PrimMonad m, GVector v a)
                    => MBundle m v a -> m (Mutable v (PrimState m) a)
{-# INLINE gmvmunstreamUnknown #-}
gmvmunstreamUnknown s
  = do
      v <- gmunsafeNew 0
      (v', n) <- sfoldlM copyChunk (v,0) (mbchunks s)
      return -- $ INTERNAL_CHECK(checkSlice) "munstreamUnknown" 0 n (length v')
             $ gmunsafeSlice 0 n v'
  where
    {-# INLINE [0] copyChunk #-}
    copyChunk (v,i) (Chunk n f)
      = do
          let j = i+n
          v' <- if gmbasicLength v < j
                  then gmunsafeGrow v (delay_inline max (gmenlarge_delta v) (j - gmbasicLength v))
                  else return v
          {- INTERNAL_CHECK(checkSlice) "munstreamUnknown.copyChunk" i n (length v')
            $ -}
          f (gmbasicUnsafeSlice i n v')
          return (v',j)

gmunsafeSlice :: GMVector v a => Int
                              -> Int
                              -> v s a
                              -> v s a
{-# INLINE gmunsafeSlice #-}
gmunsafeSlice i n v = {- UNSAFE_CHECK(checkSlice) "unsafeSlice" i n (length v)
                    $ -} gmbasicUnsafeSlice i n v

gmunsafeNew :: (PrimMonad m, GMVector v a) => Int -> m (v (PrimState m) a)
{-# INLINE gmunsafeNew #-}
gmunsafeNew n = {- UNSAFE_CHECK(checkLength) "unsafeNew" n
              $ -} gmbasicUnsafeNew n

gmunsafeGrow ::
     (PrimMonad m, GMVector v a)
  => v (PrimState m) a
  -> Int
  -> m (v (PrimState m) a)
{-# INLINE gmunsafeGrow #-}
gmunsafeGrow v n = {- UNSAFE_CHECK(checkLength) "unsafeGrow" n
                 $ -} gmbasicUnsafeGrow v n

gmlength :: GMVector v a => v s a -> Int
{-# INLINE gmlength #-}
gmlength = gmbasicLength

gmenlarge_delta :: GMVector v a => v s a -> Int
gmenlarge_delta v = max (gmlength v) 1

-----
-- Data.Vector.Generic.Mutable.Base
-----

class GMVector v a where
  gmbasicLength      :: v s a -> Int
  gmbasicUnsafeSlice :: Int
                     -> Int
                     -> v s a
                     -> v s a
  gmbasicUnsafeWrite :: PrimMonad m => v (PrimState m) a -> Int -> a -> m ()
  gmbasicUnsafeCopy  :: PrimMonad m => v (PrimState m) a
                                    -> v (PrimState m) a
                                    -> m ()
  gmbasicUnsafeNew   :: PrimMonad m => Int -> m (v (PrimState m) a)
  gmbasicUnsafeGrow  :: PrimMonad m => v (PrimState m) a
                                    -> Int
                                    -> m (v (PrimState m) a)

  {-# INLINE gmbasicUnsafeGrow #-}
  gmbasicUnsafeGrow v by
    = do
        v' <- gmbasicUnsafeNew (n+by)
        gmbasicUnsafeCopy (gmbasicUnsafeSlice 0 n v') v
        return v'
    where
      n = gmbasicLength v

-----
-- Data.Vector.Generic.New
-----

data New v a = New (forall s. ST s (Mutable v s a))

newunstream :: GVector v a => Bundle v a -> New v a
{-# INLINE [1] newunstream #-}
newunstream s = s `seq` New (gmvunstream s)

newrun :: New v a -> ST s (Mutable v s a)
{-# INLINE newrun #-}
newrun (New p) = p

-----
-- Control.Monad.Primitive
-----

class Monad m => PrimMonad m where
  type PrimState m
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST
  {-# INLINE primitive #-}

primitive_ :: PrimMonad m
              => (State# (PrimState m) -> State# (PrimState m)) -> m ()
{-# INLINE primitive_ #-}
primitive_ f = primitive (\s# ->
    case f s# of
        s'# -> (# s'#, () #))

-----
-- Data.Primitive.Array
-----

data Array a = Array
  { array# :: Array# a }

data MutableArray s a = MutableArray
  { marray# :: MutableArray# s a }

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

copyMutableArray :: PrimMonad m
          => MutableArray (PrimState m) a
          -> Int
          -> MutableArray (PrimState m) a
          -> Int
          -> Int
          -> m ()
{-# INLINE copyMutableArray #-}
copyMutableArray (MutableArray dst#) (I# doff#)
                 (MutableArray src#) (I# soff#) (I# len#)
  = primitive_ (copyMutableArray# src# soff# dst# doff# len#)

indexArrayM :: Monad m => Array a -> Int -> m a
{-# INLINE indexArrayM #-}
indexArrayM arr (I# i#)
  = case indexArray# (array# arr) i# of (# x #) -> return x

newArray :: PrimMonad m => Int -> a -> m (MutableArray (PrimState m) a)
{-# INLINE newArray #-}
newArray (I# n#) x = primitive
   (\s# -> case newArray# n# x s# of
             (# s'#, arr# #) ->
               let ma = MutableArray arr#
               in (# s'# , ma #))

unsafeFreezeArray :: PrimMonad m => MutableArray (PrimState m) a -> m (Array a)
{-# INLINE unsafeFreezeArray #-}
unsafeFreezeArray arr
  = primitive (\s# -> case unsafeFreezeArray# (marray# arr) s# of
                        (# s'#, arr'# #) ->
                          let a = Array arr'#
                          in (# s'#, a #))

writeArray :: PrimMonad m => MutableArray (PrimState m) a -> Int -> a -> m ()
{-# INLINE writeArray #-}
writeArray arr (I# i#) x = primitive_ (writeArray# (marray# arr) i# x)
