{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module T17151a
  ( computeS
  , Stencil
  , P(..)
  , D(..)
  , makeConvolutionStencilFromKernel
  , mapStencil
  , Array
  , Construct(..)
  , Source(..)
  , Load(..)
  , Mutable(..)
  ) where

import Control.Monad.ST
import Data.Functor.Identity
import GHC.STRef
import GHC.ST
import GHC.Exts
import Unsafe.Coerce
import Data.Kind

----  Hacked up stuff to simulate primitive package
class Prim e where
  indexByteArray :: ByteArray -> Int -> e
  sizeOf :: e ->Int
instance Prim Int where
  indexByteArray _ _ = 55
  sizeOf _ = 99

data ByteArray = BA
type MutableByteArray s = STRef s Int

class Monad m => PrimMonad m where
  type PrimState m
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a
instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST

unsafeFreezeByteArray :: PrimMonad m => MutableByteArray (PrimState m) -> m ByteArray
unsafeFreezeByteArray a = return (unsafeCoerce a)

newByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
newByteArray (I# n#)
  = primitive (\s# -> case newMutVar# 33 s# of
                        (# s'#, arr# #) -> (# s'#, STRef arr# #))

writeByteArray :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> e -> m ()
writeByteArray _ _ _ = return ()

-----  End of hacked up stuff

--------------
newtype Stencil ix e a =
  Stencil ((ix -> e) -> ix -> a)

mapStencil :: Source r ix e => Stencil ix e a -> Array r ix e -> Array D ix a
mapStencil (Stencil stencilF) arr = DArray (size arr) (stencilF (unsafeIndex arr))
{-# INLINE mapStencil #-}

makeConvolutionStencilFromKernel
  :: (Source r ix e, Num e)
  => Array r ix e
  -> Stencil ix e e
makeConvolutionStencilFromKernel arr = Stencil stencil
  where
    sz = size arr
    sCenter = liftIndex (`quot` 2) sz
    stencil getVal ix =
      runIdentity $
      loopM 0 (< totalElem sz) (+ 1) 0 $ \i a ->
        pure $ accum a (fromLinearIndex sz i) (unsafeLinearIndex arr i)
      where
        ixOff = liftIndex2 (+) ix sCenter
        accum acc kIx kVal = getVal (liftIndex2 (-) ixOff kIx) * kVal + acc
        {-# INLINE accum #-}
    {-# INLINE stencil #-}
{-# INLINE makeConvolutionStencilFromKernel #-}


computeS :: (Mutable r ix e, Load r' ix e) => Array r' ix e -> Array r ix e
computeS arr = runST $ do
  marr <- unsafeNew (size arr)
  unsafeLoadIntoS marr arr
  unsafeFreeze marr
{-# INLINE computeS #-}


data D = D deriving Show

data instance  Array D ix e = DArray{dSize :: ix,
                                     dIndex :: ix -> e}

instance Index ix => Construct D ix e where
  makeArray _ = DArray
  {-# INLINE makeArray #-}

instance Index ix => Source D ix e where
  unsafeIndex = dIndex
  {-# INLINE unsafeIndex #-}

instance Index ix => Load D ix e where
  size = dSize
  {-# INLINE size #-}
  loadArrayM arr = splitLinearlyWith_ (totalElem (size arr)) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}


data P = P deriving Show

data instance Array P ix e = PArray ix ByteArray

instance (Prim e, Index ix) => Construct P ix e where
  makeArray _ sz f = computeS (makeArray D sz f)
  {-# INLINE makeArray #-}

instance (Prim e, Index ix) => Source P ix e where
  unsafeIndex (PArray sz a) = indexByteArray a . toLinearIndex sz
  {-# INLINE unsafeIndex #-}

instance (Prim e, Index ix) => Mutable P ix e where
  data MArray s P ix e = MPArray ix (MutableByteArray s)
  unsafeFreeze (MPArray sz a) = PArray sz <$> unsafeFreezeByteArray a
  {-# INLINE unsafeFreeze #-}
  unsafeNew sz = MPArray sz <$> newByteArray (totalElem sz * eSize)
    where
      eSize = sizeOf (undefined :: e)
  {-# INLINE unsafeNew #-}
  unsafeLinearWrite (MPArray _ ma) = writeByteArray ma
  {-# INLINE unsafeLinearWrite #-}


instance (Prim e, Index ix) => Load P ix e where
  size (PArray sz _) = sz
  {-# INLINE size #-}
  loadArrayM arr = splitLinearlyWith_ (totalElem (size arr)) (unsafeLinearIndex arr)
  {-# INLINE loadArrayM #-}


unsafeLinearIndex :: Source r ix e => Array r ix e -> Int -> e
unsafeLinearIndex arr = unsafeIndex arr . fromLinearIndex (size arr)
{-# INLINE unsafeLinearIndex #-}


loopM :: Monad m => Int -> (Int -> Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopM init' condition increment initAcc f = go init' initAcc
  where
    go step acc
      | condition step = f step acc >>= go (increment step)
      | otherwise = return acc
{-# INLINE loopM #-}

splitLinearlyWith_ ::
     Monad m => Int -> (Int -> b) -> (Int -> b -> m ()) -> m ()
splitLinearlyWith_ totalLength index write =
  loopM 0 (< totalLength) (+1) () $ \i () -> write i (index i)
{-# INLINE splitLinearlyWith_ #-}


data family Array r ix e :: Type

class Index ix => Construct r ix e where
  makeArray :: r -> ix -> (ix -> e) -> Array r ix e

class Load r ix e => Source r ix e where
  unsafeIndex :: Array r ix e -> ix -> e

class Index ix => Load r ix e where
  size :: Array r ix e -> ix
  loadArrayM :: Monad m => Array r ix e -> (Int -> e -> m ()) -> m ()
  unsafeLoadIntoS ::
       (Mutable r' ix e, PrimMonad m) => MArray (PrimState m) r' ix e -> Array r ix e -> m ()
  unsafeLoadIntoS marr arr = loadArrayM arr (unsafeLinearWrite marr)
  {-# INLINE unsafeLoadIntoS #-}

class (Construct r ix e, Source r ix e) => Mutable r ix e where
  data MArray s r ix e :: Type
  unsafeFreeze :: PrimMonad m => MArray (PrimState m) r ix e -> m (Array r ix e)
  unsafeNew :: PrimMonad m => ix -> m (MArray (PrimState m) r ix e)
  unsafeLinearWrite :: PrimMonad m => MArray (PrimState m) r ix e -> Int -> e -> m ()


class (Eq ix, Ord ix, Show ix) =>
      Index ix
  where
  totalElem :: ix -> Int
  liftIndex2 :: (Int -> Int -> Int) -> ix -> ix -> ix
  liftIndex :: (Int -> Int) -> ix -> ix
  toLinearIndex :: ix -> ix -> Int
  fromLinearIndex :: ix -> Int -> ix

instance Index Int where
  totalElem = id
  toLinearIndex _ = id
  fromLinearIndex _ = id
  liftIndex f = f
  liftIndex2 f = f
