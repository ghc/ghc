{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vector.Hybrid.Internal (Vector) where

import Control.Monad (liftM2)
import Data.Functor.Identity (Identity(..))
import GHC.ST (ST, runST)
import Text.Read (ReadPrec, readPrec)
import Data.Kind (Type)

-----

class Monad m => PrimMonad m where
  type PrimState m

instance PrimMonad (ST s) where
  type PrimState (ST s) = s

class GMVector v a where
  gmbasicLength      :: v s a -> Int
  gmbasicUnsafeSlice :: Int -> Int -> v s a -> v s a
  gmbasicUnsafeNew   :: PrimMonad m => Int -> m (v (PrimState m) a)
  gmbasicUnsafeWrite :: PrimMonad m => v (PrimState m) a -> Int -> a -> m ()

type family GMutable (v :: Type -> Type) :: Type -> Type -> Type

class GMVector (GMutable v) a => GVector v a where
  gbasicUnsafeFreeze :: PrimMonad m => GMutable v (PrimState m) a -> m (v a)

data Step s a where
  Yield :: a -> s -> Step s a

instance Functor (Step s) where
  {-# INLINE fmap #-}
  fmap f (Yield x s) = Yield (f x) s

data Stream m a = forall s. Stream (s -> m (Step s a)) s
data Chunk v a = Chunk Int (forall m. (PrimMonad m, GVector v a) => GMutable v (PrimState m) a -> m ())
data New v a = New { newrun :: forall s. ST s (GMutable v s a) }
type MBundle m v a = Stream m (Chunk v a)
type Bundle v a = MBundle Identity v a

mbfromStream :: Monad m => Stream m a -> MBundle m v a
{-# INLINE mbfromStream #-}
mbfromStream (Stream step t) = Stream step' t
  where
    step' s = do r <- step s
                 return $ fmap (\x -> Chunk 1 (\v -> gmbasicUnsafeWrite v 0 x)) r

mbunsafeFromList :: Monad m => [a] -> MBundle m v a
{-# INLINE [1] mbunsafeFromList #-}
mbunsafeFromList xs = mbfromStream (sfromList xs)

blift :: Monad m => Bundle v a -> MBundle m v a
{-# INLINE [1] blift #-}
blift (Stream vstep t) = Stream (return . runIdentity . vstep) t

sfromList :: Monad m => [a] -> Stream m a
{-# INLINE sfromList #-}
sfromList zs = Stream step zs
  where
    step (x:xs) = return (Yield x xs)
    step _ = undefined

sfoldlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE [1] sfoldlM #-}
sfoldlM m w (Stream step t) = foldlM_loop w t
  where
    foldlM_loop z s
      = do
          r <- step s
          case r of
            Yield x s' -> do { z' <- m z x; foldlM_loop z' s' }

gmvunstream :: (PrimMonad m, GVector v a)
            => Bundle v a -> m (GMutable v (PrimState m) a)
{-# INLINE [1] gmvunstream #-}
gmvunstream s = gmvmunstreamUnknown (blift s)

gmvmunstreamUnknown :: (PrimMonad m, GVector v a)
                    => MBundle m v a -> m (GMutable v (PrimState m) a)
{-# INLINE gmvmunstreamUnknown #-}
gmvmunstreamUnknown s
  = do
      v <- gmbasicUnsafeNew 0
      (_, _) <- sfoldlM copyChunk (v,0) s
      return undefined
  where
    {-# INLINE [0] copyChunk #-}
    copyChunk (v,i) (Chunk n f)
      = do
          let j = i+n
          v' <- if gmbasicLength v < j
                  then gmbasicUnsafeNew undefined
                  else return v
          f (gmbasicUnsafeSlice i n v')
          return (v',j)

newunstream :: GVector v a => Bundle v a -> New v a
{-# INLINE [1] newunstream #-}
newunstream s = s `seq` New (gmvunstream s)

gnew :: GVector v a => New v a -> v a
{-# INLINE [1] gnew #-}
gnew m = m `seq` runST (gbasicUnsafeFreeze =<< newrun m)

gunstream :: GVector v a => Bundle v a -> v a
{-# INLINE gunstream #-}
gunstream s = gnew (newunstream s)

gfromList :: GVector v a => [a] -> v a
{-# INLINE gfromList #-}
gfromList = gunstream . mbunsafeFromList

greadPrec :: (GVector v a, Read a) => ReadPrec (v a)
{-# INLINE greadPrec #-}
greadPrec = do
  xs <- readPrec
  return (gfromList xs)

-----

data MVector :: (Type -> Type -> Type) ->
                (Type -> Type -> Type) ->
                (Type -> Type -> Type) where
  MV :: !(u s a) -> !(v s b) -> MVector u v s (a, b)

instance (GMVector u a, GMVector v b) => GMVector (MVector u v) (a, b) where
  gmbasicLength (MV ks _) = gmbasicLength ks
  gmbasicUnsafeSlice s e (MV ks vs) = MV (gmbasicUnsafeSlice s e ks) (gmbasicUnsafeSlice s e vs)

  gmbasicUnsafeNew n = liftM2 MV (gmbasicUnsafeNew n) (gmbasicUnsafeNew n)
  -- Removing this INLINE pragma makes it compile
  {-# INLINE gmbasicUnsafeNew #-}

  gmbasicUnsafeWrite (MV ks vs) n (k,v) = do
    gmbasicUnsafeWrite ks n k
    gmbasicUnsafeWrite vs n v

data Vector :: (Type -> Type) -> (Type -> Type) -> Type -> Type

type instance GMutable (Vector u v) = MVector (GMutable u) (GMutable v)

instance (GVector u a, GVector v b) => GVector (Vector u v) (a, b) where
  gbasicUnsafeFreeze = undefined

instance (GVector u a, GVector v b, Read a, Read b, c ~ (a, b)) => Read (Vector u v c) where
  readPrec = greadPrec
