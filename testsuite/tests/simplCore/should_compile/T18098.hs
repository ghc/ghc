{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
module Bug where

import Control.Monad.ST (runST, ST)
import Data.Kind (Type)
import Data.Functor.Identity (Identity(..))

gcons :: (GVector v a) => a -> Stream Identity (Chunk v a) -> v a
gcons x tb = gmvmunstreamUnknown $ sappend (ssingleton x) tb
{-# INLINE gcons #-}

data Chunk v a = MkChunk (forall s. GVector v a => Mutable v s a -> ST s ())

data Step s a = Yield a s | Done

data Stream m a = forall s. Stream (s -> m (Step s a)) s

data Mutable :: (Type -> Type) -> Type -> Type -> Type

class GVector v a where
  gmbasicLength      :: Mutable v s a -> Int
  gmbasicUnsafeSlice :: Mutable v s a -> Mutable v s a
  gmbasicUnsafeNew   :: ST s (Mutable v s a)
  gmbasicUnsafeWrite :: a -> Mutable v s a ->  ST s ()
  gmbasicUnsafeGrow  :: Mutable v s a -> Int -> m (Mutable v s a)
  gbasicUnsafeFreeze :: Mutable v s a -> ST s (v a)

sfoldlM :: (a -> b -> ST s a) -> (t -> Step t b) -> a -> t -> ST s a
sfoldlM m step = foldlM_loop
  where
    foldlM_loop  z s
      = case step s of
            Yield x s' -> do { z' <- m z x; foldlM_loop z' s' }
            Done       -> return z
{-# INLINE [1] sfoldlM #-}

sappend :: Stream Identity a -> Stream Identity a -> Stream Identity a
Stream stepa ta `sappend` Stream stepb _ = Stream step (Left ta)
  where
    {-# INLINE [0] step #-}
    step (Left  sa) = do
                        r <- stepa sa
                        return $ case r of
                          Yield x _ -> Yield x (Left  sa)
                          Done      -> Done
    step (Right sb) = do
                        r <- stepb sb
                        return $ case r of
                          Yield x _ -> Yield x (Right sb)
                          Done      -> Done
{-# INLINE [1] sappend #-}

ssingleton :: Monad m => a -> Stream m (Chunk v a)
ssingleton x = Stream (return . step) True
  where
    {-# INLINE [0] step #-}
    step True  = Yield (MkChunk (gmbasicUnsafeWrite x)) False
    step False = Done
{-# INLINE [1] ssingleton #-}

gmvmunstreamUnknown :: GVector v a => Stream Identity (Chunk v a) -> v a
gmvmunstreamUnknown (Stream vstep u)
  = runST (do
      v <- gmbasicUnsafeNew
      sfoldlM copyChunk (runIdentity . vstep) (v,0) u
      gbasicUnsafeFreeze v)
  where
    {-# INLINE [0] copyChunk #-}
    copyChunk (v,i) (MkChunk f)
      = do
          v' <- gmbasicUnsafeGrow v (gmbasicLength v)
          f (gmbasicUnsafeSlice v')
          return (v',i)
{-# INLINE gmvmunstreamUnknown #-}
