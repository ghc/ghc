{-# LANGUAGE BangPatterns, GADTs, ExistentialQuantification #-}
{-# OPTIONS_GHC -cpp #-}

module Main where


import Data.Int
import GHC.Types


foo :: Int -> Int -> IO Int
foo = \i j -> sfoldl' (+) 0 $ xs i j +++ ys i j
  where xs k l = senumFromStepN k l 200000
        ys k l = senumFromStepN k l 300000
        {-# Inline xs #-}
        {-# Inline ys #-}
{-# Inline foo #-}


-- We narrow the result to 32-bits to account for the fact that this overflows
-- on 32-bit machines.
main = do { n <- foo 1 1; print (fromIntegral n :: Int32) }



-------------------------------------------------------------------------------
-- vector junk
-------------------------------------------------------------------------------

#define PHASE_FUSED [1]
#define PHASE_INNER [0]

#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER

data Stream m a = forall s. Stream (s -> m (Step s a)) s

data Step s a where
  Yield :: a -> s -> Step s a
  Skip  :: s -> Step s a
  Done  :: Step s a

senumFromStepN :: (Num a, Monad m) => a -> a -> Int -> Stream m a
{-# INLINE_FUSED senumFromStepN #-}
senumFromStepN x y n = x `seq` y `seq` n `seq` Stream step (x,n)
  where
    {-# INLINE_INNER step #-}
    step (w,m) | m > 0     = return $ Yield w (w+y,m-1)
               | otherwise = return $ Done

sfoldl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> m a
{-# INLINE sfoldl' #-}
sfoldl' f = sfoldlM' (\a b -> return (f a b))

sfoldlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE_FUSED sfoldlM' #-}
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

infixr 5 +++
(+++) :: Monad m => Stream m a -> Stream m a -> Stream m a
{-# INLINE_FUSED (+++) #-}
Stream stepa ta +++ Stream stepb tb = Stream step (Left ta)
  where
    {-# INLINE_INNER step #-}
    step (Left  sa) = do
                        r <- stepa sa
                        case r of
                          Yield x sa' -> return $ Yield x (Left  sa')
                          Skip    sa' -> return $ Skip    (Left  sa')
                          Done        -> return $ Skip    (Right tb)
    step (Right sb) = do
                        r <- stepb sb
                        case r of
                          Yield x sb' -> return $ Yield x (Right sb')
                          Skip    sb' -> return $ Skip    (Right sb')
                          Done        -> return $ Done
