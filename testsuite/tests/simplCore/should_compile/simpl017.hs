{-# OPTIONS -XImpredicativeTypes -fno-warn-deprecated-flags -XEmptyDataDecls -XGADTs -XLiberalTypeSynonyms -XFlexibleInstances -XScopedTypeVariables #-}

-- See Trac #1627.  The point is that we should get nice
-- 		    compact code for Foo

-- In GHC 7.0 this fails, and rightly so.

module M(foo) where

import Control.Monad.ST
import Data.Array.ST

data E' v m a where
    E :: m a -> E' RValue m a
    V :: m a -> (a -> m ()) -> E' v m a

data LValue
data RValue

type E m a = E' RValue m a
type V m a = E' LValue m a

{-# INLINE runE #-}
runE :: E' v m a -> m a
runE (E t) = t
runE (V t _) = t

instance (Monad m) => Monad (E' RValue m) where
    {-# INLINE return #-}
    return x = E $ return x
    {-# INLINE (>>=) #-}
    x >>= f = E $ do
        x' <- runE x
        runE (f x')

liftArray :: forall arr m a i . (Ix i, MArray arr a m) =>
             arr i a -> E m (forall v . [E m i] -> E' v m a)
{-# INLINE liftArray #-}
liftArray a = E (do
    let ix :: [E m i] -> m i
        ix [i] = runE i
	{-# INLINE f #-}
	f is = V (ix is >>= readArray a) (\ x -> ix is >>= \ i -> writeArray a i x)
    return f
  )

{-# INLINE liftE2 #-}
liftE2 :: (Monad m) => (a -> b -> c) -> E' va m a -> E' vb m b -> E m c
liftE2 op x y = E $ do
    x' <- runE x
    y' <- runE y
    return (x' `op` y')

{-# INLINE plus #-}
plus :: (Monad m) => E m Int -> E m Int -> E m Int
plus = liftE2 (+)

foo :: forall s . STArray s Int Int -> ST s Int
foo ma = runE $ do
    a <- liftArray ma
    let one :: E (ST t) Int
        one = return 1
    a[one] `plus` a[one]

