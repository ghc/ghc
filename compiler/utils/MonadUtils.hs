
-- | Utilities related to Monad and Applicative classes
--   Mostly for backwards compatability.

module MonadUtils
        ( Applicative(..)
        , (<$>)
        
        , MonadFix(..)
        , MonadIO(..)
        
        , mapAndUnzipM, mapAndUnzip3M, mapAndUnzip4M
        , mapAccumLM
        , mapSndM
        , concatMapM
        , anyM, allM
        , foldlM, foldrM
        ) where

----------------------------------------------------------------------------------------
-- Detection of available libraries
----------------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 606
#define HAVE_APPLICATIVE 1
#else
#define HAVE_APPLICATIVE 0
#endif
-- we don't depend on MTL for now
#define HAVE_MTL 0

----------------------------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------------------------

#if HAVE_APPLICATIVE
import Control.Applicative
#endif
#if HAVE_MTL
import Control.Monad.Trans
#endif
import Control.Monad
import Control.Monad.Fix

----------------------------------------------------------------------------------------
-- Applicative
----------------------------------------------------------------------------------------

#if !HAVE_APPLICATIVE

class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> (f a -> f b)
(<$>) = fmap

infixl 4 <$>
infixl 4 <*>

instance Applicative IO where
	pure = return
	(<*>) = ap

#endif

----------------------------------------------------------------------------------------
-- MTL
----------------------------------------------------------------------------------------

#if !HAVE_MTL

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where liftIO = id
#endif

----------------------------------------------------------------------------------------
-- Common functions
--  These are used throught the compiler
----------------------------------------------------------------------------------------

-- | mapAndUnzipM for triples
mapAndUnzip3M :: Monad m => (a -> m (b,c,d)) -> [a] -> m ([b],[c],[d])
mapAndUnzip3M _ []     = return ([],[],[])
mapAndUnzip3M f (x:xs) = do
    (r1,  r2,  r3)  <- f x
    (rs1, rs2, rs3) <- mapAndUnzip3M f xs
    return (r1:rs1, r2:rs2, r3:rs3)

mapAndUnzip4M :: Monad m => (a -> m (b,c,d,e)) -> [a] -> m ([b],[c],[d],[e])
mapAndUnzip4M _ []     = return ([],[],[],[])
mapAndUnzip4M f (x:xs) = do
    (r1,  r2,  r3,  r4)  <- f x
    (rs1, rs2, rs3, rs4) <- mapAndUnzip4M f xs
    return (r1:rs1, r2:rs2, r3:rs3, r4:rs4)

-- | Monadic version of mapAccumL
mapAccumLM :: Monad m
            => (acc -> x -> m (acc, y)) -- ^ combining funcction
            -> acc                      -- ^ initial state
            -> [x]                      -- ^ inputs
            -> m (acc, [y])             -- ^ final state, outputs
mapAccumLM _ s []     = return (s, [])
mapAccumLM f s (x:xs) = do
    (s1, x')  <- f s x
    (s2, xs') <- mapAccumLM f s1 xs
    return    (s2, x' : xs')

-- | Monadic version of mapSnd
mapSndM :: Monad m => (b -> m c) -> [(a,b)] -> m [(a,c)]
mapSndM _ []         = return []
mapSndM f ((a,b):xs) = do { c <- f b; rs <- mapSndM f xs; return ((a,c):rs) }

-- | Monadic version of concatMap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

-- | Monadic version of 'any', aborts the computation at the first @True@ value
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []     = return False
anyM f (x:xs) = do b <- f x
                   if b then return True 
                        else anyM f xs

-- | Monad version of 'all', aborts the computation at the first @False@ value
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []     = return True
allM f (b:bs) = (f b) >>= (\bv -> if bv then allM f bs else return False)

-- | Monadic version of foldl
foldlM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldlM = foldM

-- | Monadic version of foldr
foldrM        :: (Monad m) => (b -> a -> m a) -> a -> [b] -> m a
foldrM _ z []     = return z
foldrM k z (x:xs) = do { r <- foldrM k z xs; k x r }
