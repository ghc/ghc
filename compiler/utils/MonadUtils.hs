
-- | Utilities related to Monad and Applicative classes
--   Mostly for backwards compatability.

module MonadUtils
        ( Applicative(..)
        , (<$>)

        , MonadFix(..)
        , MonadIO(..)

        , liftIO1, liftIO2, liftIO3, liftIO4

        , zipWith3M
        , mapAndUnzipM, mapAndUnzip3M, mapAndUnzip4M
        , mapAccumLM
        , mapSndM
        , concatMapM
        , mapMaybeM
        , fmapMaybeM, fmapEitherM
        , anyM, allM
        , foldlM, foldlM_, foldrM
        , maybeMapM
        ) where

-------------------------------------------------------------------------------
-- Detection of available libraries
-------------------------------------------------------------------------------

-- we don't depend on MTL for now
#define HAVE_MTL 0

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Maybes

import Control.Applicative
#if HAVE_MTL
import Control.Monad.Trans
#endif
import Control.Monad
import Control.Monad.Fix

-------------------------------------------------------------------------------
-- MTL
-------------------------------------------------------------------------------

#if !HAVE_MTL

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where liftIO = id
#endif

-------------------------------------------------------------------------------
-- Lift combinators
--  These are used throughout the compiler
-------------------------------------------------------------------------------

-- | Lift an 'IO' operation with 1 argument into another monad
liftIO1 :: MonadIO m => (a -> IO b) -> a -> m b
liftIO1 = (.) liftIO

-- | Lift an 'IO' operation with 2 arguments into another monad
liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 = ((.).(.)) liftIO

-- | Lift an 'IO' operation with 3 arguments into another monad
liftIO3 :: MonadIO m => (a -> b -> c -> IO d) -> a -> b -> c -> m d
liftIO3 = ((.).((.).(.))) liftIO

-- | Lift an 'IO' operation with 4 arguments into another monad
liftIO4 :: MonadIO m => (a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> m e
liftIO4 = (((.).(.)).((.).(.))) liftIO

-------------------------------------------------------------------------------
-- Common functions
--  These are used throughout the compiler
-------------------------------------------------------------------------------

zipWith3M :: Monad m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M _ []     _      _      = return []
zipWith3M _ _      []     _      = return []
zipWith3M _ _      _      []     = return []
zipWith3M f (x:xs) (y:ys) (z:zs)
  = do { r  <- f x y z
       ; rs <- zipWith3M f xs ys zs
       ; return $ r:rs
       }

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

-- | Monadic version of mapMaybe
mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = liftM catMaybes . mapM f

-- | Monadic version of fmap
fmapMaybeM :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
fmapMaybeM _ Nothing  = return Nothing
fmapMaybeM f (Just x) = f x >>= (return . Just)

-- | Monadic version of fmap
fmapEitherM :: Monad m => (a -> m b) -> (c -> m d) -> Either a c -> m (Either b d)
fmapEitherM fl _ (Left  a) = fl a >>= (return . Left)
fmapEitherM _ fr (Right b) = fr b >>= (return . Right)

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

-- | Monadic version of foldl that discards its result
foldlM_ :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ()
foldlM_ = foldM_

-- | Monadic version of foldr
foldrM        :: (Monad m) => (b -> a -> m a) -> a -> [b] -> m a
foldrM _ z []     = return z
foldrM k z (x:xs) = do { r <- foldrM k z xs; k x r }

-- | Monadic version of fmap specialised for Maybe
maybeMapM :: Monad m => (a -> m b) -> (Maybe a -> m (Maybe b))
maybeMapM _ Nothing  = return Nothing
maybeMapM m (Just x) = liftM Just $ m x
