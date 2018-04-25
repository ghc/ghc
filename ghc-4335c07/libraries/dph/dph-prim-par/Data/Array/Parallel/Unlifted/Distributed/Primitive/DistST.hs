
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Distributed ST computations.
--
--  Computations of type 'DistST' are data-parallel computations which
--  are run on each thread of a gang. At the moment, they can only access the
--  element of a (possibly mutable) distributed value owned by the current
--  thread.
--
-- /TODO:/ Add facilities for implementing parallel scans etc.
--
--  TODO: 
--
module Data.Array.Parallel.Unlifted.Distributed.Primitive.DistST 
        ( DistST

          -- * Primitives.
        , stToDistST
        , distST_, distST
        , runDistST, runDistST_seq
        , myIndex
        , myD
        , readMyMD, writeMyMD

          -- * Monadic combinators
        , mapDST_, mapDST, zipWithDST_, zipWithDST)
where
import qualified Data.Array.Parallel.Unlifted.Distributed.What as W
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DT
import Data.Array.Parallel.Unlifted.Distributed.Primitive.Gang
import Data.Array.Parallel.Unlifted.Distributed.Data.Tuple
import Data.Array.Parallel.Base (ST, runST)
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)


-- | Data-parallel computations.
--   When applied to a thread gang, the computation implicitly knows the index
--   of the thread it's working on. Alternatively, if we know the thread index
--   then we can make a regular ST computation.
newtype DistST s a = DistST { unDistST :: Int -> ST s a }

instance Functor (DistST s) where
  fmap = liftM

instance Applicative (DistST s) where
  pure = return
  (<*>) = ap

instance Monad (DistST s) where
  {-# INLINE return #-}
  return         = DistST . const . return 

  {-# INLINE (>>=) #-}
  DistST p >>= f = DistST $ \i -> do
                                    x <- p i
                                    unDistST (f x) i


-- Primitives -----------------------------------------------------------------
-- | Yields the index of the current thread within its gang.
myIndex :: DistST s Int
myIndex         = DistST return
{-# INLINE myIndex #-}


-- | Lifts an 'ST' computation into the 'DistST' monad.
--   The lifted computation should be data parallel.
stToDistST :: ST s a -> DistST s a
stToDistST p    = DistST $ \_ -> p
{-# INLINE stToDistST #-}


-- | Yields the 'Dist' element owned by the current thread.
myD :: DT a => Dist a -> DistST s a
myD dt = liftM (indexD "myD" dt) myIndex
{-# NOINLINE myD #-}


-- | Yields the 'MDist' element owned by the current thread.
readMyMD :: DT a => MDist a s -> DistST s a
readMyMD mdt 
 = do   i <- myIndex
        stToDistST $ readMD mdt i
{-# NOINLINE readMyMD #-}


-- | Writes the 'MDist' element owned by the current thread.
writeMyMD :: DT a => MDist a s -> a -> DistST s ()
writeMyMD mdt x 
 = do   i <- myIndex
        stToDistST $ writeMD mdt i x
{-# NOINLINE writeMyMD #-}



-- Running --------------------------------------------------------------------
-- | Run a data-parallel computation, yielding the distributed result.
runDistST :: DT a => W.Comp -> Gang -> (forall s. DistST s a) -> Dist a
runDistST comp g p 
 = runST $ distST comp g p
{-# NOINLINE runDistST #-}


runDistST_seq 
        :: forall a. DT a 
        => Gang -> (forall s. DistST s a) -> Dist a
runDistST_seq g p 
 = runST 
 $ do
     md <- newMD g
     go md 0
     unsafeFreezeMD md
  where
    !n = gangSize g
    go :: forall s. MDist a s -> Int -> ST s ()
    go md i | i < n     = do
                            writeMD md i =<< unDistST p i
                            go md (i+1)
            | otherwise = return ()
{-# NOINLINE runDistST_seq #-}


-- | Execute a data-parallel computation, yielding the distributed result.
distST  :: DT a 
        => W.Comp -> Gang 
        -> DistST s a -> ST s (Dist a)
distST comp g p 
 = do   md <- newMD g

        distST_ comp g 
         $ writeMyMD md =<< p

        unsafeFreezeMD md
{-# INLINE distST #-}


-- | Execute a data-parallel computation on a 'Gang'.
--   The same DistST comutation runs on each thread.
distST_ :: W.Comp -> Gang -> DistST s () -> ST s ()
distST_ comp gang proc
        = gangST gang 
                (show comp) 
                (workloadOfComp comp)
        $ unDistST proc
{-# INLINE distST_ #-}

workloadOfComp :: W.Comp -> Workload
workloadOfComp cc
 = case cc of
        W.CDist w               -> workloadOfWhat w
        _                       -> WorkUnknown

workloadOfWhat :: W.What -> Workload
workloadOfWhat ww
 = case ww of
        W.WJoinCopy elems       -> WorkCopy elems 
        _                       -> WorkUnknown

-- Combinators ----------------------------------------------------------------
-- Versions that work on DistST -----------------------------------------------
-- NOTE: The following combinators must be strict in the Dists because if they
-- are not, the Dist might be evaluated (in parallel) when it is requested in
-- the current computation which, again, is parallel. This would break our
-- model andlead to a deadlock. Hence the bangs.

mapDST  :: (DT a, DT b) 
        => W.What -> Gang -> (a -> DistST s b) -> Dist a -> ST s (Dist b)
mapDST what g p !d 
 = mapDST' what g (\x -> x `deepSeqD` p x) d
{-# INLINE mapDST #-}


mapDST_ :: DT a => W.What -> Gang -> (a -> DistST s ()) -> Dist a -> ST s ()
mapDST_ what g p !d 
 = mapDST_' what g (\x -> x `deepSeqD` p x) d
{-# INLINE mapDST_ #-}


mapDST' :: (DT a, DT b) => W.What -> Gang -> (a -> DistST s b) -> Dist a -> ST s (Dist b)
mapDST' what g p !d 
 = distST (W.CDist what) g (myD d >>= p)
{-# INLINE mapDST' #-}


mapDST_' 
        :: DT a 
        => W.What -> Gang -> (a -> DistST s ()) -> Dist a -> ST s ()
mapDST_' what g p !d 
 = distST_ (W.CDist what) g (myD d >>= p)
{-# INLINE mapDST_' #-}


zipWithDST 
        :: (DT a, DT b, DT c)
        => W.What 
        -> Gang
        -> (a -> b -> DistST s c) -> Dist a -> Dist b -> ST s (Dist c)
zipWithDST what g p !dx !dy 
 = mapDST what g (uncurry p) (zipD dx dy)
{-# INLINE zipWithDST #-}


zipWithDST_ 
        :: (DT a, DT b)
        => W.What -> Gang -> (a -> b -> DistST s ()) -> Dist a -> Dist b -> ST s ()
zipWithDST_ what g p !dx !dy 
 = mapDST_ what g (uncurry p) (zipD dx dy)
{-# INLINE zipWithDST_ #-}



