{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}

-- | Distribution of values of primitive types.
module Data.Array.Parallel.Unlifted.Distributed.Primitive.DPrim 
        ( DPrim (..)
        , primIndexD
        , primNewMD
        , primReadMD
        , primWriteMD
        , primUnsafeFreezeMD
        , primSizeD
        , primSizeMD)
        
where
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import Data.Array.Parallel.Unlifted.Sequential.Vector
import Data.Array.Parallel.Base
import Control.Monad
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as V
import qualified Data.Vector.Unboxed.Mutable                    as MV
import Prelude as P

-- DPrim ----------------------------------------------------------------------
-- | For distributed primitive values, we can just store all the members in
--   a vector. The vector has the same length as the number of threads in the gang.
--
class Unbox e => DPrim e where

  -- | Make an immutable distributed value.
  mkDPrim :: V.Vector e -> Dist  e

  -- | Unpack an immutable distributed value back into a vector.
  unDPrim :: Dist  e -> V.Vector e

  -- | Make a mutable distributed value.
  mkMDPrim :: MV.STVector s e -> MDist  e s

  -- | Unpack a mutable distributed value back into a vector.
  unMDPrim :: MDist  e s -> MV.STVector s e


-- | Get the member corresponding to a thread index.
primIndexD :: DPrim a => String -> Dist a -> Int -> a
primIndexD str = (V.index (str P.++ "/primIndexD")) . unDPrim
{-# INLINE primIndexD #-}


-- | Create a new distributed value, having as many members as threads
--   in the given 'Gang'.
primNewMD :: DPrim a => Gang -> ST s (MDist a s)
primNewMD = liftM mkMDPrim . MV.new . gangSize
{-# INLINE primNewMD #-}


-- | Read the member of a distributed value corresponding to the given thread index.
primReadMD :: DPrim a => MDist a s -> Int -> ST s a
primReadMD = MV.read . unMDPrim
{-# INLINE primReadMD #-}


-- | Write the member of a distributed value corresponding to the given thread index.
primWriteMD :: DPrim a => MDist a s -> Int -> a -> ST s ()
primWriteMD = MV.write . unMDPrim
{-# INLINE primWriteMD #-}


-- | Freeze a mutable distributed value to an immutable one.
--   You promise not to update the mutable one any further.
primUnsafeFreezeMD :: DPrim a => MDist a s -> ST s (Dist a)
primUnsafeFreezeMD = liftM mkDPrim . V.unsafeFreeze . unMDPrim
{-# INLINE primUnsafeFreezeMD #-}


-- | Get the size of a distributed value, that is, the number of threads
--   in the gang that it was created for.
primSizeD :: DPrim a => Dist a -> Int
primSizeD = V.length . unDPrim
{-# INLINE primSizeD #-}


-- | Get the size of a distributed mutable value, that is, the number of threads
--   in the gang it was created for.
primSizeMD :: DPrim a => MDist a s -> Int
primSizeMD = MV.length . unMDPrim
{-# INLINE primSizeMD #-}

