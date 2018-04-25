{-# LANGUAGE CPP, NoMonomorphismRestriction #-}
#include "fusion-phases.h"
module Data.Array.Parallel.Unlifted.Stream.Elems
        ( streamElemsFromVector
        , streamElemsFromVectors
        , streamElemsFromVectorsVSegd)
where
import Data.Array.Parallel.Unlifted.Stream.Ixs
import Data.Vector.Fusion.Bundle.Monadic                         (Bundle(..), fromStream)
import Data.Vector.Fusion.Stream.Monadic                         (Stream(..), Step(..))
import Data.Array.Parallel.Unlifted.Sequential.Vector            (Unbox, Vector)
import Data.Array.Parallel.Unlifted.Vectors                      (Unboxes, Vectors)
import Data.Array.Parallel.Unlifted.Sequential.UVSegd            (UVSegd(..))
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector  as U
import qualified Data.Array.Parallel.Unlifted.Vectors            as US
import qualified Data.Array.Parallel.Unlifted.Sequential.UVSegd  as UVSegd

-- | Take a stream of indices, 
--    look them up from a vector, 
--    and produce a stream of elements.
streamElemsFromVector
        :: (Monad m, Unbox a)
        => Vector a -> Bundle m v Int -> Bundle m v a

streamElemsFromVector vector Bundle{sElems=Stream mkStep s0,sSize=size0}
 = vector `seq` fromStream (Stream mkStep' s0) size0
 where
        {-# INLINE_INNER mkStep' #-}
        mkStep' s
         = do   step    <- mkStep s
                case step of
                 Yield ix s'
                  -> let !result = U.index "streamElemsFromVector" vector ix
                     in  return $ Yield result s'
                 
                 Skip s'        -> return $ Skip s'
                 Done           -> return Done
{-# INLINE_STREAM streamElemsFromVector #-}


-- | Take a stream of chunk and chunk element indices, 
--    look them up from some vectors, 
--    and produce a stream of elements.
streamElemsFromVectors 
        :: (Monad m, Unboxes a) 
        => Vectors a -> Bundle m v (Int, Int) -> Bundle m v a

streamElemsFromVectors vectors Bundle{sElems=Stream mkStep s0,sSize=size0}
 = vectors `seq` fromStream (Stream mkStep' s0) size0
  where
        {-# INLINE_INNER mkStep' #-}
        mkStep' s
         = do   step    <- mkStep s
                case step of
                 Yield (ix1, ix2) s' 
                  -> let !result = US.unsafeIndex2 vectors ix1 ix2
                     in  return $ Yield result s'

                 Skip s'        -> return $ Skip s'
                 Done           -> return Done
{-# INLINE_STREAM streamElemsFromVectors #-}


-- | Take a stream of virtual segment ids and element indices, 
--   pass them through a `UVSegd` to get physical segment and element indices, 
--   and produce a stream of elements.
streamElemsFromVectorsVSegd
        :: (Monad m, Unboxes a)
        => Vectors a -> UVSegd -> Bundle m v (Int, Int) -> Bundle m v a

streamElemsFromVectorsVSegd vectors uvsegd vsrcixs
 = let  -- Because we're just doing indexing here, we don't need the culled
        -- vsegids or ussegd, and can just use the redundant version.
        vsegids  = UVSegd.takeVSegidsRedundant uvsegd
        ussegd   = UVSegd.takeUSSegdRedundant  uvsegd
   in   streamElemsFromVectors        vectors
         $ streamSrcIxsThroughUSSegd  ussegd
         $ streamSrcIxsThroughVSegids vsegids
         $ vsrcixs
{-# INLINE_STREAM streamElemsFromVectorsVSegd #-}
