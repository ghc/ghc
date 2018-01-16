{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Vectors.
module Data.Array.Parallel.Unlifted.Distributed.Data.Vector
        (lengthD)
where
import Data.Array.Parallel.Unlifted.Distributed.Data.Scalar
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.Unlifted.Sequential.Vector   as V
import qualified Data.Vector                            as BV
import qualified Data.Vector.Mutable                    as MBV
import qualified Data.Array.Parallel.Base               as B
import Prelude                                          as P
import Control.Monad

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Distributed.Types.Vector." P.++ s

-------------------------------------------------------------------------------
instance Unbox a => DT (V.Vector a) where
  data Dist  (Vector a)   
        = DVector  !(Dist  Int)   
                   !(BV.Vector      (Vector a))

  data MDist (Vector a) s 
        = MDVector !(MDist Int s) 
                   !(MBV.STVector s (Vector a))

  indexD str (DVector _ a) i
   = B.check (here ("indexD[Vector]/" P.++ str)) (BV.length a) i $ a BV.! i
  {-# INLINE_DIST indexD #-}

  newMD g
   = liftM2 MDVector
        (newMD g) 
        (MBV.replicate (gangSize g) (error "MDist (Vector a) - uninitalised"))
  {-# INLINE_DIST newMD #-}

  readMD (MDVector _ marr)
   = MBV.read marr
  {-# INLINE_DIST readMD #-}

  writeMD (MDVector mlen marr) i a 
   = do writeMD mlen i (V.length a)
        MBV.write marr i $! a
  {-# INLINE_DIST writeMD #-}

  unsafeFreezeMD (MDVector len a)
   = liftM2 DVector (unsafeFreezeMD len)
                    (BV.unsafeFreeze a)
  {-# INLINE_DIST unsafeFreezeMD #-}

  sizeD  (DVector  _ a) 
   = BV.length  a
  {-# INLINE_DIST sizeD #-}

  sizeMD (MDVector _ a) 
   = MBV.length a
  {-# INLINE_DIST sizeMD #-}

  measureD xs
   = "Vector " P.++ show (V.length xs)
  {-# NOINLINE measureD #-}
  --  NOINLINE because this is only used for debugging.


instance (Unbox a, Show a) => PprPhysical (Dist (V.Vector a)) where
 pprp (DVector (DInt lengths) chunks)
  = text "DVector"
  $$ (nest 8 $ vcat
        [ text "lengths:" <+> (text $ show $ V.toList lengths)
        , text "chunks: " <+> (text $ show $ BV.toList $ BV.map V.toList chunks) ])
 {-# NOINLINE pprp #-}
 --  NOINLINE because this is only used for debugging.


-------------------------------------------------------------------------------
-- | Yield the distributed length of a distributed array.
lengthD :: Unbox a => Dist (Vector a) -> Dist Int
lengthD (DVector l _) = l
{-# INLINE_DIST lengthD #-}

