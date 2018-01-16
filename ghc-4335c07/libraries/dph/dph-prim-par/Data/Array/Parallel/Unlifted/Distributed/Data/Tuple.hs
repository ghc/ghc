{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Tuples
module Data.Array.Parallel.Unlifted.Distributed.Data.Tuple 
        ( -- * Pairs
          zipD, unzipD, fstD, sndD
        
           -- * Triples
        , zip3D, unzip3D)
where
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DT
import Data.Array.Parallel.Base
import Data.Array.Parallel.Pretty
import Control.Monad

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Distributed.Types.Tuple." ++ s


-- Pairs ----------------------------------------------------------------------
instance (DT a, DT b) => DT (a,b) where
  data Dist  (a,b)   = DProd  !(Dist a)    !(Dist b)
  data MDist (a,b) s = MDProd !(MDist a s) !(MDist b s)

  indexD str d i
   = ( indexD (str ++ "/indexD[Tuple2]") (fstD d) i
     , indexD (str ++ "/indexD[Tuple2]") (sndD d) i)
  {-# INLINE_DIST indexD #-}

  newMD g
   = liftM2 MDProd (newMD g) (newMD g)
  {-# INLINE_DIST newMD #-}

  readMD  (MDProd xs ys) i
   = liftM2 (,) (readMD xs i) (readMD ys i)
  {-# INLINE_DIST readMD #-}

  writeMD (MDProd xs ys) i (x,y)
   = do writeMD xs i x
        writeMD ys i y
  {-# INLINE_DIST writeMD #-}

  unsafeFreezeMD (MDProd xs ys)
   = liftM2 DProd (unsafeFreezeMD xs)
                  (unsafeFreezeMD ys)
  {-# INLINE_DIST unsafeFreezeMD #-}

  deepSeqD (x, y) z 
   = deepSeqD x (deepSeqD y z)
  {-# INLINE deepSeqD #-}

  sizeD  (DProd  x _) 
   = sizeD  x
  {-# INLINE_DIST sizeD #-}

  sizeMD (MDProd x _) 
   = sizeMD x
  {-# INLINE_DIST sizeMD #-}

  measureD (x, y) 
   = "Pair " ++ "(" ++ measureD x ++ ") (" ++  measureD y ++ ")"
  {-# NOINLINE measureD #-}
  --  NOINLINE beacuse this is only used during debugging.


instance (PprPhysical (Dist a), PprPhysical (Dist b)) 
        => PprPhysical (Dist (a, b)) where
 pprp (DProd xs ys)
  = text "DProd"
  $$ (nest 8 $ vcat
        [ pprp xs
        , pprp ys ])
 {-# NOINLINE pprp #-}
 --  NOINLINE because this is only used during debugging.


-- | Pairing of distributed values.
--   The two values must belong to the same 'Gang'.
zipD :: (DT a, DT b) => Dist a -> Dist b -> Dist (a,b)
zipD !x !y 
        = checkEq (here "zipDT") "Size mismatch" (sizeD x) (sizeD y) 
        $ DProd x y
{-# INLINE [0] zipD #-}                                                 -- TODO: why is this INLINE [0]???


-- | Unpairing of distributed values.
unzipD :: (DT a, DT b) => Dist (a,b) -> (Dist a, Dist b)
unzipD (DProd dx dy) = (dx,dy)
{-# INLINE_DIST unzipD #-}


-- | Extract the first elements of a distributed pair.
fstD :: (DT a, DT b) => Dist (a,b) -> Dist a
fstD = fst . unzipD
{-# INLINE_DIST fstD #-}


-- | Extract the second elements of a distributed pair.
sndD :: (DT a, DT b) => Dist (a,b) -> Dist b
sndD = snd . unzipD
{-# INLINE_DIST sndD #-}


-- Triples --------------------------------------------------------------------
instance (DT a, DT b, DT c) => DT (a,b,c) where
  data Dist  (a,b,c)   = DProd3  !(Dist a)    !(Dist b)    !(Dist c)
  data MDist (a,b,c) s = MDProd3 !(MDist a s) !(MDist b s) !(MDist c s)

  indexD str (DProd3 xs ys zs) i
   = ( indexD (here $ "indexD[Tuple3]/" ++ str) xs i
     , indexD (here $ "indexD[Tuple3]/" ++ str) ys i
     , indexD (here $ "indexD[Tuple3]/" ++ str) zs i)
  {-# INLINE_DIST indexD #-}

  newMD g
   = liftM3 MDProd3 (newMD g) (newMD g) (newMD g)
  {-# INLINE_DIST newMD #-}

  readMD  (MDProd3 xs ys zs) i
   = liftM3 (,,) (readMD xs i) (readMD ys i) (readMD zs i)
  {-# INLINE_DIST readMD #-}

  writeMD (MDProd3 xs ys zs) i (x,y,z)
   = do writeMD xs i x
        writeMD ys i y
        writeMD zs i z
  {-# INLINE_DIST writeMD #-}

  unsafeFreezeMD (MDProd3 xs ys zs)
   = liftM3 DProd3 (unsafeFreezeMD xs)
                   (unsafeFreezeMD ys)
                   (unsafeFreezeMD zs)
  {-# INLINE_DIST unsafeFreezeMD #-}

  deepSeqD (x,y,z) k 
   = deepSeqD x (deepSeqD y (deepSeqD z k))
  {-# INLINE_DIST deepSeqD #-}

  sizeD  (DProd3  x _ _) 
   = sizeD  x
  {-# INLINE_DIST sizeD #-}

  sizeMD (MDProd3 x _ _) 
   = sizeMD x
  {-# INLINE_DIST sizeMD #-}

  measureD (x,y,z)
   = "Triple " 
        ++ "(" ++ measureD x ++ ") "
        ++ "(" ++ measureD y ++ ") "
        ++ "(" ++ measureD z ++ ")"
  {-# NOINLINE measureD #-}
  --  NOINLINE because this is only used for debugging.


-- | Pairing of distributed values.
-- /The two values must belong to the same/ 'Gang'.
zip3D   :: (DT a, DT b, DT c) => Dist a -> Dist b -> Dist c -> Dist (a,b,c)
zip3D !x !y !z
        = checkEq (here "zip3DT") "Size mismatch" (sizeD x) (sizeD y) 
        $ checkEq (here "zip3DT") "Size mismatch" (sizeD x) (sizeD z) 
        $ DProd3 x y z
{-# INLINE [0] zip3D #-}                                                 -- TODO: Why is this INLINE[0]??


-- | Unpairing of distributed values.
unzip3D  :: (DT a, DT b, DT c) => Dist (a,b,c) -> (Dist a, Dist b, Dist c)
unzip3D (DProd3 dx dy dz) 
        = (dx,dy,dz)
{-# INLINE_DIST unzip3D #-}

