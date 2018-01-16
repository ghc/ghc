{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Maybes.
module Data.Array.Parallel.Unlifted.Distributed.Data.Maybe 
where
import Data.Array.Parallel.Unlifted.Distributed.Data.Bool       ()
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DPrim ()
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DT
import Control.Monad


instance DT a => DT (Maybe a) where
  data Dist  (Maybe a)   = DMaybe  !(Dist  Bool)   !(Dist  a)
  data MDist (Maybe a) s = MDMaybe !(MDist Bool s) !(MDist a s)

  indexD str (DMaybe bs as) i
    |        indexD (str ++ "/indexD[Maybe]") bs i
    = Just $ indexD (str ++ "/indexD[Maybe]" ++ str) as i

    | otherwise           = Nothing
  {-# INLINE_DIST indexD #-}

  newMD g
   = liftM2 MDMaybe (newMD g) (newMD g)
  {-# INLINE_DIST newMD #-}

  readMD (MDMaybe bs as) i 
   = do b <- readMD bs i
        if b then liftM Just $ readMD as i
             else return Nothing
  {-# INLINE_DIST readMD #-}

  writeMD (MDMaybe bs _) i Nothing 
   = writeMD bs i False

  writeMD (MDMaybe bs as) i (Just x)
   = do writeMD bs i True
        writeMD as i x
  {-# INLINE_DIST writeMD #-}

  unsafeFreezeMD (MDMaybe bs as)
   = liftM2 DMaybe (unsafeFreezeMD bs)
                   (unsafeFreezeMD as)
  {-# INLINE_DIST unsafeFreezeMD #-}

  deepSeqD Nothing  z   = z
  deepSeqD (Just x) z   = deepSeqD x z
  {-# INLINE_DIST deepSeqD #-}

  sizeD  (DMaybe  b _)  
   = sizeD  b
  {-# INLINE_DIST sizeD #-}

  sizeMD (MDMaybe b _)  
   = sizeMD b
  {-# INLINE_DIST sizeMD #-}

  measureD Nothing      = "Nothing"
  measureD (Just x)     = "Just (" ++ measureD x ++ ")"
  {-# NOINLINE measureD #-}
  --  NOINLINE because this is only used for debugging.
