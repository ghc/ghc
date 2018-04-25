{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of unit values.
module Data.Array.Parallel.Unlifted.Distributed.Data.Unit 
        (unitD)
where
import Data.Array.Parallel.Unlifted.Distributed.Primitive
import Data.Array.Parallel.Base

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Distributed.Types.Unit." ++ s


instance DT () where
  data Dist ()    = DUnit  !Int
  data MDist () s = MDUnit !Int

  indexD str (DUnit n) i
   = check (str ++ "/indexD[Unit]") n i ()
  {-# INLINE_DIST indexD #-}

  newMD
   = return . MDUnit . gangSize
  {-# INLINE_DIST newMD #-}

  readMD   (MDUnit n) i
   = check (here "readMD")  n i
   $ return ()
  {-# INLINE_DIST readMD #-}

  writeMD  (MDUnit n) i ()
   = check (here "writeMD") n i
   $ return ()
  {-# INLINE_DIST writeMD #-}

  unsafeFreezeMD (MDUnit n)
   = return $ DUnit n
  {-# INLINE_DIST unsafeFreezeMD #-}

  sizeD  
   = error $ here "sizeD  undefined"
  {-# NOINLINE sizeD #-}
  --  NOINLINE because this is only used for debugging.

  sizeMD 
   = error $ here "sizeMD undefined"
  {-# NOINLINE sizeMD #-}
  --  NOINLINE because this is only used for debugging.


-- | Yield a distributed unit.
unitD :: Gang -> Dist ()
unitD = DUnit . gangSize
{-# INLINE_DIST unitD #-}
