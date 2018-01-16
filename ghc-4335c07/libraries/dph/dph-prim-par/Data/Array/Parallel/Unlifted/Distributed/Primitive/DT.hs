
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
module Data.Array.Parallel.Unlifted.Distributed.Primitive.DT
        ( -- * Distributable Types
          DT(..)
        
          -- * Checking
        , checkGangD
        , checkGangMD

          -- * General Operations
        , newD
        , debugD)
where
import Data.Array.Parallel.Unlifted.Distributed.Primitive.Gang
import Data.Array.Parallel.Base
import Data.List

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Distributed.Primitive.DT" ++ s


-- Distributed Types ----------------------------------------------------------
infixl 9 `indexD`

-- | Class of distributable types. Instances of 'DT' can be
--   distributed across all workers of a 'Gang'. 
--   All such types must be hyperstrict as we do not want to pass thunks
--   into distributed computations.
class DT a where
  data Dist  a
  data MDist a :: * -> *

  -- | Extract a single element of an immutable distributed value.
  indexD         :: String -> Dist a -> Int -> a

  -- | Create an unitialised distributed value for the given 'Gang'.
  --   The gang is used (only) to know how many elements are needed
  --   in the distributed value.
  newMD          :: Gang                  -> ST s (MDist a s)

  -- | Extract an element from a mutable distributed value.
  readMD         :: MDist a s -> Int      -> ST s a

  -- | Write an element of a mutable distributed value.
  writeMD        :: MDist a s -> Int -> a -> ST s ()

  -- | Unsafely freeze a mutable distributed value.
  unsafeFreezeMD :: MDist a s             -> ST s (Dist a)

  -- | Ensure a distributed value is fully evaluated.
  deepSeqD       :: a -> b -> b
  deepSeqD = seq


  -- Debugging ------------------------
  -- | Number of elements in the distributed value.
  -- 
  --   * For debugging only, as code shouldn't be sensitive to the return value.
  sizeD :: Dist a -> Int

  -- | Number of elements in the mutable distributed value.
  --  
  --   * For debugging only, as code shouldn't be sensitive to the return value.
  sizeMD :: MDist a s -> Int

  -- | Show a distributed value.
  --
  --   * For debugging only.
  measureD :: a -> String
  measureD _ = "None"


-- Show -----------------------------------------------------------------------
-- Show instance (for debugging only) --
instance (Show a, DT a) => Show (Dist a) where
  show d = show (Prelude.map (indexD (here "show") d) [0 .. sizeD d - 1])


-- Checking -------------------------------------------------------------------
-- | Check that the sizes of the 'Gang' and of the distributed value match.
checkGangD :: DT a => String -> Gang -> Dist a -> b -> b
checkGangD loc g d v
        = checkEq loc "Wrong gang" (gangSize g) (sizeD d) v


-- | Check that the sizes of the 'Gang' and of the mutable distributed value match.
checkGangMD :: DT a => String -> Gang -> MDist a s -> b -> b
checkGangMD loc g d v
        = checkEq loc "Wrong gang" (gangSize g) (sizeMD d) v


-- Operations -----------------------------------------------------------------
-- | Given a computation that can write its result to a mutable distributed value, 
--   run the computation to generate an immutable distributed value.
newD :: DT a => Gang -> (forall s . MDist a s -> ST s ()) -> Dist a
newD g mkInit =
  runST (do
           mdt <- newMD g
           mkInit mdt
           unsafeFreezeMD mdt)

-- | Show all members of a distributed value.
debugD :: DT a => Dist a -> String
debugD d = "["
         ++ intercalate "," [measureD (indexD (here "debugD") d i) 
                            | i <- [0 .. sizeD d-1]]
         ++ "]"
