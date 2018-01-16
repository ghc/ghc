{-# LANGUAGE CPP, FlexibleContexts #-}

#include "fusion-phases.h"

-- | Definition of the PArray type, and functions that work on it. The PArray
--   type is a PData with an array length. The functions we export from this
--   module are just wrappers for the PD functions from Data.Array.Parallel.PArray.PRepr.
--
--   TODO: Check inconsistent use of INLINE pragmas.
--         Most have INLINE_PA, but bpermutePD and nfPD have plain INLINE
--
module Data.Array.Parallel.PArray.Base (
  PArray(..),
  lengthPA#,
  dataPA#,

  -- These functions have corresponding members in the PR class
  -- from Data.Array.Parallel.PArray.PData.
  emptyPA,
  replicatePA#,
  replicatelPA#,
  repeatPA#,
  indexPA#,
  extractPA#,
  bpermutePA#,
  appPA#,
  applPA#,
  packByTagPA#,
  combine2PA#,
  updatePA#,
  fromListPA#,  fromListPA,
  nfPA,
)
where
import Data.Array.Parallel.Lifted.Unboxed (elementsSegd#)
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.Base           (Tag)
import qualified Data.Array.Parallel.Unlifted as U
import GHC.Exts (Int#, Int(..), (+#), (*#))
import SpecConstr


-- | Lifted\/bulk parallel arrays
--   This contains the array length, along with the element data.
--
{-# ANN type PArray NoSpecConstr #-}
data PArray a = PArray Int# (PData a)


-- | Take the length field of a PArray.
lengthPA# :: PArray a -> Int#
{-# INLINE_PA lengthPA# #-}
lengthPA# (PArray n# _) = n#

-- | Take the data field of a PArray.
dataPA# :: PArray a -> PData a
{-# INLINE_PA dataPA# #-}
dataPA# (PArray _ d) = d


-- PA Wrappers ----------------------------------------------------------------
--  These wrappers work on PArrays. As the PArray contains a PData, we can 
--  can just pass this to the corresponding PD function from 
--  Data.Array.Parallel.PArray.PRepr. However, as a PData doesn't contain 
--  the array length, we need to do the length calculations here.
--
--  Note: There are some more operator# functions that work on PArrays in 
--        "Data.Array.Parallel.PArray.DataInstances". The ones there have 
--        a similar shape but need to know about the underlying representation
--        constructors.
-- 
emptyPA :: PA a => PArray a
{-# INLINE_PA emptyPA #-}
emptyPA
  = PArray 0# emptyPD

replicatePA# :: PA a => Int# -> a -> PArray a
{-# INLINE_PA replicatePA# #-}
replicatePA# n# x
  = PArray n# (replicatePD n# x)

replicatelPA# :: PA a => U.Segd -> PArray a -> PArray a
{-# INLINE_PA replicatelPA# #-}
replicatelPA# segd (PArray _ xs)
  = PArray (elementsSegd# segd) (replicatelPD segd xs)

repeatPA# :: PA a => Int# -> PArray a -> PArray a
{-# INLINE_PA repeatPA# #-}
repeatPA# m# (PArray n# xs) 
  = PArray (m# *# n#) (repeatPD m# n# xs)

indexPA# :: PA a => PArray a -> Int# -> a
{-# INLINE_PA indexPA# #-}
indexPA# (PArray _ xs) i# 
  = indexPD xs i#

extractPA# :: PA a => PArray a -> Int# -> Int# -> PArray a
{-# INLINE_PA extractPA# #-}
extractPA# (PArray _ xs) i# n#
  = PArray n# (extractPD xs i# n#)

bpermutePA# :: PA a => PArray a -> Int# -> U.Array Int -> PArray a
{-# INLINE bpermutePA# #-}
bpermutePA# (PArray _ xs) n# is
  = PArray n# (bpermutePD xs n# is)

appPA# :: PA a => PArray a -> PArray a -> PArray a
{-# INLINE_PA appPA# #-}
appPA# (PArray m# xs) (PArray n# ys)
  = PArray (m# +# n#) (appPD xs ys)

applPA# :: PA a => U.Segd -> U.Segd -> PArray a -> U.Segd -> PArray a -> PArray a
{-# INLINE_PA applPA# #-}
applPA# segd is (PArray m# xs) js (PArray n# ys)
  = PArray (m# +# n#) (applPD segd is xs js ys)

packByTagPA# :: PA a => PArray a -> Int# -> U.Array Tag -> Int# -> PArray a
{-# INLINE_PA packByTagPA# #-}
packByTagPA# (PArray _ xs) n# tags t# 
  = PArray n# (packByTagPD xs n# tags t#)

combine2PA# :: PA a => Int# -> U.Sel2 -> PArray a -> PArray a -> PArray a
{-# INLINE_PA combine2PA# #-}
combine2PA# n# sel (PArray _ as) (PArray _ bs)
  = PArray n# (combine2PD n# sel as bs)

updatePA# :: PA a => PArray a -> U.Array Int -> PArray a -> PArray a
{-# INLINE_PA updatePA# #-}
updatePA# (PArray n# xs) is (PArray _ ys)
  = PArray n# (updatePD xs is ys)

fromListPA# :: PA a => Int# -> [a] -> PArray a
{-# INLINE_PA fromListPA# #-}
fromListPA# n# xs 
  = PArray n# (fromListPD n# xs)

fromListPA :: PA a => [a] -> PArray a
{-# INLINE fromListPA #-}
fromListPA xs
  = case length xs of
     I# n# -> fromListPA# n# xs

nfPA :: PA a => PArray a -> ()
{-# INLINE nfPA #-}
nfPA (PArray _ xs) 
  = nfPD xs

