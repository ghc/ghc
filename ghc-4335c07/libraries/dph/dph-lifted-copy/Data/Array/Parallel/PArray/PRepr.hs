{-# LANGUAGE CPP, FlexibleContexts #-}

#include "fusion-phases.h"

-- | Defines the family of types that can be represented generically,
--   and the functions to convert two and from the generic representation.
--   
--   TODO: Check inconsistent use of INLINE pragmas.
--         Most have INLINE_PA, but bpermutePD and nfPD have plain INLINE
--
module Data.Array.Parallel.PArray.PRepr (
  PRepr,
  PA(..),
 
  -- These functions have corresponding members in the PR class
  -- from Data.Array.Parallel.PArray.PData.
  emptyPD,
  replicatePD,
  replicatelPD,
  repeatPD,
  indexPD,
  extractPD,
  bpermutePD,
  appPD,
  applPD,
  packByTagPD,
  combine2PD,
  updatePD,
  fromListPD,
  nfPD
)
where
import Data.Array.Parallel.PArray.PData


-- | Representable types.
--
--   The family of types that we know how to represent generically.
--   PRepr takes an arbitrary type and produces the generic type we use to 
--   represent it.
--
--   Instances for simple types are defined in Data.Array.Parallel.Lifted.Instances.
--   For algebraic types, it's up to the vectoriser/client module to create
--   a suitable instance.
--
type family PRepr a


-- | A PA dictionary contains the functions that we use to convert a
--   representable type to and from its generic representation.
--   The conversion methods should all be O(1).
--
class PR (PRepr a) => PA a where
  toPRepr       :: a -> PRepr a
  fromPRepr     :: PRepr a -> a
  toArrPRepr    :: PData a -> PData (PRepr a)
  fromArrPRepr  :: PData (PRepr a) -> PData a
  
  -- These methods aren't used in this backend, but the vecoriser expects
  -- them to be part of the PA class. It will generate instances for them, 
  -- but they will never be called at runtime.
  toArrPReprs   :: PDatas a -> PDatas (PRepr a)
  fromArrPReprs :: PDatas (PRepr a) -> PDatas a


-- PD Wrappers ----------------------------------------------------------------
--  These wrappers work on (PData a) arrays when we know the element type 'a'
--  is representable. For most of them we can just convert the PData to the 
--  underlying representation type, and use the corresponding operator from
--  the PR dictionary.
--
emptyPD :: PA a => T_emptyPR a
{-# INLINE_PA emptyPD #-}
emptyPD 
  = fromArrPRepr emptyPR

replicatePD :: PA a => T_replicatePR a
{-# INLINE_PA replicatePD #-}
replicatePD n# x 
  = fromArrPRepr
  . replicatePR n#
  $ toPRepr x

replicatelPD :: PA a => T_replicatelPR a
{-# INLINE_PA replicatelPD #-}
replicatelPD segd xs 
  = fromArrPRepr
  . replicatelPR segd
  $ toArrPRepr xs
    
repeatPD :: PA a => T_repeatPR a
{-# INLINE_PA repeatPD #-}
repeatPD n# len# xs 
  = fromArrPRepr
  . repeatPR n# len#
  $ toArrPRepr xs

indexPD :: PA a => T_indexPR a
{-# INLINE_PA indexPD #-}
indexPD xs i# 
  = fromPRepr 
  $ indexPR (toArrPRepr xs) i#

extractPD :: PA a => T_extractPR a
{-# INLINE_PA extractPD #-}
extractPD xs i# m#
  = fromArrPRepr 
  $ extractPR (toArrPRepr xs) i# m#

bpermutePD :: PA a => T_bpermutePR a
{-# INLINE bpermutePD #-}
bpermutePD xs n# is 
  = fromArrPRepr 
  $ bpermutePR (toArrPRepr xs) n# is

appPD :: PA a => T_appPR a
{-# INLINE_PA appPD #-}
appPD xs ys 
  = fromArrPRepr 
   $ appPR (toArrPRepr xs) (toArrPRepr ys)

applPD :: PA a => T_applPR a
{-# INLINE_PA applPD #-}
applPD segd is xs js ys
  = fromArrPRepr 
  $ applPR segd is (toArrPRepr xs) js (toArrPRepr ys)

packByTagPD :: PA a => T_packByTagPR a
{-# INLINE_PA packByTagPD #-}
packByTagPD xs n# tags t#
  = fromArrPRepr 
  $ packByTagPR (toArrPRepr xs) n# tags t#

combine2PD :: PA a => T_combine2PR a
{-# INLINE_PA combine2PD #-}
combine2PD n# sel as bs
  = fromArrPRepr 
  $ combine2PR n# sel (toArrPRepr as) (toArrPRepr bs)

updatePD :: PA a => T_updatePR a
{-# INLINE_PA updatePD #-}
updatePD xs is ys
  = fromArrPRepr
  $ updatePR (toArrPRepr xs) is (toArrPRepr ys)

fromListPD :: PA a => T_fromListPR a
{-# INLINE_PA fromListPD #-}
fromListPD n# xs 
 = fromArrPRepr
 $ fromListPR n# (map toPRepr xs)

nfPD :: PA a => T_nfPR a
{-# INLINE nfPD #-}
nfPD xs = nfPR (toArrPRepr xs)


