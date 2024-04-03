{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Cmm.LRegSet (
    LRegSet,

    emptyLRegSet,
    nullLRegSet,
    insertLRegSet,
    elemLRegSet,

    deleteFromLRegSet,
    sizeLRegSet,

    unionLRegSet,
    unionsLRegSet,
    elemsLRegSet
  ) where

import GHC.Prelude
import GHC.Types.Unique
import GHC.Types.Unique.Set
import GHC.Cmm.Expr

-- Compact sets for membership tests of local variables.

type LRegSet = UniqueSet

{-# INLINE emptyLRegSet #-}
emptyLRegSet :: LRegSet
emptyLRegSet = emptyUniqueSet

{-# INLINE nullLRegSet #-}
nullLRegSet :: LRegSet -> Bool
nullLRegSet = nullUniqueSet

{-# INLINE insertLRegSet #-}
insertLRegSet :: LocalReg -> LRegSet -> LRegSet
insertLRegSet l = insertUniqueSet (getUnique l)

{-# INLINE elemLRegSet #-}
elemLRegSet :: LocalReg -> LRegSet -> Bool
elemLRegSet l = memberUniqueSet (getUnique l)

{-# INLINE deleteFromLRegSet #-}
deleteFromLRegSet :: LRegSet -> LocalReg -> LRegSet
deleteFromLRegSet set reg = deleteUniqueSet (getUnique reg) set

{-# INLINE sizeLRegSet #-}
sizeLRegSet :: LRegSet -> Int
sizeLRegSet = sizeUniqueSet

{-# INLINE unionLRegSet #-}
unionLRegSet :: LRegSet -> LRegSet -> LRegSet
unionLRegSet = unionUniqueSet

{-# INLINE unionsLRegSet #-}
unionsLRegSet :: [LRegSet] -> LRegSet
unionsLRegSet = unionsUniqueSet

{-# INLINE elemsLRegSet #-}
elemsLRegSet :: LRegSet -> [Unique]
elemsLRegSet = elemsUniqueSet
