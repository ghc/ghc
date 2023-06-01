{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Cmm.LRegSet (
    LRegSet,
    LRegKey,

    emptyLRegSet,
    nullLRegSet,
    insertLRegSet,
    elemLRegSet,

    deleteFromLRegSet,
    sizeLRegSet,

    plusLRegSet,
    elemsLRegSet
  ) where

import GHC.Prelude
import GHC.Types.Unique
import GHC.Cmm.Expr
import GHC.Word

import GHC.Data.Word64Set as Word64Set

-- Compact sets for membership tests of local variables.

type LRegSet = Word64Set.Word64Set
type LRegKey = Word64

emptyLRegSet :: LRegSet
emptyLRegSet = Word64Set.empty

nullLRegSet :: LRegSet -> Bool
nullLRegSet = Word64Set.null

insertLRegSet :: LocalReg -> LRegSet -> LRegSet
insertLRegSet l = Word64Set.insert (getKey (getUnique l))

elemLRegSet :: LocalReg -> LRegSet -> Bool
elemLRegSet l = Word64Set.member (getKey (getUnique l))

deleteFromLRegSet :: LRegSet -> LocalReg -> LRegSet
deleteFromLRegSet set reg = Word64Set.delete (getKey . getUnique $ reg) set

sizeLRegSet :: Word64Set -> Int
sizeLRegSet = Word64Set.size

plusLRegSet :: Word64Set -> Word64Set -> Word64Set
plusLRegSet = Word64Set.union

elemsLRegSet :: Word64Set -> [Word64]
elemsLRegSet = Word64Set.toList
