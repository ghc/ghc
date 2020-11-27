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

import Data.IntSet as IntSet

-- Compact sets for membership tests of local variables.

type LRegSet = IntSet.IntSet
type LRegKey = Int

emptyLRegSet :: LRegSet
emptyLRegSet = IntSet.empty

nullLRegSet :: LRegSet -> Bool
nullLRegSet = IntSet.null

insertLRegSet :: LocalReg -> LRegSet -> LRegSet
insertLRegSet l = IntSet.insert (getKey (getUnique l))

elemLRegSet :: LocalReg -> LRegSet -> Bool
elemLRegSet l = IntSet.member (getKey (getUnique l))

deleteFromLRegSet :: LRegSet -> LocalReg -> LRegSet
deleteFromLRegSet set reg = IntSet.delete (getKey . getUnique $ reg) set

sizeLRegSet :: IntSet -> Int
sizeLRegSet = IntSet.size

plusLRegSet :: IntSet -> IntSet -> IntSet
plusLRegSet = IntSet.union

elemsLRegSet :: IntSet -> [Int]
elemsLRegSet = IntSet.toList
