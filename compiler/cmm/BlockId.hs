{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- BlockId module should probably go away completely, being superseded by Label -}
module BlockId
  ( BlockId, mkBlockId -- ToDo: BlockId should be abstract, but it isn't yet
  , BlockSet, BlockEnv
  , IsSet(..), setInsertList, setDeleteList, setUnions
  , IsMap(..), mapInsertList, mapDeleteList, mapUnions
  , emptyBlockSet, emptyBlockMap
  , blockLbl, infoTblLbl, retPtLbl
  ) where

import CLabel
import IdInfo
import Name
import Outputable
import Unique

import Compiler.Hoopl as Hoopl hiding (Unique)
import Compiler.Hoopl.Internals (uniqueToLbl, lblToUnique)

----------------------------------------------------------------
--- Block Ids, their environments, and their sets

{- Note [Unique BlockId]
~~~~~~~~~~~~~~~~~~~~~~~~
Although a 'BlockId' is a local label, for reasons of implementation,
'BlockId's must be unique within an entire compilation unit.  The reason
is that each local label is mapped to an assembly-language label, and in
most assembly languages allow, a label is visible throughout the entire
compilation unit in which it appears.
-}

type BlockId = Hoopl.Label

instance Uniquable BlockId where
  getUnique label = getUnique (lblToUnique label)

instance Outputable BlockId where
  ppr label = ppr (getUnique label)

mkBlockId :: Unique -> BlockId
mkBlockId unique = uniqueToLbl $ intToUnique $ getKey unique

retPtLbl :: BlockId -> CLabel
retPtLbl label = mkReturnPtLabel $ getUnique label

blockLbl :: BlockId -> CLabel
blockLbl label = mkEntryLabel (mkFCallName (getUnique label) "block") NoCafRefs

infoTblLbl :: BlockId -> CLabel
infoTblLbl label = mkInfoTableLabel (mkFCallName (getUnique label) "block") NoCafRefs

-- Block environments: Id blocks
type BlockEnv a = Hoopl.LabelMap a

instance Outputable a => Outputable (BlockEnv a) where
  ppr = ppr . mapToList

emptyBlockMap :: BlockEnv a
emptyBlockMap = mapEmpty

-- Block sets
type BlockSet = Hoopl.LabelSet

instance Outputable BlockSet where
  ppr = ppr . setElems

emptyBlockSet :: BlockSet
emptyBlockSet = setEmpty
