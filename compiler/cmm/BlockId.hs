{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- BlockId module should probably go away completely, being superseded by Label -}
module BlockId
  ( BlockId, mkBlockId -- ToDo: BlockId should be abstract, but it isn't yet
  , newBlockId
  , blockLbl, infoTblLbl
  ) where

import GhcPrelude

import CLabel
import IdInfo
import Name
import Unique
import UniqSupply

import Hoopl.Label (Label, mkHooplLabel)

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

type BlockId = Label

mkBlockId :: Unique -> BlockId
mkBlockId unique = mkHooplLabel $ getKey unique

newBlockId :: MonadUnique m => m BlockId
newBlockId = mkBlockId <$> getUniqueM

blockLbl :: BlockId -> CLabel
blockLbl label = mkLocalBlockLabel (getUnique label)

infoTblLbl :: BlockId -> CLabel
infoTblLbl label
  = mkBlockInfoTableLabel (mkFCallName (getUnique label) "block") NoCafRefs
