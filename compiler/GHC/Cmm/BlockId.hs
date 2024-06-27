{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- BlockId module should probably go away completely, being superseded by Label -}
module GHC.Cmm.BlockId
  ( BlockId, mkBlockId -- ToDo: BlockId should be abstract, but it isn't yet
  , newBlockId
  , blockLbl, infoTblLbl
  ) where

import GHC.Prelude

import GHC.Cmm.CLabel
import GHC.Data.FastString
import GHC.Types.Id.Info
import GHC.Types.Name
import GHC.Types.Unique
import qualified GHC.Types.Unique.DSM as DSM

import GHC.Cmm.Dataflow.Label (Label, mkHooplLabel)

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

-- If the monad unique instance uses a deterministic unique supply, this will
-- give you a deterministic unique. Otherwise, it will not. Note that from Cmm
-- onwards (after deterministic renaming in 'codeGen'), there should only exist
-- deterministic block labels.
newBlockId :: DSM.MonadGetUnique m => m BlockId
newBlockId = mkBlockId <$> DSM.getUniqueM

blockLbl :: BlockId -> CLabel
blockLbl label = mkLocalBlockLabel (getUnique label)

infoTblLbl :: BlockId -> CLabel
infoTblLbl label
  = mkBlockInfoTableLabel (mkFCallName (getUnique label) (fsLit "block")) NoCafRefs

