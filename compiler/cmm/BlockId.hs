module BlockId
  ( BlockId(..), mkBlockId 	-- ToDo: BlockId should be abstract, but it isn't yet
  , BlockEnv, emptyBlockEnv, lookupBlockEnv, extendBlockEnv, mkBlockEnv, mapBlockEnv
  , BlockSet, emptyBlockSet, elemBlockSet, extendBlockSet, sizeBlockSet, mkBlockSet
  , foldBlockEnv, blockLbl, infoTblLbl
  ) where

import CLabel
import IdInfo
import Name
import Outputable
import UniqFM
import Unique
import UniqSet

----------------------------------------------------------------
--- Block Ids, their environments, and their sets

{- Note [Unique BlockId]
~~~~~~~~~~~~~~~~~~~~~~~~
Although a 'BlockId' is a local label, for reasons of implementation,
'BlockId's must be unique within an entire compilation unit.  The reason
is that each local label is mapped to an assembly-language label, and in
most assembly languages allow, a label is visible throughout the enitre
compilation unit in which it appears.
-}

newtype BlockId = BlockId Unique
  deriving (Eq,Ord)

instance Uniquable BlockId where
  getUnique (BlockId u) = u

mkBlockId :: Unique -> BlockId
mkBlockId uniq = BlockId uniq

instance Show BlockId where
  show (BlockId u) = show u

instance Outputable BlockId where
  ppr = ppr . getUnique

blockLbl :: BlockId -> CLabel
blockLbl id = mkEntryLabel (mkFCallName (getUnique id) "block") NoCafRefs

infoTblLbl :: BlockId -> CLabel
infoTblLbl id = mkInfoTableLabel (mkFCallName (getUnique id) "block") NoCafRefs

type BlockEnv a = UniqFM {- BlockId -} a
emptyBlockEnv :: BlockEnv a
emptyBlockEnv = emptyUFM
mkBlockEnv :: [(BlockId,a)] -> BlockEnv a
mkBlockEnv = listToUFM
lookupBlockEnv :: BlockEnv a -> BlockId -> Maybe a
lookupBlockEnv = lookupUFM
extendBlockEnv :: BlockEnv a -> BlockId -> a -> BlockEnv a
extendBlockEnv = addToUFM
mapBlockEnv :: (a -> b) -> BlockEnv a -> BlockEnv b
mapBlockEnv = mapUFM
foldBlockEnv :: (BlockId -> a -> b -> b) -> b -> BlockEnv a -> b
foldBlockEnv f = foldUFM_Directly (\u x y -> f (mkBlockId u) x y)

type BlockSet = UniqSet BlockId
emptyBlockSet :: BlockSet
emptyBlockSet = emptyUniqSet
elemBlockSet :: BlockId -> BlockSet -> Bool
elemBlockSet = elementOfUniqSet
extendBlockSet :: BlockSet -> BlockId -> BlockSet
extendBlockSet = addOneToUniqSet
mkBlockSet :: [BlockId] -> BlockSet
mkBlockSet = mkUniqSet
sizeBlockSet :: BlockSet -> Int
sizeBlockSet = sizeUniqSet
