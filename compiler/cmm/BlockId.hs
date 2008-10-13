module BlockId
  ( BlockId(..), mkBlockId 	-- ToDo: BlockId should be abstract, but it isn't yet
  , BlockEnv, emptyBlockEnv, elemBlockEnv, lookupBlockEnv, extendBlockEnv
  , mkBlockEnv, mapBlockEnv
  , eltsBlockEnv, plusBlockEnv, delFromBlockEnv, blockEnvToList, lookupWithDefaultBEnv
  , isNullBEnv, sizeBEnv, foldBlockEnv, foldBlockEnv', addToBEnv_Acc
  , BlockSet, emptyBlockSet, unitBlockSet, isEmptyBlockSet
  , elemBlockSet, extendBlockSet, sizeBlockSet, unionBlockSets
  , removeBlockSet, mkBlockSet, blockSetToList, foldBlockSet
  , blockLbl, infoTblLbl, retPtLbl
  ) where

import CLabel
import IdInfo
import Maybes
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
most assembly languages allow, a label is visible throughout the entire
compilation unit in which it appears.
-}

data BlockId = BlockId Unique
  deriving (Eq,Ord)

instance Uniquable BlockId where
  getUnique (BlockId id) = id

mkBlockId :: Unique -> BlockId
mkBlockId uniq = BlockId uniq

instance Show BlockId where
  show (BlockId u) = show u

instance Outputable BlockId where
  ppr (BlockId id) = ppr id

retPtLbl :: BlockId -> CLabel
retPtLbl (BlockId id) = mkReturnPtLabel id

blockLbl :: BlockId -> CLabel
blockLbl (BlockId id) = mkEntryLabel (mkFCallName id "block") NoCafRefs

infoTblLbl :: BlockId -> CLabel
infoTblLbl (BlockId id) = mkInfoTableLabel (mkFCallName id "block") NoCafRefs

-- Block environments: Id blocks
newtype BlockEnv a = BlockEnv (UniqFM {- id -} a)

instance Outputable a => Outputable (BlockEnv a) where
  ppr (BlockEnv env) = ppr env

-- This is pretty horrid. There must be common patterns here that can be
-- abstracted into wrappers.
emptyBlockEnv :: BlockEnv a
emptyBlockEnv = BlockEnv emptyUFM

isNullBEnv :: BlockEnv a -> Bool
isNullBEnv (BlockEnv env) = isNullUFM env

sizeBEnv :: BlockEnv a -> Int
sizeBEnv (BlockEnv env)  = sizeUFM env

mkBlockEnv :: [(BlockId,a)] -> BlockEnv a
mkBlockEnv = foldl (uncurry . extendBlockEnv) emptyBlockEnv

eltsBlockEnv :: BlockEnv elt -> [elt]
eltsBlockEnv (BlockEnv env) = eltsUFM env

delFromBlockEnv	:: BlockEnv elt -> BlockId -> BlockEnv elt
delFromBlockEnv	  (BlockEnv env) (BlockId id) = BlockEnv (delFromUFM env id)

lookupBlockEnv :: BlockEnv a -> BlockId -> Maybe a
lookupBlockEnv (BlockEnv env) (BlockId id) = lookupUFM env id

elemBlockEnv :: BlockEnv a -> BlockId -> Bool
elemBlockEnv (BlockEnv env) (BlockId id) = isJust $ lookupUFM env id

lookupWithDefaultBEnv :: BlockEnv a -> a -> BlockId -> a
lookupWithDefaultBEnv env x id = lookupBlockEnv env id `orElse` x

extendBlockEnv :: BlockEnv a -> BlockId -> a -> BlockEnv a
extendBlockEnv (BlockEnv env) (BlockId id) x = BlockEnv (addToUFM env id x)

mapBlockEnv :: (a -> b) -> BlockEnv a -> BlockEnv b
mapBlockEnv f (BlockEnv env) = BlockEnv (mapUFM f env)

foldBlockEnv :: (BlockId -> a -> b -> b) -> b -> BlockEnv a -> b
foldBlockEnv f b (BlockEnv env) = 
  foldUFM_Directly (\u x y -> f (mkBlockId u) x y) b env

foldBlockEnv' :: (a -> b -> b) -> b -> BlockEnv a -> b
foldBlockEnv' f b (BlockEnv env) = foldUFM f b env

plusBlockEnv :: BlockEnv elt -> BlockEnv elt -> BlockEnv elt
plusBlockEnv (BlockEnv x) (BlockEnv y) = BlockEnv (plusUFM x y)

blockEnvToList :: BlockEnv elt -> [(BlockId, elt)]
blockEnvToList (BlockEnv env) =
  map (\ (id, elt) -> (BlockId id, elt)) $ ufmToList env

addToBEnv_Acc	:: (elt -> elts -> elts)	-- Add to existing
			   -> (elt -> elts)		-- New element
			   -> BlockEnv elts 		-- old
			   -> BlockId -> elt 		-- new
			   -> BlockEnv elts		-- result
addToBEnv_Acc add new (BlockEnv old) (BlockId k) v =
  BlockEnv (addToUFM_Acc add new old k v)
  -- I believe this is only used by obsolete code.


newtype BlockSet = BlockSet (UniqSet Unique)
instance Outputable BlockSet where
  ppr (BlockSet set) = ppr set


emptyBlockSet :: BlockSet
emptyBlockSet = BlockSet emptyUniqSet

isEmptyBlockSet :: BlockSet -> Bool
isEmptyBlockSet (BlockSet s) = isEmptyUniqSet s

unitBlockSet :: BlockId -> BlockSet
unitBlockSet = extendBlockSet emptyBlockSet

elemBlockSet :: BlockId -> BlockSet -> Bool
elemBlockSet (BlockId id) (BlockSet set) = elementOfUniqSet id set

extendBlockSet :: BlockSet -> BlockId -> BlockSet
extendBlockSet (BlockSet set) (BlockId id) = BlockSet (addOneToUniqSet set id)

removeBlockSet :: BlockSet -> BlockId -> BlockSet
removeBlockSet (BlockSet set) (BlockId id) = BlockSet (delOneFromUniqSet set id)

mkBlockSet :: [BlockId] -> BlockSet
mkBlockSet = foldl extendBlockSet emptyBlockSet

unionBlockSets :: BlockSet -> BlockSet -> BlockSet
unionBlockSets (BlockSet s) (BlockSet s') = BlockSet (unionUniqSets s s')

sizeBlockSet :: BlockSet -> Int
sizeBlockSet (BlockSet set) = sizeUniqSet set

blockSetToList :: BlockSet -> [BlockId]
blockSetToList (BlockSet set) = map BlockId $ uniqSetToList set

foldBlockSet :: (BlockId -> b -> b) -> b -> BlockSet -> b
foldBlockSet f z (BlockSet set) = foldUniqSet (f . BlockId) z set
