module StackSlot
    ( BlockId(..), mkBlockId 	-- ToDo: BlockId should be abstract, but it isn't yet
    , BlockEnv, emptyBlockEnv, lookupBlockEnv, extendBlockEnv, mkBlockEnv
    , BlockSet, emptyBlockSet, elemBlockSet, extendBlockSet, sizeBlockSet, mkBlockSet
    , StackArea, mkStackArea, outgoingSlot
    , StackSlot(..)) where -- StackSlot should probably be abstract
-- Why is the BlockId here? To avoid recursive module problems.

import Monad
import Outputable
import Unique
import UniqFM
import UniqSet


-- A stack area is represented by three pieces:
-- o The BlockId of the return site.
--   Maybe during the conversion to VFP offsets, this BlockId will be the entry point.
-- o The size of the outgoing parameter space
-- o The size of the incoming parameter space, if the function returns
data StackArea = StackArea BlockId Int (Maybe Int)
  deriving (Eq, Ord)

instance Outputable StackArea where
  ppr (StackArea bid f a) =
    text "StackArea" <+> ppr bid <+> text "[" <+> ppr f <+> text "," <+> ppr a <+> text ")"

-- Eventually, we'll want something proper that takes arguments and formals
-- and gives you back the calling convention code, as well as the stack area.
--mkStackArea :: BlockId -> CmmActuals -> CmmFormals -> (StackArea, ...)
-- But for now...
mkStackArea :: BlockId -> [a] -> Maybe [b] -> StackArea
mkStackArea k as fs = StackArea k (length as) (liftM length fs)

-- A stack slot is an offset from the base of a stack area.
data StackSlot = StackSlot StackArea Int
  deriving (Eq, Ord)

-- Return the last slot in the outgoing parameter area.
outgoingSlot :: StackArea -> StackSlot
outgoingSlot a@(StackArea _ outN _) = StackSlot a outN

instance Outputable StackSlot where
  ppr (StackSlot (StackArea bid _ _) n) =
    text "Stack(" <+> ppr bid <+> text "," <+> ppr n <+> text ")"


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


type BlockEnv a = UniqFM {- BlockId -} a
emptyBlockEnv :: BlockEnv a
emptyBlockEnv = emptyUFM
mkBlockEnv :: [(BlockId,a)] -> BlockEnv a
mkBlockEnv = listToUFM
lookupBlockEnv :: BlockEnv a -> BlockId -> Maybe a
lookupBlockEnv = lookupUFM
extendBlockEnv :: BlockEnv a -> BlockId -> a -> BlockEnv a
extendBlockEnv = addToUFM

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

