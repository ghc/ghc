module Trans where

import List
import Words
import qualified Word
import Arithmetic
import Cell
import Memory
import Register
import Instruction

-- Begin Signature ----------------------------------------------------------
{-

  We have used Transactions to represent instructions w/ their data.
  These have been particularly useful in pipelined and out-of-order 
  superscalar machines.  

-}

data Trans i c = Trans [c] i [c] [c]
		   deriving (Eq,Show,Read)

-- Convention: if Trans d op s i
-- we say that d is the destination, op is the instruction
-- s is the source, and i is the information

-- return a nop-like transaction
nop             :: (Instruction i,Register r) => Trans i (c r w)

-- return a PC transaction
pcTrans         :: (Cell c,Instruction i,Register r, Word w) => 
                       w -> Trans i (c r w)

isNop           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isAdd           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isAlu           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isCmp           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isBool          :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isSub           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isMul           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isDiv           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isJump          :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isMove          :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isMem           :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isLoad          :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isStore         :: (Instruction i,Register r) => Trans i (c r w) -> Bool
isBranch        :: (Cell c,Register r,Word w) => Trans i (c r w) -> Bool
isComputable    :: (Cell c,Register r,Word w) => Trans i (c r w) -> Bool

-- update destination fields
updDst          :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> [c r w] -> Trans i (c r w)

-- apply a function to the destination fields
repDst          :: Register r => (c r w -> c r w -> Bool) ->
                                 Trans i (c r w) -> [c r w] -> Trans i (c r w)

-- add to the destination
addDst          :: Register r => c r w -> Trans i (c r w) -> Trans i (c r w)

-- get the destination
getDst          :: Register r => Trans i (c r w) -> [c r w]

-- replace the dest fields
putDst          :: Register r => Trans i (c r w) -> [c r w] -> Trans i (c r w)


updSrc          :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> [c r w] -> Trans i (c r w)
addSrc          :: Register r => c r w -> Trans i (c r w) -> Trans i (c r w)
getSrc          :: Register r => Trans i (c r w) -> [c r w]
putSrc          :: Register r => Trans i (c r w) -> [c r w] -> Trans i (c r w)

addInfo         :: Register r => c r w -> Trans i (c r w) -> Trans i (c r w)
getInfo         :: Register r => Trans i (c r w) -> [c r w]
putInfo         :: Register r => Trans i (c r w) -> [c r w] -> Trans i (c r w)

getOp           :: Trans i (c r w) -> i
putOp           :: Trans i (c r w) -> i -> Trans i (c r w)


-- return the speculative PC from the info area
getSpecPC       :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Maybe (c r w)

-- return the PC from the destination area
getDstPC        :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Maybe (c r w)
getSrcPC        :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Maybe (c r w)

-- return the instructions location from memory from the destination
-- area
getLoc          :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Maybe (c r w)

-- get register references
getSrcRegs      :: (Cell c,Register r,Word w) => Trans i (c r w) -> [r]
getDstRegs      :: (Cell c,Register r,Word w) => Trans i (c r w) -> [r]

-- get register reference values
getSrcRegVals   :: (Cell c,Register r,Word w) => Trans i (c r w) -> [w]
putDstRegVal    :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> w -> Trans i (c r w)

-- evalTrans t (c,w) update the destination fields in t with w if they match
-- c
evalTrans       :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> (c r w,Maybe w) -> Trans i (c r w)

-- is there a Read-After-Write hazard between two transactions?
rawHazard       :: (Cell c,Register r,Word w) => 
                   (Trans i (c r w),Trans i (c r w)) -> Bool

-- bypass t t2 source operands of t with the dest operands of t2
-- if the references match.

bypass          :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Trans i (c r w) -> Trans i (c r w)

-- bypass the dest. operands instead of the source operands.
bypassDst       :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Trans i (c r w) -> Trans i (c r w)

-- bypass with multiple transactions
bypassMany      :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> [Trans i (c r w)] -> Trans i (c r w)
bypassDstMany   :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> [Trans i (c r w)] -> Trans i (c r w)

-- bypass to multiple transaction with multiple transactions
broadcast       :: (Cell a, Register b, Word c) => 
                   [Trans e (a b c)] -> [Trans e (a b c)] -> [Trans e (a b c)]

--source operands and dest operands all filled in?
complete        :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> Bool

-- if (x,y) = readyToRetire z, then
-- x is the lift of transactions that are "complete"
readyToRetire   :: (Cell c,Register r,Word w) => 
                   [Trans i (c r w)] -> ([Trans i (c r w)],[Trans i (c r w)])


-- if (x,y) = readyToCompute z, then
-- x is the lift of transactions with all of their source operands filled in
readyToCompute  :: (Cell c,Register r,Word w) => 
                   [Trans i (c r w)] -> ([Trans i (c r w)],[Trans i (c r w)])


updatePC        :: Register r => c r w -> Trans i (c r w) -> Trans i (c r w)


getPredicate   :: (Cell c,Register r,Word w) => Trans i (c r w) -> c r w
isPredicated   :: (Cell c,Register r,Word w) => Trans i (c r w) -> Bool

evalPredicate  :: (Cell c,Register r,Word w) => Trans i (c r w) -> w

-- End Signature ----------------------------------------------------------


updCells        :: (Cell c,Register r,Word w) => [c r w] -> [c r w] -> [c r w]
repCells        :: Register r => (c r w -> c r w -> Bool) ->
                                  [c r w] -> [c r w] -> [c r w]

-- perhaps these functions can go?
filterDst       :: Register r => (c r w -> Bool) -> Trans i (c r w) -> [c r w]

fillInCells     :: (Cell c,Register r,Word w) => [c r w] -> [c r w] -> [c r w]

fillInSrcCells  :: (Cell c,Register r,Word w) => 
                   Trans i (c r w) -> [c r w] -> Trans i (c r w)

filterOut       :: (Register r,Functor m) => 
                   (Trans i (c r w) -> Bool) -> m [Trans i (c r w)] -> 
                   m [Trans i (c r w)]



nop = Trans [] noOp [] []

pcTrans addr = Trans [putVal pcNothing (Just addr)] noOp [] []
isNop t = isNoOp (getOp t)
isAdd t = isAddOp (getOp t)
isAlu t = isAluOp (getOp t)
isCmp t = isCmpOp (getOp t)
isBool t = isBoolOp (getOp t)
isSub t = isSubOp (getOp t)
isMul t = isMultOp (getOp t)
isDiv t = isDivOp (getOp t)
isJump t = isJumpOp (getOp t)
isMem t = isMemOp (getOp t)
isMove t = isMoveOp (getOp t)
isLoad t = isLoadOp (getOp t)
isStore t = isStoreOp (getOp t)

isBranch (Trans d _ _ _) = any search d where
        search r = if isReg r then ispc (getReg r)
                   else False

isComputable = and . map isComputed . getSrc




repCells replFunc cells replacements
  = map (\cell -> foldr bypassCell cell replacements) cells
    where
      bypassCell bypassed argCell
	= if replFunc bypassed argCell
	    then bypassed
	    else argCell


updCells cells bypassCells = repCells cellHazard cells bypassCells


repDst repFunc (Trans d o s i) cells = Trans (repCells repFunc d cells) o s i
updDst = repDst cellHazard
addDst c t = putDst t (c:getDst t)
getDst (Trans d o s i) = d
putDst (Trans _ o s i) d = Trans d o s i

updSrc (Trans d o s i) cells = Trans d o (updCells s cells) i
addSrc c t = putSrc t (c:getSrc t)
getSrc (Trans d o s i) = s
putSrc (Trans d o _ i) s = Trans d o s i

addInfo c t = putInfo t (c:getInfo t)
getInfo (Trans d o s i) = i
putInfo (Trans d o s _) i = Trans d o s i 

getOp (Trans d o s i) = o
putOp (Trans d _ s i) o = Trans d o s i

getSpecPC	= find isSpecPC . getInfo
getDstPC	= find isPC . getDst
getSrcPC	= find isPC . getSrc
getLoc		= find isLoc . getInfo

getSrcRegs t = map getReg $ filter isReg $ getSrc t
getDstRegs t = map getReg $ filter isReg $ getDst t

getSrcRegVals t = map getVal $ 
			filter isReg $ getSrc t

{-
putDstRegVal (Trans [Reg r _] o s i) n
			= Trans [Reg r (Val n)] o s i
-}
putDstRegVal (Trans [r] o s i) n
			= Trans [putVal r (Just n)] o s i

getPredicate (Trans _ _ l _) = last (filter isPred l)

getPredicate' t = if isPredicated t then Just (getPredicate t)
                  else Nothing

isPredicated (Trans _ _ x _) 
    = case filter isPred  x of
		[] -> False
                _ -> True


evalPredicate t =
   case getPredicate' t of
              Just c -> if isAss c then getVal c
                   --     else error $ "evalPredicate" ++ show t
                        else error "evalPredicate" 
              Nothing -> 1

bypass tran bypassT = --updSrc tran $ getDst bypassT
                      if evalPredicate bypassT /= 0
                          then updSrc tran $ getDst bypassT
                          else tran

bypassDst tran bypassT = if evalPredicate bypassT /= 0 
                            then updDst tran $ getDst bypassT
                            else tran

bypassMany tran bypassT = foldr (\a b -> b `bypass` a) tran bypassT

bypassDstMany tran bypassT = foldr (\a b -> b `bypassDst` a) tran bypassT

broadcast xs ys = map (`bypassMany` ys) xs

{- PRE-predication
bypass tran bypassT = updSrc tran $ getDst bypassT

bypassDst tran bypassT = updDst tran $ getDst bypassT

bypassTrans tran bypassT = foldr (\a b -> b `bypass` a) tran bypassT

broadcast xs ys = map (`bypassTrans` ys) xs
-}


readyToRetire  = partition $ and . map isComputed . getDst

complete = and . map isComputed . getDst

readyToCompute =  partition $ and . map isComputed . getSrc

tran `evalTrans` (dest,val) = repDst sameLoc tran [putVal dest val]

rawHazard (preceeding,following)
  = or [ cellHazard precCell followCell |
	   precCell <- getDst preceeding,
	   followCell <- getSrc following]

filterDst f (Trans d _ _ _) = filter f d

--added 19 Nov
filterOut f = fmap (filter $ not . f)

fillInCells cells bypassCells
  = repCells (\x y -> (not $ isAss y) && cellHazard x y) cells bypassCells
  
fillInSrcCells (Trans d o s i) cells = Trans d o (fillInCells s cells) i

fillInCells' cells bypassCells
  = repCells cellHazard cells bypassCells
  
fillInSrcCells' (Trans d o s i) cells = Trans d o (fillInCells' s cells) i
  
-- TEMPORARY --- NOT ROBUST!
updatePC c (Trans _ o s i) = Trans [c] o s i










