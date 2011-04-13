
module CmmExpr
    ( CmmExpr(..), cmmExprType, cmmExprWidth, maybeInvertCmmExpr
    , CmmReg(..), cmmRegType
    , CmmLit(..), cmmLitType
    , LocalReg(..), localRegType
    , GlobalReg(..), globalRegType, spReg, hpReg, spLimReg, nodeReg, node
    , VGcPtr(..), vgcFlag 	-- Temporary!
    , DefinerOfLocalRegs, UserOfLocalRegs, foldRegsDefd, foldRegsUsed, filterRegsUsed
    , DefinerOfSlots, UserOfSlots, foldSlotsDefd, foldSlotsUsed
    , RegSet, emptyRegSet, elemRegSet, extendRegSet, deleteFromRegSet, mkRegSet
            , plusRegSet, minusRegSet, timesRegSet
    , regUsedIn
    , Area(..), AreaId(..), SubArea, SubAreaSet, AreaMap, isStackSlotOf
    , module CmmMachOp
    , module CmmType
    )
where

#include "HsVersions.h"

import CmmType
import CmmMachOp
import BlockId
import CLabel
import Unique
import UniqSet

import Data.Map (Map)

-----------------------------------------------------------------------------
--		CmmExpr
-- An expression.  Expressions have no side effects.
-----------------------------------------------------------------------------

data CmmExpr
  = CmmLit CmmLit               -- Literal
  | CmmLoad CmmExpr CmmType     -- Read memory location
  | CmmReg CmmReg		-- Contents of register
  | CmmMachOp MachOp [CmmExpr]  -- Machine operation (+, -, *, etc.)
  | CmmStackSlot Area Int       -- addressing expression of a stack slot
  | CmmRegOff CmmReg Int	
	-- CmmRegOff reg i
	--        ** is shorthand only, meaning **
	-- CmmMachOp (MO_Add rep) [x, CmmLit (CmmInt (fromIntegral i) rep)]
	--	where rep = typeWidth (cmmRegType reg)

instance Eq CmmExpr where	-- Equality ignores the types
  CmmLit l1    	    == CmmLit l2    	 = l1==l2
  CmmLoad e1 _ 	    == CmmLoad e2 _ 	 = e1==e2
  CmmReg r1    	    == CmmReg r2    	 = r1==r2
  CmmRegOff r1 i1   == CmmRegOff r2 i2   = r1==r2 && i1==i2
  CmmMachOp op1 es1 == CmmMachOp op2 es2 = op1==op2 && es1==es2
  CmmStackSlot a1 i1 == CmmStackSlot a2 i2 = a1==a2 && i1==i2
  _e1               == _e2               = False

data CmmReg 
  = CmmLocal  LocalReg
  | CmmGlobal GlobalReg
  deriving( Eq, Ord )

-- | A stack area is either the stack slot where a variable is spilled
-- or the stack space where function arguments and results are passed.
data Area
  = RegSlot  LocalReg
  | CallArea AreaId
  deriving (Eq, Ord)

data AreaId
  = Old            -- See Note [Old Area]
  | Young BlockId
  deriving (Eq, Ord)

{- Note [Old Area] 
~~~~~~~~~~~~~~~~~~
There is a single call area 'Old', allocated at the extreme old
end of the stack frame (ie just younger than the return address)
which holds:
  * incoming (overflow) parameters, 
  * outgoing (overflow) parameter to tail calls,
  * outgoing (overflow) result values 
  * the update frame (if any)

Its size is the max of all these requirements.  On entry, the stack
pointer will point to the youngest incoming parameter, which is not
necessarily at the young end of the Old area.

End of note -}

type SubArea    = (Area, Int, Int) -- area, offset, width
type SubAreaSet = Map Area [SubArea]

type AreaMap    = Map Area Int
     -- Byte offset of the oldest byte of the Area, 
     -- relative to the oldest byte of the Old Area

data CmmLit
  = CmmInt Integer  Width
	-- Interpretation: the 2's complement representation of the value
	-- is truncated to the specified size.  This is easier than trying
	-- to keep the value within range, because we don't know whether
 	-- it will be used as a signed or unsigned value (the CmmType doesn't
	-- distinguish between signed & unsigned).
  | CmmFloat  Rational Width
  | CmmLabel    CLabel			-- Address of label
  | CmmLabelOff CLabel Int		-- Address of label + byte offset
  
        -- Due to limitations in the C backend, the following
        -- MUST ONLY be used inside the info table indicated by label2
        -- (label2 must be the info label), and label1 must be an
        -- SRT, a slow entrypoint or a large bitmap (see the Mangler)
        -- Don't use it at all unless tablesNextToCode.
        -- It is also used inside the NCG during when generating
        -- position-independent code. 
  | CmmLabelDiffOff CLabel CLabel Int   -- label1 - label2 + offset
  | CmmBlock BlockId			-- Code label
  | CmmHighStackMark -- stands for the max stack space used during a procedure
  deriving Eq

cmmExprType :: CmmExpr -> CmmType
cmmExprType (CmmLit lit)      	= cmmLitType lit
cmmExprType (CmmLoad _ rep)   	= rep
cmmExprType (CmmReg reg)      	= cmmRegType reg
cmmExprType (CmmMachOp op args) = machOpResultType op (map cmmExprType args)
cmmExprType (CmmRegOff reg _)   = cmmRegType reg
cmmExprType (CmmStackSlot _ _)  = bWord -- an address
-- Careful though: what is stored at the stack slot may be bigger than
-- an address

cmmLitType :: CmmLit -> CmmType
cmmLitType (CmmInt _ width)     = cmmBits  width
cmmLitType (CmmFloat _ width)   = cmmFloat width
cmmLitType (CmmLabel lbl) 	= cmmLabelType lbl
cmmLitType (CmmLabelOff lbl _)  = cmmLabelType lbl
cmmLitType (CmmLabelDiffOff {}) = bWord
cmmLitType (CmmBlock _) 	= bWord
cmmLitType (CmmHighStackMark)   = bWord

cmmLabelType :: CLabel -> CmmType
cmmLabelType lbl | isGcPtrLabel lbl = gcWord
		 | otherwise 	    = bWord

cmmExprWidth :: CmmExpr -> Width
cmmExprWidth e = typeWidth (cmmExprType e)

--------
--- Negation for conditional branches

maybeInvertCmmExpr :: CmmExpr -> Maybe CmmExpr
maybeInvertCmmExpr (CmmMachOp op args) = do op' <- maybeInvertComparison op
                                            return (CmmMachOp op' args)
maybeInvertCmmExpr _ = Nothing

-----------------------------------------------------------------------------
--		Local registers
-----------------------------------------------------------------------------

data LocalReg
  = LocalReg !Unique CmmType
    -- ^ Parameters:
    --   1. Identifier
    --   2. Type

instance Eq LocalReg where
  (LocalReg u1 _) == (LocalReg u2 _) = u1 == u2

instance Ord LocalReg where
  compare (LocalReg u1 _) (LocalReg u2 _) = compare u1 u2

instance Uniquable LocalReg where
  getUnique (LocalReg uniq _) = uniq

cmmRegType :: CmmReg -> CmmType
cmmRegType (CmmLocal  reg) 	= localRegType reg
cmmRegType (CmmGlobal reg)	= globalRegType reg

localRegType :: LocalReg -> CmmType
localRegType (LocalReg _ rep) = rep

-----------------------------------------------------------------------------
--    Register-use information for expressions and other types 
-----------------------------------------------------------------------------

-- | Sets of local registers
type RegSet              =  UniqSet LocalReg
emptyRegSet             :: RegSet
elemRegSet              :: LocalReg -> RegSet -> Bool
extendRegSet            :: RegSet -> LocalReg -> RegSet
deleteFromRegSet        :: RegSet -> LocalReg -> RegSet
mkRegSet                :: [LocalReg] -> RegSet
minusRegSet, plusRegSet, timesRegSet :: RegSet -> RegSet -> RegSet

emptyRegSet      = emptyUniqSet
elemRegSet       = elementOfUniqSet
extendRegSet     = addOneToUniqSet
deleteFromRegSet = delOneFromUniqSet
mkRegSet         = mkUniqSet
minusRegSet      = minusUniqSet
plusRegSet       = unionUniqSets
timesRegSet      = intersectUniqSets

class UserOfLocalRegs a where
  foldRegsUsed :: (b -> LocalReg -> b) -> b -> a -> b

class DefinerOfLocalRegs a where
  foldRegsDefd :: (b -> LocalReg -> b) -> b -> a -> b

filterRegsUsed :: UserOfLocalRegs e => (LocalReg -> Bool) -> e -> RegSet
filterRegsUsed p e =
    foldRegsUsed (\regs r -> if p r then extendRegSet regs r else regs)
                 emptyRegSet e

instance UserOfLocalRegs CmmReg where
    foldRegsUsed f z (CmmLocal reg) = f z reg
    foldRegsUsed _ z (CmmGlobal _)  = z

instance DefinerOfLocalRegs CmmReg where
    foldRegsDefd f z (CmmLocal reg) = f z reg
    foldRegsDefd _ z (CmmGlobal _)  = z

instance UserOfLocalRegs LocalReg where
    foldRegsUsed f z r = f z r

instance DefinerOfLocalRegs LocalReg where
    foldRegsDefd f z r = f z r

instance UserOfLocalRegs RegSet where
    foldRegsUsed f = foldUniqSet (flip f)

instance UserOfLocalRegs CmmExpr where
  foldRegsUsed f z e = expr z e
    where expr z (CmmLit _)          = z
          expr z (CmmLoad addr _)    = foldRegsUsed f z addr
          expr z (CmmReg r)          = foldRegsUsed f z r
          expr z (CmmMachOp _ exprs) = foldRegsUsed f z exprs
          expr z (CmmRegOff r _)     = foldRegsUsed f z r
          expr z (CmmStackSlot _ _)  = z

instance UserOfLocalRegs a => UserOfLocalRegs [a] where
  foldRegsUsed _ set [] = set
  foldRegsUsed f set (x:xs) = foldRegsUsed f (foldRegsUsed f set x) xs

instance DefinerOfLocalRegs a => DefinerOfLocalRegs [a] where
  foldRegsDefd _ set [] = set
  foldRegsDefd f set (x:xs) = foldRegsDefd f (foldRegsDefd f set x) xs

instance DefinerOfLocalRegs a => DefinerOfLocalRegs (Maybe a) where
  foldRegsDefd _ set Nothing  = set
  foldRegsDefd f set (Just x) = foldRegsDefd f set x

-----------------------------------------------------------------------------
-- Another reg utility

regUsedIn :: CmmReg -> CmmExpr -> Bool
_   `regUsedIn` CmmLit _ 	 = False
reg `regUsedIn` CmmLoad e  _ 	 = reg `regUsedIn` e
reg `regUsedIn` CmmReg reg' 	 = reg == reg'
reg `regUsedIn` CmmRegOff reg' _ = reg == reg'
reg `regUsedIn` CmmMachOp _ es   = any (reg `regUsedIn`) es
_   `regUsedIn` CmmStackSlot _ _ = False

-----------------------------------------------------------------------------
--    Stack slots
-----------------------------------------------------------------------------

isStackSlotOf :: CmmExpr -> LocalReg -> Bool
isStackSlotOf (CmmStackSlot (RegSlot r) _) r' = r == r'
isStackSlotOf _ _ = False

-----------------------------------------------------------------------------
--    Stack slot use information for expressions and other types [_$_]
-----------------------------------------------------------------------------

-- Fold over the area, the offset into the area, and the width of the subarea.
class UserOfSlots a where
  foldSlotsUsed :: (b -> SubArea -> b) -> b -> a -> b

class DefinerOfSlots a where
  foldSlotsDefd :: (b -> SubArea -> b) -> b -> a -> b

instance UserOfSlots CmmExpr where
  foldSlotsUsed f z e = expr z e
    where expr z (CmmLit _)          = z
          expr z (CmmLoad (CmmStackSlot a i) ty) = f z (a, i, widthInBytes $ typeWidth ty)
          expr z (CmmLoad addr _)    = foldSlotsUsed f z addr
          expr z (CmmReg _)          = z
          expr z (CmmMachOp _ exprs) = foldSlotsUsed f z exprs
          expr z (CmmRegOff _ _)     = z
          expr z (CmmStackSlot _ _)  = z

instance UserOfSlots a => UserOfSlots [a] where
  foldSlotsUsed _ set [] = set
  foldSlotsUsed f set (x:xs) = foldSlotsUsed f (foldSlotsUsed f set x) xs

instance DefinerOfSlots a => DefinerOfSlots [a] where
  foldSlotsDefd _ set [] = set
  foldSlotsDefd f set (x:xs) = foldSlotsDefd f (foldSlotsDefd f set x) xs

instance DefinerOfSlots SubArea where
    foldSlotsDefd f z a = f z a

-----------------------------------------------------------------------------
--		Global STG registers
-----------------------------------------------------------------------------

data VGcPtr = VGcPtr | VNonGcPtr deriving( Eq, Show )
	-- TEMPORARY!!!

-----------------------------------------------------------------------------
--		Global STG registers
-----------------------------------------------------------------------------
vgcFlag :: CmmType -> VGcPtr
vgcFlag ty | isGcPtrType ty = VGcPtr
	   | otherwise	    = VNonGcPtr

data GlobalReg
  -- Argument and return registers
  = VanillaReg			-- pointers, unboxed ints and chars
	{-# UNPACK #-} !Int	-- its number
 	VGcPtr

  | FloatReg		-- single-precision floating-point registers
	{-# UNPACK #-} !Int	-- its number

  | DoubleReg		-- double-precision floating-point registers
	{-# UNPACK #-} !Int	-- its number

  | LongReg	        -- long int registers (64-bit, really)
	{-# UNPACK #-} !Int	-- its number

  -- STG registers
  | Sp			-- Stack ptr; points to last occupied stack location.
  | SpLim		-- Stack limit
  | Hp			-- Heap ptr; points to last occupied heap location.
  | HpLim		-- Heap limit register
  | CurrentTSO		-- pointer to current thread's TSO
  | CurrentNursery	-- pointer to allocation area
  | HpAlloc		-- allocation count for heap check failure

		-- We keep the address of some commonly-called 
		-- functions in the register table, to keep code
		-- size down:
  | EagerBlackholeInfo  -- stg_EAGER_BLACKHOLE_info
  | GCEnter1		-- stg_gc_enter_1
  | GCFun		-- stg_gc_fun

  -- Base offset for the register table, used for accessing registers
  -- which do not have real registers assigned to them.  This register
  -- will only appear after we have expanded GlobalReg into memory accesses
  -- (where necessary) in the native code generator.
  | BaseReg

  -- Base Register for PIC (position-independent code) calculations
  -- Only used inside the native code generator. It's exact meaning differs
  -- from platform to platform (see module PositionIndependentCode).
  | PicBaseReg

  deriving( Show )

instance Eq GlobalReg where
   VanillaReg i _ == VanillaReg j _ = i==j	-- Ignore type when seeking clashes
   FloatReg i == FloatReg j = i==j
   DoubleReg i == DoubleReg j = i==j
   LongReg i == LongReg j = i==j
   Sp == Sp = True
   SpLim == SpLim = True
   Hp == Hp = True
   HpLim == HpLim = True
   CurrentTSO == CurrentTSO = True
   CurrentNursery == CurrentNursery = True
   HpAlloc == HpAlloc = True
   GCEnter1 == GCEnter1 = True
   GCFun == GCFun = True
   BaseReg == BaseReg = True
   PicBaseReg == PicBaseReg = True
   _r1 == _r2 = False

instance Ord GlobalReg where
   compare (VanillaReg i _) (VanillaReg j _) = compare i j
     -- Ignore type when seeking clashes
   compare (FloatReg i)  (FloatReg  j) = compare i j
   compare (DoubleReg i) (DoubleReg j) = compare i j
   compare (LongReg i)   (LongReg   j) = compare i j
   compare Sp Sp = EQ
   compare SpLim SpLim = EQ
   compare Hp Hp = EQ
   compare HpLim HpLim = EQ
   compare CurrentTSO CurrentTSO = EQ
   compare CurrentNursery CurrentNursery = EQ
   compare HpAlloc HpAlloc = EQ
   compare EagerBlackholeInfo EagerBlackholeInfo = EQ
   compare GCEnter1 GCEnter1 = EQ
   compare GCFun GCFun = EQ
   compare BaseReg BaseReg = EQ
   compare PicBaseReg PicBaseReg = EQ
   compare (VanillaReg _ _) _ = LT
   compare _ (VanillaReg _ _) = GT
   compare (FloatReg _) _     = LT
   compare _ (FloatReg _)     = GT
   compare (DoubleReg _) _    = LT
   compare _ (DoubleReg _)    = GT
   compare (LongReg _) _      = LT
   compare _ (LongReg _)      = GT
   compare Sp _ = LT
   compare _ Sp = GT
   compare SpLim _ = LT
   compare _ SpLim = GT
   compare Hp _ = LT
   compare _ Hp = GT
   compare HpLim _ = LT
   compare _ HpLim = GT
   compare CurrentTSO _ = LT
   compare _ CurrentTSO = GT
   compare CurrentNursery _ = LT
   compare _ CurrentNursery = GT
   compare HpAlloc _ = LT
   compare _ HpAlloc = GT
   compare GCEnter1 _ = LT
   compare _ GCEnter1 = GT
   compare GCFun _ = LT
   compare _ GCFun = GT
   compare BaseReg _ = LT
   compare _ BaseReg = GT
   compare EagerBlackholeInfo _ = LT
   compare _ EagerBlackholeInfo = GT

-- convenient aliases
spReg, hpReg, spLimReg, nodeReg :: CmmReg
spReg = CmmGlobal Sp
hpReg = CmmGlobal Hp
spLimReg = CmmGlobal SpLim
nodeReg = CmmGlobal node

node :: GlobalReg
node = VanillaReg 1 VGcPtr

globalRegType :: GlobalReg -> CmmType
globalRegType (VanillaReg _ VGcPtr)    = gcWord
globalRegType (VanillaReg _ VNonGcPtr) = bWord
globalRegType (FloatReg _) 	= cmmFloat W32
globalRegType (DoubleReg _) 	= cmmFloat W64
globalRegType (LongReg _) 	= cmmBits W64
globalRegType Hp		= gcWord	-- The initialiser for all 
					    	-- dynamically allocated closures
globalRegType _			= bWord
