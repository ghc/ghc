
module CmmExpr
    ( CmmType	-- Abstract 
    , b8, b16, b32, b64, f32, f64, bWord, bHalfWord, gcWord
    , cInt, cLong
    , cmmBits, cmmFloat
    , typeWidth, cmmEqType, cmmEqType_ignoring_ptrhood
    , isFloatType, isGcPtrType, isWord32, isWord64, isFloat64, isFloat32
 
    , Width(..)
    , widthInBits, widthInBytes, widthInLog, widthFromBytes
    , wordWidth, halfWordWidth, cIntWidth, cLongWidth
    , narrowU, narrowS
 
    , CmmExpr(..), cmmExprType, cmmExprWidth, maybeInvertCmmExpr
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
 
   -- MachOp
    , MachOp(..) 
    , pprMachOp, isCommutableMachOp, isAssociativeMachOp
    , isComparisonMachOp, machOpResultType
    , machOpArgReps, maybeInvertComparison
 
   -- MachOp builders
    , mo_wordAdd, mo_wordSub, mo_wordEq, mo_wordNe,mo_wordMul, mo_wordSQuot
    , mo_wordSRem, mo_wordSNeg, mo_wordUQuot, mo_wordURem
    , mo_wordSGe, mo_wordSLe, mo_wordSGt, mo_wordSLt, mo_wordUGe 
    , mo_wordULe, mo_wordUGt, mo_wordULt
    , mo_wordAnd, mo_wordOr, mo_wordXor, mo_wordNot, mo_wordShl, mo_wordSShr, mo_wordUShr
    , mo_u_8To32, mo_s_8To32, mo_u_16To32, mo_s_16To32
    , mo_u_8ToWord, mo_s_8ToWord, mo_u_16ToWord, mo_s_16ToWord, mo_u_32ToWord, mo_s_32ToWord
    , mo_32To8, mo_32To16, mo_WordTo8, mo_WordTo16, mo_WordTo32
   )
where

#include "HsVersions.h"

import BlockId
import CLabel
import Constants
import FastString
import Outputable
import Unique
import UniqSet

import Data.Word
import Data.Int
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
	-- CmmMachOp (MO_S_Add rep (CmmReg reg) (CmmLit (CmmInt i rep)))
	--	where rep = cmmRegType reg

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


-----------------------------------------------------------------------------
--    		CmmType
-----------------------------------------------------------------------------

  -- NOTE: CmmType is an abstract type, not exported from this
  --	   module so you can easily change its representation
  --
  -- However Width is exported in a concrete way, 
  -- and is used extensively in pattern-matching

data CmmType 	-- The important one!
  = CmmType CmmCat Width 

data CmmCat	-- "Category" (not exported)
   = GcPtrCat	-- GC pointer
   | BitsCat 	-- Non-pointer
   | FloatCat	-- Float
   deriving( Eq )
	-- See Note [Signed vs unsigned] at the end

instance Outputable CmmType where
  ppr (CmmType cat wid) = ppr cat <> ppr (widthInBits wid)

instance Outputable CmmCat where
  ppr FloatCat	= ptext $ sLit("F")
  ppr _ 	= ptext $ sLit("I")
-- Temp Jan 08
--  ppr FloatCat	= ptext $ sLit("float")
--  ppr BitsCat   = ptext $ sLit("bits")
--  ppr GcPtrCat  = ptext $ sLit("gcptr")

-- Why is CmmType stratified?  For native code generation, 
-- most of the time you just want to know what sort of register
-- to put the thing in, and for this you need to know how
-- many bits thing has and whether it goes in a floating-point
-- register.  By contrast, the distinction between GcPtr and
-- GcNonPtr is of interest to only a few parts of the code generator.

-------- Equality on CmmType --------------
-- CmmType is *not* an instance of Eq; sometimes we care about the
-- Gc/NonGc distinction, and sometimes we don't
-- So we use an explicit function to force you to think about it
cmmEqType :: CmmType -> CmmType -> Bool	-- Exact equality
cmmEqType (CmmType c1 w1) (CmmType c2 w2) = c1==c2 && w1==w2

cmmEqType_ignoring_ptrhood :: CmmType -> CmmType -> Bool
  -- This equality is temporary; used in CmmLint
  -- but the RTS files are not yet well-typed wrt pointers
cmmEqType_ignoring_ptrhood (CmmType c1 w1) (CmmType c2 w2)
   = c1 `weak_eq` c2 && w1==w2
   where
      FloatCat `weak_eq` FloatCat = True 
      FloatCat `weak_eq` _other	  = False
      _other   `weak_eq` FloatCat = False
      _word1   `weak_eq` _word2   = True	-- Ignores GcPtr

--- Simple operations on CmmType -----
typeWidth :: CmmType -> Width
typeWidth (CmmType _ w) = w

cmmBits, cmmFloat :: Width -> CmmType
cmmBits  = CmmType BitsCat
cmmFloat = CmmType FloatCat

-------- Common CmmTypes ------------
-- Floats and words of specific widths
b8, b16, b32, b64, f32, f64 :: CmmType
b8     = cmmBits W8
b16    = cmmBits W16
b32    = cmmBits W32
b64    = cmmBits W64
f32    = cmmFloat W32
f64    = cmmFloat W64

-- CmmTypes of native word widths
bWord, bHalfWord, gcWord :: CmmType
bWord     = cmmBits wordWidth
bHalfWord = cmmBits halfWordWidth
gcWord    = CmmType GcPtrCat wordWidth

cInt, cLong :: CmmType
cInt  = cmmBits cIntWidth
cLong = cmmBits cLongWidth


------------ Predicates ----------------
isFloatType, isGcPtrType :: CmmType -> Bool
isFloatType (CmmType FloatCat    _) = True
isFloatType _other		    = False

isGcPtrType (CmmType GcPtrCat _) = True
isGcPtrType _other		 = False

isWord32, isWord64, isFloat32, isFloat64 :: CmmType -> Bool
-- isWord64 is true of 64-bit non-floats (both gc-ptrs and otherwise)
-- isFloat32 and 64 are obvious

isWord64 (CmmType BitsCat  W64) = True
isWord64 (CmmType GcPtrCat W64) = True
isWord64 _other			= False

isWord32 (CmmType BitsCat  W32) = True
isWord32 (CmmType GcPtrCat W32) = True
isWord32 _other			= False

isFloat32 (CmmType FloatCat W32) = True
isFloat32 _other		 = False

isFloat64 (CmmType FloatCat W64) = True
isFloat64 _other		 = False

-----------------------------------------------------------------------------
--    		Width
-----------------------------------------------------------------------------

data Width   = W8 | W16 | W32 | W64 
	     | W80	-- Extended double-precision float, 
			-- used in x86 native codegen only.
			-- (we use Ord, so it'd better be in this order)
	     | W128
	     deriving (Eq, Ord, Show)

instance Outputable Width where
   ppr rep = ptext (mrStr rep)

mrStr :: Width -> LitString
mrStr W8   = sLit("W8")
mrStr W16  = sLit("W16")
mrStr W32  = sLit("W32")
mrStr W64  = sLit("W64")
mrStr W128 = sLit("W128")
mrStr W80  = sLit("W80")


-------- Common Widths  ------------
wordWidth, halfWordWidth :: Width
wordWidth | wORD_SIZE == 4 = W32
	  | wORD_SIZE == 8 = W64
	  | otherwise      = panic "MachOp.wordRep: Unknown word size"

halfWordWidth | wORD_SIZE == 4 = W16
	      | wORD_SIZE == 8 = W32
	      | otherwise      = panic "MachOp.halfWordRep: Unknown word size"

-- cIntRep is the Width for a C-language 'int'
cIntWidth, cLongWidth :: Width
#if SIZEOF_INT == 4
cIntWidth = W32
#elif  SIZEOF_INT == 8
cIntWidth = W64
#endif

#if SIZEOF_LONG == 4
cLongWidth = W32
#elif  SIZEOF_LONG == 8
cLongWidth = W64
#endif

widthInBits :: Width -> Int
widthInBits W8   = 8
widthInBits W16  = 16
widthInBits W32  = 32
widthInBits W64  = 64
widthInBits W128 = 128
widthInBits W80  = 80

widthInBytes :: Width -> Int
widthInBytes W8   = 1
widthInBytes W16  = 2
widthInBytes W32  = 4
widthInBytes W64  = 8
widthInBytes W128 = 16
widthInBytes W80  = 10

widthFromBytes :: Int -> Width
widthFromBytes 1  = W8
widthFromBytes 2  = W16
widthFromBytes 4  = W32
widthFromBytes 8  = W64
widthFromBytes 16 = W128
widthFromBytes 10 = W80
widthFromBytes n  = pprPanic "no width for given number of bytes" (ppr n)

-- log_2 of the width in bytes, useful for generating shifts.
widthInLog :: Width -> Int
widthInLog W8   = 0
widthInLog W16  = 1
widthInLog W32  = 2
widthInLog W64  = 3
widthInLog W128 = 4
widthInLog W80  = panic "widthInLog: F80"

-- widening / narrowing

narrowU :: Width -> Integer -> Integer
narrowU W8  x = fromIntegral (fromIntegral x :: Word8)
narrowU W16 x = fromIntegral (fromIntegral x :: Word16)
narrowU W32 x = fromIntegral (fromIntegral x :: Word32)
narrowU W64 x = fromIntegral (fromIntegral x :: Word64)
narrowU _ _ = panic "narrowTo"

narrowS :: Width -> Integer -> Integer
narrowS W8  x = fromIntegral (fromIntegral x :: Int8)
narrowS W16 x = fromIntegral (fromIntegral x :: Int16)
narrowS W32 x = fromIntegral (fromIntegral x :: Int32)
narrowS W64 x = fromIntegral (fromIntegral x :: Int64)
narrowS _ _ = panic "narrowTo"

-----------------------------------------------------------------------------
--    		MachOp
-----------------------------------------------------------------------------

{- 
Implementation notes:

It might suffice to keep just a width, without distinguishing between
floating and integer types.  However, keeping the distinction will
help the native code generator to assign registers more easily.
-}


{- |
Machine-level primops; ones which we can reasonably delegate to the
native code generators to handle.  Basically contains C's primops
and no others.

Nomenclature: all ops indicate width and signedness, where
appropriate.  Widths: 8\/16\/32\/64 means the given size, obviously.
Nat means the operation works on STG word sized objects.
Signedness: S means signed, U means unsigned.  For operations where
signedness is irrelevant or makes no difference (for example
integer add), the signedness component is omitted.

An exception: NatP is a ptr-typed native word.  From the point of
view of the native code generators this distinction is irrelevant,
but the C code generator sometimes needs this info to emit the
right casts.  
-}

data MachOp
  -- Integer operations (insensitive to signed/unsigned)
  = MO_Add Width
  | MO_Sub Width
  | MO_Eq  Width
  | MO_Ne  Width
  | MO_Mul Width		-- low word of multiply

  -- Signed multiply/divide
  | MO_S_MulMayOflo Width 	-- nonzero if signed multiply overflows
  | MO_S_Quot Width		-- signed / (same semantics as IntQuotOp)
  | MO_S_Rem  Width		-- signed % (same semantics as IntRemOp)
  | MO_S_Neg  Width		-- unary -

  -- Unsigned multiply/divide
  | MO_U_MulMayOflo Width	-- nonzero if unsigned multiply overflows
  | MO_U_Quot Width		-- unsigned / (same semantics as WordQuotOp)
  | MO_U_Rem  Width		-- unsigned % (same semantics as WordRemOp)

  -- Signed comparisons
  | MO_S_Ge Width
  | MO_S_Le Width
  | MO_S_Gt Width
  | MO_S_Lt Width

  -- Unsigned comparisons
  | MO_U_Ge Width
  | MO_U_Le Width
  | MO_U_Gt Width
  | MO_U_Lt Width

  -- Floating point arithmetic
  | MO_F_Add  Width
  | MO_F_Sub  Width
  | MO_F_Neg  Width		-- unary -
  | MO_F_Mul  Width
  | MO_F_Quot Width

  -- Floating point comparison
  | MO_F_Eq Width
  | MO_F_Ne Width
  | MO_F_Ge Width
  | MO_F_Le Width
  | MO_F_Gt Width
  | MO_F_Lt Width

  -- Bitwise operations.  Not all of these may be supported 
  -- at all sizes, and only integral Widths are valid.
  | MO_And   Width
  | MO_Or    Width
  | MO_Xor   Width
  | MO_Not   Width
  | MO_Shl   Width
  | MO_U_Shr Width	-- unsigned shift right
  | MO_S_Shr Width	-- signed shift right

  -- Conversions.  Some of these will be NOPs.
  -- Floating-point conversions use the signed variant.
  | MO_SF_Conv Width Width 	-- Signed int -> Float
  | MO_FS_Conv Width Width 	-- Float -> Signed int
  | MO_SS_Conv Width Width 	-- Signed int -> Signed int
  | MO_UU_Conv Width Width 	-- unsigned int -> unsigned int
  | MO_FF_Conv Width Width 	-- Float -> Float
  deriving (Eq, Show)

pprMachOp :: MachOp -> SDoc
pprMachOp mo = text (show mo)



-- -----------------------------------------------------------------------------
-- Some common MachReps

-- A 'wordRep' is a machine word on the target architecture
-- Specifically, it is the size of an Int#, Word#, Addr# 
-- and the unit of allocation on the stack and the heap
-- Any pointer is also guaranteed to be a wordRep.

mo_wordAdd, mo_wordSub, mo_wordEq, mo_wordNe,mo_wordMul, mo_wordSQuot
    , mo_wordSRem, mo_wordSNeg, mo_wordUQuot, mo_wordURem
    , mo_wordSGe, mo_wordSLe, mo_wordSGt, mo_wordSLt, mo_wordUGe 
    , mo_wordULe, mo_wordUGt, mo_wordULt
    , mo_wordAnd, mo_wordOr, mo_wordXor, mo_wordNot, mo_wordShl, mo_wordSShr, mo_wordUShr
    , mo_u_8To32, mo_s_8To32, mo_u_16To32, mo_s_16To32
    , mo_u_8ToWord, mo_s_8ToWord, mo_u_16ToWord, mo_s_16ToWord, mo_u_32ToWord, mo_s_32ToWord
    , mo_32To8, mo_32To16, mo_WordTo8, mo_WordTo16, mo_WordTo32
    :: MachOp

mo_wordAdd	= MO_Add wordWidth
mo_wordSub	= MO_Sub wordWidth
mo_wordEq 	= MO_Eq  wordWidth
mo_wordNe 	= MO_Ne  wordWidth
mo_wordMul	= MO_Mul wordWidth
mo_wordSQuot	= MO_S_Quot wordWidth
mo_wordSRem	= MO_S_Rem wordWidth
mo_wordSNeg	= MO_S_Neg wordWidth
mo_wordUQuot	= MO_U_Quot wordWidth
mo_wordURem	= MO_U_Rem wordWidth

mo_wordSGe	= MO_S_Ge  wordWidth
mo_wordSLe	= MO_S_Le  wordWidth
mo_wordSGt	= MO_S_Gt  wordWidth
mo_wordSLt	= MO_S_Lt  wordWidth

mo_wordUGe	= MO_U_Ge  wordWidth
mo_wordULe	= MO_U_Le  wordWidth
mo_wordUGt	= MO_U_Gt  wordWidth
mo_wordULt	= MO_U_Lt  wordWidth

mo_wordAnd	= MO_And wordWidth
mo_wordOr 	= MO_Or	 wordWidth
mo_wordXor	= MO_Xor wordWidth
mo_wordNot	= MO_Not wordWidth
mo_wordShl	= MO_Shl wordWidth
mo_wordSShr	= MO_S_Shr wordWidth 
mo_wordUShr	= MO_U_Shr wordWidth 

mo_u_8To32	= MO_UU_Conv W8 W32
mo_s_8To32	= MO_SS_Conv W8 W32
mo_u_16To32	= MO_UU_Conv W16 W32
mo_s_16To32	= MO_SS_Conv W16 W32

mo_u_8ToWord	= MO_UU_Conv W8  wordWidth
mo_s_8ToWord	= MO_SS_Conv W8  wordWidth
mo_u_16ToWord	= MO_UU_Conv W16 wordWidth
mo_s_16ToWord	= MO_SS_Conv W16 wordWidth
mo_s_32ToWord	= MO_SS_Conv W32 wordWidth
mo_u_32ToWord	= MO_UU_Conv W32 wordWidth

mo_WordTo8	= MO_UU_Conv wordWidth W8
mo_WordTo16	= MO_UU_Conv wordWidth W16
mo_WordTo32	= MO_UU_Conv wordWidth W32

mo_32To8	= MO_UU_Conv W32 W8
mo_32To16	= MO_UU_Conv W32 W16


-- ----------------------------------------------------------------------------
-- isCommutableMachOp

{- |
Returns 'True' if the MachOp has commutable arguments.  This is used
in the platform-independent Cmm optimisations.

If in doubt, return 'False'.  This generates worse code on the
native routes, but is otherwise harmless.
-}
isCommutableMachOp :: MachOp -> Bool
isCommutableMachOp mop = 
  case mop of
	MO_Add _ 		-> True
	MO_Eq _			-> True
	MO_Ne _			-> True
	MO_Mul _		-> True
	MO_S_MulMayOflo _	-> True
	MO_U_MulMayOflo _	-> True
	MO_And _		-> True
	MO_Or _			-> True
	MO_Xor _		-> True
	_other			-> False

-- ----------------------------------------------------------------------------
-- isAssociativeMachOp

{- |
Returns 'True' if the MachOp is associative (i.e. @(x+y)+z == x+(y+z)@)
This is used in the platform-independent Cmm optimisations.

If in doubt, return 'False'.  This generates worse code on the
native routes, but is otherwise harmless.
-}
isAssociativeMachOp :: MachOp -> Bool
isAssociativeMachOp mop = 
  case mop of
	MO_Add {} -> True	-- NB: does not include
	MO_Mul {} -> True --     floatint point!
	MO_And {} -> True
	MO_Or  {} -> True
	MO_Xor {} -> True
	_other	  -> False

-- ----------------------------------------------------------------------------
-- isComparisonMachOp

{- | 
Returns 'True' if the MachOp is a comparison.

If in doubt, return False.  This generates worse code on the
native routes, but is otherwise harmless.
-}
isComparisonMachOp :: MachOp -> Bool
isComparisonMachOp mop = 
  case mop of
    MO_Eq   _  -> True
    MO_Ne   _  -> True
    MO_S_Ge _  -> True
    MO_S_Le _  -> True
    MO_S_Gt _  -> True
    MO_S_Lt _  -> True
    MO_U_Ge _  -> True
    MO_U_Le _  -> True
    MO_U_Gt _  -> True
    MO_U_Lt _  -> True
    MO_F_Eq  {}	-> True
    MO_F_Ne  {}	-> True
    MO_F_Ge  {}	-> True
    MO_F_Le  {}	-> True
    MO_F_Gt  {}	-> True
    MO_F_Lt  {}	-> True
    _other     -> False

-- -----------------------------------------------------------------------------
-- Inverting conditions

-- Sometimes it's useful to be able to invert the sense of a
-- condition.  Not all conditional tests are invertible: in
-- particular, floating point conditionals cannot be inverted, because
-- there exist floating-point values which return False for both senses
-- of a condition (eg. !(NaN > NaN) && !(NaN /<= NaN)).

maybeInvertComparison :: MachOp -> Maybe MachOp
maybeInvertComparison op
  = case op of	-- None of these Just cases include floating point
	MO_Eq r   -> Just (MO_Ne r)
	MO_Ne r	  -> Just (MO_Eq r)
	MO_U_Lt r -> Just (MO_U_Ge r)
	MO_U_Gt r -> Just (MO_U_Le r)
	MO_U_Le r -> Just (MO_U_Gt r)
	MO_U_Ge r -> Just (MO_U_Lt r)
	MO_S_Lt r -> Just (MO_S_Ge r)
	MO_S_Gt r -> Just (MO_S_Le r)
	MO_S_Le r -> Just (MO_S_Gt r)
	MO_S_Ge r -> Just (MO_S_Lt r)
    	MO_F_Eq r -> Just (MO_F_Ne r)
    	MO_F_Ne r -> Just (MO_F_Eq r)
    	MO_F_Ge r -> Just (MO_F_Le r)
    	MO_F_Le r -> Just (MO_F_Ge r)	
    	MO_F_Gt r -> Just (MO_F_Lt r)	
    	MO_F_Lt r -> Just (MO_F_Gt r)	
	_other    -> Nothing

-- ----------------------------------------------------------------------------
-- machOpResultType

{- |
Returns the MachRep of the result of a MachOp.
-}
machOpResultType :: MachOp -> [CmmType] -> CmmType
machOpResultType mop tys =
  case mop of
    MO_Add {}		-> ty1	-- Preserve GC-ptr-hood
    MO_Sub {} 		-> ty1	-- of first arg
    MO_Mul    r		-> cmmBits r
    MO_S_MulMayOflo r	-> cmmBits r
    MO_S_Quot r		-> cmmBits r
    MO_S_Rem  r		-> cmmBits r
    MO_S_Neg  r		-> cmmBits r
    MO_U_MulMayOflo r	-> cmmBits r
    MO_U_Quot r		-> cmmBits r
    MO_U_Rem  r		-> cmmBits r

    MO_Eq {}		-> comparisonResultRep
    MO_Ne {}		-> comparisonResultRep
    MO_S_Ge {}		-> comparisonResultRep
    MO_S_Le {}		-> comparisonResultRep
    MO_S_Gt {}		-> comparisonResultRep
    MO_S_Lt {}		-> comparisonResultRep

    MO_U_Ge {}		-> comparisonResultRep
    MO_U_Le {}		-> comparisonResultRep
    MO_U_Gt {}		-> comparisonResultRep
    MO_U_Lt {}		-> comparisonResultRep

    MO_F_Add r		-> cmmFloat r
    MO_F_Sub r		-> cmmFloat r
    MO_F_Mul r		-> cmmFloat r
    MO_F_Quot r		-> cmmFloat r
    MO_F_Neg r		-> cmmFloat r
    MO_F_Eq  {}		-> comparisonResultRep
    MO_F_Ne  {}		-> comparisonResultRep
    MO_F_Ge  {}		-> comparisonResultRep
    MO_F_Le  {}		-> comparisonResultRep
    MO_F_Gt  {}		-> comparisonResultRep
    MO_F_Lt  {}		-> comparisonResultRep

    MO_And {}		-> ty1	-- Used for pointer masking
    MO_Or {}		-> ty1
    MO_Xor {}		-> ty1
    MO_Not   r		-> cmmBits r
    MO_Shl   r		-> cmmBits r
    MO_U_Shr r		-> cmmBits r
    MO_S_Shr r		-> cmmBits r

    MO_SS_Conv _ to	-> cmmBits to
    MO_UU_Conv _ to	-> cmmBits to
    MO_FS_Conv _ to	-> cmmBits to
    MO_SF_Conv _ to	-> cmmFloat to
    MO_FF_Conv _ to	-> cmmFloat to
  where
    (ty1:_) = tys

comparisonResultRep :: CmmType
comparisonResultRep = bWord  -- is it?


-- -----------------------------------------------------------------------------
-- machOpArgReps

-- | This function is used for debugging only: we can check whether an
-- application of a MachOp is "type-correct" by checking that the MachReps of
-- its arguments are the same as the MachOp expects.  This is used when 
-- linting a CmmExpr.

machOpArgReps :: MachOp -> [Width]
machOpArgReps op = 
  case op of
    MO_Add    r		-> [r,r]
    MO_Sub    r		-> [r,r]
    MO_Eq     r		-> [r,r]
    MO_Ne     r		-> [r,r]
    MO_Mul    r		-> [r,r]
    MO_S_MulMayOflo r	-> [r,r]
    MO_S_Quot r		-> [r,r]
    MO_S_Rem  r		-> [r,r]
    MO_S_Neg  r		-> [r]
    MO_U_MulMayOflo r	-> [r,r]
    MO_U_Quot r		-> [r,r]
    MO_U_Rem  r		-> [r,r]

    MO_S_Ge r		-> [r,r]
    MO_S_Le r		-> [r,r]
    MO_S_Gt r		-> [r,r]
    MO_S_Lt r		-> [r,r]

    MO_U_Ge r		-> [r,r]
    MO_U_Le r		-> [r,r]
    MO_U_Gt r		-> [r,r]
    MO_U_Lt r		-> [r,r]

    MO_F_Add r		-> [r,r]
    MO_F_Sub r		-> [r,r]
    MO_F_Mul r		-> [r,r]
    MO_F_Quot r		-> [r,r]
    MO_F_Neg r		-> [r]
    MO_F_Eq  r		-> [r,r]
    MO_F_Ne  r		-> [r,r]
    MO_F_Ge  r		-> [r,r]
    MO_F_Le  r		-> [r,r]
    MO_F_Gt  r		-> [r,r]
    MO_F_Lt  r		-> [r,r]

    MO_And   r		-> [r,r]
    MO_Or    r		-> [r,r]
    MO_Xor   r		-> [r,r]
    MO_Not   r		-> [r]
    MO_Shl   r		-> [r,wordWidth]
    MO_U_Shr r		-> [r,wordWidth]
    MO_S_Shr r		-> [r,wordWidth]

    MO_SS_Conv from _	-> [from]
    MO_UU_Conv from _   -> [from]
    MO_SF_Conv from _	-> [from]
    MO_FS_Conv from _	-> [from]
    MO_FF_Conv from _	-> [from]


-------------------------------------------------------------------------
{-	Note [Signed vs unsigned]
	~~~~~~~~~~~~~~~~~~~~~~~~~
Should a CmmType include a signed vs. unsigned distinction?

This is very much like a "hint" in C-- terminology: it isn't necessary
in order to generate correct code, but it might be useful in that the
compiler can generate better code if it has access to higher-level
hints about data.  This is important at call boundaries, because the
definition of a function is not visible at all of its call sites, so
the compiler cannot infer the hints.

Here in Cmm, we're taking a slightly different approach.  We include
the int vs. float hint in the MachRep, because (a) the majority of
platforms have a strong distinction between float and int registers,
and (b) we don't want to do any heavyweight hint-inference in the
native code backend in order to get good code.  We're treating the
hint more like a type: our Cmm is always completely consistent with
respect to hints.  All coercions between float and int are explicit.

What about the signed vs. unsigned hint?  This information might be
useful if we want to keep sub-word-sized values in word-size
registers, which we must do if we only have word-sized registers.

On such a system, there are two straightforward conventions for
representing sub-word-sized values:

(a) Leave the upper bits undefined.  Comparison operations must
    sign- or zero-extend both operands before comparing them,
    depending on whether the comparison is signed or unsigned.

(b) Always keep the values sign- or zero-extended as appropriate.
    Arithmetic operations must narrow the result to the appropriate
    size.

A clever compiler might not use either (a) or (b) exclusively, instead
it would attempt to minimize the coercions by analysis: the same kind
of analysis that propagates hints around.  In Cmm we don't want to
have to do this, so we plump for having richer types and keeping the
type information consistent.

If signed/unsigned hints are missing from MachRep, then the only
choice we have is (a), because we don't know whether the result of an
operation should be sign- or zero-extended.

Many architectures have extending load operations, which work well
with (b).  To make use of them with (a), you need to know whether the
value is going to be sign- or zero-extended by an enclosing comparison
(for example), which involves knowing above the context.  This is
doable but more complex.

Further complicating the issue is foreign calls: a foreign calling
convention can specify that signed 8-bit quantities are passed as
sign-extended 32 bit quantities, for example (this is the case on the
PowerPC).  So we *do* need sign information on foreign call arguments.

Pros for adding signed vs. unsigned to MachRep:

  - It would let us use convention (b) above, and get easier
    code generation for extending loads.

  - Less information required on foreign calls.
  
  - MachOp type would be simpler

Cons:

  - More complexity

  - What is the MachRep for a VanillaReg?  Currently it is
    always wordRep, but now we have to decide whether it is
    signed or unsigned.  The same VanillaReg can thus have
    different MachReps in different parts of the program.

  - Extra coercions cluttering up expressions.

Currently for GHC, the foreign call point is moot, because we do our
own promotion of sub-word-sized values to word-sized values.  The Int8
type is represnted by an Int# which is kept sign-extended at all times
(this is slightly naughty, because we're making assumptions about the
C calling convention rather early on in the compiler).  However, given
this, the cons outweigh the pros.

-}

