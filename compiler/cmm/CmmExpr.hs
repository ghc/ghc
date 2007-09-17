
module CmmExpr
    ( CmmExpr(..), cmmExprRep, maybeInvertCmmExpr
    , CmmReg(..), cmmRegRep
    , CmmLit(..), cmmLitRep
    , LocalReg(..), localRegRep, localRegGCFollow, GCKind(..)
    , GlobalReg(..), globalRegRep, spReg, hpReg, spLimReg, nodeReg, node
    , UserOfLocalRegs, foldRegsUsed, filterRegsUsed
    , RegSet, emptyRegSet, elemRegSet, extendRegSet, deleteFromRegSet, mkRegSet
            , plusRegSet, minusRegSet, timesRegSet
    )
where

import CLabel
import MachOp
import Unique
import UniqSet

-----------------------------------------------------------------------------
--		CmmExpr
-- An expression.  Expressions have no side effects.
-----------------------------------------------------------------------------

data CmmExpr
  = CmmLit CmmLit               -- Literal
  | CmmLoad CmmExpr MachRep     -- Read memory location
  | CmmReg CmmReg		-- Contents of register
  | CmmMachOp MachOp [CmmExpr]  -- Machine operation (+, -, *, etc.)
  | CmmRegOff CmmReg Int	
	-- CmmRegOff reg i
	--        ** is shorthand only, meaning **
	-- CmmMachOp (MO_S_Add rep (CmmReg reg) (CmmLit (CmmInt i rep)))
	--	where rep = cmmRegRep reg
  deriving Eq

data CmmReg 
  = CmmLocal  LocalReg
  | CmmGlobal GlobalReg
  deriving( Eq )

data CmmLit
  = CmmInt Integer  MachRep
	-- Interpretation: the 2's complement representation of the value
	-- is truncated to the specified size.  This is easier than trying
	-- to keep the value within range, because we don't know whether
	-- it will be used as a signed or unsigned value (the MachRep doesn't
	-- distinguish between signed & unsigned).
  | CmmFloat  Rational MachRep
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
  deriving Eq

instance Eq LocalReg where
  (LocalReg u1 _ _) == (LocalReg u2 _ _) = u1 == u2

instance Uniquable LocalReg where
  getUnique (LocalReg uniq _ _) = uniq

--------
--- Negation for conditional branches

maybeInvertCmmExpr :: CmmExpr -> Maybe CmmExpr
maybeInvertCmmExpr (CmmMachOp op args) = do op' <- maybeInvertComparison op
                                            return (CmmMachOp op' args)
maybeInvertCmmExpr _ = Nothing

-----------------------------------------------------------------------------
--		Local registers
-----------------------------------------------------------------------------

-- | Whether a 'LocalReg' is a GC followable pointer
data GCKind = GCKindPtr | GCKindNonPtr deriving (Eq)

data LocalReg
  = LocalReg
      !Unique   -- ^ Identifier
      MachRep   -- ^ Type
      GCKind      -- ^ Should the GC follow as a pointer

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

-----------------------------------------------------------------------------
--    Register-use information for expressions and other types 
-----------------------------------------------------------------------------

class UserOfLocalRegs a where
  foldRegsUsed :: (b -> LocalReg -> b) -> b -> a -> b

filterRegsUsed :: UserOfLocalRegs e => (LocalReg -> Bool) -> e -> RegSet
filterRegsUsed p e =
    foldRegsUsed (\regs r -> if p r then extendRegSet regs r else regs)
                 emptyRegSet e

instance UserOfLocalRegs CmmReg where
    foldRegsUsed f z (CmmLocal reg) = f z reg
    foldRegsUsed _ z (CmmGlobal _)  = z

instance UserOfLocalRegs LocalReg where
    foldRegsUsed f z r = f z r

instance UserOfLocalRegs RegSet where
    foldRegsUsed f = foldUniqSet (flip f)

instance UserOfLocalRegs CmmExpr where
  foldRegsUsed f z e = expr z e
    where expr z (CmmLit _)          = z
          expr z (CmmLoad addr _)    = foldRegsUsed f z addr
          expr z (CmmReg r)          = foldRegsUsed f z r
          expr z (CmmMachOp _ exprs) = foldRegsUsed f z exprs
          expr z (CmmRegOff r _)     = foldRegsUsed f z r

instance UserOfLocalRegs a => UserOfLocalRegs [a] where
  foldRegsUsed _ set [] = set
  foldRegsUsed f set (x:xs) = foldRegsUsed f (foldRegsUsed f set x) xs

-----------------------------------------------------------------------------
--		MachRep
-----------------------------------------------------------------------------



cmmExprRep :: CmmExpr -> MachRep
cmmExprRep (CmmLit lit)      = cmmLitRep lit
cmmExprRep (CmmLoad _ rep)   = rep
cmmExprRep (CmmReg reg)      = cmmRegRep reg
cmmExprRep (CmmMachOp op _)  = resultRepOfMachOp op
cmmExprRep (CmmRegOff reg _) = cmmRegRep reg

cmmRegRep :: CmmReg -> MachRep
cmmRegRep (CmmLocal  reg) 	= localRegRep reg
cmmRegRep (CmmGlobal reg)	= globalRegRep reg

localRegRep :: LocalReg -> MachRep
localRegRep (LocalReg _ rep _) = rep


localRegGCFollow :: LocalReg -> GCKind
localRegGCFollow (LocalReg _ _ p) = p

cmmLitRep :: CmmLit -> MachRep
cmmLitRep (CmmInt _ rep)    = rep
cmmLitRep (CmmFloat _ rep)  = rep
cmmLitRep (CmmLabel _)      = wordRep
cmmLitRep (CmmLabelOff _ _) = wordRep
cmmLitRep (CmmLabelDiffOff _ _ _) = wordRep

-----------------------------------------------------------------------------
--		Global STG registers
-----------------------------------------------------------------------------

data GlobalReg
  -- Argument and return registers
  = VanillaReg			-- pointers, unboxed ints and chars
	{-# UNPACK #-} !Int	-- its number

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

  deriving( Eq , Show )

-- convenient aliases
spReg, hpReg, spLimReg, nodeReg :: CmmReg
spReg = CmmGlobal Sp
hpReg = CmmGlobal Hp
spLimReg = CmmGlobal SpLim
nodeReg = CmmGlobal node

node :: GlobalReg
node = VanillaReg 1

globalRegRep :: GlobalReg -> MachRep
globalRegRep (VanillaReg _) 	= wordRep
globalRegRep (FloatReg _) 	= F32
globalRegRep (DoubleReg _) 	= F64
globalRegRep (LongReg _) 	= I64
globalRegRep _			= wordRep
