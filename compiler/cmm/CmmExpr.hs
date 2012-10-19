{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module CmmExpr
    ( CmmExpr(..), cmmExprType, cmmExprWidth, maybeInvertCmmExpr
    , CmmReg(..), cmmRegType
    , CmmLit(..), cmmLitType
    , LocalReg(..), localRegType
    , GlobalReg(..), isArgReg, globalRegType, spReg, hpReg, spLimReg, nodeReg, node, baseReg
    , VGcPtr(..), vgcFlag       -- Temporary!

    , DefinerOfRegs, UserOfRegs
    , foldRegsDefd, foldRegsUsed, filterRegsUsed
    , foldLocalRegsDefd, foldLocalRegsUsed

    , RegSet, LocalRegSet, GlobalRegSet
    , emptyRegSet, elemRegSet, extendRegSet, deleteFromRegSet, mkRegSet
    , plusRegSet, minusRegSet, timesRegSet, sizeRegSet, nullRegSet
    , regSetToList
    , regUsedIn
    
    , Area(..)
    , module CmmMachOp
    , module CmmType
    )
where

#include "HsVersions.h"

import CmmType
import CmmMachOp
import BlockId
import CLabel
import DynFlags
import Unique
import Outputable (panic)

import Data.Set (Set)
import qualified Data.Set as Set

-----------------------------------------------------------------------------
--              CmmExpr
-- An expression.  Expressions have no side effects.
-----------------------------------------------------------------------------

data CmmExpr
  = CmmLit CmmLit               -- Literal
  | CmmLoad !CmmExpr !CmmType   -- Read memory location
  | CmmReg !CmmReg              -- Contents of register
  | CmmMachOp MachOp [CmmExpr]  -- Machine operation (+, -, *, etc.)
  | CmmStackSlot Area {-# UNPACK #-} !Int
                                -- addressing expression of a stack slot
  | CmmRegOff !CmmReg Int
        -- CmmRegOff reg i
        --        ** is shorthand only, meaning **
        -- CmmMachOp (MO_Add rep) [x, CmmLit (CmmInt (fromIntegral i) rep)]
        --      where rep = typeWidth (cmmRegType reg)

instance Eq CmmExpr where       -- Equality ignores the types
  CmmLit l1          == CmmLit l2          = l1==l2
  CmmLoad e1 _       == CmmLoad e2 _       = e1==e2
  CmmReg r1          == CmmReg r2          = r1==r2
  CmmRegOff r1 i1    == CmmRegOff r2 i2    = r1==r2 && i1==i2
  CmmMachOp op1 es1  == CmmMachOp op2 es2  = op1==op2 && es1==es2
  CmmStackSlot a1 i1 == CmmStackSlot a2 i2 = a1==a2 && i1==i2
  _e1                == _e2                = False

data CmmReg
  = CmmLocal  {-# UNPACK #-} !LocalReg
  | CmmGlobal GlobalReg
  deriving( Eq, Ord )

-- | A stack area is either the stack slot where a variable is spilled
-- or the stack space where function arguments and results are passed.
data Area
  = Old            -- See Note [Old Area]
  | Young {-# UNPACK #-} !BlockId  -- Invariant: must be a continuation BlockId
                   -- See Note [Continuation BlockId] in CmmNode.
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

data CmmLit
  = CmmInt !Integer  Width
        -- Interpretation: the 2's complement representation of the value
        -- is truncated to the specified size.  This is easier than trying
        -- to keep the value within range, because we don't know whether
        -- it will be used as a signed or unsigned value (the CmmType doesn't
        -- distinguish between signed & unsigned).
  | CmmFloat  Rational Width
  | CmmVec [CmmLit]                     -- Vector literal
  | CmmLabel    CLabel                  -- Address of label
  | CmmLabelOff CLabel Int              -- Address of label + byte offset

        -- Due to limitations in the C backend, the following
        -- MUST ONLY be used inside the info table indicated by label2
        -- (label2 must be the info label), and label1 must be an
        -- SRT, a slow entrypoint or a large bitmap (see the Mangler)
        -- Don't use it at all unless tablesNextToCode.
        -- It is also used inside the NCG during when generating
        -- position-independent code.
  | CmmLabelDiffOff CLabel CLabel Int   -- label1 - label2 + offset

  | CmmBlock {-# UNPACK #-} !BlockId     -- Code label
        -- Invariant: must be a continuation BlockId
        -- See Note [Continuation BlockId] in CmmNode.

  | CmmHighStackMark -- stands for the max stack space used during a procedure
  deriving Eq

cmmExprType :: DynFlags -> CmmExpr -> CmmType
cmmExprType dflags (CmmLit lit)        = cmmLitType dflags lit
cmmExprType _      (CmmLoad _ rep)     = rep
cmmExprType dflags (CmmReg reg)        = cmmRegType dflags reg
cmmExprType dflags (CmmMachOp op args) = machOpResultType dflags op (map (cmmExprType dflags) args)
cmmExprType dflags (CmmRegOff reg _)   = cmmRegType dflags reg
cmmExprType dflags (CmmStackSlot _ _)  = bWord dflags -- an address
-- Careful though: what is stored at the stack slot may be bigger than
-- an address

cmmLitType :: DynFlags -> CmmLit -> CmmType
cmmLitType _      (CmmInt _ width)     = cmmBits  width
cmmLitType _      (CmmFloat _ width)   = cmmFloat width
cmmLitType _      (CmmVec [])          = panic "cmmLitType: CmmVec []"
cmmLitType cflags (CmmVec (l:ls))      = let ty = cmmLitType cflags l
                                         in if all (`cmmEqType` ty) (map (cmmLitType cflags) ls)
                                            then cmmVec (1+length ls) ty
                                            else panic "cmmLitType: CmmVec"
cmmLitType dflags (CmmLabel lbl)       = cmmLabelType dflags lbl
cmmLitType dflags (CmmLabelOff lbl _)  = cmmLabelType dflags lbl
cmmLitType dflags (CmmLabelDiffOff {}) = bWord dflags
cmmLitType dflags (CmmBlock _)         = bWord dflags
cmmLitType dflags (CmmHighStackMark)   = bWord dflags

cmmLabelType :: DynFlags -> CLabel -> CmmType
cmmLabelType dflags lbl
 | isGcPtrLabel lbl = gcWord dflags
 | otherwise        = bWord dflags

cmmExprWidth :: DynFlags -> CmmExpr -> Width
cmmExprWidth dflags e = typeWidth (cmmExprType dflags e)

--------
--- Negation for conditional branches

maybeInvertCmmExpr :: CmmExpr -> Maybe CmmExpr
maybeInvertCmmExpr (CmmMachOp op args) = do op' <- maybeInvertComparison op
                                            return (CmmMachOp op' args)
maybeInvertCmmExpr _ = Nothing

-----------------------------------------------------------------------------
--              Local registers
-----------------------------------------------------------------------------

data LocalReg
  = LocalReg {-# UNPACK #-} !Unique CmmType
    -- ^ Parameters:
    --   1. Identifier
    --   2. Type

instance Eq LocalReg where
  (LocalReg u1 _) == (LocalReg u2 _) = u1 == u2

instance Ord LocalReg where
  compare (LocalReg u1 _) (LocalReg u2 _) = compare u1 u2

instance Uniquable LocalReg where
  getUnique (LocalReg uniq _) = uniq

cmmRegType :: DynFlags -> CmmReg -> CmmType
cmmRegType _      (CmmLocal  reg) = localRegType reg
cmmRegType dflags (CmmGlobal reg) = globalRegType dflags reg

localRegType :: LocalReg -> CmmType
localRegType (LocalReg _ rep) = rep

-----------------------------------------------------------------------------
--    Register-use information for expressions and other types
-----------------------------------------------------------------------------

-- | Sets of registers

-- These are used for dataflow facts, and a common operation is taking
-- the union of two RegSets and then asking whether the union is the
-- same as one of the inputs.  UniqSet isn't good here, because
-- sizeUniqSet is O(n) whereas Set.size is O(1), so we use ordinary
-- Sets.

type RegSet r     = Set r
type LocalRegSet  = RegSet LocalReg
type GlobalRegSet = RegSet GlobalReg

emptyRegSet             :: Ord r => RegSet r
nullRegSet              :: Ord r => RegSet r -> Bool
elemRegSet              :: Ord r => r -> RegSet r -> Bool
extendRegSet            :: Ord r => RegSet r -> r -> RegSet r
deleteFromRegSet        :: Ord r => RegSet r -> r -> RegSet r
mkRegSet                :: Ord r => [r] -> RegSet r
minusRegSet, plusRegSet, timesRegSet :: Ord r => RegSet r -> RegSet r -> RegSet r
sizeRegSet              :: Ord r => RegSet r -> Int
regSetToList            :: Ord r => RegSet r -> [r]

emptyRegSet      = Set.empty
nullRegSet       = Set.null
elemRegSet       = Set.member
extendRegSet     = flip Set.insert
deleteFromRegSet = flip Set.delete
mkRegSet         = Set.fromList
minusRegSet      = Set.difference
plusRegSet       = Set.union
timesRegSet      = Set.intersection
sizeRegSet       = Set.size
regSetToList     = Set.toList

class Ord r => UserOfRegs r a where
  foldRegsUsed :: DynFlags -> (b -> r -> b) -> b -> a -> b

foldLocalRegsUsed :: UserOfRegs LocalReg a
                  => DynFlags -> (b -> LocalReg -> b) -> b -> a -> b
foldLocalRegsUsed = foldRegsUsed

class Ord r => DefinerOfRegs r a where
  foldRegsDefd :: DynFlags -> (b -> r -> b) -> b -> a -> b

foldLocalRegsDefd :: DefinerOfRegs LocalReg a
                  => DynFlags -> (b -> LocalReg -> b) -> b -> a -> b
foldLocalRegsDefd = foldRegsDefd

filterRegsUsed :: UserOfRegs r e => DynFlags -> (r -> Bool) -> e -> RegSet r
filterRegsUsed dflags p e =
    foldRegsUsed dflags
                 (\regs r -> if p r then extendRegSet regs r else regs)
                 emptyRegSet e

instance UserOfRegs LocalReg CmmReg where
    foldRegsUsed _ f z (CmmLocal reg) = f z reg
    foldRegsUsed _ _ z (CmmGlobal _)  = z

instance DefinerOfRegs LocalReg CmmReg where
    foldRegsDefd _ f z (CmmLocal reg) = f z reg
    foldRegsDefd _ _ z (CmmGlobal _)  = z

instance UserOfRegs GlobalReg CmmReg where
    foldRegsUsed _ _ z (CmmLocal _)    = z
    foldRegsUsed _ f z (CmmGlobal reg) = f z reg

instance DefinerOfRegs GlobalReg CmmReg where
    foldRegsDefd _ _ z (CmmLocal _)    = z
    foldRegsDefd _ f z (CmmGlobal reg) = f z reg

instance Ord r => UserOfRegs r r where
    foldRegsUsed _ f z r = f z r

instance Ord r => DefinerOfRegs r r where
    foldRegsDefd _ f z r = f z r

instance Ord r => UserOfRegs r (RegSet r) where
    foldRegsUsed _ f = Set.fold (flip f)

instance UserOfRegs r CmmReg => UserOfRegs r CmmExpr where
  foldRegsUsed dflags f z e = expr z e
    where expr z (CmmLit _)          = z
          expr z (CmmLoad addr _)    = foldRegsUsed dflags f z addr
          expr z (CmmReg r)          = foldRegsUsed dflags f z r
          expr z (CmmMachOp _ exprs) = foldRegsUsed dflags f z exprs
          expr z (CmmRegOff r _)     = foldRegsUsed dflags f z r
          expr z (CmmStackSlot _ _)  = z

instance UserOfRegs r a => UserOfRegs r (Maybe a) where
    foldRegsUsed dflags f z (Just x) = foldRegsUsed dflags f z x
    foldRegsUsed _      _ z Nothing = z

instance UserOfRegs r a => UserOfRegs r [a] where
  foldRegsUsed _      _ set [] = set
  foldRegsUsed dflags f set (x:xs) = foldRegsUsed dflags f (foldRegsUsed dflags f set x) xs

instance DefinerOfRegs r a => DefinerOfRegs r [a] where
  foldRegsDefd _      _ set [] = set
  foldRegsDefd dflags f set (x:xs) = foldRegsDefd dflags f (foldRegsDefd dflags f set x) xs

instance DefinerOfRegs r a => DefinerOfRegs r (Maybe a) where
  foldRegsDefd _      _ set Nothing  = set
  foldRegsDefd dflags f set (Just x) = foldRegsDefd dflags f set x

-----------------------------------------------------------------------------
-- Another reg utility

regUsedIn :: CmmReg -> CmmExpr -> Bool
_   `regUsedIn` CmmLit _         = False
reg `regUsedIn` CmmLoad e  _     = reg `regUsedIn` e
reg `regUsedIn` CmmReg reg'      = reg == reg'
reg `regUsedIn` CmmRegOff reg' _ = reg == reg'
reg `regUsedIn` CmmMachOp _ es   = any (reg `regUsedIn`) es
_   `regUsedIn` CmmStackSlot _ _ = False

-----------------------------------------------------------------------------
--              Global STG registers
-----------------------------------------------------------------------------

data VGcPtr = VGcPtr | VNonGcPtr deriving( Eq, Show )
        -- TEMPORARY!!!

-----------------------------------------------------------------------------
--              Global STG registers
-----------------------------------------------------------------------------
vgcFlag :: CmmType -> VGcPtr
vgcFlag ty | isGcPtrType ty = VGcPtr
           | otherwise      = VNonGcPtr

data GlobalReg
  -- Argument and return registers
  = VanillaReg                  -- pointers, unboxed ints and chars
        {-# UNPACK #-} !Int     -- its number
        VGcPtr

  | FloatReg            -- single-precision floating-point registers
        {-# UNPACK #-} !Int     -- its number

  | DoubleReg           -- double-precision floating-point registers
        {-# UNPACK #-} !Int     -- its number

  | LongReg             -- long int registers (64-bit, really)
        {-# UNPACK #-} !Int     -- its number

  -- STG registers
  | Sp                  -- Stack ptr; points to last occupied stack location.
  | SpLim               -- Stack limit
  | Hp                  -- Heap ptr; points to last occupied heap location.
  | HpLim               -- Heap limit register
  | CCCS                -- Current cost-centre stack
  | CurrentTSO          -- pointer to current thread's TSO
  | CurrentNursery      -- pointer to allocation area
  | HpAlloc             -- allocation count for heap check failure

                -- We keep the address of some commonly-called
                -- functions in the register table, to keep code
                -- size down:
  | EagerBlackholeInfo  -- stg_EAGER_BLACKHOLE_info
  | GCEnter1            -- stg_gc_enter_1
  | GCFun               -- stg_gc_fun

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
   VanillaReg i _ == VanillaReg j _ = i==j -- Ignore type when seeking clashes
   FloatReg i == FloatReg j = i==j
   DoubleReg i == DoubleReg j = i==j
   LongReg i == LongReg j = i==j
   Sp == Sp = True
   SpLim == SpLim = True
   Hp == Hp = True
   HpLim == HpLim = True
   CCCS == CCCS = True
   CurrentTSO == CurrentTSO = True
   CurrentNursery == CurrentNursery = True
   HpAlloc == HpAlloc = True
   EagerBlackholeInfo == EagerBlackholeInfo = True
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
   compare CCCS CCCS = EQ
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
   compare CCCS _ = LT
   compare _ CCCS = GT
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
baseReg, spReg, hpReg, spLimReg, nodeReg :: CmmReg
baseReg = CmmGlobal BaseReg
spReg = CmmGlobal Sp
hpReg = CmmGlobal Hp
spLimReg = CmmGlobal SpLim
nodeReg = CmmGlobal node

node :: GlobalReg
node = VanillaReg 1 VGcPtr

globalRegType :: DynFlags -> GlobalReg -> CmmType
globalRegType dflags (VanillaReg _ VGcPtr)    = gcWord dflags
globalRegType dflags (VanillaReg _ VNonGcPtr) = bWord dflags
globalRegType _      (FloatReg _)      = cmmFloat W32
globalRegType _      (DoubleReg _)     = cmmFloat W64
globalRegType _      (LongReg _)       = cmmBits W64
globalRegType dflags Hp                = gcWord dflags
                                            -- The initialiser for all
                                            -- dynamically allocated closures
globalRegType dflags _                 = bWord dflags

isArgReg :: GlobalReg -> Bool
isArgReg (VanillaReg {}) = True
isArgReg (FloatReg {})   = True
isArgReg (DoubleReg {})  = True
isArgReg (LongReg {})    = True
isArgReg _               = False
