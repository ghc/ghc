{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.Cmm.Expr
    ( CmmExpr(..), cmmExprType, cmmExprWidth, cmmExprAlignment, maybeInvertCmmExpr
    , CmmReg(..), cmmRegType, cmmRegWidth
    , CmmLit(..), cmmLitType
    , LocalReg(..), LocalRegSet, localRegType
    , globalRegType
    , spReg, hpReg, spLimReg, hpLimReg, nodeReg
    , currentTSOReg, currentNurseryReg, hpAllocReg, cccsReg
    , baseReg

    , DefinerOfRegs, UserOfRegs
    , foldRegsDefd, foldRegsUsed
    , foldLocalRegsDefd, foldLocalRegsUsed

    , Area(..)
    , module GHC.Cmm.MachOp
    , module GHC.Cmm.Type
    )
where

import GHC.Prelude

import GHC.Platform
import GHC.Cmm.Reg.Global
import GHC.Cmm.Reg.Set
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.MachOp
import GHC.Cmm.Type
import GHC.Utils.Panic (panic)
import GHC.Utils.Outputable
import GHC.Types.Unique

import GHC.Types.Basic (Alignment, mkAlignment, alignmentOf)

-----------------------------------------------------------------------------
--              CmmExpr
-- An expression.  Expressions have no side effects.
-----------------------------------------------------------------------------

data CmmExpr
  = CmmLit !CmmLit               -- Literal
  | CmmLoad !CmmExpr !CmmType   -- Read memory location
  | CmmReg !CmmReg              -- Contents of register
  | CmmMachOp MachOp [CmmExpr]  -- Machine operation (+, -, *, etc.)
  | CmmStackSlot Area {-# UNPACK #-} !Int
                                -- addressing expression of a stack slot
                                -- See Note [CmmStackSlot aliasing]
  | CmmRegOff !CmmReg !Int
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
                   -- See Note [Continuation BlockId] in GHC.Cmm.Node.
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


{- Note [CmmStackSlot aliasing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When do two CmmStackSlots alias?

 - T[old+N] aliases with U[young(L)+M] for all T, U, L, N and M
 - T[old+N] aliases with U[old+M] only if the areas actually overlap

Or more informally, different Areas may overlap with each other.

An alternative semantics, that we previously had, was that different
Areas do not overlap.  The problem that lead to redefining the
semantics of stack areas is described below.

e.g. if we had

    x = Sp[old + 8]
    y = Sp[old + 16]

    Sp[young(L) + 8]  = L
    Sp[young(L) + 16] = y
    Sp[young(L) + 24] = x
    call f() returns to L

if areas semantically do not overlap, then we might optimise this to

    Sp[young(L) + 8]  = L
    Sp[young(L) + 16] = Sp[old + 8]
    Sp[young(L) + 24] = Sp[old + 16]
    call f() returns to L

and now young(L) cannot be allocated at the same place as old, and we
are doomed to use more stack.

  - old+8  conflicts with young(L)+8
  - old+16 conflicts with young(L)+16 and young(L)+8

so young(L)+8 == old+24 and we get

    Sp[-8]  = L
    Sp[-16] = Sp[8]
    Sp[-24] = Sp[0]
    Sp -= 24
    call f() returns to L

However, if areas are defined to be "possibly overlapping" in the
semantics, then we cannot commute any loads/stores of old with
young(L), and we will be able to re-use both old+8 and old+16 for
young(L).

    x = Sp[8]
    y = Sp[0]

    Sp[8] = L
    Sp[0] = y
    Sp[-8] = x
    Sp = Sp - 8
    call f() returns to L

Now, the assignments of y go away,

    x = Sp[8]
    Sp[8] = L
    Sp[-8] = x
    Sp = Sp - 8
    call f() returns to L
-}

data CmmLit
  = CmmInt !Integer  !Width
        -- Interpretation: the 2's complement representation of the value
        -- is truncated to the specified size.  This is easier than trying
        -- to keep the value within range, because we don't know whether
        -- it will be used as a signed or unsigned value (the CmmType doesn't
        -- distinguish between signed & unsigned).
  | CmmFloat  Rational !Width
  | CmmVec [CmmLit]                     -- Vector literal
  | CmmLabel    CLabel                  -- Address of label
  | CmmLabelOff CLabel !Int              -- Address of label + byte offset

        -- Due to limitations in the C backend, the following
        -- MUST ONLY be used inside the info table indicated by label2
        -- (label2 must be the info label), and label1 must be an
        -- SRT, a slow entrypoint or a large bitmap (see the Mangler)
        -- Don't use it at all unless tablesNextToCode.
        -- It is also used inside the NCG during when generating
        -- position-independent code.
  | CmmLabelDiffOff CLabel CLabel !Int !Width -- label1 - label2 + offset
        -- In an expression, the width just has the effect of MO_SS_Conv
        -- from wordWidth to the desired width.
        --
        -- In a static literal, the supported Widths depend on the
        -- architecture: wordWidth is supported on all
        -- architectures. Additionally W32 is supported on x86_64 when
        -- using the small memory model.

  | CmmBlock {-# UNPACK #-} !BlockId     -- Code label
        -- Invariant: must be a continuation BlockId
        -- See Note [Continuation BlockId] in GHC.Cmm.Node.

  | CmmHighStackMark -- A late-bound constant that stands for the max
                     -- #bytes of stack space used during a procedure.
                     -- During the stack-layout pass, CmmHighStackMark
                     -- is replaced by a CmmInt for the actual number
                     -- of bytes used
  deriving Eq

instance Outputable CmmLit where
  ppr (CmmInt n w) = text "CmmInt" <+> ppr n <+> ppr w
  ppr (CmmFloat n w) = text "CmmFloat" <+> text (show n) <+> ppr w
  ppr (CmmVec xs) = text "CmmVec" <+> ppr xs
  ppr (CmmLabel _) = text "CmmLabel"
  ppr (CmmLabelOff _ _) = text "CmmLabelOff"
  ppr (CmmLabelDiffOff _ _ _ _) = text "CmmLabelDiffOff"
  ppr (CmmBlock blk) = text "CmmBlock" <+> ppr blk
  ppr CmmHighStackMark = text "CmmHighStackMark"

cmmExprType :: Platform -> CmmExpr -> CmmType
cmmExprType platform = \case
   (CmmLit lit)        -> cmmLitType platform lit
   (CmmLoad _ rep)     -> rep
   (CmmReg reg)        -> cmmRegType platform reg
   (CmmMachOp op args) -> machOpResultType platform op (map (cmmExprType platform) args)
   (CmmRegOff reg _)   -> cmmRegType platform reg
   (CmmStackSlot _ _)  -> bWord platform -- an address
   -- Careful though: what is stored at the stack slot may be bigger than
   -- an address

cmmLitType :: Platform -> CmmLit -> CmmType
cmmLitType platform = \case
   (CmmInt _ width)     -> cmmBits  width
   (CmmFloat _ width)   -> cmmFloat width
   (CmmVec [])          -> panic "cmmLitType: CmmVec []"
   (CmmVec (l:ls))      -> let ty = cmmLitType platform l
                          in if all (`cmmEqType` ty) (map (cmmLitType platform) ls)
                               then cmmVec (1+length ls) ty
                               else panic "cmmLitType: CmmVec"
   (CmmLabel lbl)       -> cmmLabelType platform lbl
   (CmmLabelOff lbl _)  -> cmmLabelType platform lbl
   (CmmLabelDiffOff _ _ _ width) -> cmmBits width
   (CmmBlock _)         -> bWord platform
   (CmmHighStackMark)   -> bWord platform

cmmLabelType :: Platform -> CLabel -> CmmType
cmmLabelType platform lbl
 | isGcPtrLabel lbl = gcWord platform
 | otherwise        = bWord platform

cmmExprWidth :: Platform -> CmmExpr -> Width
cmmExprWidth platform e = typeWidth (cmmExprType platform e)

-- | Returns an alignment in bytes of a CmmExpr when it's a statically
-- known integer constant, otherwise returns an alignment of 1 byte.
-- The caller is responsible for using with a sensible CmmExpr
-- argument.
cmmExprAlignment :: CmmExpr -> Alignment
cmmExprAlignment (CmmLit (CmmInt intOff _)) = alignmentOf (fromInteger intOff)
cmmExprAlignment _                          = mkAlignment 1
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
  = LocalReg {-# UNPACK #-} !Unique !CmmType
    -- ^ Parameters:
    --   1. Identifier
    --   2. Type

type LocalRegSet  = RegSet LocalReg

instance Eq LocalReg where
  (LocalReg u1 _) == (LocalReg u2 _) = u1 == u2

-- This is non-deterministic but we do not currently support deterministic
-- code-generation. See Note [Unique Determinism and code generation]
-- See Note [No Ord for Unique]
instance Ord LocalReg where
  compare (LocalReg u1 _) (LocalReg u2 _) = nonDetCmpUnique u1 u2

instance Uniquable LocalReg where
  getUnique (LocalReg uniq _) = uniq

cmmRegType :: Platform -> CmmReg -> CmmType
cmmRegType _        (CmmLocal  reg) = localRegType reg
cmmRegType platform (CmmGlobal reg) = globalRegType platform reg

cmmRegWidth :: Platform -> CmmReg -> Width
cmmRegWidth platform = typeWidth . cmmRegType platform

localRegType :: LocalReg -> CmmType
localRegType (LocalReg _ rep) = rep

-----------------------------------------------------------------------------
--    Register-use information for expressions and other types
-----------------------------------------------------------------------------

class Ord r => UserOfRegs r a where
  foldRegsUsed :: Platform -> (b -> r -> b) -> b -> a -> b

foldLocalRegsUsed :: UserOfRegs LocalReg a
                  => Platform -> (b -> LocalReg -> b) -> b -> a -> b
foldLocalRegsUsed = foldRegsUsed

class Ord r => DefinerOfRegs r a where
  foldRegsDefd :: Platform -> (b -> r -> b) -> b -> a -> b

foldLocalRegsDefd :: DefinerOfRegs LocalReg a
                  => Platform -> (b -> LocalReg -> b) -> b -> a -> b
foldLocalRegsDefd = foldRegsDefd

instance UserOfRegs LocalReg CmmReg where
    foldRegsUsed _ f z (CmmLocal reg) = f z reg
    foldRegsUsed _ _ z (CmmGlobal _)  = z

instance DefinerOfRegs LocalReg CmmReg where
    foldRegsDefd _ f z (CmmLocal reg) = f z reg
    foldRegsDefd _ _ z (CmmGlobal _)  = z

instance UserOfRegs GlobalReg CmmReg where
    {-# INLINEABLE foldRegsUsed #-}
    foldRegsUsed _ _ z (CmmLocal _)    = z
    foldRegsUsed _ f z (CmmGlobal reg) = f z reg

instance DefinerOfRegs GlobalReg CmmReg where
    foldRegsDefd _ _ z (CmmLocal _)    = z
    foldRegsDefd _ f z (CmmGlobal reg) = f z reg

instance Ord r => UserOfRegs r r where
    foldRegsUsed _ f z r = f z r

instance Ord r => DefinerOfRegs r r where
    foldRegsDefd _ f z r = f z r

instance (Ord r, UserOfRegs r CmmReg) => UserOfRegs r CmmExpr where
  -- The (Ord r) in the context is necessary here
  -- See Note [Recursive superclasses] in GHC.Tc.TyCl.Instance
  {-# INLINEABLE foldRegsUsed #-}
  foldRegsUsed platform f !z e = expr z e
    where expr z (CmmLit _)          = z
          expr z (CmmLoad addr _)    = foldRegsUsed platform f z addr
          expr z (CmmReg r)          = foldRegsUsed platform f z r
          expr z (CmmMachOp _ exprs) = foldRegsUsed platform f z exprs
          expr z (CmmRegOff r _)     = foldRegsUsed platform f z r
          expr z (CmmStackSlot _ _)  = z

instance UserOfRegs r a => UserOfRegs r [a] where
  foldRegsUsed platform f set as = foldl' (foldRegsUsed platform f) set as
  {-# INLINABLE foldRegsUsed #-}

instance DefinerOfRegs r a => DefinerOfRegs r [a] where
  foldRegsDefd platform f set as = foldl' (foldRegsDefd platform f) set as
  {-# INLINABLE foldRegsDefd #-}

globalRegType :: Platform -> GlobalReg -> CmmType
globalRegType platform = \case
   (VanillaReg _ VGcPtr)    -> gcWord platform
   (VanillaReg _ VNonGcPtr) -> bWord platform
   (FloatReg _)             -> cmmFloat W32
   (DoubleReg _)            -> cmmFloat W64
   (LongReg _)              -> cmmBits W64
   -- TODO: improve the internal model of SIMD/vectorized registers
   -- the right design SHOULd improve handling of float and double code too.
   -- see remarks in "NOTE [SIMD Design for the future]"" in GHC.StgToCmm.Prim
   (XmmReg _) -> cmmVec 4 (cmmBits W32)
   (YmmReg _) -> cmmVec 8 (cmmBits W32)
   (ZmmReg _) -> cmmVec 16 (cmmBits W32)

   Hp         -> gcWord platform -- The initialiser for all
                                 -- dynamically allocated closures
   _          -> bWord platform

-- convenient aliases
baseReg, spReg, hpReg, spLimReg, hpLimReg, nodeReg,
  currentTSOReg, currentNurseryReg, hpAllocReg, cccsReg  :: CmmReg
baseReg = CmmGlobal BaseReg
spReg = CmmGlobal Sp
hpReg = CmmGlobal Hp
hpLimReg = CmmGlobal HpLim
spLimReg = CmmGlobal SpLim
nodeReg = CmmGlobal node
currentTSOReg = CmmGlobal CurrentTSO
currentNurseryReg = CmmGlobal CurrentNursery
hpAllocReg = CmmGlobal HpAlloc
cccsReg = CmmGlobal CCCS
