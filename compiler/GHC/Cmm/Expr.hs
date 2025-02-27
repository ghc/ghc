{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.Cmm.Expr
    ( CmmExpr(..), cmmExprType, cmmExprWidth, cmmExprAlignment, maybeInvertCmmExpr
    , CmmReg(..), cmmRegType, cmmRegWidth
    , CmmLit(..), cmmLitType
    , AlignmentSpec(..)
      -- TODO: Remove:
    , LocalReg(..), localRegType
    , GlobalReg(..), isArgReg, globalRegSpillType
    , GlobalRegUse(..)
    , spReg, hpReg, spLimReg, hpLimReg, nodeReg
    , currentTSOReg, currentNurseryReg, hpAllocReg, cccsReg
    , node, baseReg

    , DefinerOfRegs, UserOfRegs
    , foldRegsDefd, foldRegsUsed
    , foldLocalRegsDefd, foldLocalRegsUsed

    , RegSet, LocalRegSet, GlobalRegSet
    , emptyRegSet, elemRegSet, extendRegSet, deleteFromRegSet, mkRegSet
    , plusRegSet, minusRegSet, timesRegSet, sizeRegSet, nullRegSet
    , regSetToList

    , isTrivialCmmExpr
    , hasNoGlobalRegs
    , isLit
    , isComparisonExpr

    , Area(..)
    , module GHC.Cmm.MachOp
    , module GHC.Cmm.Type
    )
where

import GHC.Prelude

import GHC.Platform
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.MachOp
import GHC.Cmm.Type
import GHC.Cmm.Reg
import GHC.Utils.Panic (panic)
import GHC.Utils.Outputable

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric ( fromRat )
import Data.Type.Equality
import qualified Data.Foldable as Foldable

import GHC.Types.Basic (Alignment, mkAlignment, alignmentOf)

-----------------------------------------------------------------------------
--              CmmExpr
-- An expression.  Expressions have no side effects.
-----------------------------------------------------------------------------

data CmmExpr
  = CmmLit !CmmLit              -- Literal
  | CmmLoad !CmmExpr !CmmType !AlignmentSpec
                                -- Read memory location
  | CmmReg !CmmReg              -- Contents of register
  | forall (arity :: Natural). CmmMachOp !(MachOp arity) !(SizedTupleGADT arity CmmExpr)
  -- ^ Machine operation (+, -, *, etc.)
  | CmmStackSlot Area {-# UNPACK #-} !Int
                                -- Addressing expression of a stack slot
                                -- See Note [CmmStackSlot aliasing]
  | CmmRegOff !CmmReg !Int
        -- CmmRegOff reg i
        --        ** is shorthand only, meaning **
        -- CmmMachOp (MO_Add rep) [x, CmmLit (CmmInt (fromIntegral i) rep)]
        --      where rep = typeWidth (cmmRegType reg)

deriving instance Show CmmExpr

instance Eq CmmExpr where       -- Equality ignores the types
  CmmLit l1          == CmmLit l2          = l1==l2
  CmmLoad e1 _ _     == CmmLoad e2 _ _     = e1==e2
  CmmReg r1          == CmmReg r2          = r1==r2
  CmmRegOff r1 i1    == CmmRegOff r2 i2    = r1==r2 && i1==i2
  CmmMachOp op1 es1  == CmmMachOp op2 es2  =
    case testEquality (sizeOfTupleGADT es1) (sizeOfTupleGADT es2) of
      Just Refl -> op1==op2 && es1==es2
      Nothing   -> False
  CmmStackSlot a1 i1 == CmmStackSlot a2 i2 = a1==a2 && i1==i2
  _e1                == _e2                = False

instance OutputableP Platform CmmExpr where
    pdoc = pprExpr

data AlignmentSpec = NaturallyAligned | Unaligned
  deriving (Eq, Ord, Show)

-- | A stack area is either the stack slot where a variable is spilled
-- or the stack space where function arguments and results are passed.
data Area
  = Old            -- See Note [Old Area]
  | Young {-# UNPACK #-} !BlockId  -- Invariant: must be a continuation BlockId
                   -- See Note [Continuation BlockIds] in GHC.Cmm.Node.
  deriving (Eq, Ord, Show)

instance Outputable Area where
    ppr e = pprArea e

pprArea :: Area -> SDoc
pprArea Old        = text "old"
pprArea (Young id) = hcat [ text "young<", ppr id, text ">" ]


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
        -- See Note [Continuation BlockIds] in GHC.Cmm.Node.

  | CmmHighStackMark -- A late-bound constant that stands for the max
                     -- #bytes of stack space used during a procedure.
                     -- During the stack-layout pass, CmmHighStackMark
                     -- is replaced by a CmmInt for the actual number
                     -- of bytes used
  deriving (Eq, Show)

instance OutputableP Platform CmmLit where
    pdoc = pprLit

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
   (CmmLoad _ rep _)   -> rep
   (CmmReg reg)        -> cmmRegType reg
   (CmmMachOp op args) -> machOpResultType platform op (fmap (cmmExprType platform) args)
   (CmmRegOff reg _)   -> cmmRegType reg
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

---------------------------------------------------
--         CmmExpr predicates
---------------------------------------------------

isTrivialCmmExpr :: CmmExpr -> Bool
isTrivialCmmExpr (CmmLoad _ _ _)    = False
isTrivialCmmExpr (CmmMachOp _ _)    = False
isTrivialCmmExpr (CmmLit _)         = True
isTrivialCmmExpr (CmmReg _)         = True
isTrivialCmmExpr (CmmRegOff _ _)    = True
isTrivialCmmExpr (CmmStackSlot _ _) = panic "isTrivialCmmExpr CmmStackSlot"

hasNoGlobalRegs :: CmmExpr -> Bool
hasNoGlobalRegs (CmmLoad e _ _)            = hasNoGlobalRegs e
hasNoGlobalRegs (CmmMachOp _ es)           = all hasNoGlobalRegs es
hasNoGlobalRegs (CmmLit _)                 = True
hasNoGlobalRegs (CmmReg (CmmLocal _))      = True
hasNoGlobalRegs (CmmRegOff (CmmLocal _) _) = True
hasNoGlobalRegs _                          = False

isLit :: CmmExpr -> Bool
isLit (CmmLit _) = True
isLit _          = False

isComparisonExpr :: CmmExpr -> Bool
isComparisonExpr (CmmMachOp op _) = isComparisonMachOp op
isComparisonExpr _                = False


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

emptyRegSet             :: RegSet r
nullRegSet              :: RegSet r -> Bool
elemRegSet              :: Ord r => r -> RegSet r -> Bool
extendRegSet            :: Ord r => RegSet r -> r -> RegSet r
deleteFromRegSet        :: Ord r => RegSet r -> r -> RegSet r
mkRegSet                :: Ord r => [r] -> RegSet r
minusRegSet, plusRegSet, timesRegSet :: Ord r => RegSet r -> RegSet r -> RegSet r
sizeRegSet              :: RegSet r -> Int
regSetToList            :: RegSet r -> [r]

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
    foldRegsUsed _ f z (CmmGlobal (GlobalRegUse reg _)) = f z reg

instance UserOfRegs GlobalRegUse CmmReg where
    {-# INLINEABLE foldRegsUsed #-}
    foldRegsUsed _ _ z (CmmLocal _)    = z
    foldRegsUsed _ f z (CmmGlobal reg) = f z reg
instance DefinerOfRegs GlobalReg CmmReg where
    foldRegsDefd _ _ z (CmmLocal _)    = z
    foldRegsDefd _ f z (CmmGlobal (GlobalRegUse reg _)) = f z reg

instance DefinerOfRegs GlobalRegUse CmmReg where
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
          expr z (CmmLoad addr _ _)  = foldRegsUsed platform f z addr
          expr z (CmmReg r)          = foldRegsUsed platform f z r
          expr z (CmmMachOp _ exprs) = foldRegsUsed platform f z exprs
          expr z (CmmRegOff r _)     = foldRegsUsed platform f z r
          expr z (CmmStackSlot _ _)  = z

instance UserOfRegs r a => UserOfRegs r [a] where
  foldRegsUsed platform f set as = foldl' (foldRegsUsed platform f) set as
  {-# INLINABLE foldRegsUsed #-}

instance UserOfRegs r a => UserOfRegs r (SizedTupleGADT sz a) where
  foldRegsUsed platform f set as = foldl' (foldRegsUsed platform f) set as
  {-# INLINABLE foldRegsUsed #-}

instance DefinerOfRegs r a => DefinerOfRegs r [a] where
  foldRegsDefd platform f set as = foldl' (foldRegsDefd platform f) set as
  {-# INLINABLE foldRegsDefd #-}

-- --------------------------------------------------------------------------
-- Pretty-printing expressions
-- --------------------------------------------------------------------------

pprExpr :: Platform -> CmmExpr -> SDoc
pprExpr platform e
    = case e of
        CmmRegOff reg i ->
                pprExpr platform (CmmMachOp (MO_Add rep) $
                           TupleG2 (CmmReg reg) (CmmLit (CmmInt (fromIntegral i) rep)))
                where rep = typeWidth (cmmRegType reg)
        CmmLit lit -> pprLit platform lit
        _other     -> pprExpr1 platform e

-- Here's the precedence table from GHC.Cmm.Parser:
-- %nonassoc '>=' '>' '<=' '<' '!=' '=='
-- %left '|'
-- %left '^'
-- %left '&'
-- %left '>>' '<<'
-- %left '-' '+'
-- %left '/' '*' '%'
-- %right '~'

-- We just cope with the common operators for now, the rest will get
-- a default conservative behaviour.

-- %nonassoc '>=' '>' '<=' '<' '!=' '=='
pprExpr1, pprExpr7, pprExpr8 :: Platform -> CmmExpr -> SDoc
pprExpr1 platform (CmmMachOp op (TupleG2 x y))
   | Just doc <- infixMachOp1 op
   = pprExpr7 platform x <+> doc <+> pprExpr7 platform y
pprExpr1 platform e = pprExpr7 platform e

infixMachOp1, infixMachOp7, infixMachOp8 :: MachOp a -> Maybe SDoc

infixMachOp1 (MO_Eq     _) = Just (text "==")
infixMachOp1 (MO_Ne     _) = Just (text "!=")
infixMachOp1 (MO_Shl    _) = Just (text "<<")
infixMachOp1 (MO_U_Shr  _) = Just (text ">>")
infixMachOp1 (MO_U_Ge   _) = Just (text ">=")
infixMachOp1 (MO_U_Le   _) = Just (text "<=")
infixMachOp1 (MO_U_Gt   _) = Just (char '>')
infixMachOp1 (MO_U_Lt   _) = Just (char '<')
infixMachOp1 _             = Nothing

-- %left '-' '+'
pprExpr7 platform (CmmMachOp (MO_Add rep1) (TupleG2 x (CmmLit (CmmInt i rep2)))) | i < 0
   = pprExpr7 platform (CmmMachOp (MO_Sub rep1) (TupleG2 x (CmmLit (CmmInt (negate i) rep2))))
pprExpr7 platform (CmmMachOp op (TupleG2 x y))
   | Just doc <- infixMachOp7 op
   = pprExpr7 platform x <+> doc <+> pprExpr8 platform y
pprExpr7 platform e = pprExpr8 platform e

infixMachOp7 (MO_Add _)  = Just (char '+')
infixMachOp7 (MO_Sub _)  = Just (char '-')
infixMachOp7 _           = Nothing

-- %left '/' '*' '%'
pprExpr8 platform (CmmMachOp op (TupleG2 x y))
   | Just doc <- infixMachOp8 op
   = pprExpr8 platform x <+> doc <+> pprExpr9 platform y
pprExpr8 platform e = pprExpr9 platform e

infixMachOp8 (MO_U_Quot _) = Just (char '/')
infixMachOp8 (MO_Mul _)    = Just (char '*')
infixMachOp8 (MO_U_Rem _)  = Just (char '%')
infixMachOp8 _             = Nothing

pprExpr9 :: Platform -> CmmExpr -> SDoc
pprExpr9 platform e =
   case e of
        CmmLit    lit       -> pprLit1 platform lit
        CmmLoad   expr rep align
                            -> let align_mark =
                                       case align of
                                         NaturallyAligned -> empty
                                         Unaligned        -> text "^"
                                in ppr rep <> align_mark <> brackets (pdoc platform expr)
        CmmReg    reg       -> ppr reg
        CmmRegOff  reg off  -> parens (ppr reg <+> char '+' <+> int off)
        CmmStackSlot a off  -> parens (ppr a   <+> char '+' <+> int off)
        CmmMachOp mop args  -> genMachOp platform mop args

genMachOp :: Platform -> MachOp a -> SizedTupleGADT a CmmExpr -> SDoc
genMachOp platform (MO_RelaxedRead w) (TupleG1 x) =
    ppr (cmmBits w) <> text "!" <> brackets (pdoc platform x)
genMachOp platform mop args
   | Just (c, eq) <- infixMachOp mop = case (eq, args) of
        -- dyadic
        (Left Refl, TupleG2 x y) -> pprExpr9 platform x <+> char c <+> pprExpr9 platform y

        -- unary
        (Right Refl, TupleG1 x)  -> char c <> pprExpr9 platform x

   | isJust (infixMachOp1 mop)
   || isJust (infixMachOp7 mop)
   || isJust (infixMachOp8 mop)  = parens (pprExpr platform (CmmMachOp mop args))

   | otherwise = char '%' <> ppr_op <> parens (commafy (printed_args_list))
   where ppr_op = text (map (\c -> if c == ' ' then '_' else c)
                            (show mop))
           -- replace spaces in (show mop) with underscores,
         printed_args_list = map (pprExpr platform) (Foldable.toList args)

--
-- Unsigned ops on the word size of the machine get nice symbols.
-- All else get dumped in their ugly format.
--
infixMachOp :: MachOp a -> Maybe (Char, Either (a :~: 2) (a :~: 1))
infixMachOp mop
        = case mop of
            MO_And    _ -> Just ('&', Left Refl)
            MO_Or     _ -> Just ('|', Left Refl)
            MO_Xor    _ -> Just ('^', Left Refl)
            MO_Not    _ -> Just ('~', Right Refl)
            MO_S_Neg  _ -> Just ('-', Right Refl) -- there is no unsigned neg :)
            _ -> Nothing

-- --------------------------------------------------------------------------
-- Pretty-printing literals
--
--  To minimise line noise we adopt the convention that if the literal
--  has the natural machine word size, we do not append the type
-- --------------------------------------------------------------------------

pprLit :: Platform -> CmmLit -> SDoc
pprLit platform lit = case lit of
    CmmInt i rep ->
        hcat [ (if i < 0 then parens else id)(integer i)
             , ppUnless (rep == wordWidth platform) $
               space <> dcolon <+> ppr rep ]

    CmmFloat f rep     -> hsep [ double (fromRat f), dcolon, ppr rep ]
    CmmVec lits        -> char '<' <> commafy (map (pprLit platform) lits) <> char '>'
    CmmLabel clbl      -> pdoc platform clbl
    CmmLabelOff clbl i -> pdoc platform clbl <> ppr_offset i
    CmmLabelDiffOff clbl1 clbl2 i _ -> pdoc platform clbl1 <> char '-'
                                       <> pdoc platform clbl2 <> ppr_offset i
    CmmBlock id        -> ppr id
    CmmHighStackMark -> text "<highSp>"

pprLit1 :: Platform -> CmmLit -> SDoc
pprLit1 platform lit@(CmmLabelOff {}) = parens (pprLit platform lit)
pprLit1 platform lit                  = pprLit platform lit

ppr_offset :: Int -> SDoc
ppr_offset i
    | i==0      = empty
    | i>=0      = char '+' <> int i
    | otherwise = char '-' <> int (-i)

commafy :: [SDoc] -> SDoc
commafy xs = fsep $ punctuate comma xs
