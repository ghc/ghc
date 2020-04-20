----------------------------------------------------------------------------
--
-- Pretty-printing of common Cmm types
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

--
-- This is where we walk over Cmm emitting an external representation,
-- suitable for parsing, in a syntax strongly reminiscent of C--. This
-- is the "External Core" for the Cmm layer.
--
-- As such, this should be a well-defined syntax: we want it to look nice.
-- Thus, we try wherever possible to use syntax defined in [1],
-- "The C-- Reference Manual", http://www.cs.tufts.edu/~nr/c--/index.html. We
-- differ slightly, in some cases. For one, we use I8 .. I64 for types, rather
-- than C--'s bits8 .. bits64.
--
-- We try to ensure that all information available in the abstract
-- syntax is reproduced, or reproducible, in the concrete syntax.
-- Data that is not in printed out can be reconstructed according to
-- conventions used in the pretty printer. There are at least two such
-- cases:
--      1) if a value has wordRep type, the type is not appended in the
--      output.
--      2) MachOps that operate over wordRep type are printed in a
--      C-style, rather than as their internal MachRep name.
--
-- These conventions produce much more readable Cmm output.
--
-- A useful example pass over Cmm is in nativeGen/MachCodeGen.hs
--
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Cmm.Ppr.Expr
    ( pprExpr, pprLit
    )
where

import GHC.Prelude

import GHC.Platform
import GHC.Driver.Session (targetPlatform)
import GHC.Cmm.Expr

import GHC.Utils.Outputable

import Data.Maybe
import Numeric ( fromRat )

-----------------------------------------------------------------------------

instance Outputable CmmExpr where
    ppr e = sdocWithDynFlags $ \dflags ->
            pprExpr (targetPlatform dflags) e

instance Outputable CmmReg where
    ppr e = pprReg e

instance Outputable CmmLit where
    ppr l = sdocWithDynFlags $ \dflags ->
            pprLit (targetPlatform dflags) l

instance Outputable LocalReg where
    ppr e = pprLocalReg e

instance Outputable Area where
    ppr e = pprArea e

instance Outputable GlobalReg where
    ppr e = pprGlobalReg e

-- --------------------------------------------------------------------------
-- Expressions
--

pprExpr :: Platform -> CmmExpr -> SDoc
pprExpr platform e
    = case e of
        CmmRegOff reg i ->
                pprExpr platform (CmmMachOp (MO_Add rep)
                           [CmmReg reg, CmmLit (CmmInt (fromIntegral i) rep)])
                where rep = typeWidth (cmmRegType platform reg)
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
pprExpr1 platform (CmmMachOp op [x,y])
   | Just doc <- infixMachOp1 op
   = pprExpr7 platform x <+> doc <+> pprExpr7 platform y
pprExpr1 platform e = pprExpr7 platform e

infixMachOp1, infixMachOp7, infixMachOp8 :: MachOp -> Maybe SDoc

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
pprExpr7 platform (CmmMachOp (MO_Add rep1) [x, CmmLit (CmmInt i rep2)]) | i < 0
   = pprExpr7 platform (CmmMachOp (MO_Sub rep1) [x, CmmLit (CmmInt (negate i) rep2)])
pprExpr7 platform (CmmMachOp op [x,y])
   | Just doc <- infixMachOp7 op
   = pprExpr7 platform x <+> doc <+> pprExpr8 platform y
pprExpr7 platform e = pprExpr8 platform e

infixMachOp7 (MO_Add _)  = Just (char '+')
infixMachOp7 (MO_Sub _)  = Just (char '-')
infixMachOp7 _           = Nothing

-- %left '/' '*' '%'
pprExpr8 platform (CmmMachOp op [x,y])
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
        CmmLoad   expr rep  -> ppr rep <> brackets (ppr expr)
        CmmReg    reg       -> ppr reg
        CmmRegOff  reg off  -> parens (ppr reg <+> char '+' <+> int off)
        CmmStackSlot a off  -> parens (ppr a   <+> char '+' <+> int off)
        CmmMachOp mop args  -> genMachOp platform mop args

genMachOp :: Platform -> MachOp -> [CmmExpr] -> SDoc
genMachOp platform mop args
   | Just doc <- infixMachOp mop = case args of
        -- dyadic
        [x,y] -> pprExpr9 platform x <+> doc <+> pprExpr9 platform y

        -- unary
        [x]   -> doc <> pprExpr9 platform x

        _     -> pprTrace "GHC.Cmm.Ppr.Expr.genMachOp: machop with strange number of args"
                          (pprMachOp mop <+>
                            parens (hcat $ punctuate comma (map (pprExpr platform) args)))
                          empty

   | isJust (infixMachOp1 mop)
   || isJust (infixMachOp7 mop)
   || isJust (infixMachOp8 mop)  = parens (pprExpr platform (CmmMachOp mop args))

   | otherwise = char '%' <> ppr_op <> parens (commafy (map (pprExpr platform) args))
        where ppr_op = text (map (\c -> if c == ' ' then '_' else c)
                                 (show mop))
                -- replace spaces in (show mop) with underscores,

--
-- Unsigned ops on the word size of the machine get nice symbols.
-- All else get dumped in their ugly format.
--
infixMachOp :: MachOp -> Maybe SDoc
infixMachOp mop
        = case mop of
            MO_And    _ -> Just $ char '&'
            MO_Or     _ -> Just $ char '|'
            MO_Xor    _ -> Just $ char '^'
            MO_Not    _ -> Just $ char '~'
            MO_S_Neg  _ -> Just $ char '-' -- there is no unsigned neg :)
            _ -> Nothing

-- --------------------------------------------------------------------------
-- Literals.
--  To minimise line noise we adopt the convention that if the literal
--  has the natural machine word size, we do not append the type
--
pprLit :: Platform -> CmmLit -> SDoc
pprLit platform lit = case lit of
    CmmInt i rep ->
        hcat [ (if i < 0 then parens else id)(integer i)
             , ppUnless (rep == wordWidth platform) $
               space <> dcolon <+> ppr rep ]

    CmmFloat f rep     -> hsep [ double (fromRat f), dcolon, ppr rep ]
    CmmVec lits        -> char '<' <> commafy (map (pprLit platform) lits) <> char '>'
    CmmLabel clbl      -> ppr clbl
    CmmLabelOff clbl i -> ppr clbl <> ppr_offset i
    CmmLabelDiffOff clbl1 clbl2 i _ -> ppr clbl1 <> char '-'
                                  <> ppr clbl2 <> ppr_offset i
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

-- --------------------------------------------------------------------------
-- Registers, whether local (temps) or global
--
pprReg :: CmmReg -> SDoc
pprReg r
    = case r of
        CmmLocal  local  -> pprLocalReg  local
        CmmGlobal global -> pprGlobalReg global

--
-- We only print the type of the local reg if it isn't wordRep
--
pprLocalReg :: LocalReg -> SDoc
pprLocalReg (LocalReg uniq rep) =
--   = ppr rep <> char '_' <> ppr uniq
-- Temp Jan08
    char '_' <> pprUnique uniq <>
       (if isWord32 rep -- && not (isGcPtrType rep) -- Temp Jan08               -- sigh
                    then dcolon <> ptr <> ppr rep
                    else dcolon <> ptr <> ppr rep)
   where
     pprUnique unique = sdocOption sdocSuppressUniques $ \case
       True  -> text "_locVar_"
       False -> ppr unique
     ptr = empty
         --if isGcPtrType rep
         --      then doubleQuotes (text "ptr")
         --      else empty

-- Stack areas
pprArea :: Area -> SDoc
pprArea Old        = text "old"
pprArea (Young id) = hcat [ text "young<", ppr id, text ">" ]

-- needs to be kept in syn with CmmExpr.hs.GlobalReg
--
pprGlobalReg :: GlobalReg -> SDoc
pprGlobalReg gr
    = case gr of
        VanillaReg n _ -> char 'R' <> int n
-- Temp Jan08
--        VanillaReg n VNonGcPtr -> char 'R' <> int n
--        VanillaReg n VGcPtr    -> char 'P' <> int n
        FloatReg   n   -> char 'F' <> int n
        DoubleReg  n   -> char 'D' <> int n
        LongReg    n   -> char 'L' <> int n
        XmmReg     n   -> text "XMM" <> int n
        YmmReg     n   -> text "YMM" <> int n
        ZmmReg     n   -> text "ZMM" <> int n
        Sp             -> text "Sp"
        SpLim          -> text "SpLim"
        Hp             -> text "Hp"
        HpLim          -> text "HpLim"
        MachSp         -> text "MachSp"
        UnwindReturnReg-> text "UnwindReturnReg"
        CCCS           -> text "CCCS"
        CurrentTSO     -> text "CurrentTSO"
        CurrentNursery -> text "CurrentNursery"
        HpAlloc        -> text "HpAlloc"
        EagerBlackholeInfo -> text "stg_EAGER_BLACKHOLE_info"
        GCEnter1       -> text "stg_gc_enter_1"
        GCFun          -> text "stg_gc_fun"
        BaseReg        -> text "BaseReg"
        PicBaseReg     -> text "PicBaseReg"

-----------------------------------------------------------------------------

commafy :: [SDoc] -> SDoc
commafy xs = fsep $ punctuate comma xs
