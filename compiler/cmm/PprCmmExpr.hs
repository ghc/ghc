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
-- "The C-- Reference Manual", http://www.cminusminus.org/. We differ
-- slightly, in some cases. For one, we use I8 .. I64 for types, rather
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

{-# OPTIONS_GHC -fno-warn-orphans #-}
module PprCmmExpr
    ( pprExpr, pprLit
    )
where

import CmmExpr

import Outputable
import FastString

import Data.Maybe
import Numeric ( fromRat )

-----------------------------------------------------------------------------

instance Outputable CmmExpr where
    ppr e = pprExpr e

instance Outputable CmmReg where
    ppr e = pprReg e

instance Outputable CmmLit where
    ppr l = pprLit l

instance Outputable LocalReg where
    ppr e = pprLocalReg e

instance Outputable Area where
    ppr e = pprArea e

instance Outputable GlobalReg where
    ppr e = pprGlobalReg e

-- --------------------------------------------------------------------------
-- Expressions
--

pprExpr :: CmmExpr -> SDoc
pprExpr e
    = sdocWithDynFlags $ \dflags ->
      case e of
        CmmRegOff reg i ->
                pprExpr (CmmMachOp (MO_Add rep)
                           [CmmReg reg, CmmLit (CmmInt (fromIntegral i) rep)])
                where rep = typeWidth (cmmRegType dflags reg)
        CmmLit lit -> pprLit lit
        _other     -> pprExpr1 e

-- Here's the precedence table from CmmParse.y:
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
pprExpr1, pprExpr7, pprExpr8 :: CmmExpr -> SDoc
pprExpr1 (CmmMachOp op [x,y]) | Just doc <- infixMachOp1 op
   = pprExpr7 x <+> doc <+> pprExpr7 y
pprExpr1 e = pprExpr7 e

infixMachOp1, infixMachOp7, infixMachOp8 :: MachOp -> Maybe SDoc

infixMachOp1 (MO_Eq     _) = Just (ptext (sLit "=="))
infixMachOp1 (MO_Ne     _) = Just (ptext (sLit "!="))
infixMachOp1 (MO_Shl    _) = Just (ptext (sLit "<<"))
infixMachOp1 (MO_U_Shr  _) = Just (ptext (sLit ">>"))
infixMachOp1 (MO_U_Ge   _) = Just (ptext (sLit ">="))
infixMachOp1 (MO_U_Le   _) = Just (ptext (sLit "<="))
infixMachOp1 (MO_U_Gt   _) = Just (char '>')
infixMachOp1 (MO_U_Lt   _) = Just (char '<')
infixMachOp1 _             = Nothing

-- %left '-' '+'
pprExpr7 (CmmMachOp (MO_Add rep1) [x, CmmLit (CmmInt i rep2)]) | i < 0
   = pprExpr7 (CmmMachOp (MO_Sub rep1) [x, CmmLit (CmmInt (negate i) rep2)])
pprExpr7 (CmmMachOp op [x,y]) | Just doc <- infixMachOp7 op
   = pprExpr7 x <+> doc <+> pprExpr8 y
pprExpr7 e = pprExpr8 e

infixMachOp7 (MO_Add _)  = Just (char '+')
infixMachOp7 (MO_Sub _)  = Just (char '-')
infixMachOp7 _           = Nothing

-- %left '/' '*' '%'
pprExpr8 (CmmMachOp op [x,y]) | Just doc <- infixMachOp8 op
   = pprExpr8 x <+> doc <+> pprExpr9 y
pprExpr8 e = pprExpr9 e

infixMachOp8 (MO_U_Quot _) = Just (char '/')
infixMachOp8 (MO_Mul _)    = Just (char '*')
infixMachOp8 (MO_U_Rem _)  = Just (char '%')
infixMachOp8 _             = Nothing

pprExpr9 :: CmmExpr -> SDoc
pprExpr9 e =
   case e of
        CmmLit    lit       -> pprLit1 lit
        CmmLoad   expr rep  -> ppr rep <> brackets (ppr expr)
        CmmReg    reg       -> ppr reg
        CmmRegOff  reg off  -> parens (ppr reg <+> char '+' <+> int off)
        CmmStackSlot a off  -> parens (ppr a   <+> char '+' <+> int off)
        CmmMachOp mop args  -> genMachOp mop args

genMachOp :: MachOp -> [CmmExpr] -> SDoc
genMachOp mop args
   | Just doc <- infixMachOp mop = case args of
        -- dyadic
        [x,y] -> pprExpr9 x <+> doc <+> pprExpr9 y

        -- unary
        [x]   -> doc <> pprExpr9 x

        _     -> pprTrace "PprCmm.genMachOp: machop with strange number of args"
                          (pprMachOp mop <+>
                            parens (hcat $ punctuate comma (map pprExpr args)))
                          empty

   | isJust (infixMachOp1 mop)
   || isJust (infixMachOp7 mop)
   || isJust (infixMachOp8 mop)  = parens (pprExpr (CmmMachOp mop args))

   | otherwise = char '%' <> ppr_op <> parens (commafy (map pprExpr args))
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
pprLit :: CmmLit -> SDoc
pprLit lit = sdocWithDynFlags $ \dflags ->
             case lit of
    CmmInt i rep ->
        hcat [ (if i < 0 then parens else id)(integer i)
             , ppUnless (rep == wordWidth dflags) $
               space <> dcolon <+> ppr rep ]

    CmmFloat f rep     -> hsep [ double (fromRat f), dcolon, ppr rep ]
    CmmVec lits        -> char '<' <> commafy (map pprLit lits) <> char '>'
    CmmLabel clbl      -> ppr clbl
    CmmLabelOff clbl i -> ppr clbl <> ppr_offset i
    CmmLabelDiffOff clbl1 clbl2 i -> ppr clbl1 <> char '-'
                                  <> ppr clbl2 <> ppr_offset i
    CmmBlock id        -> ppr id
    CmmHighStackMark -> text "<highSp>"

pprLit1 :: CmmLit -> SDoc
pprLit1 lit@(CmmLabelOff {}) = parens (pprLit lit)
pprLit1 lit                  = pprLit lit

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
pprLocalReg (LocalReg uniq rep)
--   = ppr rep <> char '_' <> ppr uniq
-- Temp Jan08
   = char '_' <> ppr uniq <>
       (if isWord32 rep -- && not (isGcPtrType rep) -- Temp Jan08               -- sigh
                    then dcolon <> ptr <> ppr rep
                    else dcolon <> ptr <> ppr rep)
   where
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
        XmmReg     n   -> ptext (sLit "XMM") <> int n
        Sp             -> ptext (sLit "Sp")
        SpLim          -> ptext (sLit "SpLim")
        Hp             -> ptext (sLit "Hp")
        HpLim          -> ptext (sLit "HpLim")
        CCCS           -> ptext (sLit "CCCS")
        CurrentTSO     -> ptext (sLit "CurrentTSO")
        CurrentNursery -> ptext (sLit "CurrentNursery")
        HpAlloc        -> ptext (sLit "HpAlloc")
        EagerBlackholeInfo -> ptext (sLit "stg_EAGER_BLACKHOLE_info")
        GCEnter1       -> ptext (sLit "stg_gc_enter_1")
        GCFun          -> ptext (sLit "stg_gc_fun")
        BaseReg        -> ptext (sLit "BaseReg")
        PicBaseReg     -> ptext (sLit "PicBaseReg")

-----------------------------------------------------------------------------

commafy :: [SDoc] -> SDoc
commafy xs = fsep $ punctuate comma xs
