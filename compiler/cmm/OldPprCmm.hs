----------------------------------------------------------------------------
--
-- Pretty-printing of old-style Cmm as (a superset of) C--
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

module OldPprCmm (
        pprStmt,
        module PprCmmDecl,
        module PprCmmExpr
    ) where

import BlockId
import CLabel
import CmmUtils
import OldCmm
import PprCmmDecl
import PprCmmExpr

import BasicTypes
import ForeignCall
import Outputable
import FastString

import Data.List

-----------------------------------------------------------------------------

instance Outputable instr => Outputable (ListGraph instr) where
    ppr (ListGraph blocks) = vcat (map ppr blocks)

instance Outputable instr => Outputable (GenBasicBlock instr) where
    ppr = pprBBlock

instance Outputable CmmStmt where
    ppr s = pprStmt s

-- --------------------------------------------------------------------------
instance Outputable CmmSafety where
  ppr CmmUnsafe = ptext (sLit "_unsafe_call_")
  ppr CmmInterruptible = ptext (sLit "_interruptible_call_")
  ppr (CmmSafe srt) = ppr srt

-- --------------------------------------------------------------------------
-- Basic blocks look like assembly blocks.
--      lbl: stmt ; stmt ; ..
pprBBlock :: Outputable stmt => GenBasicBlock stmt -> SDoc
pprBBlock (BasicBlock ident stmts) =
    hang (ppr ident <> colon) 4 (vcat (map ppr stmts))

-- --------------------------------------------------------------------------
-- Statements. C-- usually, exceptions to this should be obvious.
--
pprStmt :: CmmStmt -> SDoc
pprStmt stmt = case stmt of

    -- ;
    CmmNop -> semi

    -- // text
    CmmComment s -> text "//" <+> ftext s

    -- reg = expr;
    CmmAssign reg expr -> ppr reg <+> equals <+> ppr expr <> semi

    -- rep[lv] = expr;
    CmmStore lv expr -> rep <> brackets(ppr lv) <+> equals <+> ppr expr <> semi
        where
          rep = ppr ( cmmExprType expr )

    -- call "ccall" foo(x, y)[r1, r2];
    -- ToDo ppr volatile
    CmmCall (CmmCallee fn cconv) results args ret ->
        sep  [ pp_lhs <+> pp_conv
             , nest 2 (pprExpr9 fn <>
                       parens (commafy (map ppr_ar args)))
             , case ret of CmmMayReturn -> empty
                           CmmNeverReturns -> ptext $ sLit (" never returns")
             ] <> semi
        where
          pp_lhs | null results = empty
                 | otherwise    = commafy (map ppr_ar results) <+> equals
                -- Don't print the hints on a native C-- call
          ppr_ar (CmmHinted ar k) = case cconv of
                            CmmCallConv -> ppr ar
                            _           -> ppr (ar,k)
          pp_conv = case cconv of
                      CmmCallConv -> empty
                      _           -> ptext (sLit("foreign")) <+> doubleQuotes (ppr cconv)

    -- Call a CallishMachOp, like sin or cos that might be implemented as a library call.
    CmmCall (CmmPrim op _) results args ret ->
        pprStmt (CmmCall (CmmCallee (CmmLit lbl) CCallConv) results args ret)
        where
          -- HACK: A CallishMachOp doesn't really correspond to a ForeignLabel, but we
          --       use one to get the label printed.
          lbl = CmmLabel (mkForeignLabel
                                (mkFastString (show op))
                                Nothing ForeignLabelInThisPackage IsFunction)

    CmmBranch ident          -> genBranch ident
    CmmCondBranch expr ident -> genCondBranch expr ident
    CmmJump expr live        -> genJump expr live
    CmmReturn                -> genReturn
    CmmSwitch arg ids        -> genSwitch arg ids

-- Just look like a tuple, since it was a tuple before
-- ... is that a good idea? --Isaac Dupree
instance (Outputable a) => Outputable (CmmHinted a) where
  ppr (CmmHinted a k) = ppr (a, k)

-- --------------------------------------------------------------------------
-- goto local label. [1], section 6.6
--
--     goto lbl;
--
genBranch :: BlockId -> SDoc
genBranch ident =
    ptext (sLit "goto") <+> ppr ident <> semi

-- --------------------------------------------------------------------------
-- Conditional. [1], section 6.4
--
--     if (expr) { goto lbl; }
--
genCondBranch :: CmmExpr -> BlockId -> SDoc
genCondBranch expr ident =
    hsep [ ptext (sLit "if")
         , parens (ppr expr)
         , ptext (sLit "goto")
         , ppr ident <> semi ]

-- --------------------------------------------------------------------------
-- A tail call. [1], Section 6.9
--
--     jump foo(a, b, c);
--
genJump :: CmmExpr -> Maybe [GlobalReg] -> SDoc
genJump expr live =
    hcat [ ptext (sLit "jump")
         , space
         , if isTrivialCmmExpr expr
                then pprExpr expr
                else case expr of
                    CmmLoad (CmmReg _) _ -> pprExpr expr
                    _                    -> parens (pprExpr expr)
         , semi <+> ptext (sLit "// ")
         , maybe empty ppr live]

-- --------------------------------------------------------------------------
-- Return from a function. [1], Section 6.8.2 of version 1.128
--
--     return (a, b, c);
--
genReturn :: SDoc
genReturn = hcat [ ptext (sLit "return") , semi ]

-- --------------------------------------------------------------------------
-- Tabled jump to local label
--
-- The syntax is from [1], section 6.5
--
--      switch [0 .. n] (expr) { case ... ; }
--
genSwitch :: CmmExpr -> [Maybe BlockId] -> SDoc
genSwitch expr maybe_ids

    = let pairs = groupBy snds (zip [0 .. ] maybe_ids )

      in hang (hcat [ ptext (sLit "switch [0 .. ")
                    , int (length maybe_ids - 1)
                    , ptext (sLit "] ")
                    , if isTrivialCmmExpr expr
                        then pprExpr expr
                        else parens (pprExpr expr)
                    , ptext (sLit " {")
                    ])
            4 (vcat ( map caseify pairs )) $$ rbrace

    where
      snds a b = (snd a) == (snd b)

      caseify :: [(Int,Maybe BlockId)] -> SDoc
      caseify ixs@((_,Nothing):_)
        = ptext (sLit "/* impossible: ") <> hcat (intersperse comma (map (int.fst) ixs))
                <> ptext (sLit " */")
      caseify as
        = let (is,ids) = unzip as
          in hsep [ ptext (sLit "case")
                  , hcat (punctuate comma (map int is))
                  , ptext (sLit ": goto")
                  , ppr (head [ id | Just id <- ids]) <> semi ]

-----------------------------------------------------------------------------

commafy :: [SDoc] -> SDoc
commafy xs = fsep $ punctuate comma xs

