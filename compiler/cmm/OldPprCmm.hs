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

module OldPprCmm
    ( pprStmt
    , module PprCmmDecl
    , module PprCmmExpr
    )
where

import BlockId
import CLabel
import CmmUtils
import OldCmm
import PprCmmDecl
import PprCmmExpr


import BasicTypes
import ForeignCall
import Outputable
import Platform
import FastString

import Data.List

-----------------------------------------------------------------------------

instance PlatformOutputable instr => PlatformOutputable (ListGraph instr) where
    pprPlatform platform (ListGraph blocks) = vcat (map (pprPlatform platform) blocks)

instance PlatformOutputable instr => PlatformOutputable (GenBasicBlock instr) where
    pprPlatform platform b = pprBBlock platform b

instance PlatformOutputable CmmStmt where
    pprPlatform = pprStmt

instance PlatformOutputable CmmInfo where
    pprPlatform = pprInfo


-- --------------------------------------------------------------------------
instance PlatformOutputable CmmSafety where
  pprPlatform _ CmmUnsafe = ptext (sLit "_unsafe_call_")
  pprPlatform _ CmmInterruptible = ptext (sLit "_interruptible_call_")
  pprPlatform platform (CmmSafe srt) = pprPlatform platform srt

-- --------------------------------------------------------------------------
-- Info tables. The current pretty printer needs refinement
-- but will work for now.
--
-- For ideas on how to refine it, they used to be printed in the
-- style of C--'s 'stackdata' declaration, just inside the proc body,
-- and were labelled with the procedure name ++ "_info".
pprInfo :: Platform -> CmmInfo -> SDoc
pprInfo platform (CmmInfo _gc_target update_frame info_table) =
    vcat [{-ptext (sLit "gc_target: ") <>
                maybe (ptext (sLit "<none>")) ppr gc_target,-}
          ptext (sLit "update_frame: ") <>
                maybe (ptext (sLit "<none>"))
                      (pprUpdateFrame platform)
                      update_frame,
          pprPlatform platform info_table]

-- --------------------------------------------------------------------------
-- Basic blocks look like assembly blocks.
--      lbl: stmt ; stmt ; ..
pprBBlock :: PlatformOutputable stmt => Platform -> GenBasicBlock stmt -> SDoc
pprBBlock platform (BasicBlock ident stmts) =
    hang (ppr ident <> colon) 4 (vcat (map (pprPlatform platform) stmts))

-- --------------------------------------------------------------------------
-- Statements. C-- usually, exceptions to this should be obvious.
--
pprStmt :: Platform -> CmmStmt -> SDoc
pprStmt platform stmt = case stmt of

    -- ;
    CmmNop -> semi

    --  // text
    CmmComment s -> text "//" <+> ftext s

    -- reg = expr;
    CmmAssign reg expr -> ppr reg <+> equals <+> pprPlatform platform expr <> semi

    -- rep[lv] = expr;
    CmmStore lv expr -> rep <> brackets(pprPlatform platform lv) <+> equals <+> pprPlatform platform expr <> semi
        where
          rep = ppr ( cmmExprType expr )

    -- call "ccall" foo(x, y)[r1, r2];
    -- ToDo ppr volatile
    CmmCall (CmmCallee fn cconv) results args ret ->
        sep  [ pp_lhs <+> pp_conv
             , nest 2 (pprExpr9 platform fn <>
                       parens (commafy (map ppr_ar args)))
             , case ret of CmmMayReturn -> empty
                           CmmNeverReturns -> ptext $ sLit (" never returns")
             ] <> semi
        where
          pp_lhs | null results = empty
                 | otherwise    = commafy (map ppr_ar results) <+> equals
                -- Don't print the hints on a native C-- call
          ppr_ar (CmmHinted ar k) = case cconv of
                            CmmCallConv -> pprPlatform platform ar
                            _           -> pprPlatform platform (ar,k)
          pp_conv = case cconv of
                      CmmCallConv -> empty
                      _           -> ptext (sLit("foreign")) <+> doubleQuotes (ppr cconv)

    -- Call a CallishMachOp, like sin or cos that might be implemented as a library call.
    CmmCall (CmmPrim op) results args ret ->
        pprStmt platform (CmmCall (CmmCallee (CmmLit lbl) CCallConv)
                                  results args ret)
        where
          -- HACK: A CallishMachOp doesn't really correspond to a ForeignLabel, but we
          --       use one to get the label printed.
          lbl = CmmLabel (mkForeignLabel
                                (mkFastString (show op))
                                Nothing ForeignLabelInThisPackage IsFunction)

    CmmBranch ident          -> genBranch ident
    CmmCondBranch expr ident -> genCondBranch platform expr ident
    CmmJump expr params      -> genJump platform expr params
    CmmReturn params         -> genReturn platform params
    CmmSwitch arg ids        -> genSwitch platform arg ids

-- Just look like a tuple, since it was a tuple before
-- ... is that a good idea? --Isaac Dupree
instance (Outputable a) => Outputable (CmmHinted a) where
  ppr (CmmHinted a k) = ppr (a, k)
instance (PlatformOutputable a) => PlatformOutputable (CmmHinted a) where
  pprPlatform platform (CmmHinted a k) = pprPlatform platform (a, k)

pprUpdateFrame :: Platform -> UpdateFrame -> SDoc
pprUpdateFrame platform (UpdateFrame expr args) =
    hcat [ ptext (sLit "jump")
         , space
         , if isTrivialCmmExpr expr
                then pprExpr platform expr
                else case expr of
                    CmmLoad (CmmReg _) _ -> pprExpr platform expr
                    _ -> parens (pprExpr platform expr)
         , space
         , parens  ( commafy $ map (pprPlatform platform) args ) ]


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
genCondBranch :: Platform -> CmmExpr -> BlockId -> SDoc
genCondBranch platform expr ident =
    hsep [ ptext (sLit "if")
         , parens(pprPlatform platform expr)
         , ptext (sLit "goto")
         , ppr ident <> semi ]

-- --------------------------------------------------------------------------
-- A tail call. [1], Section 6.9
--
--     jump foo(a, b, c);
--
genJump :: Platform -> CmmExpr -> [CmmHinted CmmExpr] -> SDoc
genJump platform expr args =
    hcat [ ptext (sLit "jump")
         , space
         , if isTrivialCmmExpr expr
                then pprExpr platform expr
                else case expr of
                    CmmLoad (CmmReg _) _ -> pprExpr platform expr
                    _ -> parens (pprExpr platform expr)
         , space
         , parens  ( commafy $ map (pprPlatform platform) args )
         , semi ]


-- --------------------------------------------------------------------------
-- Return from a function. [1], Section 6.8.2 of version 1.128
--
--     return (a, b, c);
--
genReturn :: Platform -> [CmmHinted CmmExpr] -> SDoc
genReturn platform args =
    hcat [ ptext (sLit "return")
         , space
         , parens  ( commafy $ map (pprPlatform platform) args )
         , semi ]

-- --------------------------------------------------------------------------
-- Tabled jump to local label
--
-- The syntax is from [1], section 6.5
--
--      switch [0 .. n] (expr) { case ... ; }
--
genSwitch :: Platform -> CmmExpr -> [Maybe BlockId] -> SDoc
genSwitch platform expr maybe_ids

    = let pairs = groupBy snds (zip [0 .. ] maybe_ids )

      in hang (hcat [ ptext (sLit "switch [0 .. ")
                    , int (length maybe_ids - 1)
                    , ptext (sLit "] ")
                    , if isTrivialCmmExpr expr
                        then pprExpr platform expr
                        else parens (pprExpr platform expr)
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
