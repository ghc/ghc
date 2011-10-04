----------------------------------------------------------------------------
--
-- Pretty-printing of Cmm as (a superset of) C--
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------
--
-- This is where we walk over CmmNode emitting an external representation,
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

{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts #-}
module PprCmm
  ( module PprCmmDecl
  , module PprCmmExpr
  )
where

import BlockId ()
import CLabel
import Cmm
import CmmUtils
import FastString
import Outputable
import PprCmmDecl
import PprCmmExpr
import Util

import BasicTypes
import Platform
import Compiler.Hoopl
import Data.List
import Prelude hiding (succ)

-------------------------------------------------
-- Outputable instances

instance Outputable CmmStackInfo where
    ppr = pprStackInfo

instance PlatformOutputable CmmTopInfo where
    pprPlatform = pprTopInfo


instance PlatformOutputable (CmmNode e x) where
    pprPlatform = pprNode

instance Outputable Convention where
    ppr = pprConvention

instance Outputable ForeignConvention where
    ppr = pprForeignConvention

instance PlatformOutputable ForeignTarget where
    pprPlatform = pprForeignTarget


instance PlatformOutputable (Block CmmNode C C) where
    pprPlatform = pprBlock
instance PlatformOutputable (Block CmmNode C O) where
    pprPlatform = pprBlock
instance PlatformOutputable (Block CmmNode O C) where
    pprPlatform = pprBlock
instance PlatformOutputable (Block CmmNode O O) where
    pprPlatform = pprBlock

instance PlatformOutputable (Graph CmmNode e x) where
    pprPlatform = pprGraph

instance PlatformOutputable CmmGraph where
    pprPlatform platform = pprCmmGraph platform

----------------------------------------------------------
-- Outputting types Cmm contains

pprStackInfo :: CmmStackInfo -> SDoc
pprStackInfo (StackInfo {arg_space=arg_space, updfr_space=updfr_space}) =
  ptext (sLit "arg_space: ") <> ppr arg_space <+>
  ptext (sLit "updfr_space: ") <> ppr updfr_space

pprTopInfo :: Platform -> CmmTopInfo -> SDoc
pprTopInfo platform (TopInfo {info_tbl=info_tbl, stack_info=stack_info}) =
  vcat [ptext (sLit "info_tbl: ") <> pprPlatform platform info_tbl,
        ptext (sLit "stack_info: ") <> ppr stack_info]

----------------------------------------------------------
-- Outputting blocks and graphs

pprBlock :: IndexedCO x SDoc SDoc ~ SDoc
         => Platform -> Block CmmNode e x -> IndexedCO e SDoc SDoc
pprBlock platform block
    = foldBlockNodesB3 ( ($$) . pprPlatform platform
                       , ($$) . (nest 4) . pprPlatform platform
                       , ($$) . (nest 4) . pprPlatform platform
                       )
                       block
                       empty

pprGraph :: Platform -> Graph CmmNode e x -> SDoc
pprGraph _ GNil = empty
pprGraph platform (GUnit block) = pprPlatform platform block
pprGraph platform (GMany entry body exit)
   = text "{"
  $$ nest 2 (pprMaybeO entry $$ (vcat $ map (pprPlatform platform) $ bodyToBlockList body) $$ pprMaybeO exit)
  $$ text "}"
  where pprMaybeO :: PlatformOutputable (Block CmmNode e x)
                  => MaybeO ex (Block CmmNode e x) -> SDoc
        pprMaybeO NothingO = empty
        pprMaybeO (JustO block) = pprPlatform platform block

pprCmmGraph :: Platform -> CmmGraph -> SDoc
pprCmmGraph platform g
   = text "{" <> text "offset"
  $$ nest 2 (vcat $ map (pprPlatform platform) blocks)
  $$ text "}"
  where blocks = postorderDfs g

---------------------------------------------
-- Outputting CmmNode and types which it contains

pprConvention :: Convention -> SDoc
pprConvention (NativeNodeCall   {}) = text "<native-node-call-convention>"
pprConvention (NativeDirectCall {}) = text "<native-direct-call-convention>"
pprConvention (NativeReturn {})     = text "<native-ret-convention>"
pprConvention  Slow                 = text "<slow-convention>"
pprConvention  GC                   = text "<gc-convention>"
pprConvention  PrimOpCall           = text "<primop-call-convention>"
pprConvention  PrimOpReturn         = text "<primop-ret-convention>"
pprConvention (Foreign c)           = ppr c
pprConvention (Private {})          = text "<private-convention>"

pprForeignConvention :: ForeignConvention -> SDoc
pprForeignConvention (ForeignConvention c as rs) = ppr c <> ppr as <> ppr rs

pprForeignTarget :: Platform -> ForeignTarget -> SDoc
pprForeignTarget platform (ForeignTarget fn c) = ppr_fc c <+> ppr_target fn
  where ppr_fc :: ForeignConvention -> SDoc
        ppr_fc (ForeignConvention c args res) =
          doubleQuotes (ppr c) <+> text "arg hints: " <+> ppr args <+> text " result hints: " <+> ppr res
        ppr_target :: CmmExpr -> SDoc
        ppr_target t@(CmmLit _) = pprPlatform platform t
        ppr_target fn'          = parens (pprPlatform platform fn')

pprForeignTarget platform (PrimTarget op)
 -- HACK: We're just using a ForeignLabel to get this printed, the label
 --       might not really be foreign.
 = pprPlatform platform
               (CmmLabel (mkForeignLabel
                         (mkFastString (show op))
                         Nothing ForeignLabelInThisPackage IsFunction))

pprNode :: Platform -> CmmNode e x -> SDoc
pprNode platform node = pp_node <+> pp_debug
  where
    pp_node :: SDoc
    pp_node = case node of
      -- label:
      CmmEntry id -> ppr id <> colon

      -- // text
      CmmComment s -> text "//" <+> ftext s

      -- reg = expr;
      CmmAssign reg expr -> ppr reg <+> equals <+> pprPlatform platform expr <> semi

      -- rep[lv] = expr;
      CmmStore lv expr -> rep <> brackets(pprPlatform platform lv) <+> equals <+> pprPlatform platform expr <> semi
          where
            rep = ppr ( cmmExprType expr )

      -- call "ccall" foo(x, y)[r1, r2];
      -- ToDo ppr volatile
      CmmUnsafeForeignCall target results args ->
          hsep [ ppUnless (null results) $
                    parens (commafy $ map ppr results) <+> equals,
                 ptext $ sLit "call",
                 pprPlatform platform target <> parens (commafy $ map (pprPlatform platform) args) <> semi]

      -- goto label;
      CmmBranch ident -> ptext (sLit "goto") <+> ppr ident <> semi

      -- if (expr) goto t; else goto f;
      CmmCondBranch expr t f ->
          hsep [ ptext (sLit "if")
               , parens(pprPlatform platform expr)
               , ptext (sLit "goto")
               , ppr t <> semi
               , ptext (sLit "else goto")
               , ppr f <> semi
               ]

      CmmSwitch expr maybe_ids ->
          hang (hcat [ ptext (sLit "switch [0 .. ")
                     , int (length maybe_ids - 1)
                     , ptext (sLit "] ")
                     , if isTrivialCmmExpr expr
                       then pprPlatform platform expr
                       else parens (pprPlatform platform expr)
                     , ptext (sLit " {")
                     ])
             4 (vcat ( map caseify pairs )) $$ rbrace
          where pairs = groupBy snds (zip [0 .. ] maybe_ids )
                snds a b = (snd a) == (snd b)
                caseify ixs@((_,Nothing):_) = ptext (sLit "/* impossible: ")
                                              <> hcat (intersperse comma (map (int.fst) ixs)) <> ptext (sLit " */")
                caseify as = let (is,ids) = unzip as
                             in hsep [ ptext (sLit "case")
                                     , hcat (punctuate comma (map int is))
                                     , ptext (sLit ": goto")
                                     , ppr (head [ id | Just id <- ids]) <> semi ]

      CmmCall tgt k out res updfr_off ->
          hcat [ ptext (sLit "call"), space
               , pprFun tgt, ptext (sLit "(...)"), space
               , ptext (sLit "returns to") <+> ppr k <+> parens (ppr out)
                                                     <+> parens (ppr res)
               , ptext (sLit " with update frame") <+> ppr updfr_off
               , semi ]
          where pprFun f@(CmmLit _) = pprPlatform platform f
                pprFun f = parens (pprPlatform platform f)

      CmmForeignCall {tgt=t, res=rs, args=as, succ=s, updfr=u, intrbl=i} ->
          hcat $ if i then [ptext (sLit "interruptible"), space] else [] ++
               [ ptext (sLit "foreign call"), space
               , pprPlatform platform t, ptext (sLit "(...)"), space
               , ptext (sLit "returns to") <+> ppr s
                    <+> ptext (sLit "args:") <+> parens (pprPlatform platform as)
                    <+> ptext (sLit "ress:") <+> parens (ppr rs)
               , ptext (sLit " with update frame") <+> ppr u
               , semi ]

    pp_debug :: SDoc
    pp_debug =
      if not debugIsOn then empty
      else case node of
             CmmEntry {}             -> empty -- Looks terrible with text "  // CmmEntry"
             CmmComment {}           -> empty -- Looks also terrible with text "  // CmmComment"
             CmmAssign {}            -> text "  // CmmAssign"
             CmmStore {}             -> text "  // CmmStore"
             CmmUnsafeForeignCall {} -> text "  // CmmUnsafeForeignCall"
             CmmBranch {}            -> text "  // CmmBranch"
             CmmCondBranch {}        -> text "  // CmmCondBranch"
             CmmSwitch {}            -> text "  // CmmSwitch"
             CmmCall {}              -> text "  // CmmCall"
             CmmForeignCall {}       -> text "  // CmmForeignCall"

    commafy :: [SDoc] -> SDoc
    commafy xs = hsep $ punctuate comma xs
