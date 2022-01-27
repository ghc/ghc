{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module GHC.Cmm.Ppr
  ( module GHC.Cmm.Ppr.Decl
  , module GHC.Cmm.Ppr.Expr
  )
where

import GHC.Prelude hiding (succ)

import GHC.Platform
import GHC.Cmm.CLabel
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Switch
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Cmm.Ppr.Decl
import GHC.Cmm.Ppr.Expr
import GHC.Utils.Misc

import GHC.Types.Basic
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph

-------------------------------------------------
-- Outputable instances

instance Outputable CmmStackInfo where
    ppr = pprStackInfo

instance OutputableP Platform CmmTopInfo where
    pdoc = pprTopInfo


instance OutputableP Platform (CmmNode e x) where
    pdoc = pprNode

instance Outputable Convention where
    ppr = pprConvention

instance Outputable ForeignConvention where
    ppr = pprForeignConvention

instance OutputableP Platform ForeignTarget where
    pdoc = pprForeignTarget

instance Outputable CmmReturnInfo where
    ppr = pprReturnInfo

instance OutputableP Platform (Block CmmNode C C) where
    pdoc = pprBlock
instance OutputableP Platform (Block CmmNode C O) where
    pdoc = pprBlock
instance OutputableP Platform (Block CmmNode O C) where
    pdoc = pprBlock
instance OutputableP Platform (Block CmmNode O O) where
    pdoc = pprBlock

instance OutputableP Platform (Graph CmmNode e x) where
    pdoc = pprGraph

instance OutputableP Platform CmmGraph where
    pdoc = pprCmmGraph

----------------------------------------------------------
-- Outputting types Cmm contains

pprStackInfo :: CmmStackInfo -> SDoc
pprStackInfo (StackInfo {arg_space=arg_space}) =
  text "arg_space: " <> ppr arg_space

pprTopInfo :: Platform -> CmmTopInfo -> SDoc
pprTopInfo platform (TopInfo {info_tbls=info_tbl, stack_info=stack_info}) =
  vcat [text "info_tbls: " <> pdoc platform info_tbl,
        text "stack_info: " <> ppr stack_info]

----------------------------------------------------------
-- Outputting blocks and graphs

pprBlock :: IndexedCO x SDoc SDoc ~ SDoc
         => Platform -> Block CmmNode e x -> IndexedCO e SDoc SDoc
pprBlock platform block
    = foldBlockNodesB3 ( ($$) . pdoc platform
                       , ($$) . (nest 4) . pdoc platform
                       , ($$) . (nest 4) . pdoc platform
                       )
                       block
                       empty

pprGraph :: Platform -> Graph CmmNode e x -> SDoc
pprGraph platform = \case
   GNil                  -> empty
   GUnit block           -> pdoc platform block
   GMany entry body exit ->
         text "{"
      $$ nest 2 (pprMaybeO entry $$ (vcat $ map (pdoc platform) $ bodyToBlockList body) $$ pprMaybeO exit)
      $$ text "}"
      where pprMaybeO :: OutputableP Platform (Block CmmNode e x)
                      => MaybeO ex (Block CmmNode e x) -> SDoc
            pprMaybeO NothingO = empty
            pprMaybeO (JustO block) = pdoc platform block

pprCmmGraph :: Platform -> CmmGraph -> SDoc
pprCmmGraph platform g
   = text "{" <> text "offset"
  $$ nest 2 (vcat $ map (pdoc platform) blocks)
  $$ text "}"
  where blocks = revPostorder g
    -- revPostorder has the side-effect of discarding unreachable code,
    -- so pretty-printed Cmm will omit any unreachable blocks.  This can
    -- sometimes be confusing.

---------------------------------------------
-- Outputting CmmNode and types which it contains

pprConvention :: Convention -> SDoc
pprConvention (NativeNodeCall   {}) = text "<native-node-call-convention>"
pprConvention (NativeDirectCall {}) = text "<native-direct-call-convention>"
pprConvention (NativeReturn {})     = text "<native-ret-convention>"
pprConvention  Slow                 = text "<slow-convention>"
pprConvention  GC                   = text "<gc-convention>"

pprForeignConvention :: ForeignConvention -> SDoc
pprForeignConvention (ForeignConvention c args res ret) =
          doubleQuotes (ppr c) <+> text "arg hints: " <+> ppr args <+> text " result hints: " <+> ppr res <+> ppr ret

pprReturnInfo :: CmmReturnInfo -> SDoc
pprReturnInfo CmmMayReturn = empty
pprReturnInfo CmmNeverReturns = text "never returns"

pprForeignTarget :: Platform -> ForeignTarget -> SDoc
pprForeignTarget platform (ForeignTarget fn c) = ppr c <+> ppr_target fn
  where
        ppr_target :: CmmExpr -> SDoc
        ppr_target t@(CmmLit _) = pdoc platform t
        ppr_target fn'          = parens (pdoc platform fn')

pprForeignTarget platform (PrimTarget op)
 -- HACK: We're just using a ForeignLabel to get this printed, the label
 --       might not really be foreign.
 = pdoc platform
               (CmmLabel (mkForeignLabel
                         (mkFastString (show op))
                         Nothing ForeignLabelInThisPackage IsFunction))

pprNode :: Platform -> CmmNode e x -> SDoc
pprNode platform node = pp_node <+> pp_debug
  where
    pp_node :: SDoc
    pp_node = case node of
      -- label:
      CmmEntry id tscope ->
         (sdocOption sdocSuppressUniques $ \case
            True  -> text "_lbl_"
            False -> ppr id
         )
         <> colon
         <+> ppUnlessOption sdocSuppressTicks (text "//" <+> ppr tscope)

      -- // text
      CmmComment s -> text "//" <+> ftext s

      -- //tick bla<...>
      CmmTick t -> ppUnlessOption sdocSuppressTicks
                     (text "//tick" <+> ppr t)

      -- unwind reg = expr;
      CmmUnwind regs ->
          text "unwind "
          <> commafy (map (\(r,e) -> ppr r <+> char '=' <+> pdoc platform e) regs) <> semi

      -- reg = expr;
      CmmAssign reg expr -> ppr reg <+> equals <+> pdoc platform expr <> semi

      -- rep[lv] = expr;
      CmmStore lv expr align -> rep <> align_mark <> brackets (pdoc platform lv) <+> equals <+> pdoc platform expr <> semi
          where
            align_mark = case align of
                           Unaligned -> text "^"
                           NaturallyAligned -> empty
            rep = ppr ( cmmExprType platform expr )

      -- call "ccall" foo(x, y)[r1, r2];
      -- ToDo ppr volatile
      CmmUnsafeForeignCall target results args ->
          hsep [ ppUnless (null results) $
                    parens (commafy $ map ppr results) <+> equals,
                 text "call",
                 pdoc platform target <> parens (commafy $ map (pdoc platform) args) <> semi]

      -- goto label;
      CmmBranch ident -> text "goto" <+> ppr ident <> semi

      -- if (expr) goto t; else goto f;
      CmmCondBranch expr t f l ->
          hsep [ text "if"
               , parens (pdoc platform expr)
               , case l of
                   Nothing -> empty
                   Just b -> parens (text "likely:" <+> ppr b)
               , text "goto"
               , ppr t <> semi
               , text "else goto"
               , ppr f <> semi
               ]

      CmmSwitch expr ids ->
          hang (hsep [ text "switch"
                     , range
                     , if isTrivialCmmExpr expr
                       then pdoc platform expr
                       else parens (pdoc platform expr)
                     , text "{"
                     ])
             4 (vcat (map ppCase cases) $$ def) $$ rbrace
          where
            (cases, mbdef) = switchTargetsFallThrough ids
            ppCase (is,l) = hsep
                            [ text "case"
                            , commafy $ map integer is
                            , text ": goto"
                            , ppr l <> semi
                            ]
            def | Just l <- mbdef = hsep
                            [ text "default:"
                            , braces (text "goto" <+> ppr l <> semi)
                            ]
                | otherwise = empty

            range = brackets $ hsep [integer lo, text "..", integer hi]
              where (lo,hi) = switchTargetsRange ids

      CmmCall tgt k regs out res updfr_off ->
          hcat [ text "call", space
               , pprFun tgt, parens (interpp'SP regs), space
               , returns <+>
                 text "args: " <> ppr out <> comma <+>
                 text "res: " <> ppr res <> comma <+>
                 text "upd: " <> ppr updfr_off
               , semi ]
          where pprFun f@(CmmLit _) = pdoc platform f
                pprFun f = parens (pdoc platform f)

                returns
                  | Just r <- k = text "returns to" <+> ppr r <> comma
                  | otherwise   = empty

      CmmForeignCall {tgt=t, res=rs, args=as, succ=s, ret_args=a, ret_off=u, intrbl=i} ->
          hcat $ if i then [text "interruptible", space] else [] ++
               [ text "foreign call", space
               , pdoc platform t, text "(...)", space
               , text "returns to" <+> ppr s
                    <+> text "args:" <+> parens (pdoc platform as)
                    <+> text "ress:" <+> parens (ppr rs)
               , text "ret_args:" <+> ppr a
               , text "ret_off:" <+> ppr u
               , semi ]

    pp_debug :: SDoc
    pp_debug =
      if not debugIsOn then empty
      else case node of
             CmmEntry {}             -> empty -- Looks terrible with text "  // CmmEntry"
             CmmComment {}           -> empty -- Looks also terrible with text "  // CmmComment"
             CmmTick {}              -> empty
             CmmUnwind {}            -> text "  // CmmUnwind"
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
