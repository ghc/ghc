{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
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

module PprCmm
  ( module PprCmmDecl
  , module PprCmmExpr
  )
where

import GhcPrelude hiding (succ)

import BlockId ()
import CLabel
import Cmm
import CmmUtils
import CmmSwitch
import DynFlags
import FastString
import Outputable
import PprCmmDecl
import PprCmmExpr
import Util
import PprCore ()

import BasicTypes
import Hoopl.Block
import Hoopl.Graph

-------------------------------------------------
-- Outputable instances

instance Outputable CmmStackInfo where
    ppr = pprStackInfo

instance Outputable CmmTopInfo where
    ppr = pprTopInfo


instance Outputable (CmmNode e x) where
    ppr = pprNode

instance Outputable Convention where
    ppr = pprConvention

instance Outputable ForeignConvention where
    ppr = pprForeignConvention

instance Outputable ForeignTarget where
    ppr = pprForeignTarget

instance Outputable CmmReturnInfo where
    ppr = pprReturnInfo

instance Outputable (Block CmmNode C C) where
    ppr = pprBlock
instance Outputable (Block CmmNode C O) where
    ppr = pprBlock
instance Outputable (Block CmmNode O C) where
    ppr = pprBlock
instance Outputable (Block CmmNode O O) where
    ppr = pprBlock

instance Outputable (Graph CmmNode e x) where
    ppr = pprGraph

instance Outputable CmmGraph where
    ppr = pprCmmGraph

----------------------------------------------------------
-- Outputting types Cmm contains

pprStackInfo :: CmmStackInfo -> SDoc
pprStackInfo (StackInfo {arg_space=arg_space, updfr_space=updfr_space}) =
  text "arg_space: " <> ppr arg_space <+>
  text "updfr_space: " <> ppr updfr_space

pprTopInfo :: CmmTopInfo -> SDoc
pprTopInfo (TopInfo {info_tbls=info_tbl, stack_info=stack_info}) =
  vcat [text "info_tbls: " <> ppr info_tbl,
        text "stack_info: " <> ppr stack_info]

----------------------------------------------------------
-- Outputting blocks and graphs

pprBlock :: IndexedCO x SDoc SDoc ~ SDoc
         => Block CmmNode e x -> IndexedCO e SDoc SDoc
pprBlock block
    = foldBlockNodesB3 ( ($$) . ppr
                       , ($$) . (nest 4) . ppr
                       , ($$) . (nest 4) . ppr
                       )
                       block
                       empty

pprGraph :: Graph CmmNode e x -> SDoc
pprGraph GNil = empty
pprGraph (GUnit block) = ppr block
pprGraph (GMany entry body exit)
   = text "{"
  $$ nest 2 (pprMaybeO entry $$ (vcat $ map ppr $ bodyToBlockList body) $$ pprMaybeO exit)
  $$ text "}"
  where pprMaybeO :: Outputable (Block CmmNode e x)
                  => MaybeO ex (Block CmmNode e x) -> SDoc
        pprMaybeO NothingO = empty
        pprMaybeO (JustO block) = ppr block

pprCmmGraph :: CmmGraph -> SDoc
pprCmmGraph g
   = text "{" <> text "offset"
  $$ nest 2 (vcat $ map ppr blocks)
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

pprForeignTarget :: ForeignTarget -> SDoc
pprForeignTarget (ForeignTarget fn c) = ppr c <+> ppr_target fn
  where
        ppr_target :: CmmExpr -> SDoc
        ppr_target t@(CmmLit _) = ppr t
        ppr_target fn'          = parens (ppr fn')

pprForeignTarget (PrimTarget op)
 -- HACK: We're just using a ForeignLabel to get this printed, the label
 --       might not really be foreign.
 = ppr
               (CmmLabel (mkForeignLabel
                         (mkFastString (show op))
                         Nothing ForeignLabelInThisPackage IsFunction))

pprNode :: CmmNode e x -> SDoc
pprNode node = pp_node <+> pp_debug
  where
    pp_node :: SDoc
    pp_node = sdocWithDynFlags $ \dflags -> case node of
      -- label:
      CmmEntry id tscope -> lbl <> colon <+>
         (sdocWithDynFlags $ \dflags ->
           ppUnless (gopt Opt_SuppressTicks dflags) (text "//" <+> ppr tscope))
          where
            lbl = if gopt Opt_SuppressUniques dflags
                then text "_lbl_"
                else ppr id

      -- // text
      CmmComment s -> text "//" <+> ftext s

      -- //tick bla<...>
      CmmTick t -> ppUnless (gopt Opt_SuppressTicks dflags) $
                   text "//tick" <+> ppr t

      -- unwind reg = expr;
      CmmUnwind regs ->
          text "unwind "
          <> commafy (map (\(r,e) -> ppr r <+> char '=' <+> ppr e) regs) <> semi

      -- reg = expr;
      CmmAssign reg expr -> ppr reg <+> equals <+> ppr expr <> semi

      -- rep[lv] = expr;
      CmmStore lv expr -> rep <> brackets(ppr lv) <+> equals <+> ppr expr <> semi
          where
            rep = sdocWithDynFlags $ \dflags ->
                  ppr ( cmmExprType dflags expr )

      -- call "ccall" foo(x, y)[r1, r2];
      -- ToDo ppr volatile
      CmmUnsafeForeignCall target results args ->
          hsep [ ppUnless (null results) $
                    parens (commafy $ map ppr results) <+> equals,
                 text "call",
                 ppr target <> parens (commafy $ map ppr args) <> semi]

      -- goto label;
      CmmBranch ident -> text "goto" <+> ppr ident <> semi

      -- if (expr) goto t; else goto f;
      CmmCondBranch expr t f l ->
          hsep [ text "if"
               , parens(ppr expr)
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
                       then ppr expr
                       else parens (ppr expr)
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
          where pprFun f@(CmmLit _) = ppr f
                pprFun f = parens (ppr f)

                returns
                  | Just r <- k = text "returns to" <+> ppr r <> comma
                  | otherwise   = empty

      CmmForeignCall {tgt=t, res=rs, args=as, succ=s, ret_args=a, ret_off=u, intrbl=i} ->
          hcat $ if i then [text "interruptible", space] else [] ++
               [ text "foreign call", space
               , ppr t, text "(...)", space
               , text "returns to" <+> ppr s
                    <+> text "args:" <+> parens (ppr as)
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
