{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module GHC.Hs.Extension

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

-- | Abstract Haskell syntax for expressions.
module GHC.Hs.Expr
  ( module GHC.Hs.Expr.Types
  , module GHC.Hs.Expr
  ) where

#include "HsVersions.h"

import GHC.Hs.Expr.Types

-- friends:
import GHC.Prelude

import GHC.Hs.Decls
import GHC.Hs.Pat
import GHC.Hs.Lit
import GHC.Hs.Extension
import GHC.Hs.Extension.GhcPass
import GHC.Hs.Type
import GHC.Hs.Binds

-- others:
import GHC.Tc.Types.Evidence
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Core.ConLike
import GHC.Unit.Module (ModuleName)
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.FastString
import GHC.Core.Type
import GHC.Builtin.Types (mkTupleStr)
import GHC.Tc.Utils.TcType (TcType)
import {-# SOURCE #-} GHC.Tc.Types (TcLclEnv)

-- libraries:
import Data.Data hiding (Fixity(..))
import qualified Data.Data as Data (Fixity(..))
import qualified Data.Kind
import Data.Maybe (isJust)

{-
************************************************************************
*                                                                      *
\subsection{Expressions proper}
*                                                                      *
************************************************************************
-}

-- | Post-Type checking Expression
--
-- PostTcExpr is an evidence expression attached to the syntax tree by the
-- type checker (c.f. postTcType).
type PostTcExpr  = HsExpr GhcTc

-- | Post-Type checking Table
--
-- We use a PostTcTable where there are a bunch of pieces of evidence, more
-- than is convenient to keep individually.
type PostTcTable = [(Name, PostTcExpr)]

-------------------------

-- Defining SyntaxExpr in two stages allows for better type inference, because
-- we can declare SyntaxExprGhc to be injective (and closed). Without injectivity,
-- noSyntaxExpr would be ambiguous.
type instance SyntaxExpr (GhcPass p) = SyntaxExprGhc p

type family SyntaxExprGhc (p :: Pass) = (r :: Data.Kind.Type) | r -> p where
  SyntaxExprGhc 'Parsed      = NoExtField
  SyntaxExprGhc 'Renamed     = SyntaxExprRn
  SyntaxExprGhc 'Typechecked = SyntaxExprTc

-- | The function to use in rebindable syntax. See Note [NoSyntaxExpr].
data SyntaxExprRn = SyntaxExprRn (HsExpr GhcRn)
    -- Why is the payload not just a Name?
    -- See Note [Monad fail : Rebindable syntax, overloaded strings] in "GHC.Rename.Expr"
                  | NoSyntaxExprRn

-- | An expression with wrappers, used for rebindable syntax
--
-- This should desugar to
--
-- > syn_res_wrap $ syn_expr (syn_arg_wraps[0] arg0)
-- >                         (syn_arg_wraps[1] arg1) ...
--
-- where the actual arguments come from elsewhere in the AST.
data SyntaxExprTc = SyntaxExprTc { syn_expr      :: HsExpr GhcTc
                                 , syn_arg_wraps :: [HsWrapper]
                                 , syn_res_wrap  :: HsWrapper }
                  | NoSyntaxExprTc  -- See Note [NoSyntaxExpr]

-- | This is used for rebindable-syntax pieces that are too polymorphic
-- for tcSyntaxOp (trS_fmap and the mzip in ParStmt)
noExpr :: HsExpr (GhcPass p)
noExpr = HsLit noExtField (HsString (SourceText  "noExpr") (fsLit "noExpr"))

noSyntaxExpr :: forall p. IsPass p => SyntaxExpr (GhcPass p)
                              -- Before renaming, and sometimes after
                              -- See Note [NoSyntaxExpr]
noSyntaxExpr = case ghcPass @p of
  GhcPs -> noExtField
  GhcRn -> NoSyntaxExprRn
  GhcTc -> NoSyntaxExprTc

-- | Make a 'SyntaxExpr GhcRn' from an expression
-- Used only in getMonadFailOp.
-- See Note [Monad fail : Rebindable syntax, overloaded strings] in "GHC.Rename.Expr"
mkSyntaxExpr :: HsExpr GhcRn -> SyntaxExprRn
mkSyntaxExpr = SyntaxExprRn

-- | Make a 'SyntaxExpr' from a 'Name' (the "rn" is because this is used in the
-- renamer).
mkRnSyntaxExpr :: Name -> SyntaxExprRn
mkRnSyntaxExpr name = SyntaxExprRn $ HsVar noExtField $ noLoc name

instance Outputable SyntaxExprRn where
  ppr (SyntaxExprRn expr) = ppr expr
  ppr NoSyntaxExprRn      = text "<no syntax expr>"

instance Outputable SyntaxExprTc where
  ppr (SyntaxExprTc { syn_expr      = expr
                    , syn_arg_wraps = arg_wraps
                    , syn_res_wrap  = res_wrap })
    = sdocOption sdocPrintExplicitCoercions $ \print_co ->
      getPprDebug $ \debug ->
      if debug || print_co
      then ppr expr <> braces (pprWithCommas ppr arg_wraps)
                    <> braces (ppr res_wrap)
      else ppr expr

  ppr NoSyntaxExprTc = text "<no syntax expr>"

-- | Extra data fields for a 'RecordCon', added by the type checker
data RecordConTc = RecordConTc
      { rcon_con_like :: ConLike      -- The data constructor or pattern synonym
      , rcon_con_expr :: PostTcExpr   -- Instantiated constructor function
      }

-- | Extra data fields for a 'RecordUpd', added by the type checker
data RecordUpdTc = RecordUpdTc
      { rupd_cons :: [ConLike]
                -- Filled in by the type checker to the
                -- _non-empty_ list of DataCons that have
                -- all the upd'd fields

      , rupd_in_tys  :: [Type]  -- Argument types of *input* record type
      , rupd_out_tys :: [Type]  --             and  *output* record type
                -- For a data family, these are the type args of the
                -- /representation/ type constructor

      , rupd_wrap :: HsWrapper  -- See note [Record Update HsWrapper]
      }

-- | HsWrap appears only in typechecker output
-- Invariant: The contained Expr is *NOT* itself an HsWrap.
-- See Note [Detecting forced eta expansion] in "GHC.HsToCore.Expr".
-- This invariant is maintained by 'GHC.Hs.Utils.mkHsWrap'.
-- hs_syn is something like HsExpr or HsCmd
data HsWrap hs_syn = HsWrap HsWrapper      -- the wrapper
                            (hs_syn GhcTc) -- the thing that is wrapped

deriving instance (Data (hs_syn GhcTc), Typeable hs_syn) => Data (HsWrap hs_syn)

type instance HsDoRn (GhcPass _) = GhcRn
type instance HsBracketRn (GhcPass _) = GhcRn
type instance PendingRnSplice' (GhcPass _) = PendingRnSplice
type instance PendingTcSplice' (GhcPass _) = PendingTcSplice

-- ---------------------------------------------------------------------

type instance XVar           (GhcPass _) = NoExtField
type instance XConLikeOut    (GhcPass _) = NoExtField
type instance XRecFld        (GhcPass _) = NoExtField
type instance XOverLabel     (GhcPass _) = NoExtField
type instance XIPVar         (GhcPass _) = NoExtField
type instance XOverLitE      (GhcPass _) = NoExtField
type instance XLitE          (GhcPass _) = NoExtField
type instance XLam           (GhcPass _) = NoExtField
type instance XLamCase       (GhcPass _) = NoExtField
type instance XApp           (GhcPass _) = NoExtField

type instance XUnboundVar    GhcPs = NoExtField
type instance XUnboundVar    GhcRn = NoExtField
type instance XUnboundVar    GhcTc = HoleExprRef
  -- We really don't need the whole HoleExprRef; just the IORef EvTerm
  -- would be enough. But then deriving a Data instance becomes impossible.
  -- Much, much easier just to define HoleExprRef with a Data instance and
  -- store the whole structure.

type instance XAppTypeE      GhcPs = NoExtField
type instance XAppTypeE      GhcRn = NoExtField
type instance XAppTypeE      GhcTc = Type

type instance XOpApp         GhcPs = NoExtField
type instance XOpApp         GhcRn = Fixity
type instance XOpApp         GhcTc = Fixity

type instance XNegApp        (GhcPass _) = NoExtField
type instance XPar           (GhcPass _) = NoExtField
type instance XSectionL      (GhcPass _) = NoExtField
type instance XSectionR      (GhcPass _) = NoExtField
type instance XExplicitTuple (GhcPass _) = NoExtField

type instance XExplicitSum   GhcPs = NoExtField
type instance XExplicitSum   GhcRn = NoExtField
type instance XExplicitSum   GhcTc = [Type]

type instance XCase          (GhcPass _) = NoExtField

type instance XIf            (GhcPass _) = NoExtField

type instance XMultiIf       GhcPs = NoExtField
type instance XMultiIf       GhcRn = NoExtField
type instance XMultiIf       GhcTc = Type

type instance XLet           (GhcPass _) = NoExtField

type instance XDo            GhcPs = NoExtField
type instance XDo            GhcRn = NoExtField
type instance XDo            GhcTc = Type

type instance XExplicitList  GhcPs = NoExtField
type instance XExplicitList  GhcRn = NoExtField
type instance XExplicitList  GhcTc = Type

type instance XRecordCon     GhcPs = NoExtField
type instance XRecordCon     GhcRn = NoExtField
type instance XRecordCon     GhcTc = RecordConTc

type instance XRecordUpd     GhcPs = NoExtField
type instance XRecordUpd     GhcRn = NoExtField
type instance XRecordUpd     GhcTc = RecordUpdTc

type instance XExprWithTySig (GhcPass _) = NoExtField

type instance XArithSeq      GhcPs = NoExtField
type instance XArithSeq      GhcRn = NoExtField
type instance XArithSeq      GhcTc = PostTcExpr

type instance XBracket       (GhcPass _) = NoExtField

type instance XRnBracketOut  (GhcPass _) = NoExtField
type instance XTcBracketOut  (GhcPass _) = NoExtField

type instance XSpliceE       (GhcPass _) = NoExtField
type instance XProc          (GhcPass _) = NoExtField

type instance XStatic        GhcPs = NoExtField
type instance XStatic        GhcRn = NameSet
type instance XStatic        GhcTc = NameSet

type instance XTick          (GhcPass _) = NoExtField
type instance XBinTick       (GhcPass _) = NoExtField

type instance XPragE         (GhcPass _) = NoExtField

type instance XXExpr         GhcPs       = NoExtCon

-- See Note [Rebindable syntax and HsExpansion] below
type instance XXExpr         GhcRn       = HsExpansion (HsExpr GhcRn)
                                                       (HsExpr GhcRn)
type instance XXExpr         GhcTc       = XXExprGhcTc

data XXExprGhcTc
  = WrapExpr {-# UNPACK #-} !(HsWrap HsExpr)
  | ExpansionExpr {-# UNPACK #-} !(HsExpansion (HsExpr GhcRn) (HsExpr GhcTc))



-- ---------------------------------------------------------------------

type instance XSCC           (GhcPass _) = NoExtField
type instance XXPragE        (GhcPass _) = NoExtCon

type instance XPresent         (GhcPass _) = NoExtField

type instance XMissing         GhcPs = NoExtField
type instance XMissing         GhcRn = NoExtField
type instance XMissing         GhcTc = Scaled Type

type instance XXTupArg         (GhcPass _) = NoExtCon

tupArgPresent :: LHsTupArg (GhcPass p) -> Bool
tupArgPresent (L _ (Present {})) = True
tupArgPresent (L _ (Missing {})) = False

instance (OutputableBndrId p) => Outputable (HsExpr (GhcPass p)) where
    ppr expr = pprExpr expr

-----------------------
-- pprExpr, pprLExpr, pprBinds call pprDeeper;
-- the underscore versions do not
pprLExpr :: (OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc
pprLExpr (L _ e) = pprExpr e

pprExpr :: (OutputableBndrId p) => HsExpr (GhcPass p) -> SDoc
pprExpr e | isAtomicHsExpr e || isQuietHsExpr e =            ppr_expr e
          | otherwise                           = pprDeeper (ppr_expr e)

isQuietHsExpr :: HsExpr id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsExpr (HsPar {})        = True
-- applications don't display anything themselves
isQuietHsExpr (HsApp {})        = True
isQuietHsExpr (HsAppType {})    = True
isQuietHsExpr (OpApp {})        = True
isQuietHsExpr _ = False

pprBinds :: (OutputableBndrId idL, OutputableBndrId idR)
         => HsLocalBindsLR (GhcPass idL) (GhcPass idR) -> SDoc
pprBinds b = pprDeeper (ppr b)

-----------------------
ppr_lexpr :: (OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc
ppr_lexpr e = ppr_expr (unLoc e)

ppr_expr :: forall p. (OutputableBndrId p)
         => HsExpr (GhcPass p) -> SDoc
ppr_expr (HsVar _ (L _ v))   = pprPrefixOcc v
ppr_expr (HsUnboundVar _ uv) = pprPrefixOcc uv
ppr_expr (HsConLikeOut _ c)  = pprPrefixOcc c
ppr_expr (HsRecFld _ f)      = pprPrefixOcc f
ppr_expr (HsIPVar _ v)       = ppr v
ppr_expr (HsOverLabel _ _ l) = char '#' <> ppr l
ppr_expr (HsLit _ lit)       = ppr lit
ppr_expr (HsOverLit _ lit)   = ppr lit
ppr_expr (HsPar _ e)         = parens (ppr_lexpr e)

ppr_expr (HsPragE _ prag e) = sep [ppr prag, ppr_lexpr e]

ppr_expr e@(HsApp {})        = ppr_apps e []
ppr_expr e@(HsAppType {})    = ppr_apps e []

ppr_expr (OpApp _ e1 op e2)
  | Just pp_op <- ppr_infix_expr (unLoc op)
  = pp_infixly pp_op
  | otherwise
  = pp_prefixly

  where
    pp_e1 = pprDebugParendExpr opPrec e1   -- In debug mode, add parens
    pp_e2 = pprDebugParendExpr opPrec e2   -- to make precedence clear

    pp_prefixly
      = hang (ppr op) 2 (sep [pp_e1, pp_e2])

    pp_infixly pp_op
      = hang pp_e1 2 (sep [pp_op, nest 2 pp_e2])

ppr_expr (NegApp _ e _) = char '-' <+> pprDebugParendExpr appPrec e

ppr_expr (SectionL _ expr op)
  | Just pp_op <- ppr_infix_expr (unLoc op)
  = pp_infixly pp_op
  | otherwise
  = pp_prefixly
  where
    pp_expr = pprDebugParendExpr opPrec expr

    pp_prefixly = hang (hsep [text " \\ x_ ->", ppr op])
                       4 (hsep [pp_expr, text "x_ )"])

    pp_infixly v = (sep [pp_expr, v])

ppr_expr (SectionR _ op expr)
  | Just pp_op <- ppr_infix_expr (unLoc op)
  = pp_infixly pp_op
  | otherwise
  = pp_prefixly
  where
    pp_expr = pprDebugParendExpr opPrec expr

    pp_prefixly = hang (hsep [text "( \\ x_ ->", ppr op, text "x_"])
                       4 (pp_expr <> rparen)

    pp_infixly v = sep [v, pp_expr]

ppr_expr (ExplicitTuple _ exprs boxity)
    -- Special-case unary boxed tuples so that they are pretty-printed as
    -- `Solo x`, not `(x)`
  | [L _ (Present _ expr)] <- exprs
  , Boxed <- boxity
  = hsep [text (mkTupleStr Boxed 1), ppr expr]
  | otherwise
  = tupleParens (boxityTupleSort boxity) (fcat (ppr_tup_args $ map unLoc exprs))
  where
    ppr_tup_args []               = []
    ppr_tup_args (Present _ e : es) = (ppr_lexpr e <> punc es) : ppr_tup_args es
    ppr_tup_args (Missing _   : es) = punc es : ppr_tup_args es

    punc (Present {} : _) = comma <> space
    punc (Missing {} : _) = comma
    punc (XTupArg {} : _) = comma <> space
    punc []               = empty

ppr_expr (ExplicitSum _ alt arity expr)
  = text "(#" <+> ppr_bars (alt - 1) <+> ppr expr <+> ppr_bars (arity - alt) <+> text "#)"
  where
    ppr_bars n = hsep (replicate n (char '|'))

ppr_expr (HsLam _ matches)
  = pprMatches matches

ppr_expr (HsLamCase _ matches)
  = sep [ sep [text "\\case"],
          nest 2 (pprMatches matches) ]

ppr_expr (HsCase _ expr matches@(MG { mg_alts = L _ [_] }))
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of {")],
          nest 2 (pprMatches matches) <+> char '}']
ppr_expr (HsCase _ expr matches)
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of")],
          nest 2 (pprMatches matches) ]

ppr_expr (HsIf _ e1 e2 e3)
  = sep [hsep [text "if", nest 2 (ppr e1), ptext (sLit "then")],
         nest 4 (ppr e2),
         text "else",
         nest 4 (ppr e3)]

ppr_expr (HsMultiIf _ alts)
  = hang (text "if") 3  (vcat (map ppr_alt alts))
  where ppr_alt (L _ (GRHS _ guards expr)) =
          hang vbar 2 (ppr_one one_alt)
          where
            ppr_one [] = panic "ppr_exp HsMultiIf"
            ppr_one (h:t) = hang h 2 (sep t)
            one_alt = [ interpp'SP guards
                      , text "->" <+> pprDeeper (ppr expr) ]
        ppr_alt (L _ (XGRHS x)) = ppr x

-- special case: let ... in let ...
ppr_expr (HsLet _ (L _ binds) expr@(L _ (HsLet _ _ _)))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lexpr expr]

ppr_expr (HsLet _ (L _ binds) expr)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr expr)]

ppr_expr (HsDo _ do_or_list_comp (L _ stmts)) = pprDo do_or_list_comp stmts

ppr_expr (ExplicitList _ _ exprs)
  = brackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (RecordCon { rcon_con_name = con_id, rcon_flds = rbinds })
  = hang (ppr con_id) 2 (ppr rbinds)

ppr_expr (RecordUpd { rupd_expr = L _ aexp, rupd_flds = rbinds })
  = hang (ppr aexp) 2 (braces (fsep (punctuate comma (map ppr rbinds))))

ppr_expr (ExprWithTySig _ expr sig)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
         4 (ppr sig)

ppr_expr (ArithSeq _ _ info) = brackets (ppr info)

ppr_expr (HsSpliceE _ s)         = pprSplice s
ppr_expr (HsBracket _ b)         = pprHsBracket b
ppr_expr (HsRnBracketOut _ e []) = ppr e
ppr_expr (HsRnBracketOut _ e ps) = ppr e $$ text "pending(rn)" <+> ppr ps
ppr_expr (HsTcBracketOut _ _wrap e []) = ppr e
ppr_expr (HsTcBracketOut _ _wrap e ps) = ppr e $$ text "pending(tc)" <+> pprIfTc @p (ppr ps)

ppr_expr (HsProc _ pat (L _ (HsCmdTop _ cmd)))
  = hsep [text "proc", ppr pat, ptext (sLit "->"), ppr cmd]

ppr_expr (HsStatic _ e)
  = hsep [text "static", ppr e]

ppr_expr (HsTick _ tickish exp)
  = pprTicks (ppr exp) $
    ppr tickish <+> ppr_lexpr exp
ppr_expr (HsBinTick _ tickIdTrue tickIdFalse exp)
  = pprTicks (ppr exp) $
    hcat [text "bintick<",
          ppr tickIdTrue,
          text ",",
          ppr tickIdFalse,
          text ">(",
          ppr exp, text ")"]

ppr_expr (XExpr x) = case ghcPass @p of
#if __GLASGOW_HASKELL__ < 811
  GhcPs -> ppr x
#endif
  GhcRn -> ppr x
  GhcTc -> case x of
    WrapExpr (HsWrap co_fn e) -> pprHsWrapper co_fn
      (\parens -> if parens then pprExpr e else pprExpr e)
    ExpansionExpr e -> ppr e -- e is an HsExpansion, we print the original
                             -- expression (LHsExpr GhcPs), not the
                             -- desugared one (LHsExpr GhcT).

ppr_infix_expr :: forall p. (OutputableBndrId p) => HsExpr (GhcPass p) -> Maybe SDoc
ppr_infix_expr (HsVar _ (L _ v))    = Just (pprInfixOcc v)
ppr_infix_expr (HsConLikeOut _ c)   = Just (pprInfixOcc (conLikeName c))
ppr_infix_expr (HsRecFld _ f)       = Just (pprInfixOcc f)
ppr_infix_expr (HsUnboundVar _ occ) = Just (pprInfixOcc occ)
ppr_infix_expr (XExpr x)            = case (ghcPass @p, x) of
#if __GLASGOW_HASKELL__ <= 810
  (GhcPs, _)                              -> Nothing
#endif
  (GhcRn, HsExpanded a _)                 -> ppr_infix_expr a
  (GhcTc, WrapExpr (HsWrap _ e))          -> ppr_infix_expr e
  (GhcTc, ExpansionExpr (HsExpanded a _)) -> ppr_infix_expr a
ppr_infix_expr _ = Nothing

ppr_apps :: (OutputableBndrId p)
         => HsExpr (GhcPass p)
         -> [Either (LHsExpr (GhcPass p)) (LHsWcType (NoGhcTc (GhcPass p)))]
         -> SDoc
ppr_apps (HsApp _ (L _ fun) arg)        args
  = ppr_apps fun (Left arg : args)
ppr_apps (HsAppType _ (L _ fun) arg)    args
  = ppr_apps fun (Right arg : args)
ppr_apps fun args = hang (ppr_expr fun) 2 (fsep (map pp args))
  where
    pp (Left arg)                             = ppr arg
    -- pp (Right (LHsWcTypeX (HsWC { hswc_body = L _ arg })))
    --   = char '@' <> pprHsType arg
    pp (Right arg)
      = text "@" <> ppr arg


pprDebugParendExpr :: (OutputableBndrId p)
                   => PprPrec -> LHsExpr (GhcPass p) -> SDoc
pprDebugParendExpr p expr
  = getPprDebug $ \case
      True  -> pprParendLExpr p expr
      False -> pprLExpr         expr

pprParendLExpr :: (OutputableBndrId p)
               => PprPrec -> LHsExpr (GhcPass p) -> SDoc
pprParendLExpr p (L _ e) = pprParendExpr p e

pprParendExpr :: (OutputableBndrId p)
              => PprPrec -> HsExpr (GhcPass p) -> SDoc
pprParendExpr p expr
  | hsExprNeedsParens p expr = parens (pprExpr expr)
  | otherwise                = pprExpr expr
        -- Using pprLExpr makes sure that we go 'deeper'
        -- I think that is usually (always?) right

-- | @'hsExprNeedsParens' p e@ returns 'True' if the expression @e@ needs
-- parentheses under precedence @p@.
hsExprNeedsParens :: forall p. IsPass p => PprPrec -> HsExpr (GhcPass p) -> Bool
hsExprNeedsParens p = go
  where
    go (HsVar{})                      = False
    go (HsUnboundVar{})               = False
    go (HsConLikeOut{})               = False
    go (HsIPVar{})                    = False
    go (HsOverLabel{})                = False
    go (HsLit _ l)                    = hsLitNeedsParens p l
    go (HsOverLit _ ol)               = hsOverLitNeedsParens p ol
    go (HsPar{})                      = False
    go (HsApp{})                      = p >= appPrec
    go (HsAppType {})                 = p >= appPrec
    go (OpApp{})                      = p >= opPrec
    go (NegApp{})                     = p > topPrec
    go (SectionL{})                   = True
    go (SectionR{})                   = True
    -- Special-case unary boxed tuple applications so that they are
    -- parenthesized as `Identity (Solo x)`, not `Identity Solo x` (#18612)
    -- See Note [One-tuples] in GHC.Builtin.Types
    go (ExplicitTuple _ [L _ Present{}] Boxed)
                                      = p >= appPrec
    go (ExplicitTuple{})              = False
    go (ExplicitSum{})                = False
    go (HsLam{})                      = p > topPrec
    go (HsLamCase{})                  = p > topPrec
    go (HsCase{})                     = p > topPrec
    go (HsIf{})                       = p > topPrec
    go (HsMultiIf{})                  = p > topPrec
    go (HsLet{})                      = p > topPrec
    go (HsDo _ sc _)
      | isComprehensionContext sc     = False
      | otherwise                     = p > topPrec
    go (ExplicitList{})               = False
    go (RecordUpd{})                  = False
    go (ExprWithTySig{})              = p >= sigPrec
    go (ArithSeq{})                   = False
    go (HsPragE{})                    = p >= appPrec
    go (HsSpliceE{})                  = False
    go (HsBracket{})                  = False
    go (HsRnBracketOut{})             = False
    go (HsTcBracketOut{})             = False
    go (HsProc{})                     = p > topPrec
    go (HsStatic{})                   = p >= appPrec
    go (HsTick _ _ (L _ e))           = go e
    go (HsBinTick _ _ _ (L _ e))      = go e
    go (RecordCon{})                  = False
    go (HsRecFld{})                   = False
    go (XExpr x)
      | GhcTc <- ghcPass @p
      = case x of
          WrapExpr      (HsWrap _ e)     -> go e
          ExpansionExpr (HsExpanded a _) -> hsExprNeedsParens p a
      | GhcRn <- ghcPass @p
      = case x of HsExpanded a _ -> hsExprNeedsParens p a
#if __GLASGOW_HASKELL__ <= 900
      | otherwise
      = True
#endif


-- | @'parenthesizeHsExpr' p e@ checks if @'hsExprNeedsParens' p e@ is true,
-- and if so, surrounds @e@ with an 'HsPar'. Otherwise, it simply returns @e@.
parenthesizeHsExpr :: IsPass p => PprPrec -> LHsExpr (GhcPass p) -> LHsExpr (GhcPass p)
parenthesizeHsExpr p le@(L loc e)
  | hsExprNeedsParens p e = L loc (HsPar noExtField le)
  | otherwise             = le

stripParensLHsExpr :: LHsExpr (GhcPass p) -> LHsExpr (GhcPass p)
stripParensLHsExpr (L _ (HsPar _ e)) = stripParensLHsExpr e
stripParensLHsExpr e = e

stripParensHsExpr :: HsExpr (GhcPass p) -> HsExpr (GhcPass p)
stripParensHsExpr (HsPar _ (L _ e)) = stripParensHsExpr e
stripParensHsExpr e = e

isAtomicHsExpr :: forall p. IsPass p => HsExpr (GhcPass p) -> Bool
-- True of a single token
isAtomicHsExpr (HsVar {})        = True
isAtomicHsExpr (HsConLikeOut {}) = True
isAtomicHsExpr (HsLit {})        = True
isAtomicHsExpr (HsOverLit {})    = True
isAtomicHsExpr (HsIPVar {})      = True
isAtomicHsExpr (HsOverLabel {})  = True
isAtomicHsExpr (HsUnboundVar {}) = True
isAtomicHsExpr (HsRecFld{})      = True
isAtomicHsExpr (XExpr x)
  | GhcTc <- ghcPass @p          = case x of
      WrapExpr      (HsWrap _ e)     -> isAtomicHsExpr e
      ExpansionExpr (HsExpanded a _) -> isAtomicHsExpr a
  | GhcRn <- ghcPass @p          = case x of
      HsExpanded a _         -> isAtomicHsExpr a
isAtomicHsExpr _                 = False

instance Outputable (HsPragE (GhcPass p)) where
  ppr (HsPragSCC _ st (StringLiteral stl lbl)) =
    pprWithSourceText st (text "{-# SCC")
     -- no doublequotes if stl empty, for the case where the SCC was written
     -- without quotes.
    <+> pprWithSourceText stl (ftext lbl) <+> text "#-}"

{-
************************************************************************
*                                                                      *
\subsection{Commands (in arrow abstractions)}
*                                                                      *
************************************************************************
-}

type instance XCmdArrApp  GhcPs = NoExtField
type instance XCmdArrApp  GhcRn = NoExtField
type instance XCmdArrApp  GhcTc = Type

type instance XCmdArrForm (GhcPass _) = NoExtField
type instance XCmdApp     (GhcPass _) = NoExtField
type instance XCmdLam     (GhcPass _) = NoExtField
type instance XCmdPar     (GhcPass _) = NoExtField
type instance XCmdCase    (GhcPass _) = NoExtField
type instance XCmdLamCase (GhcPass _) = NoExtField
type instance XCmdIf      (GhcPass _) = NoExtField
type instance XCmdLet     (GhcPass _) = NoExtField

type instance XCmdDo      GhcPs = NoExtField
type instance XCmdDo      GhcRn = NoExtField
type instance XCmdDo      GhcTc = Type

type instance XCmdWrap    (GhcPass _) = NoExtField

type instance XXCmd       GhcPs = NoExtCon
type instance XXCmd       GhcRn = NoExtCon
type instance XXCmd       GhcTc = HsWrap HsCmd
    -- If   cmd :: arg1 --> res
    --      wrap :: arg1 "->" arg2
    -- Then (XCmd (HsWrap wrap cmd)) :: arg2 --> res

data CmdTopTc
  = CmdTopTc Type    -- Nested tuple of inputs on the command's stack
             Type    -- return type of the command
             (CmdSyntaxTable GhcTc) -- See Note [CmdSyntaxTable]

type instance XCmdTop  GhcPs = NoExtField
type instance XCmdTop  GhcRn = CmdSyntaxTable GhcRn -- See Note [CmdSyntaxTable]
type instance XCmdTop  GhcTc = CmdTopTc

type instance XXCmdTop (GhcPass _) = NoExtCon

instance (OutputableBndrId p) => Outputable (HsCmd (GhcPass p)) where
    ppr cmd = pprCmd cmd

-----------------------
-- pprCmd and pprLCmd call pprDeeper;
-- the underscore versions do not
pprLCmd :: (OutputableBndrId p) => LHsCmd (GhcPass p) -> SDoc
pprLCmd (L _ c) = pprCmd c

pprCmd :: (OutputableBndrId p) => HsCmd (GhcPass p) -> SDoc
pprCmd c | isQuietHsCmd c =            ppr_cmd c
         | otherwise      = pprDeeper (ppr_cmd c)

isQuietHsCmd :: HsCmd id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsCmd (HsCmdPar {}) = True
-- applications don't display anything themselves
isQuietHsCmd (HsCmdApp {}) = True
isQuietHsCmd _ = False

-----------------------
ppr_lcmd :: (OutputableBndrId p) => LHsCmd (GhcPass p) -> SDoc
ppr_lcmd c = ppr_cmd (unLoc c)

ppr_cmd :: forall p. (OutputableBndrId p) => HsCmd (GhcPass p) -> SDoc
ppr_cmd (HsCmdPar _ c) = parens (ppr_lcmd c)

ppr_cmd (HsCmdApp _ c e)
  = let (fun, args) = collect_args c [e] in
    hang (ppr_lcmd fun) 2 (sep (map ppr args))
  where
    collect_args (L _ (HsCmdApp _ fun arg)) args = collect_args fun (arg:args)
    collect_args fun args = (fun, args)

ppr_cmd (HsCmdLam _ matches)
  = pprMatches matches

ppr_cmd (HsCmdCase _ expr matches)
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of")],
          nest 2 (pprMatches matches) ]

ppr_cmd (HsCmdLamCase _ matches)
  = sep [ text "\\case", nest 2 (pprMatches matches) ]

ppr_cmd (HsCmdIf _ _ e ct ce)
  = sep [hsep [text "if", nest 2 (ppr e), ptext (sLit "then")],
         nest 4 (ppr ct),
         text "else",
         nest 4 (ppr ce)]

-- special case: let ... in let ...
ppr_cmd (HsCmdLet _ (L _ binds) cmd@(L _ (HsCmdLet {})))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lcmd cmd]

ppr_cmd (HsCmdLet _ (L _ binds) cmd)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr cmd)]

ppr_cmd (HsCmdDo _ (L _ stmts))  = pprDo ArrowExpr stmts

ppr_cmd (HsCmdArrApp _ arrow arg HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp _ arrow arg HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_cmd (HsCmdArrApp _ arrow arg HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp _ arrow arg HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]

ppr_cmd (HsCmdArrForm _ (L _ (HsVar _ (L _ v))) _ (Just _) [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc v
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm _ (L _ (HsVar _ (L _ v))) Infix _    [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc v
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm _ (L _ (HsConLikeOut _ c)) _ (Just _) [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc (conLikeName c)
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm _ (L _ (HsConLikeOut _ c)) Infix _    [arg1, arg2])
  = hang (pprCmdArg (unLoc arg1)) 4 (sep [ pprInfixOcc (conLikeName c)
                                         , pprCmdArg (unLoc arg2)])
ppr_cmd (HsCmdArrForm _ op _ _ args)
  = hang (text "(|" <+> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <+> text "|)")
ppr_cmd (XCmd x) = case ghcPass @p of
#if __GLASGOW_HASKELL__ < 811
  GhcPs -> ppr x
  GhcRn -> ppr x
#endif
  GhcTc -> case x of
    HsWrap w cmd -> pprHsWrapper w (\_ -> parens (ppr_cmd cmd))

pprCmdArg :: (OutputableBndrId p) => HsCmdTop (GhcPass p) -> SDoc
pprCmdArg (HsCmdTop _ cmd)
  = ppr_lcmd cmd

instance (OutputableBndrId p) => Outputable (HsCmdTop (GhcPass p)) where
    ppr = pprCmdArg

{-
************************************************************************
*                                                                      *
\subsection{@Match@, @GRHSs@, and @GRHS@ datatypes}
*                                                                      *
************************************************************************
-}

type instance XMG         GhcPs b = NoExtField
type instance XMG         GhcRn b = NoExtField
type instance XMG         GhcTc b = MatchGroupTc

type instance XXMatchGroup (GhcPass _) b = NoExtCon

type instance XCMatch (GhcPass _) b = NoExtField
type instance XXMatch (GhcPass _) b = NoExtCon

instance (OutputableBndrId pr, Outputable body)
            => Outputable (Match (GhcPass pr) body) where
  ppr = pprMatch

isEmptyMatchGroup :: MatchGroup (GhcPass p) body -> Bool
isEmptyMatchGroup (MG { mg_alts = ms }) = null $ unLoc ms

-- | Is there only one RHS in this list of matches?
isSingletonMatchGroup :: [LMatch (GhcPass p) body] -> Bool
isSingletonMatchGroup matches
  | [L _ match] <- matches
  , Match { m_grhss = GRHSs { grhssGRHSs = [_] } } <- match
  = True
  | otherwise
  = False

matchGroupArity :: MatchGroup (GhcPass id) body -> Arity
-- Precondition: MatchGroup is non-empty
-- This is called before type checking, when mg_arg_tys is not set
matchGroupArity (MG { mg_alts = alts })
  | L _ (alt1:_) <- alts = length (hsLMatchPats alt1)
  | otherwise        = panic "matchGroupArity"

hsLMatchPats :: LMatch (GhcPass id) body -> [LPat (GhcPass id)]
hsLMatchPats (L _ (Match { m_pats = pats })) = pats

type instance XCGRHSs (GhcPass _) b = NoExtField
type instance XXGRHSs (GhcPass _) b = NoExtCon

type instance XCGRHS (GhcPass _) b = NoExtField
type instance XXGRHS (GhcPass _) b = NoExtCon

pprMatches :: (OutputableBndrId idR, Outputable body)
           => MatchGroup (GhcPass idR) body -> SDoc
pprMatches MG { mg_alts = matches }
    = vcat (map pprMatch (map unLoc (unLoc matches)))
      -- Don't print the type; it's only a place-holder before typechecking

-- Exported to GHC.Hs.Binds, which can't see the defn of HsMatchContext
pprFunBind :: (OutputableBndrId idR, Outputable body)
           => MatchGroup (GhcPass idR) body -> SDoc
pprFunBind matches = pprMatches matches

-- Exported to GHC.Hs.Binds, which can't see the defn of HsMatchContext
pprPatBind :: forall bndr p body. (OutputableBndrId bndr,
                                   OutputableBndrId p,
                                   Outputable body)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) body -> SDoc
pprPatBind pat (grhss)
 = sep [ppr pat,
       nest 2 (pprGRHSs (PatBindRhs :: HsMatchContext (GhcPass p)) grhss)]

pprMatch :: (OutputableBndrId idR, Outputable body)
         => Match (GhcPass idR) body -> SDoc
pprMatch (Match { m_pats = pats, m_ctxt = ctxt, m_grhss = grhss })
  = sep [ sep (herald : map (nest 2 . pprParendLPat appPrec) other_pats)
        , nest 2 (pprGRHSs ctxt grhss) ]
  where
    (herald, other_pats)
        = case ctxt of
            FunRhs {mc_fun=L _ fun, mc_fixity=fixity, mc_strictness=strictness}
                | SrcStrict <- strictness
                -> ASSERT(null pats)     -- A strict variable binding
                   (char '!'<>pprPrefixOcc fun, pats)

                | Prefix <- fixity
                -> (pprPrefixOcc fun, pats) -- f x y z = e
                                            -- Not pprBndr; the AbsBinds will
                                            -- have printed the signature
                | otherwise
                -> case pats of
                     (p1:p2:rest)
                        | null rest -> (pp_infix, [])           -- x &&& y = e
                        | otherwise -> (parens pp_infix, rest)  -- (x &&& y) z = e
                        where
                          pp_infix = pprParendLPat opPrec p1
                                     <+> pprInfixOcc fun
                                     <+> pprParendLPat opPrec p2
                     _ -> pprPanic "pprMatch" (ppr ctxt $$ ppr pats)

            LambdaExpr -> (char '\\', pats)

            _ -> case pats of
                   []    -> (empty, [])
                   [pat] -> (ppr pat, [])  -- No parens around the single pat in a case
                   _     -> pprPanic "pprMatch" (ppr ctxt $$ ppr pats)

pprGRHSs :: (OutputableBndrId idR, Outputable body)
         => HsMatchContext passL -> GRHSs (GhcPass idR) body -> SDoc
pprGRHSs ctxt (GRHSs _ grhss (L _ binds))
  = vcat (map (pprGRHS ctxt . unLoc) grhss)
  -- Print the "where" even if the contents of the binds is empty. Only
  -- EmptyLocalBinds means no "where" keyword
 $$ ppUnless (eqEmptyLocalBinds binds)
      (text "where" $$ nest 4 (pprBinds binds))

pprGRHS :: (OutputableBndrId idR, Outputable body)
        => HsMatchContext passL -> GRHS (GhcPass idR) body -> SDoc
pprGRHS ctxt (GRHS _ [] body)
 =  pp_rhs ctxt body

pprGRHS ctxt (GRHS _ guards body)
 = sep [vbar <+> interpp'SP guards, pp_rhs ctxt body]

pp_rhs :: Outputable body => HsMatchContext passL -> body -> SDoc
pp_rhs ctxt rhs = matchSeparator ctxt <+> pprDeeper (ppr rhs)

{-
************************************************************************
*                                                                      *
\subsection{Do stmts and list comprehensions}
*                                                                      *
************************************************************************
-}

-- Extra fields available post typechecking for RecStmt.
data RecStmtTc =
  RecStmtTc
     { recS_bind_ty :: Type       -- S in (>>=) :: Q -> (R -> S) -> T
     , recS_later_rets :: [PostTcExpr] -- (only used in the arrow version)
     , recS_rec_rets :: [PostTcExpr] -- These expressions correspond 1-to-1
                                  -- with recS_later_ids and recS_rec_ids,
                                  -- and are the expressions that should be
                                  -- returned by the recursion.
                                  -- They may not quite be the Ids themselves,
                                  -- because the Id may be *polymorphic*, but
                                  -- the returned thing has to be *monomorphic*,
                                  -- so they may be type applications

      , recS_ret_ty :: Type        -- The type of
                                   -- do { stmts; return (a,b,c) }
                                   -- With rebindable syntax the type might not
                                   -- be quite as simple as (m (tya, tyb, tyc)).
      }


type instance XLastStmt        (GhcPass _) (GhcPass _) b = NoExtField

type instance XBindStmt        (GhcPass _) GhcPs b = NoExtField
type instance XBindStmt        (GhcPass _) GhcRn b = XBindStmtRn
type instance XBindStmt        (GhcPass _) GhcTc b = XBindStmtTc

data XBindStmtRn = XBindStmtRn
  { xbsrn_bindOp :: SyntaxExpr GhcRn
  , xbsrn_failOp :: FailOperator GhcRn
  }

data XBindStmtTc = XBindStmtTc
  { xbstc_bindOp :: SyntaxExpr GhcTc
  , xbstc_boundResultType :: Type -- If (>>=) :: Q -> (R -> S) -> T, this is S
  , xbstc_boundResultMult :: Mult -- If (>>=) :: Q -> (R -> S) -> T, this is S
  , xbstc_failOp :: FailOperator GhcTc
  }

type instance XApplicativeStmt (GhcPass _) GhcPs b = NoExtField
type instance XApplicativeStmt (GhcPass _) GhcRn b = NoExtField
type instance XApplicativeStmt (GhcPass _) GhcTc b = Type

type instance XBodyStmt        (GhcPass _) GhcPs b = NoExtField
type instance XBodyStmt        (GhcPass _) GhcRn b = NoExtField
type instance XBodyStmt        (GhcPass _) GhcTc b = Type

type instance XLetStmt         (GhcPass _) (GhcPass _) b = NoExtField

type instance XParStmt         (GhcPass _) GhcPs b = NoExtField
type instance XParStmt         (GhcPass _) GhcRn b = NoExtField
type instance XParStmt         (GhcPass _) GhcTc b = Type

type instance XTransStmt       (GhcPass _) GhcPs b = NoExtField
type instance XTransStmt       (GhcPass _) GhcRn b = NoExtField
type instance XTransStmt       (GhcPass _) GhcTc b = Type

type instance XRecStmt         (GhcPass _) GhcPs b = NoExtField
type instance XRecStmt         (GhcPass _) GhcRn b = NoExtField
type instance XRecStmt         (GhcPass _) GhcTc b = RecStmtTc

type instance XXStmtLR         (GhcPass _) (GhcPass _) b = NoExtCon

type instance XParStmtBlock  (GhcPass pL) (GhcPass pR) = NoExtField
type instance XXParStmtBlock (GhcPass pL) (GhcPass pR) = NoExtCon

type instance XApplicativeArgOne GhcPs = NoExtField
type instance XApplicativeArgOne GhcRn = FailOperator GhcRn
type instance XApplicativeArgOne GhcTc = FailOperator GhcTc

type instance XApplicativeArgMany (GhcPass _) = NoExtField
type instance XXApplicativeArg    (GhcPass _) = NoExtCon

type instance ApplicativeArgStmCtxPass _ = GhcRn

instance (Outputable (StmtLR (GhcPass idL) (GhcPass idL) (LHsExpr (GhcPass idL))),
          Outputable (XXParStmtBlock (GhcPass idL) (GhcPass idR)))
        => Outputable (ParStmtBlock (GhcPass idL) (GhcPass idR)) where
  ppr (ParStmtBlock _ stmts _ _) = interpp'SP stmts

instance (OutputableBndrId pl, OutputableBndrId pr,
          Outputable body)
         => Outputable (StmtLR (GhcPass pl) (GhcPass pr) body) where
    ppr stmt = pprStmt stmt

pprStmt :: forall idL idR body . (OutputableBndrId idL,
                                  OutputableBndrId idR,
                                  Outputable body)
        => (StmtLR (GhcPass idL) (GhcPass idR) body) -> SDoc
pprStmt (LastStmt _ expr m_dollar_stripped _)
  = whenPprDebug (text "[last]") <+>
      (case m_dollar_stripped of
        Just True -> text "return $"
        Just False -> text "return"
        Nothing -> empty) <+>
      ppr expr
pprStmt (BindStmt _ pat expr) = pprBindStmt pat expr
pprStmt (LetStmt _ (L _ binds))   = hsep [text "let", pprBinds binds]
pprStmt (BodyStmt _ expr _ _)     = ppr expr
pprStmt (ParStmt _ stmtss _ _)   = sep (punctuate (text " | ") (map ppr stmtss))

pprStmt (TransStmt { trS_stmts = stmts, trS_by = by
                   , trS_using = using, trS_form = form })
  = sep $ punctuate comma (map ppr stmts ++ [pprTransStmt by using form])

pprStmt (RecStmt { recS_stmts = segment, recS_rec_ids = rec_ids
                 , recS_later_ids = later_ids })
  = text "rec" <+>
    vcat [ ppr_do_stmts segment
         , whenPprDebug (vcat [ text "rec_ids=" <> ppr rec_ids
                            , text "later_ids=" <> ppr later_ids])]

pprStmt (ApplicativeStmt _ args mb_join)
  = getPprStyle $ \style ->
      if userStyle style
         then pp_for_user
         else pp_debug
  where
  -- make all the Applicative stuff invisible in error messages by
  -- flattening the whole ApplicativeStmt nest back to a sequence
  -- of statements.
   pp_for_user = vcat $ concatMap flattenArg args

   -- ppr directly rather than transforming here, because we need to
   -- inject a "return" which is hard when we're polymorphic in the id
   -- type.
   flattenStmt :: ExprLStmt (GhcPass idL) -> [SDoc]
   flattenStmt (L _ (ApplicativeStmt _ args _)) = concatMap flattenArg args
   flattenStmt stmt = [ppr stmt]

   flattenArg :: forall a . (a, ApplicativeArg (GhcPass idL)) -> [SDoc]
   flattenArg (_, ApplicativeArgOne _ pat expr isBody)
     | isBody =  [ppr expr] -- See Note [Applicative BodyStmt]
     | otherwise = [pprBindStmt pat expr]
   flattenArg (_, ApplicativeArgMany _ stmts _ _ _) =
     concatMap flattenStmt stmts

   pp_debug =
     let
         ap_expr = sep (punctuate (text " |") (map pp_arg args))
     in
       whenPprDebug (if isJust mb_join then text "[join]" else empty) <+>
       (if lengthAtLeast args 2 then parens else id) ap_expr

   pp_arg :: (a, ApplicativeArg (GhcPass idL)) -> SDoc
   pp_arg (_, applicativeArg) = ppr applicativeArg

pprBindStmt :: (Outputable pat, Outputable expr) => pat -> expr -> SDoc
pprBindStmt pat expr = hsep [ppr pat, larrow, ppr expr]

instance (OutputableBndrId idL)
      => Outputable (ApplicativeArg (GhcPass idL)) where
  ppr = pprArg

pprArg :: forall idL . (OutputableBndrId idL) => ApplicativeArg (GhcPass idL) -> SDoc
pprArg (ApplicativeArgOne _ pat expr isBody)
  | isBody = ppr expr -- See Note [Applicative BodyStmt]
  | otherwise = pprBindStmt pat expr
pprArg (ApplicativeArgMany _ stmts return pat ctxt) =
     ppr pat <+>
     text "<-" <+>
     pprDo ctxt (stmts ++
                   [noLoc (LastStmt noExtField (noLoc return) Nothing noSyntaxExpr)])

pprTransformStmt :: (OutputableBndrId p)
                 => [IdP (GhcPass p)] -> LHsExpr (GhcPass p)
                 -> Maybe (LHsExpr (GhcPass p)) -> SDoc
pprTransformStmt bndrs using by
  = sep [ text "then" <+> whenPprDebug (braces (ppr bndrs))
        , nest 2 (ppr using)
        , nest 2 (pprBy by)]

pprTransStmt :: Outputable body => Maybe body -> body -> TransForm -> SDoc
pprTransStmt by using ThenForm
  = sep [ text "then", nest 2 (ppr using), nest 2 (pprBy by)]
pprTransStmt by using GroupForm
  = sep [ text "then group", nest 2 (pprBy by), nest 2 (ptext (sLit "using") <+> ppr using)]

pprBy :: Outputable body => Maybe body -> SDoc
pprBy Nothing  = empty
pprBy (Just e) = text "by" <+> ppr e

pprDo :: (OutputableBndrId p, Outputable body)
      => HsStmtContext any -> [LStmt (GhcPass p) body] -> SDoc
pprDo (DoExpr m)    stmts =
  ppr_module_name_prefix m <> text "do"  <+> ppr_do_stmts stmts
pprDo GhciStmtCtxt  stmts = text "do"  <+> ppr_do_stmts stmts
pprDo ArrowExpr     stmts = text "do"  <+> ppr_do_stmts stmts
pprDo (MDoExpr m)   stmts =
  ppr_module_name_prefix m <> text "mdo"  <+> ppr_do_stmts stmts
pprDo ListComp      stmts = brackets    $ pprComp stmts
pprDo MonadComp     stmts = brackets    $ pprComp stmts
pprDo _             _     = panic "pprDo" -- PatGuard, ParStmtCxt

ppr_module_name_prefix :: Maybe ModuleName -> SDoc
ppr_module_name_prefix = \case
  Nothing -> empty
  Just module_name -> ppr module_name <> char '.'

ppr_do_stmts :: (OutputableBndrId idL, OutputableBndrId idR,
                 Outputable body)
             => [LStmtLR (GhcPass idL) (GhcPass idR) body] -> SDoc
-- Print a bunch of do stmts
ppr_do_stmts stmts = pprDeeperList vcat (map ppr stmts)

pprComp :: (OutputableBndrId p, Outputable body)
        => [LStmt (GhcPass p) body] -> SDoc
pprComp quals     -- Prints:  body | qual1, ..., qualn
  | Just (initStmts, L _ (LastStmt _ body _ _)) <- snocView quals
  = if null initStmts
       -- If there are no statements in a list comprehension besides the last
       -- one, we simply treat it like a normal list. This does arise
       -- occasionally in code that GHC generates, e.g., in implementations of
       -- 'range' for derived 'Ix' instances for product datatypes with exactly
       -- one constructor (e.g., see #12583).
       then ppr body
       else hang (ppr body <+> vbar) 2 (pprQuals initStmts)
  | otherwise
  = pprPanic "pprComp" (pprQuals quals)

pprQuals :: (OutputableBndrId p, Outputable body)
         => [LStmt (GhcPass p) body] -> SDoc
-- Show list comprehension qualifiers separated by commas
pprQuals quals = interpp'SP quals

{-
************************************************************************
*                                                                      *
                Template Haskell quotation brackets
*                                                                      *
************************************************************************
-}

newtype HsSplicedT = HsSplicedT DelayedSplice deriving (Data)

type instance XTypedSplice   (GhcPass _) = NoExtField
type instance XUntypedSplice (GhcPass _) = NoExtField
type instance XQuasiQuote    (GhcPass _) = NoExtField
type instance XSpliced       (GhcPass _) = NoExtField
type instance XXSplice       GhcPs       = NoExtCon
type instance XXSplice       GhcRn       = NoExtCon
type instance XXSplice       GhcTc       = HsSplicedT

-- See Note [Running typed splices in the zonker]
-- These are the arguments that are passed to `GHC.Tc.Gen.Splice.runTopSplice`
data DelayedSplice =
  DelayedSplice
    TcLclEnv          -- The local environment to run the splice in
    (LHsExpr GhcRn)   -- The original renamed expression
    TcType            -- The result type of running the splice, unzonked
    (LHsExpr GhcTc)   -- The typechecked expression to run and splice in the result

-- A Data instance which ignores the argument of 'DelayedSplice'.
instance Data DelayedSplice where
  gunfold _ _ _ = panic "DelayedSplice"
  toConstr  a   = mkConstr (dataTypeOf a) "DelayedSplice" [] Data.Prefix
  dataTypeOf a  = mkDataType "HsExpr.DelayedSplice" [toConstr a]

-- | Pending Renamer Splice
data PendingRnSplice
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (LHsExpr GhcRn)

-- | Pending Type-checker Splice
data PendingTcSplice
  = PendingTcSplice SplicePointName (LHsExpr GhcTc)

{-
Note [Pending Splices]
~~~~~~~~~~~~~~~~~~~~~~
When we rename an untyped bracket, we name and lift out all the nested
splices, so that when the typechecker hits the bracket, it can
typecheck those nested splices without having to walk over the untyped
bracket code.  So for example
    [| f $(g x) |]
looks like

    HsBracket (HsApp (HsVar "f") (HsSpliceE _ (g x)))

which the renamer rewrites to

    HsRnBracketOut (HsApp (HsVar f) (HsSpliceE sn (g x)))
                   [PendingRnSplice UntypedExpSplice sn (g x)]

* The 'sn' is the Name of the splice point, the SplicePointName

* The PendingRnExpSplice gives the splice that splice-point name maps to;
  and the typechecker can now conveniently find these sub-expressions

* The other copy of the splice, in the second argument of HsSpliceE
                                in the renamed first arg of HsRnBracketOut
  is used only for pretty printing

There are four varieties of pending splices generated by the renamer,
distinguished by their UntypedSpliceFlavour

 * Pending expression splices (UntypedExpSplice), e.g.,
       [|$(f x) + 2|]

   UntypedExpSplice is also used for
     * quasi-quotes, where the pending expression expands to
          $(quoter "...blah...")
       (see GHC.Rename.Splice.makePending, HsQuasiQuote case)

     * cross-stage lifting, where the pending expression expands to
          $(lift x)
       (see GHC.Rename.Splice.checkCrossStageLifting)

 * Pending pattern splices (UntypedPatSplice), e.g.,
       [| \$(f x) -> x |]

 * Pending type splices (UntypedTypeSplice), e.g.,
       [| f :: $(g x) |]

 * Pending declaration (UntypedDeclSplice), e.g.,
       [| let $(f x) in ... |]

There is a fifth variety of pending splice, which is generated by the type
checker:

  * Pending *typed* expression splices, (PendingTcSplice), e.g.,
        [||1 + $$(f 2)||]

It would be possible to eliminate HsRnBracketOut and use HsBracketOut for the
output of the renamer. However, when pretty printing the output of the renamer,
e.g., in a type error message, we *do not* want to print out the pending
splices. In contrast, when pretty printing the output of the type checker, we
*do* want to print the pending splices. So splitting them up seems to make
sense, although I hate to add another constructor to HsExpr.
-}

instance OutputableBndrId p
       => Outputable (HsSplicedThing (GhcPass p)) where
  ppr (HsSplicedExpr e) = ppr_expr e
  ppr (HsSplicedTy   t) = ppr t
  ppr (HsSplicedPat  p) = ppr p

instance (OutputableBndrId p) => Outputable (HsSplice (GhcPass p)) where
  ppr s = pprSplice s

pprPendingSplice :: (OutputableBndrId p)
                 => SplicePointName -> LHsExpr (GhcPass p) -> SDoc
pprPendingSplice n e = angleBrackets (ppr n <> comma <+> ppr (stripParensLHsExpr e))

pprSpliceDecl ::  (OutputableBndrId p)
          => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc
pprSpliceDecl e@HsQuasiQuote{} _ = pprSplice e
pprSpliceDecl e ExplicitSplice   = text "$" <> ppr_splice_decl e
pprSpliceDecl e ImplicitSplice   = ppr_splice_decl e

ppr_splice_decl :: (OutputableBndrId p)
                => HsSplice (GhcPass p) -> SDoc
ppr_splice_decl (HsUntypedSplice _ _ n e) = ppr_splice empty n e empty
ppr_splice_decl e = pprSplice e

pprSplice :: forall p. (OutputableBndrId p) => HsSplice (GhcPass p) -> SDoc
pprSplice (HsTypedSplice _ DollarSplice n e)
  = ppr_splice (text "$$") n e empty
pprSplice (HsTypedSplice _ BareSplice _ _ )
  = panic "Bare typed splice"  -- impossible
pprSplice (HsUntypedSplice _ DollarSplice n e)
  = ppr_splice (text "$")  n e empty
pprSplice (HsUntypedSplice _ BareSplice n e)
  = ppr_splice empty  n e empty
pprSplice (HsQuasiQuote _ n q _ s)      = ppr_quasi n q s
pprSplice (HsSpliced _ _ thing)         = ppr thing
pprSplice (XSplice x)                   = case ghcPass @p of
#if __GLASGOW_HASKELL__ < 811
                                            GhcPs -> noExtCon x
                                            GhcRn -> noExtCon x
#endif
                                            GhcTc -> case x of
                                                       HsSplicedT _ -> text "Unevaluated typed splice"

ppr_quasi :: OutputableBndr p => p -> p -> FastString -> SDoc
ppr_quasi n quoter quote = whenPprDebug (brackets (ppr n)) <>
                           char '[' <> ppr quoter <> vbar <>
                           ppr quote <> text "|]"

ppr_splice :: (OutputableBndrId p)
           => SDoc -> (IdP (GhcPass p)) -> LHsExpr (GhcPass p) -> SDoc -> SDoc
ppr_splice herald n e trail
    = herald <> whenPprDebug (brackets (ppr n)) <> ppr e <> trail

type instance XExpBr      (GhcPass _) = NoExtField
type instance XPatBr      (GhcPass _) = NoExtField
type instance XDecBrL     (GhcPass _) = NoExtField
type instance XDecBrG     (GhcPass _) = NoExtField
type instance XTypBr      (GhcPass _) = NoExtField
type instance XVarBr      (GhcPass _) = NoExtField
type instance XTExpBr     (GhcPass _) = NoExtField
type instance XXBracket   (GhcPass _) = NoExtCon

instance OutputableBndrId p
          => Outputable (HsBracket (GhcPass p)) where
  ppr = pprHsBracket


pprHsBracket :: (OutputableBndrId p) => HsBracket (GhcPass p) -> SDoc
pprHsBracket (ExpBr _ e)   = thBrackets empty (ppr e)
pprHsBracket (PatBr _ p)   = thBrackets (char 'p') (ppr p)
pprHsBracket (DecBrG _ gp) = thBrackets (char 'd') (ppr gp)
pprHsBracket (DecBrL _ ds) = thBrackets (char 'd') (vcat (map ppr ds))
pprHsBracket (TypBr _ t)   = thBrackets (char 't') (ppr t)
pprHsBracket (VarBr _ True n)
  = char '\'' <> pprPrefixOcc n
pprHsBracket (VarBr _ False n)
  = text "''" <> pprPrefixOcc n
pprHsBracket (TExpBr _ e)  = thTyBrackets (ppr e)

thBrackets :: SDoc -> SDoc -> SDoc
thBrackets pp_kind pp_body = char '[' <> pp_kind <> vbar <+>
                             pp_body <+> text "|]"

thTyBrackets :: SDoc -> SDoc
thTyBrackets pp_body = text "[||" <+> pp_body <+> ptext (sLit "||]")

instance Outputable PendingRnSplice where
  ppr (PendingRnSplice _ n e) = pprPendingSplice n e

instance Outputable PendingTcSplice where
  ppr (PendingTcSplice n e) = pprPendingSplice n e

{-
************************************************************************
*                                                                      *
\subsection{Enumerations and list comprehensions}
*                                                                      *
************************************************************************
-}

instance OutputableBndrId p
         => Outputable (ArithSeqInfo (GhcPass p)) where
    ppr (From e1)             = hcat [ppr e1, pp_dotdot]
    ppr (FromThen e1 e2)      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot]
    ppr (FromTo e1 e3)        = hcat [ppr e1, pp_dotdot, ppr e3]
    ppr (FromThenTo e1 e2 e3)
      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot, ppr e3]

pp_dotdot :: SDoc
pp_dotdot = text " .. "

{-
************************************************************************
*                                                                      *
\subsection{HsMatchCtxt}
*                                                                      *
************************************************************************
-}

instance OutputableBndrId p => Outputable (HsMatchContext (GhcPass p)) where
  ppr m@(FunRhs{})          = text "FunRhs" <+> ppr (mc_fun m) <+> ppr (mc_fixity m)
  ppr LambdaExpr            = text "LambdaExpr"
  ppr CaseAlt               = text "CaseAlt"
  ppr IfAlt                 = text "IfAlt"
  ppr ProcExpr              = text "ProcExpr"
  ppr PatBindRhs            = text "PatBindRhs"
  ppr PatBindGuards         = text "PatBindGuards"
  ppr RecUpd                = text "RecUpd"
  ppr (StmtCtxt _)          = text "StmtCtxt _"
  ppr ThPatSplice           = text "ThPatSplice"
  ppr ThPatQuote            = text "ThPatQuote"
  ppr PatSyn                = text "PatSyn"

-----------------

instance OutputableBndrId p
      => Outputable (HsStmtContext (GhcPass p)) where
    ppr = pprStmtContext

-- Used to generate the string for a *runtime* error message
matchContextErrString :: OutputableBndrId p
                      => HsMatchContext (GhcPass p) -> SDoc
matchContextErrString (FunRhs{mc_fun=L _ fun})   = text "function" <+> ppr fun
matchContextErrString CaseAlt                    = text "case"
matchContextErrString IfAlt                      = text "multi-way if"
matchContextErrString PatBindRhs                 = text "pattern binding"
matchContextErrString PatBindGuards              = text "pattern binding guards"
matchContextErrString RecUpd                     = text "record update"
matchContextErrString LambdaExpr                 = text "lambda"
matchContextErrString ProcExpr                   = text "proc"
matchContextErrString ThPatSplice                = panic "matchContextErrString"  -- Not used at runtime
matchContextErrString ThPatQuote                 = panic "matchContextErrString"  -- Not used at runtime
matchContextErrString PatSyn                     = panic "matchContextErrString"  -- Not used at runtime
matchContextErrString (StmtCtxt (ParStmtCtxt c))   = matchContextErrString (StmtCtxt c)
matchContextErrString (StmtCtxt (TransStmtCtxt c)) = matchContextErrString (StmtCtxt c)
matchContextErrString (StmtCtxt (PatGuard _))      = text "pattern guard"
matchContextErrString (StmtCtxt GhciStmtCtxt)      = text "interactive GHCi command"
matchContextErrString (StmtCtxt (DoExpr m))        = prependQualified m (text "'do' block")
matchContextErrString (StmtCtxt ArrowExpr)         = text "'do' block"
matchContextErrString (StmtCtxt (MDoExpr m))       = prependQualified m (text "'mdo' block")
matchContextErrString (StmtCtxt ListComp)          = text "list comprehension"
matchContextErrString (StmtCtxt MonadComp)         = text "monad comprehension"

pprMatchInCtxt :: (OutputableBndrId idR, Outputable body)
               => Match (GhcPass idR) body -> SDoc
pprMatchInCtxt match  = hang (text "In" <+> pprMatchContext (m_ctxt match)
                                        <> colon)
                             4 (pprMatch match)

pprStmtInCtxt :: (OutputableBndrId idL,
                  OutputableBndrId idR,
                  Outputable body)
              => HsStmtContext (GhcPass idL)
              -> StmtLR (GhcPass idL) (GhcPass idR) body
              -> SDoc
pprStmtInCtxt ctxt (LastStmt _ e _ _)
  | isComprehensionContext ctxt      -- For [ e | .. ], do not mutter about "stmts"
  = hang (text "In the expression:") 2 (ppr e)

pprStmtInCtxt ctxt stmt
  = hang (text "In a stmt of" <+> pprAStmtContext ctxt <> colon)
       2 (ppr_stmt stmt)
  where
    -- For Group and Transform Stmts, don't print the nested stmts!
    ppr_stmt (TransStmt { trS_by = by, trS_using = using
                        , trS_form = form }) = pprTransStmt by using form
    ppr_stmt stmt = pprStmt stmt
