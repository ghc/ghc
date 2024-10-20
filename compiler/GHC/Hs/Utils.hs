{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections   #-}

{-|
Module      : GHC.Hs.Utils
Description : Generic helpers for the HsSyn type.
Copyright   : (c) The University of Glasgow, 1992-2023

Here we collect a variety of helper functions that construct or
analyse HsSyn.  All these functions deal with generic HsSyn; functions
which deal with the instantiated versions are located elsewhere:

   Parameterised by          Module
   ----------------          -------------
   GhcPs/RdrName             GHC.Parser.PostProcess
   GhcRn/Name                GHC.Rename.*
   GhcTc/Id                  GHC.Tc.Zonk.Type

The @mk*@ functions attempt to construct a not-completely-useless SrcSpan
from their components, compared with the @nl*@ functions which
just attach noSrcSpan to everything.

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Hs.Utils(
  -- * Terms
  mkHsPar, mkHsApp, mkHsAppWith, mkHsApps, mkHsAppsWith, mkHsSyntaxApps,
  mkHsAppType, mkHsAppTypes, mkHsCaseAlt,
  mkSimpleMatch, unguardedGRHSs, unguardedRHS,
  mkMatchGroup, mkLamCaseMatchGroup, mkMatch, mkPrefixFunRhs, mkHsLam, mkHsIf,
  mkHsWrap, mkLHsWrap, mkHsWrapCo, mkHsWrapCoR, mkLHsWrapCo,
  mkHsDictLet, mkHsLams,
  mkHsOpApp, mkHsDo, mkHsDoAnns, mkHsComp, mkHsCompAnns,
  mkHsWrapPat, mkLHsWrapPat, mkHsWrapPatCo,
  mkLHsPar, mkHsCmdWrap, mkLHsCmdWrap,
  mkHsCmdIf, mkConLikeTc,

  nlHsTyApp, nlHsTyApps, nlHsVar, nl_HsVar, nlHsDataCon,
  nlHsLit, nlHsApp, nlHsApps, nlHsSyntaxApps,
  nlHsIntLit, nlHsVarApps,
  nlHsDo, nlHsOpApp, nlHsLam, nlHsPar, nlHsIf, nlHsCase, nlList,
  mkLHsTupleExpr, mkLHsVarTuple, missingTupArg,
  mkLocatedList, nlAscribe,

  -- * Bindings
  mkFunBind, mkVarBind, mkHsVarBind, mkSimpleGeneratedFunBind, mkTopFunBind,
  mkPatSynBind,
  isInfixFunBind,
  spanHsLocaLBinds,

  -- * Literals
  mkHsIntegral, mkHsFractional, mkHsIsString, mkHsString, mkHsStringFS, mkHsStringPrimLit,
  mkHsCharPrimLit,

  -- * Patterns
  mkNPat, mkNPlusKPat, nlVarPat, nlLitPat, nlConVarPat, nlConVarPatName, nlConPat,
  nlConPatName, nlInfixConPat, nlNullaryConPat, nlWildConPat, nlWildPat,
  nlWildPatName, nlTuplePat, mkParPat, nlParPat,
  mkBigLHsVarTup, mkBigLHsTup, mkBigLHsVarPatTup, mkBigLHsPatTup,

  -- * Types
  mkHsAppTy, mkHsAppKindTy,
  hsTypeToHsSigType, hsTypeToHsSigWcType, mkClassOpSigs, mkHsSigEnv,
  nlHsAppTy, nlHsAppKindTy, nlHsTyVar, nlHsFunTy, nlHsParTy, nlHsTyConApp,

  -- * Stmts
  mkTransformStmt, mkTransformByStmt, mkBodyStmt,
  mkPsBindStmt, mkRnBindStmt, mkTcBindStmt,
  mkLastStmt,
  emptyTransStmt, mkGroupUsingStmt, mkGroupByUsingStmt,
  emptyRecStmt, emptyRecStmtName, emptyRecStmtId, mkRecStmt,
  unitRecStmtTc,
  mkLetStmt,

  -- * Collecting binders
  isUnliftedHsBind, isUnliftedHsBinds, isBangedHsBind,

  collectLocalBinders, collectHsValBinders, collectHsBindListBinders,
  collectHsIdBinders,
  collectHsBindsBinders, collectHsBindBinders, collectMethodBinders,

  collectPatBinders, collectPatsBinders,
  collectLStmtsBinders, collectStmtsBinders,
  collectLStmtBinders, collectStmtBinders,
  CollectPass(..), CollectFlag(..),

  TyDeclBinders(..), LConsWithFields(..),
  hsLTyClDeclBinders, hsTyClForeignBinders,
  hsPatSynSelectors, getPatSynBinds,
  hsForeignDeclsBinders, hsGroupBinders, hsDataFamInstBinders,

  -- * Collecting implicit binders
  ImplicitFieldBinders(..),
  lStmtsImplicits, hsValBindsImplicits, lPatImplicits,
  lHsRecFieldsImplicits
  ) where

import GHC.Prelude hiding (head, init, last, tail)

import GHC.Hs.Decls
import GHC.Hs.Binds
import GHC.Hs.Expr
import GHC.Hs.Pat
import GHC.Hs.Type
import GHC.Hs.Lit
import Language.Haskell.Syntax.Decls
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Parser.Annotation

import GHC.Tc.Types.Evidence

import GHC.Core.Coercion( isReflCo )
import GHC.Core.Multiplicity ( pattern ManyTy )
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Core.Make   ( mkChunkified )
import GHC.Core.Type   ( Type, isUnliftedType )

import GHC.Builtin.Types ( unitTy )

import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Set hiding ( unitFV )
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
import GHC.Types.Var
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Fixity
import GHC.Types.SourceText

import GHC.Data.FastString

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.Arrow ( first )
import Data.Foldable ( toList )
import Data.List ( partition )
import Data.List.NonEmpty ( nonEmpty )
import qualified Data.List.NonEmpty as NE

import Data.IntMap ( IntMap )
import qualified Data.IntMap.Strict as IntMap
import Data.Map ( Map )
import qualified Data.Map.Strict as Map

{-
************************************************************************
*                                                                      *
        Some useful helpers for constructing syntax
*                                                                      *
************************************************************************

These functions attempt to construct a not-completely-useless 'SrcSpan'
from their components, compared with the @nl*@ functions below which
just attach 'noSrcSpan' to everything.
-}

-- | @e => (e)@
mkHsPar :: IsPass p => LHsExpr (GhcPass p) -> LHsExpr (GhcPass p)
mkHsPar e = L (getLoc e) (gHsPar e)

mkSimpleMatch :: (Anno (Match (GhcPass p) (LocatedA (body (GhcPass p))))
                        ~ SrcSpanAnnA,
                  Anno (GRHS (GhcPass p) (LocatedA (body (GhcPass p))))
                        ~ EpAnn NoEpAnns)
              => HsMatchContext (LIdP (NoGhcTc (GhcPass p)))
              -> LocatedE [LPat (GhcPass p)] -> LocatedA (body (GhcPass p))
              -> LMatch (GhcPass p) (LocatedA (body (GhcPass p)))
mkSimpleMatch ctxt (L l pats) rhs
  = L loc $
    Match { m_ext = noExtField, m_ctxt = ctxt, m_pats = L l pats
          , m_grhss = unguardedGRHSs (locA loc) rhs noAnn }
  where
    loc = case pats of
                []      -> getLoc rhs
                (pat:_) -> combineSrcSpansA (getLoc pat) (getLoc rhs)

unguardedGRHSs :: Anno (GRHS (GhcPass p) (LocatedA (body (GhcPass p))))
                     ~ EpAnn NoEpAnns
               => SrcSpan -> LocatedA (body (GhcPass p)) -> EpAnn GrhsAnn
               -> GRHSs (GhcPass p) (LocatedA (body (GhcPass p)))
unguardedGRHSs loc rhs an
  = GRHSs emptyComments (unguardedRHS an loc rhs) emptyLocalBinds

unguardedRHS :: Anno (GRHS (GhcPass p) (LocatedA (body (GhcPass p))))
                     ~ EpAnn NoEpAnns
             => EpAnn GrhsAnn -> SrcSpan -> LocatedA (body (GhcPass p))
             -> [LGRHS (GhcPass p) (LocatedA (body (GhcPass p)))]
unguardedRHS an loc rhs = [L (noAnnSrcSpan loc) (GRHS an [] rhs)]

type AnnoBody p body
  = ( XMG (GhcPass p) (LocatedA (body (GhcPass p))) ~ Origin
    , Anno [LocatedA (Match (GhcPass p) (LocatedA (body (GhcPass p))))] ~ SrcSpanAnnLW
    , Anno (Match (GhcPass p) (LocatedA (body (GhcPass p)))) ~ SrcSpanAnnA
    )

mkMatchGroup :: AnnoBody p body
             => Origin
             -> LocatedLW [LocatedA (Match (GhcPass p) (LocatedA (body (GhcPass p))))]
             -> MatchGroup (GhcPass p) (LocatedA (body (GhcPass p)))
mkMatchGroup origin matches = MG { mg_ext = origin
                                 , mg_alts = matches }

mkLamCaseMatchGroup :: AnnoBody p body
                    => Origin
                    -> HsLamVariant
                    -> LocatedLW [LocatedA (Match (GhcPass p) (LocatedA (body (GhcPass p))))]
                    -> MatchGroup (GhcPass p) (LocatedA (body (GhcPass p)))
mkLamCaseMatchGroup origin lam_variant (L l matches)
  = mkMatchGroup origin (L l $ map fixCtxt matches)
  where fixCtxt (L a match) = L a match{m_ctxt = LamAlt lam_variant}

mkLocatedList :: (Semigroup a, NoAnn an)
  => [GenLocated (EpAnn a) e2] -> LocatedAn an [GenLocated (EpAnn a) e2]
mkLocatedList ms = case nonEmpty ms of
    Nothing -> noLocA []
    Just ms1 -> L (noAnnSrcSpan $ locA $ combineLocsA (NE.head ms1) (NE.last ms1)) ms

mkHsApp :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
mkHsApp e1 e2 = addCLocA e1 e2 (HsApp noExtField e1 e2)

mkHsAppWith
  :: (LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> HsExpr (GhcPass id) -> LHsExpr (GhcPass id))
  -> LHsExpr (GhcPass id)
  -> LHsExpr (GhcPass id)
  -> LHsExpr (GhcPass id)
mkHsAppWith mkLocated e1 e2 = mkLocated e1 e2 (HsApp noExtField e1 e2)

mkHsApps
  :: LHsExpr (GhcPass id) -> [LHsExpr (GhcPass id)] -> LHsExpr (GhcPass id)
mkHsApps = mkHsAppsWith addCLocA

mkHsAppsWith
 :: (LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> HsExpr (GhcPass id) -> LHsExpr (GhcPass id))
 -> LHsExpr (GhcPass id)
 -> [LHsExpr (GhcPass id)]
 -> LHsExpr (GhcPass id)
mkHsAppsWith mkLocated = foldl' (mkHsAppWith mkLocated)

mkHsAppType :: LHsExpr GhcRn -> LHsWcType GhcRn -> LHsExpr GhcRn
mkHsAppType e t = addCLocA t_body e (HsAppType noExtField e paren_wct)
  where
    t_body    = hswc_body t
    paren_wct = t { hswc_body = t_body }

mkHsAppTypes :: LHsExpr GhcRn -> [LHsWcType GhcRn] -> LHsExpr GhcRn
mkHsAppTypes = foldl' mkHsAppType

mkHsLam :: (IsPass p, XMG (GhcPass p) (LHsExpr (GhcPass p)) ~ Origin)
        => LocatedE [LPat (GhcPass p)]
        -> LHsExpr (GhcPass p)
        -> LHsExpr (GhcPass p)
mkHsLam (L l pats) body = mkHsPar (L (getLoc body) (HsLam noAnn LamSingle matches))
  where
    matches = mkMatchGroup (Generated OtherExpansion SkipPmc)
                           (noLocA [mkSimpleMatch (LamAlt LamSingle) (L l pats') body])
    pats' = map (parenthesizePat appPrec) pats

mkHsLams :: [TyVar] -> [EvVar] -> LHsExpr GhcTc -> LHsExpr GhcTc
mkHsLams tyvars dicts expr = mkLHsWrap (mkWpTyLams tyvars
                                       <.> mkWpEvLams dicts) expr

mkHsSyntaxApps :: SrcSpanAnnA -> SyntaxExprTc -> [LHsExpr GhcTc]
               -> LHsExpr GhcTc
mkHsSyntaxApps ann (SyntaxExprTc { syn_expr      = fun
                                 , syn_arg_wraps = arg_wraps
                                 , syn_res_wrap  = res_wrap }) args
  = mkLHsWrap res_wrap (foldl' mkHsApp (L ann fun) (zipWithEqual "mkHsSyntaxApps"
                                                     mkLHsWrap arg_wraps args))
mkHsSyntaxApps _ NoSyntaxExprTc args = pprPanic "mkHsSyntaxApps" (ppr args)
  -- this function should never be called in scenarios where there is no
  -- syntax expr

-- |A simple case alternative with a single pattern, no binds, no guards;
-- pre-typechecking
mkHsCaseAlt :: (Anno (GRHS (GhcPass p) (LocatedA (body (GhcPass p))))
                     ~ EpAnn NoEpAnns,
                 Anno (Match (GhcPass p) (LocatedA (body (GhcPass p))))
                        ~ SrcSpanAnnA)
            => LPat (GhcPass p) -> (LocatedA (body (GhcPass p)))
            -> LMatch (GhcPass p) (LocatedA (body (GhcPass p)))
mkHsCaseAlt (L l pat) expr
  = mkSimpleMatch CaseAlt (L (l2l l) [L l pat]) expr

nlHsTyApp :: Id -> [Type] -> LHsExpr GhcTc
nlHsTyApp fun_id tys
  = noLocA (mkHsWrap (mkWpTyApps tys) (HsVar noExtField (noLocA fun_id)))

nlHsTyApps :: Id -> [Type] -> [LHsExpr GhcTc] -> LHsExpr GhcTc
nlHsTyApps fun_id tys xs = foldl' nlHsApp (nlHsTyApp fun_id tys) xs

--------- Adding parens ---------
-- | Wrap in parens if @'hsExprNeedsParens' appPrec@ says it needs them
-- So @f x@ becomes @(f x)@, but @3@ stays as @3@.
mkLHsPar :: IsPass id => LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
mkLHsPar = parenthesizeHsExpr appPrec

mkParPat :: IsPass p => LPat (GhcPass p) -> LPat (GhcPass p)
mkParPat = parenthesizePat appPrec

nlParPat :: IsPass p => LPat (GhcPass p) -> LPat (GhcPass p)
nlParPat p = noLocA (gParPat p)

-------------------------------
-- These are the bits of syntax that contain rebindable names
-- See GHC.Rename.Env.lookupSyntax

mkHsIntegral   :: IntegralLit -> HsOverLit GhcPs
mkHsFractional :: FractionalLit -> HsOverLit GhcPs
mkHsIsString   :: SourceText -> FastString -> HsOverLit GhcPs
mkHsDo         :: HsDoFlavour -> LocatedLW [ExprLStmt GhcPs] -> HsExpr GhcPs
mkHsDoAnns     :: HsDoFlavour -> LocatedLW [ExprLStmt GhcPs] -> AnnList EpaLocation -> HsExpr GhcPs
mkHsComp       :: HsDoFlavour -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
               -> HsExpr GhcPs
mkHsCompAnns   :: HsDoFlavour -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
               -> AnnList EpaLocation
               -> HsExpr GhcPs

mkNPat      :: LocatedAn NoEpAnns (HsOverLit GhcPs) -> Maybe (SyntaxExpr GhcPs) -> EpToken "-"
            -> Pat GhcPs
mkNPlusKPat :: LocatedN RdrName -> LocatedAn NoEpAnns (HsOverLit GhcPs) -> EpaLocation
            -> Pat GhcPs

-- NB: The following functions all use noSyntaxExpr: the generated expressions
--     will not work with rebindable syntax if used after the renamer
mkLastStmt :: IsPass idR => LocatedA (bodyR (GhcPass idR))
           -> StmtLR (GhcPass idL) (GhcPass idR) (LocatedA (bodyR (GhcPass idR)))
mkBodyStmt :: LocatedA (bodyR GhcPs)
           -> StmtLR (GhcPass idL) GhcPs (LocatedA (bodyR GhcPs))
mkPsBindStmt :: EpUniToken "<-" "←" -> LPat GhcPs -> LocatedA (bodyR GhcPs)
             -> StmtLR GhcPs GhcPs (LocatedA (bodyR GhcPs))
mkRnBindStmt :: LPat GhcRn -> LocatedA (bodyR GhcRn)
             -> StmtLR GhcRn GhcRn (LocatedA (bodyR GhcRn))
mkTcBindStmt :: LPat GhcTc -> LocatedA (bodyR GhcTc)
             -> StmtLR GhcTc GhcTc (LocatedA (bodyR GhcTc))

emptyRecStmt     :: (Anno [GenLocated
                             (Anno (StmtLR (GhcPass idL) GhcPs bodyR))
                             (StmtLR (GhcPass idL) GhcPs bodyR)]
                        ~ SrcSpanAnnLW)
                 => StmtLR (GhcPass idL) GhcPs bodyR
emptyRecStmtName :: (Anno [GenLocated
                             (Anno (StmtLR GhcRn GhcRn bodyR))
                             (StmtLR GhcRn GhcRn bodyR)]
                        ~ SrcSpanAnnLW)
                 => StmtLR GhcRn GhcRn bodyR
emptyRecStmtId   :: Stmt GhcTc (LocatedA (HsCmd GhcTc))

mkRecStmt :: forall (idL :: Pass) bodyR.
                    (Anno [GenLocated
                             (Anno (StmtLR (GhcPass idL) GhcPs bodyR))
                             (StmtLR (GhcPass idL) GhcPs bodyR)]
                        ~ SrcSpanAnnLW)
                 => AnnList (EpToken "rec")
                 -> LocatedLW [LStmtLR (GhcPass idL) GhcPs bodyR]
                 -> StmtLR (GhcPass idL) GhcPs bodyR
mkRecStmt anns stmts  = (emptyRecStmt' anns :: StmtLR (GhcPass idL) GhcPs bodyR)
                             { recS_stmts = stmts }


mkHsIntegral     i  = OverLit noExtField (HsIntegral       i)
mkHsFractional   f  = OverLit noExtField (HsFractional     f)
mkHsIsString src s  = OverLit noExtField (HsIsString   src s)

mkHsDo     ctxt stmts      = HsDo noAnn ctxt stmts
mkHsDoAnns ctxt stmts anns = HsDo anns  ctxt stmts
mkHsComp ctxt stmts expr = mkHsCompAnns ctxt stmts expr noAnn
mkHsCompAnns ctxt stmts expr@(L l e) anns = mkHsDoAnns ctxt (L loc (stmts ++ [last_stmt])) anns
  where
    -- Move the annotations to the top of the last_stmt
    last = mkLastStmt (L (noAnnSrcSpan $ getLocA expr) e)
    last_stmt = L l last
    -- last_stmt actually comes first in a list comprehension, consider all spans
    loc  = noAnnSrcSpan $ getHasLocList (last_stmt:stmts)

-- restricted to GhcPs because other phases might need a SyntaxExpr
mkHsIf :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs -> AnnsIf
       -> HsExpr GhcPs
mkHsIf c a b anns = HsIf anns c a b

-- restricted to GhcPs because other phases might need a SyntaxExpr
mkHsCmdIf :: LHsExpr GhcPs -> LHsCmd GhcPs -> LHsCmd GhcPs -> AnnsIf
       -> HsCmd GhcPs
mkHsCmdIf c a b anns = HsCmdIf anns noSyntaxExpr c a b

mkNPat lit neg anns  = NPat anns lit neg noSyntaxExpr
mkNPlusKPat id lit anns
  = NPlusKPat anns id lit (unLoc lit) noSyntaxExpr noSyntaxExpr

mkTransformStmt    :: AnnTransStmt -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkTransformByStmt  :: AnnTransStmt -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> LHsExpr GhcPs -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkGroupUsingStmt   :: AnnTransStmt -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkGroupByUsingStmt :: AnnTransStmt -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> LHsExpr GhcPs
                   -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)

emptyTransStmt :: AnnTransStmt -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
emptyTransStmt anns = TransStmt { trS_ext = anns
                                , trS_form = panic "emptyTransStmt: form"
                                , trS_stmts = [], trS_bndrs = []
                                , trS_by = Nothing, trS_using = noLocA noExpr
                                , trS_ret = noSyntaxExpr, trS_bind = noSyntaxExpr
                                , trS_fmap = noExpr }
mkTransformStmt    a ss u   = (emptyTransStmt a) { trS_form = ThenForm,  trS_stmts = ss, trS_using = u }
mkTransformByStmt  a ss u b = (emptyTransStmt a) { trS_form = ThenForm,  trS_stmts = ss, trS_using = u, trS_by = Just b }
mkGroupUsingStmt   a ss u   = (emptyTransStmt a) { trS_form = GroupForm, trS_stmts = ss, trS_using = u }
mkGroupByUsingStmt a ss b u = (emptyTransStmt a) { trS_form = GroupForm, trS_stmts = ss, trS_using = u, trS_by = Just b }

mkLastStmt body = LastStmt noExtField body Nothing noSyntaxExpr
mkBodyStmt body
  = BodyStmt noExtField body noSyntaxExpr noSyntaxExpr
mkPsBindStmt ann pat body = BindStmt ann pat body
mkRnBindStmt pat body = BindStmt (XBindStmtRn { xbsrn_bindOp = noSyntaxExpr, xbsrn_failOp = Nothing }) pat body
mkTcBindStmt pat body = BindStmt (XBindStmtTc { xbstc_bindOp = noSyntaxExpr,
                                                xbstc_boundResultType = unitTy,
                                                   -- unitTy is a dummy value
                                                   -- can't panic here: it's forced during zonking
                                                xbstc_boundResultMult = ManyTy,
                                                xbstc_failOp = Nothing }) pat body

emptyRecStmt' :: forall idL idR body .
  (WrapXRec (GhcPass idR) [LStmtLR (GhcPass idL) (GhcPass idR) body], IsPass idR)
              => XRecStmt (GhcPass idL) (GhcPass idR) body
              -> StmtLR (GhcPass idL) (GhcPass idR) body
emptyRecStmt' tyVal =
   RecStmt
     { recS_stmts = wrapXRec @(GhcPass idR) []
     , recS_later_ids = []
     , recS_rec_ids = []
     , recS_ret_fn = noSyntaxExpr
     , recS_mfix_fn = noSyntaxExpr
     , recS_bind_fn = noSyntaxExpr
     , recS_ext = tyVal }

unitRecStmtTc :: RecStmtTc
unitRecStmtTc = RecStmtTc { recS_bind_ty = unitTy
                          , recS_later_rets = []
                          , recS_rec_rets = []
                          , recS_ret_ty = unitTy }

emptyRecStmt     = emptyRecStmt' noAnn
emptyRecStmtName = emptyRecStmt' noExtField
emptyRecStmtId   = emptyRecStmt' unitRecStmtTc
                                        -- a panic might trigger during zonking

mkLetStmt :: EpToken "let" -> HsLocalBinds GhcPs -> StmtLR GhcPs GhcPs (LocatedA b)
mkLetStmt anns binds = LetStmt anns binds

-------------------------------
-- | A useful function for building @OpApps@.  The operator is always a
-- variable, and we don't know the fixity yet.
mkHsOpApp :: LHsExpr GhcPs -> IdP GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
mkHsOpApp e1 op e2 = OpApp noExtField e1 (noLocA (HsVar noExtField (noLocA op))) e2

mkHsString :: String -> HsLit (GhcPass p)
mkHsString s = HsString NoSourceText (mkFastString s)

mkHsStringFS :: FastString -> HsLit (GhcPass p)
mkHsStringFS s = HsString NoSourceText s

mkHsStringPrimLit :: FastString -> HsLit (GhcPass p)
mkHsStringPrimLit fs = HsStringPrim NoSourceText (bytesFS fs)

mkHsCharPrimLit :: Char -> HsLit (GhcPass p)
mkHsCharPrimLit c = HsChar NoSourceText c

mkConLikeTc :: ConLike -> HsExpr GhcTc
mkConLikeTc con = XExpr (ConLikeTc con [] [])

{-
************************************************************************
*                                                                      *
        Constructing syntax with no location info
*                                                                      *
************************************************************************
-}

nlHsVar :: IsSrcSpanAnn p a
        => IdP (GhcPass p) -> LHsExpr (GhcPass p)
nlHsVar n = noLocA (HsVar noExtField (noLocA n))

nl_HsVar :: IsSrcSpanAnn p a
        => IdP (GhcPass p) -> HsExpr (GhcPass p)
nl_HsVar n = HsVar noExtField (noLocA n)

-- | NB: Only for 'LHsExpr' 'Id'.
nlHsDataCon :: DataCon -> LHsExpr GhcTc
nlHsDataCon con = noLocA (mkConLikeTc (RealDataCon con))

nlHsLit :: HsLit (GhcPass p) -> LHsExpr (GhcPass p)
nlHsLit n = noLocA (HsLit noExtField n)

nlHsIntLit :: Integer -> LHsExpr (GhcPass p)
nlHsIntLit n = noLocA (HsLit noExtField (HsInt noExtField (mkIntegralLit n)))

nlVarPat :: IsSrcSpanAnn p a
        => IdP (GhcPass p) -> LPat (GhcPass p)
nlVarPat n = noLocA (VarPat noExtField (noLocA n))

nlLitPat :: HsLit GhcPs -> LPat GhcPs
nlLitPat l = noLocA (LitPat noExtField l)

nlHsApp :: IsPass id => LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
nlHsApp f x = noLocA (HsApp noExtField f (mkLHsPar x))

nlHsSyntaxApps :: SyntaxExprTc -> [LHsExpr GhcTc]
               -> LHsExpr GhcTc
nlHsSyntaxApps = mkHsSyntaxApps noSrcSpanA

nlHsApps :: IsSrcSpanAnn p a
         => IdP (GhcPass p) -> [LHsExpr (GhcPass p)] -> LHsExpr (GhcPass p)
nlHsApps f xs = foldl' nlHsApp (nlHsVar f) xs

nlHsVarApps :: IsSrcSpanAnn p a
            => IdP (GhcPass p) -> [IdP (GhcPass p)] -> LHsExpr (GhcPass p)
nlHsVarApps f xs = noLocA (foldl' mk (HsVar noExtField (noLocA f))
                                         (map ((HsVar noExtField) . noLocA) xs))
                 where
                   mk f a = HsApp noExtField (noLocA f) (noLocA a)

nlConVarPat :: RdrName -> [RdrName] -> LPat GhcPs
nlConVarPat con vars = nlConPat con (map nlVarPat vars)

nlConVarPatName :: Name -> [Name] -> LPat GhcRn
nlConVarPatName con vars = nlConPatName con (map nlVarPat vars)

nlInfixConPat :: RdrName -> LPat GhcPs -> LPat GhcPs -> LPat GhcPs
nlInfixConPat con l r = noLocA $ ConPat
  { pat_con = noLocA con
  , pat_args = InfixCon (parenthesizePat opPrec l)
                        (parenthesizePat opPrec r)
  , pat_con_ext = noAnn
  }

nlConPat :: RdrName -> [LPat GhcPs] -> LPat GhcPs
nlConPat con pats = noLocA $ ConPat
  { pat_con_ext = noAnn
  , pat_con = noLocA con
  , pat_args = PrefixCon [] (map (parenthesizePat appPrec) pats)
  }

nlConPatName :: Name -> [LPat GhcRn] -> LPat GhcRn
nlConPatName con pats = noLocA $ ConPat
  { pat_con_ext = noExtField
  , pat_con = noLocA con
  , pat_args = PrefixCon [] (map (parenthesizePat appPrec) pats)
  }

nlNullaryConPat :: RdrName -> LPat GhcPs
nlNullaryConPat con = noLocA $ ConPat
  { pat_con_ext = noAnn
  , pat_con = noLocA con
  , pat_args = PrefixCon [] []
  }

nlWildConPat :: DataCon -> LPat GhcPs
nlWildConPat con = noLocA $ ConPat
  { pat_con_ext = noAnn
  , pat_con = noLocA $ getRdrName con
  , pat_args = PrefixCon [] $
     replicate (dataConSourceArity con)
               nlWildPat
  }

-- | Wildcard pattern - after parsing
nlWildPat :: LPat GhcPs
nlWildPat  = noLocA (WildPat noExtField )

-- | Wildcard pattern - after renaming
nlWildPatName :: LPat GhcRn
nlWildPatName  = noLocA (WildPat noExtField )

nlHsDo :: HsDoFlavour -> [LStmt GhcPs (LHsExpr GhcPs)]
       -> LHsExpr GhcPs
nlHsDo ctxt stmts = noLocA (mkHsDo ctxt (noLocA stmts))

nlHsOpApp :: LHsExpr GhcPs -> IdP GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
nlHsOpApp e1 op e2 = noLocA (mkHsOpApp e1 op e2)

nlHsLam  :: LMatch GhcPs (LHsExpr GhcPs) -> LHsExpr GhcPs
nlHsPar  :: IsPass p => LHsExpr (GhcPass p) -> LHsExpr (GhcPass p)
nlHsCase :: LHsExpr GhcPs -> [LMatch GhcPs (LHsExpr GhcPs)]
         -> LHsExpr GhcPs
nlList   :: [LHsExpr GhcPs] -> LHsExpr GhcPs

-- AZ:Is this used?
nlHsLam match = noLocA $ HsLam noAnn LamSingle
                  $ mkMatchGroup (Generated OtherExpansion SkipPmc) (noLocA [match])

nlHsPar e     = noLocA (gHsPar e)

-- nlHsIf should generate if-expressions which are NOT subject to
-- RebindableSyntax, so the first field of HsIf is False. (#12080)
nlHsIf :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
nlHsIf cond true false = noLocA (HsIf noAnn cond true false)

nlHsCase expr matches
  = noLocA (HsCase noAnn expr (mkMatchGroup (Generated OtherExpansion SkipPmc) (noLocA matches)))
nlList exprs          = noLocA (ExplicitList noAnn exprs)

nlHsAppTy :: LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
nlHsTyVar :: IsSrcSpanAnn p a
          => PromotionFlag -> IdP (GhcPass p)           -> LHsType (GhcPass p)
nlHsFunTy :: forall p. IsPass p
          => LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
nlHsParTy :: LHsType (GhcPass p)                        -> LHsType (GhcPass p)

nlHsAppTy f t = noLocA (HsAppTy noExtField f t)
nlHsTyVar p x = noLocA (HsTyVar noAnn p (noLocA x))
nlHsFunTy a b = noLocA (HsFunTy noExtField (HsUnrestrictedArrow x) a b)
  where
    x = case ghcPass @p of
      GhcPs -> noAnn
      GhcRn -> noExtField
      GhcTc -> noExtField
nlHsParTy t   = noLocA (HsParTy noAnn t)

nlHsTyConApp :: forall p a. IsSrcSpanAnn p a
             => PromotionFlag
             -> LexicalFixity -> IdP (GhcPass p)
             -> [LHsTypeArg (GhcPass p)] -> LHsType (GhcPass p)
nlHsTyConApp prom fixity tycon tys
  | Infix <- fixity
  , HsValArg _ ty1 : HsValArg _ ty2 : rest <- tys
  = foldl' mk_app (noLocA $ HsOpTy noExtField prom ty1 (noLocA tycon) ty2) rest
  | otherwise
  = foldl' mk_app (nlHsTyVar prom tycon) tys
  where
    mk_app :: LHsType (GhcPass p) -> LHsTypeArg (GhcPass p) -> LHsType (GhcPass p)
    mk_app fun@(L _ (HsOpTy {})) arg = mk_app (nlHsParTy fun) arg
      -- parenthesize things like `(A + B) C`
    mk_app fun (HsValArg _ ty) = nlHsAppTy fun ty
    mk_app fun (HsTypeArg _ ki) = nlHsAppKindTy fun ki
    mk_app fun (HsArgPar _) = nlHsParTy fun

nlHsAppKindTy :: forall p. IsPass p =>
  LHsType (GhcPass p) -> LHsKind (GhcPass p) -> LHsType (GhcPass p)
nlHsAppKindTy f k = noLocA (HsAppKindTy x f k)
  where
    x = case ghcPass @p of
      GhcPs -> noAnn
      GhcRn -> noExtField
      GhcTc -> noExtField

{-
Tuples.  All these functions are *pre-typechecker* because they lack
types on the tuple.
-}

mkLHsTupleExpr :: [LHsExpr (GhcPass p)] -> XExplicitTuple (GhcPass p)
               -> LHsExpr (GhcPass p)
-- Makes a pre-typechecker boxed tuple, deals with 1 case
mkLHsTupleExpr [e] _ = e
mkLHsTupleExpr es ext
  = noLocA $ ExplicitTuple ext (map (Present noExtField) es) Boxed

mkLHsVarTuple :: IsSrcSpanAnn p a
               => [IdP (GhcPass p)]  -> XExplicitTuple (GhcPass p)
              -> LHsExpr (GhcPass p)
mkLHsVarTuple ids ext = mkLHsTupleExpr (map nlHsVar ids) ext

nlTuplePat :: [LPat GhcPs] -> Boxity -> LPat GhcPs
nlTuplePat pats box = noLocA (TuplePat noAnn pats box)

missingTupArg :: EpAnn Bool -> HsTupArg GhcPs
missingTupArg ann = Missing ann

mkLHsPatTup :: [LPat GhcRn] -> LPat GhcRn
mkLHsPatTup []     = noLocA $ TuplePat noExtField [] Boxed
mkLHsPatTup [lpat] = lpat
mkLHsPatTup lpats@(lpat:_) = L (getLoc lpat) $ TuplePat noExtField lpats Boxed

-- | The Big equivalents for the source tuple expressions
mkBigLHsVarTup :: IsSrcSpanAnn p a
               => [IdP (GhcPass p)] -> XExplicitTuple (GhcPass p)
               -> LHsExpr (GhcPass p)
mkBigLHsVarTup ids anns = mkBigLHsTup (map nlHsVar ids) anns

mkBigLHsTup :: [LHsExpr (GhcPass id)] -> XExplicitTuple (GhcPass id)
            -> LHsExpr (GhcPass id)
mkBigLHsTup es anns = mkChunkified (\e -> mkLHsTupleExpr e anns) es

-- | The Big equivalents for the source tuple patterns
mkBigLHsVarPatTup :: [IdP GhcRn] -> LPat GhcRn
mkBigLHsVarPatTup bs = mkBigLHsPatTup (map nlVarPat bs)

mkBigLHsPatTup :: [LPat GhcRn] -> LPat GhcRn
mkBigLHsPatTup = mkChunkified mkLHsPatTup

{-
************************************************************************
*                                                                      *
        LHsSigType and LHsSigWcType
*                                                                      *
********************************************************************* -}

-- | Convert an 'LHsType' to an 'LHsSigType'.
hsTypeToHsSigType :: LHsType GhcPs -> LHsSigType GhcPs
hsTypeToHsSigType lty@(L loc ty) = case ty of
  HsForAllTy { hst_tele = HsForAllInvis { hsf_xinvis = an
                                        , hsf_invis_bndrs = bndrs }
             , hst_body = body }
    -> L loc $ mkHsExplicitSigType an bndrs body
  _ -> L (l2l loc) $ mkHsImplicitSigType lty -- The annotations are in lty, erase them from loc

-- | Convert an 'LHsType' to an 'LHsSigWcType'.
hsTypeToHsSigWcType :: LHsType GhcPs -> LHsSigWcType GhcPs
hsTypeToHsSigWcType = mkHsWildCardBndrs . hsTypeToHsSigType

mkHsSigEnv :: forall a. (LSig GhcRn -> Maybe ([LocatedN Name], a))
                     -> [LSig GhcRn]
                     -> NameEnv a
mkHsSigEnv get_info sigs
  = mkNameEnv          (mk_pairs ordinary_sigs)
   `extendNameEnvList` (mk_pairs gen_dm_sigs)
   -- The subtlety is this: in a class decl with a
   -- default-method signature as well as a method signature
   -- we want the latter to win (#12533)
   --    class C x where
   --       op :: forall a . x a -> x a
   --       default op :: forall b . x b -> x b
   --       op x = ...(e :: b -> b)...
   -- The scoped type variables of the 'default op', namely 'b',
   -- scope over the code for op.   The 'forall a' does not!
   -- This applies both in the renamer and typechecker, both
   -- of which use this function
  where
    (gen_dm_sigs, ordinary_sigs) = partition is_gen_dm_sig sigs
    is_gen_dm_sig (L _ (ClassOpSig _ True _ _)) = True
    is_gen_dm_sig _                             = False

    mk_pairs :: [LSig GhcRn] -> [(Name, a)]
    mk_pairs sigs = [ (n,a) | Just (ns,a) <- map get_info sigs
                            , L _ n <- ns ]

mkClassOpSigs :: [LSig GhcPs] -> [LSig GhcPs]
-- ^ Convert 'TypeSig' to 'ClassOpSig'.
-- The former is what is parsed, but the latter is
-- what we need in class/instance declarations
mkClassOpSigs sigs
  = map fiddle sigs
  where
    fiddle (L loc (TypeSig anns nms ty))
      = L loc (ClassOpSig anns False nms (dropWildCards ty))
    fiddle sig = sig


-- | Type ascription: (e :: ty)
nlAscribe :: RdrName -> LHsExpr GhcPs -> LHsExpr GhcPs
nlAscribe ty e = noLocA $ ExprWithTySig noAnn e
                           $ mkHsWildCardBndrs $ noLocA $ mkHsImplicitSigType
                           $ nlHsTyVar NotPromoted ty

{- *********************************************************************
*                                                                      *
    --------- HsWrappers: type args, dict args, casts ---------
*                                                                      *
********************************************************************* -}

mkLHsWrap :: HsWrapper -> LHsExpr GhcTc -> LHsExpr GhcTc
mkLHsWrap co_fn (L loc e) = L loc (mkHsWrap co_fn e)

mkHsWrap :: HsWrapper -> HsExpr GhcTc -> HsExpr GhcTc
mkHsWrap co_fn e | isIdHsWrapper co_fn = e
mkHsWrap co_fn e                       = XExpr (WrapExpr co_fn e)

mkHsWrapCo :: TcCoercionN   -- A Nominal coercion  a ~N b
           -> HsExpr GhcTc -> HsExpr GhcTc
mkHsWrapCo co e = mkHsWrap (mkWpCastN co) e

mkHsWrapCoR :: TcCoercionR   -- A Representational coercion  a ~R b
            -> HsExpr GhcTc -> HsExpr GhcTc
mkHsWrapCoR co e = mkHsWrap (mkWpCastR co) e

mkLHsWrapCo :: TcCoercionN -> LHsExpr GhcTc -> LHsExpr GhcTc
mkLHsWrapCo co (L loc e) = L loc (mkHsWrapCo co e)

mkHsCmdWrap :: HsWrapper -> HsCmd GhcTc -> HsCmd GhcTc
mkHsCmdWrap w cmd | isIdHsWrapper w = cmd
                  | otherwise       = XCmd (HsWrap w cmd)

mkLHsCmdWrap :: HsWrapper -> LHsCmd GhcTc -> LHsCmd GhcTc
mkLHsCmdWrap w (L loc c) = L loc (mkHsCmdWrap w c)

mkHsWrapPat :: HsWrapper -> Pat GhcTc -> Type -> Pat GhcTc
mkHsWrapPat co_fn p ty | isIdHsWrapper co_fn = p
                       | otherwise           = XPat $ CoPat co_fn p ty

mkLHsWrapPat :: HsWrapper -> LPat GhcTc -> Type -> LPat GhcTc
mkLHsWrapPat co_fn (L loc p) ty = L loc (mkHsWrapPat co_fn p ty)

mkHsWrapPatCo :: TcCoercionN -> Pat GhcTc -> Type -> Pat GhcTc
mkHsWrapPatCo co pat ty | isReflCo co = pat
                        | otherwise     = XPat $ CoPat (mkWpCastN co) pat ty

mkHsDictLet :: TcEvBinds -> LHsExpr GhcTc -> LHsExpr GhcTc
mkHsDictLet ev_binds expr = mkLHsWrap (mkWpLet ev_binds) expr

{-
l
************************************************************************
*                                                                      *
                Bindings; with a location at the top
*                                                                      *
************************************************************************
-}

mkFunBind :: Origin -> LocatedN RdrName -> [LMatch GhcPs (LHsExpr GhcPs)]
          -> HsBind GhcPs
-- ^ Not infix, with place holders for coercion and free vars
mkFunBind origin fn ms
  = FunBind { fun_id = fn
            , fun_matches = mkMatchGroup origin (noLocA ms)
            , fun_ext = noExtField
            }

mkTopFunBind :: Origin -> LocatedN Name -> [LMatch GhcRn (LHsExpr GhcRn)]
             -> HsBind GhcRn
-- ^ In Name-land, with empty bind_fvs
mkTopFunBind origin fn ms = FunBind { fun_id = fn
                                    , fun_matches = mkMatchGroup origin (noLocA ms)
                                    , fun_ext  = emptyNameSet -- NB: closed
                                                              --     binding
                                    }

mkHsVarBind :: SrcSpan -> RdrName -> LHsExpr GhcPs -> LHsBind GhcPs
mkHsVarBind loc var rhs = mkSimpleGeneratedFunBind loc var (noLocA []) rhs

mkVarBind :: IdP GhcTc -> LHsExpr GhcTc -> LHsBind GhcTc
mkVarBind var rhs = L (getLoc rhs) $
                    VarBind { var_ext = noExtField,
                              var_id = var, var_rhs = rhs }

mkPatSynBind :: LocatedN RdrName -> HsPatSynDetails GhcPs
             -> LPat GhcPs -> HsPatSynDir GhcPs -> AnnPSB -> HsBind GhcPs
mkPatSynBind name details lpat dir anns = PatSynBind noExtField psb
  where
    psb = PSB{ psb_ext = anns
             , psb_id = name
             , psb_args = details
             , psb_def = lpat
             , psb_dir = dir }

-- |If any of the matches in the 'FunBind' are infix, the 'FunBind' is
-- considered infix.
isInfixFunBind :: forall id1 id2. UnXRec id2 => HsBindLR id1 id2 -> Bool
isInfixFunBind (FunBind { fun_matches = MG _ matches })
  = any (isInfixMatch . unXRec @id2) (unXRec @id2 matches)
isInfixFunBind _ = False

-- |Return the 'SrcSpan' encompassing the contents of any enclosed binds
spanHsLocaLBinds :: HsLocalBinds (GhcPass p) -> SrcSpan
spanHsLocaLBinds (EmptyLocalBinds _) = noSrcSpan
spanHsLocaLBinds (HsValBinds _ (ValBinds _ bs sigs))
  = foldr combineSrcSpans noSrcSpan (bsSpans ++ sigsSpans)
  where
    bsSpans :: [SrcSpan]
    bsSpans = map getLocA bs
    sigsSpans :: [SrcSpan]
    sigsSpans = map getLocA sigs
spanHsLocaLBinds (HsValBinds _ (XValBindsLR (NValBinds bs sigs)))
  = foldr combineSrcSpans noSrcSpan (bsSpans ++ sigsSpans)
  where
    bsSpans :: [SrcSpan]
    bsSpans = map getLocA $ concatMap snd bs
    sigsSpans :: [SrcSpan]
    sigsSpans = map getLocA sigs
spanHsLocaLBinds (HsIPBinds _ (IPBinds _ bs))
  = foldr combineSrcSpans noSrcSpan (map getLocA bs)

------------
-- | Convenience function using 'mkFunBind'.
-- This is for generated bindings only, do not use for user-written code.
mkSimpleGeneratedFunBind :: SrcSpan -> RdrName -> LocatedE [LPat GhcPs]
                         -> LHsExpr GhcPs -> LHsBind GhcPs
mkSimpleGeneratedFunBind loc fun pats expr
  = L (noAnnSrcSpan loc) $ mkFunBind (Generated OtherExpansion SkipPmc) (L (noAnnSrcSpan loc) fun)
                                     [mkMatch ctxt pats expr emptyLocalBinds]
  where
    ctxt :: HsMatchContextPs
    ctxt = mkPrefixFunRhs (L (noAnnSrcSpan loc) fun) noAnn

-- | Make a prefix, non-strict function 'HsMatchContext'
mkPrefixFunRhs :: fn -> AnnFunRhs -> HsMatchContext fn
mkPrefixFunRhs n an = FunRhs { mc_fun        = n
                          , mc_fixity     = Prefix
                          , mc_strictness = NoSrcStrict
                          , mc_an         = an }

------------
mkMatch :: forall p. IsPass p
        => HsMatchContext (LIdP (NoGhcTc (GhcPass p)))
        -> LocatedE [LPat (GhcPass p)]
        -> LHsExpr (GhcPass p)
        -> HsLocalBinds (GhcPass p)
        -> LMatch (GhcPass p) (LHsExpr (GhcPass p))
mkMatch ctxt pats expr binds
  = noLocA (Match { m_ext   = noExtField
                  , m_ctxt  = ctxt
                  , m_pats  = pats
                  , m_grhss = GRHSs emptyComments (unguardedRHS noAnn noSrcSpan expr) binds })

{-
************************************************************************
*                                                                      *
        Collecting binders
*                                                                      *
************************************************************************

Get all the binders in some HsBindGroups, IN THE ORDER OF APPEARANCE. eg.

...
where
  (x, y) = ...
  f i j  = ...
  [a, b] = ...

it should return [x, y, f, a, b] (remember, order important).

Note [Collect binders only after renaming]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These functions should only be used on HsSyn *after* the renamer,
to return a [Name] or [Id].  Before renaming the record punning
and wild-card mechanism makes it hard to know what is bound.
So these functions should not be applied to (HsSyn RdrName)

Note [isUnliftedHsBind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The function isUnliftedHsBind tells if the binding binds a variable of
unlifted type.  e.g.

  - I# x = blah
  - Just (I# x) = blah

isUnliftedHsBind is used in two ways:

* To complain if we make a top-level binding for a variable of unlifted
  type. E.g. any of the above bindings are illegal at top level

* To generate a case expression for a non-recursive local let.  E.g.
     let Just (I# x) = blah in body
  ==>
     case blah of Just (I# x) -> body
  See GHC.HsToCore.Expr.dsUnliftedBind.

Wrinkles:

(W1) For AbsBinds we must check if the local letrec generated by desugaring
     AbsBinds would be unlifted; so we just recurse into the abs_binds. E.g.
       f :: Num a => (# a, a #)
       g :: Num a => a -> a
       f = ...g...
       g = ...g...

    The top-level bindings for f,g are not unlifted (because of the Num a =>),
    but the local, recursive, monomorphic bindings are:
      t = /\a \(d:Num a).
         letrec fm :: (# a, a #) = ...g...
                gm :: a -> a = ...f...
         in (fm, gm)

   Here the binding for 'fm' is illegal.  So we recurse into the abs_binds

(W2) BUT we have a special case when abs_sig is true;
     see Note [The abs_sig field of AbsBinds] in GHC.Hs.Binds

(W3) isUnliftedHsBind returns False even if the binding itself is
     unlifted, provided it binds only lifted variables. E.g.
      -  (# a,b #) = (# reverse xs, xs #)

      -  x = sqrt# y#  :: Float#

      -  type Unl :: UnliftedType
         data Unl = MkUnl Int
         MkUnl z = blah

     In each case the RHS of the "=" has unlifted type, but isUnliftedHsBind
     returns False.  Reason: see GHC Proposal #35
        https://github.com/ghc-proposals/ghc-proposals/blob/master/
        proposals/0035-unbanged-strict-patterns.rst

(W4) In particular, (W3) applies to a pattern that binds no variables at all.
     So   { _ = sqrt# y :: Float# } returns False from isUnliftedHsBind, but
          { x = sqrt# y :: Float# } returns True.
     This is arguably a bit confusing (see #22719)
-}

----------------- Bindings --------------------------

-- | Should we treat this as an unlifted bind? This will be true for any
-- bind that binds an unlifted variable, but we must be careful around
-- AbsBinds. See Note [isUnliftedHsBind]. For usage
-- information, see Note [Strict binds checks] is GHC.HsToCore.Binds.
isUnliftedHsBind :: HsBind GhcTc -> Bool  -- works only over typechecked binds
isUnliftedHsBind (XHsBindsLR (AbsBinds { abs_exports = exports
                                       , abs_sig     = has_sig
                                       , abs_binds   = binds }))
  | has_sig   = any (is_unlifted_id . abe_poly) exports
  | otherwise = isUnliftedHsBinds binds
    -- See wrinkle (W1) and (W2) in Note [isUnliftedHsBind]
    -- If has_sig is True we will never generate a binding for abe_mono,
    -- so we don't need to worry about it being unlifted. The abe_poly
    -- binding might not be: e.g. forall a. Num a => (# a, a #)
    -- If has_sig is False, just recurse

isUnliftedHsBind (FunBind { fun_id = L _ fun })
  = is_unlifted_id fun

isUnliftedHsBind (VarBind { var_id = var })
  = is_unlifted_id var

isUnliftedHsBind (PatBind { pat_lhs = pat })
  = any is_unlifted_id (collectPatBinders CollNoDictBinders pat)
    -- If we changed our view on (W3) you could add
    --    || isUnliftedType pat_ty
    -- to this check

isUnliftedHsBind (PatSynBind {}) = panic "isUnliftedBind: PatSynBind"

isUnliftedHsBinds :: LHsBinds GhcTc -> Bool
isUnliftedHsBinds = any (isUnliftedHsBind . unLoc)

is_unlifted_id :: Id -> Bool
is_unlifted_id id = isUnliftedType (idType id)
   -- Bindings always have a fixed RuntimeRep, so it's OK
   -- to call isUnliftedType here

-- | Is a binding a strict variable or pattern bind (e.g. @!x = ...@)?
isBangedHsBind :: HsBind GhcTc -> Bool
isBangedHsBind (XHsBindsLR (AbsBinds { abs_binds = binds }))
  = any (isBangedHsBind . unLoc) binds
isBangedHsBind (FunBind {fun_matches = matches})
  | [L _ match] <- unLoc $ mg_alts matches
  , FunRhs{mc_strictness = SrcStrict} <- m_ctxt match
  = True
isBangedHsBind (PatBind {pat_lhs = pat})
  = isBangedLPat pat
isBangedHsBind _
  = False

collectLocalBinders :: CollectPass (GhcPass idL)
                    => CollectFlag (GhcPass idL)
                    -> HsLocalBindsLR (GhcPass idL) (GhcPass idR)
                    -> [IdP (GhcPass idL)]
collectLocalBinders flag = \case
    HsValBinds _ binds -> collectHsIdBinders flag binds
                          -- No pattern synonyms here
    HsIPBinds {}       -> []
    EmptyLocalBinds _  -> []

collectHsIdBinders :: CollectPass (GhcPass idL)
                   => CollectFlag (GhcPass idL)
                   -> HsValBindsLR (GhcPass idL) (GhcPass idR)
                   -> [IdP (GhcPass idL)]
-- ^ Collect 'Id' binders only, or 'Id's + pattern synonyms, respectively
collectHsIdBinders flag = collect_hs_val_binders True flag

collectHsValBinders :: CollectPass (GhcPass idL)
                    => CollectFlag (GhcPass idL)
                    -> HsValBindsLR (GhcPass idL) idR
                    -> [IdP (GhcPass idL)]
collectHsValBinders flag = collect_hs_val_binders False flag

collectHsBindBinders :: CollectPass p
                     => CollectFlag p
                     -> HsBindLR p idR
                     -> [IdP p]
-- ^ Collect both 'Id's and pattern-synonym binders
collectHsBindBinders flag b = collect_bind False flag b []

collectHsBindsBinders :: CollectPass p
                      => CollectFlag p
                      -> LHsBindsLR p idR
                      -> [IdP p]
collectHsBindsBinders flag binds = collect_binds False flag binds []

collectHsBindListBinders :: forall p idR. CollectPass p
                         => CollectFlag p
                         -> [LHsBindLR p idR]
                         -> [IdP p]
-- ^ Same as 'collectHsBindsBinders', but works over a list of bindings
collectHsBindListBinders flag = foldr (collect_bind False flag . unXRec @p) []

collect_hs_val_binders :: CollectPass (GhcPass idL)
                       => Bool
                       -> CollectFlag (GhcPass idL)
                       -> HsValBindsLR (GhcPass idL) idR
                       -> [IdP (GhcPass idL)]
collect_hs_val_binders ps flag = \case
    ValBinds _ binds _              -> collect_binds ps flag binds []
    XValBindsLR (NValBinds binds _) -> collect_out_binds ps flag binds

collect_out_binds :: forall p. CollectPass p
                  => Bool
                  -> CollectFlag p
                  -> [(RecFlag, LHsBinds p)]
                  -> [IdP p]
collect_out_binds ps flag = foldr (collect_binds ps flag . snd) []

collect_binds :: forall p idR. CollectPass p
              => Bool
              -> CollectFlag p
              -> LHsBindsLR p idR
              -> [IdP p]
              -> [IdP p]
-- ^ Collect 'Id's, or 'Id's + pattern synonyms, depending on boolean flag
collect_binds ps flag binds acc = foldr (collect_bind ps flag . unXRec @p) acc binds

collect_bind :: forall p idR. CollectPass p
             => Bool
             -> CollectFlag p
             -> HsBindLR p idR
             -> [IdP p]
             -> [IdP p]
collect_bind _ _    (FunBind { fun_id = f })         acc = unXRec @p f : acc
collect_bind _ flag (PatBind { pat_lhs = p })        acc = collect_lpat flag p acc
collect_bind _ _    (VarBind { var_id = f })         acc = f : acc
collect_bind omitPatSyn _ (PatSynBind _ (PSB { psb_id = ps })) acc
  | omitPatSyn                  = acc
  | otherwise                   = unXRec @p ps : acc
collect_bind _ _ (PatSynBind _ (XPatSynBind _)) acc = acc
collect_bind _ _ (XHsBindsLR b) acc = collectXXHsBindsLR @p @idR b acc


collectMethodBinders :: forall idL idR. UnXRec idL => LHsBindsLR idL idR -> [LIdP idL]
-- ^ Used exclusively for the bindings of an instance decl which are all
-- 'FunBinds'
collectMethodBinders binds = foldr (get . unXRec @idL) [] binds
  where
    get (FunBind { fun_id = f }) fs = f : fs
    get _                        fs = fs
       -- Someone else complains about non-FunBinds

----------------- Statements --------------------------
--
collectLStmtsBinders
  :: (IsPass idL, IsPass idR, CollectPass (GhcPass idL))
  => CollectFlag (GhcPass idL)
  -> [LStmtLR (GhcPass idL) (GhcPass idR) body]
  -> [IdP (GhcPass idL)]
collectLStmtsBinders flag = concatMap (collectLStmtBinders flag)

collectStmtsBinders
  :: (IsPass idL, IsPass idR, CollectPass (GhcPass idL))
  => CollectFlag (GhcPass idL)
  -> [StmtLR (GhcPass idL) (GhcPass idR) body]
  -> [IdP (GhcPass idL)]
collectStmtsBinders flag = concatMap (collectStmtBinders flag)

collectLStmtBinders
  :: (IsPass idL, IsPass idR, CollectPass (GhcPass idL))
  => CollectFlag (GhcPass idL)
  -> LStmtLR (GhcPass idL) (GhcPass idR) body
  -> [IdP (GhcPass idL)]
collectLStmtBinders flag = collectStmtBinders flag . unLoc

collectStmtBinders
  :: forall idL idR body . (IsPass idL, IsPass idR, CollectPass (GhcPass idL))
  => CollectFlag (GhcPass idL)
  -> StmtLR (GhcPass idL) (GhcPass idR) body
  -> [IdP (GhcPass idL)]
  -- Id Binders for a Stmt... [but what about pattern-sig type vars]?
collectStmtBinders flag = \case
    BindStmt _ pat _ -> collectPatBinders flag pat
    LetStmt _  binds -> collectLocalBinders flag binds
    BodyStmt {}      -> []
    LastStmt {}      -> []
    ParStmt _ xs _ _ -> collectLStmtsBinders flag [s | ParStmtBlock _ ss _ _ <- xs, s <- ss]
    TransStmt { trS_stmts = stmts } -> collectLStmtsBinders flag stmts
    RecStmt { recS_stmts = L _ ss } -> collectLStmtsBinders flag ss
    XStmtLR x -> case ghcPass :: GhcPass idR of
        GhcRn -> collectApplicativeStmtBndrs x
        GhcTc -> collectApplicativeStmtBndrs x
  where
    collectApplicativeStmtBndrs :: ApplicativeStmt (GhcPass idL) a -> [IdP (GhcPass idL)]
    collectApplicativeStmtBndrs (ApplicativeStmt _ args _) = concatMap (collectArgBinders . snd) args

    collectArgBinders = \case
        ApplicativeArgOne { app_arg_pattern = pat } -> collectPatBinders flag pat
        ApplicativeArgMany { bv_pattern = pat }     -> collectPatBinders flag pat

----------------- Patterns --------------------------

collectPatBinders
    :: CollectPass p
    => CollectFlag p
    -> LPat p
    -> [IdP p]
collectPatBinders flag pat = collect_lpat flag pat []

collectPatsBinders
    :: CollectPass p
    => CollectFlag p
    -> [LPat p]
    -> [IdP p]
collectPatsBinders flag pats = foldr (collect_lpat flag) [] pats

-------------

-- | Indicate if evidence binders and type variable binders have
--   to be collected.
--
-- This type enumerates the modes of collecting bound variables
--                     | evidence |   type    |   term    |  ghc  |
--                     | binders  | variables | variables |  pass |
--                     --------------------------------------------
-- CollNoDictBinders   |  no      |    no     |    yes    |  any  |
-- CollWithDictBinders |  yes     |    no     |    yes    | GhcTc |
-- CollVarTyVarBinders |  no      |    yes    |    yes    | GhcRn |
--
-- See Note [Dictionary binders in ConPatOut]
data CollectFlag p where
    -- | Don't collect evidence binders
    CollNoDictBinders   :: CollectFlag p
    -- | Collect evidence binders
    CollWithDictBinders :: CollectFlag GhcTc
    -- | Collect variable and type variable binders, but no evidence binders
    CollVarTyVarBinders :: CollectFlag GhcRn


collect_lpat :: forall p. CollectPass p
             => CollectFlag p
             -> LPat p
             -> [IdP p]
             -> [IdP p]
collect_lpat flag pat bndrs = collect_pat flag (unXRec @p pat) bndrs

collect_pat :: forall p. CollectPass p
            => CollectFlag p
            -> Pat p
            -> [IdP p]
            -> [IdP p]
collect_pat flag pat bndrs = case pat of
  VarPat _ var          -> unXRec @p var : bndrs
  WildPat _             -> bndrs
  LazyPat _ pat         -> collect_lpat flag pat bndrs
  BangPat _ pat         -> collect_lpat flag pat bndrs
  AsPat _ a pat         -> unXRec @p a : collect_lpat flag pat bndrs
  ViewPat _ _ pat       -> collect_lpat flag pat bndrs
  ParPat _ pat          -> collect_lpat flag pat bndrs
  ListPat _ pats        -> foldr (collect_lpat flag) bndrs pats
  TuplePat _ pats _     -> foldr (collect_lpat flag) bndrs pats
  OrPat _ _             -> []
      -- See Note [Implementation of OrPatterns], Renamer:
      -- evidence binders in an OrPat currently aren't visible outside their
      -- binding pattern, so we return [].
  SumPat _ pat _ _      -> collect_lpat flag pat bndrs
  LitPat _ _            -> bndrs
  NPat {}               -> bndrs
  NPlusKPat _ n _ _ _ _ -> unXRec @p n : bndrs
  SigPat _ pat sig      -> case flag of
    CollNoDictBinders   -> collect_lpat flag pat bndrs
    CollWithDictBinders -> collect_lpat flag pat bndrs
    CollVarTyVarBinders -> collect_lpat flag pat bndrs ++ collectPatSigBndrs sig
  XPat ext              -> collectXXPat @p flag ext bndrs
  SplicePat ext _       -> collectXSplicePat @p flag ext bndrs
  EmbTyPat _ tp         -> collect_ty_pat_bndrs flag tp bndrs
  InvisPat _ tp         -> collect_ty_pat_bndrs flag tp bndrs

  -- See Note [Dictionary binders in ConPatOut]
  ConPat {pat_args=ps}  -> case flag of
    CollNoDictBinders   -> foldr (collect_lpat flag) bndrs (hsConPatArgs ps)
    CollWithDictBinders -> foldr (collect_lpat flag) bndrs (hsConPatArgs ps)
                           ++ collectEvBinders (cpt_binds (pat_con_ext pat))
    CollVarTyVarBinders -> foldr (collect_lpat flag) bndrs (hsConPatArgs ps)
                           ++ concatMap collectConPatTyArgBndrs (hsConPatTyArgs ps)

collectEvBinders :: TcEvBinds -> [Id]
collectEvBinders (EvBinds bs)   = foldr add_ev_bndr [] bs
collectEvBinders (TcEvBinds {}) = panic "ToDo: collectEvBinders"

collectConPatTyArgBndrs :: HsConPatTyArg GhcRn -> [Name]
collectConPatTyArgBndrs (HsConPatTyArg _ tp) = collectTyPatBndrs tp

collect_ty_pat_bndrs :: CollectFlag p -> HsTyPat (NoGhcTc p) -> [IdP p] -> [IdP p]
collect_ty_pat_bndrs CollNoDictBinders _ bndrs = bndrs
collect_ty_pat_bndrs CollWithDictBinders _ bndrs = bndrs
collect_ty_pat_bndrs CollVarTyVarBinders tp bndrs = collectTyPatBndrs tp ++ bndrs

collectTyPatBndrs :: HsTyPat GhcRn -> [Name]
collectTyPatBndrs (HsTP (HsTPRn nwcs imp_tvs exp_tvs) _) = nwcs ++ imp_tvs ++ exp_tvs

collectPatSigBndrs :: HsPatSigType GhcRn -> [Name]
collectPatSigBndrs (HsPS (HsPSRn nwcs imp_tvs) _) = nwcs ++ imp_tvs

add_ev_bndr :: EvBind -> [Id] -> [Id]
add_ev_bndr (EvBind { eb_lhs = b }) bs | isId b    = b:bs
                                       | otherwise = bs
  -- A worry: what about coercion variable binders??


-- | This class specifies how to collect variable identifiers from extension patterns in the given pass.
-- Consumers of the GHC API that define their own passes should feel free to implement instances in order
-- to make use of functions which depend on it.
--
-- In particular, Haddock already makes use of this, with an instance for its 'DocNameI' pass so that
-- it can reuse the code in GHC for collecting binders.
class UnXRec p => CollectPass p where
  collectXXPat :: CollectFlag p -> XXPat p -> [IdP p] -> [IdP p]
  collectXXHsBindsLR :: forall pR. XXHsBindsLR p pR -> [IdP p] -> [IdP p]
  collectXSplicePat :: CollectFlag p -> XSplicePat p -> [IdP p] -> [IdP p]

instance IsPass p => CollectPass (GhcPass p) where
  collectXXPat flag ext =
    case ghcPass @p of
      GhcPs -> dataConCantHappen ext
      GhcRn
        | HsPatExpanded _ pat <- ext
        -> collect_pat flag pat
      GhcTc -> case ext of
        CoPat _ pat _      -> collect_pat flag pat
        ExpansionPat _ pat -> collect_pat flag pat
  collectXXHsBindsLR ext =
    case ghcPass @p of
      GhcPs -> dataConCantHappen ext
      GhcRn -> dataConCantHappen ext
      GhcTc -> case ext of
        AbsBinds { abs_exports = dbinds } -> (map abe_poly dbinds ++)
        -- I don't think we want the binders from the abe_binds

        -- binding (hence see AbsBinds) is in zonking in GHC.Tc.Zonk.Type

  collectXSplicePat flag ext =
      case ghcPass @p of
        GhcPs -> id
        GhcRn | (HsUntypedSpliceTop _ pat) <- ext -> collect_pat flag pat
        GhcRn | (HsUntypedSpliceNested _)  <- ext -> id
        GhcTc -> dataConCantHappen ext


{-
Note [Dictionary binders in ConPatOut]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Should we collect dictionary binders in ConPatOut? It depends! Use CollectFlag
to choose.

1. Pre-typechecker there are no ConPatOuts. Use CollNoDictBinders flag.

2. In the desugarer, most of the time we don't want to collect evidence binders,
   so we also use CollNoDictBinders flag.

   Example of why it matters:

   In a lazy pattern, for example f ~(C x y) = ..., we want to generate bindings
   for x,y but not for dictionaries bound by C.
   (The type checker ensures they would not be used.)

   Here's the problem.  Consider

        data T a where
           C :: Num a => a -> Int -> T a

        f ~(C (n+1) m) = (n,m)

   Here, the pattern (C (n+1)) binds a hidden dictionary (d::Num a),
   and *also* uses that dictionary to match the (n+1) pattern.  Yet, the
   variables bound by the lazy pattern are n,m, *not* the dictionary d.
   So in mkSelectorBinds in GHC.HsToCore.Utils, we want just m,n as the
   variables bound.

   So in this case, we do *not* gather (a) dictionary and (b) dictionary
   bindings as binders of a ConPatOut pattern.


3. On the other hand, desugaring of arrows needs evidence bindings and uses
   CollWithDictBinders flag.

   Consider

        h :: (ArrowChoice a, Arrow a) => Int -> a (Int,Int) Int
        h x = proc (y,z) -> case compare x y of
                        GT -> returnA -< z+x

   The type checker turns the case into

        case compare x y of
          GT { $dNum_123 = $dNum_Int } -> returnA -< (+) $dNum_123 z x

   That is, it attaches the $dNum_123 binding to a ConPatOut in scope.

   During desugaring, evidence binders must be collected because their sets are
   intersected with free variable sets of subsequent commands to create
   (minimal) command environments.  Failing to do it properly leads to bugs
   (e.g., #18950).

   Note: attaching evidence binders to existing ConPatOut may be suboptimal for
   arrows.  In the example above we would prefer to generate:

        case compare x y of
          GT -> returnA -< let $dNum_123 = $dNum_Int in (+) $dNum_123 z x

   So that the evidence isn't passed into the command environment. This issue
   doesn't arise with desugaring of non-arrow code because the simplifier can
   freely float and inline let-expressions created for evidence binders. But
   with arrow desugaring, the simplifier would have to see through the command
   environment tuple which is more complicated.

-}

hsGroupBinders :: HsGroup GhcRn -> [Name]
hsGroupBinders (HsGroup { hs_valds = val_decls, hs_tyclds = tycl_decls,
                          hs_fords = foreign_decls })
  =  collectHsValBinders CollNoDictBinders val_decls
  ++ hsTyClForeignBinders tycl_decls foreign_decls

hsTyClForeignBinders :: [TyClGroup GhcRn]
                     -> [LForeignDecl GhcRn]
                     -> [Name]
-- We need to look at instance declarations too,
-- because their associated types may bind data constructors
hsTyClForeignBinders tycl_decls foreign_decls
  =    map unLoc (hsForeignDeclsBinders foreign_decls)
    ++ getSelectorNames
         (foldMap (foldMap (tyDeclBinders . hsLTyClDeclBinders) . group_tyclds) tycl_decls
         `mappend`
         (foldMap (foldMap hsLInstDeclBinders . group_instds) tycl_decls))
  where
    getSelectorNames :: ([LocatedA Name], [LFieldOcc GhcRn]) -> [Name]
    getSelectorNames (ns, fs) = map unLoc ns ++ map (foExt . unLoc) fs

-------------------

data TyDeclBinders p
  = TyDeclBinders
  { tyDeclMainBinder     :: !(LocatedA (IdP (GhcPass p)), TyConFlavour ())
  , tyDeclATs            :: ![(LocatedA (IdP (GhcPass p)), TyConFlavour ())]
  , tyDeclOpSigs         :: ![LocatedA (IdP (GhcPass p))]
  , tyDeclConsWithFields :: !(LConsWithFields p) }

tyDeclBinders :: TyDeclBinders p -> ([LocatedA (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
tyDeclBinders (TyDeclBinders main ats sigs consWithFields)
  = (fst main : (fmap fst ats ++ sigs ++ cons), flds)
  where
    (cons, flds) = lconsWithFieldsBinders consWithFields

hsLTyClDeclBinders :: (IsPass p, OutputableBndrId p)
                   => LocatedA (TyClDecl (GhcPass p))
                   -> TyDeclBinders p
-- ^ Returns all the /binding/ names of the decl.  The first one is
-- guaranteed to be the name of the decl. The first component
-- represents all binding names except record fields; the second
-- represents field occurrences. For record fields mentioned in
-- multiple constructors, the SrcLoc will be from the first occurrence.
--
-- Each returned (Located name) has a SrcSpan for the /whole/ declaration.
-- See Note [SrcSpan for binders]

hsLTyClDeclBinders (L loc (FamDecl { tcdFam = FamilyDecl
                                            { fdLName = (L _ name)
                                            , fdInfo  = fd_info } }))
  = TyDeclBinders
  { tyDeclMainBinder = (L loc name, familyInfoTyConFlavour Nothing fd_info)
  , tyDeclATs = [], tyDeclOpSigs = []
  , tyDeclConsWithFields = emptyLConsWithFields }
hsLTyClDeclBinders (L loc (SynDecl
                               { tcdLName = (L _ name) }))
  = TyDeclBinders
  { tyDeclMainBinder = (L loc name, TypeSynonymFlavour)
  , tyDeclATs = [], tyDeclOpSigs = []
  , tyDeclConsWithFields = emptyLConsWithFields }
hsLTyClDeclBinders (L loc (ClassDecl
                               { tcdLName = (L _ cls_name)
                               , tcdSigs  = sigs
                               , tcdATs   = ats }))
  = TyDeclBinders
  { tyDeclMainBinder = (L loc cls_name, ClassFlavour)
  , tyDeclATs = [ (L fam_loc fam_name, familyInfoTyConFlavour (Just ()) fd_info)
                | (L fam_loc (FamilyDecl { fdLName = L _ fam_name
                                         , fdInfo = fd_info })) <- ats ]
  , tyDeclOpSigs = [ L mem_loc mem_name
                   | (L mem_loc (ClassOpSig _ False ns _)) <- sigs
                   , (L _ mem_name) <- ns ]
  , tyDeclConsWithFields = emptyLConsWithFields }
hsLTyClDeclBinders (L loc (DataDecl    { tcdLName = (L _ name)
                                       , tcdDataDefn = defn }))
  = TyDeclBinders
  { tyDeclMainBinder = (L loc name, flav )
  , tyDeclATs = []
  , tyDeclOpSigs = []
  , tyDeclConsWithFields = hsDataDefnBinders defn }
  where
    flav = newOrDataToFlavour $ dataDefnConsNewOrData $ dd_cons defn

-------------------
hsForeignDeclsBinders :: forall p a. (UnXRec (GhcPass p), IsSrcSpanAnn p a)
                      => [LForeignDecl (GhcPass p)] -> [LIdP (GhcPass p)]
-- ^ See Note [SrcSpan for binders]
hsForeignDeclsBinders foreign_decls
  = [ L (noAnnSrcSpan (locA decl_loc)) n
    | L decl_loc (ForeignImport { fd_name = L _ n })
        <- foreign_decls]


-------------------
hsPatSynSelectors :: IsPass p => HsValBinds (GhcPass p) -> [FieldOcc (GhcPass p)]
-- ^ Collects record pattern-synonym selectors only; the pattern synonym
-- names are collected by 'collectHsValBinders'.
hsPatSynSelectors (ValBinds _ _ _) = panic "hsPatSynSelectors"
hsPatSynSelectors (XValBindsLR (NValBinds binds _))
  = foldr addPatSynSelector [] . concat $ map snd binds

addPatSynSelector :: forall p. UnXRec p => LHsBind p -> [FieldOcc p] -> [FieldOcc p]
addPatSynSelector bind sels
  | PatSynBind _ (PSB { psb_args = RecCon as }) <- unXRec @p bind
  = map recordPatSynField as ++ sels
  | otherwise = sels

getPatSynBinds :: forall id. UnXRec id
               => [(RecFlag, LHsBinds id)] -> [PatSynBind id id]
getPatSynBinds binds
  = [ psb | (_, lbinds) <- binds
          , (unXRec @id -> (PatSynBind _ psb)) <- lbinds ]

-------------------
hsLInstDeclBinders :: (IsPass p, OutputableBndrId p)
                   => LInstDecl (GhcPass p)
                   -> ([(LocatedA (IdP (GhcPass p)))], [LFieldOcc (GhcPass p)])
hsLInstDeclBinders (L _ (ClsInstD
                             { cid_inst = ClsInstDecl
                                          { cid_datafam_insts = dfis }}))
  = foldMap (lconsWithFieldsBinders . hsDataFamInstBinders . unLoc) dfis
hsLInstDeclBinders (L _ (DataFamInstD { dfid_inst = fi }))
  = lconsWithFieldsBinders $ hsDataFamInstBinders fi
hsLInstDeclBinders (L _ (TyFamInstD {})) = mempty

-------------------
-- | the 'SrcLoc' returned are for the whole declarations, not just the names
hsDataFamInstBinders :: (IsPass p, OutputableBndrId p)
                     => DataFamInstDecl (GhcPass p)
                     -> LConsWithFields p
hsDataFamInstBinders (DataFamInstDecl { dfid_eqn = FamEqn { feqn_rhs = defn }})
  = hsDataDefnBinders defn
  -- There can't be repeated symbols because only data instances have binders

-------------------
-- | the 'SrcLoc' returned are for the whole declarations, not just the names
hsDataDefnBinders :: (IsPass p, OutputableBndrId p)
                  => HsDataDefn (GhcPass p)
                  -> LConsWithFields p
hsDataDefnBinders (HsDataDefn { dd_cons = cons })
  = hsConDeclsBinders (toList cons)
  -- See Note [Binders in family instances]

-------------------

{- Note [Collecting record fields in data declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When renaming a data declaration that includes record constructors, we are, in
the end, going to to create a mapping from constructor to its field labels,
to store in 'GREInfo' (see 'IAmConLike'). This allows us to know, in the renamer,
which constructor has what fields.

In order to achieve this, we return the constructor and field information from
hsConDeclsBinders in the following format:

  - [(ConRdrName, [Located Int])], a list of the constructors, each associated
    with its record fields, in the form of a list of Int indices into...
  - IntMap FieldOcc, an IntMap of record fields.

(In actual fact, we use [(ConRdrName, Maybe [Located Int])], with Nothing indicating
that the constructor has unlabelled fields: see Note [Local constructor info in the renamer]
in GHC.Types.GREInfo.)

This allows us to do the following (see GHC.Rename.Names.getLocalNonValBinders.new_tc):

  - create 'Name's for each of the record fields, to get IntMap FieldLabel,
  - create 'Name's for each of the constructors, to get [(ConName, [Int])],
  - look up the FieldLabels of each constructor, to get [(ConName, [FieldLabel])].

NB: This can be a bit tricky to get right in the presence of data types with
duplicate constructors or fields. Storing locations allows us to report an error
for duplicate field declarations, see test cases T9156 T9156_DF.
Other relevant test cases: rnfail015.

-}

-- | A mapping from constructors to all of their fields.
--
-- See Note [Collecting record fields in data declarations].
data LConsWithFields p =
  LConsWithFields
    { consWithFieldIndices :: [(LocatedA (IdP (GhcPass p)), Maybe [Located Int])]
    , consFields :: IntMap (LFieldOcc (GhcPass p))
    }

lconsWithFieldsBinders :: LConsWithFields p
                       -> ([(LocatedA (IdP (GhcPass p)))], [LFieldOcc (GhcPass p)])
lconsWithFieldsBinders (LConsWithFields cons fields)
  = (map fst cons, IntMap.elems fields)

emptyLConsWithFields :: LConsWithFields p
emptyLConsWithFields = LConsWithFields [] IntMap.empty

hsConDeclsBinders :: forall p. (IsPass p, OutputableBndrId p)
                  => [LConDecl (GhcPass p)]
                  -> LConsWithFields p
  -- The function is boringly complicated because of the records
  -- And since we only have equality, we have to be a little careful
hsConDeclsBinders cons = go emptyFieldIndices cons
  where
    go :: FieldIndices p -> [LConDecl (GhcPass p)] -> LConsWithFields p
    go seen [] = LConsWithFields [] (fields seen)
    go seen (r:rs)
      -- Don't re-mangle the location of field names, because we don't
      -- have a record of the full location of the field declaration anyway
      = let loc = getLoc r
        in case unLoc r of
           ConDeclGADT { con_names = names, con_g_args = args }
             -> LConsWithFields (cons ++ ns) fs
             where
                cons = map ( , con_flds ) $ toList (L loc . unLoc <$> names)
                (con_flds, seen') = get_flds_gadt seen args
                LConsWithFields ns fs = go seen' rs

           ConDeclH98 { con_name = name, con_args = args }
             -> LConsWithFields ([(L loc (unLoc name), con_flds)] ++ ns) fs
             where
                (con_flds, seen') = get_flds_h98 seen args
                LConsWithFields ns fs = go seen' rs

    get_flds_h98 :: FieldIndices p -> HsConDeclH98Details (GhcPass p)
                 -> (Maybe [Located Int], FieldIndices p)
    get_flds_h98 seen (RecCon flds) = first Just $ get_flds seen flds
    get_flds_h98 seen (PrefixCon _ []) = (Just [], seen)
    get_flds_h98 seen _ = (Nothing, seen)

    get_flds_gadt :: FieldIndices p -> HsConDeclGADTDetails (GhcPass p)
                  -> (Maybe [Located Int], FieldIndices p)
    get_flds_gadt seen (RecConGADT _ flds) = first Just $ get_flds seen flds
    get_flds_gadt seen (PrefixConGADT _ []) = (Just [], seen)
    get_flds_gadt seen _ = (Nothing, seen)

    get_flds :: FieldIndices p -> LocatedL [LConDeclField (GhcPass p)]
             -> ([Located Int], FieldIndices p)
    get_flds seen flds =
      foldr add_fld ([], seen) fld_names
      where
        add_fld fld (is, ixs) =
          let (i, ixs') = insertField fld ixs
          in  (i:is, ixs')
        fld_names = concatMap (cd_fld_names . unLoc) (unLoc flds)

-- | A bijection between record fields of a datatype and integers,
-- used to implement Note [Collecting record fields in data declarations].
data FieldIndices p =
  FieldIndices
    { fields       :: IntMap (LFieldOcc (GhcPass p))
        -- ^ Look up a field from its index.
    , fieldIndices :: Map RdrName Int
        -- ^ Look up the index of a field label in the previous 'IntMap'.
    , newInt       :: !Int
        -- ^ An integer @i@ such that no integer @i' >= i@ appears in the 'IntMap'.
    }

emptyFieldIndices :: FieldIndices p
emptyFieldIndices =
  FieldIndices { fields       = IntMap.empty
               , fieldIndices = Map.empty
               , newInt       = 0 }

insertField :: LFieldOcc (GhcPass p) -> FieldIndices p -> (Located Int, FieldIndices p)
insertField new_fld fi@(FieldIndices flds idxs new_idx)
  | Just i <- Map.lookup rdr idxs
  = (L loc i, fi)
  | otherwise
  = (L loc new_idx,
      FieldIndices (IntMap.insert new_idx new_fld flds)
                   (Map.insert rdr new_idx idxs)
                   (new_idx + 1))
  where
    loc = getLocA new_fld
    rdr = unLoc . foLabel . unLoc $ new_fld

{-

Note [SrcSpan for binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~
When extracting the (Located RdrName) for a binder, at least for the
main name (the TyCon of a type declaration etc), we want to give it
the @SrcSpan@ of the whole /declaration/, not just the name itself
(which is how it appears in the syntax tree).  This SrcSpan (for the
entire declaration) is used as the SrcSpan for the Name that is
finally produced, and hence for error messages.  (See #8607.)

Note [Binders in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type or data family instance declaration, the type
constructor is an *occurrence* not a binding site
    type instance T Int = Int -> Int   -- No binders
    data instance S Bool = S1 | S2     -- Binders are S1,S2


************************************************************************
*                                                                      *
        Collecting binders the user did not write
*                                                                      *
************************************************************************

The job of the following family of functions is to run through binding sites and find
the set of all Names that were defined "implicitly", without being explicitly written
by the user.

Note [Collecting implicit binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We collect all the RHS Names that are implicitly introduced by record wildcards,
so that we can:

  - avoid warning the user when they don't use those names (#4404),
  - report deprecation warnings for deprecated fields that are used (#23382).

The functions that collect implicit binders return a collection of 'ImplicitFieldBinders',
which associates each implicitly-introduced record field with the bound variables in the
RHS of the record field pattern, e.g. in

  data R = MkR { fld :: Int }
  foo (MkR { .. }) = fld

the renamer will elaborate this to

  foo (MkR { fld = fld_var }) = fld_var

and the implicit binders function will return

  [ ImplicitFieldBinders { implFlBndr_field = fld
                         , implFlBndr_binders = [fld_var] } ]

This information is then used:

  - in the calls to GHC.Rename.Utils.checkUnusedRecordWildcard, to emit
    a warning when a record wildcard binds no new variables (redundant record wildcard)
    or none of the bound variables are used (unused record wildcard).
  - in GHC.Rename.Utils.deprecateUsedRecordWildcard, to emit a warning
    when the field is deprecated and any of the binders are used.

NOTE: the implFlBndr_binders field should always be a singleton
      (since the RHS of an implicit binding should always be a VarPat,
      created in rnHsRecPatsAndThen.mkVarPat)

-}

-- | All binders corresponding to a single implicit record field pattern.
--
-- See Note [Collecting implicit binders].
data ImplicitFieldBinders
  = ImplicitFieldBinders { implFlBndr_field :: Name
                             -- ^ The 'Name' of the record field
                         , implFlBndr_binders :: [Name]
                             -- ^ The binders of the RHS of the record field pattern
                             -- (in practice, always a singleton: see Note [Collecting implicit binders])
                         }

lStmtsImplicits :: forall idR body . IsPass idR => [LStmtLR GhcRn (GhcPass idR) (LocatedA (body (GhcPass idR)))]
                -> [(SrcSpan, [ImplicitFieldBinders])]
lStmtsImplicits = hs_lstmts
  where
    hs_lstmts :: forall idR body . IsPass idR => [LStmtLR GhcRn (GhcPass idR) (LocatedA (body (GhcPass idR)))]
              -> [(SrcSpan, [ImplicitFieldBinders])]
    hs_lstmts = concatMap (hs_stmt . unLoc)

    hs_stmt :: forall idR body . IsPass idR => StmtLR GhcRn (GhcPass idR) (LocatedA (body (GhcPass idR)))
            -> [(SrcSpan, [ImplicitFieldBinders])]
    hs_stmt (BindStmt _ pat _) = lPatImplicits pat
    hs_stmt (XStmtLR x) = case ghcPass :: GhcPass idR of
        GhcRn -> hs_applicative_stmt x
        GhcTc -> hs_applicative_stmt x
    hs_stmt (LetStmt _ binds)     = hs_local_binds binds
    hs_stmt (BodyStmt {})         = []
    hs_stmt (LastStmt {})         = []
    hs_stmt (ParStmt _ xs _ _)    = hs_lstmts [s | ParStmtBlock _ ss _ _ <- xs , s <- ss]
    hs_stmt (TransStmt { trS_stmts = stmts }) = hs_lstmts stmts
    hs_stmt (RecStmt { recS_stmts = L _ ss }) = hs_lstmts ss

    hs_local_binds (HsValBinds _ val_binds) = hsValBindsImplicits val_binds
    hs_local_binds (HsIPBinds {})           = []
    hs_local_binds (EmptyLocalBinds _)      = []

    hs_applicative_stmt (ApplicativeStmt _ args _) = concatMap do_arg args
      where do_arg (_, ApplicativeArgOne { app_arg_pattern = pat }) = lPatImplicits pat
            do_arg (_, ApplicativeArgMany { app_stmts = stmts }) = hs_lstmts stmts

hsValBindsImplicits :: HsValBindsLR GhcRn (GhcPass idR)
                    -> [(SrcSpan, [ImplicitFieldBinders])]
hsValBindsImplicits (XValBindsLR (NValBinds binds _))
  = concatMap (lhsBindsImplicits . snd) binds
hsValBindsImplicits (ValBinds _ binds _)
  = lhsBindsImplicits binds

lhsBindsImplicits :: LHsBindsLR GhcRn idR -> [(SrcSpan, [ImplicitFieldBinders])]
lhsBindsImplicits = concatMap (lhs_bind . unLoc)
  where
    lhs_bind (PatBind { pat_lhs = lpat }) = lPatImplicits lpat
    lhs_bind _ = []

-- | Collect all record wild card binders in the given pattern.
--
-- These are all the variables bound in all (possibly nested) record wildcard patterns
-- appearing inside the pattern.
--
-- See Note [Collecting implicit binders].
lPatImplicits :: LPat GhcRn -> [(SrcSpan, [ImplicitFieldBinders])]
lPatImplicits = hs_lpat
  where
    hs_lpat lpat = hs_pat (unLoc lpat)

    hs_lpats = foldr (\pat rest -> hs_lpat pat ++ rest) []

    hs_pat (LazyPat _ pat)      = hs_lpat pat
    hs_pat (BangPat _ pat)      = hs_lpat pat
    hs_pat (AsPat _ _ pat)      = hs_lpat pat
    hs_pat (ViewPat _ _ pat)    = hs_lpat pat
    hs_pat (ParPat _ pat)       = hs_lpat pat
    hs_pat (ListPat _ pats)     = hs_lpats pats
    hs_pat (TuplePat _ pats _)  = hs_lpats pats
    hs_pat (SigPat _ pat _)     = hs_lpat pat

    hs_pat (ConPat {pat_args=ps}) = details ps

    hs_pat _ = []

    details :: HsConPatDetails GhcRn -> [(SrcSpan, [ImplicitFieldBinders])]
    details (PrefixCon _ ps) = hs_lpats ps
    details (RecCon (HsRecFields { rec_dotdot = Nothing, rec_flds }))
      = hs_lpats $ map (hfbRHS . unLoc) rec_flds
    details (RecCon (HsRecFields { rec_dotdot = Just (L err_loc rec_dotdot), rec_flds }))
          = [(l2l err_loc, implicit_field_binders)]
          ++ hs_lpats explicit_pats

          where (explicit_pats, implicit_field_binders)
                  = rec_field_expl_impl rec_flds rec_dotdot

    details (InfixCon p1 p2) = hs_lpat p1 ++ hs_lpat p2

lHsRecFieldsImplicits :: [LHsRecField GhcRn (LPat GhcRn)]
                      -> RecFieldsDotDot
                      -> [ImplicitFieldBinders]
lHsRecFieldsImplicits rec_flds rec_dotdot
  = snd $ rec_field_expl_impl rec_flds rec_dotdot

rec_field_expl_impl :: [LHsRecField GhcRn (LPat GhcRn)]
                    -> RecFieldsDotDot
                    -> ([LPat GhcRn], [ImplicitFieldBinders])
rec_field_expl_impl rec_flds (RecFieldsDotDot { .. })
  = ( map (hfbRHS . unLoc) explicit_binds
    , map implicit_field_binders implicit_binds )
  where (explicit_binds, implicit_binds) = splitAt unRecFieldsDotDot rec_flds
        implicit_field_binders (L _ (HsFieldBind { hfbLHS = L _ fld, hfbRHS = rhs }))
          = ImplicitFieldBinders
              { implFlBndr_field   = foExt fld
              , implFlBndr_binders = collectPatBinders CollNoDictBinders rhs }
