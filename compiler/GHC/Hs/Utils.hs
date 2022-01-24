{-# LANGUAGE ConstraintKinds #-}
{-|
Module      : GHC.Hs.Utils
Description : Generic helpers for the HsSyn type.
Copyright   : (c) The University of Glasgow, 1992-2006

Here we collect a variety of helper functions that construct or
analyse HsSyn.  All these functions deal with generic HsSyn; functions
which deal with the instantiated versions are located elsewhere:

   Parameterised by          Module
   ----------------          -------------
   GhcPs/RdrName             GHC.Parser.PostProcess
   GhcRn/Name                GHC.Rename.*
   GhcTc/Id                  GHC.Tc.Utils.Zonk

The @mk*@ functions attempt to construct a not-completely-useless SrcSpan
from their components, compared with the @nl*@ functions which
just attach noSrcSpan to everything.

-}


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Hs.Utils(
  -- * Terms
  mkHsPar, mkHsApp, mkHsAppWith, mkHsApps, mkHsAppsWith,
  mkHsAppType, mkHsAppTypes, mkHsCaseAlt,
  mkSimpleMatch, unguardedGRHSs, unguardedRHS,
  mkMatchGroup, mkMatch, mkPrefixFunRhs, mkHsLam, mkHsIf,
  mkHsWrap, mkLHsWrap, mkHsWrapCo, mkHsWrapCoR, mkLHsWrapCo,
  mkHsDictLet, mkHsLams,
  mkHsOpApp, mkHsDo, mkHsDoAnns, mkHsComp, mkHsCompAnns, mkHsWrapPat, mkHsWrapPatCo,
  mkLHsPar, mkHsCmdWrap, mkLHsCmdWrap,
  mkHsCmdIf, mkConLikeTc,

  nlHsTyApp, nlHsTyApps, nlHsVar, nl_HsVar, nlHsDataCon,
  nlHsLit, nlHsApp, nlHsApps, nlHsSyntaxApps,
  nlHsIntLit, nlHsVarApps,
  nlHsDo, nlHsOpApp, nlHsLam, nlHsPar, nlHsIf, nlHsCase, nlList,
  mkLHsTupleExpr, mkLHsVarTuple, missingTupArg,
  mkLocatedList,

  -- * Constructing general big tuples
  -- $big_tuples
  mkChunkified, chunkify,

  -- * Bindings
  mkFunBind, mkVarBind, mkHsVarBind, mkSimpleGeneratedFunBind, mkTopFunBind,
  mkPatSynBind,
  isInfixFunBind,
  spanHsLocaLBinds,

  -- * Literals
  mkHsIntegral, mkHsFractional, mkHsIsString, mkHsString, mkHsStringPrimLit,
  mkHsCharPrimLit,

  -- * Patterns
  mkNPat, mkNPlusKPat, nlVarPat, nlLitPat, nlConVarPat, nlConVarPatName, nlConPat,
  nlConPatName, nlInfixConPat, nlNullaryConPat, nlWildConPat, nlWildPat,
  nlWildPatName, nlTuplePat, nlParPat,
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

  -- * Template Haskell
  mkUntypedSplice, mkTypedSplice,
  mkHsQuasiQuote,

  -- * Collecting binders
  isUnliftedHsBind, isBangedHsBind,

  collectLocalBinders, collectHsValBinders, collectHsBindListBinders,
  collectHsIdBinders,
  collectHsBindsBinders, collectHsBindBinders, collectMethodBinders,

  collectPatBinders, collectPatsBinders, collectLMatchPatsBinders,
  collectLMatchPatBinders, collectLStmtsBinders, collectStmtsBinders,
  collectLStmtBinders, collectStmtBinders,
  CollectPass(..), CollectFlag(..),

  hsLTyClDeclBinders, hsTyClForeignBinders,
  hsPatSynSelectors, getPatSynBinds,
  hsForeignDeclsBinders, hsGroupBinders, hsDataFamInstBinders,

  -- * Collecting implicit binders
  lStmtsImplicits, hsValBindsImplicits, lPatImplicits
  ) where

import GHC.Prelude

import GHC.Hs.Decls
import GHC.Hs.Binds
import GHC.Hs.Expr
import GHC.Hs.Pat
import GHC.Hs.Type
import GHC.Hs.Lit
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Parser.Annotation

import GHC.Tc.Types.Evidence
import GHC.Core.TyCo.Rep
import GHC.Core.Multiplicity ( pattern Many )
import GHC.Builtin.Types ( unitTy )
import GHC.Tc.Utils.TcType
import GHC.Core.DataCon
import GHC.Core.ConLike
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
import GHC.Data.Bag
import GHC.Settings.Constants

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Either
import Data.Function
import Data.List ( partition, deleteBy )
import Data.Proxy

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
mkHsPar :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
mkHsPar e = L (getLoc e) (gHsPar e)

mkSimpleMatch :: (Anno (Match (GhcPass p) (LocatedA (body (GhcPass p))))
                        ~ SrcSpanAnnA,
                  Anno (GRHS (GhcPass p) (LocatedA (body (GhcPass p))))
                        ~ SrcAnn NoEpAnns)
              => HsMatchContext (GhcPass p)
              -> [LMatchPat (GhcPass p)] -> LocatedA (body (GhcPass p))
              -> LMatch (GhcPass p) (LocatedA (body (GhcPass p)))
mkSimpleMatch ctxt pats rhs
  = L loc $
    Match { m_ext = noAnn, m_ctxt = ctxt, m_pats = pats
          , m_grhss = unguardedGRHSs (locA loc) rhs noAnn }
  where
    loc = case pats of
                []      -> getLoc rhs
                (pat:_) -> combineSrcSpansA (noAnnSrcSpan (getLocA pat)) (getLoc rhs)

unguardedGRHSs :: Anno (GRHS (GhcPass p) (LocatedA (body (GhcPass p))))
                     ~ SrcAnn NoEpAnns
               => SrcSpan -> LocatedA (body (GhcPass p)) -> EpAnn GrhsAnn
               -> GRHSs (GhcPass p) (LocatedA (body (GhcPass p)))
unguardedGRHSs loc rhs an
  = GRHSs emptyComments (unguardedRHS an loc rhs) emptyLocalBinds

unguardedRHS :: Anno (GRHS (GhcPass p) (LocatedA (body (GhcPass p))))
                     ~ SrcAnn NoEpAnns
             => EpAnn GrhsAnn -> SrcSpan -> LocatedA (body (GhcPass p))
             -> [LGRHS (GhcPass p) (LocatedA (body (GhcPass p)))]
unguardedRHS an loc rhs = [L (noAnnSrcSpan loc) (GRHS an [] rhs)]

type AnnoBody p body
  = ( XMG (GhcPass p) (LocatedA (body (GhcPass p))) ~ NoExtField
    , Anno [LocatedA (Match (GhcPass p) (LocatedA (body (GhcPass p))))] ~ SrcSpanAnnL
    , Anno (Match (GhcPass p) (LocatedA (body (GhcPass p)))) ~ SrcSpanAnnA
    )

mkMatchGroup :: AnnoBody p body
             => Origin
             -> LocatedL [LocatedA (Match (GhcPass p) (LocatedA (body (GhcPass p))))]
             -> MatchGroup (GhcPass p) (LocatedA (body (GhcPass p)))
mkMatchGroup origin matches = MG { mg_ext = noExtField
                                 , mg_alts = matches
                                 , mg_origin = origin }

mkLocatedList :: Semigroup a
  => [GenLocated (SrcAnn a) e2] -> LocatedAn an [GenLocated (SrcAnn a) e2]
mkLocatedList [] = noLocA []
mkLocatedList ms = L (noAnnSrcSpan $ locA $ combineLocsA (head ms) (last ms)) ms

mkHsApp :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
mkHsApp e1 e2 = addCLocAA e1 e2 (HsApp noComments e1 e2)

mkHsAppWith
  :: (LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> HsExpr (GhcPass id) -> LHsExpr (GhcPass id))
  -> LHsExpr (GhcPass id)
  -> LHsExpr (GhcPass id)
  -> LHsExpr (GhcPass id)
mkHsAppWith mkLocated e1 e2 = mkLocated e1 e2 (HsApp noAnn e1 e2)

mkHsApps
  :: LHsExpr (GhcPass id) -> [LHsExpr (GhcPass id)] -> LHsExpr (GhcPass id)
mkHsApps = mkHsAppsWith addCLocAA

mkHsAppsWith
 :: (LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> HsExpr (GhcPass id) -> LHsExpr (GhcPass id))
 -> LHsExpr (GhcPass id)
 -> [LHsExpr (GhcPass id)]
 -> LHsExpr (GhcPass id)
mkHsAppsWith mkLocated = foldl' (mkHsAppWith mkLocated)

mkHsAppType :: LHsExpr GhcRn -> LHsWcType GhcRn -> LHsExpr GhcRn
mkHsAppType e t = addCLocAA t_body e (HsAppType noExtField e paren_wct)
  where
    t_body    = hswc_body t
    paren_wct = t { hswc_body = parenthesizeHsType appPrec t_body }

mkHsAppTypes :: LHsExpr GhcRn -> [LHsWcType GhcRn] -> LHsExpr GhcRn
mkHsAppTypes = foldl' mkHsAppType

mkHsLam :: (IsPass p, XMG (GhcPass p) (LHsExpr (GhcPass p)) ~ NoExtField)
        => [LMatchPat (GhcPass p)]
        -> LHsExpr (GhcPass p)
        -> LHsExpr (GhcPass p)
mkHsLam pats body = mkHsPar (L (getLoc body) (HsLam noExtField matches))
  where
    matches = mkMatchGroup Generated
                           (noLocA [mkSimpleMatch LambdaExpr pats' body])
    pats' = map (parenthesizeLMatchPat appPrec) pats

mkHsLams :: [TyVar] -> [EvVar] -> LHsExpr GhcTc -> LHsExpr GhcTc
mkHsLams tyvars dicts expr = mkLHsWrap (mkWpTyLams tyvars
                                       <.> mkWpLams dicts) expr

-- |A simple case alternative with a single pattern, no binds, no guards;
-- pre-typechecking
mkHsCaseAlt :: (Anno (GRHS (GhcPass p) (LocatedA (body (GhcPass p))))
                     ~ SrcAnn NoEpAnns,
                 Anno (Match (GhcPass p) (LocatedA (body (GhcPass p))))
                        ~ SrcSpanAnnA)
            => LPat (GhcPass p) -> (LocatedA (body (GhcPass p)))
            -> LMatch (GhcPass p) (LocatedA (body (GhcPass p)))
mkHsCaseAlt pat expr
  = mkSimpleMatch CaseAlt [mkVisMatchPat pat] expr

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

mkParMatchPat :: IsPass p => LMatchPat (GhcPass p) -> LMatchPat (GhcPass p)
mkParMatchPat = parenthesizeLMatchPat appPrec

nlParPat :: LPat (GhcPass name) -> LPat (GhcPass name)
nlParPat p = noLocA (gParPat p)

-------------------------------
-- These are the bits of syntax that contain rebindable names
-- See GHC.Rename.Env.lookupSyntax

mkHsIntegral   :: IntegralLit -> HsOverLit GhcPs
mkHsFractional :: FractionalLit -> HsOverLit GhcPs
mkHsIsString   :: SourceText -> FastString -> HsOverLit GhcPs
mkHsDo         :: HsDoFlavour -> LocatedL [ExprLStmt GhcPs] -> HsExpr GhcPs
mkHsDoAnns     :: HsDoFlavour -> LocatedL [ExprLStmt GhcPs] -> EpAnn AnnList -> HsExpr GhcPs
mkHsComp       :: HsDoFlavour -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
               -> HsExpr GhcPs
mkHsCompAnns   :: HsDoFlavour -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
               -> EpAnn AnnList
               -> HsExpr GhcPs

mkNPat      :: LocatedAn NoEpAnns (HsOverLit GhcPs) -> Maybe (SyntaxExpr GhcPs) -> EpAnn [AddEpAnn]
            -> Pat GhcPs
mkNPlusKPat :: LocatedN RdrName -> LocatedAn NoEpAnns (HsOverLit GhcPs) -> EpAnn EpaLocation
            -> Pat GhcPs

-- NB: The following functions all use noSyntaxExpr: the generated expressions
--     will not work with rebindable syntax if used after the renamer
mkLastStmt :: IsPass idR => LocatedA (bodyR (GhcPass idR))
           -> StmtLR (GhcPass idL) (GhcPass idR) (LocatedA (bodyR (GhcPass idR)))
mkBodyStmt :: LocatedA (bodyR GhcPs)
           -> StmtLR (GhcPass idL) GhcPs (LocatedA (bodyR GhcPs))
mkPsBindStmt :: EpAnn [AddEpAnn] -> LPat GhcPs -> LocatedA (bodyR GhcPs)
             -> StmtLR GhcPs GhcPs (LocatedA (bodyR GhcPs))
mkRnBindStmt :: LPat GhcRn -> LocatedA (bodyR GhcRn)
             -> StmtLR GhcRn GhcRn (LocatedA (bodyR GhcRn))
mkTcBindStmt :: LPat GhcTc -> LocatedA (bodyR GhcTc)
             -> StmtLR GhcTc GhcTc (LocatedA (bodyR GhcTc))

emptyRecStmt     :: (Anno [GenLocated
                             (Anno (StmtLR (GhcPass idL) GhcPs bodyR))
                             (StmtLR (GhcPass idL) GhcPs bodyR)]
                        ~ SrcSpanAnnL)
                 => StmtLR (GhcPass idL) GhcPs bodyR
emptyRecStmtName :: (Anno [GenLocated
                             (Anno (StmtLR GhcRn GhcRn bodyR))
                             (StmtLR GhcRn GhcRn bodyR)]
                        ~ SrcSpanAnnL)
                 => StmtLR GhcRn GhcRn bodyR
emptyRecStmtId   :: Stmt GhcTc (LocatedA (HsCmd GhcTc))
mkRecStmt        :: (Anno [GenLocated
                             (Anno (StmtLR (GhcPass idL) GhcPs bodyR))
                             (StmtLR (GhcPass idL) GhcPs bodyR)]
                        ~ SrcSpanAnnL)
                 => EpAnn AnnList
                 -> LocatedL [LStmtLR (GhcPass idL) GhcPs bodyR]
                 -> StmtLR (GhcPass idL) GhcPs bodyR


mkHsIntegral     i  = OverLit noExtField (HsIntegral       i)
mkHsFractional   f  = OverLit noExtField (HsFractional     f)
mkHsIsString src s  = OverLit noExtField (HsIsString   src s)

mkHsDo     ctxt stmts      = HsDo noAnn ctxt stmts
mkHsDoAnns ctxt stmts anns = HsDo anns  ctxt stmts
mkHsComp ctxt stmts expr = mkHsCompAnns ctxt stmts expr noAnn
mkHsCompAnns ctxt stmts expr anns = mkHsDoAnns ctxt (mkLocatedList (stmts ++ [last_stmt])) anns
  where
    -- Strip the annotations from the location, they are in the embedded expr
    last_stmt = L (noAnnSrcSpan $ getLocA expr) $ mkLastStmt expr

-- restricted to GhcPs because other phases might need a SyntaxExpr
mkHsIf :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs -> EpAnn AnnsIf
       -> HsExpr GhcPs
mkHsIf c a b anns = HsIf anns c a b

-- restricted to GhcPs because other phases might need a SyntaxExpr
mkHsCmdIf :: LHsExpr GhcPs -> LHsCmd GhcPs -> LHsCmd GhcPs -> EpAnn AnnsIf
       -> HsCmd GhcPs
mkHsCmdIf c a b anns = HsCmdIf anns noSyntaxExpr c a b

mkNPat lit neg anns  = NPat anns lit neg noSyntaxExpr
mkNPlusKPat id lit anns
  = NPlusKPat anns id lit (unLoc lit) noSyntaxExpr noSyntaxExpr

mkTransformStmt    :: EpAnn [AddEpAnn] -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkTransformByStmt  :: EpAnn [AddEpAnn] -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> LHsExpr GhcPs -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkGroupUsingStmt   :: EpAnn [AddEpAnn] -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
mkGroupByUsingStmt :: EpAnn [AddEpAnn] -> [ExprLStmt GhcPs] -> LHsExpr GhcPs
                   -> LHsExpr GhcPs
                   -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)

emptyTransStmt :: EpAnn [AddEpAnn] -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
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
                                                xbstc_boundResultMult = Many,
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
mkRecStmt anns stmts  = (emptyRecStmt' anns) { recS_stmts = stmts }

mkLetStmt :: EpAnn [AddEpAnn] -> HsLocalBinds GhcPs -> StmtLR GhcPs GhcPs (LocatedA b)
mkLetStmt anns binds = LetStmt anns binds

-------------------------------
-- | A useful function for building @OpApps@.  The operator is always a
-- variable, and we don't know the fixity yet.
mkHsOpApp :: LHsExpr GhcPs -> IdP GhcPs -> LHsExpr GhcPs -> HsExpr GhcPs
mkHsOpApp e1 op e2 = OpApp noAnn e1 (noLocA (HsVar noExtField (noLocA op))) e2

unqualSplice :: RdrName
unqualSplice = mkRdrUnqual (mkVarOccFS (fsLit "splice"))

mkUntypedSplice :: EpAnn [AddEpAnn] -> SpliceDecoration -> LHsExpr GhcPs -> HsSplice GhcPs
mkUntypedSplice ann hasParen e = HsUntypedSplice ann hasParen unqualSplice e

mkTypedSplice :: EpAnn [AddEpAnn] -> SpliceDecoration -> LHsExpr GhcPs -> HsSplice GhcPs
mkTypedSplice ann hasParen e = HsTypedSplice ann hasParen unqualSplice e

mkHsQuasiQuote :: RdrName -> SrcSpan -> FastString -> HsSplice GhcPs
mkHsQuasiQuote quoter span quote
  = HsQuasiQuote noExtField unqualSplice quoter span quote

mkHsString :: String -> HsLit (GhcPass p)
mkHsString s = HsString NoSourceText (mkFastString s)

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
nlHsLit n = noLocA (HsLit noComments n)

nlHsIntLit :: Integer -> LHsExpr (GhcPass p)
nlHsIntLit n = noLocA (HsLit noComments (HsInt noExtField (mkIntegralLit n)))

nlVarPat :: IsSrcSpanAnn p a
        => IdP (GhcPass p) -> LPat (GhcPass p)
nlVarPat n = noLocA (VarPat noExtField (noLocA n))

nlLitPat :: HsLit GhcPs -> LPat GhcPs
nlLitPat l = noLocA (LitPat noExtField l)

nlHsApp :: IsPass id => LHsExpr (GhcPass id) -> LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
nlHsApp f x = noLocA (HsApp noComments f (mkLHsPar x))

nlHsSyntaxApps :: SyntaxExprTc -> [LHsExpr GhcTc]
               -> LHsExpr GhcTc
nlHsSyntaxApps (SyntaxExprTc { syn_expr      = fun
                             , syn_arg_wraps = arg_wraps
                             , syn_res_wrap  = res_wrap }) args
  = mkLHsWrap res_wrap (foldl' nlHsApp (noLocA fun) (zipWithEqual "nlHsSyntaxApps"
                                                     mkLHsWrap arg_wraps args))
nlHsSyntaxApps NoSyntaxExprTc args = pprPanic "nlHsSyntaxApps" (ppr args)
  -- this function should never be called in scenarios where there is no
  -- syntax expr

nlHsApps :: IsSrcSpanAnn p a
         => IdP (GhcPass p) -> [LHsExpr (GhcPass p)] -> LHsExpr (GhcPass p)
nlHsApps f xs = foldl' nlHsApp (nlHsVar f) xs

nlHsVarApps :: IsSrcSpanAnn p a
            => IdP (GhcPass p) -> [IdP (GhcPass p)] -> LHsExpr (GhcPass p)
nlHsVarApps f xs = noLocA (foldl' mk (HsVar noExtField (noLocA f))
                                         (map ((HsVar noExtField) . noLocA) xs))
                 where
                   mk f a = HsApp noComments (noLocA f) (noLocA a)

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
nlHsPar  :: LHsExpr (GhcPass id) -> LHsExpr (GhcPass id)
nlHsCase :: LHsExpr GhcPs -> [LMatch GhcPs (LHsExpr GhcPs)]
         -> LHsExpr GhcPs
nlList   :: [LHsExpr GhcPs] -> LHsExpr GhcPs

-- AZ:Is this used?
nlHsLam match = noLocA (HsLam noExtField (mkMatchGroup Generated (noLocA [match])))
nlHsPar e     = noLocA (gHsPar e)

-- nlHsIf should generate if-expressions which are NOT subject to
-- RebindableSyntax, so the first field of HsIf is False. (#12080)
nlHsIf :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
nlHsIf cond true false = noLocA (HsIf noAnn cond true false)

nlHsCase expr matches
  = noLocA (HsCase noAnn expr (mkMatchGroup Generated (noLocA matches)))
nlList exprs          = noLocA (ExplicitList noAnn exprs)

nlHsAppTy :: LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
nlHsTyVar :: IsSrcSpanAnn p a
          => IdP (GhcPass p)                            -> LHsType (GhcPass p)
nlHsFunTy :: LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
nlHsParTy :: LHsType (GhcPass p)                        -> LHsType (GhcPass p)

nlHsAppTy f t = noLocA (HsAppTy noExtField f (parenthesizeHsType appPrec t))
nlHsTyVar x   = noLocA (HsTyVar noAnn NotPromoted (noLocA x))
nlHsFunTy a b = noLocA (HsFunTy noAnn (HsUnrestrictedArrow noHsUniTok) (parenthesizeHsType funPrec a) b)
nlHsParTy t   = noLocA (HsParTy noAnn t)

nlHsTyConApp :: IsSrcSpanAnn p a
             => LexicalFixity -> IdP (GhcPass p)
             -> [LHsTypeArg (GhcPass p)] -> LHsType (GhcPass p)
nlHsTyConApp fixity tycon tys
  | Infix <- fixity
  , HsValArg ty1 : HsValArg ty2 : rest <- tys
  = foldl' mk_app (noLocA $ HsOpTy noExtField ty1 (noLocA tycon) ty2) rest
  | otherwise
  = foldl' mk_app (nlHsTyVar tycon) tys
  where
    mk_app :: LHsType (GhcPass p) -> LHsTypeArg (GhcPass p) -> LHsType (GhcPass p)
    mk_app fun@(L _ (HsOpTy {})) arg = mk_app (noLocA $ HsParTy noAnn fun) arg
      -- parenthesize things like `(A + B) C`
    mk_app fun (HsValArg ty) = noLocA (HsAppTy noExtField fun (parenthesizeHsType appPrec ty))
    mk_app fun (HsTypeArg _ ki) = noLocA (HsAppKindTy noSrcSpan fun (parenthesizeHsType appPrec ki))
    mk_app fun (HsArgPar _) = noLocA (HsParTy noAnn fun)

nlHsAppKindTy ::
  LHsType (GhcPass p) -> LHsKind (GhcPass p) -> LHsType (GhcPass p)
nlHsAppKindTy f k
  = noLocA (HsAppKindTy noSrcSpan f (parenthesizeHsType appPrec k))

{-
Tuples.  All these functions are *pre-typechecker* because they lack
types on the tuple.
-}

mkLHsTupleExpr :: [LHsExpr (GhcPass p)] -> XExplicitTuple (GhcPass p)
               -> LHsExpr (GhcPass p)
-- Makes a pre-typechecker boxed tuple, deals with 1 case
mkLHsTupleExpr [e] _ = e
mkLHsTupleExpr es ext
  = noLocA $ ExplicitTuple ext (map (Present noAnn) es) Boxed

mkLHsVarTuple :: IsSrcSpanAnn p a
               => [IdP (GhcPass p)]  -> XExplicitTuple (GhcPass p)
              -> LHsExpr (GhcPass p)
mkLHsVarTuple ids ext = mkLHsTupleExpr (map nlHsVar ids) ext

nlTuplePat :: [LPat GhcPs] -> Boxity -> LPat GhcPs
nlTuplePat pats box = noLocA (TuplePat noAnn pats box)

missingTupArg :: EpAnn EpaLocation -> HsTupArg GhcPs
missingTupArg ann = Missing ann

mkLHsPatTup :: [LPat GhcRn] -> LPat GhcRn
mkLHsPatTup []     = noLocA $ TuplePat noExtField [] Boxed
mkLHsPatTup [lpat] = lpat
mkLHsPatTup lpats  = L (getLoc (head lpats)) $ TuplePat noExtField lpats Boxed

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

-- $big_tuples
-- #big_tuples#
--
-- GHCs built in tuples can only go up to 'mAX_TUPLE_SIZE' in arity, but
-- we might conceivably want to build such a massive tuple as part of the
-- output of a desugaring stage (notably that for list comprehensions).
--
-- We call tuples above this size \"big tuples\", and emulate them by
-- creating and pattern matching on >nested< tuples that are expressible
-- by GHC.
--
-- Nesting policy: it's better to have a 2-tuple of 10-tuples (3 objects)
-- than a 10-tuple of 2-tuples (11 objects), so we want the leaves of any
-- construction to be big.
--
-- If you just use the 'mkBigCoreTup', 'mkBigCoreVarTupTy', 'mkTupleSelector'
-- and 'mkTupleCase' functions to do all your work with tuples you should be
-- fine, and not have to worry about the arity limitation at all.

-- | Lifts a \"small\" constructor into a \"big\" constructor by recursive decomposition
mkChunkified :: ([a] -> a)      -- ^ \"Small\" constructor function, of maximum input arity 'mAX_TUPLE_SIZE'
             -> [a]             -- ^ Possible \"big\" list of things to construct from
             -> a               -- ^ Constructed thing made possible by recursive decomposition
mkChunkified small_tuple as = mk_big_tuple (chunkify as)
  where
        -- Each sub-list is short enough to fit in a tuple
    mk_big_tuple [as] = small_tuple as
    mk_big_tuple as_s = mk_big_tuple (chunkify (map small_tuple as_s))

chunkify :: [a] -> [[a]]
-- ^ Split a list into lists that are small enough to have a corresponding
-- tuple arity. The sub-lists of the result all have length <= 'mAX_TUPLE_SIZE'
-- But there may be more than 'mAX_TUPLE_SIZE' sub-lists
chunkify xs
  | n_xs <= mAX_TUPLE_SIZE = [xs]
  | otherwise              = split xs
  where
    n_xs     = length xs
    split [] = []
    split xs = take mAX_TUPLE_SIZE xs : split (drop mAX_TUPLE_SIZE xs)

{-
************************************************************************
*                                                                      *
        LHsSigType and LHsSigWcType
*                                                                      *
********************************************************************* -}

-- | Convert an 'LHsType' to an 'LHsSigType'.
hsTypeToHsSigType :: LHsType GhcPs -> LHsSigType GhcPs
hsTypeToHsSigType lty@(L loc ty) = L loc $ case ty of
  HsForAllTy { hst_tele = HsForAllInvis { hsf_xinvis = an
                                        , hsf_invis_bndrs = bndrs }
             , hst_body = body }
    -> mkHsExplicitSigType an bndrs body
  _ -> mkHsImplicitSigType lty

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

{- *********************************************************************
*                                                                      *
    --------- HsWrappers: type args, dict args, casts ---------
*                                                                      *
********************************************************************* -}

mkLHsWrap :: HsWrapper -> LHsExpr GhcTc -> LHsExpr GhcTc
mkLHsWrap co_fn (L loc e) = L loc (mkHsWrap co_fn e)

mkHsWrap :: HsWrapper -> HsExpr GhcTc -> HsExpr GhcTc
mkHsWrap co_fn e | isIdHsWrapper co_fn = e
mkHsWrap co_fn e                       = XExpr (WrapExpr $ HsWrap co_fn e)

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

mkHsWrapPatCo :: TcCoercionN -> Pat GhcTc -> Type -> Pat GhcTc
mkHsWrapPatCo co pat ty | isTcReflCo co = pat
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
            , fun_tick = [] }

mkTopFunBind :: Origin -> LocatedN Name -> [LMatch GhcRn (LHsExpr GhcRn)]
             -> HsBind GhcRn
-- ^ In Name-land, with empty bind_fvs
mkTopFunBind origin fn ms = FunBind { fun_id = fn
                                    , fun_matches = mkMatchGroup origin (noLocA ms)
                                    , fun_ext  = emptyNameSet -- NB: closed
                                                              --     binding
                                    , fun_tick = [] }

mkHsVarBind :: SrcSpan -> RdrName -> LHsExpr GhcPs -> LHsBind GhcPs
mkHsVarBind loc var rhs = mkSimpleGeneratedFunBind loc var [] rhs

mkVarBind :: IdP (GhcPass p) -> LHsExpr (GhcPass p) -> LHsBind (GhcPass p)
mkVarBind var rhs = L (getLoc rhs) $
                    VarBind { var_ext = noExtField,
                              var_id = var, var_rhs = rhs }

mkPatSynBind :: LocatedN RdrName -> HsPatSynDetails GhcPs
             -> LPat GhcPs -> HsPatSynDir GhcPs -> EpAnn [AddEpAnn] -> HsBind GhcPs
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
isInfixFunBind (FunBind { fun_matches = MG _ matches _ })
  = any (isInfixMatch . unXRec @id2) (unXRec @id2 matches)
isInfixFunBind _ = False

-- |Return the 'SrcSpan' encompassing the contents of any enclosed binds
spanHsLocaLBinds :: HsLocalBinds (GhcPass p) -> SrcSpan
spanHsLocaLBinds (EmptyLocalBinds _) = noSrcSpan
spanHsLocaLBinds (HsValBinds _ (ValBinds _ bs sigs))
  = foldr combineSrcSpans noSrcSpan (bsSpans ++ sigsSpans)
  where
    bsSpans :: [SrcSpan]
    bsSpans = map getLocA $ bagToList bs
    sigsSpans :: [SrcSpan]
    sigsSpans = map getLocA sigs
spanHsLocaLBinds (HsValBinds _ (XValBindsLR (NValBinds bs sigs)))
  = foldr combineSrcSpans noSrcSpan (bsSpans ++ sigsSpans)
  where
    bsSpans :: [SrcSpan]
    bsSpans = map getLocA $ concatMap (bagToList . snd) bs
    sigsSpans :: [SrcSpan]
    sigsSpans = map getLocA sigs
spanHsLocaLBinds (HsIPBinds _ (IPBinds _ bs))
  = foldr combineSrcSpans noSrcSpan (map getLocA bs)

------------
-- | Convenience function using 'mkFunBind'.
-- This is for generated bindings only, do not use for user-written code.
mkSimpleGeneratedFunBind :: SrcSpan -> RdrName -> [LMatchPat GhcPs]
                -> LHsExpr GhcPs -> LHsBind GhcPs
mkSimpleGeneratedFunBind loc fun pats expr
  = L (noAnnSrcSpan loc) $ mkFunBind Generated (L (noAnnSrcSpan loc) fun)
              [mkMatch (mkPrefixFunRhs (L (noAnnSrcSpan loc) fun)) pats expr
                       emptyLocalBinds]

-- | Make a prefix, non-strict function 'HsMatchContext'
mkPrefixFunRhs :: LIdP p -> HsMatchContext p
mkPrefixFunRhs n = FunRhs { mc_fun = n
                          , mc_fixity = Prefix
                          , mc_strictness = NoSrcStrict }

------------
mkMatch :: forall p. IsPass p
        => HsMatchContext (GhcPass p)
        -> [LMatchPat (GhcPass p)]
        -> LHsExpr (GhcPass p)
        -> HsLocalBinds (GhcPass p)
        -> LMatch (GhcPass p) (LHsExpr (GhcPass p))
mkMatch ctxt pats expr binds
  = noLocA (Match { m_ext   = noAnn
                  , m_ctxt  = ctxt
                  , m_pats  = map mkParMatchPat pats
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

Note [Unlifted id check in isUnliftedHsBind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The function isUnliftedHsBind is used to complain if we make a top-level
binding for a variable of unlifted type.

Such a binding is illegal if the top-level binding would be unlifted;
but also if the local letrec generated by desugaring AbsBinds would be.
E.g.
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

Here the binding for 'fm' is illegal.  So generally we check the abe_mono types.

BUT we have a special case when abs_sig is true;
  see Note [The abs_sig field of AbsBinds] in GHC.Hs.Binds
-}

----------------- Bindings --------------------------

-- | Should we treat this as an unlifted bind? This will be true for any
-- bind that binds an unlifted variable, but we must be careful around
-- AbsBinds. See Note [Unlifted id check in isUnliftedHsBind]. For usage
-- information, see Note [Strict binds checks] is GHC.HsToCore.Binds.
isUnliftedHsBind :: HsBind GhcTc -> Bool  -- works only over typechecked binds
isUnliftedHsBind bind
  | AbsBinds { abs_exports = exports, abs_sig = has_sig } <- bind
  = if has_sig
    then any (is_unlifted_id . abe_poly) exports
    else any (is_unlifted_id . abe_mono) exports
    -- If has_sig is True we will never generate a binding for abe_mono,
    -- so we don't need to worry about it being unlifted. The abe_poly
    -- binding might not be: e.g. forall a. Num a => (# a, a #)

  | otherwise
  = any is_unlifted_id (collectHsBindBinders CollNoDictBinders bind)
  where
    is_unlifted_id id = isUnliftedType (idType id)

-- | Is a binding a strict variable or pattern bind (e.g. @!x = ...@)?
isBangedHsBind :: HsBind GhcTc -> Bool
isBangedHsBind (AbsBinds { abs_binds = binds })
  = anyBag (isBangedHsBind . unLoc) binds
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
                    -> HsValBindsLR (GhcPass idL) (GhcPass idR)
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
                       -> HsValBindsLR (GhcPass idL) (GhcPass idR)
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
collect_bind _ flag (PatBind { pat_lhs = p })           acc = collect_lpat flag p acc
collect_bind _ _ (FunBind { fun_id = f })            acc = unXRec @p f : acc
collect_bind _ _ (VarBind { var_id = f })            acc = f : acc
collect_bind _ _ (AbsBinds { abs_exports = dbinds }) acc = map abe_poly dbinds ++ acc
        -- I don't think we want the binders from the abe_binds

        -- binding (hence see AbsBinds) is in zonking in GHC.Tc.Utils.Zonk
collect_bind omitPatSyn _ (PatSynBind _ (PSB { psb_id = ps })) acc
  | omitPatSyn                  = acc
  | otherwise                   = unXRec @p ps : acc
collect_bind _ _ (PatSynBind _ (XPatSynBind _)) acc = acc
collect_bind _ _ (XHsBindsLR _) acc = acc

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
  :: CollectPass (GhcPass idL)
  => CollectFlag (GhcPass idL)
  -> [LStmtLR (GhcPass idL) (GhcPass idR) body]
  -> [IdP (GhcPass idL)]
collectLStmtsBinders flag = concatMap (collectLStmtBinders flag)

collectStmtsBinders
  :: (CollectPass (GhcPass idL))
  => CollectFlag (GhcPass idL)
  -> [StmtLR (GhcPass idL) (GhcPass idR) body]
  -> [IdP (GhcPass idL)]
collectStmtsBinders flag = concatMap (collectStmtBinders flag)

collectLStmtBinders
  :: (CollectPass (GhcPass idL))
  => CollectFlag (GhcPass idL)
  -> LStmtLR (GhcPass idL) (GhcPass idR) body
  -> [IdP (GhcPass idL)]
collectLStmtBinders flag = collectStmtBinders flag . unLoc

collectStmtBinders
  :: CollectPass (GhcPass idL)
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
    ApplicativeStmt _ args _        -> concatMap collectArgBinders args
        where
         collectArgBinders = \case
            (_, ApplicativeArgOne { app_arg_pattern = pat }) -> collectPatBinders flag pat
            (_, ApplicativeArgMany { bv_pattern = pat })     -> collectPatBinders flag pat


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

collectLMatchPatBinders
    :: CollectPass p
    => CollectFlag p
    -> LMatchPat p
    -> [IdP p]
-- ^ Return all the variables bound by the `[LMatchPat p]`,
--   including both type variables and term variables
collectLMatchPatBinders flag pat = collect_lmatchpat flag pat []


collectLMatchPatsBinders
    :: CollectPass p
    => CollectFlag p
    -> [LMatchPat p]
    -> [IdP p]
-- ^ Return all the variables bound by the `[LMatchPat p]`,
--   including both type variables and term variables
collectLMatchPatsBinders flag pats = foldr (collect_lmatchpat flag) [] pats

-------------

-- | Indicate if evidence binders have to be collected.
--
-- This type is used as a boolean (should we collect evidence binders or not?)
-- but also to pass an evidence that the AST has been typechecked when we do
-- want to collect evidence binders, otherwise these binders are not available.
--
-- See Note [Dictionary binders in ConPatOut]
data CollectFlag p where
    -- | Don't collect evidence binders
    CollNoDictBinders   :: CollectFlag p
    -- | Collect evidence binders
    CollWithDictBinders :: CollectFlag GhcTc

collect_lpat :: forall p. (CollectPass p)
             => CollectFlag p
             -> LPat p
             -> [IdP p]
             -> [IdP p]
collect_lpat flag pat bndrs = collect_pat flag (unXRec @p pat) bndrs

collect_lmatchpat :: forall p. (CollectPass p)
                  => CollectFlag p
                  -> LMatchPat p
                  -> [IdP p]
                  -> [IdP p]
collect_lmatchpat flag match_pat bndrs   = case (unXRec @p match_pat) of
  VisPat _ pat        -> collect_lpat flag pat bndrs
  InvisTyVarPat _ idp -> (unXRec @p idp) : bndrs
  InvisWildTyPat _    -> bndrs
  XMatchPat _         -> bndrs

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
  ParPat _ _ pat _      -> collect_lpat flag pat bndrs
  ListPat _ pats        -> foldr (collect_lpat flag) bndrs pats
  TuplePat _ pats _     -> foldr (collect_lpat flag) bndrs pats
  SumPat _ pat _ _      -> collect_lpat flag pat bndrs
  LitPat _ _            -> bndrs
  NPat {}               -> bndrs
  NPlusKPat _ n _ _ _ _ -> unXRec @p n : bndrs
  SigPat _ pat _        -> collect_lpat flag pat bndrs
  XPat ext              -> collectXXPat (Proxy @p) flag ext bndrs
  SplicePat _ (HsSpliced _ _ (HsSplicedPat pat))
                        -> collect_pat flag pat bndrs
  SplicePat _ _         -> bndrs
  -- See Note [Dictionary binders in ConPatOut]
  ConPat {pat_args=ps}  -> case flag of
    CollNoDictBinders   -> foldr (collect_lpat flag) bndrs (hsConPatArgs ps)
    CollWithDictBinders -> foldr (collect_lpat flag) bndrs (hsConPatArgs ps)
                           ++ collectEvBinders (cpt_binds (pat_con_ext pat))

collectEvBinders :: TcEvBinds -> [Id]
collectEvBinders (EvBinds bs)   = foldr add_ev_bndr [] bs
collectEvBinders (TcEvBinds {}) = panic "ToDo: collectEvBinders"

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
  collectXXPat :: Proxy p -> CollectFlag p -> XXPat p -> [IdP p] -> [IdP p]

instance IsPass p => CollectPass (GhcPass p) where
  collectXXPat _ flag ext =
    case ghcPass @p of
      GhcPs -> dataConCantHappen ext
      GhcRn
        | HsPatExpanded _ pat <- ext
        -> collect_pat flag pat
      GhcTc -> case ext of
        CoPat _ pat _      -> collect_pat flag pat
        ExpansionPat _ pat -> collect_pat flag pat

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
         (foldMap (foldMap hsLTyClDeclBinders . group_tyclds) tycl_decls
         `mappend`
         foldMap (foldMap hsLInstDeclBinders . group_instds) tycl_decls)
  where
    getSelectorNames :: ([LocatedA Name], [LFieldOcc GhcRn]) -> [Name]
    getSelectorNames (ns, fs) = map unLoc ns ++ map (foExt . unLoc) fs

-------------------
hsLTyClDeclBinders :: IsPass p
                   => LocatedA (TyClDecl (GhcPass p))
                   -> ([LocatedA (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
-- ^ Returns all the /binding/ names of the decl.  The first one is
-- guaranteed to be the name of the decl. The first component
-- represents all binding names except record fields; the second
-- represents field occurrences. For record fields mentioned in
-- multiple constructors, the SrcLoc will be from the first occurrence.
--
-- Each returned (Located name) has a SrcSpan for the /whole/ declaration.
-- See Note [SrcSpan for binders]

hsLTyClDeclBinders (L loc (FamDecl { tcdFam = FamilyDecl
                                            { fdLName = (L _ name) } }))
  = ([L loc name], [])
hsLTyClDeclBinders (L loc (SynDecl
                               { tcdLName = (L _ name) }))
  = ([L loc name], [])
hsLTyClDeclBinders (L loc (ClassDecl
                               { tcdLName = (L _ cls_name)
                               , tcdSigs  = sigs
                               , tcdATs   = ats }))
  = (L loc cls_name :
     [ L fam_loc fam_name | (L fam_loc (FamilyDecl
                                        { fdLName = L _ fam_name })) <- ats ]
     ++
     [ L mem_loc mem_name
                          | (L mem_loc (ClassOpSig _ False ns _)) <- sigs
                          , (L _ mem_name) <- ns ]
    , [])
hsLTyClDeclBinders (L loc (DataDecl    { tcdLName = (L _ name)
                                       , tcdDataDefn = defn }))
  = (\ (xs, ys) -> (L loc name : xs, ys)) $ hsDataDefnBinders defn


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
  = foldr addPatSynSelector [] . unionManyBags $ map snd binds

addPatSynSelector :: forall p. UnXRec p => LHsBind p -> [FieldOcc p] -> [FieldOcc p]
addPatSynSelector bind sels
  | PatSynBind _ (PSB { psb_args = RecCon as }) <- unXRec @p bind
  = map recordPatSynField as ++ sels
  | otherwise = sels

getPatSynBinds :: forall id. UnXRec id
               => [(RecFlag, LHsBinds id)] -> [PatSynBind id id]
getPatSynBinds binds
  = [ psb | (_, lbinds) <- binds
          , (unXRec @id -> (PatSynBind _ psb)) <- bagToList lbinds ]

-------------------
hsLInstDeclBinders :: IsPass p
                   => LInstDecl (GhcPass p)
                   -> ([LocatedA (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
hsLInstDeclBinders (L _ (ClsInstD
                             { cid_inst = ClsInstDecl
                                          { cid_datafam_insts = dfis }}))
  = foldMap (hsDataFamInstBinders . unLoc) dfis
hsLInstDeclBinders (L _ (DataFamInstD { dfid_inst = fi }))
  = hsDataFamInstBinders fi
hsLInstDeclBinders (L _ (TyFamInstD {})) = mempty

-------------------
-- | the 'SrcLoc' returned are for the whole declarations, not just the names
hsDataFamInstBinders :: IsPass p
                     => DataFamInstDecl (GhcPass p)
                     -> ([LocatedA (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
hsDataFamInstBinders (DataFamInstDecl { dfid_eqn = FamEqn { feqn_rhs = defn }})
  = hsDataDefnBinders defn
  -- There can't be repeated symbols because only data instances have binders

-------------------
-- | the 'SrcLoc' returned are for the whole declarations, not just the names
hsDataDefnBinders :: IsPass p
                  => HsDataDefn (GhcPass p)
                  -> ([LocatedA (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
hsDataDefnBinders (HsDataDefn { dd_cons = cons })
  = hsConDeclsBinders cons
  -- See Note [Binders in family instances]

-------------------
type Seen p = [LFieldOcc (GhcPass p)] -> [LFieldOcc (GhcPass p)]
                 -- Filters out ones that have already been seen

hsConDeclsBinders :: forall p. IsPass p
                  => [LConDecl (GhcPass p)]
                  -> ([LocatedA (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
   -- See hsLTyClDeclBinders for what this does
   -- The function is boringly complicated because of the records
   -- And since we only have equality, we have to be a little careful
hsConDeclsBinders cons
  = go id cons
  where
    go :: Seen p -> [LConDecl (GhcPass p)]
       -> ([LocatedA (IdP (GhcPass p))], [LFieldOcc (GhcPass p)])
    go _ [] = ([], [])
    go remSeen (r:rs)
      -- Don't re-mangle the location of field names, because we don't
      -- have a record of the full location of the field declaration anyway
      = let loc = getLoc r
        in case unLoc r of
           -- remove only the first occurrence of any seen field in order to
           -- avoid circumventing detection of duplicate fields (#9156)
           ConDeclGADT { con_names = names, con_g_args = args }
             -> (map (L loc . unLoc) names ++ ns, flds ++ fs)
             where
                (remSeen', flds) = get_flds_gadt remSeen args
                (ns, fs) = go remSeen' rs

           ConDeclH98 { con_name = name, con_args = args }
             -> ([L loc (unLoc name)] ++ ns, flds ++ fs)
             where
                (remSeen', flds) = get_flds_h98 remSeen args
                (ns, fs) = go remSeen' rs

    get_flds_h98 :: Seen p -> HsConDeclH98Details (GhcPass p)
                 -> (Seen p, [LFieldOcc (GhcPass p)])
    get_flds_h98 remSeen (RecCon flds) = get_flds remSeen flds
    get_flds_h98 remSeen _ = (remSeen, [])

    get_flds_gadt :: Seen p -> HsConDeclGADTDetails (GhcPass p)
                  -> (Seen p, [LFieldOcc (GhcPass p)])
    get_flds_gadt remSeen (RecConGADT flds _) = get_flds remSeen flds
    get_flds_gadt remSeen _ = (remSeen, [])

    get_flds :: Seen p -> LocatedL [LConDeclField (GhcPass p)]
             -> (Seen p, [LFieldOcc (GhcPass p)])
    get_flds remSeen flds = (remSeen', fld_names)
       where
          fld_names = remSeen (concatMap (cd_fld_names . unLoc) (unLoc flds))
          remSeen' = foldr (.) remSeen
                               [deleteBy ((==) `on` unLoc . foLabel . unLoc) v
                               | v <- fld_names]

{-

Note [SrcSpan for binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~
When extracting the (Located RdrNme) for a binder, at least for the
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

The job of this family of functions is to run through binding sites and find the set of all Names
that were defined "implicitly", without being explicitly written by the user.

The main purpose is to find names introduced by record wildcards so that we can avoid
warning the user when they don't use those names (#4404)

Since the addition of -Wunused-record-wildcards, this function returns a pair
of [(SrcSpan, [Name])]. Each element of the list is one set of implicit
binders, the first component of the tuple is the document describes the possible
fix to the problem (by removing the ..).

This means there is some unfortunate coupling between this function and where it
is used but it's only used for one specific purpose in one place so it seemed
easier.
-}

lStmtsImplicits :: [LStmtLR GhcRn (GhcPass idR) (LocatedA (body (GhcPass idR)))]
                -> [(SrcSpan, [Name])]
lStmtsImplicits = hs_lstmts
  where
    hs_lstmts :: [LStmtLR GhcRn (GhcPass idR) (LocatedA (body (GhcPass idR)))]
              -> [(SrcSpan, [Name])]
    hs_lstmts = concatMap (hs_stmt . unLoc)

    hs_stmt :: StmtLR GhcRn (GhcPass idR) (LocatedA (body (GhcPass idR)))
            -> [(SrcSpan, [Name])]
    hs_stmt (BindStmt _ pat _) = lPatImplicits pat
    hs_stmt (ApplicativeStmt _ args _) = concatMap do_arg args
      where do_arg (_, ApplicativeArgOne { app_arg_pattern = pat }) = lPatImplicits pat
            do_arg (_, ApplicativeArgMany { app_stmts = stmts }) = hs_lstmts stmts
    hs_stmt (LetStmt _ binds)     = hs_local_binds binds
    hs_stmt (BodyStmt {})         = []
    hs_stmt (LastStmt {})         = []
    hs_stmt (ParStmt _ xs _ _)    = hs_lstmts [s | ParStmtBlock _ ss _ _ <- xs
                                                , s <- ss]
    hs_stmt (TransStmt { trS_stmts = stmts }) = hs_lstmts stmts
    hs_stmt (RecStmt { recS_stmts = L _ ss }) = hs_lstmts ss

    hs_local_binds (HsValBinds _ val_binds) = hsValBindsImplicits val_binds
    hs_local_binds (HsIPBinds {})           = []
    hs_local_binds (EmptyLocalBinds _)      = []

hsValBindsImplicits :: HsValBindsLR GhcRn (GhcPass idR) -> [(SrcSpan, [Name])]
hsValBindsImplicits (XValBindsLR (NValBinds binds _))
  = concatMap (lhsBindsImplicits . snd) binds
hsValBindsImplicits (ValBinds _ binds _)
  = lhsBindsImplicits binds

lhsBindsImplicits :: LHsBindsLR GhcRn idR -> [(SrcSpan, [Name])]
lhsBindsImplicits = foldBag (++) (lhs_bind . unLoc) []
  where
    lhs_bind (PatBind { pat_lhs = lpat }) = lPatImplicits lpat
    lhs_bind _ = []

lPatImplicits :: LPat GhcRn -> [(SrcSpan, [Name])]
lPatImplicits = hs_lpat
  where
    hs_lpat lpat = hs_pat (unLoc lpat)

    hs_lpats = foldr (\pat rest -> hs_lpat pat ++ rest) []

    hs_pat (LazyPat _ pat)      = hs_lpat pat
    hs_pat (BangPat _ pat)      = hs_lpat pat
    hs_pat (AsPat _ _ pat)      = hs_lpat pat
    hs_pat (ViewPat _ _ pat)    = hs_lpat pat
    hs_pat (ParPat _ _ pat _)   = hs_lpat pat
    hs_pat (ListPat _ pats)     = hs_lpats pats
    hs_pat (TuplePat _ pats _)  = hs_lpats pats

    hs_pat (SigPat _ pat _)     = hs_lpat pat

    hs_pat (ConPat {pat_con=con, pat_args=ps}) = details con ps

    hs_pat _ = []

    details :: LocatedN Name -> HsConPatDetails GhcRn -> [(SrcSpan, [Name])]
    details _ (PrefixCon _ ps) = hs_lpats ps
    details n (RecCon fs)      =
      [(err_loc, collectPatsBinders CollNoDictBinders implicit_pats) | Just{} <- [rec_dotdot fs] ]
        ++ hs_lpats explicit_pats

      where implicit_pats = map (hfbRHS . unLoc) implicit
            explicit_pats = map (hfbRHS . unLoc) explicit


            (explicit, implicit) = partitionEithers [if pat_explicit then Left fld else Right fld
                                                    | (i, fld) <- [0..] `zip` rec_flds fs
                                                    ,  let  pat_explicit =
                                                              maybe True ((i<) . unLoc)
                                                                         (rec_dotdot fs)]
            err_loc = maybe (getLocA n) getLoc (rec_dotdot fs)

    details _ (InfixCon p1 p2) = hs_lpat p1 ++ hs_lpat p2
