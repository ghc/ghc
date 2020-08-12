{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module GHC.Hs.ExprType where

import GHC.Prelude
import GHC.Hs
import GHC.Core.Type
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.PatSyn
import GHC.Types.SrcLoc
import GHC.Types.Id
import GHC.Builtin.Types
import GHC.Tc.Utils.Zonk
import GHC.Core.TyCo.Rep

-- | A pass which annotates all nodes in an 'HsExpr' with
data With (f :: * -> *) pass

type instance XRec (With f p) a = (f a, XRec p a)
type instance XXValBindsLR  (With a pass) (With a pass) = NHsValBindsLR (With a pass)
type instance XXExpr (With a pass) = XXExprGhcTc (With a pass)
type instance XXPat (With a pass) = CoPat (With a pass)
type instance XXCmd (With a pass) = HsWrap HsCmd (With a pass)


-- HsExpr extension points
type instance XVar               (With a pass) = XVar            pass
type instance XUnboundVar        (With a pass) = XUnboundVar     pass
type instance XConLikeOut        (With a pass) = XConLikeOut     pass
type instance XRecFld            (With a pass) = XRecFld         pass
type instance XOverLabel         (With a pass) = XOverLabel      pass
type instance XIPVar             (With a pass) = XIPVar          pass
type instance XOverLitE          (With a pass) = XOverLitE       pass
type instance XLitE              (With a pass) = XLitE           pass
type instance XLam               (With a pass) = XLam            pass
type instance XLamCase           (With a pass) = XLamCase        pass
type instance XApp               (With a pass) = XApp            pass
type instance XAppTypeE          (With a pass) = XAppTypeE       pass
type instance XOpApp             (With a pass) = XOpApp          pass
type instance XNegApp            (With a pass) = XNegApp         pass
type instance XPar               (With a pass) = XPar            pass
type instance XSectionL          (With a pass) = XSectionL       pass
type instance XSectionR          (With a pass) = XSectionR       pass
type instance XExplicitTuple     (With a pass) = XExplicitTuple  pass
type instance XExplicitSum       (With a pass) = XExplicitSum    pass
type instance XCase              (With a pass) = XCase           pass
type instance XIf                (With a pass) = XIf             pass
type instance XMultiIf           (With a pass) = XMultiIf        pass
type instance XLet               (With a pass) = XLet            pass
type instance XDo                (With a pass) = XDo             pass
type instance XExplicitList      (With a pass) = XExplicitList   pass
type instance XRecordCon         (With a pass) = XRecordCon      pass
type instance XRecordUpd         (With a pass) = XRecordUpd      pass
type instance XExprWithTySig     (With a pass) = XExprWithTySig  pass
type instance XArithSeq          (With a pass) = XArithSeq       pass
type instance XSCC               (With a pass) = XSCC            pass
type instance XCoreAnn           (With a pass) = XCoreAnn        pass
type instance XBracket           (With a pass) = XBracket        pass
type instance XRnBracketOut      (With a pass) = XRnBracketOut   pass
type instance XTcBracketOut      (With a pass) = XTcBracketOut   pass
type instance XSpliceE           (With a pass) = XSpliceE        pass
type instance XProc              (With a pass) = XProc           pass
type instance XStatic            (With a pass) = XStatic         pass
type instance XTick              (With a pass) = XTick           pass
type instance XBinTick           (With a pass) = XBinTick        pass
type instance XTickPragma        (With a pass) = XTickPragma     pass
type instance XPragE             (With a pass) = XPragE     pass

-- Other extension points
type instance IdP                (With a pass) = IdP pass
type instance XCIPBind           (With a pass) = XCIPBind pass
type instance XOverLit           (With a pass) = XOverLit pass
type instance XCFieldOcc         (With a pass) = XCFieldOcc pass
type instance XApplicativeArgOne (With a pass) = XApplicativeArgOne pass
type instance XApplicativeArgMany (With a pass) = XApplicativeArgMany pass
type instance XUnambiguous       (With a pass) = XUnambiguous pass
type instance XAmbiguous         (With a pass) = XAmbiguous pass
type instance XABE               (With a pass) = XABE pass

type instance SyntaxExpr         (With a pass) = SyntaxExpr pass

-- Pat extension points
type instance XWildPat           (With a pass) = XWildPat pass
type instance XVarPat            (With a pass) = XVarPat pass
type instance XLazyPat           (With a pass) = XLazyPat pass
type instance XAsPat             (With a pass) = XAsPat pass
type instance XParPat            (With a pass) = XParPat pass
type instance XBangPat           (With a pass) = XBangPat pass
type instance XConPat            (With a pass) = XConPat pass
type instance XListPat           (With a pass) = XListPat pass
type instance XTuplePat          (With a pass) = XTuplePat pass
type instance XSumPat            (With a pass) = XSumPat pass
type instance XViewPat           (With a pass) = XViewPat pass
type instance XLitPat            (With a pass) = XLitPat pass
type instance XNPat              (With a pass) = XNPat pass
type instance XNPlusKPat         (With a pass) = XNPlusKPat pass
type instance XSigPat            (With a pass) = XSigPat pass

type instance ConLikeP           (With a pass) = ConLikeP pass
 
-- Bind extension points
type instance XFunBind           (With a pass) (With a pass) = XFunBind pass pass
type instance XPatBind           (With a pass) (With a pass) = XPatBind pass pass
type instance XVarBind           (With a pass) (With a pass) = XVarBind pass pass
type instance XAbsBinds          (With a pass) (With a pass) = XAbsBinds pass pass
type instance XValBinds          (With a pass) (With a pass) = XValBinds pass pass
type instance XHsValBinds        (With a pass) (With a pass) = XHsValBinds pass pass
type instance XHsIPBinds         (With a pass) (With a pass) = XHsIPBinds pass pass

type instance XPSB               (With a pass) (With a pass) = XPSB pass pass
 
type instance XEmptyLocalBinds   (With a pass) (With a pass) = XEmptyLocalBinds pass pass
type instance XIPBinds           (With a pass)               = XIPBinds pass
 
type instance XMG                (With a pass) body = XMG pass body
type instance XCGRHS             (With a pass) body = XCGRHS pass body
type instance XCGRHSs            (With a pass) body = XCGRHSs pass body
type instance XCMatch            (With a pass) body = XCMatch pass body
 
-- Stmt extension points
type instance XApplicativeStmt   (With a pass) (With a pass) body = XApplicativeStmt pass pass body
type instance XBindStmt          (With a pass) (With a pass) body = XBindStmt pass pass body
type instance XBodyStmt          (With a pass) (With a pass) body = XBodyStmt pass pass body
type instance XLastStmt          (With a pass) (With a pass) body = XLastStmt pass pass body
type instance XParStmt           (With a pass) (With a pass) body = XParStmt pass pass body
type instance XRecStmt           (With a pass) (With a pass) body = XRecStmt pass pass body
type instance XTransStmt         (With a pass) (With a pass) body = XTransStmt pass pass body
type instance XLetStmt           (With a pass) (With a pass) body = XLetStmt pass pass body
 
type instance XParStmtBlock      (With a pass) (With a pass) = XParStmtBlock pass pass
-- Lit extension points
type instance XHsChar            (With a pass)  = XHsChar pass
type instance XHsCharPrim        (With a pass)  = XHsCharPrim pass
type instance XHsWordPrim        (With a pass)  = XHsWordPrim pass
type instance XHsInt64Prim       (With a pass)  = XHsInt64Prim pass
type instance XHsWord64Prim      (With a pass)  = XHsWord64Prim pass
type instance XHsInteger         (With a pass)  = XHsInteger pass
type instance XHsRat             (With a pass)  = XHsRat pass
type instance XHsFloatPrim       (With a pass)  = XHsFloatPrim pass
type instance XHsDoublePrim      (With a pass)  = XHsDoublePrim pass
type instance XHsInt             (With a pass)  = XHsInt pass
type instance XHsIntPrim         (With a pass)  = XHsIntPrim pass
type instance XHsString          (With a pass)  = XHsString pass
type instance XHsStringPrim      (With a pass)  = XHsStringPrim pass
type instance XXLit              (With a pass)  = XXLit pass

type instance XXFieldOcc         (With a pass)  = XXFieldOcc pass
type instance XPresent           (With a pass)  = XPresent pass
type instance XMissing           (With a pass)  = XMissing pass
type instance XAmbiguous         (With a pass)  = XAmbiguous pass
 
-- Cmd extension point
type instance XCmdTop      (With a pass) = XCmdTop pass
type instance XCmdArrApp   (With a pass) = XCmdArrApp pass
type instance XCmdArrForm  (With a pass) = XCmdArrForm pass
type instance XCmdApp      (With a pass) = XCmdApp pass
type instance XCmdLam      (With a pass) = XCmdLam pass
type instance XCmdPar      (With a pass) = XCmdPar pass
type instance XCmdCase     (With a pass) = XCmdCase pass
type instance XCmdIf       (With a pass) = XCmdIf pass
type instance XCmdLet      (With a pass) = XCmdLet pass
type instance XCmdDo       (With a pass) = XCmdDo pass
type instance XCmdWrap     (With a pass) = XCmdWrap pass

type instance XPatSynBind        (With a pass) (With a pass) = XPatSynBind pass pass

type instance NoGhcTcNonGhc (With a pass) = NoGhcTc pass
 
type family HasType a where
  HasType (HsExpr _) = Type
  HasType (Pat _) = Type
  HasType Var = Type
  HasType (HsCmdTop _) = Type
  HasType _ = ()

newtype Typed a = Typed { getType :: HasType a }

{-
untyped :: HasType a ~ () => XRec p a -> XRec (With Typed p) a
untyped x = (Typed (), x)
-}

project :: XRec (With f p) (a p) -> f (a p)
project = fst

lhsExprType :: LHsExpr GhcTc -> LHsExpr (With Typed GhcTc)
lhsExprType (L l e) = (Typed t, L l e')
  where
    (t, e') = hsExprType' e

hsExprType :: HsExpr GhcTc -> HsExpr (With Typed GhcTc)
hsExprType = snd . hsExprType'

hsExprType' :: HsExpr GhcTc -> (Type, HsExpr (With Typed GhcTc))
hsExprType' (HsVar a id) = (ty, HsVar a (Typed ty, id))
  where ty = idType $ unLoc id
hsExprType' (HsUnboundVar a id) = (anyTy, HsUnboundVar a id)
hsExprType' (HsConLikeOut a conlike) = (ty, HsConLikeOut a conlike)
  where
    ty = case conlike of
      RealDataCon con -> dataConNonlinearType con
      PatSynCon patsyn
        | Just (id, _) <- patSynBuilder patsyn -> idType id
        | otherwise    -> invalid
hsExprType' (HsRecFld a r@(selectorAmbiguousFieldOcc -> id)) = (idType id, HsRecFld a (convertAmbiguousFieldOcc r))
hsExprType' HsOverLabel{} = invalid
hsExprType' HsIPVar{} = invalid
hsExprType' (HsOverLit a lit) = (overLitType lit, HsOverLit a (hsOverLitType lit))
hsExprType' (HsLit a lit) = (hsLitType lit, HsLit a (convertLit1 lit))
hsExprType' (HsLam a match) = (ty, HsLam a match')
  where match' = matchGroupType lhsExprType match
        ty = case mg_ext match' of
          MatchGroupTc args res -> mkVisFunTys args res
hsExprType' (HsLamCase a match) = (ty, HsLamCase a match')
  where match' = matchGroupType lhsExprType match
        ty = case mg_ext match' of
          MatchGroupTc args res -> mkVisFunTys args res
hsExprType' (HsApp a f x) = (funResultTy f_ty, HsApp a f' (lhsExprType x))
  where f'@(Typed f_ty, _) = lhsExprType f
hsExprType' (HsAppType x f t) = (ty, HsAppType x f' t)
  where f'@(Typed pi, _) = lhsExprType f
        ty = piResultTy pi x
hsExprType' (OpApp a f x y) = (ty, OpApp a f' x' y')
  where
    f'@(Typed f_ty, _) = lhsExprType f
    x' = lhsExprType x
    y' = lhsExprType y
    ty = funResultTy $ funResultTy f_ty
hsExprType' (NegApp a x op) = (ty, NegApp a x' op')
  where
    x' = lhsExprType x
    (mop_ty, op') = syntaxExprType' op
    ty = case mop_ty of
      Nothing -> invalid
      Just op_ty -> funResultTy $ op_ty
hsExprType' (HsPar a x) = (ty, HsPar a x')
  where x'@(Typed ty, _) = lhsExprType x
hsExprType' (SectionL a x f) = (ty, SectionL a x' f')
  where f'@(Typed f_ty, _) = lhsExprType f
        x'@(Typed _, _) = lhsExprType x
        ty = funResultTy f_ty
hsExprType' (SectionR a f y) = (ty, SectionR a f' y')
  where f'@(Typed f_ty, _) = lhsExprType f
        y'@(Typed _y_ty, _) = lhsExprType y
        x_ty = funArgTy f_ty
        ty = mkVisFunTy Many x_ty $ funResultTy (funResultTy f_ty)
hsExprType' (ExplicitTuple a args box) = (ty, ExplicitTuple a (map (\x -> (Typed (), snd x)) args') box)
  where args' = map lhsTupArgType args
        ty = mkTupleTy box (map fst args')
hsExprType' (ExplicitSum a tag arity x) = (ty, ExplicitSum a tag arity x')
  where x' = lhsExprType x
        ty = mkSumTy a
hsExprType' (HsCase a scrut match) = (ty, HsCase a scrut' match')
  where scrut' = lhsExprType scrut
        match' = matchGroupType lhsExprType match
        ty = case mg_ext match' of
          MatchGroupTc args res -> mkVisFunTys args res
hsExprType' (HsIf a pred t e) = (ty, HsIf a pred' t' e')
  where pred' = lhsExprType pred
        t'@(Typed ty, _) = lhsExprType t
        e' = lhsExprType e
hsExprType' (HsMultiIf ty grhss) = (ty, HsMultiIf ty grhss')
  where grhss' = map (\x -> (Typed (), fmap (hsGRHSType lhsExprType) x)) grhss
hsExprType' (HsLet a binds body) = (ty, HsLet a binds' body')
  where binds' = (Typed (), fmap hsLocalBindsTypes binds)
        body'@(Typed ty, _) = lhsExprType body
hsExprType' (HsDo ty ctxt stmts) = (ty, HsDo ty ctxt (Typed (), stmts'))
  where stmts' = fmap (map (\x -> (Typed (), fmap (stmtType lhsExprType) x))) stmts
hsExprType' (ExplicitList ty op xs) = (ty, ExplicitList ty op' xs')
  where xs' = map lhsExprType xs
        op' = fmap syntaxExprType op
hsExprType' (RecordCon a con flds) = (ty, RecordCon a (Typed conty, con) flds')
  where flds' = hsRecordBindsType flds
        (ty, _) = hsExprType' $ rcon_con_expr a
        conty = varType $ unLoc con
hsExprType' (RecordUpd a x flds) = (ty, RecordUpd a x' flds')
  where x' = lhsExprType x
        flds' = map (fmap $ hsRecFieldType convertAmbiguousFieldOcc lhsExprType) flds
        ty = case rupd_cons a of
          con_like:_ -> conLikeResTy con_like (rupd_out_tys a)
hsExprType' (ExprWithTySig a x sig) = (ty, ExprWithTySig a x' sig)
  where x'@(Typed ty, _) = lhsExprType x
hsExprType' (ArithSeq a op seqInfo) = (ty, ArithSeq a op' seqInfo')
  where op' = fmap syntaxExprType op
        (el_ty, seqInfo') = arithSeqInfoType seqInfo
        ty = mkListTy el_ty
hsExprType' (HsBracket{}) = invalid
hsExprType' (HsRnBracketOut{}) = invalid
hsExprType' (HsTcBracketOut _a _wrap _bracket _pending) = undefined
hsExprType' (HsSpliceE _a _splice) = invalid
hsExprType' (HsProc a pat cmd) = (ty, HsProc a (lPatType pat) cmd')
  where cmd'@(Typed ty, _) = lhsCmdTopType cmd
hsExprType' (HsStatic a x) = (ty, HsStatic a x')
  where x'@(Typed ty, _) = lhsExprType x
hsExprType' (HsTick a tickish x) = (ty, HsTick a tickish x')
  where x'@(Typed ty, _) = lhsExprType x
hsExprType' (HsBinTick a n m x) = (ty, HsBinTick a n m x')
  where x'@(Typed ty, _) = lhsExprType x
hsExprType' (HsPragE a prag x) = (ty, HsPragE a (convertPrag prag) x')
  where x'@(Typed ty, _) = lhsExprType x
hsExprType' (XExpr (WrapExpr (HsWrap wrap x))) = (undefined, XExpr (WrapExpr (HsWrap wrap x')))
  where (_ty, x') = hsExprType' x
hsExprType' (XExpr (ExpansionExpr (HsExpanded rn tc))) = (ty, XExpr $ ExpansionExpr $ HsExpanded rn tc')
  where (ty, tc') = hsExprType' tc

invalid :: a
invalid = error "invalid"

convertPrag :: HsPragE GhcTc -> HsPragE (With Typed GhcTc)
convertPrag (HsPragSCC a b c) = HsPragSCC a b c
convertPrag (HsPragTick a b c d) = HsPragTick a b c d

convertLit1 :: HsLit GhcTc -> HsLit (With Typed GhcTc)
convertLit1 (HsChar a x)       = HsChar a x
convertLit1 (HsCharPrim a x)   = HsCharPrim a x
convertLit1 (HsString a x)     = HsString a x
convertLit1 (HsStringPrim a x) = HsStringPrim a x
convertLit1 (HsInt a x)        = HsInt a x
convertLit1 (HsIntPrim a x)    = HsIntPrim a x
convertLit1 (HsWordPrim a x)   = HsWordPrim a x
convertLit1 (HsInt64Prim a x)  = HsInt64Prim a x
convertLit1 (HsWord64Prim a x) = HsWord64Prim a x
convertLit1 (HsInteger a x b)  = HsInteger a x b
convertLit1 (HsRat a x b)      = HsRat a x b
convertLit1 (HsFloatPrim a x)  = HsFloatPrim a x
convertLit1 (HsDoublePrim a x) = HsDoublePrim a x

convertAmbiguousFieldOcc :: AmbiguousFieldOcc GhcTc -> AmbiguousFieldOcc (With Typed GhcTc)
convertAmbiguousFieldOcc (Unambiguous a b) = Unambiguous a b
convertAmbiguousFieldOcc (Ambiguous a b) = Ambiguous a b

hsOverLitType :: HsOverLit GhcTc -> HsOverLit (With Typed GhcTc)
hsOverLitType (OverLit ext val witness) = OverLit ext val (hsExprType witness)

hsRecordBindsType :: HsRecordBinds GhcTc -> HsRecordBinds (With Typed GhcTc)
hsRecordBindsType = hsRecFieldsType convertFieldOcc lhsExprType

arithSeqInfoType :: ArithSeqInfo GhcTc -> (Type, ArithSeqInfo (With Typed GhcTc))
arithSeqInfoType (From x) =
    let x'@(Typed ty, _) = lhsExprType x
    in (ty, From x')
arithSeqInfoType (FromThen x y) =
    let x'@(Typed ty, _) = lhsExprType x
        y' = lhsExprType y
    in (ty, FromThen x' y')
arithSeqInfoType (FromTo x y) =
    let x'@(Typed ty, _) = lhsExprType x
        y' = lhsExprType y
    in (ty, FromTo x' y')
arithSeqInfoType (FromThenTo x y z) =
    let x'@(Typed ty, _) = lhsExprType x
        y' = lhsExprType y
        z' = lhsExprType z
    in (ty, FromThenTo x' y' z')

convertFieldOcc :: (XCFieldOcc pass ~ XCFieldOcc pass', XXFieldOcc pass ~ XXFieldOcc pass')
                => FieldOcc pass -> FieldOcc pass'
convertFieldOcc (FieldOcc a b) = FieldOcc a b
convertFieldOcc (XFieldOcc ext) = XFieldOcc ext

hsRecFieldsType :: (FieldOcc p -> FieldOcc p')
                -> (body -> body')
                -> HsRecFields p body
                -> HsRecFields p' body'
hsRecFieldsType f g (HsRecFields {..}) =
    HsRecFields { rec_flds = map (fmap (hsRecFieldType f g)) rec_flds, .. }

hsRecFieldType :: (id -> id')
               -> (body -> body')
               -> HsRecField' id body
               -> HsRecField' id' body'
hsRecFieldType f g (HsRecField {..}) =
    HsRecField { hsRecFieldArg = g hsRecFieldArg
               , hsRecFieldLbl = fmap f hsRecFieldLbl
               , .. }

lPatType :: LPat GhcTc -> LPat (With Typed GhcTc)
lPatType (L l p) = (Typed t, L l p')
  where
    (t, p') = patType' p

patType :: Pat GhcTc -> Pat (With Typed GhcTc)
patType = snd . patType'

patType' :: Pat GhcTc -> (Type, Pat (With Typed GhcTc))
patType' (WildPat ty)              = (ty, WildPat ty)
patType' (VarPat a v)              = (ty, VarPat a (Typed ty , v))
  where ty = varType $ unLoc v
patType' (LazyPat a pat)           = (ty, LazyPat a pat')
  where pat'@(Typed ty, _) = lPatType pat
patType' (AsPat a id pat)          = (ty, AsPat a (Typed ty, id) (lPatType pat))
  where ty = varType $ unLoc id
patType' (ParPat a pat)            = (ty, ParPat a pat')
  where pat'@(Typed ty, _) = lPatType pat
patType' (BangPat a pat)           = (ty, BangPat a pat')
  where pat'@(Typed ty, _) = lPatType pat
patType' (ListPat a pats)          = (ty, ListPat a (map lPatType pats))
  where
    ty = case a of
      ListPatTc ty Nothing -> ty
      ListPatTc _ (Just (ty, _)) -> ty
patType' (TuplePat tys pats box)   = (mkTupleTy1 box tys, TuplePat tys (map lPatType pats) box)
patType' (SumPat a pat tag arity)  = (mkSumTy a, SumPat a (lPatType pat) tag arity)
patType' (ConPat a lcon dets)      = (conLikeResTy (unLoc lcon) (cpt_arg_tys a), ConPat a (Typed (), lcon) (detsType dets))
patType' (ViewPat ty f pat)        = (ty, ViewPat ty (lhsExprType f) (lPatType pat))
patType' (LitPat a lit)            = (hsLitType lit, LitPat a (convertLit1 lit))
patType' (NPat ty (L l a) b c)     = (ty, NPat ty (Typed (), L l $ hsOverLitType a) b c)
patType' (NPlusKPat ty a (L l b) c d e)  = (ty, NPlusKPat ty (Typed aty, a) (Typed (), L l $ hsOverLitType b) (hsOverLitType c) d e)
  where aty = varType $ unLoc a
patType' (SigPat ty p sig)         = (ty, SigPat ty (lPatType p) sig)
patType' (SplicePat _a _splice)      = (undefined , undefined)
patType' (XPat (CoPat wrap inner ty)) = (ty, XPat (CoPat wrap inner' ty))
  where inner' = patType inner

detsType :: HsConPatDetails GhcTc -> HsConPatDetails (With Typed GhcTc)
detsType = convertHsConDets lPatType (hsRecFieldsType convertFieldOcc lPatType)

convertHsConDets :: (a -> a') -> (b -> b') -> HsConDetails a b -> HsConDetails a' b'
convertHsConDets f _ (PrefixCon pats) = PrefixCon (map f pats)
convertHsConDets f _ (InfixCon a b) = InfixCon (f a) (f b)
convertHsConDets _ g (RecCon rec) = RecCon (g rec)

matchGroupType :: (body -> body')
               -> MatchGroup GhcTc body
               -> MatchGroup (With Typed GhcTc) body'
matchGroupType f (MG ext alts origin) =
    MG ext (Typed (), fmap (map (\a -> (Typed (), fmap (matchType f) a))) alts) origin

matchType :: (body -> body')
          -> Match GhcTc body
          -> Match (With Typed GhcTc) body'
matchType f (Match ext ctxt pats grhss) =
    Match ext ctxt (map lPatType pats) grhss'
  where
    grhss' = hsGRHSsType f grhss

hsGRHSsType :: (body -> body')
            -> GRHSs GhcTc body
            -> GRHSs (With Typed GhcTc) body'
hsGRHSsType f (GRHSs ext grhss binds) =
    GRHSs ext grhss' binds'
  where
    binds' = (Typed (), fmap hsLocalBindsTypes binds)
    grhss' = (map (\a -> (Typed (), fmap (hsGRHSType f) a)) grhss)

hsGRHSType :: (body -> body')
           -> GRHS GhcTc body
           -> GRHS (With Typed GhcTc) body'
hsGRHSType f (GRHS ext guards body) =
    GRHS ext guards' (f body)
  where
    guards' = map (\x ->(Typed (), fmap (stmtType lhsExprType) x)) guards

stmtType :: (body -> body')
         -> Stmt GhcTc body
         -> Stmt (With Typed GhcTc) body'
stmtType f (LastStmt ext body stripped returnOp) =
    LastStmt ext (f body) stripped (syntaxExprType returnOp)
stmtType f (BindStmt ext pat body) =
    BindStmt ext (lPatType pat) (f body)
stmtType _ (ApplicativeStmt ext args joinOp) =
    ApplicativeStmt ext args' (fmap syntaxExprType joinOp)
  where
    args' = [ (syntaxExprType op, applicativeArgType arg)
            | (op, arg) <- args
            ]
stmtType f (BodyStmt ext body thenOp guardOp) =
    BodyStmt ext (f body) (syntaxExprType thenOp) (syntaxExprType guardOp)
stmtType _ (LetStmt ext localBinds) =
    LetStmt ext (Typed (), fmap hsLocalBindsTypes localBinds)
stmtType _ (ParStmt ext blocks mzip bindOp) =
    ParStmt ext blocks' (hsExprType mzip) (syntaxExprType bindOp)
  where
    blocks' = map parStmtBlockType blocks
stmtType _ (TransStmt {..}) =
    TransStmt { trS_stmts = map ((Typed (),) . fmap (stmtType lhsExprType)) trS_stmts
              , trS_using = lhsExprType trS_using
              , trS_by    = fmap lhsExprType trS_by
              , trS_ret   = syntaxExprType trS_ret
              , trS_bind  = syntaxExprType trS_bind
              , trS_fmap  = hsExprType trS_fmap
              , .. }
stmtType f (RecStmt {..}) =
    RecStmt { recS_stmts   = map ((Typed (),) . fmap (stmtType f)) recS_stmts
            , recS_bind_fn = syntaxExprType recS_bind_fn
            , recS_ret_fn  = syntaxExprType recS_ret_fn
            , recS_mfix_fn = syntaxExprType recS_mfix_fn
            , .. }

parStmtBlockType :: ParStmtBlock GhcTc GhcTc -> ParStmtBlock (With Typed GhcTc) (With Typed GhcTc)
parStmtBlockType (ParStmtBlock ext stmts bndrs returnOp) =
    ParStmtBlock ext (map (\x -> (Typed (),  fmap (stmtType lhsExprType) x)) stmts) bndrs (syntaxExprType returnOp)

syntaxExprType :: SyntaxExpr GhcTc -> SyntaxExpr (With Typed GhcTc)
syntaxExprType = snd . syntaxExprType'

syntaxExprType' :: SyntaxExpr GhcTc -> (Maybe Type, SyntaxExpr (With Typed GhcTc))
syntaxExprType' (SyntaxExprTc expr arg_wraps res_wrap) =
    (Just ty, SyntaxExprTc expr arg_wraps res_wrap)
  where
    (ty, _) = hsExprType' expr
syntaxExprType' NoSyntaxExprTc = (Nothing, NoSyntaxExprTc)

lhsTupArgType :: LHsTupArg GhcTc -> (Type, Located (HsTupArg (With Typed GhcTc)))
lhsTupArgType (L loc (Present ext x)) =
    let x'@(Typed ty, _) = lhsExprType x
    in (ty, L loc (Present ext x'))
lhsTupArgType (L loc (Missing a@(Scaled _ ty))) =
    (ty, L loc (Missing a))

applicativeArgType :: ApplicativeArg GhcTc -> ApplicativeArg (With Typed GhcTc)
applicativeArgType (ApplicativeArgOne {..}) =
    ApplicativeArgOne { app_arg_pattern = lPatType app_arg_pattern
                      , arg_expr = lhsExprType arg_expr
                      , xarg_app_arg_one = syntaxExprType <$> xarg_app_arg_one
                      , .. }
applicativeArgType (ApplicativeArgMany {..}) =
    ApplicativeArgMany { app_stmts = map (\x -> (Typed () , fmap (stmtType lhsExprType) x)) app_stmts
                       , final_expr = hsExprType final_expr
                       , bv_pattern = lPatType bv_pattern
                       , .. }

hsLocalBindsTypes :: HsLocalBinds GhcTc -> HsLocalBinds (With Typed GhcTc)
hsLocalBindsTypes (HsValBinds ext valBinds) =
    HsValBinds ext (hsValBindsTypes valBinds)
hsLocalBindsTypes (HsIPBinds ext ipBinds) =
    HsIPBinds ext (hsIPBindsTypes ipBinds)
hsLocalBindsTypes (EmptyLocalBinds ext) =
    EmptyLocalBinds ext

hsIPBindsTypes :: HsIPBinds GhcTc -> HsIPBinds (With Typed GhcTc)
hsIPBindsTypes (IPBinds ext binds) = IPBinds ext (map (\x -> (Typed (), fmap ipBindType x)) binds)
  where
    ipBindType (IPBind ext bndr x) = IPBind ext bndr' (lhsExprType x)
      where bndr' = case bndr of
              Left x -> Left (Typed (), x)
              Right x -> Right x

hsValBindsTypes :: HsValBinds GhcTc -> HsValBinds (With Typed GhcTc)
hsValBindsTypes (ValBinds ext binds _sigs) =
    ValBinds ext (lhsBindsTypes binds) []
hsValBindsTypes (XValBindsLR (NValBinds xs ys)) = XValBindsLR (NValBinds (map (fmap lhsBindsTypes) xs) ys)

lhsBindsTypes :: LHsBinds GhcTc -> LHsBinds (With Typed GhcTc)
lhsBindsTypes bs = fmap (\x -> (Typed (), fmap hsBindType x)) bs

hsBindType :: HsBind GhcTc -> HsBind (With Typed GhcTc)
hsBindType (FunBind ext id matches ticks) =
    FunBind ext (Typed ty, id) matches' ticks
  where
    matches' = matchGroupType lhsExprType matches
    ty = varType $ unLoc id
hsBindType (PatBind ext lhs rhs ticks) =
    PatBind ext lhs' rhs' ticks
  where
    lhs' = lPatType lhs
    rhs' = hsGRHSsType lhsExprType rhs
hsBindType (VarBind ext id rhs) =
    VarBind ext id (lhsExprType rhs)
hsBindType (AbsBinds {..}) =
    AbsBinds { abs_binds = fmap (\x -> (Typed (), fmap hsBindType x)) abs_binds
             , abs_exports = map convertABExport abs_exports
             , .. }
hsBindType (PatSynBind ext bind) =
    PatSynBind ext (convertPatSynBind bind)

convertPatSynBind :: PatSynBind GhcTc GhcTc -> PatSynBind (With Typed GhcTc) (With Typed GhcTc)
convertPatSynBind (PSB ext id args def dir) =
  PSB ext (Typed ty, id) (convertArgs args) (lPatType def) (convertPatSynDir dir)
  where ty = varType $ unLoc id

convertPatSynDir :: HsPatSynDir GhcTc -> HsPatSynDir (With Typed GhcTc)
convertPatSynDir Unidirectional = Unidirectional
convertPatSynDir ImplicitBidirectional = ImplicitBidirectional
convertPatSynDir (ExplicitBidirectional mg) = ExplicitBidirectional $ matchGroupType lhsExprType mg

convertArgs :: HsPatSynDetails (Located Id) -> HsPatSynDetails (Typed Var, Located Var)
convertArgs = convertHsConDets (\x -> (Typed $ varType $ unLoc x, x)) (map convertRecordPatSynField)

convertRecordPatSynField :: RecordPatSynField (Located Id) -> RecordPatSynField (Typed Var, Located Var)
convertRecordPatSynField (RecordPatSynField a b) = RecordPatSynField (Typed ta, a) (Typed tb, b)
  where ta = varType $ unLoc a
        tb = varType $ unLoc b

convertABExport :: ABExport GhcTc -> ABExport (With Typed GhcTc)
convertABExport (ABE ext poly mono wrap prags) = ABE ext poly mono wrap prags

lhsCmdTopType :: LHsCmdTop GhcTc -> LHsCmdTop (With Typed GhcTc)
lhsCmdTopType (L loc (HsCmdTop ext cmd)) =
    let cmd' = lhsCmdType cmd
        ty = case ext of CmdTopTc _ ret _ -> ret
    in (Typed ty, L loc (HsCmdTop ext cmd'))

lhsCmdType :: LHsCmd GhcTc -> LHsCmd (With Typed GhcTc)
lhsCmdType cmd = (Typed (), fmap hsCmdType cmd)

hsCmdType :: HsCmd GhcTc -> HsCmd (With Typed GhcTc)
hsCmdType cmd =
  case cmd of
    HsCmdArrApp ext f arg ty order ->
      HsCmdArrApp ext (lhsExprType f) (lhsExprType arg) ty order
    HsCmdArrForm ext op lexical_fixity fixity cmds ->
      let cmds' = map lhsCmdTopType cmds
      in HsCmdArrForm ext (lhsExprType op) lexical_fixity fixity cmds'
    HsCmdApp ext cmd x ->
      HsCmdApp ext (Typed (), (fmap hsCmdType cmd)) (lhsExprType x)
    HsCmdLam ext matches ->
      HsCmdLam ext (matchGroupType lhsCmdType matches)
    HsCmdLamCase ext matches ->
      HsCmdLam ext (matchGroupType lhsCmdType matches)
    HsCmdPar ext cmd ->
      HsCmdPar ext (lhsCmdType cmd)
    HsCmdCase ext x matches ->
      let x' = lhsExprType x
          matches' = matchGroupType lhsCmdType matches
      in HsCmdCase ext x' matches'
    HsCmdIf ext op pred t e ->
      let op' = syntaxExprType op
          pred' = lhsExprType pred
          t' = lhsCmdType t
          e' = lhsCmdType e
      in HsCmdIf ext op' pred' t' e'
    HsCmdLet ext binds cmd ->
      let binds' = fmap hsLocalBindsTypes binds
          cmd' = lhsCmdType cmd
      in HsCmdLet ext (Typed (), binds') cmd'
    HsCmdDo ext stmts ->
      let stmts' = fmap (map (\x -> (Typed (), fmap (stmtType lhsCmdType) x))) stmts
      in HsCmdDo ext (Typed (), stmts')
    XCmd (HsWrap wrap cmd) -> XCmd (HsWrap wrap $ hsCmdType cmd)
