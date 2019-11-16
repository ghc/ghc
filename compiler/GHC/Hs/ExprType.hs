{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Hs.ExprType (project, hsExprType) where

import Id (idType)
import PatSyn (patSynBuilder)
import TysWiredIn (anyTy, mkTupleTy, mkListTy)
import DataCon (dataConUserType)
import Type
import TcHsSyn (hsLitType)

import GHC.Hs.Lit (HsOverLit(..), overLitType, convertLit)
import GHC.Hs.Extension
import GHC.Hs.Expr
import GHC.Hs.Types (FieldOcc, convertFieldOcc, convertAmbiguousFieldOcc)
import GHC.Hs.Binds
import GHC.Hs.Pat
import ConLike
import SrcLoc

-- | A pass which annotates all nodes in an 'HsExpr' with
data With a pass

-- HsExpr extension points
type instance XVar               (With a pass) = (a, XVar            pass)
type instance XUnboundVar        (With a pass) = (a, XUnboundVar     pass)
type instance XConLikeOut        (With a pass) = (a, XConLikeOut     pass)
type instance XRecFld            (With a pass) = (a, XRecFld         pass)
type instance XOverLabel         (With a pass) = (a, XOverLabel      pass)
type instance XIPVar             (With a pass) = (a, XIPVar          pass)
type instance XOverLitE          (With a pass) = (a, XOverLitE       pass)
type instance XLitE              (With a pass) = (a, XLitE           pass)
type instance XLam               (With a pass) = (a, XLam            pass)
type instance XLamCase           (With a pass) = (a, XLamCase        pass)
type instance XApp               (With a pass) = (a, XApp            pass)
type instance XAppTypeE          (With a pass) = (a, XAppTypeE       pass)
type instance XOpApp             (With a pass) = (a, XOpApp          pass)
type instance XNegApp            (With a pass) = (a, XNegApp         pass)
type instance XPar               (With a pass) = (a, XPar            pass)
type instance XSectionL          (With a pass) = (a, XSectionL       pass)
type instance XSectionR          (With a pass) = (a, XSectionR       pass)
type instance XExplicitTuple     (With a pass) = (a, XExplicitTuple  pass)
type instance XExplicitSum       (With a pass) = (a, XExplicitSum    pass)
type instance XCase              (With a pass) = (a, XCase           pass)
type instance XIf                (With a pass) = (a, XIf             pass)
type instance XMultiIf           (With a pass) = (a, XMultiIf        pass)
type instance XLet               (With a pass) = (a, XLet            pass)
type instance XDo                (With a pass) = (a, XDo             pass)
type instance XExplicitList      (With a pass) = (a, XExplicitList   pass)
type instance XRecordCon         (With a pass) = (a, XRecordCon      pass)
type instance XRecordUpd         (With a pass) = (a, XRecordUpd      pass)
type instance XExprWithTySig     (With a pass) = (a, XExprWithTySig  pass)
type instance XArithSeq          (With a pass) = (a, XArithSeq       pass)
type instance XSCC               (With a pass) = (a, XSCC            pass)
type instance XCoreAnn           (With a pass) = (a, XCoreAnn        pass)
type instance XBracket           (With a pass) = (a, XBracket        pass)
type instance XRnBracketOut      (With a pass) = (a, XRnBracketOut   pass)
type instance XTcBracketOut      (With a pass) = (a, XTcBracketOut   pass)
type instance XSpliceE           (With a pass) = (a, XSpliceE        pass)
type instance XProc              (With a pass) = (a, XProc           pass)
type instance XStatic            (With a pass) = (a, XStatic         pass)
type instance XTick              (With a pass) = (a, XTick           pass)
type instance XBinTick           (With a pass) = (a, XBinTick        pass)
type instance XTickPragma        (With a pass) = (a, XTickPragma     pass)
type instance XWrap              (With a pass) = (a, XWrap           pass)
type instance XXExpr             (With a pass) = (a, XXExpr          pass)

-- Other extension points
type instance IdP                (With a pass) = IdP pass
type instance XCIPBind           (With a pass) = XCIPBind pass
type instance XOverLit           (With a pass) = XOverLit pass
type instance XCFieldOcc         (With a pass) = XCFieldOcc pass
type instance XApplicativeArgOne (With a pass) = XApplicativeArgOne pass
type instance XApplicativeArgMany (With a pass) = XApplicativeArgMany pass
type instance XUnambiguous       (With a pass) = XUnambiguous pass
type instance XABE               (With a pass) = XABE pass

-- Pat extension points
type instance XWildPat           (With a pass) = XWildPat pass
type instance XVarPat            (With a pass) = XVarPat pass
type instance XLazyPat           (With a pass) = XLazyPat pass
type instance XAsPat             (With a pass) = XAsPat pass
type instance XParPat            (With a pass) = XParPat pass
type instance XBangPat           (With a pass) = XBangPat pass
type instance XListPat           (With a pass) = XListPat pass
type instance XTuplePat          (With a pass) = XTuplePat pass
type instance XSumPat            (With a pass) = XSumPat pass
type instance XViewPat           (With a pass) = XViewPat pass
type instance XLitPat            (With a pass) = XLitPat pass

-- Bind extension points
type instance XFunBind           (With a pass) (With a pass) = XFunBind pass pass
type instance XPatBind           (With a pass) (With a pass) = XPatBind pass pass
type instance XVarBind           (With a pass) (With a pass) = XVarBind pass pass
type instance XAbsBinds          (With a pass) (With a pass) = XAbsBinds pass pass
type instance XValBinds          (With a pass) (With a pass) = XValBinds pass pass
type instance XHsValBinds        (With a pass) (With a pass) = XHsValBinds pass pass
type instance XHsIPBinds         (With a pass) (With a pass) = XHsIPBinds pass pass

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
type instance XRec               (With a pass) f = Located (f (With a pass))

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


project :: HsExpr (With a p) -> a
project expr =
  case expr of
    HsVar a _id                              -> fst a
    HsUnboundVar a _id                       -> fst a
    HsConLikeOut a _conlike                  -> fst a
    HsRecFld a _                             -> fst a
    HsOverLabel a _ _                        -> fst a
    HsIPVar a _                              -> fst a
    HsOverLit a _lit                         -> fst a
    HsLit a _lit                             -> fst a
    HsLam a _match                           -> fst a
    HsLamCase a _match                       -> fst a
    HsApp a _f _x                            -> fst a
    HsAppType a _f _t                        -> fst a
    OpApp a _f _x _y                         -> fst a
    NegApp a _x _op                          -> fst a
    HsPar a _x                               -> fst a
    SectionL a _x _f                         -> fst a
    SectionR a _f _x                         -> fst a
    ExplicitTuple a _args _box               -> fst a
    ExplicitSum a _tag _arity _x             -> fst a
    HsCase a _scrut _match                   -> fst a
    HsIf a _op _pred _t _e                   -> fst a
    HsMultiIf a _grhss                       -> fst a
    HsLet a _binds _body                     -> fst a
    HsDo a _ctxt _stmts                      -> fst a
    ExplicitList a _op _xs                   -> fst a
    RecordCon a _con _flds                   -> fst a
    RecordUpd a _x _flds                     -> fst a
    ExprWithTySig a _x _sig                  -> fst a
    ArithSeq a _op _seqInfo                  -> fst a
    HsSCC a _stxt _name _x                   -> fst a
    HsCoreAnn a _stxt _name _x               -> fst a
    HsBracket a _bracket                     -> fst a
    HsRnBracketOut a _bracket _pending       -> fst a
    HsTcBracketOut a _bracket _pending       -> fst a
    HsSpliceE a _splice                      -> fst a
    HsProc a _pat _cmd                       -> fst a
    HsStatic a _x                            -> fst a
    HsTick a _tickish _x                     -> fst a
    HsBinTick a _n _m _x                     -> fst a
    HsTickPragma a _stxt _stuff _stuff' _x   -> fst a
    HsWrap a _wrapper _x                     -> fst a
    XExpr _                                  -> undefined

lhsExprType :: LHsExpr GhcTc -> LHsExpr (With Type GhcTc)
lhsExprType = fmap hsExprType

hsExprType :: HsExpr GhcTc -> HsExpr (With Type GhcTc)
hsExprType = snd . hsExprType'

lhsExprType' :: LHsExpr GhcTc -> (Type, LHsExpr (With Type GhcTc))
lhsExprType' (L loc expr) =
    case hsExprType' expr of (ty, expr') -> (ty, L loc expr')

hsExprType' :: HsExpr GhcTc -> (Type, HsExpr (With Type GhcTc))
hsExprType' expr =
  case expr of
    HsVar a id               -> let ty = idType $ unLoc id
                                in (ty, HsVar (ty, a) id)
    HsUnboundVar a id        -> (anyTy, HsUnboundVar (anyTy, a) id)
    HsConLikeOut a conlike   ->
      let ty = case conlike of
            RealDataCon con -> dataConUserType con
            PatSynCon patsyn
              | Just (id, _) <- patSynBuilder patsyn -> idType id
              | otherwise    -> invalid
      in (ty, HsConLikeOut (ty, a) conlike)
    HsRecFld _ _             -> invalid
    HsOverLabel _ _ _        -> invalid
    HsIPVar _ _              -> invalid
    HsOverLit a lit          -> let ty = overLitType lit
                                in (ty, HsOverLit (ty, a) (hsOverLitType lit))
    HsLit a lit              -> let ty = hsLitType lit
                                in (ty, HsLit (ty, a) (convertLit lit))
    HsLam a match            -> let match' = matchGroupType lhsExprType match
                                    ty = case mg_ext match' of
                                           MatchGroupTc args res -> mkVisFunTys args res
                                in (ty, HsLam (ty, a) match')
    HsLamCase a match        -> let match' = matchGroupType lhsExprType match
                                    ty = case mg_ext match' of
                                           MatchGroupTc args res -> mkVisFunTys args res
                                in (ty, HsLamCase (ty, a) match')
    HsApp a f x              -> let (f_ty, f') = lhsExprType' f
                                    (x_ty, x') = lhsExprType' x
                                    ty = funResultTy f_ty
                                in (ty, HsApp (ty, a) f' x')
    HsAppType a f t          -> let (f_ty, f') = lhsExprType' f
                                    ty = undefined
                                in (ty, HsAppType (ty, a) f' undefined)
    OpApp a f x y            -> let (f_ty, f') = lhsExprType' f
                                    (x_ty, x') = lhsExprType' x
                                    (y_ty, y') = lhsExprType' y
                                    ty = funResultTy $ funResultTy f_ty
                                in (ty, OpApp (ty, a) f' x' y')
    NegApp a x op            -> let (_, x') = lhsExprType' x
                                    (op_ty, op') = syntaxExprType' op
                                    ty = funResultTy $ op_ty
                                in (ty, NegApp (ty, a) x' op')
    HsPar a x                -> let (ty, x') = lhsExprType' x
                                in (ty, HsPar (ty, a) x')
    SectionL a x f           -> let (f_ty, f') = lhsExprType' f
                                    (x_ty, x') = lhsExprType' x
                                    ty = funResultTy $ f_ty `mkAppTy` x_ty
                                in (ty, SectionL (ty, a) x' f')
    SectionR a f y           -> let (f_ty, f') = lhsExprType' f
                                    (y_ty, y') = lhsExprType' y
                                    x_ty = funArgTy f_ty
                                    ty = x_ty `mkVisFunTy` funResultTy (funResultTy f_ty)
                                in (ty, SectionR (ty, a) f' y')
    ExplicitTuple a args box -> let args' = map lhsTupArgType args
                                    ty = mkTupleTy box (map fst args')
                                in (ty, ExplicitTuple (ty, a) (map snd args') box)
    ExplicitSum a tag arity x
                             -> let (x_ty, x') = lhsExprType' x
                                    ty = mkSumTy a
                                in (ty, ExplicitSum (ty, a) tag arity x')
    HsCase a scrut match     -> let (scrut_ty, scrut') = lhsExprType' scrut
                                    match' = matchGroupType lhsExprType match
                                    ty = case mg_ext match' of
                                           MatchGroupTc args res -> mkVisFunTys args res
                                in (ty, HsCase (ty, a) scrut' match')
    HsIf a op pred t e       -> let op' = fmap syntaxExprType op
                                    pred' = lhsExprType pred
                                    (ty, t') = lhsExprType' t
                                    (_ , e') = lhsExprType' e
                                in (ty, HsIf (ty, a) op' pred' t' e')
    HsMultiIf a grhss        -> let grhss' = map (fmap $ hsGRHSType lhsExprType) grhss
                                    L _ (GRHS _ _ body) : _ = grhss'
                                    ty = project $ unLoc body
                                in (ty, HsMultiIf (ty, a) grhss')
    HsLet a binds body       -> let binds' = fmap hsLocalBindsTypes binds
                                    (ty, body') = lhsExprType' body
                                in (ty, HsLet (ty, a) binds' body')
    HsDo ty ctxt stmts       -> let stmts' = fmap (map (fmap $ stmtType lhsExprType)) stmts
                                in (ty, HsDo (ty, ty) ctxt stmts')
    ExplicitList ty op xs    -> let xs' = map lhsExprType xs
                                    op' = fmap syntaxExprType op
                                in (ty, ExplicitList (ty, ty) op' xs')
    RecordCon a con flds     -> let flds' = hsRecordBindsType flds
                                    (ty, _) = hsExprType' $ rcon_con_expr a
                                in (ty, RecordCon (ty, a) con flds')
    RecordUpd a x flds       -> let (x_ty, x') = lhsExprType' x
                                    flds' = map (fmap $ hsRecFieldType convertAmbiguousFieldOcc lhsExprType) flds
                                    ty = case rupd_cons a of
                                           con_like:_ -> conLikeResTy con_like (rupd_out_tys a)
                                in (ty, RecordUpd (ty, a) x' flds')
    ExprWithTySig a x sig    -> let (ty, x') = lhsExprType' x
                                in (ty, ExprWithTySig (ty, a) x' undefined)
    ArithSeq a op seqInfo    -> let op' = fmap syntaxExprType op
                                    (el_ty, seqInfo') = arithSeqInfoType seqInfo
                                    ty = mkListTy el_ty
                                in (ty, ArithSeq (ty, a) op' seqInfo')
    HsSCC a stxt name x      -> let (ty, x') = lhsExprType' x
                                in (ty, HsSCC (ty, a) stxt name x')
    HsCoreAnn a stxt name x  -> let (ty, x') = lhsExprType' x
                                in (ty, HsCoreAnn (ty, a) stxt name x')
    HsBracket{}              -> invalid
    HsRnBracketOut{}         -> invalid
    HsTcBracketOut a bracket pending
                             -> undefined
    HsSpliceE a splice       -> undefined
    HsProc a pat cmd         -> let (ty, cmd') = lhsCmdTopType cmd
                                in (ty, HsProc (ty, a) (lPatType pat) cmd')
    HsStatic a x             -> let (ty, x') = lhsExprType' x
                                in (ty, HsStatic (ty, a) x')
    HsTick a tickish x       -> let (ty, x') = lhsExprType' x
                                in (ty, HsTick (ty, a) tickish x')
    HsBinTick a n m x        -> let (ty, x') = lhsExprType' x
                                in (ty, HsBinTick (ty, a) n m x')
    HsTickPragma a stxt stuff stuff' x
                             -> let (ty, x') = lhsExprType' x
                                in (ty, HsTickPragma (ty, a) stxt stuff stuff' x')
    HsWrap a wrapper x       -> let (x_ty, x') = hsExprType' x
                                    ty = undefined
                                in (ty, HsWrap (ty, a) wrapper x')
    XExpr _                  -> undefined
  where
    invalid = error "invalid"

lPatType :: LPat GhcTc
         -> LPat (With Type GhcTc)
lPatType = fmap patType

patType :: Pat GhcTc
        -> Pat (With Type GhcTc)
patType pat =
  case pat of
    WildPat a -> WildPat a
    VarPat a v -> VarPat a v
    LazyPat a pat -> LazyPat a (lPatType pat)
    AsPat a id pat -> AsPat a id (lPatType pat)
    ParPat a pat -> ParPat a (lPatType pat)
    BangPat a pat -> BangPat a (lPatType pat)
    ListPat a pats -> ListPat a (map lPatType pats)
    TuplePat a pats box -> TuplePat a (map lPatType pats) box
    SumPat a pat tag arity -> SumPat a (lPatType pat) tag arity
    ConPatIn a details -> undefined
    ConPatOut {..} -> undefined
    ViewPat a f pat -> ViewPat a (lhsExprType f) (lPatType pat)
    SplicePat a splice -> undefined
    LitPat a lit -> LitPat a (convertLit lit)

lhsCmdTopType :: LHsCmdTop GhcTc
              -> (Type, LHsCmdTop (With Type GhcTc))
lhsCmdTopType (L loc (HsCmdTop ext cmd)) =
    let cmd' = lhsCmdType cmd
        ty = case ext of CmdTopTc _ ret _ -> ret
    in (ty, L loc (HsCmdTop ext cmd'))

lhsCmdType :: LHsCmd GhcTc
           -> LHsCmd (With Type GhcTc)
lhsCmdType = fmap hsCmdType

hsCmdType :: HsCmd GhcTc
          -> HsCmd (With Type GhcTc)
hsCmdType cmd =
  case cmd of
    HsCmdArrApp ext f arg ty order ->
      HsCmdArrApp ext (lhsExprType f) (lhsExprType arg) ty order
    HsCmdArrForm ext op lexical_fixity fixity cmds ->
      let cmds' = map (snd . lhsCmdTopType) cmds
      in HsCmdArrForm ext (lhsExprType op) lexical_fixity fixity cmds'
    HsCmdApp ext cmd x ->
      HsCmdApp ext (fmap hsCmdType cmd) (fmap hsExprType x)
    HsCmdLam ext matches ->
      HsCmdLam ext (matchGroupType lhsCmdType matches)
    HsCmdPar ext cmd ->
      HsCmdPar ext (lhsCmdType cmd)
    HsCmdCase ext x matches ->
      let x' = lhsExprType x
          matches' = matchGroupType lhsCmdType matches
      in HsCmdCase ext x' matches'
    HsCmdIf ext op pred t e ->
      let op' = fmap syntaxExprType op
          (pred_ty, pred') = lhsExprType' pred
          t' = lhsCmdType t
          e' = lhsCmdType e
      in HsCmdIf ext op' pred' t' e'
    HsCmdLet ext binds cmd ->
      let binds' = fmap hsLocalBindsTypes binds
          cmd' = lhsCmdType cmd
      in HsCmdLet ext binds' cmd'
    HsCmdDo ext stmts ->
      let stmts' = fmap (map $ fmap $ stmtType lhsCmdType) stmts
      in HsCmdDo ext stmts'
    HsCmdWrap ext wrap cmd ->
      let cmd' = hsCmdType cmd
      in HsCmdWrap ext wrap cmd'

hsRecordBindsType :: HsRecordBinds GhcTc
                  -> HsRecordBinds (With Type GhcTc)
hsRecordBindsType = hsRecFieldsType convertFieldOcc lhsExprType

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

lhsTupArgType :: LHsTupArg GhcTc
              -> (Type, LHsTupArg (With Type GhcTc))
lhsTupArgType (L loc (Present ext x)) =
    let (ty, x') = lhsExprType' x
    in (ty, L loc (Present ext x'))
lhsTupArgType (L loc (Missing ty)) =
    (ty, L loc (Missing ty))

arithSeqInfoType :: ArithSeqInfo GhcTc
                 -> (Type, ArithSeqInfo (With Type GhcTc))
arithSeqInfoType (From x) =
    let (ty, x') = lhsExprType' x
    in (ty, From x')
arithSeqInfoType (FromThen x y) =
    let (ty, x') = lhsExprType' x
        (_,  y') = lhsExprType' y
    in (ty, FromThen x' y')
arithSeqInfoType (FromTo x y) =
    let (ty, x') = lhsExprType' x
        (_,  y') = lhsExprType' y
    in (ty, FromTo x' y')
arithSeqInfoType (FromThenTo x y z) =
    let (ty, x') = lhsExprType' x
        (_, y')  = lhsExprType' y
        (_, z')  = lhsExprType' z
    in (ty, FromThenTo x' y' z')

matchGroupType :: (body -> body')
               -> MatchGroup GhcTc body
               -> MatchGroup (With Type GhcTc) body'
matchGroupType f (MG ext alts origin) =
    MG ext alts' origin
  where
    alts' = fmap (map $ fmap (matchType f)) alts

matchType :: (body -> body')
          -> Match GhcTc body
          -> Match (With Type GhcTc) body'
matchType f (Match ext ctxt pats grhss) =
    Match ext ctxt (map lPatType pats) grhss'
  where
    grhss' = hsGRHSsType f grhss

hsGRHSsType :: (body -> body')
            -> GRHSs GhcTc body
            -> GRHSs (With Type GhcTc) body'
hsGRHSsType f (GRHSs ext grhss binds) =
    GRHSs ext grhss' binds'
  where
    binds' = fmap hsLocalBindsTypes binds
    grhss' = map (fmap (hsGRHSType f)) grhss

hsGRHSType :: (body -> body')
           -> GRHS GhcTc body
           -> GRHS (With Type GhcTc) body'
hsGRHSType f (GRHS ext guards body) =
    GRHS ext guards' (f body)
  where
    guards' = map (fmap $ stmtType lhsExprType) guards

stmtType :: (body -> body')
         -> Stmt GhcTc body
         -> Stmt (With Type GhcTc) body'
stmtType f (LastStmt ext body stripped returnOp) =
    LastStmt ext (f body) stripped (syntaxExprType returnOp)
stmtType f (BindStmt ext pat body bindOp failOp) =
    BindStmt ext (lPatType pat) (f body) (syntaxExprType bindOp) (syntaxExprType failOp)
stmtType f (ApplicativeStmt ext args joinOp) =
    ApplicativeStmt ext args' (fmap syntaxExprType joinOp)
  where
    args' = [ (syntaxExprType op, applicativeArgType arg)
            | (op, arg) <- args
            ]
stmtType f (BodyStmt ext body thenOp guardOp) =
    BodyStmt ext (f body) (syntaxExprType thenOp) (syntaxExprType guardOp)
stmtType f (LetStmt ext localBinds) =
    LetStmt ext (fmap hsLocalBindsTypes localBinds)
stmtType f (ParStmt ext blocks mzip bindOp) =
    ParStmt ext blocks' (hsExprType mzip) (syntaxExprType bindOp)
  where
    blocks' = map parStmtBlockType blocks
stmtType f (TransStmt {..}) =
    TransStmt { trS_stmts = map (fmap (stmtType lhsExprType)) trS_stmts
              , trS_using = lhsExprType trS_using
              , trS_by    = fmap lhsExprType trS_by
              , trS_ret   = syntaxExprType trS_ret
              , trS_bind  = syntaxExprType trS_bind
              , trS_fmap  = hsExprType trS_fmap
              , .. }
stmtType f (RecStmt {..}) =
    RecStmt { recS_stmts   = map (fmap (stmtType f)) recS_stmts
            , recS_bind_fn = syntaxExprType recS_bind_fn
            , recS_ret_fn  = syntaxExprType recS_ret_fn
            , recS_mfix_fn = syntaxExprType recS_mfix_fn
            , .. }

parStmtBlockType :: ParStmtBlock GhcTc GhcTc
                 -> ParStmtBlock (With Type GhcTc) (With Type GhcTc)
parStmtBlockType (ParStmtBlock ext stmts bndrs returnOp) =
    ParStmtBlock ext (map (fmap (stmtType lhsExprType)) stmts) bndrs (syntaxExprType returnOp)

applicativeArgType :: ApplicativeArg GhcTc
                   -> ApplicativeArg (With Type GhcTc)
applicativeArgType (ApplicativeArgOne {..}) =
    ApplicativeArgOne { app_arg_pattern = lPatType app_arg_pattern
                      , arg_expr = lhsExprType arg_expr
                      , fail_operator = syntaxExprType fail_operator
                      , .. }
applicativeArgType (ApplicativeArgMany {..}) =
    ApplicativeArgMany { app_stmts = map (fmap $ stmtType lhsExprType) app_stmts
                       , final_expr = hsExprType final_expr
                       , bv_pattern = lPatType bv_pattern
                       , .. }

hsLocalBindsTypes :: HsLocalBinds GhcTc
                  -> HsLocalBinds (With Type GhcTc)
hsLocalBindsTypes (HsValBinds ext valBinds) =
    HsValBinds ext (hsValBindsTypes valBinds)
hsLocalBindsTypes (HsIPBinds ext ipBinds) =
    HsIPBinds ext (hsIPBindsTypes ipBinds)
hsLocalBindsTypes (EmptyLocalBinds ext) =
    EmptyLocalBinds ext

hsIPBindsTypes :: HsIPBinds GhcTc
               -> HsIPBinds (With Type GhcTc)
hsIPBindsTypes (IPBinds ext binds) = IPBinds ext (map (fmap ipBindType) binds)
  where
    ipBindType (IPBind ext bndr x) = IPBind ext bndr (lhsExprType x)

hsValBindsTypes :: HsValBinds GhcTc
                -> HsValBinds (With Type GhcTc)
hsValBindsTypes (ValBinds ext binds sigs) =
    ValBinds ext (lhsBindsTypes binds) (map convertLSig sigs)

lhsBindsTypes :: LHsBinds GhcTc
              -> LHsBinds (With Type GhcTc)
lhsBindsTypes = fmap (fmap hsBindType)

hsBindType :: HsBind GhcTc
           -> HsBind (With Type GhcTc)
hsBindType (FunBind ext id matches co_fn ticks) =
    FunBind ext id matches' co_fn ticks
  where
    matches' = matchGroupType lhsExprType matches
hsBindType (PatBind ext lhs rhs ticks) =
    PatBind ext lhs' rhs' ticks
  where
    lhs' = lPatType lhs
    rhs' = hsGRHSsType lhsExprType rhs
hsBindType (VarBind ext id rhs inline) =
    VarBind ext id (lhsExprType rhs) inline
hsBindType (AbsBinds {..}) =
    AbsBinds { abs_binds = fmap (fmap hsBindType) abs_binds
             , abs_exports = map convertABExport abs_exports
             , .. }
hsBindType (PatSynBind ext bind) =
    PatSynBind ext undefined

syntaxExprType :: SyntaxExpr GhcTc
               -> SyntaxExpr (With Type GhcTc)
syntaxExprType = snd . syntaxExprType'

syntaxExprType' :: SyntaxExpr GhcTc
                -> (Type, SyntaxExpr (With Type GhcTc))
syntaxExprType' (SyntaxExpr expr arg_wraps res_wrap) =
    (ty, SyntaxExpr expr' arg_wraps res_wrap)
  where
    (ty, expr') = hsExprType' expr

hsOverLitType :: HsOverLit GhcTc -> HsOverLit (With Type GhcTc)
hsOverLitType (OverLit ext val witness) = OverLit ext val (hsExprType witness)
