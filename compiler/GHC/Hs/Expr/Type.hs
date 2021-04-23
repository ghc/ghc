{-# LANGUAGE EmptyCase #-}
-- | Compute the 'Type' of an @'HsExpr' 'GhcTc'@ in a pure fashion.
--
-- Note that this does /not/ currently support the use case of annotating
-- every subexpression in an 'HsExpr' with its 'Type'. For more information on
-- this task, see #12706, #15320, #16804, and #17331.
module GHC.Hs.Expr.Type (lhsExprType, hsExprType) where

import GHC.Prelude

import GHC.Builtin.Types
import GHC.Core.Coercion
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.PatSyn
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Core.Utils
import GHC.Hs
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.Zonk
import GHC.Types.Id
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic

-- | Compute the 'Type' of an @'LHsExpr' 'GhcTc'@ in a pure fashion.
lhsExprType :: LHsExpr GhcTc -> Type
lhsExprType (L _ e) = hsExprType e

-- | Compute the 'Type' of an @'HsExpr' 'GhcTc'@ in a pure fashion.
hsExprType :: HsExpr GhcTc -> Type
hsExprType (HsVar _ (L _ id)) = idType id
hsExprType (HsUnboundVar (HER _ ty _) _) = ty
hsExprType (HsRecFld _ af) = idType $ selectorAmbiguousFieldOcc af
hsExprType (HsOverLabel v _) = case v of {}
hsExprType e@(HsIPVar{}) = pprPanic "hsExprType: HsIPVar present after typechecking"
                                    (ppr e)
hsExprType (HsOverLit _ lit) = overLitType lit
hsExprType (HsLit _ lit) = hsLitType lit
hsExprType (HsLam     _ (MG { mg_ext = match_group })) = matchGroupTcType match_group
hsExprType (HsLamCase _ (MG { mg_ext = match_group })) = matchGroupTcType match_group
hsExprType (HsApp _ f _) = funResultTy $ lhsExprType f
hsExprType (HsAppType x f _) = piResultTy (lhsExprType f) x
hsExprType (OpApp v _ _ _) = case v of {}
hsExprType (NegApp _ _ se) = syntaxExprType se
hsExprType (HsPar _ e) = lhsExprType e
hsExprType (SectionL v _ _) = case v of {}
hsExprType (SectionR v _ _) = case v of {}
hsExprType (ExplicitTuple _ args box) = mkTupleTy box $ map hsTupArgType args
hsExprType (ExplicitSum alt_tys _ _ _) = mkSumTy alt_tys
hsExprType (HsCase _ _ (MG { mg_ext = match_group })) = mg_res_ty match_group
hsExprType (HsIf _ _ t _) = lhsExprType t
hsExprType (HsMultiIf ty _) = ty
hsExprType (HsLet _ _ body) = lhsExprType body
hsExprType (HsDo ty _ _) = ty
hsExprType (ExplicitList ty _) = mkListTy ty
hsExprType (RecordCon con_expr _ _) = hsExprType con_expr
hsExprType e@(RecordUpd (RecordUpdTc { rupd_cons = cons, rupd_out_tys = out_tys }) _ _) =
  case cons of
    con_like:_ -> conLikeResTy con_like out_tys
    []         -> pprPanic "hsExprType: RecordUpdTc with empty rupd_cons"
                           (ppr e)
hsExprType (HsGetField { gf_ext = v }) = case v of {}
hsExprType (HsProjection { proj_ext = v }) = case v of {}
hsExprType (ExprWithTySig _ e _) = lhsExprType e
hsExprType (ArithSeq _ mb_overloaded_op asi) = case mb_overloaded_op of
  Just op -> piResultTy (syntaxExprType op) asi_ty
  Nothing -> asi_ty
  where
    asi_ty = arithSeqInfoType asi
hsExprType e@(HsBracket{}) = pprPanic "hsExprType: Unexpected HsBracket"
                                      (ppr e)
hsExprType e@(HsRnBracketOut{}) = pprPanic "hsExprType: Unexpected HsRnBracketOut"
                                           (ppr e)
-- TODO RGS: How should this work? There's a brackTy function in GHC.Tc.Gen.Splice
-- that *almost* does what we want, but it's monadic...
hsExprType (HsTcBracketOut _a _wrap _bracket _pending) = undefined
hsExprType e@(HsSpliceE{}) = pprPanic "hsExprType: Unexpected HsSpliceE"
                                      (ppr e)
hsExprType (HsProc _ _ lcmd_top) = lhsCmdTopType lcmd_top
hsExprType (HsStatic _ e) = lhsExprType e
hsExprType (HsTick _ _ e) = lhsExprType e
hsExprType (HsBinTick _ _ _ e) = lhsExprType e
hsExprType (HsPragE _ _ e) = lhsExprType e
hsExprType (XExpr (WrapExpr (HsWrap wrap e))) = hsWrapperType wrap $ hsExprType e
hsExprType (XExpr (ExpansionExpr (HsExpanded _ tc_e))) = hsExprType tc_e
hsExprType (XExpr (ConLikeTc con _ _)) = conLikeType con

arithSeqInfoType :: ArithSeqInfo GhcTc -> Type
arithSeqInfoType asi = mkListTy $ case asi of
  From x           -> lhsExprType x
  FromThen x _     -> lhsExprType x
  FromTo x _       -> lhsExprType x
  FromThenTo x _ _ -> lhsExprType x

conLikeType :: ConLike -> Type
conLikeType (RealDataCon con)  = dataConNonlinearType con
conLikeType (PatSynCon patsyn) = case patSynBuilder patsyn of
    Just (_, ty, _) -> ty
    Nothing         -> pprPanic "conLikeType: Unidirectional pattern synonym in expression position"
                                (ppr patsyn)

hsTupArgType :: HsTupArg GhcTc -> Type
hsTupArgType (Present _ e)           = lhsExprType e
hsTupArgType (Missing (Scaled _ ty)) = ty

hsWrapperType :: HsWrapper -> Type -> Type
hsWrapperType WpHole              = id
hsWrapperType (w1 `WpCompose` w2) = hsWrapperType w1 . hsWrapperType w2
hsWrapperType (WpFun _ w2 (Scaled m exp_arg) _) = \t ->
  let act_res = funResultTy t
      exp_res = hsWrapperType w2 act_res
  in mkFunctionType m exp_arg exp_res
hsWrapperType (WpCast co)         = \_ -> coercionRKind co
hsWrapperType (WpEvLam v)         = mkInvisFunTyMany (idType v)
hsWrapperType (WpEvApp _)         = funResultTy
hsWrapperType (WpTyLam tv)        = mkForAllTy tv Inferred
hsWrapperType (WpTyApp t)         = (`piResultTy` t)
hsWrapperType (WpLet _)           = id
hsWrapperType (WpMultCoercion _)  = id

lhsCmdTopType :: LHsCmdTop GhcTc -> Type
lhsCmdTopType (L _ (HsCmdTop (CmdTopTc _ ret_ty _) _)) = ret_ty

matchGroupTcType :: MatchGroupTc -> Type
matchGroupTcType (MatchGroupTc args res) = mkVisFunTys args res

syntaxExprType :: SyntaxExpr GhcTc -> Type
syntaxExprType (SyntaxExprTc e _ _) = hsExprType e
syntaxExprType NoSyntaxExprTc       = panic "syntaxExprType: Unexpected NoSyntaxExprTc"
