-- | Compute the 'Type' of an @'HsExpr' 'GhcTc'@ in a pure fashion.
--
-- Note that this does /not/ currently support the use case of annotating
-- every subexpression in an 'HsExpr' with its 'Type'. For more information on
-- this task, see #12706, #15320, #16804, and #17331.
module GHC.Hs.Syn.Type (
    -- * Extracting types from HsExpr
    lhsExprType, hsExprType, hsWrapperType,
    -- * Extracting types from HsSyn
    hsLitType, hsPatType, hsLPatType

  ) where

import GHC.Prelude

import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Core.Coercion
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.PatSyn
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Hs
import GHC.Tc.Types.Evidence
import GHC.Types.Id
import GHC.Types.Var( VarBndr(..) )
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic

{-
************************************************************************
*                                                                      *
       Extracting the type from HsSyn
*                                                                      *
************************************************************************

-}

hsLPatType :: LPat GhcTc -> Type
hsLPatType (L _ p) = hsPatType p

hsPatType :: Pat GhcTc -> Type
hsPatType (ParPat _ pat)                = hsLPatType pat
hsPatType (WildPat ty)                  = ty
hsPatType (VarPat _ lvar)               = idType (unLoc lvar)
hsPatType (BangPat _ pat)               = hsLPatType pat
hsPatType (LazyPat _ pat)               = hsLPatType pat
hsPatType (LitPat _ lit)                = hsLitType lit
hsPatType (AsPat _ var _)               = idType (unLoc var)
hsPatType (ViewPat ty _ _)              = ty
hsPatType (ListPat ty _)                = mkListTy ty
hsPatType (OrPat ty _)                  = ty
hsPatType (TuplePat tys _ bx)           = mkTupleTy1 bx tys
                  -- See Note [Don't flatten tuples from HsSyn] in GHC.Core.Make
hsPatType (SumPat tys _ _ _ )           = mkSumTy tys
hsPatType (ConPat { pat_con = lcon
                  , pat_con_ext = ConPatTc
                    { cpt_arg_tys = tys
                    }
                  })
                                        = conLikeResTy (unLoc lcon) tys
hsPatType (SigPat ty _ _)               = ty
hsPatType (NPat ty _ _ _)               = ty
hsPatType (NPlusKPat ty _ _ _ _ _)      = ty
hsPatType (EmbTyPat ty _)               = typeKind ty
hsPatType (InvisPat ty _)               = typeKind ty
hsPatType (XPat ext) =
  case ext of
    CoPat _ _ ty       -> ty
    ExpansionPat _ pat -> hsPatType pat
hsPatType (SplicePat v _)               = dataConCantHappen v

hsLitType :: HsLit (GhcPass p) -> Type
hsLitType (HsChar _ _)       = charTy
hsLitType (HsCharPrim _ _)   = charPrimTy
hsLitType (HsString _ _)     = stringTy
hsLitType (HsStringPrim _ _) = addrPrimTy
hsLitType (HsInt _ _)        = intTy
hsLitType (HsIntPrim _ _)    = intPrimTy
hsLitType (HsWordPrim _ _)   = wordPrimTy
hsLitType (HsInt8Prim _ _)   = int8PrimTy
hsLitType (HsInt16Prim _ _)  = int16PrimTy
hsLitType (HsInt32Prim _ _)  = int32PrimTy
hsLitType (HsInt64Prim _ _)  = int64PrimTy
hsLitType (HsWord8Prim _ _)  = word8PrimTy
hsLitType (HsWord16Prim _ _) = word16PrimTy
hsLitType (HsWord32Prim _ _) = word32PrimTy
hsLitType (HsWord64Prim _ _) = word64PrimTy
hsLitType (HsInteger _ _ ty) = ty
hsLitType (HsRat _ _ ty)     = ty
hsLitType (HsFloatPrim _ _)  = floatPrimTy
hsLitType (HsDoublePrim _ _) = doublePrimTy


-- | Compute the 'Type' of an @'LHsExpr' 'GhcTc'@ in a pure fashion.
lhsExprType :: LHsExpr GhcTc -> Type
lhsExprType (L _ e) = hsExprType e

-- | Compute the 'Type' of an @'HsExpr' 'GhcTc'@ in a pure fashion.
hsExprType :: HsExpr GhcTc -> Type
hsExprType (HsVar _ (L _ id)) = idType id
hsExprType (HsRecSel _ (FieldOcc id _)) = idType id
hsExprType (HsOverLabel v _ _) = dataConCantHappen v
hsExprType (HsIPVar v _) = dataConCantHappen v
hsExprType (HsOverLit _ lit) = overLitType lit
hsExprType (HsLit _ lit) = hsLitType lit
hsExprType (HsLam _ _ (MG { mg_ext = match_group })) = matchGroupTcType match_group
hsExprType (HsApp _ f _) = funResultTy $ lhsExprType f
hsExprType (HsAppType x f _) = piResultTy (lhsExprType f) x
hsExprType (OpApp v _ _ _) = dataConCantHappen v
hsExprType (NegApp _ _ se) = syntaxExprType se
hsExprType (HsPar _ e) = lhsExprType e
hsExprType (SectionL v _ _) = dataConCantHappen v
hsExprType (SectionR v _ _) = dataConCantHappen v
hsExprType (ExplicitTuple _ args box) = mkTupleTy box $ map hsTupArgType args
hsExprType (ExplicitSum alt_tys _ _ _) = mkSumTy alt_tys
hsExprType (HsCase _ _ (MG { mg_ext = match_group })) = mg_res_ty match_group
hsExprType (HsIf _ _ t _) = lhsExprType t
hsExprType (HsMultiIf ty _) = ty
hsExprType (HsLet _ _ body) = lhsExprType body
hsExprType (HsDo ty _ _) = ty
hsExprType (ExplicitList ty _) = mkListTy ty
hsExprType (RecordCon con_expr _ _) = hsExprType con_expr
hsExprType (RecordUpd v _ _) = dataConCantHappen v
hsExprType (HsGetField { gf_ext = v }) = dataConCantHappen v
hsExprType (HsProjection { proj_ext = v }) = dataConCantHappen v
hsExprType (ExprWithTySig _ e _) = lhsExprType e
hsExprType (ArithSeq _ mb_overloaded_op asi) = case mb_overloaded_op of
  Just op -> piResultTy (syntaxExprType op) asi_ty
  Nothing -> asi_ty
  where
    asi_ty = arithSeqInfoType asi
hsExprType (HsTypedBracket   (HsBracketTc { hsb_ty = ty }) _) = ty
hsExprType (HsUntypedBracket (HsBracketTc { hsb_ty = ty }) _) = ty
hsExprType e@(HsTypedSplice{}) = pprPanic "hsExprType: Unexpected HsTypedSplice"
                                          (ppr e)
                                      -- Typed splices should have been eliminated during zonking, but we
                                      -- can't use `dataConCantHappen` since they are still present before
                                      -- than in the typechecked AST.
hsExprType (HsUntypedSplice ext _) = dataConCantHappen ext
hsExprType (HsProc _ _ lcmd_top) = lhsCmdTopType lcmd_top
hsExprType (HsStatic (_, ty) _s) = ty
hsExprType (HsPragE _ _ e) = lhsExprType e
hsExprType (HsEmbTy x _) = dataConCantHappen x
hsExprType (XExpr (WrapExpr (HsWrap wrap e))) = hsWrapperType wrap $ hsExprType e
hsExprType (XExpr (ExpandedThingTc _ e))  = hsExprType e
hsExprType (XExpr (ConLikeTc con _ _)) = conLikeType con
hsExprType (XExpr (HsTick _ e)) = lhsExprType e
hsExprType (XExpr (HsBinTick _ _ e)) = lhsExprType e
hsExprType (XExpr (HsUnboundVarTc (HER _ ty _) _)) = ty

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


-- | The PRType (ty, tas) is short for (piResultTys ty (reverse tas))
type PRType = (Type, [Type])

prTypeType :: PRType -> Type
prTypeType (ty, tys)
  | null tys  = ty
  | otherwise = piResultTys ty (reverse tys)

liftPRType :: (Type -> Type) -> PRType -> PRType
liftPRType f pty = (f (prTypeType pty), [])

hsWrapperType :: HsWrapper -> Type -> Type
hsWrapperType wrap ty = prTypeType $ go wrap (ty,[])
  where
    go WpHole              = id
    go (w1 `WpCompose` w2) = go w1 . go w2
    go (WpFun _ w2 (Scaled m exp_arg)) = liftPRType $ \t ->
      let act_res = funResultTy t
          exp_res = hsWrapperType w2 act_res
      in mkFunctionType m exp_arg exp_res
    go (WpCast co)        = liftPRType $ \_ -> coercionRKind co
    go (WpEvLam v)        = liftPRType $ mkInvisFunTy (idType v)
    go (WpEvApp _)        = liftPRType $ funResultTy
    go (WpTyLam tv)       = liftPRType $ mkForAllTy (Bndr tv Inferred)
    go (WpTyApp ta)       = \(ty,tas) -> (ty, ta:tas)
    go (WpLet _)          = id
    go (WpMultCoercion _) = id

lhsCmdTopType :: LHsCmdTop GhcTc -> Type
lhsCmdTopType (L _ (HsCmdTop (CmdTopTc _ ret_ty _) _)) = ret_ty

matchGroupTcType :: MatchGroupTc -> Type
matchGroupTcType (MatchGroupTc args res _) = mkScaledFunTys args res

syntaxExprType :: SyntaxExpr GhcTc -> Type
syntaxExprType (SyntaxExprTc e _ _) = hsExprType e
syntaxExprType NoSyntaxExprTc       = panic "syntaxExprType: Unexpected NoSyntaxExprTc"
