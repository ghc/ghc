%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcPatSyn]{Typechecking pattern synonym declarations}

\begin{code}
module TcPatSyn (tcPatSynDecl) where

import HsSyn
import TcPat
import TcRnMonad
import TcEnv
import TcMType
import TysPrim
import Name
import SrcLoc
import PatSyn
import Maybes
import NameSet
import Panic
import Outputable
import FastString
import Var
import Id
import TcBinds
import BasicTypes
import TcSimplify
import TcType
import VarSet
import Data.Monoid
import Bag
import TcEvidence
import BuildTyCl

#include "HsVersions.h"
\end{code}

\begin{code}
tcPatSynDecl :: Located Name
             -> HsPatSynDetails (Located Name)
             -> LPat Name
             -> HsPatSynDir Name
             -> TcM (PatSyn, LHsBinds Id)
tcPatSynDecl lname@(L _ name) details lpat dir
  = do { traceTc "tcPatSynDecl {" $ ppr name $$ ppr lpat
       ; pat_ty <- newFlexiTyVarTy openTypeKind

       ; let (arg_names, is_infix) = case details of
                 PrefixPatSyn names -> (map unLoc names, False)
                 InfixPatSyn name1 name2 -> (map unLoc [name1, name2], True)
       ; ((lpat', args), wanted) <- captureConstraints $
                                      tcPat PatSyn lpat pat_ty $ mapM tcLookupId arg_names
       ; let named_taus = (name, pat_ty):map (\arg -> (getName arg, varType arg)) args

       ; traceTc "tcPatSynDecl::wanted" (ppr named_taus $$ ppr wanted)
       ; (qtvs, given_dicts, _mr_bites, ev_binds) <- simplifyInfer True False named_taus wanted
       ; let req_dicts = given_dicts

       ; (ex_vars, prov_dicts) <- tcCollectEx lpat'
       ; let univ_tvs = filter (not . (`elemVarSet` ex_vars)) qtvs
             ex_tvs = varSetElems ex_vars

       ; pat_ty <- zonkTcType pat_ty
       ; args <- mapM zonkId args

       ; univ_tvs <- mapM zonkQuantifiedTyVar univ_tvs
       ; let prov_theta = map evVarPred prov_dicts
             req_theta = map evVarPred req_dicts
       ; prov_theta <- zonkTcThetaType prov_theta
       ; req_theta <- zonkTcThetaType req_theta

       ; traceTc "tcPatSynDecl: ex" (ppr ex_tvs $$
                                     ppr prov_theta $$
                                     ppr prov_dicts)
       ; traceTc "tcPatSynDecl: univ" (ppr univ_tvs $$
                                       ppr req_theta $$
                                       ppr req_dicts $$
                                       ppr ev_binds)

       ; let theta = prov_theta ++ req_theta

       ; traceTc "tcPatSynDecl: type" (ppr name $$
                                       ppr univ_tvs $$
                                       ppr (map varType args) $$
                                       ppr pat_ty)

       ; (matcher_id, matcher_bind) <- tcPatSynMatcher lname lpat' args
                                         univ_tvs ex_tvs
                                         ev_binds
                                         prov_dicts req_dicts
                                         prov_theta req_theta
                                         pat_ty
       ; m_wrapper <- tcPatSynWrapper lname lpat dir args
                        univ_tvs ex_tvs theta pat_ty
       ; let binds = matcher_bind `unionBags` maybe emptyBag snd m_wrapper

       ; traceTc "tcPatSynDecl }" $ ppr name
       ; let patSyn = mkPatSyn name is_infix
                        args
                        univ_tvs ex_tvs
                        prov_theta req_theta
                        pat_ty
                        matcher_id (fmap fst m_wrapper)
       ; return (patSyn, binds) }

tcPatSynMatcher :: Located Name
                -> LPat Id
                -> [Var]
                -> [TcTyVar] -> [TcTyVar]
                -> TcEvBinds
                -> [EvVar] -> [EvVar]
                -> ThetaType -> ThetaType
                -> TcType
                -> TcM (Id, LHsBinds Id)
tcPatSynMatcher (L loc name) lpat args univ_tvs ex_tvs ev_binds prov_dicts req_dicts prov_theta req_theta pat_ty
  = do { res_tv <- zonkQuantifiedTyVar =<< newFlexiTyVar liftedTypeKind
       ; (matcher_id, res_ty, cont_ty) <- mkPatSynMatcherId name args
                                            univ_tvs ex_tvs
                                            prov_theta req_theta
                                            pat_ty res_tv
       ; traceTc "tcPatSynMatcher" (ppr name $$ ppr (idType matcher_id))
       ; let matcher_lid = L loc matcher_id

       ; scrutinee <- mkId "scrut" pat_ty
       ; cont <- mkId "cont" cont_ty
       ; let cont' = nlHsApps cont $ map nlHsVar (ex_tvs ++ prov_dicts ++ args)
       ; fail <- mkId "fail" res_ty
       ; let fail' = nlHsVar fail


       ; let args = map nlVarPat [scrutinee, cont, fail]
             lwpat = noLoc $ WildPat pat_ty
             cases = if isIrrefutableHsPat lpat
                     then [mkSimpleHsAlt lpat  cont']
                     else [mkSimpleHsAlt lpat  cont',
                           mkSimpleHsAlt lwpat fail']
             body = mkLHsWrap (mkWpLet ev_binds) $
                    L (getLoc lpat) $
                    HsCase (nlHsVar scrutinee) $
                    MG{ mg_alts = cases
                      , mg_arg_tys = [pat_ty]
                      , mg_res_ty = res_ty
                      }
             body' = noLoc $
                     HsLam $
                     MG{ mg_alts = [mkSimpleMatch args body]
                       , mg_arg_tys = [pat_ty, cont_ty, res_ty]
                       , mg_res_ty = res_ty
                       }

             match = mkMatch [] (mkHsLams (res_tv:univ_tvs) req_dicts body') EmptyLocalBinds
             mg = MG{ mg_alts = [match]
                    , mg_arg_tys = []
                    , mg_res_ty = res_ty
                    }

       ; let bind = FunBind{ fun_id = matcher_lid
                           , fun_infix = False
                           , fun_matches = mg
                           , fun_co_fn = idHsWrapper
                           , bind_fvs = emptyNameSet
                           , fun_tick = Nothing }
             matcher_bind = unitBag (Generated, noLoc bind)

       ; traceTc "tcPatSynMatcher" (ppr matcher_bind)

       ; return (matcher_id, matcher_bind) }
  where
    mkId s ty = do
        name <- newName . mkVarOccFS . fsLit $ s
        return $ mkLocalId name ty

tcPatSynWrapper :: Located Name
                -> LPat Name
                -> HsPatSynDir Name
                -> [Var]
                -> [TyVar] -> [TyVar]
                -> ThetaType
                -> TcType
                -> TcM (Maybe (Id, LHsBinds Id))
tcPatSynWrapper lname lpat dir args univ_tvs ex_tvs theta pat_ty
  = do { let argNames = mkNameSet (map Var.varName args)
       ; m_expr <- runMaybeT $ tcPatToExpr argNames lpat
       ; case (dir, m_expr) of
           (Unidirectional, _) ->
               return Nothing
           (ImplicitBidirectional, Nothing) ->
               cannotInvertPatSynErr lpat
           (ImplicitBidirectional, Just lexpr) ->
               fmap Just $ tc_pat_syn_wrapper_from_expr lname lexpr args univ_tvs ex_tvs theta pat_ty }

tc_pat_syn_wrapper_from_expr :: Located Name
                             -> LHsExpr Name
                             -> [Var]
                             -> [TyVar] -> [TyVar]
                             -> ThetaType
                             -> Type
                             -> TcM (Id, LHsBinds Id)
tc_pat_syn_wrapper_from_expr (L loc name) lexpr args univ_tvs ex_tvs theta pat_ty
  = do { let qtvs = univ_tvs ++ ex_tvs
       ; (subst, qtvs') <- tcInstSigTyVars qtvs
       ; let theta' = substTheta subst theta
             pat_ty' = substTy subst pat_ty
             args' = map (\arg -> setVarType arg $ substTy subst (varType arg)) args

       ; wrapper_id <- mkPatSynWrapperId name args qtvs theta pat_ty
       ; let wrapper_name = getName wrapper_id
             wrapper_lname = L loc wrapper_name
             -- (wrapper_tvs, wrapper_theta, wrapper_tau) = tcSplitSigmaTy (idType wrapper_id)
             wrapper_tvs = qtvs'
             wrapper_theta = theta'
             wrapper_tau = mkFunTys (map varType args') pat_ty'

       ; let wrapper_args = map (noLoc . VarPat . Var.varName) args'
             wrapper_match = mkMatch wrapper_args lexpr EmptyLocalBinds
             bind = mkTopFunBind wrapper_lname [wrapper_match]
             lbind = noLoc bind
       ; let sig = TcSigInfo{ sig_id = wrapper_id
                            , sig_tvs = map (\tv -> (Nothing, tv)) wrapper_tvs
                            , sig_theta = wrapper_theta
                            , sig_tau = wrapper_tau
                            , sig_loc = loc
                            }
       ; (wrapper_binds, _, _) <- tcPolyCheck NonRecursive (const []) sig (Generated, lbind)
       ; traceTc "tcPatSynDecl wrapper" $ ppr wrapper_binds
       ; traceTc "tcPatSynDecl wrapper type" $ ppr (varType wrapper_id)
       ; return (wrapper_id, wrapper_binds) }

tcNothing :: MaybeT TcM a
tcNothing = MaybeT (return Nothing)

withLoc :: (a -> MaybeT TcM b) -> Located a -> MaybeT TcM (Located b)
withLoc fn (L loc x) = MaybeT $ setSrcSpan loc $
    do { y <- runMaybeT $ fn x
       ; return (fmap (L loc) y) }

tcPatToExpr :: NameSet -> LPat Name -> MaybeT TcM (LHsExpr Name)
tcPatToExpr lhsVars = go
  where
    go :: LPat Name -> MaybeT TcM (LHsExpr Name)
    go (L loc (ConPatIn conName info))
      = MaybeT . setSrcSpan loc . runMaybeT $ do
          { let con = L loc (HsVar (unLoc conName))
          ; exprs <- mapM go (hsConPatArgs info)
          ; return $ foldl (\x y -> L loc (HsApp x y)) con exprs }
    go p = withLoc go1 p

    go1 :: Pat Name -> MaybeT TcM (HsExpr Name)
    go1   (VarPat var)
      | var `elemNameSet` lhsVars  = return (HsVar var)
      | otherwise                  = tcNothing
    go1 p@(AsPat _ _)              = asPatInPatSynErr p
    go1   (LazyPat pat)            = fmap HsPar (go pat)
    go1   (ParPat pat)             = fmap HsPar (go pat)
    go1   (BangPat pat)            = fmap HsPar (go pat)
    go1   (PArrPat pats ptt)
      = do { exprs <- mapM go pats
           ; return (ExplicitPArr ptt exprs) }
    go1   (ListPat pats ptt reb)
      = do { exprs <- mapM go pats
           ; return (ExplicitList ptt (fmap snd reb) exprs) }
    go1   (TuplePat pats box _)
      = do { exprs <- mapM go pats
           ; return (ExplicitTuple (map Present exprs) box)
           }
    go1   (LitPat lit)  = return (HsLit lit)
    go1   (NPat n Nothing _)       = return (HsOverLit n)
    go1   (NPat n (Just neg) _)    = return (noLoc neg `HsApp` noLoc (HsOverLit n))
    go1   (SigPatIn pat (HsWB ty _ _))
      = do { expr <- go pat
           ; return (ExprWithTySig expr ty) }
    go1   (ConPatOut{})            = panic "ConPatOut in output of renamer"
    go1   (SigPatOut{})            = panic "SigPatOut in output of renamer"
    go1   (CoPat{})                = panic "CoPat in output of renamer"
    go1   _                        = tcNothing

asPatInPatSynErr :: OutputableBndr name => Pat name -> MaybeT TcM a
asPatInPatSynErr pat
  = MaybeT . failWithTc $
    hang (ptext (sLit "Pattern synonym definition cannot contain as-patterns (@):"))
       2 (ppr pat)

cannotInvertPatSynErr :: OutputableBndr name => LPat name -> TcM a
cannotInvertPatSynErr (L loc pat)
  = setSrcSpan loc $ failWithTc $
    hang (ptext (sLit "Right-hand side of bidirectional pattern synonym cannot be used as an expression"))
       2 (ppr pat)

tcCollectEx :: LPat Id -> TcM (TyVarSet, [EvVar])
tcCollectEx = return . go
  where
    go :: LPat Id -> (TyVarSet, [EvVar])
    go = go1 . unLoc

    go1 :: Pat Id -> (TyVarSet, [EvVar])
    go1 (LazyPat p)         = go p
    go1 (AsPat _ p)         = go p
    go1 (ParPat p)          = go p
    go1 (BangPat p)         = go p
    go1 (ListPat ps _ _)    = mconcat . map go $ ps
    go1 (TuplePat ps _ _)   = mconcat . map go $ ps
    go1 (PArrPat ps _)      = mconcat . map go $ ps
    go1 (ViewPat _ p _)     = go p
    go1 (QuasiQuotePat qq)  = pprPanic "TODO: tcInstPatSyn QuasiQuotePat" $ ppr qq
    go1 con@ConPatOut{}     = mappend (mkVarSet (pat_tvs con), pat_dicts con) $
                                 goConDetails $ pat_args con
    go1 (SigPatOut p _)     = go p
    go1 (CoPat _ p _)       = go1 p
    go1 (NPlusKPat n k geq subtract)
      = pprPanic "TODO: NPlusKPat" $ ppr n $$ ppr k $$ ppr geq $$ ppr subtract
    go1 _                   = mempty

    goConDetails :: HsConPatDetails Id -> (TyVarSet, [EvVar])
    goConDetails (PrefixCon ps) = mconcat . map go $ ps
    goConDetails (InfixCon p1 p2) = go p1 `mappend` go p2
    goConDetails (RecCon HsRecFields{ rec_flds = flds })
      = mconcat . map goRecFd $ flds

    goRecFd :: HsRecField Id (LPat Id) -> (TyVarSet, [EvVar])
    goRecFd HsRecField{ hsRecFieldArg = p } = go p

\end{code}
