{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcPatSyn]{Typechecking pattern synonym declarations}
-}

{-# LANGUAGE CPP #-}

module TcPatSyn ( tcInferPatSynDecl, tcCheckPatSynDecl
                , tcPatSynBuilderBind, tcPatSynBuilderOcc
  ) where

import HsSyn
import TcPat
import TcRnMonad
import TcEnv
import TcMType
import TysPrim
import Name
import SrcLoc
import PatSyn
import NameSet
import Panic
import Outputable
import FastString
import Var
import Id
import IdInfo( IdDetails(..) )
import TcBinds
import BasicTypes
import TcSimplify
import TcUnify
import TcType
import TcEvidence
import BuildTyCl
import VarSet
import MkId
import VarEnv
import Inst
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid
#endif
import Bag
import Util
import Data.Maybe
import Control.Monad (forM)

#include "HsVersions.h"

{-
************************************************************************
*                                                                      *
                    Type checking a pattern synonym
*                                                                      *
************************************************************************
-}

tcInferPatSynDecl :: PatSynBind Name Name
                  -> TcM (PatSyn, LHsBinds Id)
tcInferPatSynDecl PSB{ psb_id = lname@(L loc name), psb_args = details,
                       psb_def = lpat, psb_dir = dir }
  = setSrcSpan loc $
    do { traceTc "tcInferPatSynDecl {" $ ppr name
       ; tcCheckPatSynPat lpat

       ; let (arg_names, is_infix) = case details of
                 PrefixPatSyn names      -> (map unLoc names, False)
                 InfixPatSyn name1 name2 -> (map unLoc [name1, name2], True)
       ; (((lpat', (args, pat_ty)), tclvl), wanted)
            <- captureConstraints  $
               captureTcLevel      $
               do { pat_ty <- newFlexiTyVarTy openTypeKind
                  ; tcPat PatSyn lpat pat_ty $
               do { args <- mapM tcLookupId arg_names
                  ; return (args, pat_ty) } }

       ; let named_taus = (name, pat_ty) : map (\arg -> (getName arg, varType arg)) args

       ; (qtvs, req_dicts, _mr_bites, ev_binds) <- simplifyInfer tclvl False named_taus wanted

       ; (ex_vars, prov_dicts) <- tcCollectEx lpat'
       ; let univ_tvs   = filter (not . (`elemVarSet` ex_vars)) qtvs
             ex_tvs     = varSetElems ex_vars
             prov_theta = map evVarPred prov_dicts
             req_theta  = map evVarPred req_dicts

       ; traceTc "tcInferPatSynDecl }" $ ppr name
       ; tc_patsyn_finish lname dir is_infix lpat'
                          (univ_tvs, req_theta, ev_binds, req_dicts)
                          (ex_tvs, map mkTyVarTy ex_tvs, prov_theta, emptyTcEvBinds, prov_dicts)
                          (zip args $ repeat idHsWrapper)
                          pat_ty }

tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (PatSyn, LHsBinds Id)
tcCheckPatSynDecl PSB{ psb_id = lname@(L loc name), psb_args = details,
                       psb_def = lpat, psb_dir = dir }
                  TPSI{ patsig_tau = tau,
                        patsig_ex = ex_tvs, patsig_univ = univ_tvs,
                        patsig_prov = prov_theta, patsig_req = req_theta }
  = setSrcSpan loc $
    do { traceTc "tcCheckPatSynDecl" $
         ppr (ex_tvs, prov_theta) $$
         ppr (univ_tvs, req_theta) $$
         ppr arg_tys $$
         ppr tau
       ; tcCheckPatSynPat lpat

       ; req_dicts <- newEvVars req_theta

       -- TODO: find a better SkolInfo
       ; let skol_info = SigSkol (FunSigCtxt name) (mkFunTys arg_tys pat_ty)

       ; let (arg_names, is_infix) = case details of
                 PrefixPatSyn names      -> (map unLoc names, False)
                 InfixPatSyn name1 name2 -> (map unLoc [name1, name2], True)

       ; let ty_arity = length arg_tys
       ; checkTc (length arg_names == ty_arity)
                 (wrongNumberOfParmsErr ty_arity)

         -- Typecheck the pattern against pat_ty, then unify the type of args
         -- against arg_tys, with ex_tvs changed to SigTyVars.
         -- We get out of this:
         --  * The evidence bindings for the requested theta: req_ev_binds
         --  * The typechecked pattern: lpat'
         --  * The arguments, type-coerced to the SigTyVars: wrapped_args
         --  * The instantiation of ex_tvs to pass to the success continuation: ex_tys
         --  * The provided theta substituted with the SigTyVars: prov_theta'
       ; (implic1, req_ev_binds, (lpat', (ex_tys, prov_theta', wrapped_args))) <-
           buildImplication skol_info univ_tvs req_dicts $
           tcPat PatSyn lpat pat_ty $ do
           { ex_sigtvs <- mapM (\tv -> newSigTyVar (getName tv) (tyVarKind tv)) ex_tvs
           ; let subst = mkTvSubst (mkInScopeSet (zipVarEnv ex_sigtvs ex_sigtvs)) $
                         zipTyEnv ex_tvs (map mkTyVarTy ex_sigtvs)
           ; let ex_tys = substTys subst $ map mkTyVarTy ex_tvs
                 prov_theta' = substTheta subst prov_theta
           ; wrapped_args <- forM (zipEqual "tcCheckPatSynDecl" arg_names arg_tys) $ \(arg_name, arg_ty) -> do
               { arg <- tcLookupId arg_name
               ; let arg_ty' = substTy subst arg_ty
               ; coi <- unifyType (varType arg) arg_ty'
               ; return (setVarType arg arg_ty, coToHsWrapper coi) }
           ; return (ex_tys, prov_theta', wrapped_args) }

       ; (ex_vars_rhs, prov_dicts_rhs) <- tcCollectEx lpat'
       ; let ex_tvs_rhs  = varSetElems ex_vars_rhs

         -- Check that prov_theta' can be satisfied with the dicts from the pattern
       ; (implic2, prov_ev_binds, prov_dicts) <-
           buildImplication skol_info ex_tvs_rhs prov_dicts_rhs $ do
           { let origin = PatOrigin -- TODO
           ; emitWanteds origin prov_theta' }

       -- Solve the constraints now, because we are about to make a PatSyn,
       -- which should not contain unification variables and the like (Trac #10997)
       -- Since all the inputs are implications the returned bindings will be empty
       ; _ <- simplifyTop (emptyWC `addImplics` (implic1 `unionBags` implic2))

       ; traceTc "tcCheckPatSynDecl }" $ ppr name
       ; tc_patsyn_finish lname dir is_infix lpat'
                          (univ_tvs, req_theta, req_ev_binds, req_dicts)
                          (ex_tvs, ex_tys, prov_theta, prov_ev_binds, prov_dicts)
                          wrapped_args
                          pat_ty }
  where
    (arg_tys, pat_ty) = tcSplitFunTys tau

wrongNumberOfParmsErr :: Arity -> SDoc
wrongNumberOfParmsErr ty_arity
  = ptext (sLit "Number of pattern synonym arguments doesn't match type; expected")
    <+> ppr ty_arity

-------------------------
-- Shared by both tcInferPatSyn and tcCheckPatSyn
tc_patsyn_finish :: Located Name
                 -> HsPatSynDir Name
                 -> Bool
                 -> LPat Id
                 -> ([TcTyVar], [PredType], TcEvBinds, [EvVar])
                 -> ([TcTyVar], [TcType], [PredType], TcEvBinds, [EvVar])
                 -> [(Var, HsWrapper)]
                 -> TcType
                 -> TcM (PatSyn, LHsBinds Id)
tc_patsyn_finish lname dir is_infix lpat'
                 (univ_tvs, req_theta, req_ev_binds, req_dicts)
                 (ex_tvs, subst, prov_theta, prov_ev_binds, prov_dicts)
                 wrapped_args
                 pat_ty
  = do { -- Zonk everything.  We are about to build a final PatSyn
         -- so there had better be no unification variables in there
         univ_tvs     <- mapM zonkQuantifiedTyVar univ_tvs
       ; ex_tvs       <- mapM zonkQuantifiedTyVar ex_tvs
       ; prov_theta   <- zonkTcThetaType prov_theta
       ; req_theta    <- zonkTcThetaType req_theta
       ; pat_ty       <- zonkTcType pat_ty
       ; wrapped_args <- mapM zonk_wrapped_arg wrapped_args
       ; let qtvs    = univ_tvs ++ ex_tvs
             theta   = prov_theta ++ req_theta
             arg_tys = map (varType . fst) wrapped_args

       ; traceTc "tc_patsyn_finish {" $
           ppr (unLoc lname) $$ ppr (unLoc lpat') $$
           ppr (univ_tvs, req_theta, req_ev_binds, req_dicts) $$
           ppr (ex_tvs, subst, prov_theta, prov_ev_binds, prov_dicts) $$
           ppr wrapped_args $$
           ppr pat_ty

       -- Make the 'matcher'
       ; (matcher_id, matcher_bind) <- tcPatSynMatcher lname lpat'
                                         (univ_tvs, req_theta, req_ev_binds, req_dicts)
                                         (ex_tvs, subst, prov_theta, prov_ev_binds, prov_dicts)
                                         wrapped_args  -- Not necessarily zonked
                                         pat_ty

       -- Make the 'builder'
       ; builder_id <- mkPatSynBuilderId dir lname qtvs theta arg_tys pat_ty

       -- Make the PatSyn itself
       ; let patSyn = mkPatSyn (unLoc lname) is_infix
                        (univ_tvs, req_theta)
                        (ex_tvs, prov_theta)
                        arg_tys
                        pat_ty
                        matcher_id builder_id

       ; return (patSyn, matcher_bind) }
  where
    zonk_wrapped_arg :: (Var, HsWrapper) -> TcM (Var, HsWrapper)
    -- The HsWrapper will get zonked later, as part of the LHsBinds
    zonk_wrapped_arg (arg_id, wrap) = do { arg_id <- zonkId arg_id
                                         ; return (arg_id, wrap) }

{-
************************************************************************
*                                                                      *
         Constructing the "matcher" Id and its binding
*                                                                      *
************************************************************************
-}

tcPatSynMatcher :: Located Name
                -> LPat Id
                -> ([TcTyVar], ThetaType, TcEvBinds, [EvVar])
                -> ([TcTyVar], [TcType], ThetaType, TcEvBinds, [EvVar])
                -> [(Var, HsWrapper)]
                -> TcType
                -> TcM ((Id, Bool), LHsBinds Id)
-- See Note [Matchers and builders for pattern synonyms] in PatSyn
tcPatSynMatcher (L loc name) lpat
                (univ_tvs, req_theta, req_ev_binds, req_dicts)
                (ex_tvs, ex_tys, prov_theta, prov_ev_binds, prov_dicts)
                wrapped_args pat_ty
  = do { uniq <- newUnique
       ; let tv_name = mkInternalName uniq (mkTyVarOcc "r") loc
             res_tv  = mkTcTyVar tv_name openTypeKind (SkolemTv False)
             is_unlifted = null wrapped_args && null prov_dicts
             res_ty = mkTyVarTy res_tv
             (cont_arg_tys, cont_args)
               | is_unlifted = ([voidPrimTy], [nlHsVar voidPrimId])
               | otherwise   = unzip [ (varType arg, mkLHsWrap wrap $ nlHsVar arg)
                                     | (arg, wrap) <- wrapped_args
                                     ]
             cont_ty = mkSigmaTy ex_tvs prov_theta $
                       mkFunTys cont_arg_tys res_ty

             fail_ty = mkFunTy voidPrimTy res_ty

       ; matcher_name <- newImplicitBinder name mkMatcherOcc
       ; scrutinee    <- newSysLocalId (fsLit "scrut") pat_ty
       ; cont         <- newSysLocalId (fsLit "cont")  cont_ty
       ; fail         <- newSysLocalId (fsLit "fail")  fail_ty

       ; let matcher_tau   = mkFunTys [pat_ty, cont_ty, fail_ty] res_ty
             matcher_sigma = mkSigmaTy (res_tv:univ_tvs) req_theta matcher_tau
             matcher_id    = mkExportedLocalId VanillaId matcher_name matcher_sigma
                             -- See Note [Exported LocalIds] in Id

             cont_dicts = map nlHsVar prov_dicts
             cont' = mkLHsWrap (mkWpLet prov_ev_binds) $
                     nlHsTyApps cont ex_tys (cont_dicts ++ cont_args)

             fail' = nlHsApps fail [nlHsVar voidPrimId]

             args = map nlVarPat [scrutinee, cont, fail]
             lwpat = noLoc $ WildPat pat_ty
             cases = if isIrrefutableHsPat lpat
                     then [mkSimpleHsAlt lpat  cont']
                     else [mkSimpleHsAlt lpat  cont',
                           mkSimpleHsAlt lwpat fail']
             body = mkLHsWrap (mkWpLet req_ev_binds) $
                    L (getLoc lpat) $
                    HsCase (nlHsVar scrutinee) $
                    MG{ mg_alts = cases
                      , mg_arg_tys = [pat_ty]
                      , mg_res_ty = res_ty
                      , mg_origin = Generated
                      }
             body' = noLoc $
                     HsLam $
                     MG{ mg_alts = [mkSimpleMatch args body]
                       , mg_arg_tys = [pat_ty, cont_ty, res_ty]
                       , mg_res_ty = res_ty
                       , mg_origin = Generated
                       }
             match = mkMatch [] (mkHsLams (res_tv:univ_tvs) req_dicts body') EmptyLocalBinds
             mg = MG{ mg_alts = [match]
                    , mg_arg_tys = []
                    , mg_res_ty = res_ty
                    , mg_origin = Generated
                    }

       ; let bind = FunBind{ fun_id = L loc matcher_id
                           , fun_infix = False
                           , fun_matches = mg
                           , fun_co_fn = idHsWrapper
                           , bind_fvs = emptyNameSet
                           , fun_tick = [] }
             matcher_bind = unitBag (noLoc bind)

       ; traceTc "tcPatSynMatcher" (ppr name $$ ppr (idType matcher_id))
       ; traceTc "tcPatSynMatcher" (ppr matcher_bind)

       ; return ((matcher_id, is_unlifted), matcher_bind) }


isUnidirectional :: HsPatSynDir a -> Bool
isUnidirectional Unidirectional          = True
isUnidirectional ImplicitBidirectional   = False
isUnidirectional ExplicitBidirectional{} = False

{-
************************************************************************
*                                                                      *
         Constructing the "builder" Id
*                                                                      *
************************************************************************
-}

mkPatSynBuilderId :: HsPatSynDir a -> Located Name
                  -> [TyVar] -> ThetaType -> [Type] -> Type
                  -> TcM (Maybe (Id, Bool))
mkPatSynBuilderId dir  (L _ name) qtvs theta arg_tys pat_ty
  | isUnidirectional dir
  = return Nothing
  | otherwise
  = do { builder_name <- newImplicitBinder name mkBuilderOcc
       ; let builder_sigma = mkSigmaTy qtvs theta (mkFunTys builder_arg_tys pat_ty)
             builder_id    = mkExportedLocalId VanillaId builder_name builder_sigma
                             -- See Note [Exported LocalIds] in Id
       ; return (Just (builder_id, need_dummy_arg)) }
  where
    builder_arg_tys | need_dummy_arg = [voidPrimTy]
                    | otherwise = arg_tys
    need_dummy_arg = isUnLiftedType pat_ty && null arg_tys && null theta

tcPatSynBuilderBind :: PatSynBind Name Name
                    -> TcM (LHsBinds Id)
-- See Note [Matchers and builders for pattern synonyms] in PatSyn
tcPatSynBuilderBind PSB{ psb_id = L loc name, psb_def = lpat
                       , psb_dir = dir, psb_args = details }
  | isUnidirectional dir
  = return emptyBag

  | isNothing mb_match_group       -- Can't invert the pattern
  = setSrcSpan (getLoc lpat) $ failWithTc $
    hang (ptext (sLit "Right-hand side of bidirectional pattern synonym cannot be used as an expression"))
       2 (ppr lpat)

  | otherwise
  = do { patsyn <- tcLookupPatSyn name
       ; let (worker_id, need_dummy_arg) = fromMaybe (panic "mkPatSynWrapper") $
                                           patSynBuilder patsyn

       ; let match_dummy = mkMatch [nlWildPatName] (noLoc $ HsLam mg) emptyLocalBinds
             mg' | need_dummy_arg = mkMatchGroupName Generated [match_dummy]
                 | otherwise      = mg

       ; let (worker_tvs, worker_theta, worker_tau) = tcSplitSigmaTy (idType worker_id)
             bind = FunBind { fun_id = L loc (idName worker_id)
                            , fun_infix = False
                            , fun_matches = mg'
                            , fun_co_fn = idHsWrapper
                            , bind_fvs = placeHolderNamesTc
                            , fun_tick = [] }

             sig = TcSigInfo{ sig_id = worker_id
                            , sig_tvs = map (\tv -> (Nothing, tv)) worker_tvs
                            , sig_theta = worker_theta
                            , sig_tau = worker_tau
                            , sig_loc = noSrcSpan
                            , sig_extra_cts = Nothing
                            , sig_partial = False
                            , sig_nwcs = []
                            }

       ; (worker_binds, _, _) <- tcPolyCheck NonRecursive (const []) sig (noLoc bind)
       ; traceTc "tcPatSynDecl worker" $ ppr worker_binds
       ; return worker_binds }
  where
    Just mg = mb_match_group
    mb_match_group = case dir of
                        Unidirectional           -> Nothing
                        ExplicitBidirectional mg -> Just mg
                        ImplicitBidirectional    -> fmap mk_mg (tcPatToExpr args lpat)

    mk_mg :: LHsExpr Name -> MatchGroup Name (LHsExpr Name)
    mk_mg body = mkMatchGroupName Generated [wrapper_match]
               where
                 wrapper_args  = [L loc (VarPat n) | L loc n <- args]
                 wrapper_match = mkMatch wrapper_args body EmptyLocalBinds

    args = case details of
              PrefixPatSyn args -> args
              InfixPatSyn arg1 arg2 -> [arg1, arg2]

tcPatSynBuilderOcc :: CtOrigin -> PatSyn -> TcM (HsExpr TcId, TcRhoType)
-- The result type should be fully instantiated
tcPatSynBuilderOcc orig ps
  | Just (builder_id, add_void_arg) <- builder
  = do { (wrap, rho) <- deeplyInstantiate orig (idType builder_id)
       ; let inst_fun = mkHsWrap wrap (HsVar builder_id)
       ; if add_void_arg
         then return ( HsApp (noLoc inst_fun) (nlHsVar voidPrimId)
                     , tcFunResultTy rho )
         else return ( inst_fun, rho ) }

  | otherwise  -- Unidirectional
  = failWithTc $
    ptext (sLit "non-bidirectional pattern synonym")
    <+> quotes (ppr name) <+> ptext (sLit "used in an expression")
  where
    name    = patSynName ps
    builder = patSynBuilder ps

{-
************************************************************************
*                                                                      *
         Helper functions
*                                                                      *
************************************************************************

Note [As-patterns in pattern synonym definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The rationale for rejecting as-patterns in pattern synonym definitions
is that an as-pattern would introduce nonindependent pattern synonym
arguments, e.g. given a pattern synonym like:

        pattern K x y = x@(Just y)

one could write a nonsensical function like

        f (K Nothing x) = ...

or
        g (K (Just True) False) = ...
-}

tcCheckPatSynPat :: LPat Name -> TcM ()
tcCheckPatSynPat = go
  where
    go :: LPat Name -> TcM ()
    go = addLocM go1

    go1 :: Pat Name -> TcM ()
    go1   (ConPatIn _ info)   = mapM_ go (hsConPatArgs info)
    go1   VarPat{}            = return ()
    go1   WildPat{}           = return ()
    go1 p@(AsPat _ _)         = asPatInPatSynErr p
    go1   (LazyPat pat)       = go pat
    go1   (ParPat pat)        = go pat
    go1   (BangPat pat)       = go pat
    go1   (PArrPat pats _)    = mapM_ go pats
    go1   (ListPat pats _ _)  = mapM_ go pats
    go1   (TuplePat pats _ _) = mapM_ go pats
    go1   LitPat{}            = return ()
    go1   NPat{}              = return ()
    go1   (SigPatIn pat _)    = go pat
    go1   (ViewPat _ pat _)   = go pat
    go1 p@SplicePat{}         = thInPatSynErr p
    go1 p@QuasiQuotePat{}     = thInPatSynErr p
    go1 p@NPlusKPat{}         = nPlusKPatInPatSynErr p
    go1   ConPatOut{}         = panic "ConPatOut in output of renamer"
    go1   SigPatOut{}         = panic "SigPatOut in output of renamer"
    go1   CoPat{}             = panic "CoPat in output of renamer"

asPatInPatSynErr :: OutputableBndr name => Pat name -> TcM a
asPatInPatSynErr pat
  = failWithTc $
    hang (ptext (sLit "Pattern synonym definition cannot contain as-patterns (@):"))
       2 (ppr pat)

thInPatSynErr :: OutputableBndr name => Pat name -> TcM a
thInPatSynErr pat
  = failWithTc $
    hang (ptext (sLit "Pattern synonym definition cannot contain Template Haskell:"))
       2 (ppr pat)

nPlusKPatInPatSynErr :: OutputableBndr name => Pat name -> TcM a
nPlusKPatInPatSynErr pat
  = failWithTc $
    hang (ptext (sLit "Pattern synonym definition cannot contain n+k-pattern:"))
       2 (ppr pat)

tcPatToExpr :: [Located Name] -> LPat Name -> Maybe (LHsExpr Name)
tcPatToExpr args = go
  where
    lhsVars = mkNameSet (map unLoc args)

    go :: LPat Name -> Maybe (LHsExpr Name)
    go (L loc (ConPatIn conName info))
      = do { let con = L loc (HsVar (unLoc conName))
           ; exprs <- mapM go (hsConPatArgs info)
           ; return $ foldl (\x y -> L loc (HsApp x y)) con exprs }
    go (L loc p) = fmap (L loc) $ go1 p

    go1 :: Pat Name -> Maybe (HsExpr Name)
    go1   (VarPat var)
      | var `elemNameSet` lhsVars  = return $ HsVar var
      | otherwise                  = Nothing
    go1   (LazyPat pat)            = fmap HsPar $ go pat
    go1   (ParPat pat)             = fmap HsPar $ go pat
    go1   (BangPat pat)            = fmap HsPar $ go pat
    go1   (PArrPat pats ptt)
      = do { exprs <- mapM go pats
           ; return $ ExplicitPArr ptt exprs }
    go1   (ListPat pats ptt reb)
      = do { exprs <- mapM go pats
           ; return $ ExplicitList ptt (fmap snd reb) exprs }
    go1   (TuplePat pats box _)
      = do { exprs <- mapM go pats
           ; return (ExplicitTuple (map (noLoc . Present) exprs) box)
           }
    go1   (LitPat lit)             = return $ HsLit lit
    go1   (NPat (L _ n) Nothing _) = return $ HsOverLit n
    go1   (NPat (L _ n) (Just neg) _)
      = return $ noLoc neg `HsApp` noLoc (HsOverLit n)
    go1   (SigPatIn pat (HsWB ty _ _ wcs))
      = do { expr <- go pat
           ; return $ ExprWithTySig expr ty wcs }
    go1   (ConPatOut{})            = panic "ConPatOut in output of renamer"
    go1   (SigPatOut{})            = panic "SigPatOut in output of renamer"
    go1   (CoPat{})                = panic "CoPat in output of renamer"
    go1   _                        = Nothing

-- Walk the whole pattern and for all ConPatOuts, collect the
-- existentially-bound type variables and evidence binding variables.
--
-- These are used in computing the type of a pattern synonym and also
-- in generating matcher functions, since success continuations need
-- to be passed these pattern-bound evidences.
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

    goRecFd :: LHsRecField Id (LPat Id) -> (TyVarSet, [EvVar])
    goRecFd (L _ HsRecField{ hsRecFieldArg = p }) = go p
