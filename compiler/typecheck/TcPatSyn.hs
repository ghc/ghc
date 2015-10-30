{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcPatSyn]{Typechecking pattern synonym declarations}
-}

{-# LANGUAGE CPP #-}

module TcPatSyn ( tcInferPatSynDecl, tcCheckPatSynDecl
                , tcPatSynBuilderBind, tcPatSynBuilderOcc, nonBidirectionalErr
  ) where

import HsSyn
import TcPat
import TcRnMonad
import TcEnv
import TcMType
import TysPrim
import TypeRep
import Name
import SrcLoc
import PatSyn
import NameSet
import Panic
import Outputable
import FastString
import Var
import Id
import IdInfo( IdDetails(..), RecSelParent(..))
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
import TcTyDecls
import ConLike
import FieldLabel
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
                  -> TcM (PatSyn, LHsBinds Id, TcGblEnv)
tcInferPatSynDecl PSB{ psb_id = lname@(L loc name), psb_args = details,
                       psb_def = lpat, psb_dir = dir }
  = setSrcSpan loc $
    do { traceTc "tcInferPatSynDecl {" $ ppr name
       ; tcCheckPatSynPat lpat

       ; let (arg_names, rec_fields, is_infix) = collectPatSynArgInfo details
       ; (tclvl, wanted, (lpat', (args, pat_ty)))
            <- pushLevelAndCaptureConstraints  $
               do { pat_ty <- newFlexiTyVarTy openTypeKind
                  ; tcPat PatSyn lpat pat_ty $
               do { args <- mapM tcLookupId arg_names
                  ; return (args, pat_ty) } }

       ; let named_taus = (name, pat_ty) : map (\arg -> (getName arg, varType arg)) args

       ; (qtvs, req_dicts, ev_binds) <- simplifyInfer tclvl False [] named_taus wanted

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
                          pat_ty rec_fields }


tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (PatSyn, LHsBinds Id, TcGblEnv)
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
       ; let skol_info = SigSkol (PatSynCtxt name) (mkFunTys arg_tys pat_ty)

       ; let (arg_names, rec_fields, is_infix) = collectPatSynArgInfo details

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
                          pat_ty rec_fields  }
  where
    (arg_tys, pat_ty) = tcSplitFunTys tau

collectPatSynArgInfo :: HsPatSynDetails (Located Name) -> ([Name], [Name], Bool)
collectPatSynArgInfo details =
  case details of
    PrefixPatSyn names      -> (map unLoc names, [], False)
    InfixPatSyn name1 name2 -> (map unLoc [name1, name2], [], True)
    RecordPatSyn names ->
      let (vars, sels) = unzip (map splitRecordPatSyn names)
      in (vars, sels, False)

  where
    splitRecordPatSyn :: RecordPatSynField (Located Name) -> (Name, Name)
    splitRecordPatSyn (RecordPatSynField { recordPatSynPatVar = L _ patVar
                                         , recordPatSynSelectorId = L _ selId })
      = (patVar, selId)

wrongNumberOfParmsErr :: Arity -> SDoc
wrongNumberOfParmsErr ty_arity
  = ptext (sLit "Number of pattern synonym arguments doesn't match type; expected")
    <+> ppr ty_arity

-------------------------
-- Shared by both tcInferPatSyn and tcCheckPatSyn
tc_patsyn_finish :: Located Name  -- ^ PatSyn Name
                 -> HsPatSynDir Name  -- ^ PatSyn type (Uni/Bidir/ExplicitBidir)
                 -> Bool              -- ^ Whether infix
                 -> LPat Id           -- ^ Pattern of the PatSyn
                 -> ([TcTyVar], [PredType], TcEvBinds, [EvVar])
                 -> ([TcTyVar], [TcType], [PredType], TcEvBinds, [EvVar])
                 -> [(Var, HsWrapper)]  -- ^ Pattern arguments
                 -> TcType              -- ^ Pattern type
                 -> [Name]              -- ^ Selector names
                 -- ^ Whether fields, empty if not record PatSyn
                 -> TcM (PatSyn, LHsBinds Id, TcGblEnv)
tc_patsyn_finish lname dir is_infix lpat'
                 (univ_tvs, req_theta, req_ev_binds, req_dicts)
                 (ex_tvs, subst, prov_theta, prov_ev_binds, prov_dicts)
                 wrapped_args
                 pat_ty field_labels
  = do { -- Zonk everything.  We are about to build a final PatSyn
         -- so there had better be no unification variables in there
         univ_tvs     <- mapM zonkQuantifiedTyVar univ_tvs
       ; ex_tvs       <- mapM zonkQuantifiedTyVar ex_tvs
       ; prov_theta   <- zonkTcThetaType prov_theta
       ; req_theta    <- zonkTcThetaType req_theta
       ; pat_ty       <- zonkTcType pat_ty
       ; wrapped_args <- mapM zonk_wrapped_arg wrapped_args
       ; let qtvs    = univ_tvs ++ ex_tvs
             -- See Note [Record PatSyn Desugaring]
             theta   = prov_theta ++ req_theta
             arg_tys = map (varType . fst) wrapped_args

       ; (patSyn, matcher_bind) <- fixM $ \ ~(patSyn,_) -> do {

        traceTc "tc_patsyn_finish {" $
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
       ; builder_id <- mkPatSynBuilderId dir lname qtvs theta
                                         arg_tys pat_ty patSyn

         -- TODO: Make this have the proper information
       ; let mkFieldLabel name = FieldLabel (occNameFS (nameOccName name)) False name
             field_labels' = (map mkFieldLabel field_labels)


       -- Make the PatSyn itself
       ; let patSyn' = mkPatSyn (unLoc lname) is_infix
                        (univ_tvs, req_theta)
                        (ex_tvs, prov_theta)
                        arg_tys
                        pat_ty
                        matcher_id builder_id
                        field_labels'
       ; return (patSyn', matcher_bind) }

       -- Selectors
       ; let (sigs, selector_binds) =
                unzip (mkPatSynRecSelBinds patSyn (patSynFieldLabels patSyn))
       ; let tything = AConLike (PatSynCon patSyn)
       ; tcg_env <-
          tcExtendGlobalEnv [tything] $
            tcRecSelBinds
              (ValBindsOut (zip (repeat NonRecursive) selector_binds) sigs)

       ; return (patSyn, matcher_bind, tcg_env) }

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

mkPatSynRecSelBinds :: PatSyn
                    -> [FieldLabel]
                    -- ^ Visible field labels
                    -> [(LSig Name, LHsBinds Name)]
mkPatSynRecSelBinds ps fields =
    map (mkOneRecordSelector [PatSynCon ps] (RecSelPatSyn ps)) fields

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
                  -> [TyVar] -> ThetaType -> [Type] -> Type -> PatSyn
                  -> TcM (Maybe (Id, Bool))
mkPatSynBuilderId dir  (L _ name) qtvs theta arg_tys pat_ty pat_syn
  | isUnidirectional dir
  = return Nothing
  | otherwise
  = do { builder_name <- newImplicitBinder name mkBuilderOcc
       ; let builder_sigma = mkSigmaTy qtvs theta (mkFunTys builder_arg_tys pat_ty)
             builder_id    =
              -- See Note [Exported LocalIds] in Id
              mkExportedLocalId (PatSynBuilderId pat_syn)
                                builder_name builder_sigma
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

  | otherwise  -- Bidirectional
  = do { patsyn <- tcLookupPatSyn name
       ; let Just (builder_id, need_dummy_arg) = patSynBuilder patsyn
                   -- Bidirectional, so patSynBuilder returns Just

             match_group' | need_dummy_arg = add_dummy_arg match_group
                          | otherwise      = match_group

             bind = FunBind { fun_id      = L loc (idName builder_id)
                            , fun_infix   = False
                            , fun_matches = match_group'
                            , fun_co_fn   = idHsWrapper
                            , bind_fvs    = placeHolderNamesTc
                            , fun_tick    = [] }

       ; sig <- instTcTySigFromId builder_id
                -- See Note [Redundant constraints for builder]

       ; (builder_binds, _) <- tcPolyCheck NonRecursive emptyPragEnv sig (noLoc bind)
       ; traceTc "tcPatSynBuilderBind }" $ ppr builder_binds
       ; return builder_binds }
  where
    Just match_group = mb_match_group
    mb_match_group
       = case dir of
           Unidirectional                    -> Nothing
           ExplicitBidirectional explicit_mg -> Just explicit_mg
           ImplicitBidirectional             -> fmap mk_mg (tcPatToExpr args lpat)

    mk_mg :: LHsExpr Name -> MatchGroup Name (LHsExpr Name)
    mk_mg body = mkMatchGroupName Generated [builder_match]
               where
                 builder_args  = [L loc (VarPat n) | L loc n <- args]
                 builder_match = mkMatch builder_args body EmptyLocalBinds

    args = case details of
              PrefixPatSyn args     -> args
              InfixPatSyn arg1 arg2 -> [arg1, arg2]
              RecordPatSyn args     -> map recordPatSynPatVar args

    add_dummy_arg :: MatchGroup Name (LHsExpr Name) -> MatchGroup Name (LHsExpr Name)
    add_dummy_arg mg@(MG { mg_alts = [L loc (Match Nothing [] ty grhss)] })
      = mg { mg_alts = [L loc (Match Nothing [nlWildPatName] ty grhss)] }
    add_dummy_arg other_mg = pprPanic "add_dummy_arg" $
                             pprMatches (PatSyn :: HsMatchContext Name) other_mg

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
    = nonBidirectionalErr name
  where
    name    = patSynName ps
    builder = patSynBuilder ps

{-
Note [Redundant constraints for builder]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The builder can have redundant constraints, which are awkard to eliminate.
Consider
   pattern P = Just 34
To match against this pattern we need (Eq a, Num a).  But to build
(Just 34) we need only (Num a).  Fortunately instTcSigFromId sets
sig_warn_redundant to False.

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

Note [Type signatures and the builder expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   pattern L x = Left x :: Either [a] [b]

In tc{Infer/Check}PatSynDecl we will check that the pattern has the
specified type.  We check the pattern *as a pattern*, so the type
signature is a pattern signature, and so brings 'a' and 'b' into
scope.  But we don't have a way to bind 'a, b' in the LHS, as we do
'x', say.  Nevertheless, the sigature may be useful to constrain
the type.

When making the binding for the *builder*, though, we don't want
  $buildL x = Left x :: Either [a] [b]
because that wil either mean (forall a b. Either [a] [b]), or we'll
get a complaint that 'a' and 'b' are out of scope. (Actually the
latter; Trac #9867.)  No, the job of the signature is done, so when
converting the pattern to an expression (for the builder RHS) we
simply discard the signature.

Note [Record PatSyn Desugaring]
-------------------------------

It is important that prov_theta comes before req_theta as this ordering is used
when desugaring record pattern synonym updates.

Any change to this ordering should make sure to change deSugar/DsExpr.hs if you
want to avoid difficult to decipher core lint errors!
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

nonBidirectionalErr :: Outputable name => name -> TcM a
nonBidirectionalErr name = failWithTc $
    ptext (sLit "non-bidirectional pattern synonym")
    <+> quotes (ppr name) <+> ptext (sLit "used in an expression")

tcPatToExpr :: [Located Name] -> LPat Name -> Maybe (LHsExpr Name)
tcPatToExpr args = go
  where
    lhsVars = mkNameSet (map unLoc args)

    go :: LPat Name -> Maybe (LHsExpr Name)
    go (L loc (ConPatIn (L _ con) info))
      = do { exprs <- mapM go (hsConPatArgs info)
           ; return $ L loc $
             foldl (\x y -> HsApp (L loc x) y) (HsVar con) exprs }

    go (L _ (SigPatIn pat _)) = go pat
        -- See Note [Type signatures and the builder expression]

    go (L loc p) = fmap (L loc) $ go1 p

    go1 :: Pat Name -> Maybe (HsExpr Name)
    go1   (VarPat var)
      | var `elemNameSet` lhsVars     = return $ HsVar var
      | otherwise                     = Nothing
    go1   (LazyPat pat)               = fmap HsPar $ go pat
    go1   (ParPat pat)                = fmap HsPar $ go pat
    go1   (BangPat pat)               = fmap HsPar $ go pat
    go1   (PArrPat pats ptt)          = do { exprs <- mapM go pats
                                           ; return $ ExplicitPArr ptt exprs }
    go1   (ListPat pats ptt reb)      = do { exprs <- mapM go pats
                                           ; return $ ExplicitList ptt (fmap snd reb) exprs }
    go1   (TuplePat pats box _)       = do { exprs <- mapM go pats
                                           ; return $ ExplicitTuple
                                                (map (noLoc . Present) exprs) box }
    go1   (LitPat lit)                = return $ HsLit lit
    go1   (NPat (L _ n) Nothing _)    = return $ HsOverLit n
    go1   (NPat (L _ n) (Just neg) _) = return $ noLoc neg `HsApp` noLoc (HsOverLit n)
    go1   (ConPatOut{})               = panic "ConPatOut in output of renamer"
    go1   (SigPatOut{})               = panic "SigPatOut in output of renamer"
    go1   (CoPat{})                   = panic "CoPat in output of renamer"
    go1   _                           = Nothing

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
