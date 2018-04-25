{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcPatSyn]{Typechecking pattern synonym declarations}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module TcPatSyn ( tcInferPatSynDecl, tcCheckPatSynDecl
                , tcPatSynBuilderBind, tcPatSynBuilderOcc, nonBidirectionalErr
  ) where

import GhcPrelude

import HsSyn
import TcPat
import Type( mkEmptyTCvSubst, tidyTyVarBinders, tidyTypes, tidyType )
import TcRnMonad
import TcSigs( emptyPragEnv, completeSigFromId )
import TcType( mkMinimalBySCs )
import TcEnv
import TcMType
import TcHsSyn( zonkTyVarBindersX, zonkTcTypeToTypes
              , zonkTcTypeToType, emptyZonkEnv )
import TysPrim
import TysWiredIn  ( runtimeRepTy )
import Name
import SrcLoc
import PatSyn
import NameSet
import Panic
import Outputable
import FastString
import Var
import VarEnv( emptyTidyEnv, mkInScopeSet )
import Id
import IdInfo( RecSelParent(..), setLevityInfoWithType )
import TcBinds
import BasicTypes
import TcSimplify
import TcUnify
import TcType
import TcEvidence
import BuildTyCl
import VarSet
import MkId
import TcTyDecls
import ConLike
import FieldLabel
import Bag
import Util
import ErrUtils
import Control.Monad ( zipWithM )
import Data.List( partition )

#include "HsVersions.h"

{-
************************************************************************
*                                                                      *
                    Type checking a pattern synonym
*                                                                      *
************************************************************************
-}

tcInferPatSynDecl :: PatSynBind GhcRn GhcRn
                  -> TcM (LHsBinds GhcTc, TcGblEnv)
tcInferPatSynDecl PSB{ psb_id = lname@(L _ name), psb_args = details,
                       psb_def = lpat, psb_dir = dir }
  = addPatSynCtxt lname $
    do { traceTc "tcInferPatSynDecl {" $ ppr name
       ; tcCheckPatSynPat lpat

       ; let (arg_names, rec_fields, is_infix) = collectPatSynArgInfo details
       ; (tclvl, wanted, ((lpat', args), pat_ty))
            <- pushLevelAndCaptureConstraints  $
               tcInferNoInst $ \ exp_ty ->
               tcPat PatSyn lpat exp_ty $
               mapM tcLookupId arg_names

       ; let named_taus = (name, pat_ty) : map (\arg -> (getName arg, varType arg)) args

       ; (qtvs, req_dicts, ev_binds, _) <- simplifyInfer tclvl NoRestrictions []
                                                         named_taus wanted

       ; let (ex_tvs, prov_dicts) = tcCollectEx lpat'
             ex_tv_set  = mkVarSet ex_tvs
             univ_tvs   = filterOut (`elemVarSet` ex_tv_set) qtvs
             req_theta  = map evVarPred req_dicts

       ; prov_dicts <- mapM zonkId prov_dicts
       ; let filtered_prov_dicts = mkMinimalBySCs evVarPred prov_dicts
             prov_theta = map evVarPred filtered_prov_dicts
             -- Filtering: see Note [Remove redundant provided dicts]

       ; traceTc "tcInferPatSynDecl }" $ (ppr name $$ ppr ex_tvs)
       ; tc_patsyn_finish lname dir is_infix lpat'
                          (mkTyVarBinders Inferred univ_tvs
                            , req_theta,  ev_binds, req_dicts)
                          (mkTyVarBinders Inferred ex_tvs
                            , mkTyVarTys ex_tvs, prov_theta, map EvId filtered_prov_dicts)
                          (map nlHsVar args, map idType args)
                          pat_ty rec_fields }

{- Note [Remove redundant provided dicts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Recall that
   HRefl :: forall k1 k2 (a1:k1) (a2:k2). (k1 ~ k2, a1 ~ a2)
                                       => a1 :~~: a2
(NB: technically the (k1~k2) existential dictionary is not necessary,
but it's there at the moment.)

Now consider (Trac #14394):
   pattern Foo = HRefl
in a non-poly-kinded module.  We don't want to get
    pattern Foo :: () => (* ~ *, b ~ a) => a :~~: b
with that redundant (* ~ *).  We'd like to remove it; hence the call to
mkMinimalWithSCs.

Similarly consider
  data S a where { MkS :: Ord a => a -> S a }
  pattern Bam x y <- (MkS (x::a), MkS (y::a)))

The pattern (Bam x y) binds two (Ord a) dictionaries, but we only
need one.  Agian mkMimimalWithSCs removes the redundant one.
-}

tcCheckPatSynDecl :: PatSynBind GhcRn GhcRn
                  -> TcPatSynInfo
                  -> TcM (LHsBinds GhcTc, TcGblEnv)
tcCheckPatSynDecl psb@PSB{ psb_id = lname@(L _ name), psb_args = details
                         , psb_def = lpat, psb_dir = dir }
                  TPSI{ patsig_implicit_bndrs = implicit_tvs
                      , patsig_univ_bndrs = explicit_univ_tvs, patsig_prov = prov_theta
                      , patsig_ex_bndrs   = explicit_ex_tvs,   patsig_req  = req_theta
                      , patsig_body_ty    = sig_body_ty }
  = addPatSynCtxt lname $
    do { let decl_arity = length arg_names
             (arg_names, rec_fields, is_infix) = collectPatSynArgInfo details

       ; traceTc "tcCheckPatSynDecl" $
         vcat [ ppr implicit_tvs, ppr explicit_univ_tvs, ppr req_theta
              , ppr explicit_ex_tvs, ppr prov_theta, ppr sig_body_ty ]

       ; tcCheckPatSynPat lpat

       ; (arg_tys, pat_ty) <- case tcSplitFunTysN decl_arity sig_body_ty of
                                 Right stuff  -> return stuff
                                 Left missing -> wrongNumberOfParmsErr name decl_arity missing

       -- Complain about:  pattern P :: () => forall x. x -> P x
       -- The existential 'x' should not appear in the result type
       -- Can't check this until we know P's arity
       ; let bad_tvs = filter (`elemVarSet` tyCoVarsOfType pat_ty) explicit_ex_tvs
       ; checkTc (null bad_tvs) $
         hang (sep [ text "The result type of the signature for" <+> quotes (ppr name) <> comma
                   , text "namely" <+> quotes (ppr pat_ty) ])
            2 (text "mentions existential type variable" <> plural bad_tvs
               <+> pprQuotedList bad_tvs)

         -- See Note [The pattern-synonym signature splitting rule]
       ; let univ_fvs = closeOverKinds $
                        (tyCoVarsOfTypes (pat_ty : req_theta) `extendVarSetList` explicit_univ_tvs)
             (extra_univ, extra_ex) = partition ((`elemVarSet` univ_fvs) . binderVar) implicit_tvs
             univ_bndrs = extra_univ ++ mkTyVarBinders Specified explicit_univ_tvs
             ex_bndrs   = extra_ex   ++ mkTyVarBinders Specified explicit_ex_tvs
             univ_tvs   = binderVars univ_bndrs
             ex_tvs     = binderVars ex_bndrs

       -- Right!  Let's check the pattern against the signature
       -- See Note [Checking against a pattern signature]
       ; req_dicts <- newEvVars req_theta
       ; (tclvl, wanted, (lpat', (ex_tvs', prov_dicts, args'))) <-
           ASSERT2( equalLength arg_names arg_tys, ppr name $$ ppr arg_names $$ ppr arg_tys )
           pushLevelAndCaptureConstraints            $
           tcExtendTyVarEnv univ_tvs                 $
           tcPat PatSyn lpat (mkCheckExpType pat_ty) $
           do { let in_scope    = mkInScopeSet (mkVarSet univ_tvs)
                    empty_subst = mkEmptyTCvSubst in_scope
              ; (subst, ex_tvs') <- mapAccumLM newMetaTyVarX empty_subst ex_tvs
                    -- newMetaTyVarX: see the "Existential type variables"
                    -- part of Note [Checking against a pattern signature]
              ; traceTc "tcpatsyn1" (vcat [ ppr v <+> dcolon <+> ppr (tyVarKind v) | v <- ex_tvs])
              ; traceTc "tcpatsyn2" (vcat [ ppr v <+> dcolon <+> ppr (tyVarKind v) | v <- ex_tvs'])
              ; let prov_theta' = substTheta subst prov_theta
                  -- Add univ_tvs to the in_scope set to
                  -- satisfy the substitution invariant. There's no need to
                  -- add 'ex_tvs' as they are already in the domain of the
                  -- substitution.
                  -- See also Note [The substitution invariant] in TyCoRep.
              ; prov_dicts <- mapM (emitWanted (ProvCtxtOrigin psb)) prov_theta'
              ; args'      <- zipWithM (tc_arg subst) arg_names arg_tys
              ; return (ex_tvs', prov_dicts, args') }

       ; let skol_info = SigSkol (PatSynCtxt name) pat_ty []
                         -- The type here is a bit bogus, but we do not print
                         -- the type for PatSynCtxt, so it doesn't matter
                         -- See TcRnTypes Note [Skolem info for pattern synonyms]
       ; (implics, ev_binds) <- buildImplicationFor tclvl skol_info univ_tvs req_dicts wanted

       -- Solve the constraints now, because we are about to make a PatSyn,
       -- which should not contain unification variables and the like (Trac #10997)
       ; simplifyTopImplic implics

       -- ToDo: in the bidirectional case, check that the ex_tvs' are all distinct
       -- Otherwise we may get a type error when typechecking the builder,
       -- when that should be impossible

       ; traceTc "tcCheckPatSynDecl }" $ ppr name
       ; tc_patsyn_finish lname dir is_infix lpat'
                          (univ_bndrs, req_theta, ev_binds, req_dicts)
                          (ex_bndrs, mkTyVarTys ex_tvs', prov_theta, prov_dicts)
                          (args', arg_tys)
                          pat_ty rec_fields }
  where
    tc_arg :: TCvSubst -> Name -> Type -> TcM (LHsExpr GhcTcId)
    tc_arg subst arg_name arg_ty
      = do {   -- Look up the variable actually bound by lpat
               -- and check that it has the expected type
             arg_id <- tcLookupId arg_name
           ; wrap <- tcSubType_NC GenSigCtxt
                                 (idType arg_id)
                                 (substTyUnchecked subst arg_ty)
                -- Why do we need tcSubType here?
                -- See Note [Pattern synonyms and higher rank types]
           ; return (mkLHsWrap wrap $ nlHsVar arg_id) }

{- [Pattern synonyms and higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data T = MkT (forall a. a->a)

  pattern P :: (Int -> Int) -> T
  pattern P x <- MkT x

This should work.  But in the matcher we must match against MkT, and then
instantiate its argument 'x', to get a function of type (Int -> Int).
Equality is not enough!  Trac #13752 was an example.

Note [Checking against a pattern signature]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking the actual supplied pattern against the pattern synonym
signature, we need to be quite careful.

----- Provided constraints
Example

    data T a where
      MkT :: Ord a => a -> T a

    pattern P :: () => Eq a => a -> [T a]
    pattern P x = [MkT x]

We must check that the (Eq a) that P claims to bind (and to
make available to matches against P), is derivable from the
actual pattern.  For example:
    f (P (x::a)) = ...here (Eq a) should be available...
And yes, (Eq a) is derivable from the (Ord a) bound by P's rhs.

----- Existential type variables
Unusually, we instantiate the existential tyvars of the pattern with
*meta* type variables.  For example

    data S where
      MkS :: Eq a => [a] -> S

    pattern P :: () => Eq x => x -> S
    pattern P x <- MkS x

The pattern synonym conceals from its client the fact that MkS has a
list inside it.  The client just thinks it's a type 'x'.  So we must
unify x := [a] during type checking, and then use the instantiating type
[a] (called ex_tys) when building the matcher.  In this case we'll get

   $mP :: S -> (forall x. Ex x => x -> r) -> r -> r
   $mP x k = case x of
               MkS a (d:Eq a) (ys:[a]) -> let dl :: Eq [a]
                                              dl = $dfunEqList d
                                          in k [a] dl ys

All this applies when type-checking the /matching/ side of
a pattern synonym.  What about the /building/ side?

* For Unidirectional, there is no builder

* For ExplicitBidirectional, the builder is completely separate
  code, typechecked in tcPatSynBuilderBind

* For ImplicitBidirectional, the builder is still typechecked in
  tcPatSynBuilderBind, by converting the pattern to an expression and
  typechecking it.

  At one point, for ImplicitBidirectional I used SigTvs (instead of
  TauTvs) in tcCheckPatSynDecl.  But (a) strengthening the check here
  is redundant since tcPatSynBuilderBind does the job, (b) it was
  still incomplete (SigTvs can unify with each other), and (c) it
  didn't even work (Trac #13441 was accepted with
  ExplicitBidirectional, but rejected if expressed in
  ImplicitBidirectional form.  Conclusion: trying to be too clever is
  a bad idea.
-}

collectPatSynArgInfo :: HsPatSynDetails (Located Name)
                     -> ([Name], [Name], Bool)
collectPatSynArgInfo details =
  case details of
    PrefixPatSyn names      -> (map unLoc names, [], False)
    InfixPatSyn name1 name2 -> (map unLoc [name1, name2], [], True)
    RecordPatSyn names ->
      let (vars, sels) = unzip (map splitRecordPatSyn names)
      in (vars, sels, False)

  where
    splitRecordPatSyn :: RecordPatSynField (Located Name)
                      -> (Name, Name)
    splitRecordPatSyn (RecordPatSynField { recordPatSynPatVar = L _ patVar
                                         , recordPatSynSelectorId = L _ selId })
      = (patVar, selId)

addPatSynCtxt :: Located Name -> TcM a -> TcM a
addPatSynCtxt (L loc name) thing_inside
  = setSrcSpan loc $
    addErrCtxt (text "In the declaration for pattern synonym"
                <+> quotes (ppr name)) $
    thing_inside

wrongNumberOfParmsErr :: Name -> Arity -> Arity -> TcM a
wrongNumberOfParmsErr name decl_arity missing
  = failWithTc $
    hang (text "Pattern synonym" <+> quotes (ppr name) <+> ptext (sLit "has")
          <+> speakNOf decl_arity (text "argument"))
       2 (text "but its type signature has" <+> int missing <+> text "fewer arrows")

-------------------------
-- Shared by both tcInferPatSyn and tcCheckPatSyn
tc_patsyn_finish :: Located Name      -- ^ PatSyn Name
                 -> HsPatSynDir GhcRn -- ^ PatSyn type (Uni/Bidir/ExplicitBidir)
                 -> Bool              -- ^ Whether infix
                 -> LPat GhcTc        -- ^ Pattern of the PatSyn
                 -> ([TcTyVarBinder], [PredType], TcEvBinds, [EvVar])
                 -> ([TcTyVarBinder], [TcType], [PredType], [EvTerm])
                 -> ([LHsExpr GhcTcId], [TcType])   -- ^ Pattern arguments and
                                                    -- types
                 -> TcType            -- ^ Pattern type
                 -> [Name]            -- ^ Selector names
                 -- ^ Whether fields, empty if not record PatSyn
                 -> TcM (LHsBinds GhcTc, TcGblEnv)
tc_patsyn_finish lname dir is_infix lpat'
                 (univ_tvs, req_theta, req_ev_binds, req_dicts)
                 (ex_tvs,   ex_tys,    prov_theta,   prov_dicts)
                 (args, arg_tys)
                 pat_ty field_labels
  = do { -- Zonk everything.  We are about to build a final PatSyn
         -- so there had better be no unification variables in there

         (ze, univ_tvs') <- zonkTyVarBindersX emptyZonkEnv univ_tvs
       ; req_theta'      <- zonkTcTypeToTypes ze req_theta
       ; (ze, ex_tvs')   <- zonkTyVarBindersX ze ex_tvs
       ; prov_theta'     <- zonkTcTypeToTypes ze prov_theta
       ; pat_ty'         <- zonkTcTypeToType ze pat_ty
       ; arg_tys'        <- zonkTcTypeToTypes ze arg_tys

       ; let (env1, univ_tvs) = tidyTyVarBinders emptyTidyEnv univ_tvs'
             (env2, ex_tvs)   = tidyTyVarBinders env1 ex_tvs'
             req_theta  = tidyTypes env2 req_theta'
             prov_theta = tidyTypes env2 prov_theta'
             arg_tys    = tidyTypes env2 arg_tys'
             pat_ty     = tidyType  env2 pat_ty'

       ; traceTc "tc_patsyn_finish {" $
           ppr (unLoc lname) $$ ppr (unLoc lpat') $$
           ppr (univ_tvs, req_theta, req_ev_binds, req_dicts) $$
           ppr (ex_tvs, prov_theta, prov_dicts) $$
           ppr args $$
           ppr arg_tys $$
           ppr pat_ty

       -- Make the 'matcher'
       ; (matcher_id, matcher_bind) <- tcPatSynMatcher lname lpat'
                                         (binderVars univ_tvs, req_theta, req_ev_binds, req_dicts)
                                         (binderVars ex_tvs, ex_tys, prov_theta, prov_dicts)
                                         (args, arg_tys)
                                         pat_ty

       -- Make the 'builder'
       ; builder_id <- mkPatSynBuilderId dir lname
                                         univ_tvs req_theta
                                         ex_tvs   prov_theta
                                         arg_tys pat_ty

         -- TODO: Make this have the proper information
       ; let mkFieldLabel name = FieldLabel { flLabel = occNameFS (nameOccName name)
                                            , flIsOverloaded = False
                                            , flSelector = name }
             field_labels' = map mkFieldLabel field_labels


       -- Make the PatSyn itself
       ; let patSyn = mkPatSyn (unLoc lname) is_infix
                        (univ_tvs, req_theta)
                        (ex_tvs, prov_theta)
                        arg_tys
                        pat_ty
                        matcher_id builder_id
                        field_labels'

       -- Selectors
       ; let rn_rec_sel_binds = mkPatSynRecSelBinds patSyn (patSynFieldLabels patSyn)
             tything = AConLike (PatSynCon patSyn)
       ; tcg_env <- tcExtendGlobalEnv [tything] $
                    tcRecSelBinds rn_rec_sel_binds

       ; traceTc "tc_patsyn_finish }" empty
       ; return (matcher_bind, tcg_env) }

{-
************************************************************************
*                                                                      *
         Constructing the "matcher" Id and its binding
*                                                                      *
************************************************************************
-}

tcPatSynMatcher :: Located Name
                -> LPat GhcTc
                -> ([TcTyVar], ThetaType, TcEvBinds, [EvVar])
                -> ([TcTyVar], [TcType], ThetaType, [EvTerm])
                -> ([LHsExpr GhcTcId], [TcType])
                -> TcType
                -> TcM ((Id, Bool), LHsBinds GhcTc)
-- See Note [Matchers and builders for pattern synonyms] in PatSyn
tcPatSynMatcher (L loc name) lpat
                (univ_tvs, req_theta, req_ev_binds, req_dicts)
                (ex_tvs, ex_tys, prov_theta, prov_dicts)
                (args, arg_tys) pat_ty
  = do { rr_name <- newNameAt (mkTyVarOcc "rep") loc
       ; tv_name <- newNameAt (mkTyVarOcc "r")   loc
       ; let rr_tv  = mkTcTyVar rr_name runtimeRepTy vanillaSkolemTv
             rr     = mkTyVarTy rr_tv
             res_tv = mkTcTyVar tv_name (tYPE rr) vanillaSkolemTv
             res_ty = mkTyVarTy res_tv
             is_unlifted = null args && null prov_dicts
             (cont_args, cont_arg_tys)
               | is_unlifted = ([nlHsVar voidPrimId], [voidPrimTy])
               | otherwise   = (args,                 arg_tys)
             cont_ty = mkInfSigmaTy ex_tvs prov_theta $
                       mkFunTys cont_arg_tys res_ty

             fail_ty  = mkFunTy voidPrimTy res_ty

       ; matcher_name <- newImplicitBinder name mkMatcherOcc
       ; scrutinee    <- newSysLocalId (fsLit "scrut") pat_ty
       ; cont         <- newSysLocalId (fsLit "cont")  cont_ty
       ; fail         <- newSysLocalId (fsLit "fail")  fail_ty

       ; let matcher_tau   = mkFunTys [pat_ty, cont_ty, fail_ty] res_ty
             matcher_sigma = mkInfSigmaTy (rr_tv:res_tv:univ_tvs) req_theta matcher_tau
             matcher_id    = mkExportedVanillaId matcher_name matcher_sigma
                             -- See Note [Exported LocalIds] in Id

             inst_wrap = mkWpEvApps prov_dicts <.> mkWpTyApps ex_tys
             cont' = foldl nlHsApp (mkLHsWrap inst_wrap (nlHsVar cont)) cont_args

             fail' = nlHsApps fail [nlHsVar voidPrimId]

             args = map nlVarPat [scrutinee, cont, fail]
             lwpat = noLoc $ WildPat pat_ty
             cases = if isIrrefutableHsPat lpat
                     then [mkHsCaseAlt lpat  cont']
                     else [mkHsCaseAlt lpat  cont',
                           mkHsCaseAlt lwpat fail']
             body = mkLHsWrap (mkWpLet req_ev_binds) $
                    L (getLoc lpat) $
                    HsCase (nlHsVar scrutinee) $
                    MG{ mg_alts = L (getLoc lpat) cases
                      , mg_arg_tys = [pat_ty]
                      , mg_res_ty = res_ty
                      , mg_origin = Generated
                      }
             body' = noLoc $
                     HsLam $
                     MG{ mg_alts = noLoc [mkSimpleMatch LambdaExpr
                                                        args body]
                       , mg_arg_tys = [pat_ty, cont_ty, fail_ty]
                       , mg_res_ty = res_ty
                       , mg_origin = Generated
                       }
             match = mkMatch (mkPrefixFunRhs (L loc name)) []
                             (mkHsLams (rr_tv:res_tv:univ_tvs)
                             req_dicts body')
                             (noLoc EmptyLocalBinds)
             mg :: MatchGroup GhcTc (LHsExpr GhcTc)
             mg = MG{ mg_alts = L (getLoc match) [match]
                    , mg_arg_tys = []
                    , mg_res_ty = res_ty
                    , mg_origin = Generated
                    }

       ; let bind = FunBind{ fun_id = L loc matcher_id
                           , fun_matches = mg
                           , fun_co_fn = idHsWrapper
                           , bind_fvs = emptyNameSet
                           , fun_tick = [] }
             matcher_bind = unitBag (noLoc bind)

       ; traceTc "tcPatSynMatcher" (ppr name $$ ppr (idType matcher_id))
       ; traceTc "tcPatSynMatcher" (ppr matcher_bind)

       ; return ((matcher_id, is_unlifted), matcher_bind) }

mkPatSynRecSelBinds :: PatSyn
                    -> [FieldLabel]  -- ^ Visible field labels
                    -> HsValBinds GhcRn
mkPatSynRecSelBinds ps fields
  = ValBindsOut selector_binds sigs
  where
    (sigs, selector_binds) = unzip (map mkRecSel fields)
    mkRecSel fld_lbl = mkOneRecordSelector [PatSynCon ps] (RecSelPatSyn ps) fld_lbl

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
                  -> [TyVarBinder] -> ThetaType
                  -> [TyVarBinder] -> ThetaType
                  -> [Type] -> Type
                  -> TcM (Maybe (Id, Bool))
mkPatSynBuilderId dir (L _ name)
                  univ_bndrs req_theta ex_bndrs prov_theta
                  arg_tys pat_ty
  | isUnidirectional dir
  = return Nothing
  | otherwise
  = do { builder_name <- newImplicitBinder name mkBuilderOcc
       ; let theta          = req_theta ++ prov_theta
             need_dummy_arg = isUnliftedType pat_ty && null arg_tys && null theta
             builder_sigma  = add_void need_dummy_arg $
                              mkForAllTys univ_bndrs $
                              mkForAllTys ex_bndrs $
                              mkFunTys theta $
                              mkFunTys arg_tys $
                              pat_ty
             builder_id     = mkExportedVanillaId builder_name builder_sigma
              -- See Note [Exported LocalIds] in Id

             builder_id'    = modifyIdInfo (`setLevityInfoWithType` pat_ty) builder_id

       ; return (Just (builder_id', need_dummy_arg)) }
  where

tcPatSynBuilderBind :: PatSynBind GhcRn GhcRn
                    -> TcM (LHsBinds GhcTc)
-- See Note [Matchers and builders for pattern synonyms] in PatSyn
tcPatSynBuilderBind (PSB { psb_id = L loc name, psb_def = lpat
                         , psb_dir = dir, psb_args = details })
  | isUnidirectional dir
  = return emptyBag

  | Left why <- mb_match_group       -- Can't invert the pattern
  = setSrcSpan (getLoc lpat) $ failWithTc $
    vcat [ hang (text "Invalid right-hand side of bidirectional pattern synonym"
                 <+> quotes (ppr name) <> colon)
              2 why
         , text "RHS pattern:" <+> ppr lpat ]

  | Right match_group <- mb_match_group  -- Bidirectional
  = do { patsyn <- tcLookupPatSyn name
       ; let Just (builder_id, need_dummy_arg) = patSynBuilder patsyn
                   -- Bidirectional, so patSynBuilder returns Just

             match_group' | need_dummy_arg = add_dummy_arg match_group
                          | otherwise      = match_group

             bind = FunBind { fun_id      = L loc (idName builder_id)
                            , fun_matches = match_group'
                            , fun_co_fn   = idHsWrapper
                            , bind_fvs    = placeHolderNamesTc
                            , fun_tick    = [] }

             sig = completeSigFromId (PatSynCtxt name) builder_id

       ; traceTc "tcPatSynBuilderBind {" $
         ppr patsyn $$ ppr builder_id <+> dcolon <+> ppr (idType builder_id)
       ; (builder_binds, _) <- tcPolyCheck emptyPragEnv sig (noLoc bind)
       ; traceTc "tcPatSynBuilderBind }" $ ppr builder_binds
       ; return builder_binds }

  | otherwise = panic "tcPatSynBuilderBind"  -- Both cases dealt with
  where
    mb_match_group
       = case dir of
           ExplicitBidirectional explicit_mg -> Right explicit_mg
           ImplicitBidirectional -> fmap mk_mg (tcPatToExpr name args lpat)
           Unidirectional -> panic "tcPatSynBuilderBind"

    mk_mg :: LHsExpr GhcRn -> MatchGroup GhcRn (LHsExpr GhcRn)
    mk_mg body = mkMatchGroup Generated [builder_match]
             where
               builder_args  = [L loc (VarPat (L loc n)) | L loc n <- args]
               builder_match = mkMatch (mkPrefixFunRhs (L loc name))
                                       builder_args body
                                       (noLoc EmptyLocalBinds)

    args = case details of
              PrefixPatSyn args     -> args
              InfixPatSyn arg1 arg2 -> [arg1, arg2]
              RecordPatSyn args     -> map recordPatSynPatVar args

    add_dummy_arg :: MatchGroup GhcRn (LHsExpr GhcRn)
                  -> MatchGroup GhcRn (LHsExpr GhcRn)
    add_dummy_arg mg@(MG { mg_alts = L l [L loc match@(Match { m_pats = pats })] })
      = mg { mg_alts = L l [L loc (match { m_pats = nlWildPatName : pats })] }
    add_dummy_arg other_mg = pprPanic "add_dummy_arg" $
                             pprMatches other_mg

tcPatSynBuilderOcc :: PatSyn -> TcM (HsExpr GhcTcId, TcSigmaType)
-- monadic only for failure
tcPatSynBuilderOcc ps
  | Just (builder_id, add_void_arg) <- builder
  , let builder_expr = HsConLikeOut (PatSynCon ps)
        builder_ty   = idType builder_id
  = return $
    if add_void_arg
    then ( builder_expr   -- still just return builder_expr; the void# arg is added
                          -- by dsConLike in the desugarer
         , tcFunResultTy builder_ty )
    else (builder_expr, builder_ty)

  | otherwise  -- Unidirectional
  = nonBidirectionalErr name
  where
    name    = patSynName ps
    builder = patSynBuilder ps

add_void :: Bool -> Type -> Type
add_void need_dummy_arg ty
  | need_dummy_arg = mkFunTy voidPrimTy ty
  | otherwise      = ty

tcPatToExpr :: Name -> [Located Name] -> LPat GhcRn
            -> Either MsgDoc (LHsExpr GhcRn)
-- Given a /pattern/, return an /expression/ that builds a value
-- that matches the pattern.  E.g. if the pattern is (Just [x]),
-- the expression is (Just [x]).  They look the same, but the
-- input uses constructors from HsPat and the output uses constructors
-- from HsExpr.
--
-- Returns (Left r) if the pattern is not invertible, for reason r.
-- See Note [Builder for a bidirectional pattern synonym]
tcPatToExpr name args pat = go pat
  where
    lhsVars = mkNameSet (map unLoc args)

    -- Make a prefix con for prefix and infix patterns for simplicity
    mkPrefixConExpr :: Located Name -> [LPat GhcRn]
                    -> Either MsgDoc (HsExpr GhcRn)
    mkPrefixConExpr lcon@(L loc _) pats
      = do { exprs <- mapM go pats
           ; return (foldl (\x y -> HsApp (L loc x) y)
                           (HsVar lcon) exprs) }

    mkRecordConExpr :: Located Name -> HsRecFields GhcRn (LPat GhcRn)
                    -> Either MsgDoc (HsExpr GhcRn)
    mkRecordConExpr con fields
      = do { exprFields <- mapM go fields
           ; return (RecordCon con PlaceHolder noPostTcExpr exprFields) }

    go :: LPat GhcRn -> Either MsgDoc (LHsExpr GhcRn)
    go (L loc p) = L loc <$> go1 p

    go1 :: Pat GhcRn -> Either MsgDoc (HsExpr GhcRn)
    go1 (ConPatIn con info)
      = case info of
          PrefixCon ps  -> mkPrefixConExpr con ps
          InfixCon l r  -> mkPrefixConExpr con [l,r]
          RecCon fields -> mkRecordConExpr con fields

    go1 (SigPatIn pat _) = go1 (unLoc pat)
        -- See Note [Type signatures and the builder expression]

    go1 (VarPat (L l var))
        | var `elemNameSet` lhsVars
        = return $ HsVar (L l var)
        | otherwise
        = Left (quotes (ppr var) <+> text "is not bound by the LHS of the pattern synonym")
    go1 (ParPat pat)                = fmap HsPar $ go pat
    go1 (PArrPat pats ptt)          = do { exprs <- mapM go pats
                                         ; return $ ExplicitPArr ptt exprs }
    go1 p@(ListPat pats ptt reb)
      | Nothing <- reb              = do { exprs <- mapM go pats
                                         ; return $ ExplicitList ptt Nothing exprs }
      | otherwise                   = notInvertibleListPat p
    go1 (TuplePat pats box _)       = do { exprs <- mapM go pats
                                         ; return $ ExplicitTuple
                                              (map (noLoc . Present) exprs) box }
    go1 (SumPat pat alt arity _)    = do { expr <- go1 (unLoc pat)
                                         ; return $ ExplicitSum alt arity (noLoc expr) PlaceHolder
                                         }
    go1 (LitPat lit)                = return $ HsLit lit
    go1 (NPat (L _ n) mb_neg _ _)
        | Just neg <- mb_neg        = return $ unLoc $ nlHsSyntaxApps neg [noLoc (HsOverLit n)]
        | otherwise                 = return $ HsOverLit n
    go1 (ConPatOut{})               = panic "ConPatOut in output of renamer"
    go1 (SigPatOut{})               = panic "SigPatOut in output of renamer"
    go1 (CoPat{})                   = panic "CoPat in output of renamer"
    go1 (SplicePat (HsSpliced _ (HsSplicedPat pat)))
                                    = go1 pat
    go1 (SplicePat (HsSpliced{}))   = panic "Invalid splice variety"

    -- The following patterns are not invertible.
    go1 p@(BangPat {})                     = notInvertible p -- #14112
    go1 p@(LazyPat {})                     = notInvertible p
    go1 p@(WildPat {})                     = notInvertible p
    go1 p@(AsPat {})                       = notInvertible p
    go1 p@(ViewPat {})                     = notInvertible p
    go1 p@(NPlusKPat {})                   = notInvertible p
    go1 p@(SplicePat (HsTypedSplice {}))   = notInvertible p
    go1 p@(SplicePat (HsUntypedSplice {})) = notInvertible p
    go1 p@(SplicePat (HsQuasiQuote {}))    = notInvertible p

    notInvertible p = Left (not_invertible_msg p)

    not_invertible_msg p
      =   text "Pattern" <+> quotes (ppr p) <+> text "is not invertible"
      $+$ hang (text "Suggestion: instead use an explicitly bidirectional"
                <+> text "pattern synonym, e.g.")
             2 (hang (text "pattern" <+> pp_name <+> pp_args <+> larrow
                      <+> ppr pat <+> text "where")
                   2 (pp_name <+> pp_args <+> equals <+> text "..."))
      where
        pp_name = ppr name
        pp_args = hsep (map ppr args)

    -- We should really be able to invert list patterns, even when
    -- rebindable syntax is on, but doing so involves a bit of
    -- refactoring; see Trac #14380.  Until then we reject with a
    -- helpful error message.
    notInvertibleListPat p
      = Left (vcat [ not_invertible_msg p
                   , text "Reason: rebindable syntax is on."
                   , text "This is fixable: add use-case to Trac #14380" ])

{- Note [Builder for a bidirectional pattern synonym]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a bidirectional pattern synonym we need to produce an /expression/
that matches the supplied /pattern/, given values for the arguments
of the pattern synoymy.  For example
  pattern F x y = (Just x, [y])
The 'builder' for F looks like
  $builderF x y = (Just x, [y])

We can't always do this:
 * Some patterns aren't invertible; e.g. view patterns
      pattern F x = (reverse -> x:_)

 * The RHS pattern might bind more variables than the pattern
   synonym, so again we can't invert it
      pattern F x = (x,y)

 * Ditto wildcards
      pattern F x = (x,_)


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

tcCheckPatSynPat :: LPat GhcRn -> TcM ()
tcCheckPatSynPat = go
  where
    go :: LPat GhcRn -> TcM ()
    go = addLocM go1

    go1 :: Pat GhcRn -> TcM ()
    -- See Note [Bad patterns]
    go1 p@(AsPat _ _)         = asPatInPatSynErr p
    go1 p@NPlusKPat{}         = nPlusKPatInPatSynErr p

    go1   (ConPatIn _ info)   = mapM_ go (hsConPatArgs info)
    go1   VarPat{}            = return ()
    go1   WildPat{}           = return ()
    go1   (LazyPat pat)       = go pat
    go1   (ParPat pat)        = go pat
    go1   (BangPat pat)       = go pat
    go1   (PArrPat pats _)    = mapM_ go pats
    go1   (ListPat pats _ _)  = mapM_ go pats
    go1   (TuplePat pats _ _) = mapM_ go pats
    go1   (SumPat pat _ _ _)  = go pat
    go1   LitPat{}            = return ()
    go1   NPat{}              = return ()
    go1   (SigPatIn pat _)    = go pat
    go1   (ViewPat _ pat _)   = go pat
    go1   (SplicePat splice)
      | HsSpliced mod_finalizers (HsSplicedPat pat) <- splice
                              = do addModFinalizersWithLclEnv mod_finalizers
                                   go1 pat
      | otherwise             = panic "non-pattern from spliced thing"
    go1   ConPatOut{}         = panic "ConPatOut in output of renamer"
    go1   SigPatOut{}         = panic "SigPatOut in output of renamer"
    go1   CoPat{}             = panic "CoPat in output of renamer"

asPatInPatSynErr :: (SourceTextX p, OutputableBndrId p) => Pat p -> TcM a
asPatInPatSynErr pat
  = failWithTc $
    hang (text "Pattern synonym definition cannot contain as-patterns (@):")
       2 (ppr pat)

nPlusKPatInPatSynErr :: (SourceTextX p, OutputableBndrId p) => Pat p -> TcM a
nPlusKPatInPatSynErr pat
  = failWithTc $
    hang (text "Pattern synonym definition cannot contain n+k-pattern:")
       2 (ppr pat)

{- Note [Bad patterns]
~~~~~~~~~~~~~~~~~~~~~~
We don't currently allow as-patterns or n+k patterns in a pattern synonym.
Reason: consider
  pattern P x y = x@(Just y)

What would
  f (P Nothing False) = e
mean?  Presumably something like
  f Nothing@(Just False) = e
But as-patterns don't allow a pattern before the @ sign!  Perhaps they
should -- with p1@p2 meaning match both p1 and p2 -- but they don't
currently.  Hence bannning them in pattern synonyms.  Actually lifting
the restriction would be simple and well-defined.  See Trac #9793.
-}


nonBidirectionalErr :: Outputable name => name -> TcM a
nonBidirectionalErr name = failWithTc $
    text "non-bidirectional pattern synonym"
    <+> quotes (ppr name) <+> text "used in an expression"

-- Walk the whole pattern and for all ConPatOuts, collect the
-- existentially-bound type variables and evidence binding variables.
--
-- These are used in computing the type of a pattern synonym and also
-- in generating matcher functions, since success continuations need
-- to be passed these pattern-bound evidences.
tcCollectEx
  :: LPat GhcTc
  -> ( [TyVar]        -- Existentially-bound type variables
                      -- in correctly-scoped order; e.g. [ k:*, x:k ]
     , [EvVar] )      -- and evidence variables

tcCollectEx pat = go pat
  where
    go :: LPat GhcTc -> ([TyVar], [EvVar])
    go = go1 . unLoc

    go1 :: Pat GhcTc -> ([TyVar], [EvVar])
    go1 (LazyPat p)         = go p
    go1 (AsPat _ p)         = go p
    go1 (ParPat p)          = go p
    go1 (BangPat p)         = go p
    go1 (ListPat ps _ _)    = mergeMany . map go $ ps
    go1 (TuplePat ps _ _)   = mergeMany . map go $ ps
    go1 (SumPat p _ _ _)    = go p
    go1 (PArrPat ps _)      = mergeMany . map go $ ps
    go1 (ViewPat _ p _)     = go p
    go1 con@ConPatOut{}     = merge (pat_tvs con, pat_dicts con) $
                              goConDetails $ pat_args con
    go1 (SigPatOut p _)     = go p
    go1 (CoPat _ p _)       = go1 p
    go1 (NPlusKPat n k _ geq subtract _)
      = pprPanic "TODO: NPlusKPat" $ ppr n $$ ppr k $$ ppr geq $$ ppr subtract
    go1 _                   = empty

    goConDetails :: HsConPatDetails GhcTc -> ([TyVar], [EvVar])
    goConDetails (PrefixCon ps) = mergeMany . map go $ ps
    goConDetails (InfixCon p1 p2) = go p1 `merge` go p2
    goConDetails (RecCon HsRecFields{ rec_flds = flds })
      = mergeMany . map goRecFd $ flds

    goRecFd :: LHsRecField GhcTc (LPat GhcTc) -> ([TyVar], [EvVar])
    goRecFd (L _ HsRecField{ hsRecFieldArg = p }) = go p

    merge (vs1, evs1) (vs2, evs2) = (vs1 ++ vs2, evs1 ++ evs2)
    mergeMany = foldr merge empty
    empty     = ([], [])
