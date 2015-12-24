{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcPatSyn]{Typechecking pattern synonym declarations}
-}

{-# LANGUAGE CPP #-}

module TcPatSyn ( tcPatSynSig, tcInferPatSynDecl, tcCheckPatSynDecl
                , tcPatSynBuilderBind, tcPatSynBuilderOcc, nonBidirectionalErr
  ) where

import HsSyn
import TcPat
import TcHsType( tcImplicitTKBndrs, tcHsTyVarBndrs
               , tcHsContext, tcHsLiftedType, tcHsOpenType )
import TcRnMonad
import TcEnv
import TcMType
import TysPrim
import TysWiredIn  ( levityTy )
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
import TcTyDecls
import ConLike
import FieldLabel
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid( mconcat, mappend, mempty )
#endif
import Bag
import Util
import Data.Maybe
import Control.Monad ( unless, zipWithM )
import Data.List( partition )
import Pair( Pair(..) )

#include "HsVersions.h"

{- *********************************************************************
*                                                                      *
        Type checking a pattern synonym signature
*                                                                      *
************************************************************************

Note [Pattern synonym signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Pattern synonym signatures are surprisingly tricky (see Trac #11224 for example).
In general they look like this:

   pattern P :: forall univ_tvs. req
             => forall ex_tvs. prov
             => arg1 -> .. -> argn -> body_ty

For parsing and renaming we treat the signature as an ordinary LHsSigType.

Once we get to type checking, we decompose it into its parts, in tcPatSynSig.

* Note that 'forall univ_tvs' and 'req =>'
        and 'forall ex_tvs'   and 'prov =>'
  are all optional.  We gather the pieces at the the top of tcPatSynSig

* Initially the implicitly-bound tyvars (added by the renamer) include both
  universal and existential vars.

* After we kind-check the pieces and convert to Types, we do kind generalisation.

* Note [Splitting the implicit tyvars in a pattern synonym]
  Now the tricky bit: we must split
     the implicitly-bound variables, and
     the kind-generalised variables
  into universal and existential.  We do so as follows:

     Note [The pattern-synonym signature splitting rule]
     the universals are the ones mentioned in
          - univ_tvs (and the kinds thereof)
          - prov
          - body_ty
     the existential are the rest

* Moreover see Note
-}

tcPatSynSig :: Name -> LHsSigType Name -> TcM TcPatSynInfo
tcPatSynSig name sig_ty
  | HsIB { hsib_vars = implicit_hs_tvs
         , hsib_body = hs_ty }  <- sig_ty
  , (univ_hs_tvs, hs_req,  hs_ty1) <- splitLHsSigmaTy hs_ty
  , (ex_hs_tvs,   hs_prov, hs_ty2) <- splitLHsSigmaTy hs_ty1
  , (hs_arg_tys, hs_body_ty)       <- splitHsFunType  hs_ty2
  = do { (implicit_tvs, (univ_tvs, req, ex_tvs, prov, arg_tys, body_ty))
           <- solveEqualities $
              tcImplicitTKBndrs implicit_hs_tvs $
              tcHsTyVarBndrs univ_hs_tvs  $ \ univ_tvs ->
              tcHsTyVarBndrs ex_hs_tvs    $ \ ex_tvs   ->
              do { req     <- tcHsContext hs_req
                 ; prov    <- tcHsContext hs_prov
                 ; arg_tys <- mapM tcHsOpenType (hs_arg_tys :: [LHsType Name])
                 ; body_ty <- tcHsLiftedType hs_body_ty
                 ; let bound_tvs
                         = unionVarSets [ allBoundVariabless req
                                        , allBoundVariabless prov
                                        , allBoundVariabless (body_ty : arg_tys)
                                        ]
                 ; return ( (univ_tvs, req, ex_tvs, prov, arg_tys, body_ty)
                          , bound_tvs) }

       -- These are /signatures/ so we zonk to squeeze out any kind
       -- unification variables.
       -- ToDo: checkValidType?
       ; implicit_tvs <- mapM zonkTcTyCoVarBndr implicit_tvs
       ; univ_tvs     <- mapM zonkTcTyCoVarBndr univ_tvs
       ; ex_tvs       <- mapM zonkTcTyCoVarBndr ex_tvs
       ; req          <- zonkTcTypes req
       ; prov         <- zonkTcTypes prov
       ; arg_tys      <- zonkTcTypes arg_tys
       ; body_ty      <- zonkTcType  body_ty

       -- Kind generalisation; c.f. kindGeneralise
       ; let free_kvs = tyCoVarsOfTelescope (implicit_tvs ++ univ_tvs ++ ex_tvs) $
                        tyCoVarsOfTypes (body_ty : req ++ prov ++ arg_tys)

       ; kvs <- quantifyTyVars emptyVarSet (Pair free_kvs emptyVarSet)

       -- Complain about:  pattern P :: () => forall x. x -> P x
       -- The renamer thought it was fine, but the existential 'x'
       -- should not appear in the result type
       ; let bad_tvs = filter (`elemVarSet` tyCoVarsOfType body_ty) ex_tvs
       ; unless (null bad_tvs) $ addErr $
         hang (ptext (sLit "The result type") <+> quotes (ppr body_ty))
            2 (ptext (sLit "mentions existential type variable") <> plural bad_tvs
               <+> pprQuotedList bad_tvs)

         -- Split [Splitting the implicit tyvars in a pattern synonym]
       ; let univ_fvs = closeOverKinds $
                        (tyCoVarsOfTypes (body_ty : req) `extendVarSetList` univ_tvs)
             (extra_univ, extra_ex) = partition (`elemVarSet` univ_fvs) $
                                      kvs ++ implicit_tvs
       ; traceTc "tcTySig }" $
         vcat [ text "implicit_tvs" <+> ppr implicit_tvs
              , text "kvs" <+> ppr kvs
              , text "extra_univ" <+> ppr extra_univ
              , text "univ_tvs" <+> ppr univ_tvs
              , text "req" <+> ppr req
              , text "extra_ex" <+> ppr extra_ex
              , text "ex_tvs_" <+> ppr ex_tvs
              , text "prov" <+> ppr prov
              , text "arg_tys" <+> ppr arg_tys
              , text "body_ty" <+> ppr body_ty ]
       ; return (TPSI { patsig_name = name
                      , patsig_univ_tvs = extra_univ ++ univ_tvs
                      , patsig_req      = req
                      , patsig_ex_tvs   = extra_ex   ++ ex_tvs
                      , patsig_prov     = prov
                      , patsig_arg_tys  = arg_tys
                      , patsig_body_ty  = body_ty }) }


{-
************************************************************************
*                                                                      *
                    Type checking a pattern synonym
*                                                                      *
************************************************************************
-}

tcInferPatSynDecl :: PatSynBind Name Name
                  -> TcM (LHsBinds Id, TcGblEnv)
tcInferPatSynDecl PSB{ psb_id = lname@(L _ name), psb_args = details,
                       psb_def = lpat, psb_dir = dir }
  = addPatSynCtxt lname $
    do { traceTc "tcInferPatSynDecl {" $ ppr name
       ; tcCheckPatSynPat lpat

       ; let (arg_names, rec_fields, is_infix) = collectPatSynArgInfo details
       ; (tclvl, wanted, (lpat', (args, pat_ty)))
            <- pushLevelAndCaptureConstraints  $
               do { pat_ty <- newOpenFlexiTyVarTy
                  ; tcPat PatSyn lpat pat_ty $
               do { args <- mapM tcLookupId arg_names
                  ; return (args, pat_ty) } }

       ; let named_taus = (name, pat_ty) : map (\arg -> (getName arg, varType arg)) args

       ; (qtvs, req_dicts, ev_binds) <- simplifyInfer tclvl False [] named_taus wanted

       ; let (ex_vars, prov_dicts) = tcCollectEx lpat'
             univ_tvs   = filter (not . (`elemVarSet` ex_vars)) qtvs
             ex_tvs     = varSetElems ex_vars
             prov_theta = map evVarPred prov_dicts
             req_theta  = map evVarPred req_dicts

       ; traceTc "tcInferPatSynDecl }" $ ppr name
       ; tc_patsyn_finish lname dir False {- no sig -} is_infix lpat'
                          (univ_tvs, req_theta,  ev_binds, req_dicts)
                          (ex_tvs,   mkTyVarTys ex_tvs, prov_theta, map EvId prov_dicts)
                          (map nlHsVar args, map idType args)
                          pat_ty rec_fields }


tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (LHsBinds Id, TcGblEnv)
tcCheckPatSynDecl PSB{ psb_id = lname@(L _ name), psb_args = details
                     , psb_def = lpat, psb_dir = dir }
                  TPSI{ patsig_univ_tvs = univ_tvs, patsig_prov = prov_theta
                      , patsig_ex_tvs   = ex_tvs,   patsig_req  = req_theta
                      , patsig_arg_tys  = arg_tys,  patsig_body_ty = pat_ty }
  = addPatSynCtxt lname $
    do { let origin     = PatOrigin -- TODO
             skol_info  = SigSkol (PatSynCtxt name) (mkFunTys arg_tys pat_ty)
             decl_arity = length arg_names
             ty_arity   = length arg_tys
             (arg_names, rec_fields, is_infix) = collectPatSynArgInfo details

       ; traceTc "tcCheckPatSynDecl" $
         vcat [ ppr univ_tvs, ppr req_theta, ppr ex_tvs
              , ppr prov_theta, ppr arg_tys, ppr pat_ty ]

       ; checkTc (decl_arity == ty_arity)
                 (wrongNumberOfParmsErr name decl_arity ty_arity)

       ; tcCheckPatSynPat lpat

       -- Right!  Let's check the pattern against the signature
       -- See Note [Checking against a pattern signature]
       ; req_dicts <- newEvVars req_theta
       ; (tclvl, wanted, (lpat', (ex_tvs', prov_dicts, args'))) <-
           ASSERT2( equalLength arg_names arg_tys, ppr name $$ ppr arg_names $$ ppr arg_tys )
           pushLevelAndCaptureConstraints $
           tcPat PatSyn lpat pat_ty $
           do { (subst, ex_tvs') <- if   isUnidirectional dir
                                    then newMetaTyVars    ex_tvs
                                    else newMetaSigTyVars ex_tvs
                    -- See the "Existential type variables part of
                    -- Note [Checking against a pattern signature]
              ; prov_dicts <- mapM (emitWanted origin) (substTheta subst prov_theta)
              ; args'      <- zipWithM (tc_arg subst) arg_names arg_tys
              ; return (ex_tvs', prov_dicts, args') }

       ; (implics, ev_binds) <- buildImplicationFor tclvl skol_info univ_tvs req_dicts wanted

       -- Solve the constraints now, because we are about to make a PatSyn,
       -- which should not contain unification variables and the like (Trac #10997)
       -- Since all the inputs are implications the returned bindings will be empty
       ; _ <- simplifyTop (emptyWC `addImplics` implics)

       -- ToDo: in the bidirectional case, check that the ex_tvs' are all distinct
       -- Otherwise we may get a type error when typechecking the builder,
       -- when that should be impossible

       ; traceTc "tcCheckPatSynDecl }" $ ppr name
       ; tc_patsyn_finish lname dir True {- has a sig -} is_infix lpat'
                          (univ_tvs, req_theta, ev_binds, req_dicts)
                          (ex_tvs, mkTyVarTys ex_tvs', prov_theta, prov_dicts)
                          (args', arg_tys)
                          pat_ty rec_fields }
  where
    tc_arg :: TCvSubst -> Name -> Type -> TcM (LHsExpr TcId)
    tc_arg subst arg_name arg_ty
      = do {   -- Look up the variable actually bound by lpat
               -- and check that it has the expected type
             arg_id <- tcLookupId arg_name
           ; coi <- unifyType (Just arg_id)
                              (idType arg_id)
                              (substTy subst arg_ty)
           ; return (mkLHsWrapCo coi $ nlHsVar arg_id) }

{- Note [Checking against a pattern signature]
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
make available to matches against P, is derivable from the
acutal pattern.  For example:
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

This "concealing" story works for /uni-directional/ pattern synonmys,
but obviously not for bidirectional ones.  So in the bidirectional case
we use SigTv, rather than a generic TauTv, meta-tyvar so that.  And
we should really check that those SigTvs don't get unified with each
other.

-}

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

addPatSynCtxt :: Located Name -> TcM a -> TcM a
addPatSynCtxt (L loc name) thing_inside
  = setSrcSpan loc $
    addErrCtxt (ptext (sLit "In the declaration for pattern synonym")
                <+> quotes (ppr name)) $
    thing_inside

wrongNumberOfParmsErr :: Name -> Arity -> Arity -> SDoc
wrongNumberOfParmsErr name decl_arity ty_arity
  = hang (ptext (sLit "Patten synonym") <+> quotes (ppr name) <+> ptext (sLit "has")
          <+> speakNOf decl_arity (ptext (sLit "argument")))
       2 (ptext (sLit "but its type signature has") <+> speakN ty_arity)

-------------------------
-- Shared by both tcInferPatSyn and tcCheckPatSyn
tc_patsyn_finish :: Located Name  -- ^ PatSyn Name
                 -> HsPatSynDir Name  -- ^ PatSyn type (Uni/Bidir/ExplicitBidir)
                 -> Bool              -- ^ True <=> signature provided
                 -> Bool              -- ^ Whether infix
                 -> LPat Id           -- ^ Pattern of the PatSyn
                 -> ([TcTyVar], [PredType], TcEvBinds, [EvVar])
                 -> ([TcTyVar], [TcType], [PredType], [EvTerm])
                 -> ([LHsExpr TcId], [TcType])   -- ^ Pattern arguments and types
                 -> TcType              -- ^ Pattern type
                 -> [Name]              -- ^ Selector names
                 -- ^ Whether fields, empty if not record PatSyn
                 -> TcM (LHsBinds Id, TcGblEnv)
tc_patsyn_finish lname dir has_sig is_infix lpat'
                 (univ_tvs, req_theta, req_ev_binds, req_dicts)
                 (ex_tvs, ex_tys, prov_theta, prov_dicts)
                 (args, arg_tys)
                 pat_ty field_labels
  = do { -- Zonk everything.  We are about to build a final PatSyn
         -- so there had better be no unification variables in there
         univ_tvs     <- mapMaybeM zonkQuantifiedTyVar univ_tvs
       ; ex_tvs       <- mapMaybeM zonkQuantifiedTyVar ex_tvs
       ; prov_theta   <- zonkTcTypes prov_theta
       ; req_theta    <- zonkTcTypes req_theta
       ; pat_ty       <- zonkTcType pat_ty
       ; arg_tys      <- zonkTcTypes arg_tys
       ; let qtvs    = univ_tvs ++ ex_tvs
             -- See Note [Record PatSyn Desugaring]
             theta   = prov_theta ++ req_theta

       ;

        traceTc "tc_patsyn_finish {" $
           ppr (unLoc lname) $$ ppr (unLoc lpat') $$
           ppr (univ_tvs, req_theta, req_ev_binds, req_dicts) $$
           ppr (ex_tvs, prov_theta, prov_dicts) $$
           ppr args $$
           ppr arg_tys $$
           ppr pat_ty

       -- Make the 'matcher'
       ; (matcher_id, matcher_bind) <- tcPatSynMatcher has_sig lname lpat'
                                         (univ_tvs, req_theta, req_ev_binds, req_dicts)
                                         (ex_tvs, ex_tys, prov_theta, prov_dicts)
                                         (args, arg_tys)
                                         pat_ty


       -- Make the 'builder'
       ; builder_id <- mkPatSynBuilderId has_sig dir lname qtvs theta
                                         arg_tys pat_ty

         -- TODO: Make this have the proper information
       ; let mkFieldLabel name = FieldLabel (occNameFS (nameOccName name)) False name
             field_labels' = (map mkFieldLabel field_labels)


       -- Make the PatSyn itself
       ; let patSyn = mkPatSyn (unLoc lname) is_infix
                        (univ_tvs, req_theta)
                        (ex_tvs, prov_theta)
                        arg_tys
                        pat_ty
                        matcher_id builder_id
                        field_labels'

       -- Selectors
       ; let (sigs, selector_binds) =
                unzip (mkPatSynRecSelBinds patSyn (patSynFieldLabels patSyn))
       ; let tything = AConLike (PatSynCon patSyn)
       ; tcg_env <-
          tcExtendGlobalEnv [tything] $
            tcRecSelBinds
              (ValBindsOut (zip (repeat NonRecursive) selector_binds) sigs)

       ; traceTc "tc_patsyn_finish }" empty
       ; return (matcher_bind, tcg_env) }

{-
************************************************************************
*                                                                      *
         Constructing the "matcher" Id and its binding
*                                                                      *
************************************************************************
-}

tcPatSynMatcher :: Bool  -- True <=> signature provided
                -> Located Name
                -> LPat Id
                -> ([TcTyVar], ThetaType, TcEvBinds, [EvVar])
                -> ([TcTyVar], [TcType], ThetaType, [EvTerm])
                -> ([LHsExpr TcId], [TcType])
                -> TcType
                -> TcM ((Id, Bool), LHsBinds Id)
-- See Note [Matchers and builders for pattern synonyms] in PatSyn
tcPatSynMatcher has_sig (L loc name) lpat
                (univ_tvs, req_theta, req_ev_binds, req_dicts)
                (ex_tvs, ex_tys, prov_theta, prov_dicts)
                (args, arg_tys) pat_ty
  = do { lev_uniq <- newUnique
       ; tv_uniq  <- newUnique
       ; let lev_name = mkInternalName lev_uniq (mkTyVarOcc "rlev") loc
             tv_name  = mkInternalName tv_uniq (mkTyVarOcc "r") loc
             lev_tv   = mkTcTyVar lev_name levityTy   (SkolemTv False)
             lev      = mkTyVarTy lev_tv
             res_tv   = mkTcTyVar tv_name  (tYPE lev) (SkolemTv False)
             is_unlifted = null args && null prov_dicts
             res_ty = mkTyVarTy res_tv
             (cont_args, cont_arg_tys)
               | is_unlifted = ([nlHsVar voidPrimId], [voidPrimTy])
               | otherwise   = (args,                 arg_tys)
             mk_sigma = if has_sig then mkSpecSigmaTy else mkInvSigmaTy
             cont_ty = mk_sigma ex_tvs prov_theta $
                       mkFunTys cont_arg_tys res_ty

             fail_ty  = mkFunTy voidPrimTy res_ty

       ; matcher_name <- newImplicitBinder name mkMatcherOcc
       ; scrutinee    <- newSysLocalId (fsLit "scrut") pat_ty
       ; cont         <- newSysLocalId (fsLit "cont")  cont_ty
       ; fail         <- newSysLocalId (fsLit "fail")  fail_ty

       ; let matcher_tau   = mkFunTys [pat_ty, cont_ty, fail_ty] res_ty
             matcher_sigma = mkInvSigmaTy (lev_tv:res_tv:univ_tvs) req_theta matcher_tau
             matcher_id    = mkExportedLocalId PatSynId matcher_name matcher_sigma
                             -- See Note [Exported LocalIds] in Id

             inst_wrap = mkWpEvApps prov_dicts <.> mkWpTyApps ex_tys
             cont' = foldl nlHsApp (mkLHsWrap inst_wrap (nlHsVar cont)) cont_args

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
                    MG{ mg_alts = L (getLoc lpat) cases
                      , mg_arg_tys = [pat_ty]
                      , mg_res_ty = res_ty
                      , mg_origin = Generated
                      }
             body' = noLoc $
                     HsLam $
                     MG{ mg_alts = noLoc [mkSimpleMatch args body]
                       , mg_arg_tys = [pat_ty, cont_ty, res_ty]
                       , mg_res_ty = res_ty
                       , mg_origin = Generated
                       }
             match = mkMatch [] (mkHsLams (lev_tv:res_tv:univ_tvs) req_dicts body')
                             (noLoc EmptyLocalBinds)
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
                    -> [FieldLabel]
                    -- ^ Visible field labels
                    -> [(LSig Name, LHsBinds Name)]
mkPatSynRecSelBinds ps fields = map mkRecSel fields
  where
    mkRecSel fld_lbl =
      case mkOneRecordSelector [PatSynCon ps] (RecSelPatSyn ps) fld_lbl of
        (name, (_rec_flag, binds)) -> (name, binds)

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

mkPatSynBuilderId :: Bool  -- True <=> signature provided
                  -> HsPatSynDir a -> Located Name
                  -> [TyVar] -> ThetaType -> [Type] -> Type
                  -> TcM (Maybe (Id, Bool))
mkPatSynBuilderId has_sig dir  (L _ name) qtvs theta arg_tys pat_ty
  | isUnidirectional dir
  = return Nothing
  | otherwise
  = do { builder_name <- newImplicitBinder name mkBuilderOcc
       ; let mk_sigma      = if has_sig then mkSpecSigmaTy else mkInvSigmaTy
             builder_sigma = add_void $
                             mk_sigma qtvs theta (mkFunTys arg_tys pat_ty)
             builder_id    =
              -- See Note [Exported LocalIds] in Id
              mkExportedLocalId VanillaId builder_name builder_sigma
       ; return (Just (builder_id, need_dummy_arg)) }
  where
    add_void | need_dummy_arg = mkFunTy voidPrimTy
             | otherwise      = id
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
       ; traceTc "tcPatSynBuilderBind {" $ ppr patsyn
       ; let Just (builder_id, need_dummy_arg) = patSynBuilder patsyn
                   -- Bidirectional, so patSynBuilder returns Just

             match_group' | need_dummy_arg = add_dummy_arg match_group
                          | otherwise      = match_group

             bind = FunBind { fun_id      = L loc (idName builder_id)
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
               builder_args  = [L loc (VarPat (L loc n)) | L loc n <- args]
               builder_match = mkMatch builder_args body (noLoc EmptyLocalBinds)

    args = case details of
              PrefixPatSyn args     -> args
              InfixPatSyn arg1 arg2 -> [arg1, arg2]
              RecordPatSyn args     -> map recordPatSynPatVar args

    add_dummy_arg :: MatchGroup Name (LHsExpr Name)
                  -> MatchGroup Name (LHsExpr Name)
    add_dummy_arg mg@(MG { mg_alts
                            = L l [L loc (Match NonFunBindMatch [] ty grhss)] })
      = mg { mg_alts
                = L l [L loc (Match NonFunBindMatch [nlWildPatName] ty grhss)] }
    add_dummy_arg other_mg = pprPanic "add_dummy_arg" $
                             pprMatches (PatSyn :: HsMatchContext Name) other_mg

tcPatSynBuilderOcc :: PatSyn -> TcM (HsExpr TcId, TcSigmaType)
-- monadic only for failure
tcPatSynBuilderOcc ps
  | Just (builder_id, add_void_arg) <- builder
  , let builder_expr = HsVar (noLoc builder_id)
        builder_ty   = idType builder_id
  = return $
    if add_void_arg
    then ( HsApp (noLoc $ builder_expr) (nlHsVar voidPrimId)
         , tcFunResultTy builder_ty )
    else (builder_expr, builder_ty)

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
             foldl (\x y -> HsApp (L loc x) y) (HsVar (L loc con)) exprs }

    go (L _ (SigPatIn pat _)) = go pat
        -- See Note [Type signatures and the builder expression]

    go (L loc p) = fmap (L loc) $ go1 p

    go1 :: Pat Name -> Maybe (HsExpr Name)
    go1   (VarPat (L l var))
      | var `elemNameSet` lhsVars     = return $ HsVar (L l var)
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
tcCollectEx :: LPat Id -> (TyVarSet, [EvVar])
tcCollectEx pat = go pat
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
