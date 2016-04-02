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
import TcHsType( tcImplicitTKBndrs, tcExplicitTKBndrs
               , tcHsContext, tcHsLiftedType, tcHsOpenType )
import TcRnMonad
import TcEnv
import TcMType
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
import VarEnv( emptyTidyEnv )
import Type( tidyTyCoVarBndrs, tidyTypes, tidyType )
import Id
import IdInfo( RecSelParent(..))
import TcBinds
import BasicTypes
import TcSimplify
import TcUnify
import TcType
import Type
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
import Control.Monad ( unless, zipWithM )
import Data.List( partition )
import Pair( Pair(..) )
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid( mconcat, mappend, mempty )
import Data.Traversable( mapM )
import Prelude hiding ( mapM )
#endif

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
          - req
          - body_ty
     the existentials are the rest

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
              tcExplicitTKBndrs univ_hs_tvs  $ \ univ_tvs ->
              tcExplicitTKBndrs ex_hs_tvs    $ \ ex_tvs   ->
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

       -- Kind generalisation; c.f. kindGeneralise
       ; free_kvs <- zonkTcTypeAndFV $
                     mkSpecForAllTys (implicit_tvs ++ univ_tvs) $
                     mkFunTys req $
                     mkSpecForAllTys ex_tvs $
                     mkFunTys prov $
                     mkFunTys arg_tys $
                     body_ty

       ; kvs <- quantifyZonkedTyVars emptyVarSet (Pair free_kvs emptyVarSet)

       -- These are /signatures/ so we zonk to squeeze out any kind
       -- unification variables.  Do this after quantifyTyVars which may
       -- default kind variables to *.
       -- ToDo: checkValidType?
       ; traceTc "about zonk" empty
       ; implicit_tvs <- mapM zonkTcTyCoVarBndr implicit_tvs
       ; univ_tvs     <- mapM zonkTcTyCoVarBndr univ_tvs
       ; ex_tvs       <- mapM zonkTcTyCoVarBndr ex_tvs
       ; req          <- zonkTcTypes req
       ; prov         <- zonkTcTypes prov
       ; arg_tys      <- zonkTcTypes arg_tys
       ; body_ty      <- zonkTcType  body_ty

       -- Complain about:  pattern P :: () => forall x. x -> P x
       -- The renamer thought it was fine, but the existential 'x'
       -- should not appear in the result type
       ; let bad_tvs = filter (`elemVarSet` tyCoVarsOfType body_ty) ex_tvs
       ; unless (null bad_tvs) $ addErr $
         hang (text "The result type" <+> quotes (ppr body_ty))
            2 (text "mentions existential type variable" <> plural bad_tvs
               <+> pprQuotedList bad_tvs)

         -- Split [Splitting the implicit tyvars in a pattern synonym]
       ; let univ_fvs = closeOverKinds $
                        (tyCoVarsOfTypes (body_ty : req) `extendVarSetList` univ_tvs)
             (extra_univ, extra_ex) = partition ((`elemVarSet` univ_fvs) .
                                                 binderVar "tcPatSynSig") $
                                      mkNamedBinders Invisible kvs ++
                                      mkNamedBinders Specified implicit_tvs
       ; traceTc "tcTySig }" $
         vcat [ text "implicit_tvs" <+> ppr_tvs implicit_tvs
              , text "kvs" <+> ppr_tvs kvs
              , text "extra_univ" <+> ppr extra_univ
              , text "univ_tvs" <+> ppr_tvs univ_tvs
              , text "req" <+> ppr req
              , text "extra_ex" <+> ppr extra_ex
              , text "ex_tvs" <+> ppr_tvs ex_tvs
              , text "prov" <+> ppr prov
              , text "arg_tys" <+> ppr arg_tys
              , text "body_ty" <+> ppr body_ty ]
       ; return (TPSI { patsig_name = name
                      , patsig_univ_bndrs = extra_univ ++
                                            mkNamedBinders Specified univ_tvs
                      , patsig_req        = req
                      , patsig_ex_bndrs   = extra_ex   ++
                                            mkNamedBinders Specified ex_tvs
                      , patsig_prov       = prov
                      , patsig_arg_tys    = arg_tys
                      , patsig_body_ty    = body_ty }) }
  where

ppr_tvs :: [TyVar] -> SDoc
ppr_tvs tvs = braces (vcat [ ppr tv <+> dcolon <+> ppr (tyVarKind tv)
                           | tv <- tvs])


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
       ; (tclvl, wanted, ((lpat', args), pat_ty))
            <- pushLevelAndCaptureConstraints  $
               do { pat_ty <- newOpenInferExpType
                  ; stuff <- tcPat PatSyn lpat pat_ty $
                             mapM tcLookupId arg_names
                  ; pat_ty <- readExpType pat_ty
                  ; return (stuff, pat_ty) }

       ; let named_taus = (name, pat_ty) : map (\arg -> (getName arg, varType arg)) args

       ; (qtvs, req_dicts, ev_binds) <- simplifyInfer tclvl False [] named_taus wanted

       ; let (ex_vars, prov_dicts) = tcCollectEx lpat'
             univ_tvs   = filter (not . (`elemVarSet` ex_vars)) qtvs
             ex_tvs     = varSetElems ex_vars
             prov_theta = map evVarPred prov_dicts
             req_theta  = map evVarPred req_dicts

       ; traceTc "tcInferPatSynDecl }" $ ppr name
       ; tc_patsyn_finish lname dir is_infix lpat'
                          (univ_tvs, mkNamedBinders Invisible univ_tvs
                            , req_theta,  ev_binds, req_dicts)
                          (ex_tvs,   mkNamedBinders Invisible ex_tvs
                            , mkTyVarTys ex_tvs, prov_theta, map EvId prov_dicts)
                          (map nlHsVar args, map idType args)
                          pat_ty rec_fields }


tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (LHsBinds Id, TcGblEnv)
tcCheckPatSynDecl psb@PSB{ psb_id = lname@(L _ name), psb_args = details
                         , psb_def = lpat, psb_dir = dir }
                  TPSI{ patsig_univ_bndrs = univ_bndrs, patsig_prov = prov_theta
                      , patsig_ex_bndrs   = ex_bndrs,   patsig_req  = req_theta
                      , patsig_arg_tys    = arg_tys,    patsig_body_ty = pat_ty }
  = addPatSynCtxt lname $
    do { let origin     = ProvCtxtOrigin psb
             skol_info  = PatSynSigSkol name
             decl_arity = length arg_names
             ty_arity   = length arg_tys
             (arg_names, rec_fields, is_infix) = collectPatSynArgInfo details

             univ_tvs   = map (binderVar "tcCheckPatSynDecl 1") univ_bndrs
             ex_tvs     = map (binderVar "tcCheckPatSynDecl 2") ex_bndrs

       ; traceTc "tcCheckPatSynDecl" $
         vcat [ ppr univ_bndrs, ppr req_theta, ppr ex_bndrs
              , ppr prov_theta, ppr arg_tys, ppr pat_ty ]

       ; checkTc (decl_arity == ty_arity)
                 (wrongNumberOfParmsErr name decl_arity ty_arity)

       ; tcCheckPatSynPat lpat

       -- Right!  Let's check the pattern against the signature
       -- See Note [Checking against a pattern signature]
       ; req_dicts <- newEvVars req_theta
       ; (tclvl, wanted, (lpat', (ex_tvs', prov_dicts, args'))) <-
           ASSERT2( equalLength arg_names arg_tys, ppr name $$ ppr arg_names $$ ppr arg_tys )
           pushLevelAndCaptureConstraints            $
           tcExtendTyVarEnv univ_tvs                 $
           tcPat PatSyn lpat (mkCheckExpType pat_ty) $
           do { (subst, ex_tvs') <- if   isUnidirectional dir
                                    then newMetaTyVars    ex_tvs
                                    else newMetaSigTyVars ex_tvs
                    -- See the "Existential type variables" part of
                    -- Note [Checking against a pattern signature]
              ; traceTc "tcpatsyn1" (vcat [ ppr v <+> dcolon <+> ppr (tyVarKind v) | v <- ex_tvs])
              ; traceTc "tcpatsyn2" (vcat [ ppr v <+> dcolon <+> ppr (tyVarKind v) | v <- ex_tvs'])
              ; prov_dicts <- mapM (emitWanted origin)
                  (substTheta (extendTCvInScopeList subst univ_tvs) prov_theta)
                  -- Add the free vars of 'prov_theta' to the in_scope set to
                  -- satisfy the substition invariant. There's no need to
                  -- add 'ex_tvs' as they are already in the domain of the
                  -- substitution.
                  -- See also Note [The substitution invariant] in TyCoRep.
              ; args'      <- zipWithM (tc_arg subst) arg_names arg_tys
              ; return (ex_tvs', prov_dicts, args') }

       ; (implics, ev_binds) <- buildImplicationFor tclvl skol_info univ_tvs req_dicts wanted

       -- Solve the constraints now, because we are about to make a PatSyn,
       -- which should not contain unification variables and the like (Trac #10997)
       -- Since all the inputs are implications the returned bindings will be empty
       ; _ <- simplifyTop (mkImplicWC implics)

       -- ToDo: in the bidirectional case, check that the ex_tvs' are all distinct
       -- Otherwise we may get a type error when typechecking the builder,
       -- when that should be impossible

       ; traceTc "tcCheckPatSynDecl }" $ ppr name
       ; tc_patsyn_finish lname dir is_infix lpat'
                          (univ_tvs, univ_bndrs, req_theta, ev_binds, req_dicts)
                          (ex_tvs, ex_bndrs, mkTyVarTys ex_tvs', prov_theta, prov_dicts)
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
                              (substTyUnchecked subst arg_ty)
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
    addErrCtxt (text "In the declaration for pattern synonym"
                <+> quotes (ppr name)) $
    thing_inside

wrongNumberOfParmsErr :: Name -> Arity -> Arity -> SDoc
wrongNumberOfParmsErr name decl_arity ty_arity
  = hang (text "Pattern synonym" <+> quotes (ppr name) <+> ptext (sLit "has")
          <+> speakNOf decl_arity (text "argument"))
       2 (text "but its type signature has" <+> speakN ty_arity)

-------------------------
-- Shared by both tcInferPatSyn and tcCheckPatSyn
tc_patsyn_finish :: Located Name  -- ^ PatSyn Name
                 -> HsPatSynDir Name  -- ^ PatSyn type (Uni/Bidir/ExplicitBidir)
                 -> Bool              -- ^ Whether infix
                 -> LPat Id           -- ^ Pattern of the PatSyn
                 -> ([TcTyVar], [TcTyBinder], [PredType], TcEvBinds, [EvVar])
                 -> ([TcTyVar], [TcTyBinder], [TcType], [PredType], [EvTerm])
                 -> ([LHsExpr TcId], [TcType])   -- ^ Pattern arguments and types
                 -> TcType              -- ^ Pattern type
                 -> [Name]              -- ^ Selector names
                 -- ^ Whether fields, empty if not record PatSyn
                 -> TcM (LHsBinds Id, TcGblEnv)
tc_patsyn_finish lname dir is_infix lpat'
                 (univ_tvs, univ_bndrs, req_theta, req_ev_binds, req_dicts)
                 (ex_tvs, ex_bndrs, ex_tys, prov_theta, prov_dicts)
                 (args, arg_tys)
                 pat_ty field_labels
  = do { -- Zonk everything.  We are about to build a final PatSyn
         -- so there had better be no unification variables in there
         univ_tvs'    <- mapMaybeM zonkQuantifiedTyVar univ_tvs
       ; ex_tvs'      <- mapMaybeM zonkQuantifiedTyVar ex_tvs
       ; prov_theta'  <- zonkTcTypes prov_theta
       ; req_theta'   <- zonkTcTypes req_theta
       ; pat_ty'      <- zonkTcType pat_ty
       ; arg_tys'     <- zonkTcTypes arg_tys

       ; let (env1, univ_tvs) = tidyTyCoVarBndrs emptyTidyEnv univ_tvs'
             (env2, ex_tvs)   = tidyTyCoVarBndrs env1 ex_tvs'
             req_theta  = tidyTypes env2 req_theta'
             prov_theta = tidyTypes env2 prov_theta'
             arg_tys    = tidyTypes env2 arg_tys'
             pat_ty     = tidyType  env2 pat_ty'

          -- We need to update the univ and ex binders after zonking.
          -- But zonking may have defaulted some erstwhile binders,
          -- so we need to make sure the tyvars and tybinders remain
          -- lined up
       ; let update_binders :: [TyVar] -> [TcTyBinder] -> [TyBinder]
             update_binders [] _ = []
             update_binders all_tvs@(tv:tvs) (bndr:bndrs)
               | tv == bndr_var
               = mkNamedBinder (binderVisibility bndr) tv : update_binders tvs bndrs
               | otherwise
               = update_binders all_tvs bndrs
               where
                 bndr_var = binderVar "tc_patsyn_finish" bndr
             update_binders tvs _ = pprPanic "tc_patsyn_finish" (ppr lname $$ ppr tvs)

             univ_bndrs' = update_binders univ_tvs univ_bndrs
             ex_bndrs'   = update_binders ex_tvs   ex_bndrs

       ; traceTc "tc_patsyn_finish {" $
           ppr (unLoc lname) $$ ppr (unLoc lpat') $$
           ppr (univ_tvs, univ_bndrs', req_theta, req_ev_binds, req_dicts) $$
           ppr (ex_tvs, ex_bndrs', prov_theta, prov_dicts) $$
           ppr args $$
           ppr arg_tys $$
           ppr pat_ty

       -- Make the 'matcher'
       ; (matcher_id, matcher_bind) <- tcPatSynMatcher lname lpat'
                                         (univ_tvs, req_theta, req_ev_binds, req_dicts)
                                         (ex_tvs, ex_tys, prov_theta, prov_dicts)
                                         (args, arg_tys)
                                         pat_ty


       -- Make the 'builder'
       ; builder_id <- mkPatSynBuilderId dir lname
                                         univ_bndrs' req_theta
                                         ex_bndrs'   prov_theta
                                         arg_tys pat_ty

         -- TODO: Make this have the proper information
       ; let mkFieldLabel name = FieldLabel (occNameFS (nameOccName name)) False name
             field_labels' = (map mkFieldLabel field_labels)


       -- Make the PatSyn itself
       ; let patSyn = mkPatSyn (unLoc lname) is_infix
                        (univ_tvs, univ_bndrs', req_theta)
                        (ex_tvs, ex_bndrs', prov_theta)
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

tcPatSynMatcher :: Located Name
                -> LPat Id
                -> ([TcTyVar], ThetaType, TcEvBinds, [EvVar])
                -> ([TcTyVar], [TcType], ThetaType, [EvTerm])
                -> ([LHsExpr TcId], [TcType])
                -> TcType
                -> TcM ((Id, Bool), LHsBinds Id)
-- See Note [Matchers and builders for pattern synonyms] in PatSyn
tcPatSynMatcher (L loc name) lpat
                (univ_tvs, req_theta, req_ev_binds, req_dicts)
                (ex_tvs, ex_tys, prov_theta, prov_dicts)
                (args, arg_tys) pat_ty
  = do { rr_uniq <- newUnique
       ; tv_uniq <- newUnique
       ; let rr_name  = mkInternalName rr_uniq (mkTyVarOcc "rep") loc
             tv_name  = mkInternalName tv_uniq (mkTyVarOcc "r") loc
             rr_tv    = mkTcTyVar rr_name runtimeRepTy (SkolemTv False)
             rr       = mkTyVarTy rr_tv
             res_tv   = mkTcTyVar tv_name  (tYPE rr) (SkolemTv False)
             is_unlifted = null args && null prov_dicts
             res_ty = mkTyVarTy res_tv
             (cont_args, cont_arg_tys)
               | is_unlifted = ([nlHsVar voidPrimId], [voidPrimTy])
               | otherwise   = (args,                 arg_tys)
             cont_ty = mkInvSigmaTy ex_tvs prov_theta $
                       mkFunTys cont_arg_tys res_ty

             fail_ty  = mkFunTy voidPrimTy res_ty

       ; matcher_name <- newImplicitBinder name mkMatcherOcc
       ; scrutinee    <- newSysLocalId (fsLit "scrut") pat_ty
       ; cont         <- newSysLocalId (fsLit "cont")  cont_ty
       ; fail         <- newSysLocalId (fsLit "fail")  fail_ty

       ; let matcher_tau   = mkFunTys [pat_ty, cont_ty, fail_ty] res_ty
             matcher_sigma = mkInvSigmaTy (rr_tv:res_tv:univ_tvs) req_theta matcher_tau
             matcher_id    = mkExportedVanillaId matcher_name matcher_sigma
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
             match = mkMatch [] (mkHsLams (rr_tv:res_tv:univ_tvs) req_dicts body')
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

mkPatSynBuilderId :: HsPatSynDir a -> Located Name
                  -> [TyBinder] -> ThetaType
                  -> [TyBinder] -> ThetaType
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

       ; return (Just (builder_id, need_dummy_arg)) }
  where

tcPatSynBuilderBind :: TcSigFun
                    -> PatSynBind Name Name
                    -> TcM (LHsBinds Id)
-- See Note [Matchers and builders for pattern synonyms] in PatSyn
tcPatSynBuilderBind sig_fun PSB{ psb_id = L loc name, psb_def = lpat
                               , psb_dir = dir, psb_args = details }
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

       ; sig <- get_builder_sig sig_fun name builder_id need_dummy_arg

       ; (builder_binds, _) <- tcPolyCheck NonRecursive emptyPragEnv sig (noLoc bind)
       ; traceTc "tcPatSynBuilderBind }" $ ppr builder_binds
       ; return builder_binds }

  | otherwise = panic "tcPatSynBuilderBind"  -- Both cases dealt with
  where
    mb_match_group
       = case dir of
           ExplicitBidirectional explicit_mg -> Right explicit_mg
           ImplicitBidirectional             -> fmap mk_mg (tcPatToExpr args lpat)
           Unidirectional -> panic "tcPatSynBuilderBind"

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
    add_dummy_arg mg@(MG { mg_alts = L l [L loc match@(Match { m_pats = pats })] })
      = mg { mg_alts = L l [L loc (match { m_pats = nlWildPatName : pats })] }
    add_dummy_arg other_mg = pprPanic "add_dummy_arg" $
                             pprMatches (PatSyn :: HsMatchContext Name) other_mg

get_builder_sig :: TcSigFun -> Name -> Id -> Bool -> TcM TcIdSigInfo
get_builder_sig sig_fun name builder_id need_dummy_arg
  | Just (TcPatSynSig sig) <- sig_fun name
  , TPSI { patsig_univ_bndrs = univ_bndrs
         , patsig_req        = req
         , patsig_ex_bndrs   = ex_bndrs
         , patsig_prov       = prov
         , patsig_arg_tys    = arg_tys
         , patsig_body_ty    = body_ty } <- sig
  = -- Constuct a TcIdSigInfo from a TcPatSynInfo
    -- This does unfortunately mean that we have to know how to
    -- make the builder Id's type from the TcPatSynInfo, which
    -- duplicates the construction in mkPatSynBuilderId
    -- But we really want to use the scoped type variables from
    -- the actual sigature, so this is really the Right Thing
    return (TISI { sig_bndr  = CompleteSig builder_id
                 , sig_skols = [ (tyVarName tv, tv)
                               | bndr <- univ_bndrs ++ ex_bndrs
                               , let tv = binderVar "get_builder_sig" bndr ]
                 , sig_theta = req ++ prov
                 , sig_tau   = add_void need_dummy_arg $
                               mkFunTys arg_tys body_ty
                 , sig_ctxt  = PatSynBuilderCtxt name
                 , sig_loc   = getSrcSpan name })
  | otherwise
  = -- No signature, so fake up a TcIdSigInfo from the builder Id
    instTcTySigFromId builder_id
    -- See Note [Redundant constraints for builder]

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

add_void :: Bool -> Type -> Type
add_void need_dummy_arg ty
  | need_dummy_arg = mkFunTy voidPrimTy ty
  | otherwise      = ty

tcPatToExpr :: [Located Name] -> LPat Name -> Either MsgDoc (LHsExpr Name)
-- Given a /pattern/, return an /expression/ that builds a value
-- that matches the pattern.  E.g. if the pattern is (Just [x]),
-- the expression is (Just [x]).  They look the same, but the
-- input uses constructors from HsPat and the output uses constructors
-- from HsExpr.
--
-- Returns (Left r) if the pattern is not invertible, for reason r.
-- See Note [Builder for a bidirectional pattern synonym]
tcPatToExpr args pat = go pat
  where
    lhsVars = mkNameSet (map unLoc args)

    -- Make a prefix con for prefix and infix patterns for simplicity
    mkPrefixConExpr :: Located Name -> [LPat Name] -> Either MsgDoc (HsExpr Name)
    mkPrefixConExpr lcon@(L loc _) pats
      = do { exprs <- mapM go pats
           ; return (foldl (\x y -> HsApp (L loc x) y)
                           (HsVar lcon) exprs) }

    mkRecordConExpr :: Located Name -> HsRecFields Name (LPat Name)
                    -> Either MsgDoc (HsExpr Name)
    mkRecordConExpr con fields
      = do { exprFields <- mapM go fields
           ; return (RecordCon con PlaceHolder noPostTcExpr exprFields) }

    go :: LPat Name -> Either MsgDoc (LHsExpr Name)
    go (L loc p) = L loc <$> go1 p

    go1 :: Pat Name -> Either MsgDoc (HsExpr Name)
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
    go1 (LazyPat pat)               = go1 (unLoc pat)
    go1 (BangPat pat)               = go1 (unLoc pat)
    go1 (PArrPat pats ptt)          = do { exprs <- mapM go pats
                                         ; return $ ExplicitPArr ptt exprs }
    go1 (ListPat pats ptt reb)      = do { exprs <- mapM go pats
                                         ; return $ ExplicitList ptt (fmap snd reb) exprs }
    go1 (TuplePat pats box _)       = do { exprs <- mapM go pats
                                         ; return $ ExplicitTuple
                                              (map (noLoc . Present) exprs) box }
    go1 (LitPat lit)                = return $ HsLit lit
    go1 (NPat (L _ n) mb_neg _ _)
        | Just neg <- mb_neg        = return $ unLoc $ nlHsSyntaxApps neg [noLoc (HsOverLit n)]
        | otherwise                 = return $ HsOverLit n
    go1 (ConPatOut{})               = panic "ConPatOut in output of renamer"
    go1 (SigPatOut{})               = panic "SigPatOut in output of renamer"
    go1 (CoPat{})                   = panic "CoPat in output of renamer"
    go1 p = Left (text "pattern" <+> quotes (ppr p) <+> text "is not invertible")

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
    hang (text "Pattern synonym definition cannot contain as-patterns (@):")
       2 (ppr pat)

thInPatSynErr :: OutputableBndr name => Pat name -> TcM a
thInPatSynErr pat
  = failWithTc $
    hang (text "Pattern synonym definition cannot contain Template Haskell:")
       2 (ppr pat)

nPlusKPatInPatSynErr :: OutputableBndr name => Pat name -> TcM a
nPlusKPatInPatSynErr pat
  = failWithTc $
    hang (text "Pattern synonym definition cannot contain n+k-pattern:")
       2 (ppr pat)

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
    go1 (NPlusKPat n k _ geq subtract _)
      = pprPanic "TODO: NPlusKPat" $ ppr n $$ ppr k $$ ppr geq $$ ppr subtract
    go1 _                   = mempty

    goConDetails :: HsConPatDetails Id -> (TyVarSet, [EvVar])
    goConDetails (PrefixCon ps) = mconcat . map go $ ps
    goConDetails (InfixCon p1 p2) = go p1 `mappend` go p2
    goConDetails (RecCon HsRecFields{ rec_flds = flds })
      = mconcat . map goRecFd $ flds

    goRecFd :: LHsRecField Id (LPat Id) -> (TyVarSet, [EvVar])
    goRecFd (L _ HsRecField{ hsRecFieldArg = p }) = go p
