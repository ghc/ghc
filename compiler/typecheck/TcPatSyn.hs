{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[TcPatSyn]{Typechecking pattern synonym declarations}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module TcPatSyn ( tcPatSynDecl, tcPatSynBuilderBind
                , tcPatSynBuilderOcc, nonBidirectionalErr
  ) where

import GhcPrelude

import GHC.Hs
import TcPat
import Type( tidyTyCoVarBinders, tidyTypes, tidyType )
import TcRnMonad
import TcSigs( emptyPragEnv, completeSigFromId )
import TcEnv
import TcMType
import TcHsSyn
import TysPrim
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
import Predicate
import TysWiredIn
import TcType
import TcEvidence
import TcOrigin
import BuildTyCl
import VarSet
import MkId
import TcTyDecls
import ConLike
import FieldLabel
import Bag
import Util
import ErrUtils
import Data.Maybe( mapMaybe )
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

tcPatSynDecl :: PatSynBind GhcRn GhcRn
             -> Maybe TcSigInfo
             -> TcM (LHsBinds GhcTc, TcGblEnv)
tcPatSynDecl psb mb_sig
  = recoverM (recoverPSB psb) $
    case mb_sig of
      Nothing                 -> tcInferPatSynDecl psb
      Just (TcPatSynSig tpsi) -> tcCheckPatSynDecl psb tpsi
      _                       -> panic "tcPatSynDecl"

recoverPSB :: PatSynBind GhcRn GhcRn
           -> TcM (LHsBinds GhcTc, TcGblEnv)
-- See Note [Pattern synonym error recovery]
recoverPSB (PSB { psb_id = L _ name
                , psb_args = details })
 = do { matcher_name <- newImplicitBinder name mkMatcherOcc
      ; let placeholder = AConLike $ PatSynCon $
                          mk_placeholder matcher_name
      ; gbl_env <- tcExtendGlobalEnv [placeholder] getGblEnv
      ; return (emptyBag, gbl_env) }
  where
    (_arg_names, _rec_fields, is_infix) = collectPatSynArgInfo details
    mk_placeholder matcher_name
      = mkPatSyn name is_infix
                        ([mkTyVarBinder Specified alphaTyVar], []) ([], [])
                        [] -- Arg tys
                        alphaTy
                        (matcher_id, True) Nothing
                        []  -- Field labels
       where
         -- The matcher_id is used only by the desugarer, so actually
         -- and error-thunk would probably do just as well here.
         matcher_id = mkLocalId matcher_name $
                      mkSpecForAllTys [alphaTyVar] alphaTy

recoverPSB (XPatSynBind nec) = noExtCon nec

{- Note [Pattern synonym error recovery]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If type inference for a pattern synonym fails, we can't continue with
the rest of tc_patsyn_finish, because we may get knock-on errors, or
even a crash.  E.g. from
   pattern What = True :: Maybe
we get a kind error; and we must stop right away (#15289).

We stop if there are /any/ unsolved constraints, not just insoluble
ones; because pattern synonyms are top-level things, we will never
solve them later if we can't solve them now.  And if we were to carry
on, tc_patsyn_finish does zonkTcTypeToType, which defaults any
unsolved unificatdion variables to Any, which confuses the error
reporting no end (#15685).

So we use simplifyTop to completely solve the constraint, report
any errors, throw an exception.

Even in the event of such an error we can recover and carry on, just
as we do for value bindings, provided we plug in placeholder for the
pattern synonym: see recoverPSB.  The goal of the placeholder is not
to cause a raft of follow-on errors.  I've used the simplest thing for
now, but we might need to elaborate it a bit later.  (e.g.  I've given
it zero args, which may cause knock-on errors if it is used in a
pattern.) But it'll do for now.

-}

tcInferPatSynDecl :: PatSynBind GhcRn GhcRn
                  -> TcM (LHsBinds GhcTc, TcGblEnv)
tcInferPatSynDecl (PSB { psb_id = lname@(L _ name), psb_args = details
                       , psb_def = lpat, psb_dir = dir })
  = addPatSynCtxt lname $
    do { traceTc "tcInferPatSynDecl {" $ ppr name

       ; let (arg_names, rec_fields, is_infix) = collectPatSynArgInfo details
       ; (tclvl, wanted, ((lpat', args), pat_ty))
            <- pushLevelAndCaptureConstraints  $
               tcInferNoInst                   $ \ exp_ty ->
               tcPat PatSyn lpat exp_ty        $
               mapM tcLookupId arg_names

       ; let (ex_tvs, prov_dicts) = tcCollectEx lpat'

             named_taus = (name, pat_ty) : map mk_named_tau args
             mk_named_tau arg
               = (getName arg, mkSpecForAllTys ex_tvs (varType arg))
               -- The mkSpecForAllTys is important (#14552), albeit
               -- slightly artificial (there is no variable with this funny type).
               -- We do not want to quantify over variable (alpha::k)
               -- that mention the existentially-bound type variables
               -- ex_tvs in its kind k.
               -- See Note [Type variables whose kind is captured]

       ; (univ_tvs, req_dicts, ev_binds, residual, _)
               <- simplifyInfer tclvl NoRestrictions [] named_taus wanted
       ; top_ev_binds <- checkNoErrs (simplifyTop residual)
       ; addTopEvBinds top_ev_binds $

    do { prov_dicts <- mapM zonkId prov_dicts
       ; let filtered_prov_dicts = mkMinimalBySCs evVarPred prov_dicts
             -- Filtering: see Note [Remove redundant provided dicts]
             (prov_theta, prov_evs)
                 = unzip (mapMaybe mkProvEvidence filtered_prov_dicts)
             req_theta = map evVarPred req_dicts

       -- Report coercions that escape
       -- See Note [Coercions that escape]
       ; args <- mapM zonkId args
       ; let bad_args = [ (arg, bad_cos) | arg <- args ++ prov_dicts
                              , let bad_cos = filterDVarSet isId $
                                              (tyCoVarsOfTypeDSet (idType arg))
                              , not (isEmptyDVarSet bad_cos) ]
       ; mapM_ dependentArgErr bad_args

       ; traceTc "tcInferPatSynDecl }" $ (ppr name $$ ppr ex_tvs)
       ; tc_patsyn_finish lname dir is_infix lpat'
                          (mkTyVarBinders Inferred univ_tvs
                            , req_theta,  ev_binds, req_dicts)
                          (mkTyVarBinders Inferred ex_tvs
                            , mkTyVarTys ex_tvs, prov_theta, prov_evs)
                          (map nlHsVar args, map idType args)
                          pat_ty rec_fields } }
tcInferPatSynDecl (XPatSynBind nec) = noExtCon nec

mkProvEvidence :: EvId -> Maybe (PredType, EvTerm)
-- See Note [Equality evidence in pattern synonyms]
mkProvEvidence ev_id
  | EqPred r ty1 ty2 <- classifyPredType pred
  , let k1 = tcTypeKind ty1
        k2 = tcTypeKind ty2
        is_homo = k1 `tcEqType` k2
        homo_tys   = [k1, ty1, ty2]
        hetero_tys = [k1, k2, ty1, ty2]
  = case r of
      ReprEq | is_homo
             -> Just ( mkClassPred coercibleClass    homo_tys
                     , evDataConApp coercibleDataCon homo_tys eq_con_args )
             | otherwise -> Nothing
      NomEq  | is_homo
             -> Just ( mkClassPred eqClass    homo_tys
                     , evDataConApp eqDataCon homo_tys eq_con_args )
             | otherwise
             -> Just ( mkClassPred heqClass    hetero_tys
                     , evDataConApp heqDataCon hetero_tys eq_con_args )

  | otherwise
  = Just (pred, EvExpr (evId ev_id))
  where
    pred = evVarPred ev_id
    eq_con_args = [evId ev_id]

dependentArgErr :: (Id, DTyCoVarSet) -> TcM ()
-- See Note [Coercions that escape]
dependentArgErr (arg, bad_cos)
  = addErrTc $
    vcat [ text "Iceland Jack!  Iceland Jack! Stop torturing me!"
         , hang (text "Pattern-bound variable")
              2 (ppr arg <+> dcolon <+> ppr (idType arg))
         , nest 2 $
           hang (text "has a type that mentions pattern-bound coercion"
                 <> plural bad_co_list <> colon)
              2 (pprWithCommas ppr bad_co_list)
         , text "Hint: use -fprint-explicit-coercions to see the coercions"
         , text "Probable fix: add a pattern signature" ]
  where
    bad_co_list = dVarSetElems bad_cos

{- Note [Type variables whose kind is captured]
~~-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data AST a = Sym [a]
  class Prj s where { prj :: [a] -> Maybe (s a) }
  pattern P x <= Sym (prj -> Just x)

Here we get a matcher with this type
  $mP :: forall s a. Prj s => AST a -> (s a -> r) -> r -> r

No problem.  But note that 's' is not fixed by the type of the
pattern (AST a), nor is it existentially bound.  It's really only
fixed by the type of the continuation.

#14552 showed that this can go wrong if the kind of 's' mentions
existentially bound variables.  We obviously can't make a type like
  $mP :: forall (s::k->*) a. Prj s => AST a -> (forall k. s a -> r)
                                   -> r -> r
But neither is 's' itself existentially bound, so the forall (s::k->*)
can't go in the inner forall either.  (What would the matcher apply
the continuation to?)

Solution: do not quantiify over any unification variable whose kind
mentions the existentials.  We can conveniently do that by making the
"taus" passed to simplifyInfer look like
   forall ex_tvs. arg_ty

After that, Note [Naughty quantification candidates] in TcMType takes
over and errors.

Note [Remove redundant provided dicts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Recall that
   HRefl :: forall k1 k2 (a1:k1) (a2:k2). (k1 ~ k2, a1 ~ a2)
                                       => a1 :~~: a2
(NB: technically the (k1~k2) existential dictionary is not necessary,
but it's there at the moment.)

Now consider (#14394):
   pattern Foo = HRefl
in a non-poly-kinded module.  We don't want to get
    pattern Foo :: () => (* ~ *, b ~ a) => a :~~: b
with that redundant (* ~ *).  We'd like to remove it; hence the call to
mkMinimalWithSCs.

Similarly consider
  data S a where { MkS :: Ord a => a -> S a }
  pattern Bam x y <- (MkS (x::a), MkS (y::a)))

The pattern (Bam x y) binds two (Ord a) dictionaries, but we only
need one.  Again mkMimimalWithSCs removes the redundant one.

Note [Equality evidence in pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data X a where
     MkX :: Eq a => [a] -> X (Maybe a)
  pattern P x = MkG x

Then there is a danger that GHC will infer
  P :: forall a.  () =>
       forall b. (a ~# Maybe b, Eq b) => [b] -> X a

The 'builder' for P, which is called in user-code, will then
have type
  $bP :: forall a b. (a ~# Maybe b, Eq b) => [b] -> X a

and that is bad because (a ~# Maybe b) is not a predicate type
(see Note [Types for coercions, predicates, and evidence] in TyCoRep
and is not implicitly instantiated.

So in mkProvEvidence we lift (a ~# b) to (a ~ b).  Tiresome, and
marginally less efficient, if the builder/martcher are not inlined.

See also Note [Lift equality constraints when quantifying] in TcType

Note [Coercions that escape]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#14507 showed an example where the inferred type of the matcher
for the pattern synonym was something like
   $mSO :: forall (r :: TYPE rep) kk (a :: k).
           TypeRep k a
           -> ((Bool ~ k) => TypeRep Bool (a |> co_a2sv) -> r)
           -> (Void# -> r)
           -> r

What is that co_a2sv :: Bool ~# *??  It was bound (via a superclass
selection) by the pattern being matched; and indeed it is implicit in
the context (Bool ~ k).  You could imagine trying to extract it like
this:
   $mSO :: forall (r :: TYPE rep) kk (a :: k).
           TypeRep k a
           -> ( co :: ((Bool :: *) ~ (k :: *)) =>
                  let co_a2sv = sc_sel co
                  in TypeRep Bool (a |> co_a2sv) -> r)
           -> (Void# -> r)
           -> r

But we simply don't allow that in types.  Maybe one day but not now.

How to detect this situation?  We just look for free coercion variables
in the types of any of the arguments to the matcher.  The error message
is not very helpful, but at least we don't get a Lint error.
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

         -- See Note [The pattern-synonym signature splitting rule] in TcSigs
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
                  -- See also Note [The substitution invariant] in TyCoSubst.
              ; prov_dicts <- mapM (emitWanted (ProvCtxtOrigin psb)) prov_theta'
              ; args'      <- zipWithM (tc_arg subst) arg_names arg_tys
              ; return (ex_tvs', prov_dicts, args') }

       ; let skol_info = SigSkol (PatSynCtxt name) pat_ty []
                         -- The type here is a bit bogus, but we do not print
                         -- the type for PatSynCtxt, so it doesn't matter
                         -- See Note [Skolem info for pattern synonyms] in Origin
       ; (implics, ev_binds) <- buildImplicationFor tclvl skol_info univ_tvs req_dicts wanted

       -- Solve the constraints now, because we are about to make a PatSyn,
       -- which should not contain unification variables and the like (#10997)
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
tcCheckPatSynDecl (XPatSynBind nec) _ = noExtCon nec

{- [Pattern synonyms and higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data T = MkT (forall a. a->a)

  pattern P :: (Int -> Int) -> T
  pattern P x <- MkT x

This should work.  But in the matcher we must match against MkT, and then
instantiate its argument 'x', to get a function of type (Int -> Int).
Equality is not enough!  #13752 was an example.


Note [The pattern-synonym signature splitting rule]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a pattern signature, we must split
     the kind-generalised variables, and
     the implicitly-bound variables
into universal and existential.  The rule is this
(see discussion on #11224):

     The universal tyvars are the ones mentioned in
          - univ_tvs: the user-specified (forall'd) universals
          - req_theta
          - res_ty
     The existential tyvars are all the rest

For example

   pattern P :: () => b -> T a
   pattern P x = ...

Here 'a' is universal, and 'b' is existential.  But there is a wrinkle:
how do we split the arg_tys from req_ty?  Consider

   pattern Q :: () => b -> S c -> T a
   pattern Q x = ...

This is an odd example because Q has only one syntactic argument, and
so presumably is defined by a view pattern matching a function.  But
it can happen (#11977, #12108).

We don't know Q's arity from the pattern signature, so we have to wait
until we see the pattern declaration itself before deciding res_ty is,
and hence which variables are existential and which are universal.

And that in turn is why TcPatSynInfo has a separate field,
patsig_implicit_bndrs, to capture the implicitly bound type variables,
because we don't yet know how to split them up.

It's a slight compromise, because it means we don't really know the
pattern synonym's real signature until we see its declaration.  So,
for example, in hs-boot file, we may need to think what to do...
(eg don't have any implicitly-bound variables).


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

  At one point, for ImplicitBidirectional I used TyVarTvs (instead of
  TauTvs) in tcCheckPatSynDecl.  But (a) strengthening the check here
  is redundant since tcPatSynBuilderBind does the job, (b) it was
  still incomplete (TyVarTvs can unify with each other), and (c) it
  didn't even work (#13441 was accepted with
  ExplicitBidirectional, but rejected if expressed in
  ImplicitBidirectional form.  Conclusion: trying to be too clever is
  a bad idea.
-}

collectPatSynArgInfo :: HsPatSynDetails (Located Name)
                     -> ([Name], [Name], Bool)
collectPatSynArgInfo details =
  case details of
    PrefixCon names      -> (map unLoc names, [], False)
    InfixCon name1 name2 -> (map unLoc [name1, name2], [], True)
    RecCon names         -> (vars, sels, False)
                         where
                            (vars, sels) = unzip (map splitRecordPatSyn names)
  where
    splitRecordPatSyn :: RecordPatSynField (Located Name)
                      -> (Name, Name)
    splitRecordPatSyn (RecordPatSynField
                       { recordPatSynPatVar     = L _ patVar
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

         (ze, univ_tvs') <- zonkTyVarBinders univ_tvs
       ; req_theta'      <- zonkTcTypesToTypesX ze req_theta
       ; (ze, ex_tvs')   <- zonkTyVarBindersX ze ex_tvs
       ; prov_theta'     <- zonkTcTypesToTypesX ze prov_theta
       ; pat_ty'         <- zonkTcTypeToTypeX ze pat_ty
       ; arg_tys'        <- zonkTcTypesToTypesX ze arg_tys

       ; let (env1, univ_tvs) = tidyTyCoVarBinders emptyTidyEnv univ_tvs'
             (env2, ex_tvs)   = tidyTyCoVarBinders env1 ex_tvs'
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
       ; let rr_tv  = mkTyVar rr_name runtimeRepTy
             rr     = mkTyVarTy rr_tv
             res_tv = mkTyVar tv_name (tYPE rr)
             res_ty = mkTyVarTy res_tv
             is_unlifted = null args && null prov_dicts
             (cont_args, cont_arg_tys)
               | is_unlifted = ([nlHsVar voidPrimId], [voidPrimTy])
               | otherwise   = (args,                 arg_tys)
             cont_ty = mkInfSigmaTy ex_tvs prov_theta $
                       mkVisFunTys cont_arg_tys res_ty

             fail_ty  = mkVisFunTy voidPrimTy res_ty

       ; matcher_name <- newImplicitBinder name mkMatcherOcc
       ; scrutinee    <- newSysLocalId (fsLit "scrut") pat_ty
       ; cont         <- newSysLocalId (fsLit "cont")  cont_ty
       ; fail         <- newSysLocalId (fsLit "fail")  fail_ty

       ; let matcher_tau   = mkVisFunTys [pat_ty, cont_ty, fail_ty] res_ty
             matcher_sigma = mkInfSigmaTy (rr_tv:res_tv:univ_tvs) req_theta matcher_tau
             matcher_id    = mkExportedVanillaId matcher_name matcher_sigma
                             -- See Note [Exported LocalIds] in Id

             inst_wrap = mkWpEvApps prov_dicts <.> mkWpTyApps ex_tys
             cont' = foldl' nlHsApp (mkLHsWrap inst_wrap (nlHsVar cont)) cont_args

             fail' = nlHsApps fail [nlHsVar voidPrimId]

             args = map nlVarPat [scrutinee, cont, fail]
             lwpat = noLoc $ WildPat pat_ty
             cases = if isIrrefutableHsPat lpat
                     then [mkHsCaseAlt lpat  cont']
                     else [mkHsCaseAlt lpat  cont',
                           mkHsCaseAlt lwpat fail']
             body = mkLHsWrap (mkWpLet req_ev_binds) $
                    L (getLoc lpat) $
                    HsCase noExtField (nlHsVar scrutinee) $
                    MG{ mg_alts = L (getLoc lpat) cases
                      , mg_ext = MatchGroupTc [pat_ty] res_ty
                      , mg_origin = Generated
                      }
             body' = noLoc $
                     HsLam noExtField $
                     MG{ mg_alts = noLoc [mkSimpleMatch LambdaExpr
                                                        args body]
                       , mg_ext = MatchGroupTc [pat_ty, cont_ty, fail_ty] res_ty
                       , mg_origin = Generated
                       }
             match = mkMatch (mkPrefixFunRhs (L loc name)) []
                             (mkHsLams (rr_tv:res_tv:univ_tvs)
                                       req_dicts body')
                             (noLoc (EmptyLocalBinds noExtField))
             mg :: MatchGroup GhcTc (LHsExpr GhcTc)
             mg = MG{ mg_alts = L (getLoc match) [match]
                    , mg_ext = MatchGroupTc [] res_ty
                    , mg_origin = Generated
                    }

       ; let bind = FunBind{ fun_id = L loc matcher_id
                           , fun_matches = mg
                           , fun_ext = idHsWrapper
                           , fun_tick = [] }
             matcher_bind = unitBag (noLoc bind)

       ; traceTc "tcPatSynMatcher" (ppr name $$ ppr (idType matcher_id))
       ; traceTc "tcPatSynMatcher" (ppr matcher_bind)

       ; return ((matcher_id, is_unlifted), matcher_bind) }

mkPatSynRecSelBinds :: PatSyn
                    -> [FieldLabel]  -- ^ Visible field labels
                    -> [(Id, LHsBind GhcRn)]
mkPatSynRecSelBinds ps fields
  = [ mkOneRecordSelector [PatSynCon ps] (RecSelPatSyn ps) fld_lbl
    | fld_lbl <- fields ]

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
                              mkPhiTy theta $
                              mkVisFunTys arg_tys $
                              pat_ty
             builder_id     = mkExportedVanillaId builder_name builder_sigma
              -- See Note [Exported LocalIds] in Id

             builder_id'    = modifyIdInfo (`setLevityInfoWithType` pat_ty) builder_id

       ; return (Just (builder_id', need_dummy_arg)) }
  where

tcPatSynBuilderBind :: PatSynBind GhcRn GhcRn
                    -> TcM (LHsBinds GhcTc)
-- See Note [Matchers and builders for pattern synonyms] in PatSyn
tcPatSynBuilderBind (PSB { psb_id = L loc name
                         , psb_def = lpat
                         , psb_dir = dir
                         , psb_args = details })
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
       ; case patSynBuilder patsyn of {
           Nothing -> return emptyBag ;
             -- This case happens if we found a type error in the
             -- pattern synonym, recovered, and put a placeholder
             -- with patSynBuilder=Nothing in the environment

           Just (builder_id, need_dummy_arg) ->  -- Normal case
    do { -- Bidirectional, so patSynBuilder returns Just
         let match_group' | need_dummy_arg = add_dummy_arg match_group
                          | otherwise      = match_group

             bind = FunBind { fun_id      = L loc (idName builder_id)
                            , fun_matches = match_group'
                            , fun_ext     = emptyNameSet
                            , fun_tick    = [] }

             sig = completeSigFromId (PatSynCtxt name) builder_id

       ; traceTc "tcPatSynBuilderBind {" $
         ppr patsyn $$ ppr builder_id <+> dcolon <+> ppr (idType builder_id)
       ; (builder_binds, _) <- tcPolyCheck emptyPragEnv sig (noLoc bind)
       ; traceTc "tcPatSynBuilderBind }" $ ppr builder_binds
       ; return builder_binds } } }

#if __GLASGOW_HASKELL__ <= 810
  | otherwise = panic "tcPatSynBuilderBind"  -- Both cases dealt with
#endif
  where
    mb_match_group
       = case dir of
           ExplicitBidirectional explicit_mg -> Right explicit_mg
           ImplicitBidirectional -> fmap mk_mg (tcPatToExpr name args lpat)
           Unidirectional -> panic "tcPatSynBuilderBind"

    mk_mg :: LHsExpr GhcRn -> MatchGroup GhcRn (LHsExpr GhcRn)
    mk_mg body = mkMatchGroup Generated [builder_match]
          where
            builder_args  = [L loc (VarPat noExtField (L loc n))
                            | L loc n <- args]
            builder_match = mkMatch (mkPrefixFunRhs (L loc name))
                                    builder_args body
                                    (noLoc (EmptyLocalBinds noExtField))

    args = case details of
              PrefixCon args     -> args
              InfixCon arg1 arg2 -> [arg1, arg2]
              RecCon args        -> map recordPatSynPatVar args

    add_dummy_arg :: MatchGroup GhcRn (LHsExpr GhcRn)
                  -> MatchGroup GhcRn (LHsExpr GhcRn)
    add_dummy_arg mg@(MG { mg_alts =
                           (L l [L loc match@(Match { m_pats = pats })]) })
      = mg { mg_alts = L l [L loc (match { m_pats = nlWildPatName : pats })] }
    add_dummy_arg other_mg = pprPanic "add_dummy_arg" $
                             pprMatches other_mg
tcPatSynBuilderBind (XPatSynBind nec) = noExtCon nec

tcPatSynBuilderOcc :: PatSyn -> TcM (HsExpr GhcTcId, TcSigmaType)
-- monadic only for failure
tcPatSynBuilderOcc ps
  | Just (builder_id, add_void_arg) <- builder
  , let builder_expr = HsConLikeOut noExtField (PatSynCon ps)
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
  | need_dummy_arg = mkVisFunTy voidPrimTy ty
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
           ; return (foldl' (\x y -> HsApp noExtField (L loc x) y)
                            (HsVar noExtField lcon) exprs) }

    mkRecordConExpr :: Located Name -> HsRecFields GhcRn (LPat GhcRn)
                    -> Either MsgDoc (HsExpr GhcRn)
    mkRecordConExpr con fields
      = do { exprFields <- mapM go fields
           ; return (RecordCon noExtField con exprFields) }

    go :: LPat GhcRn -> Either MsgDoc (LHsExpr GhcRn)
    go (L loc p) = L loc <$> go1 p

    go1 :: Pat GhcRn -> Either MsgDoc (HsExpr GhcRn)
    go1 (ConPat con info NoExtField)
      = case info of
          PrefixCon ps  -> mkPrefixConExpr con ps
          InfixCon l r  -> mkPrefixConExpr con [l,r]
          RecCon fields -> mkRecordConExpr con fields

    go1 (SigPat _ pat _) = go1 (unLoc pat)
        -- See Note [Type signatures and the builder expression]

    go1 (VarPat _ (L l var))
        | var `elemNameSet` lhsVars
        = return $ HsVar noExtField (L l var)
        | otherwise
        = Left (quotes (ppr var) <+> text "is not bound by the LHS of the pattern synonym")
    go1 (ParPat _ pat)          = fmap (HsPar noExtField) $ go pat
    go1 p@(ListPat reb pats)
      | Nothing <- reb = do { exprs <- mapM go pats
                            ; return $ ExplicitList noExtField Nothing exprs }
      | otherwise                   = notInvertibleListPat p
    go1 (TuplePat _ pats box)       = do { exprs <- mapM go pats
                                         ; return $ ExplicitTuple noExtField
                                           (map (noLoc . (Present noExtField)) exprs)
                                                                           box }
    go1 (SumPat _ pat alt arity)    = do { expr <- go1 (unLoc pat)
                                         ; return $ ExplicitSum noExtField alt arity
                                                                   (noLoc expr)
                                         }
    go1 (LitPat _ lit)              = return $ HsLit noExtField lit
    go1 (NPat _ (L _ n) mb_neg _)
        | Just (SyntaxExprRn neg) <- mb_neg
                                    = return $ unLoc $ foldl' nlHsApp (noLoc neg)
                                                       [noLoc (HsOverLit noExtField n)]
        | otherwise                 = return $ HsOverLit noExtField n
    go1 (SplicePat _ (HsSpliced _ _ (HsSplicedPat pat)))
                                    = go1 pat
    go1 (SplicePat _ (HsSpliced{})) = panic "Invalid splice variety"
    go1 (SplicePat _ (HsSplicedT{})) = panic "Invalid splice variety"

    -- The following patterns are not invertible.
    go1 p@(BangPat {})                       = notInvertible p -- #14112
    go1 p@(LazyPat {})                       = notInvertible p
    go1 p@(WildPat {})                       = notInvertible p
    go1 p@(AsPat {})                         = notInvertible p
    go1 p@(ViewPat {})                       = notInvertible p
    go1 p@(NPlusKPat {})                     = notInvertible p
    go1 p@(XPat {})                          = notInvertible p
    go1 p@(SplicePat _ (HsTypedSplice {}))   = notInvertible p
    go1 p@(SplicePat _ (HsUntypedSplice {})) = notInvertible p
    go1 p@(SplicePat _ (HsQuasiQuote {}))    = notInvertible p
    go1 p@(SplicePat _ (XSplice {}))         = notInvertible p

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
    -- refactoring; see #14380.  Until then we reject with a
    -- helpful error message.
    notInvertibleListPat p
      = Left (vcat [ not_invertible_msg p
                   , text "Reason: rebindable syntax is on."
                   , text "This is fixable: add use-case to #14380" ])

{- Note [Builder for a bidirectional pattern synonym]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a bidirectional pattern synonym we need to produce an /expression/
that matches the supplied /pattern/, given values for the arguments
of the pattern synonym.  For example
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
The builder can have redundant constraints, which are awkward to eliminate.
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
'x', say.  Nevertheless, the signature may be useful to constrain
the type.

When making the binding for the *builder*, though, we don't want
  $buildL x = Left x :: Either [a] [b]
because that wil either mean (forall a b. Either [a] [b]), or we'll
get a complaint that 'a' and 'b' are out of scope. (Actually the
latter; #9867.)  No, the job of the signature is done, so when
converting the pattern to an expression (for the builder RHS) we
simply discard the signature.

Note [Record PatSyn Desugaring]
-------------------------------
It is important that prov_theta comes before req_theta as this ordering is used
when desugaring record pattern synonym updates.

Any change to this ordering should make sure to change deSugar/DsExpr.hs if you
want to avoid difficult to decipher core lint errors!
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
    go1 (LazyPat _ p)      = go p
    go1 (AsPat _ _ p)      = go p
    go1 (ParPat _ p)       = go p
    go1 (BangPat _ p)      = go p
    go1 (ListPat _ ps)     = mergeMany . map go $ ps
    go1 (TuplePat _ ps _)  = mergeMany . map go $ ps
    go1 (SumPat _ p _ _)   = go p
    go1 (ViewPat _ _ p)    = go p
    go1 con@ConPat{ pat_con_ext = con' }
                           = merge (pat_tvs con', pat_dicts con') $
                              goConDetails $ pat_args con
    go1 (SigPat _ p _)     = go p
    go1 (XPat (CoPat _ p _)) = go1 p
    go1 (NPlusKPat _ n k _ geq subtract)
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
