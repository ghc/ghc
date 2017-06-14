{-
(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-2002

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module TcSigs(
       TcSigInfo(..),
       TcIdSigInfo(..), TcIdSigInst,
       TcPatSynInfo(..),
       TcSigFun,

       isPartialSig, hasCompleteSig, tcIdSigName, tcSigInfoName,
       completeSigPolyId_maybe,

       tcTySigs, tcUserTypeSig, completeSigFromId,
       tcInstSig,

       TcPragEnv, emptyPragEnv, lookupPragEnv, extendPragEnv,
       mkPragEnv, tcSpecPrags, tcSpecWrapper, tcImpPrags, addInlinePrags
   ) where

#include "HsVersions.h"

import HsSyn
import TcHsType
import TcRnTypes
import TcRnMonad
import TcType
import TcMType
import TcValidity ( checkValidType )
import TcUnify( tcSkolemise, unifyType )
import Inst( topInstantiate )
import TcEnv( tcLookupId )
import TcEvidence( HsWrapper, (<.>) )
import Type( mkTyVarBinders )

import DynFlags
import Var      ( TyVar, tyVarName, tyVarKind )
import Id       ( Id, idName, idType, idInlinePragma, setInlinePragma, mkLocalId )
import PrelNames( mkUnboundName )
import BasicTypes
import Bag( foldrBag )
import Module( getModule )
import Name
import NameEnv
import VarSet
import Outputable
import SrcLoc
import Util( singleton )
import Maybes( orElse )
import Data.Maybe( mapMaybe )
import Control.Monad( unless )


{- -------------------------------------------------------------
          Note [Overview of type signatures]
----------------------------------------------------------------
Type signatures, including partial signatures, are jolly tricky,
especially on value bindings.  Here's an overview.

    f :: forall a. [a] -> [a]
    g :: forall b. _ -> b

    f = ...g...
    g = ...f...

* HsSyn: a signature in a binding starts of as a TypeSig, in
  type HsBinds.Sig

* When starting a mutually recursive group, like f/g above, we
  call tcTySig on each signature in the group.

* tcTySig: Sig -> TcIdSigInfo
  - For a /complete/ signature, like 'f' above, tcTySig kind-checks
    the HsType, producing a Type, and wraps it in a CompleteSig, and
    extend the type environment with this polymorphic 'f'.

  - For a /partial/signature, like 'g' above, tcTySig does nothing
    Instead it just wraps the pieces in a PartialSig, to be handled
    later.

* tcInstSig: TcIdSigInfo -> TcIdSigInst
  In tcMonoBinds, when looking at an individual binding, we use
  tcInstSig to instantiate the signature forall's in the signature,
  and attribute that instantiated (monomorphic) type to the
  binder.  You can see this in TcBinds.tcLhsId.

  The instantiation does the obvious thing for complete signatures,
  but for /partial/ signatures it starts from the HsSyn, so it
  has to kind-check it etc: tcHsPartialSigType.  It's convenient
  to do this at the same time as instantiation, because we can
  make the wildcards into unification variables right away, raather
  than somehow quantifying over them.  And the "TcLevel" of those
  unification variables is correct because we are in tcMonoBinds.


Note [Scoped tyvars]
~~~~~~~~~~~~~~~~~~~~
The -XScopedTypeVariables flag brings lexically-scoped type variables
into scope for any explicitly forall-quantified type variables:
        f :: forall a. a -> a
        f x = e
Then 'a' is in scope inside 'e'.

However, we do *not* support this
  - For pattern bindings e.g
        f :: forall a. a->a
        (f,g) = e

Note [Binding scoped type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type variables *brought into lexical scope* by a type signature
may be a subset of the *quantified type variables* of the signatures,
for two reasons:

* With kind polymorphism a signature like
    f :: forall f a. f a -> f a
  may actually give rise to
    f :: forall k. forall (f::k -> *) (a:k). f a -> f a
  So the sig_tvs will be [k,f,a], but only f,a are scoped.
  NB: the scoped ones are not necessarily the *inital* ones!

* Even aside from kind polymorphism, there may be more instantiated
  type variables than lexically-scoped ones.  For example:
        type T a = forall b. b -> (a,b)
        f :: forall c. T c
  Here, the signature for f will have one scoped type variable, c,
  but two instantiated type variables, c' and b'.

However, all of this only applies to the renamer.  The typechecker
just puts all of them into the type environment; any lexical-scope
errors were dealt with by the renamer.

-}


{- *********************************************************************
*                                                                      *
             Utility functions for TcSigInfo
*                                                                      *
********************************************************************* -}

tcIdSigName :: TcIdSigInfo -> Name
tcIdSigName (CompleteSig { sig_bndr = id }) = idName id
tcIdSigName (PartialSig { psig_name = n })  = n

tcSigInfoName :: TcSigInfo -> Name
tcSigInfoName (TcIdSig     idsi) = tcIdSigName idsi
tcSigInfoName (TcPatSynSig tpsi) = patsig_name tpsi

completeSigPolyId_maybe :: TcSigInfo -> Maybe TcId
completeSigPolyId_maybe sig
  | TcIdSig sig_info <- sig
  , CompleteSig { sig_bndr = id } <- sig_info = Just id
  | otherwise                                 = Nothing


{- *********************************************************************
*                                                                      *
               Typechecking user signatures
*                                                                      *
********************************************************************* -}

tcTySigs :: [LSig GhcRn] -> TcM ([TcId], TcSigFun)
tcTySigs hs_sigs
  = checkNoErrs $   -- See Note [Fail eagerly on bad signatures]
    do { ty_sigs_s <- mapAndRecoverM tcTySig hs_sigs
       ; let ty_sigs  = concat ty_sigs_s
             poly_ids = mapMaybe completeSigPolyId_maybe ty_sigs
                        -- The returned [TcId] are the ones for which we have
                        -- a complete type signature.
                        -- See Note [Complete and partial type signatures]
             env = mkNameEnv [(tcSigInfoName sig, sig) | sig <- ty_sigs]
       ; return (poly_ids, lookupNameEnv env) }

tcTySig :: LSig GhcRn -> TcM [TcSigInfo]
tcTySig (L _ (IdSig id))
  = do { let ctxt = FunSigCtxt (idName id) False
                    -- False: do not report redundant constraints
                    -- The user has no control over the signature!
             sig = completeSigFromId ctxt id
       ; return [TcIdSig sig] }

tcTySig (L loc (TypeSig names sig_ty))
  = setSrcSpan loc $
    do { sigs <- sequence [ tcUserTypeSig loc sig_ty (Just name)
                          | L _ name <- names ]
       ; return (map TcIdSig sigs) }

tcTySig (L loc (PatSynSig names sig_ty))
  = setSrcSpan loc $
    do { tpsigs <- sequence [ tcPatSynSig name sig_ty
                            | L _ name <- names ]
       ; return (map TcPatSynSig tpsigs) }

tcTySig _ = return []


tcUserTypeSig :: SrcSpan -> LHsSigWcType GhcRn -> Maybe Name
              -> TcM TcIdSigInfo
-- A function or expression type signature
-- Returns a fully quantified type signature; even the wildcards
-- are quantified with ordinary skolems that should be instantiated
--
-- The SrcSpan is what to declare as the binding site of the
-- any skolems in the signature. For function signatures we
-- use the whole `f :: ty' signature; for expression signatures
-- just the type part.
--
-- Just n  => Function type signature       name :: type
-- Nothing => Expression type signature   <expr> :: type
tcUserTypeSig loc hs_sig_ty mb_name
  | isCompleteHsSig hs_sig_ty
  = do { sigma_ty <- tcHsSigWcType ctxt_F hs_sig_ty
       ; return $
         CompleteSig { sig_bndr  = mkLocalId name sigma_ty
                     , sig_ctxt  = ctxt_T
                     , sig_loc   = loc } }
                       -- Location of the <type> in   f :: <type>

  -- Partial sig with wildcards
  | otherwise
  = return (PartialSig { psig_name = name, psig_hs_ty = hs_sig_ty
                       , sig_ctxt = ctxt_F, sig_loc = loc })
  where
    name   = case mb_name of
               Just n  -> n
               Nothing -> mkUnboundName (mkVarOcc "<expression>")
    ctxt_F = case mb_name of
               Just n  -> FunSigCtxt n False
               Nothing -> ExprSigCtxt
    ctxt_T = case mb_name of
               Just n  -> FunSigCtxt n True
               Nothing -> ExprSigCtxt



completeSigFromId :: UserTypeCtxt -> Id -> TcIdSigInfo
-- Used for instance methods and record selectors
completeSigFromId ctxt id
  = CompleteSig { sig_bndr = id
                , sig_ctxt = ctxt
                , sig_loc  = getSrcSpan id }

isCompleteHsSig :: LHsSigWcType GhcRn -> Bool
-- ^ If there are no wildcards, return a LHsSigType
isCompleteHsSig (HsWC { hswc_wcs = wcs }) = null wcs

{- Note [Fail eagerly on bad signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a type signature is wrong, fail immediately:

 * the type sigs may bind type variables, so proceeding without them
   can lead to a cascade of errors

 * the type signature might be ambiguous, in which case checking
   the code against the signature will give a very similar error
   to the ambiguity error.

ToDo: this means we fall over if any type sig
is wrong (eg at the top level of the module),
which is over-conservative
-}

{- *********************************************************************
*                                                                      *
        Type checking a pattern synonym signature
*                                                                      *
************************************************************************

Note [Pattern synonym signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Pattern synonym signatures are surprisingly tricky (see Trac #11224 for example).
In general they look like this:

   pattern P :: forall univ_tvs. req_theta
             => forall ex_tvs. prov_theta
             => arg1 -> .. -> argn -> res_ty

For parsing and renaming we treat the signature as an ordinary LHsSigType.

Once we get to type checking, we decompose it into its parts, in tcPatSynSig.

* Note that 'forall univ_tvs' and 'req_theta =>'
        and 'forall ex_tvs'   and 'prov_theta =>'
  are all optional.  We gather the pieces at the the top of tcPatSynSig

* Initially the implicitly-bound tyvars (added by the renamer) include both
  universal and existential vars.

* After we kind-check the pieces and convert to Types, we do kind generalisation.

Note [The pattern-synonym signature splitting rule]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a pattern signature, we must split
     the kind-generalised variables, and
     the implicitly-bound variables
into universal and existential.  The rule is this
(see discussion on Trac #11224):

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
it can happen (Trac #11977, #12108).

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
-}

tcPatSynSig :: Name -> LHsSigType GhcRn -> TcM TcPatSynInfo
tcPatSynSig name sig_ty
  | HsIB { hsib_vars = implicit_hs_tvs
         , hsib_body = hs_ty }  <- sig_ty
  , (univ_hs_tvs, hs_req,  hs_ty1)     <- splitLHsSigmaTy hs_ty
  , (ex_hs_tvs,   hs_prov, hs_body_ty) <- splitLHsSigmaTy hs_ty1
  = do { (implicit_tvs, (univ_tvs, req, ex_tvs, prov, body_ty))
           <- solveEqualities $
              tcImplicitTKBndrs implicit_hs_tvs $
              tcExplicitTKBndrs univ_hs_tvs  $ \ univ_tvs ->
              tcExplicitTKBndrs ex_hs_tvs    $ \ ex_tvs   ->
              do { req     <- tcHsContext hs_req
                 ; prov    <- tcHsContext hs_prov
                 ; body_ty <- tcHsOpenType hs_body_ty
                     -- A (literal) pattern can be unlifted;
                     -- e.g. pattern Zero <- 0#   (Trac #12094)
                 ; let bound_tvs
                         = unionVarSets [ allBoundVariabless req
                                        , allBoundVariabless prov
                                        , allBoundVariables body_ty
                                        ]
                 ; return ( (univ_tvs, req, ex_tvs, prov, body_ty)
                          , bound_tvs) }

       -- Kind generalisation
       ; kvs <- kindGeneralize $
                build_patsyn_type [] implicit_tvs univ_tvs req
                                  ex_tvs prov body_ty

       -- These are /signatures/ so we zonk to squeeze out any kind
       -- unification variables.  Do this after quantifyTyVars which may
       -- default kind variables to *.
       ; traceTc "about zonk" empty
       ; implicit_tvs <- mapM zonkTcTyCoVarBndr implicit_tvs
       ; univ_tvs     <- mapM zonkTcTyCoVarBndr univ_tvs
       ; ex_tvs       <- mapM zonkTcTyCoVarBndr ex_tvs
       ; req          <- zonkTcTypes req
       ; prov         <- zonkTcTypes prov
       ; body_ty      <- zonkTcType  body_ty

       -- Now do validity checking
       ; checkValidType ctxt $
         build_patsyn_type kvs implicit_tvs univ_tvs req ex_tvs prov body_ty

       -- arguments become the types of binders. We thus cannot allow
       -- levity polymorphism here
       ; let (arg_tys, _) = tcSplitFunTys body_ty
       ; mapM_ (checkForLevPoly empty) arg_tys

       ; traceTc "tcTySig }" $
         vcat [ text "implicit_tvs" <+> ppr_tvs implicit_tvs
              , text "kvs" <+> ppr_tvs kvs
              , text "univ_tvs" <+> ppr_tvs univ_tvs
              , text "req" <+> ppr req
              , text "ex_tvs" <+> ppr_tvs ex_tvs
              , text "prov" <+> ppr prov
              , text "body_ty" <+> ppr body_ty ]
       ; return (TPSI { patsig_name = name
                      , patsig_implicit_bndrs = mkTyVarBinders Inferred kvs ++
                                                mkTyVarBinders Specified implicit_tvs
                      , patsig_univ_bndrs     = univ_tvs
                      , patsig_req            = req
                      , patsig_ex_bndrs       = ex_tvs
                      , patsig_prov           = prov
                      , patsig_body_ty        = body_ty }) }
  where
    ctxt = PatSynCtxt name

    build_patsyn_type kvs imp univ req ex prov body
      = mkInvForAllTys kvs $
        mkSpecForAllTys (imp ++ univ) $
        mkFunTys req $
        mkSpecForAllTys ex $
        mkFunTys prov $
        body

ppr_tvs :: [TyVar] -> SDoc
ppr_tvs tvs = braces (vcat [ ppr tv <+> dcolon <+> ppr (tyVarKind tv)
                           | tv <- tvs])


{- *********************************************************************
*                                                                      *
               Instantiating user signatures
*                                                                      *
********************************************************************* -}


tcInstSig :: TcIdSigInfo -> TcM TcIdSigInst
-- Instantiate a type signature; only used with plan InferGen
tcInstSig sig@(CompleteSig { sig_bndr = poly_id, sig_loc = loc })
  = setSrcSpan loc $  -- Set the binding site of the tyvars
    do { (tv_prs, theta, tau) <- tcInstType newMetaSigTyVars poly_id
              -- See Note [Pattern bindings and complete signatures]

       ; return (TISI { sig_inst_sig   = sig
                      , sig_inst_skols = tv_prs
                      , sig_inst_wcs   = []
                      , sig_inst_wcx   = Nothing
                      , sig_inst_theta = theta
                      , sig_inst_tau   = tau }) }

tcInstSig sig@(PartialSig { psig_hs_ty = hs_ty
                          , sig_ctxt = ctxt
                          , sig_loc = loc })
  = setSrcSpan loc $  -- Set the binding site of the tyvars
    do { (wcs, wcx, tvs, theta, tau) <- tcHsPartialSigType ctxt hs_ty
       ; return (TISI { sig_inst_sig   = sig
                      , sig_inst_skols = map (\tv -> (tyVarName tv, tv)) tvs
                      , sig_inst_wcs   = wcs
                      , sig_inst_wcx   = wcx
                      , sig_inst_theta = theta
                      , sig_inst_tau   = tau }) }


{- Note [Pattern bindings and complete signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
      data T a = MkT a a
      f :: forall a. a->a
      g :: forall b. b->b
      MkT f g = MkT (\x->x) (\y->y)
Here we'll infer a type from the pattern of 'T a', but if we feed in
the signature types for f and g, we'll end up unifying 'a' and 'b'

So we instantiate f and g's signature with SigTv skolems
(newMetaSigTyVars) that can unify with each other.  If too much
unification takes place, we'll find out when we do the final
impedance-matching check in TcBinds.mkExport

See Note [Signature skolems] in TcType

None of this applies to a function binding with a complete
signature, which doesn't use tcInstSig.  See TcBinds.tcPolyCheck.
-}

{- *********************************************************************
*                                                                      *
                   Pragmas and PragEnv
*                                                                      *
********************************************************************* -}

type TcPragEnv = NameEnv [LSig GhcRn]

emptyPragEnv :: TcPragEnv
emptyPragEnv = emptyNameEnv

lookupPragEnv :: TcPragEnv -> Name -> [LSig GhcRn]
lookupPragEnv prag_fn n = lookupNameEnv prag_fn n `orElse` []

extendPragEnv :: TcPragEnv -> (Name, LSig GhcRn) -> TcPragEnv
extendPragEnv prag_fn (n, sig) = extendNameEnv_Acc (:) singleton prag_fn n sig

---------------
mkPragEnv :: [LSig GhcRn] -> LHsBinds GhcRn -> TcPragEnv
mkPragEnv sigs binds
  = foldl extendPragEnv emptyNameEnv prs
  where
    prs = mapMaybe get_sig sigs

    get_sig :: LSig GhcRn -> Maybe (Name, LSig GhcRn)
    get_sig (L l (SpecSig lnm@(L _ nm) ty inl)) = Just (nm, L l $ SpecSig   lnm ty (add_arity nm inl))
    get_sig (L l (InlineSig lnm@(L _ nm) inl))  = Just (nm, L l $ InlineSig lnm    (add_arity nm inl))
    get_sig (L l (SCCFunSig st lnm@(L _ nm) str))  = Just (nm, L l $ SCCFunSig st lnm str)
    get_sig _                                   = Nothing

    add_arity n inl_prag   -- Adjust inl_sat field to match visible arity of function
      | Inline <- inl_inline inl_prag
        -- add arity only for real INLINE pragmas, not INLINABLE
      = case lookupNameEnv ar_env n of
          Just ar -> inl_prag { inl_sat = Just ar }
          Nothing -> WARN( True, text "mkPragEnv no arity" <+> ppr n )
                     -- There really should be a binding for every INLINE pragma
                     inl_prag
      | otherwise
      = inl_prag

    -- ar_env maps a local to the arity of its definition
    ar_env :: NameEnv Arity
    ar_env = foldrBag lhsBindArity emptyNameEnv binds

lhsBindArity :: LHsBind GhcRn -> NameEnv Arity -> NameEnv Arity
lhsBindArity (L _ (FunBind { fun_id = id, fun_matches = ms })) env
  = extendNameEnv env (unLoc id) (matchGroupArity ms)
lhsBindArity _ env = env        -- PatBind/VarBind


-----------------
addInlinePrags :: TcId -> [LSig GhcRn] -> TcM TcId
addInlinePrags poly_id prags_for_me
  | inl@(L _ prag) : inls <- inl_prags
  = do { traceTc "addInlinePrag" (ppr poly_id $$ ppr prag)
       ; unless (null inls) (warn_multiple_inlines inl inls)
       ; return (poly_id `setInlinePragma` prag) }
  | otherwise
  = return poly_id
  where
    inl_prags = [L loc prag | L loc (InlineSig _ prag) <- prags_for_me]

    warn_multiple_inlines _ [] = return ()

    warn_multiple_inlines inl1@(L loc prag1) (inl2@(L _ prag2) : inls)
       | inlinePragmaActivation prag1 == inlinePragmaActivation prag2
       , isEmptyInlineSpec (inlinePragmaSpec prag1)
       =    -- Tiresome: inl1 is put there by virtue of being in a hs-boot loop
            -- and inl2 is a user NOINLINE pragma; we don't want to complain
         warn_multiple_inlines inl2 inls
       | otherwise
       = setSrcSpan loc $
         addWarnTc NoReason
                     (hang (text "Multiple INLINE pragmas for" <+> ppr poly_id)
                       2 (vcat (text "Ignoring all but the first"
                                : map pp_inl (inl1:inl2:inls))))

    pp_inl (L loc prag) = ppr prag <+> parens (ppr loc)


{- *********************************************************************
*                                                                      *
                   SPECIALISE pragmas
*                                                                      *
************************************************************************

Note [Handling SPECIALISE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea is this:

   foo :: Num a => a -> b -> a
   {-# SPECIALISE foo :: Int -> b -> Int #-}

We check that
   (forall a b. Num a => a -> b -> a)
      is more polymorphic than
   forall b. Int -> b -> Int
(for which we could use tcSubType, but see below), generating a HsWrapper
to connect the two, something like
      wrap = /\b. <hole> Int b dNumInt
This wrapper is put in the TcSpecPrag, in the ABExport record of
the AbsBinds.


        f :: (Eq a, Ix b) => a -> b -> Bool
        {-# SPECIALISE f :: (Ix p, Ix q) => Int -> (p,q) -> Bool #-}
        f = <poly_rhs>

From this the typechecker generates

    AbsBinds [ab] [d1,d2] [([ab], f, f_mono, prags)] binds

    SpecPrag (wrap_fn :: forall a b. (Eq a, Ix b) => XXX
                      -> forall p q. (Ix p, Ix q) => XXX[ Int/a, (p,q)/b ])

From these we generate:

    Rule:       forall p, q, (dp:Ix p), (dq:Ix q).
                    f Int (p,q) dInt ($dfInPair dp dq) = f_spec p q dp dq

    Spec bind:  f_spec = wrap_fn <poly_rhs>

Note that

  * The LHS of the rule may mention dictionary *expressions* (eg
    $dfIxPair dp dq), and that is essential because the dp, dq are
    needed on the RHS.

  * The RHS of f_spec, <poly_rhs> has a *copy* of 'binds', so that it
    can fully specialise it.



From the TcSpecPrag, in DsBinds we generate a binding for f_spec and a RULE:

   f_spec :: Int -> b -> Int
   f_spec = wrap<f rhs>

   RULE: forall b (d:Num b). f b d = f_spec b

The RULE is generated by taking apart the HsWrapper, which is a little
delicate, but works.

Some wrinkles

1. We don't use full-on tcSubType, because that does co and contra
   variance and that in turn will generate too complex a LHS for the
   RULE.  So we use a single invocation of skolemise /
   topInstantiate in tcSpecWrapper.  (Actually I think that even
   the "deeply" stuff may be too much, because it introduces lambdas,
   though I think it can be made to work without too much trouble.)

2. We need to take care with type families (Trac #5821).  Consider
      type instance F Int = Bool
      f :: Num a => a -> F a
      {-# SPECIALISE foo :: Int -> Bool #-}

  We *could* try to generate an f_spec with precisely the declared type:
      f_spec :: Int -> Bool
      f_spec = <f rhs> Int dNumInt |> co

      RULE: forall d. f Int d = f_spec |> sym co

  but the 'co' and 'sym co' are (a) playing no useful role, and (b) are
  hard to generate.  At all costs we must avoid this:
      RULE: forall d. f Int d |> co = f_spec
  because the LHS will never match (indeed it's rejected in
  decomposeRuleLhs).

  So we simply do this:
    - Generate a constraint to check that the specialised type (after
      skolemiseation) is equal to the instantiated function type.
    - But *discard* the evidence (coercion) for that constraint,
      so that we ultimately generate the simpler code
          f_spec :: Int -> F Int
          f_spec = <f rhs> Int dNumInt

          RULE: forall d. f Int d = f_spec
      You can see this discarding happening in

3. Note that the HsWrapper can transform *any* function with the right
   type prefix
       forall ab. (Eq a, Ix b) => XXX
   regardless of XXX.  It's sort of polymorphic in XXX.  This is
   useful: we use the same wrapper to transform each of the class ops, as
   well as the dict.  That's what goes on in TcInstDcls.mk_meth_spec_prags
-}

tcSpecPrags :: Id -> [LSig GhcRn]
            -> TcM [LTcSpecPrag]
-- Add INLINE and SPECIALSE pragmas
--    INLINE prags are added to the (polymorphic) Id directly
--    SPECIALISE prags are passed to the desugarer via TcSpecPrags
-- Pre-condition: the poly_id is zonked
-- Reason: required by tcSubExp
tcSpecPrags poly_id prag_sigs
  = do { traceTc "tcSpecPrags" (ppr poly_id <+> ppr spec_sigs)
       ; unless (null bad_sigs) warn_discarded_sigs
       ; pss <- mapAndRecoverM (wrapLocM (tcSpecPrag poly_id)) spec_sigs
       ; return $ concatMap (\(L l ps) -> map (L l) ps) pss }
  where
    spec_sigs = filter isSpecLSig prag_sigs
    bad_sigs  = filter is_bad_sig prag_sigs
    is_bad_sig s = not (isSpecLSig s || isInlineLSig s || isSCCFunSig s)

    warn_discarded_sigs
      = addWarnTc NoReason
                  (hang (text "Discarding unexpected pragmas for" <+> ppr poly_id)
                      2 (vcat (map (ppr . getLoc) bad_sigs)))

--------------
tcSpecPrag :: TcId -> Sig GhcRn -> TcM [TcSpecPrag]
tcSpecPrag poly_id prag@(SpecSig fun_name hs_tys inl)
-- See Note [Handling SPECIALISE pragmas]
--
-- The Name fun_name in the SpecSig may not be the same as that of the poly_id
-- Example: SPECIALISE for a class method: the Name in the SpecSig is
--          for the selector Id, but the poly_id is something like $cop
-- However we want to use fun_name in the error message, since that is
-- what the user wrote (Trac #8537)
  = addErrCtxt (spec_ctxt prag) $
    do  { warnIf (not (isOverloadedTy poly_ty || isInlinePragma inl))
                 (text "SPECIALISE pragma for non-overloaded function"
                  <+> quotes (ppr fun_name))
                  -- Note [SPECIALISE pragmas]
        ; spec_prags <- mapM tc_one hs_tys
        ; traceTc "tcSpecPrag" (ppr poly_id $$ nest 2 (vcat (map ppr spec_prags)))
        ; return spec_prags }
  where
    name      = idName poly_id
    poly_ty   = idType poly_id
    spec_ctxt prag = hang (text "In the SPECIALISE pragma") 2 (ppr prag)

    tc_one hs_ty
      = do { spec_ty <- tcHsSigType   (FunSigCtxt name False) hs_ty
           ; wrap    <- tcSpecWrapper (FunSigCtxt name True)  poly_ty spec_ty
           ; return (SpecPrag poly_id wrap inl) }

tcSpecPrag _ prag = pprPanic "tcSpecPrag" (ppr prag)

--------------
tcSpecWrapper :: UserTypeCtxt -> TcType -> TcType -> TcM HsWrapper
-- A simpler variant of tcSubType, used for SPECIALISE pragmas
-- See Note [Handling SPECIALISE pragmas], wrinkle 1
tcSpecWrapper ctxt poly_ty spec_ty
  = do { (sk_wrap, inst_wrap)
               <- tcSkolemise ctxt spec_ty $ \ _ spec_tau ->
                  do { (inst_wrap, tau) <- topInstantiate orig poly_ty
                     ; _ <- unifyType Nothing spec_tau tau
                            -- Deliberately ignore the evidence
                            -- See Note [Handling SPECIALISE pragmas],
                            --   wrinkle (2)
                     ; return inst_wrap }
       ; return (sk_wrap <.> inst_wrap) }
  where
    orig = SpecPragOrigin ctxt

--------------
tcImpPrags :: [LSig GhcRn] -> TcM [LTcSpecPrag]
-- SPECIALISE pragmas for imported things
tcImpPrags prags
  = do { this_mod <- getModule
       ; dflags <- getDynFlags
       ; if (not_specialising dflags) then
            return []
         else do
            { pss <- mapAndRecoverM (wrapLocM tcImpSpec)
                     [L loc (name,prag)
                               | (L loc prag@(SpecSig (L _ name) _ _)) <- prags
                               , not (nameIsLocalOrFrom this_mod name) ]
            ; return $ concatMap (\(L l ps) -> map (L l) ps) pss } }
  where
    -- Ignore SPECIALISE pragmas for imported things
    -- when we aren't specialising, or when we aren't generating
    -- code.  The latter happens when Haddocking the base library;
    -- we don't want complaints about lack of INLINABLE pragmas
    not_specialising dflags
      | not (gopt Opt_Specialise dflags) = True
      | otherwise = case hscTarget dflags of
                      HscNothing -> True
                      HscInterpreted -> True
                      _other         -> False

tcImpSpec :: (Name, Sig GhcRn) -> TcM [TcSpecPrag]
tcImpSpec (name, prag)
 = do { id <- tcLookupId name
      ; unless (isAnyInlinePragma (idInlinePragma id))
               (addWarnTc NoReason (impSpecErr name))
      ; tcSpecPrag id prag }

impSpecErr :: Name -> SDoc
impSpecErr name
  = hang (text "You cannot SPECIALISE" <+> quotes (ppr name))
       2 (vcat [ text "because its definition has no INLINE/INLINABLE pragma"
               , parens $ sep
                   [ text "or its defining module" <+> quotes (ppr mod)
                   , text "was compiled without -O"]])
  where
    mod = nameModule name
