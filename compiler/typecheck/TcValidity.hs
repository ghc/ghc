{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP, TupleSections, ViewPatterns #-}

module TcValidity (
  Rank, UserTypeCtxt(..), checkValidType, checkValidMonoType,
  ContextKind(..), expectedKindInCtxt,
  checkValidTheta, checkValidFamPats,
  checkValidInstance, checkValidInstHead, validDerivPred,
  checkTySynRhs,
  ClsInstInfo, checkValidCoAxiom, checkValidCoAxBranch,
  checkValidTyFamEqn,
  arityErr, badATErr,
  checkValidTelescope,
  allDistinctTyVars
  ) where

#include "HsVersions.h"

import GhcPrelude

import Maybes

-- friends:
import TcUnify    ( tcSubType_NC )
import TcSimplify ( simplifyAmbiguityCheck )
import ClsInst    ( matchGlobalInst, ClsInstResult(..), InstanceWhat(..) )
import TyCoRep
import TcType hiding ( sizeType, sizeTypes )
import TysWiredIn ( heqTyConName, eqTyConName, coercibleTyConName, omegaDataConTy )
import PrelNames
import Type
import Coercion
import CoAxiom
import Class
import TyCon

-- others:
import HsSyn            -- HsType
import TcRnMonad        -- TcType, amongst others
import TcEnv       ( tcInitTidyEnv, tcInitOpenTidyEnv )
import FunDeps
import FamInstEnv  ( isDominatedBy, injectiveBranches,
                     InjectivityCheckResult(..) )
import FamInst     ( makeInjectivityErrors )
import Name
import VarEnv
import VarSet
import Id          ( idType, idName )
import Var         ( VarBndr(..), mkTyVar )
import ErrUtils
import DynFlags
import Util
import ListSetOps
import SrcLoc
import Outputable
import Bag         ( emptyBag )
import Unique      ( mkAlphaTyVarUnique )
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.Foldable
import Data.List        ( (\\), nub )
import qualified Data.List.NonEmpty as NE

{-
************************************************************************
*                                                                      *
          Checking for ambiguity
*                                                                      *
************************************************************************

Note [The ambiguity check for type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
checkAmbiguity is a check on *user-supplied type signatures*.  It is
*purely* there to report functions that cannot possibly be called.  So for
example we want to reject:
   f :: C a => Int
The idea is there can be no legal calls to 'f' because every call will
give rise to an ambiguous constraint.  We could soundly omit the
ambiguity check on type signatures entirely, at the expense of
delaying ambiguity errors to call sites.  Indeed, the flag
-XAllowAmbiguousTypes switches off the ambiguity check.

What about things like this:
   class D a b | a -> b where ..
   h :: D Int b => Int
The Int may well fix 'b' at the call site, so that signature should
not be rejected.  Moreover, using *visible* fundeps is too
conservative.  Consider
   class X a b where ...
   class D a b | a -> b where ...
   instance D a b => X [a] b where...
   h :: X a b => a -> a
Here h's type looks ambiguous in 'b', but here's a legal call:
   ...(h [True])...
That gives rise to a (X [Bool] beta) constraint, and using the
instance means we need (D Bool beta) and that fixes 'beta' via D's
fundep!

Behind all these special cases there is a simple guiding principle.
Consider

  f :: <type>
  f = ...blah...

  g :: <type>
  g = f

You would think that the definition of g would surely typecheck!
After all f has exactly the same type, and g=f. But in fact f's type
is instantiated and the instantiated constraints are solved against
the originals, so in the case an ambiguous type it won't work.
Consider our earlier example f :: C a => Int.  Then in g's definition,
we'll instantiate to (C alpha) and try to deduce (C alpha) from (C a),
and fail.

So in fact we use this as our *definition* of ambiguity.  We use a
very similar test for *inferred* types, to ensure that they are
unambiguous. See Note [Impedance matching] in TcBinds.

This test is very conveniently implemented by calling
    tcSubType <type> <type>
This neatly takes account of the functional dependecy stuff above,
and implicit parameter (see Note [Implicit parameters and ambiguity]).
And this is what checkAmbiguity does.

What about this, though?
   g :: C [a] => Int
Is every call to 'g' ambiguous?  After all, we might have
   instance C [a] where ...
at the call site.  So maybe that type is ok!  Indeed even f's
quintessentially ambiguous type might, just possibly be callable:
with -XFlexibleInstances we could have
  instance C a where ...
and now a call could be legal after all!  Well, we'll reject this
unless the instance is available *here*.

Note [When to call checkAmbiguity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We call checkAmbiguity
   (a) on user-specified type signatures
   (b) in checkValidType

Conncerning (b), you might wonder about nested foralls.  What about
    f :: forall b. (forall a. Eq a => b) -> b
The nested forall is ambiguous.  Originally we called checkAmbiguity
in the forall case of check_type, but that had two bad consequences:
  * We got two error messages about (Eq b) in a nested forall like this:
       g :: forall a. Eq a => forall b. Eq b => a -> a
  * If we try to check for ambiguity of a nested forall like
    (forall a. Eq a => b), the implication constraint doesn't bind
    all the skolems, which results in "No skolem info" in error
    messages (see Trac #10432).

To avoid this, we call checkAmbiguity once, at the top, in checkValidType.
(I'm still a bit worried about unbound skolems when the type mentions
in-scope type variables.)

In fact, because of the co/contra-variance implemented in tcSubType,
this *does* catch function f above. too.

Concerning (a) the ambiguity check is only used for *user* types, not
for types coming from inteface files.  The latter can legitimately
have ambiguous types. Example

   class S a where s :: a -> (Int,Int)
   instance S Char where s _ = (1,1)
   f:: S a => [a] -> Int -> (Int,Int)
   f (_::[a]) x = (a*x,b)
        where (a,b) = s (undefined::a)

Here the worker for f gets the type
        fw :: forall a. S a => Int -> (# Int, Int #)


Note [Implicit parameters and ambiguity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Only a *class* predicate can give rise to ambiguity
An *implicit parameter* cannot.  For example:
        foo :: (?x :: [a]) => Int
        foo = length ?x
is fine.  The call site will supply a particular 'x'

Furthermore, the type variables fixed by an implicit parameter
propagate to the others.  E.g.
        foo :: (Show a, ?x::[a]) => Int
        foo = show (?x++?x)
The type of foo looks ambiguous.  But it isn't, because at a call site
we might have
        let ?x = 5::Int in foo
and all is well.  In effect, implicit parameters are, well, parameters,
so we can take their type variables into account as part of the
"tau-tvs" stuff.  This is done in the function 'FunDeps.grow'.
-}

checkAmbiguity :: UserTypeCtxt -> Type -> TcM ()
checkAmbiguity ctxt ty
  | wantAmbiguityCheck ctxt
  = do { traceTc "Ambiguity check for" (ppr ty)
         -- Solve the constraints eagerly because an ambiguous type
         -- can cause a cascade of further errors.  Since the free
         -- tyvars are skolemised, we can safely use tcSimplifyTop
       ; allow_ambiguous <- xoptM LangExt.AllowAmbiguousTypes
       ; (_wrap, wanted) <- addErrCtxt (mk_msg allow_ambiguous) $
                            captureConstraints $
                            tcSubType_NC ctxt ty ty
       ; simplifyAmbiguityCheck ty wanted

       ; traceTc "Done ambiguity check for" (ppr ty) }

  | otherwise
  = return ()
 where
   mk_msg allow_ambiguous
     = vcat [ text "In the ambiguity check for" <+> what
            , ppUnless allow_ambiguous ambig_msg ]
   ambig_msg = text "To defer the ambiguity check to use sites, enable AllowAmbiguousTypes"
   what | Just n <- isSigMaybe ctxt = quotes (ppr n)
        | otherwise                 = pprUserTypeCtxt ctxt

wantAmbiguityCheck :: UserTypeCtxt -> Bool
wantAmbiguityCheck ctxt
  = case ctxt of  -- See Note [When we don't check for ambiguity]
      GhciCtxt     -> False
      TySynCtxt {} -> False
      TypeAppCtxt  -> False
      _            -> True

checkUserTypeError :: Type -> TcM ()
-- Check to see if the type signature mentions "TypeError blah"
-- anywhere in it, and fail if so.
--
-- Very unsatisfactorily (Trac #11144) we need to tidy the type
-- because it may have come from an /inferred/ signature, not a
-- user-supplied one.  This is really only a half-baked fix;
-- the other errors in checkValidType don't do tidying, and so
-- may give bad error messages when given an inferred type.
checkUserTypeError = check
  where
  check ty
    | Just msg     <- userTypeError_maybe ty  = fail_with msg
    | Just (_,ts)  <- splitTyConApp_maybe ty  = mapM_ check ts
    | Just (t1,t2) <- splitAppTy_maybe ty     = check t1 >> check t2
    | Just (_,t1)  <- splitForAllTy_maybe ty  = check t1
    | otherwise                               = return ()

  fail_with msg = do { env0 <- tcInitTidyEnv
                     ; let (env1, tidy_msg) = tidyOpenType env0 msg
                     ; failWithTcM (env1, pprUserTypeErrorTy tidy_msg) }


{- Note [When we don't check for ambiguity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a few places we do not want to check a user-specified type for ambiguity

* GhciCtxt: Allow ambiguous types in GHCi's :kind command
  E.g.   type family T a :: *  -- T :: forall k. k -> *
  Then :k T should work in GHCi, not complain that
  (T k) is ambiguous!

* TySynCtxt: type T a b = C a b => blah
  It may be that when we /use/ T, we'll give an 'a' or 'b' that somehow
  cure the ambiguity.  So we defer the ambiguity check to the use site.

  There is also an implementation reason (Trac #11608).  In the RHS of
  a type synonym we don't (currently) instantiate 'a' and 'b' with
  TcTyVars before calling checkValidType, so we get asertion failures
  from doing an ambiguity check on a type with TyVars in it.  Fixing this
  would not be hard, but let's wait till there's a reason.

* TypeAppCtxt: visible type application
     f @ty
  No need to check ty for ambiguity


************************************************************************
*                                                                      *
          Checking validity of a user-defined type
*                                                                      *
************************************************************************

When dealing with a user-written type, we first translate it from an HsType
to a Type, performing kind checking, and then check various things that should
be true about it.  We don't want to perform these checks at the same time
as the initial translation because (a) they are unnecessary for interface-file
types and (b) when checking a mutually recursive group of type and class decls,
we can't "look" at the tycons/classes yet.  Also, the checks are rather
diverse, and used to really mess up the other code.

One thing we check for is 'rank'.

        Rank 0:         monotypes (no foralls)
        Rank 1:         foralls at the front only, Rank 0 inside
        Rank 2:         foralls at the front, Rank 1 on left of fn arrow,

        basic ::= tyvar | T basic ... basic

        r2  ::= forall tvs. cxt => r2a
        r2a ::= r1 -> r2a | basic
        r1  ::= forall tvs. cxt => r0
        r0  ::= r0 -> r0 | basic

Another thing is to check that type synonyms are saturated.
This might not necessarily show up in kind checking.
        type A i = i
        data T k = MkT (k Int)
        f :: T A        -- BAD!
-}

checkValidType :: UserTypeCtxt -> Type -> TcM ()
-- Checks that a user-written type is valid for the given context
-- Assumes argument is fully zonked
-- Not used for instance decls; checkValidInstance instead
checkValidType ctxt ty
  = do { traceTc "checkValidType" (ppr ty <+> text "::" <+> ppr (typeKind ty))
       ; rankn_flag  <- xoptM LangExt.RankNTypes
       ; impred_flag <- xoptM LangExt.ImpredicativeTypes
       ; let gen_rank :: Rank -> Rank
             gen_rank r | rankn_flag = ArbitraryRank
                        | otherwise  = r

             rank1 = gen_rank r1
             rank0 = gen_rank r0

             r0 = rankZeroMonoType
             r1 = LimitedRank True r0

             rank
               = case ctxt of
                 DefaultDeclCtxt-> MustBeMonoType
                 ResSigCtxt     -> MustBeMonoType
                 PatSigCtxt     -> rank0
                 RuleSigCtxt _  -> rank1
                 TySynCtxt _    -> rank0

                 ExprSigCtxt    -> rank1
                 KindSigCtxt    -> rank1
                 TypeAppCtxt | impred_flag -> ArbitraryRank
                             | otherwise   -> tyConArgMonoType
                    -- Normally, ImpredicativeTypes is handled in check_arg_type,
                    -- but visible type applications don't go through there.
                    -- So we do this check here.

                 FunSigCtxt {}  -> rank1
                 InfSigCtxt _   -> ArbitraryRank        -- Inferred type
                 ConArgCtxt _   -> rank1 -- We are given the type of the entire
                                         -- constructor, hence rank 1
                 PatSynCtxt _   -> rank1

                 ForSigCtxt _   -> rank1
                 SpecInstCtxt   -> rank1
                 ThBrackCtxt    -> rank1
                 GhciCtxt       -> ArbitraryRank

                 TyVarBndrKindCtxt _ -> rank0
                 DataKindCtxt _      -> rank1
                 TySynKindCtxt _     -> rank1
                 TyFamResKindCtxt _  -> rank1

                 _              -> panic "checkValidType"
                                          -- Can't happen; not used for *user* sigs

       ; env <- tcInitOpenTidyEnv (tyCoVarsOfTypeList ty)

        -- Check the internal validity of the type itself
       ; check_type env ctxt rank ty

       ; checkUserTypeError ty

       -- Check for ambiguous types.  See Note [When to call checkAmbiguity]
       -- NB: this will happen even for monotypes, but that should be cheap;
       --     and there may be nested foralls for the subtype test to examine
       ; checkAmbiguity ctxt ty

       ; traceTc "checkValidType done" (ppr ty <+> text "::" <+> ppr (typeKind ty)) }

checkValidMonoType :: Type -> TcM ()
-- Assumes argument is fully zonked
checkValidMonoType ty
  = do { env <- tcInitOpenTidyEnv (tyCoVarsOfTypeList ty)
       ; check_type env SigmaCtxt MustBeMonoType ty }

checkTySynRhs :: UserTypeCtxt -> TcType -> TcM ()
checkTySynRhs ctxt ty
  | tcReturnsConstraintKind actual_kind
  = do { ck <- xoptM LangExt.ConstraintKinds
       ; if ck
         then  when (tcIsConstraintKind actual_kind)
                    (do { dflags <- getDynFlags
                        ; check_pred_ty emptyTidyEnv dflags ctxt ty })
         else addErrTcM (constraintSynErr emptyTidyEnv actual_kind) }

  | otherwise
  = return ()
  where
    actual_kind = typeKind ty

-- | The kind expected in a certain context.
data ContextKind = TheKind Kind   -- ^ a specific kind
                 | AnythingKind   -- ^ any kind will do
                 | OpenKind       -- ^ something of the form @TYPE _@

-- Depending on the context, we might accept any kind (for instance, in a TH
-- splice), or only certain kinds (like in type signatures).
expectedKindInCtxt :: UserTypeCtxt -> ContextKind
expectedKindInCtxt (TySynCtxt _)   = AnythingKind
expectedKindInCtxt ThBrackCtxt     = AnythingKind
expectedKindInCtxt GhciCtxt        = AnythingKind
-- The types in a 'default' decl can have varying kinds
-- See Note [Extended defaults]" in TcEnv
expectedKindInCtxt DefaultDeclCtxt     = AnythingKind
expectedKindInCtxt TypeAppCtxt         = AnythingKind
expectedKindInCtxt (ForSigCtxt _)      = TheKind liftedTypeKind
expectedKindInCtxt (InstDeclCtxt {})   = TheKind constraintKind
expectedKindInCtxt SpecInstCtxt        = TheKind constraintKind
expectedKindInCtxt _                   = OpenKind

{-
Note [Higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~
Technically
            Int -> forall a. a->a
is still a rank-1 type, but it's not Haskell 98 (Trac #5957).  So the
validity checker allow a forall after an arrow only if we allow it
before -- that is, with Rank2Types or RankNTypes
-}

data Rank = ArbitraryRank         -- Any rank ok

          | LimitedRank   -- Note [Higher rank types]
                 Bool     -- Forall ok at top
                 Rank     -- Use for function arguments

          | MonoType SDoc   -- Monotype, with a suggestion of how it could be a polytype

          | MustBeMonoType  -- Monotype regardless of flags


rankZeroMonoType, tyConArgMonoType, synArgMonoType, constraintMonoType :: Rank
rankZeroMonoType   = MonoType (text "Perhaps you intended to use RankNTypes or Rank2Types")
tyConArgMonoType   = MonoType (text "GHC doesn't yet support impredicative polymorphism")
synArgMonoType     = MonoType (text "Perhaps you intended to use LiberalTypeSynonyms")
constraintMonoType = MonoType (vcat [ text "A constraint must be a monotype"
                                    , text "Perhaps you intended to use QuantifiedConstraints" ])

funArgResRank :: Rank -> (Rank, Rank)             -- Function argument and result
funArgResRank (LimitedRank _ arg_rank) = (arg_rank, LimitedRank (forAllAllowed arg_rank) arg_rank)
funArgResRank other_rank               = (other_rank, other_rank)

forAllAllowed :: Rank -> Bool
forAllAllowed ArbitraryRank             = True
forAllAllowed (LimitedRank forall_ok _) = forall_ok
forAllAllowed _                         = False

----------------------------------------
check_type :: TidyEnv -> UserTypeCtxt -> Rank -> Type -> TcM ()
-- The args say what the *type context* requires, independent
-- of *flag* settings.  You test the flag settings at usage sites.
--
-- Rank is allowed rank for function args
-- Rank 0 means no for-alls anywhere

check_type env ctxt rank ty
  | not (null tvbs && null theta)
  = do  { traceTc "check_type" (ppr ty $$ ppr (forAllAllowed rank))
        ; checkTcM (forAllAllowed rank) (forAllTyErr env rank ty)
                -- Reject e.g. (Maybe (?x::Int => Int)),
                -- with a decent error message

        ; check_valid_theta env' SigmaCtxt theta
                -- Allow     type T = ?x::Int => Int -> Int
                -- but not   type T = ?x::Int

        ; check_type env' ctxt rank tau      -- Allow foralls to right of arrow

        ; checkTcM (not (any (`elemVarSet` tyCoVarsOfType phi_kind) tvs))
                   (forAllEscapeErr env' ty tau_kind)
        }
  where
    (tvbs, phi)  = tcSplitForAllVarBndrs ty
    (theta, tau) = tcSplitPhiTy phi

    tvs          = binderVars tvbs
    (env', _)    = tidyVarBndrs env tvs

    tau_kind              = typeKind tau
    phi_kind | null theta = tau_kind
             | otherwise  = liftedTypeKind
        -- If there are any constraints, the kind is *. (#11405)

check_type _ _ _ (TyVarTy _) = return ()

check_type env ctxt rank (FunTy _ arg_ty res_ty)
  = do  { check_type env ctxt arg_rank arg_ty
        ; check_type env ctxt res_rank res_ty }
  where
    (arg_rank, res_rank) = funArgResRank rank

check_type env ctxt rank (AppTy ty1 ty2)
  = do  { check_type env ctxt rank ty1
        ; check_arg_type env ctxt rank ty2 }

check_type env ctxt rank ty@(TyConApp tc tys)
  | isTypeSynonymTyCon tc || isTypeFamilyTyCon tc
  = check_syn_tc_app env ctxt rank ty tc tys
  | isUnboxedTupleTyCon tc = check_ubx_tuple  env ctxt      ty    tys
  | otherwise              = mapM_ (check_arg_type env ctxt rank) tys

check_type _ _ _ (LitTy {}) = return ()

check_type env ctxt rank (CastTy ty _) = check_type env ctxt rank ty

check_type _ _ _ ty = pprPanic "check_type" (ppr ty)

----------------------------------------
check_syn_tc_app :: TidyEnv -> UserTypeCtxt -> Rank -> KindOrType
                 -> TyCon -> [KindOrType] -> TcM ()
-- Used for type synonyms and type synonym families,
-- which must be saturated,
-- but not data families, which need not be saturated
check_syn_tc_app env ctxt rank ty tc tys
  | tys `lengthAtLeast` tc_arity   -- Saturated
       -- Check that the synonym has enough args
       -- This applies equally to open and closed synonyms
       -- It's OK to have an *over-applied* type synonym
       --      data Tree a b = ...
       --      type Foo a = Tree [a]
       --      f :: Foo a b -> ...
  = do  { -- See Note [Liberal type synonyms]
        ; liberal <- xoptM LangExt.LiberalTypeSynonyms
        ; if not liberal || isTypeFamilyTyCon tc then
                -- For H98 and synonym families, do check the type args
                mapM_ check_arg tys

          else  -- In the liberal case (only for closed syns), expand then check
          case tcView ty of
             Just ty' -> check_type env ctxt rank ty'
             Nothing  -> pprPanic "check_tau_type" (ppr ty)  }

  | GhciCtxt <- ctxt  -- Accept under-saturated type synonyms in
                      -- GHCi :kind commands; see Trac #7586
  = mapM_ check_arg tys

  | otherwise
  = failWithTc (tyConArityErr tc tys)
  where
    tc_arity  = tyConArity tc
    check_arg | isTypeFamilyTyCon tc = check_arg_type  env ctxt rank
              | otherwise            = check_type      env ctxt synArgMonoType

----------------------------------------
check_ubx_tuple :: TidyEnv -> UserTypeCtxt -> KindOrType
                -> [KindOrType] -> TcM ()
check_ubx_tuple env ctxt ty tys
  = do  { ub_tuples_allowed <- xoptM LangExt.UnboxedTuples
        ; checkTcM ub_tuples_allowed (ubxArgTyErr env ty)

        ; impred <- xoptM LangExt.ImpredicativeTypes
        ; let rank' = if impred then ArbitraryRank else tyConArgMonoType
                -- c.f. check_arg_type
                -- However, args are allowed to be unlifted, or
                -- more unboxed tuples, so can't use check_arg_ty
        ; mapM_ (check_type env ctxt rank') tys }

----------------------------------------
check_arg_type :: TidyEnv -> UserTypeCtxt -> Rank -> KindOrType -> TcM ()
-- The sort of type that can instantiate a type variable,
-- or be the argument of a type constructor.
-- Not an unboxed tuple, but now *can* be a forall (since impredicativity)
-- Other unboxed types are very occasionally allowed as type
-- arguments depending on the kind of the type constructor
--
-- For example, we want to reject things like:
--
--      instance Ord a => Ord (forall s. T s a)
-- and
--      g :: T s (forall b.b)
--
-- NB: unboxed tuples can have polymorphic or unboxed args.
--     This happens in the workers for functions returning
--     product types with polymorphic components.
--     But not in user code.
-- Anyway, they are dealt with by a special case in check_tau_type

check_arg_type _ _ _ (CoercionTy {}) = return ()

check_arg_type env ctxt rank ty
  = do  { impred <- xoptM LangExt.ImpredicativeTypes
        ; let rank' = case rank of          -- Predictive => must be monotype
                        MustBeMonoType     -> MustBeMonoType  -- Monotype, regardless
                        _other | impred    -> ArbitraryRank
                               | otherwise -> tyConArgMonoType
                        -- Make sure that MustBeMonoType is propagated,
                        -- so that we don't suggest -XImpredicativeTypes in
                        --    (Ord (forall a.a)) => a -> a
                        -- and so that if it Must be a monotype, we check that it is!

        ; check_type env ctxt rank' ty }

----------------------------------------
forAllTyErr :: TidyEnv -> Rank -> Type -> (TidyEnv, SDoc)
forAllTyErr env rank ty
   = ( env
     , vcat [ hang herald 2 (ppr_tidy env ty)
            , suggestion ] )
  where
    (tvs, _theta, _tau) = tcSplitSigmaTy ty
    herald | null tvs  = text "Illegal qualified type:"
           | otherwise = text "Illegal polymorphic type:"
    suggestion = case rank of
                   LimitedRank {} -> text "Perhaps you intended to use RankNTypes or Rank2Types"
                   MonoType d     -> d
                   _              -> Outputable.empty -- Polytype is always illegal

forAllEscapeErr :: TidyEnv -> Type -> Kind -> (TidyEnv, SDoc)
forAllEscapeErr env ty tau_kind
  = ( env
    , hang (vcat [ text "Quantified type's kind mentions quantified type variable"
                 , text "(skolem escape)" ])
         2 (vcat [ text "   type:" <+> ppr_tidy env ty
                 , text "of kind:" <+> ppr_tidy env tau_kind ]) )

ubxArgTyErr :: TidyEnv -> Type -> (TidyEnv, SDoc)
ubxArgTyErr env ty
  = ( env, vcat [ sep [ text "Illegal unboxed tuple type as function argument:"
                      , ppr_tidy env ty ]
                , text "Perhaps you intended to use UnboxedTuples" ] )

{-
Note [Liberal type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If -XLiberalTypeSynonyms is on, expand closed type synonyms *before*
doing validity checking.  This allows us to instantiate a synonym defn
with a for-all type, or with a partially-applied type synonym.
        e.g.   type T a b = a
               type S m   = m ()
               f :: S (T Int)
Here, T is partially applied, so it's illegal in H98.  But if you
expand S first, then T we get just
               f :: Int
which is fine.

IMPORTANT: suppose T is a type synonym.  Then we must do validity
checking on an appliation (T ty1 ty2)

        *either* before expansion (i.e. check ty1, ty2)
        *or* after expansion (i.e. expand T ty1 ty2, and then check)
        BUT NOT BOTH

If we do both, we get exponential behaviour!!

  data TIACons1 i r c = c i ::: r c
  type TIACons2 t x = TIACons1 t (TIACons1 t x)
  type TIACons3 t x = TIACons2 t (TIACons1 t x)
  type TIACons4 t x = TIACons2 t (TIACons2 t x)
  type TIACons7 t x = TIACons4 t (TIACons3 t x)


************************************************************************
*                                                                      *
\subsection{Checking a theta or source type}
*                                                                      *
************************************************************************

Note [Implicit parameters in instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Implicit parameters _only_ allowed in type signatures; not in instance
decls, superclasses etc. The reason for not allowing implicit params in
instances is a bit subtle.  If we allowed
  instance (?x::Int, Eq a) => Foo [a] where ...
then when we saw
     (e :: (?x::Int) => t)
it would be unclear how to discharge all the potential uses of the ?x
in e.  For example, a constraint Foo [Int] might come out of e, and
applying the instance decl would show up two uses of ?x.  Trac #8912.
-}

checkValidTheta :: UserTypeCtxt -> ThetaType -> TcM ()
-- Assumes argument is fully zonked
checkValidTheta ctxt theta
  = addErrCtxtM (checkThetaCtxt ctxt theta) $
    do { env <- tcInitOpenTidyEnv (tyCoVarsOfTypesList theta)
       ; check_valid_theta env ctxt theta }

-------------------------
check_valid_theta :: TidyEnv -> UserTypeCtxt -> [PredType] -> TcM ()
check_valid_theta _ _ []
  = return ()
check_valid_theta env ctxt theta
  = do { dflags <- getDynFlags
       ; warnTcM (Reason Opt_WarnDuplicateConstraints)
                 (wopt Opt_WarnDuplicateConstraints dflags && notNull dups)
                 (dupPredWarn env dups)
       ; traceTc "check_valid_theta" (ppr theta)
       ; mapM_ (check_pred_ty env dflags ctxt) theta }
  where
    (_,dups) = removeDups nonDetCmpType theta
    -- It's OK to use nonDetCmpType because dups only appears in the
    -- warning

-------------------------
{- Note [Validity checking for constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We look through constraint synonyms so that we can see the underlying
constraint(s).  For example
   type Foo = ?x::Int
   instance Foo => C T
We should reject the instance because it has an implicit parameter in
the context.

But we record, in 'under_syn', whether we have looked under a synonym
to avoid requiring language extensions at the use site.  Main example
(Trac #9838):

   {-# LANGUAGE ConstraintKinds #-}
   module A where
      type EqShow a = (Eq a, Show a)

   module B where
      import A
      foo :: EqShow a => a -> String

We don't want to require ConstraintKinds in module B.
-}

check_pred_ty :: TidyEnv -> DynFlags -> UserTypeCtxt -> PredType -> TcM ()
-- Check the validity of a predicate in a signature
-- See Note [Validity checking for constraints]
check_pred_ty env dflags ctxt pred
  = do { check_type env SigmaCtxt rank pred
       ; check_pred_help False env dflags ctxt pred }
  where
    rank | xopt LangExt.QuantifiedConstraints dflags
         = ArbitraryRank
         | otherwise
         = constraintMonoType

check_pred_help :: Bool    -- True <=> under a type synonym
                -> TidyEnv
                -> DynFlags -> UserTypeCtxt
                -> PredType -> TcM ()
check_pred_help under_syn env dflags ctxt pred
  | Just pred' <- tcView pred  -- Switch on under_syn when going under a
                                 -- synonym (Trac #9838, yuk)
  = check_pred_help True env dflags ctxt pred'

  | otherwise  -- A bit like classifyPredType, but not the same
               -- E.g. we treat (~) like (~#); and we look inside tuples
  = case classifyPredType pred of
      ClassPred cls tys
        | isCTupleClass cls   -> check_tuple_pred under_syn env dflags ctxt pred tys
        | otherwise           -> check_class_pred env dflags ctxt pred cls tys

      EqPred NomEq _ _  -> -- a ~# b
                           check_eq_pred env dflags pred

      EqPred ReprEq _ _ -> -- Ugh!  When inferring types we may get
                           -- f :: (a ~R# b) => blha
                           -- And we want to treat that like (Coercible a b)
                           -- We should probably check argument shapes, but we
                           -- didn't do so before, so I'm leaving it for now
                           return ()

      ForAllPred _ theta head -> check_quant_pred env dflags ctxt pred theta head
      IrredPred {}            -> check_irred_pred under_syn env dflags ctxt pred

check_eq_pred :: TidyEnv -> DynFlags -> PredType -> TcM ()
check_eq_pred env dflags pred
  =         -- Equational constraints are valid in all contexts if type
            -- families are permitted
    checkTcM (xopt LangExt.TypeFamilies dflags
              || xopt LangExt.GADTs dflags)
             (eqPredTyErr env pred)

check_quant_pred :: TidyEnv -> DynFlags -> UserTypeCtxt
                 -> PredType -> ThetaType -> PredType -> TcM ()
check_quant_pred env dflags _ctxt pred theta head_pred
  = addErrCtxt (text "In the quantified constraint" <+> quotes (ppr pred)) $
    do { -- Check the instance head
         case classifyPredType head_pred of
            ClassPred cls tys -> checkValidInstHead SigmaCtxt cls tys
                                 -- SigmaCtxt tells checkValidInstHead that
                                 -- this is the head of a quantified constraint
            IrredPred {}      | hasTyVarHead head_pred
                              -> return ()
            _                 -> failWithTcM (badQuantHeadErr env pred)

         -- Check for termination
       ; unless (xopt LangExt.UndecidableInstances dflags) $
         checkInstTermination theta head_pred
    }

check_tuple_pred :: Bool -> TidyEnv -> DynFlags -> UserTypeCtxt -> PredType -> [PredType] -> TcM ()
check_tuple_pred under_syn env dflags ctxt pred ts
  = do { -- See Note [ConstraintKinds in predicates]
         checkTcM (under_syn || xopt LangExt.ConstraintKinds dflags)
                  (predTupleErr env pred)
       ; mapM_ (check_pred_help under_syn env dflags ctxt) ts }
    -- This case will not normally be executed because without
    -- -XConstraintKinds tuple types are only kind-checked as *

check_irred_pred :: Bool -> TidyEnv -> DynFlags -> UserTypeCtxt -> PredType -> TcM ()
check_irred_pred under_syn env dflags ctxt pred
    -- The predicate looks like (X t1 t2) or (x t1 t2) :: Constraint
    -- where X is a type function
  = do { -- If it looks like (x t1 t2), require ConstraintKinds
         --   see Note [ConstraintKinds in predicates]
         -- But (X t1 t2) is always ok because we just require ConstraintKinds
         -- at the definition site (Trac #9838)
        failIfTcM (not under_syn && not (xopt LangExt.ConstraintKinds dflags)
                                && hasTyVarHead pred)
                  (predIrredErr env pred)

         -- Make sure it is OK to have an irred pred in this context
         -- See Note [Irreducible predicates in superclasses]
       ; failIfTcM (is_superclass ctxt
                    && not (xopt LangExt.UndecidableInstances dflags)
                    && has_tyfun_head pred)
                   (predSuperClassErr env pred) }
  where
    is_superclass ctxt = case ctxt of { ClassSCCtxt _ -> True; _ -> False }
    has_tyfun_head ty
      = case tcSplitTyConApp_maybe ty of
          Just (tc, _) -> isTypeFamilyTyCon tc
          Nothing      -> False

{- Note [ConstraintKinds in predicates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Don't check for -XConstraintKinds under a type synonym, because that
was done at the type synonym definition site; see Trac #9838
e.g.   module A where
          type C a = (Eq a, Ix a)   -- Needs -XConstraintKinds
       module B where
          import A
          f :: C a => a -> a        -- Does *not* need -XConstraintKinds

Note [Irreducible predicates in superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Allowing type-family calls in class superclasses is somewhat dangerous
because we can write:

 type family Fooish x :: * -> Constraint
 type instance Fooish () = Foo
 class Fooish () a => Foo a where

This will cause the constraint simplifier to loop because every time we canonicalise a
(Foo a) class constraint we add a (Fooish () a) constraint which will be immediately
solved to add+canonicalise another (Foo a) constraint.  -}

-------------------------
check_class_pred :: TidyEnv -> DynFlags -> UserTypeCtxt
                 -> PredType -> Class -> [TcType] -> TcM ()
check_class_pred env dflags ctxt pred cls tys
  |  cls `hasKey` heqTyConKey   -- (~) and (~~) are classified as classes,
  || cls `hasKey` eqTyConKey    -- but here we want to treat them as equalities
  = -- pprTrace "check_class" (ppr cls) $
    check_eq_pred env dflags pred

  | isIPClass cls
  = do { check_arity
       ; checkTcM (okIPCtxt ctxt) (badIPPred env pred) }

  | otherwise     -- Includes Coercible
  = do { check_arity
       ; checkSimplifiableClassConstraint env dflags ctxt cls tys
       ; checkTcM arg_tys_ok (predTyVarErr env pred) }
  where
    check_arity = checkTc (tys `lengthIs` classArity cls)
                          (tyConArityErr (classTyCon cls) tys)

    -- Check the arguments of a class constraint
    flexible_contexts = xopt LangExt.FlexibleContexts     dflags
    undecidable_ok    = xopt LangExt.UndecidableInstances dflags
    arg_tys_ok = case ctxt of
        SpecInstCtxt -> True    -- {-# SPECIALISE instance Eq (T Int) #-} is fine
        InstDeclCtxt {} -> checkValidClsArgs (flexible_contexts || undecidable_ok) cls tys
                                -- Further checks on head and theta
                                -- in checkInstTermination
        _               -> checkValidClsArgs flexible_contexts cls tys

checkSimplifiableClassConstraint :: TidyEnv -> DynFlags -> UserTypeCtxt
                                 -> Class -> [TcType] -> TcM ()
-- See Note [Simplifiable given constraints]
checkSimplifiableClassConstraint env dflags ctxt cls tys
  | not (wopt Opt_WarnSimplifiableClassConstraints dflags)
  = return ()
  | xopt LangExt.MonoLocalBinds dflags
  = return ()

  | DataTyCtxt {} <- ctxt   -- Don't do this check for the "stupid theta"
  = return ()               -- of a data type declaration

  | cls `hasKey` coercibleTyConKey
  = return ()   -- Oddly, we treat (Coercible t1 t2) as unconditionally OK
                -- matchGlobalInst will reply "yes" because we can reduce
                -- (Coercible a b) to (a ~R# b)

  | otherwise
  = do { result <- matchGlobalInst dflags False cls tys
       ; case result of
           OneInst { cir_what = what }
              -> addWarnTc (Reason Opt_WarnSimplifiableClassConstraints)
                                   (simplifiable_constraint_warn what)
           _          -> return () }
  where
    pred = mkClassPred cls tys

    simplifiable_constraint_warn :: InstanceWhat -> SDoc
    simplifiable_constraint_warn what
     = vcat [ hang (text "The constraint" <+> quotes (ppr (tidyType env pred))
                    <+> text "matches")
                 2 (ppr_what what)
            , hang (text "This makes type inference for inner bindings fragile;")
                 2 (text "either use MonoLocalBinds, or simplify it using the instance") ]

    ppr_what BuiltinInstance = text "a built-in instance"
    ppr_what LocalInstance   = text "a locally-quantified instance"
    ppr_what (TopLevInstance { iw_dfun_id = dfun })
      = hang (text "instance" <+> pprSigmaType (idType dfun))
           2 (text "--" <+> pprDefinedAt (idName dfun))


{- Note [Simplifiable given constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A type signature like
   f :: Eq [(a,b)] => a -> b
is very fragile, for reasons described at length in TcInteract
Note [Instance and Given overlap].  As that Note discusses, for the
most part the clever stuff in TcInteract means that we don't use a
top-level instance if a local Given might fire, so there is no
fragility. But if we /infer/ the type of a local let-binding, things
can go wrong (Trac #11948 is an example, discussed in the Note).

So this warning is switched on only if we have NoMonoLocalBinds; in
that case the warning discourages users from writing simplifiable
class constraints.

The warning only fires if the constraint in the signature
matches the top-level instances in only one way, and with no
unifiers -- that is, under the same circumstances that
TcInteract.matchInstEnv fires an interaction with the top
level instances.  For example (Trac #13526), consider

  instance {-# OVERLAPPABLE #-} Eq (T a) where ...
  instance                   Eq (T Char) where ..
  f :: Eq (T a) => ...

We don't want to complain about this, even though the context
(Eq (T a)) matches an instance, because the user may be
deliberately deferring the choice so that the Eq (T Char)
has a chance to fire when 'f' is called.  And the fragility
only matters when there's a risk that the instance might
fire instead of the local 'given'; and there is no such
risk in this case.  Just use the same rules as for instance
firing!
-}

-------------------------
okIPCtxt :: UserTypeCtxt -> Bool
  -- See Note [Implicit parameters in instance decls]
okIPCtxt (FunSigCtxt {})        = True
okIPCtxt (InfSigCtxt {})        = True
okIPCtxt ExprSigCtxt            = True
okIPCtxt TypeAppCtxt            = True
okIPCtxt PatSigCtxt             = True
okIPCtxt ResSigCtxt             = True
okIPCtxt GenSigCtxt             = True
okIPCtxt (ConArgCtxt {})        = True
okIPCtxt (ForSigCtxt {})        = True  -- ??
okIPCtxt ThBrackCtxt            = True
okIPCtxt GhciCtxt               = True
okIPCtxt SigmaCtxt              = True
okIPCtxt (DataTyCtxt {})        = True
okIPCtxt (PatSynCtxt {})        = True
okIPCtxt (TySynCtxt {})         = True   -- e.g.   type Blah = ?x::Int
                                         -- Trac #11466

okIPCtxt (KindSigCtxt {})       = False
okIPCtxt (ClassSCCtxt {})       = False
okIPCtxt (InstDeclCtxt {})      = False
okIPCtxt (SpecInstCtxt {})      = False
okIPCtxt (RuleSigCtxt {})       = False
okIPCtxt DefaultDeclCtxt        = False
okIPCtxt DerivClauseCtxt        = False
okIPCtxt (TyVarBndrKindCtxt {}) = False
okIPCtxt (DataKindCtxt {})      = False
okIPCtxt (TySynKindCtxt {})     = False
okIPCtxt (TyFamResKindCtxt {})  = False

{-
Note [Kind polymorphic type classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MultiParam check:

    class C f where...   -- C :: forall k. k -> Constraint
    instance C Maybe where...

  The dictionary gets type [C * Maybe] even if it's not a MultiParam
  type class.

Flexibility check:

    class C f where...   -- C :: forall k. k -> Constraint
    data D a = D a
    instance C D where

  The dictionary gets type [C * (D *)]. IA0_TODO it should be
  generalized actually.
-}

checkThetaCtxt :: UserTypeCtxt -> ThetaType -> TidyEnv -> TcM (TidyEnv, SDoc)
checkThetaCtxt ctxt theta env
  = return ( env
           , vcat [ text "In the context:" <+> pprTheta (tidyTypes env theta)
                  , text "While checking" <+> pprUserTypeCtxt ctxt ] )

eqPredTyErr, predTupleErr, predIrredErr,
   predSuperClassErr, badQuantHeadErr :: TidyEnv -> PredType -> (TidyEnv, SDoc)
badQuantHeadErr env pred
  = ( env
    , hang (text "Quantified predicate must have a class or type variable head:")
         2 (ppr_tidy env pred) )
eqPredTyErr  env pred
  = ( env
    , text "Illegal equational constraint" <+> ppr_tidy env pred $$
      parens (text "Use GADTs or TypeFamilies to permit this") )
predTupleErr env pred
  = ( env
    , hang (text "Illegal tuple constraint:" <+> ppr_tidy env pred)
         2 (parens constraintKindsMsg) )
predIrredErr env pred
  = ( env
    , hang (text "Illegal constraint:" <+> ppr_tidy env pred)
         2 (parens constraintKindsMsg) )
predSuperClassErr env pred
  = ( env
    , hang (text "Illegal constraint" <+> quotes (ppr_tidy env pred)
            <+> text "in a superclass context")
         2 (parens undecidableMsg) )

predTyVarErr :: TidyEnv -> PredType -> (TidyEnv, SDoc)
predTyVarErr env pred
  = (env
    , vcat [ hang (text "Non type-variable argument")
                2 (text "in the constraint:" <+> ppr_tidy env pred)
           , parens (text "Use FlexibleContexts to permit this") ])

badIPPred :: TidyEnv -> PredType -> (TidyEnv, SDoc)
badIPPred env pred
  = ( env
    , text "Illegal implicit parameter" <+> quotes (ppr_tidy env pred) )

constraintSynErr :: TidyEnv -> Type -> (TidyEnv, SDoc)
constraintSynErr env kind
  = ( env
    , hang (text "Illegal constraint synonym of kind:" <+> quotes (ppr_tidy env kind))
         2 (parens constraintKindsMsg) )

dupPredWarn :: TidyEnv -> [NE.NonEmpty PredType] -> (TidyEnv, SDoc)
dupPredWarn env dups
  = ( env
    , text "Duplicate constraint" <> plural primaryDups <> text ":"
      <+> pprWithCommas (ppr_tidy env) primaryDups )
  where
    primaryDups = map NE.head dups

tyConArityErr :: TyCon -> [TcType] -> SDoc
-- For type-constructor arity errors, be careful to report
-- the number of /visible/ arguments required and supplied,
-- ignoring the /invisible/ arguments, which the user does not see.
-- (e.g. Trac #10516)
tyConArityErr tc tks
  = arityErr (ppr (tyConFlavour tc)) (tyConName tc)
             tc_type_arity tc_type_args
  where
    vis_tks = filterOutInvisibleTypes tc tks

    -- tc_type_arity = number of *type* args expected
    -- tc_type_args  = number of *type* args encountered
    tc_type_arity = count isVisibleTyConBinder (tyConBinders tc)
    tc_type_args  = length vis_tks

arityErr :: Outputable a => SDoc -> a -> Int -> Int -> SDoc
arityErr what name n m
  = hsep [ text "The" <+> what, quotes (ppr name), text "should have",
           n_arguments <> comma, text "but has been given",
           if m==0 then text "none" else int m]
    where
        n_arguments | n == 0 = text "no arguments"
                    | n == 1 = text "1 argument"
                    | True   = hsep [int n, text "arguments"]

{-
************************************************************************
*                                                                      *
\subsection{Checking for a decent instance head type}
*                                                                      *
************************************************************************

@checkValidInstHead@ checks the type {\em and} its syntactic constraints:
it must normally look like: @instance Foo (Tycon a b c ...) ...@

The exceptions to this syntactic checking: (1)~if the @GlasgowExts@
flag is on, or (2)~the instance is imported (they must have been
compiled elsewhere). In these cases, we let them go through anyway.

We can also have instances for functions: @instance Foo (a -> b) ...@.
-}

checkValidInstHead :: UserTypeCtxt -> Class -> [Type] -> TcM ()
checkValidInstHead ctxt clas cls_args
  = do { dflags   <- getDynFlags
       ; is_boot  <- tcIsHsBootOrSig
       ; is_sig   <- tcIsHsig
       ; check_valid_inst_head dflags is_boot is_sig ctxt clas cls_args
       }

{-

Note [Instances of built-in classes in signature files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

User defined instances for KnownNat, KnownSymbol and Typeable are
disallowed -- they are generated when needed by GHC itself on-the-fly.

However, if they occur in a Backpack signature file, they have an
entirely different meaning. Suppose in M.hsig we see

  signature M where
    data T :: Nat
    instance KnownNat T

That says that any module satisfying M.hsig must provide a KnownNat
instance for T.  We absolultely need that instance when compiling a
module that imports M.hsig: see Trac #15379 and
Note [Fabricating Evidence for Literals in Backpack] in ClsInst.

Hence, checkValidInstHead accepts a user-written instance declaration
in hsig files, where `is_sig` is True.

-}

check_valid_inst_head :: DynFlags -> Bool -> Bool
                      -> UserTypeCtxt -> Class -> [Type] -> TcM ()
-- Wow!  There are a surprising number of ad-hoc special cases here.
check_valid_inst_head dflags is_boot is_sig ctxt clas cls_args

  -- If not in an hs-boot file, abstract classes cannot have instances
  | isAbstractClass clas
  , not is_boot
  = failWithTc abstract_class_msg

  -- For Typeable, don't complain about instances for
  -- standalone deriving; they are no-ops, and we warn about
  -- it in TcDeriv.deriveStandalone.
  | clas_nm == typeableClassName
  , not is_sig
    -- Note [Instances of built-in classes in signature files]
  , hand_written_bindings
  = failWithTc rejected_class_msg

  -- Handwritten instances of KnownNat/KnownSymbol class
  -- are always forbidden (#12837)
  | clas_nm `elem` [ knownNatClassName, knownSymbolClassName ]
  , not is_sig
    -- Note [Instances of built-in classes in signature files]
  , hand_written_bindings
  = failWithTc rejected_class_msg

  -- For the most part we don't allow
  -- instances for (~), (~~), or Coercible;
  -- but we DO want to allow them in quantified constraints:
  --   f :: (forall a b. Coercible a b => Coercible (m a) (m b)) => ...m...
  | clas_nm `elem` [ heqTyConName, eqTyConName, coercibleTyConName ]
  , not quantified_constraint
  = failWithTc rejected_class_msg

  -- Check for hand-written Generic instances (disallowed in Safe Haskell)
  | clas_nm `elem` genericClassNames
  , hand_written_bindings
  =  do { failIfTc (safeLanguageOn dflags) gen_inst_err
        ; when (safeInferOn dflags) (recordUnsafeInfer emptyBag) }

  | clas_nm == hasFieldClassName
  = checkHasFieldInst clas cls_args

  | isCTupleClass clas
  = failWithTc tuple_class_msg

  -- Check language restrictions on the args to the class
  | check_h98_arg_shape
  , Just msg <- mb_ty_args_msg
  = failWithTc (instTypeErr clas cls_args msg)

  | otherwise
  = checkValidTypePats (classTyCon clas) cls_args
  where
    clas_nm = getName clas
    ty_args = filterOutInvisibleTypes (classTyCon clas) cls_args

    hand_written_bindings
        = case ctxt of
            InstDeclCtxt stand_alone -> not stand_alone
            SpecInstCtxt             -> False
            DerivClauseCtxt          -> False
            _                        -> True

    check_h98_arg_shape = case ctxt of
                            SpecInstCtxt    -> False
                            DerivClauseCtxt -> False
                            SigmaCtxt       -> False
                            _               -> True
        -- SigmaCtxt: once we are in quantified-constraint land, we
        -- aren't so picky about enforcing H98-language restrictions
        -- E.g. we want to allow a head like Coercible (m a) (m b)


    -- When we are looking at the head of a quantified constraint,
    -- check_quant_pred sets ctxt to SigmaCtxt
    quantified_constraint = case ctxt of
                              SigmaCtxt -> True
                              _         -> False

    head_type_synonym_msg = parens (
                text "All instance types must be of the form (T t1 ... tn)" $$
                text "where T is not a synonym." $$
                text "Use TypeSynonymInstances if you want to disable this.")

    head_type_args_tyvars_msg = parens (vcat [
                text "All instance types must be of the form (T a1 ... an)",
                text "where a1 ... an are *distinct type variables*,",
                text "and each type variable appears at most once in the instance head.",
                text "Use FlexibleInstances if you want to disable this."])

    head_one_type_msg = parens $
                        text "Only one type can be given in an instance head." $$
                        text "Use MultiParamTypeClasses if you want to allow more, or zero."

    rejected_class_msg = text "Class" <+> quotes (ppr clas_nm)
                         <+> text "does not support user-specified instances"
    tuple_class_msg    = text "You can't specify an instance for a tuple constraint"

    gen_inst_err = rejected_class_msg $$ nest 2 (text "(in Safe Haskell)")

    abstract_class_msg = text "Cannot define instance for abstract class"
                         <+> quotes (ppr clas_nm)

    mb_ty_args_msg
      | not (xopt LangExt.TypeSynonymInstances dflags)
      , not (all tcInstHeadTyNotSynonym ty_args)
      = Just head_type_synonym_msg

      | not (xopt LangExt.FlexibleInstances dflags)
      , not (all tcInstHeadTyAppAllTyVars ty_args)
      = Just head_type_args_tyvars_msg

      | length ty_args /= 1
      , not (xopt LangExt.MultiParamTypeClasses dflags)
      , not (xopt LangExt.NullaryTypeClasses dflags && null ty_args)
      = Just head_one_type_msg

      | otherwise
      = Nothing

tcInstHeadTyNotSynonym :: Type -> Bool
-- Used in Haskell-98 mode, for the argument types of an instance head
-- These must not be type synonyms, but everywhere else type synonyms
-- are transparent, so we need a special function here
tcInstHeadTyNotSynonym ty
  = case ty of  -- Do not use splitTyConApp,
                -- because that expands synonyms!
        TyConApp tc _ -> not (isTypeSynonymTyCon tc) || tc == unrestrictedFunTyCon
                -- Allow (->), e.g. instance Category (->),
                -- even though it's a type synonym for FUN 'Omega
        _ -> True

tcInstHeadTyAppAllTyVars :: Type -> Bool
-- Used in Haskell-98 mode, for the argument types of an instance head
-- These must be a constructor applied to type variable arguments
-- or a type-level literal.
-- But we allow
-- 1) kind instantiations
-- 2) the type (->) = FUN 'Omega, even though it's not in this form.
tcInstHeadTyAppAllTyVars ty
  | Just (tc, tys) <- tcSplitTyConApp_maybe (dropCasts ty)
  = let tys' = filterOutInvisibleTypes tc tys  -- avoid kinds
        tys'' | tc == funTyCon, tys_h:tys_t <- tys', tys_h `eqType` omegaDataConTy = tys_t
              | otherwise = tys'
    in ok tys''
  | LitTy _ <- ty = True  -- accept type literals (Trac #13833)
  | otherwise
  = False
  where
        -- Check that all the types are type variables,
        -- and that each is distinct
    ok tys = equalLength tvs tys && hasNoDups tvs
           where
             tvs = mapMaybe tcGetTyVar_maybe tys

dropCasts :: Type -> Type
-- See Note [Casts during validity checking]
-- This function can turn a well-kinded type into an ill-kinded
-- one, so I've kept it local to this module
-- To consider: drop only HoleCo casts
dropCasts (CastTy ty _)     = dropCasts ty
dropCasts (AppTy t1 t2)     = mkAppTy (dropCasts t1) (dropCasts t2)
dropCasts (FunTy w t1 t2)   = mkFunTy w (dropCasts t1) (dropCasts t2)
dropCasts (TyConApp tc tys) = mkTyConApp tc (map dropCasts tys)
dropCasts (ForAllTy b ty)   = ForAllTy (dropCastsB b) (dropCasts ty)
dropCasts ty                = ty  -- LitTy, TyVarTy, CoercionTy

dropCastsB :: TyVarBinder -> TyVarBinder
dropCastsB b = b   -- Don't bother in the kind of a forall

instTypeErr :: Class -> [Type] -> SDoc -> SDoc
instTypeErr cls tys msg
  = hang (hang (text "Illegal instance declaration for")
             2 (quotes (pprClassPred cls tys)))
       2 msg

-- | See Note [Validity checking of HasField instances]
checkHasFieldInst :: Class -> [Type] -> TcM ()
checkHasFieldInst cls tys@[_k_ty, x_ty, r_ty, _a_ty] =
  case splitTyConApp_maybe r_ty of
    Nothing -> whoops (text "Record data type must be specified")
    Just (tc, _)
      | isFamilyTyCon tc
                  -> whoops (text "Record data type may not be a data family")
      | otherwise -> case isStrLitTy x_ty of
       Just lbl
         | isJust (lookupTyConFieldLabel lbl tc)
                     -> whoops (ppr tc <+> text "already has a field"
                                       <+> quotes (ppr lbl))
         | otherwise -> return ()
       Nothing
         | null (tyConFieldLabels tc) -> return ()
         | otherwise -> whoops (ppr tc <+> text "has fields")
  where
    whoops = addErrTc . instTypeErr cls tys
checkHasFieldInst _ tys = pprPanic "checkHasFieldInst" (ppr tys)

{- Note [Casts during validity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the (bogus)
     instance Eq Char#
We elaborate to  'Eq (Char# |> UnivCo(hole))'  where the hole is an
insoluble equality constraint for * ~ #.  We'll report the insoluble
constraint separately, but we don't want to *also* complain that Eq is
not applied to a type constructor.  So we look gaily look through
CastTys here.

Another example:  Eq (Either a).  Then we actually get a cast in
the middle:
   Eq ((Either |> g) a)


Note [Validity checking of HasField instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The HasField class has magic constraint solving behaviour (see Note
[HasField instances] in TcInteract).  However, we permit users to
declare their own instances, provided they do not clash with the
built-in behaviour.  In particular, we forbid:

  1. `HasField _ r _` where r is a variable

  2. `HasField _ (T ...) _` if T is a data family
     (because it might have fields introduced later)

  3. `HasField x (T ...) _` where x is a variable,
      if T has any fields at all

  4. `HasField "foo" (T ...) _` if T has a "foo" field

The usual functional dependency checks also apply.


Note [Valid 'deriving' predicate]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
validDerivPred checks for OK 'deriving' context.  See Note [Exotic
derived instance contexts] in TcDeriv.  However the predicate is
here because it uses sizeTypes, fvTypes.

It checks for three things

  * No repeated variables (hasNoDups fvs)

  * No type constructors.  This is done by comparing
        sizeTypes tys == length (fvTypes tys)
    sizeTypes counts variables and constructors; fvTypes returns variables.
    So if they are the same, there must be no constructors.  But there
    might be applications thus (f (g x)).

    Note that tys only includes the visible arguments of the class type
    constructor. Including the non-visible arguments can cause the following,
    perfectly valid instance to be rejected:
       class Category (cat :: k -> k -> *) where ...
       newtype T (c :: * -> * -> *) a b = MkT (c a b)
       instance Category c => Category (T c) where ...
    since the first argument to Category is a non-visible *, which sizeTypes
    would count as a constructor! See Trac #11833.

  * Also check for a bizarre corner case, when the derived instance decl
    would look like
       instance C a b => D (T a) where ...
    Note that 'b' isn't a parameter of T.  This gives rise to all sorts of
    problems; in particular, it's hard to compare solutions for equality
    when finding the fixpoint, and that means the inferContext loop does
    not converge.  See Trac #5287.

Note [Equality class instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can't have users writing instances for the equality classes. But we
still need to be able to write instances for them ourselves. So we allow
instances only in the defining module.

-}

validDerivPred :: TyVarSet -> PredType -> Bool
-- See Note [Valid 'deriving' predicate]
validDerivPred tv_set pred
  = case classifyPredType pred of
       ClassPred cls tys -> cls `hasKey` typeableClassKey
                -- Typeable constraints are bigger than they appear due
                -- to kind polymorphism, but that's OK
                       || check_tys cls tys
       EqPred {}       -> False  -- reject equality constraints
       _               -> True   -- Non-class predicates are ok
  where
    check_tys cls tys
              = hasNoDups fvs
                   -- use sizePred to ignore implicit args
                && lengthIs fvs (sizePred pred)
                && all (`elemVarSet` tv_set) fvs
      where tys' = filterOutInvisibleTypes (classTyCon cls) tys
            fvs  = fvTypes tys'

{-
************************************************************************
*                                                                      *
\subsection{Checking instance for termination}
*                                                                      *
************************************************************************
-}

{- Note [Instances and constraint synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, we don't allow instances for constraint synonyms at all.
Consider these (Trac #13267):
  type C1 a = Show (a -> Bool)
  instance C1 Int where    -- I1
    show _ = "ur"

This elicits "show is not a (visible) method of class C1", which isn't
a great message. But it comes from the renamer, so it's hard to improve.

This needs a bit more care:
  type C2 a = (Show a, Show Int)
  instance C2 Int           -- I2

If we use (splitTyConApp_maybe tau) in checkValidInstance to decompose
the instance head, we'll expand the synonym on fly, and it'll look like
  instance (%,%) (Show Int, Show Int)
and we /really/ don't want that.  So we carefully do /not/ expand
synonyms, by matching on TyConApp directly.
-}

checkValidInstance :: UserTypeCtxt -> LHsSigType GhcRn -> Type
                   -> TcM ([TyVar], ThetaType, Class, [Type])
checkValidInstance ctxt hs_type ty
  | not is_tc_app
  = failWithTc (hang (text "Instance head is not headed by a class:")
                   2 ( ppr tau))

  | isNothing mb_cls
  = failWithTc (vcat [ text "Illegal instance for a" <+> ppr (tyConFlavour tc)
                     , text "A class instance must be for a class" ])

  | not arity_ok
  = failWithTc (text "Arity mis-match in instance head")

  | otherwise
  = do  { setSrcSpan head_loc $
          checkValidInstHead ctxt clas inst_tys

        ; traceTc "checkValidInstance {" (ppr ty)

        ; env0 <- tcInitTidyEnv
        ; check_valid_theta env0 ctxt theta

        -- The Termination and Coverate Conditions
        -- Check that instance inference will terminate (if we care)
        -- For Haskell 98 this will already have been done by checkValidTheta,
        -- but as we may be using other extensions we need to check.
        --
        -- Note that the Termination Condition is *more conservative* than
        -- the checkAmbiguity test we do on other type signatures
        --   e.g.  Bar a => Bar Int is ambiguous, but it also fails
        --   the termination condition, because 'a' appears more often
        --   in the constraint than in the head
        ; undecidable_ok <- xoptM LangExt.UndecidableInstances
        ; if undecidable_ok
          then checkAmbiguity ctxt ty
          else checkInstTermination theta tau

        ; traceTc "cvi 2" (ppr ty)

        ; case (checkInstCoverage undecidable_ok clas theta inst_tys) of
            IsValid      -> return ()   -- Check succeeded
            NotValid msg -> addErrTc (instTypeErr clas inst_tys msg)

        ; traceTc "End checkValidInstance }" empty

        ; return (tvs, theta, clas, inst_tys) }
  where
    (tvs, theta, tau)    = tcSplitSigmaTy ty
    is_tc_app            = case tau of { TyConApp {} -> True; _ -> False }
    TyConApp tc inst_tys = tau   -- See Note [Instances and constraint synonyms]
    mb_cls               = tyConClass_maybe tc
    Just clas            = mb_cls
    arity_ok             = inst_tys `lengthIs` classArity clas

        -- The location of the "head" of the instance
    head_loc = getLoc (getLHsInstDeclHead hs_type)

{-
Note [Paterson conditions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Termination test: the so-called "Paterson conditions" (see Section 5 of
"Understanding functional dependencies via Constraint Handling Rules,
JFP Jan 2007).

We check that each assertion in the context satisfies:
 (1) no variable has more occurrences in the assertion than in the head, and
 (2) the assertion has fewer constructors and variables (taken together
     and counting repetitions) than the head.
This is only needed with -fglasgow-exts, as Haskell 98 restrictions
(which have already been checked) guarantee termination.

The underlying idea is that

    for any ground substitution, each assertion in the
    context has fewer type constructors than the head.
-}

checkInstTermination :: ThetaType -> TcPredType -> TcM ()
-- See Note [Paterson conditions]
checkInstTermination theta head_pred
  = check_preds emptyVarSet theta
  where
   head_fvs  = fvType head_pred
   head_size = sizeType head_pred

   check_preds :: VarSet -> [PredType] -> TcM ()
   check_preds foralld_tvs preds = mapM_ (check foralld_tvs) preds

   check :: VarSet -> PredType -> TcM ()
   check foralld_tvs pred
     = case classifyPredType pred of
         EqPred {}    -> return ()  -- See Trac #4200.
         IrredPred {} -> check2 foralld_tvs pred (sizeType pred)
         ClassPred cls tys
           | isTerminatingClass cls
           -> return ()

           | isCTupleClass cls  -- Look inside tuple predicates; Trac #8359
           -> check_preds foralld_tvs tys

           | otherwise          -- Other ClassPreds
           -> check2 foralld_tvs pred bogus_size
           where
              bogus_size = 1 + sizeTypes (filterOutInvisibleTypes (classTyCon cls) tys)
                               -- See Note [Invisible arguments and termination]

         ForAllPred tvs _ head_pred'
           -> check (foralld_tvs `extendVarSetList` binderVars tvs) head_pred'
              -- Termination of the quantified predicate itself is checked
              -- when the predicates are individually checked for validity

   check2 foralld_tvs pred pred_size
     | not (null bad_tvs)     = failWithTc (noMoreMsg bad_tvs what (ppr head_pred))
     | not (isTyFamFree pred) = failWithTc (nestedMsg what)
     | pred_size >= head_size = failWithTc (smallerMsg what (ppr head_pred))
     | otherwise              = return ()
     -- isTyFamFree: see Note [Type families in instance contexts]
     where
        what    = text "constraint" <+> quotes (ppr pred)
        bad_tvs = filterOut (`elemVarSet` foralld_tvs) (fvType pred)
                  \\ head_fvs

smallerMsg :: SDoc -> SDoc -> SDoc
smallerMsg what inst_head
  = vcat [ hang (text "The" <+> what)
              2 (sep [ text "is no smaller than"
                     , text "the instance head" <+> quotes inst_head ])
         , parens undecidableMsg ]

noMoreMsg :: [TcTyVar] -> SDoc -> SDoc -> SDoc
noMoreMsg tvs what inst_head
  = vcat [ hang (text "Variable" <> plural tvs1 <+> quotes (pprWithCommas ppr tvs1)
                <+> occurs <+> text "more often")
              2 (sep [ text "in the" <+> what
                     , text "than in the instance head" <+> quotes inst_head ])
         , parens undecidableMsg ]
  where
   tvs1   = nub tvs
   occurs = if isSingleton tvs1 then text "occurs"
                               else text "occur"

undecidableMsg, constraintKindsMsg :: SDoc
undecidableMsg     = text "Use UndecidableInstances to permit this"
constraintKindsMsg = text "Use ConstraintKinds to permit this"

{- Note [Type families in instance contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Are these OK?
  type family F a
  instance F a    => C (Maybe [a]) where ...
  intance C (F a) => C [[[a]]]     where ...

No: the type family in the instance head might blow up to an
arbitrarily large type, depending on how 'a' is instantiated.
So we require UndecidableInstances if we have a type family
in the instance head.  Trac #15172.

Note [Associated type instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We allow this:
  class C a where
    type T x a
  instance C Int where
    type T (S y) Int = y
    type T Z     Int = Char

Note that
  a) The variable 'x' is not bound by the class decl
  b) 'x' is instantiated to a non-type-variable in the instance
  c) There are several type instance decls for T in the instance

All this is fine.  Of course, you can't give any *more* instances
for (T ty Int) elsewhere, because it's an *associated* type.

Note [Checking consistent instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #11450 for background discussion on this check.

  class C a b where
    type T a x b

With this class decl, if we have an instance decl
  instance C ty1 ty2 where ...
then the type instance must look like
     type T ty1 v ty2 = ...
with exactly 'ty1' for 'a', 'ty2' for 'b', and some type 'v' for 'x'.
For example:

  instance C [p] Int
    type T [p] y Int = (p,y,y)

Note that

* We used to allow completely different bound variables in the
  associated type instance; e.g.
    instance C [p] Int
      type T [q] y Int = ...
  But from GHC 8.2 onwards, we don't.  It's much simpler this way.
  See Trac #11450.

* When the class variable isn't used on the RHS of the type instance,
  it's tempting to allow wildcards, thus
    instance C [p] Int
      type T [_] y Int = (y,y)
  But it's awkward to do the test, and it doesn't work if the
  variable is repeated:
    instance C (p,p) Int
      type T (_,_) y Int = (y,y)
  Even though 'p' is not used on the RHS, we still need to use 'p'
  on the LHS to establish the repeated pattern.  So to keep it simple
  we just require equality.

* For variables in associated type families that are not bound by the class
  itself, we do _not_ check if they are over-specific. In other words,
  it's perfectly acceptable to have an instance like this:

    instance C [p] Int where
      type T [p] (Maybe x) Int = x

  While the first and third arguments to T are required to be exactly [p] and
  Int, respectively, since they are bound by C, the second argument is allowed
  to be more specific than just a type variable. Furthermore, it is permissible
  to define multiple equations for T that differ only in the non-class-bound
  argument:

    instance C [p] Int where
      type T [p] (Maybe x)    Int = x
      type T [p] (Either x y) Int = x -> y

  We once considered requiring that non-class-bound variables in associated
  type family instances be instantiated with distinct type variables. However,
  that requirement proved too restrictive in practice, as there were examples
  of extremely simple associated type family instances that this check would
  reject, and fixing them required tiresome boilerplate in the form of
  auxiliary type families. For instance, you would have to define the above
  example as:

    instance C [p] Int where
      type T [p] x Int = CAux x

    type family CAux x where
      CAux (Maybe x)    = x
      CAux (Either x y) = x -> y

  We decided that this restriction wasn't buying us much, so we opted not
  to pursue that design (see also GHC Trac #13398).

Implementation
  * Form the mini-envt from the class type variables a,b
    to the instance decl types [p],Int:   [a->[p], b->Int]

  * Look at the tyvars a,x,b of the type family constructor T
    (it shares tyvars with the class C)

  * Apply the mini-evnt to them, and check that the result is
    consistent with the instance types [p] y Int. (where y can be any type, as
    it is not scoped over the class type variables.

We make all the instance type variables scope over the
type instances, of course, which picks up non-obvious kinds.  Eg
   class Foo (a :: k) where
      type F a
   instance Foo (b :: k -> k) where
      type F b = Int
Here the instance is kind-indexed and really looks like
      type F (k->k) (b::k->k) = Int
But if the 'b' didn't scope, we would make F's instance too
poly-kinded.

Note [Invisible arguments and termination]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking the ​Paterson conditions for termination an instance
declaration, we check for the number of "constructors and variables"
in the instance head and constraints. Question: Do we look at

 * All the arguments, visible or invisible?
 * Just the visible arguments?

I think both will ensure termination, provided we are consistent.
Currently we are /not/ consistent, which is really a bug.  It's
described in Trac #15177, which contains a number of examples.
The suspicious bits are the calls to filterOutInvisibleTypes.
-}

-- | Extra information about the parent instance declaration, needed
-- when type-checking associated types. The 'Class' is the enclosing
-- class, the [TyVar] are the type variable of the instance decl,
-- and and the @VarEnv Type@ maps class variables to their instance
-- types.
type ClsInstInfo = (Class, [TyVar], VarEnv Type)

type AssocInstArgShape = (Maybe Type, Type)
  -- AssocInstArgShape is used only for associated family instances
  --    (mb_exp, actual)
  -- mb_exp = Just ty  => this arg corresponds to a class variable
  --        = Nothing  => it doesn't correspond to a class variable
  -- e.g.  class C b where
  --          type F a b c
  --       instance C [x] where
  --          type F p [x] q
  -- We get [AssocInstArgShape] = [ (Nothing,  p)
  --                              , (Just [x], [x])
  --                              , (Nothing,  q)]

checkConsistentFamInst
               :: Maybe ClsInstInfo
               -> TyCon              -- ^ Family tycon
               -> [Type]             -- ^ Type patterns from instance
               -> SDoc               -- ^ pretty-printed user-written instance head
               -> TcM ()
-- See Note [Checking consistent instantiation]

checkConsistentFamInst Nothing _ _ _ = return ()
checkConsistentFamInst (Just (clas, inst_tvs, mini_env)) fam_tc at_tys pp_hs_pats
  = do { -- Check that the associated type indeed comes from this class
         -- See [Mismatched class methods and associated type families]
         -- in TcInstDecls.

         checkTc (Just (classTyCon clas) == tyConAssoc_maybe fam_tc)
                 (badATErr (className clas) (tyConName fam_tc))

       -- Check type args first (more comprehensible)
       ; checkTc (all check_arg type_shapes)   pp_wrong_at_arg

       -- And now kind args
       ; checkTcM (all check_arg kind_shapes)
                  (tidy_env2, pprWithExplicitKindsWhen True pp_wrong_at_arg)

       ; traceTc "checkConsistentFamInst" (vcat [ ppr inst_tvs
                                                , ppr arg_shapes
                                                , ppr mini_env ]) }
  where
    arg_shapes :: [AssocInstArgShape]
    arg_shapes = [ (lookupVarEnv mini_env fam_tc_tv, at_ty)
                 | (fam_tc_tv, at_ty) <- tyConTyVars fam_tc `zip` at_tys ]

    kind_shapes, type_shapes :: [AssocInstArgShape]
    (kind_shapes, type_shapes) = partitionInvisibles $
                                 arg_shapes `zip` tyConArgFlags fam_tc at_tys

    check_arg :: AssocInstArgShape -> Bool
    check_arg (Just exp_ty, at_ty) = exp_ty `tcEqType` at_ty
    check_arg (Nothing,     _    ) = True -- Arg position does not correspond
                                          -- to a class variable

    pp_wrong_at_arg
      = vcat [ text "Type indexes must match class instance head"
             , pp_exp_act ]

    pp_exp_act
      = vcat [ text "Expected:" <+> ppr (mkTyConApp fam_tc expected_args)
             , text "  Actual:" <+> pp_hs_pats
             , sdocWithDynFlags $ \dflags ->
               ppWhen (has_poly_args dflags) $
               vcat [ text "where the `<tv>' arguments are type variables,"
                    , text "distinct from each other and from the instance variables" ] ]

    -- We need to tidy, since it's possible that expected_args will contain
    -- inferred kind variables with names identical to those in at_tys. If we
    -- don't, we'll end up with horrible messages like this one (#13972):
    --
    --   Expected: T (a -> Either a b)
    --     Actual: T (a -> Either a b)
    (tidy_env1, _) = tidyOpenTypes emptyTidyEnv at_tys
    (tidy_env2, expected_args)
      = tidyOpenTypes tidy_env1 [ exp_ty `orElse` mk_tv at_ty
                                | (exp_ty, at_ty) <- arg_shapes ]
    mk_tv at_ty   = mkTyVarTy (mkTyVar tv_name (typeKind at_ty))
    tv_name = mkInternalName (mkAlphaTyVarUnique 1) (mkTyVarOcc "<tv>") noSrcSpan

    has_poly_args dflags = any (isNothing . fst) shapes
      where
        shapes | gopt Opt_PrintExplicitKinds dflags = arg_shapes
               | otherwise                          = type_shapes

badATErr :: Name -> Name -> SDoc
badATErr clas op
  = hsep [text "Class", quotes (ppr clas),
          text "does not have an associated type", quotes (ppr op)]


{-
************************************************************************
*                                                                      *
        Checking type instance well-formedness and termination
*                                                                      *
************************************************************************
-}

checkValidCoAxiom :: CoAxiom Branched -> TcM ()
checkValidCoAxiom ax@(CoAxiom { co_ax_tc = fam_tc, co_ax_branches = branches })
  = do { mapM_ (checkValidCoAxBranch Nothing fam_tc) branch_list
       ; foldlM_ check_branch_compat [] branch_list }
  where
    branch_list = fromBranches branches
    injectivity = tyConInjectivityInfo fam_tc

    check_branch_compat :: [CoAxBranch]    -- previous branches in reverse order
                        -> CoAxBranch      -- current branch
                        -> TcM [CoAxBranch]-- current branch : previous branches
    -- Check for
    --   (a) this branch is dominated by previous ones
    --   (b) failure of injectivity
    check_branch_compat prev_branches cur_branch
      | cur_branch `isDominatedBy` prev_branches
      = do { addWarnAt NoReason (coAxBranchSpan cur_branch) $
             inaccessibleCoAxBranch ax cur_branch
           ; return prev_branches }
      | otherwise
      = do { check_injectivity prev_branches cur_branch
           ; return (cur_branch : prev_branches) }

     -- Injectivity check: check whether a new (CoAxBranch) can extend
     -- already checked equations without violating injectivity
     -- annotation supplied by the user.
     -- See Note [Verifying injectivity annotation] in FamInstEnv
    check_injectivity prev_branches cur_branch
      | Injective inj <- injectivity
      = do { let conflicts =
                     fst $ foldl' (gather_conflicts inj prev_branches cur_branch)
                                 ([], 0) prev_branches
           ; mapM_ (\(err, span) -> setSrcSpan span $ addErr err)
                   (makeInjectivityErrors ax cur_branch inj conflicts) }
      | otherwise
      = return ()

    gather_conflicts inj prev_branches cur_branch (acc, n) branch
               -- n is 0-based index of branch in prev_branches
      = case injectiveBranches inj cur_branch branch of
          InjectivityUnified ax1 ax2
            | ax1 `isDominatedBy` (replace_br prev_branches n ax2)
                -> (acc, n + 1)
            | otherwise
                -> (branch : acc, n + 1)
          InjectivityAccepted -> (acc, n + 1)

    -- Replace n-th element in the list. Assumes 0-based indexing.
    replace_br :: [CoAxBranch] -> Int -> CoAxBranch -> [CoAxBranch]
    replace_br brs n br = take n brs ++ [br] ++ drop (n+1) brs


-- Check that a "type instance" is well-formed (which includes decidability
-- unless -XUndecidableInstances is given).
--
checkValidCoAxBranch :: Maybe ClsInstInfo
                     -> TyCon -> CoAxBranch -> TcM ()
checkValidCoAxBranch mb_clsinfo fam_tc
                    (CoAxBranch { cab_tvs = tvs, cab_cvs = cvs
                                , cab_lhs = typats
                                , cab_rhs = rhs, cab_loc = loc })
  = checkValidTyFamEqn mb_clsinfo fam_tc tvs cvs typats rhs pp_lhs loc
  where
    pp_lhs = ppr (mkTyConApp fam_tc typats)

-- | Do validity checks on a type family equation, including consistency
-- with any enclosing class instance head, termination, and lack of
-- polytypes.
checkValidTyFamEqn :: Maybe ClsInstInfo
                   -> TyCon   -- ^ of the type family
                   -> [TyVar] -- ^ bound tyvars in the equation
                   -> [CoVar] -- ^ bound covars in the equation
                   -> [Type]  -- ^ type patterns
                   -> Type    -- ^ rhs
                   -> SDoc    -- ^ user-written LHS
                   -> SrcSpan
                   -> TcM ()
checkValidTyFamEqn mb_clsinfo fam_tc tvs cvs typats rhs pp_lhs loc
  = setSrcSpan loc $
    do { checkValidFamPats mb_clsinfo fam_tc tvs cvs typats [] pp_lhs

         -- The argument patterns, and RHS, are all boxed tau types
         -- E.g  Reject type family F (a :: k1) :: k2
         --             type instance F (forall a. a->a) = ...
         --             type instance F Int#             = ...
         --             type instance F Int              = forall a. a->a
         --             type instance F Int              = Int#
         -- See Trac #9357
       ; checkValidMonoType rhs

         -- We have a decidable instance unless otherwise permitted
       ; undecidable_ok <- xoptM LangExt.UndecidableInstances
       ; traceTc "checkVTFE" (pp_lhs $$ ppr rhs $$ ppr (tcTyFamInsts rhs))
       ; unless undecidable_ok $
           mapM_ addErrTc (checkFamInstRhs fam_tc typats (tcTyFamInsts rhs)) }

-- Make sure that each type family application is
--   (1) strictly smaller than the lhs,
--   (2) mentions no type variable more often than the lhs, and
--   (3) does not contain any further type family instances.
--
checkFamInstRhs :: TyCon -> [Type]         -- LHS
                -> [(TyCon, [Type])]       -- type family calls in RHS
                -> [MsgDoc]
checkFamInstRhs lhs_tc lhs_tys famInsts
  = mapMaybe check famInsts
  where
   lhs_size  = sizeTyConAppArgs lhs_tc lhs_tys
   inst_head = pprType (TyConApp lhs_tc lhs_tys)
   lhs_fvs   = fvTypes lhs_tys
   check (tc, tys)
      | not (all isTyFamFree tys) = Just (nestedMsg what)
      | not (null bad_tvs)        = Just (noMoreMsg bad_tvs what inst_head)
      | lhs_size <= fam_app_size  = Just (smallerMsg what inst_head)
      | otherwise                 = Nothing
      where
        what = text "type family application"
               <+> quotes (pprType (TyConApp tc tys))
        fam_app_size = sizeTyConAppArgs tc tys
        bad_tvs      = fvTypes tys \\ lhs_fvs
                       -- The (\\) is list difference; e.g.
                       --   [a,b,a,a] \\ [a,a] = [b,a]
                       -- So we are counting repetitions

checkValidFamPats :: Maybe ClsInstInfo -> TyCon -> [TyVar] -> [CoVar]
                  -> [Type]   -- ^ patterns the user wrote
                  -> [Type]   -- ^ "extra" patterns from a data instance kind sig
                  -> SDoc     -- ^ pretty-printed user-written instance head
                  -> TcM ()
-- Patterns in a 'type instance' or 'data instance' decl should
-- a) contain no type family applications
--    (vanilla synonyms are fine, though)
-- b) properly bind all their free type variables
--    e.g. we disallow (Trac #7536)
--         type T a = Int
--         type instance F (T a) = a
-- c) For associated types, are consistently instantiated
checkValidFamPats mb_clsinfo fam_tc tvs cvs user_ty_pats extra_ty_pats pp_hs_pats
  = do { checkValidTypePats fam_tc user_ty_pats

       ; let unbound_tcvs = filterOut (`elemVarSet` exactTyCoVarsOfTypes user_ty_pats)
                                      (tvs ++ cvs)
       ; checkTc (null unbound_tcvs) (famPatErr fam_tc unbound_tcvs user_ty_pats)

         -- Check that type patterns match the class instance head
       ; checkConsistentFamInst mb_clsinfo fam_tc (user_ty_pats `chkAppend` extra_ty_pats) pp_hs_pats }

-- | Checks for occurrences of type families in class instances and type/data
-- family instances.
checkValidTypePats :: TyCon -> [Type] -> TcM ()
checkValidTypePats tc pat_ty_args = do
  -- Check that each of pat_ty_args is a monotype.
  -- One could imagine generalising to allow
  --      instance C (forall a. a->a)
  -- but we don't know what all the consequences might be.
  traverse_ checkValidMonoType pat_ty_args

  -- Ensure that no type family instances occur a type pattern
  case tcTyConAppTyFamInstsAndVis tc pat_ty_args of
    [] -> pure ()
    ((tf_is_invis_arg, tf_tc, tf_args):_) -> failWithTc $
      ty_fam_inst_illegal_err tf_is_invis_arg (mkTyConApp tf_tc tf_args)
  where
    inst_ty = mkTyConApp tc pat_ty_args

    ty_fam_inst_illegal_err :: Bool -> Type -> SDoc
    ty_fam_inst_illegal_err invis_arg ty
      = pprWithExplicitKindsWhen invis_arg $
        hang (text "Illegal type synonym family application"
                <+> quotes (ppr ty) <+> text "in instance" <> colon)
           2 (ppr inst_ty)

-- Error messages

inaccessibleCoAxBranch :: CoAxiom br -> CoAxBranch -> SDoc
inaccessibleCoAxBranch fi_ax cur_branch
  = text "Type family instance equation is overlapped:" $$
    nest 2 (pprCoAxBranch fi_ax cur_branch)

nestedMsg :: SDoc -> SDoc
nestedMsg what
  = sep [ text "Illegal nested" <+> what
        , parens undecidableMsg ]

famPatErr :: TyCon -> [TyVar] -> [Type] -> SDoc
famPatErr fam_tc tvs pats
  = hang (text "Family instance purports to bind type variable" <> plural tvs
          <+> pprQuotedList tvs)
       2 (hang (text "but the real LHS (expanding synonyms) is:")
             2 (pprTypeApp fam_tc (map expandTypeSynonyms pats) <+>
                text "= ..."))

{-
************************************************************************
*                                                                      *
   Telescope checking
*                                                                      *
************************************************************************

Note [Bad telescopes]
~~~~~~~~~~~~~~~~~~~~~
Now that we can mix type and kind variables, there are an awful lot of
ways to shoot yourself in the foot. Here are some.

  data SameKind :: k -> k -> *   -- just to force unification

1.  data T1 a k (b :: k) (x :: SameKind a b)

The problem here is that we discover that a and b should have the same
kind. But this kind mentions k, which is bound *after* a.
(Testcase: dependent/should_fail/BadTelescope)

2.  data T2 a (c :: Proxy b) (d :: Proxy a) (x :: SameKind b d)

Note that b is not bound. Yet its kind mentions a. Because we have
a nice rule that all implicitly bound variables come before others,
this is bogus.

To catch these errors, we call checkValidTelescope during kind-checking
datatype declarations. See also
Note [Required, Specified, and Inferred for types] in TcTyClsDecls.

Note [Keeping scoped variables in order: Explicit] discusses how this
check works for `forall x y z.` written in a type.

-}

-- | Check a list of binders to see if they make a valid telescope.
-- The key property we're checking for is scoping. For example:
-- > data SameKind :: k -> k -> *
-- > data X a k (b :: k) (c :: SameKind a b)
-- Kind inference says that a's kind should be k. But that's impossible,
-- because k isn't in scope when a is bound. This check has to come before
-- general validity checking, because once we kind-generalise, this sort
-- of problem is harder to spot (as we'll generalise over the unbound
-- k in a's type.)
--
-- See Note [Generalisation for type constructors] in TcTyClsDecls for
--     data type declarations
-- and Note [Keeping scoped variables in order: Explicit] in TcHsType
--     for foralls
checkValidTelescope :: [TyConBinder]   -- explicit vars (zonked)
                    -> SDoc            -- original, user-written telescope
                    -> TcM ()
checkValidTelescope tvbs user_tyvars
  = unless (null bad_tvbs) $ addErr $
    vcat [ hang (text "These kind and type variables:" <+> user_tyvars $$
                 text "are out of dependency order. Perhaps try this ordering:")
              2 (pprTyVars sorted_tidied_tvs)
         , extra ]
  where
    tvs = binderVars tvbs
    (_, sorted_tidied_tvs) = tidyVarBndrs emptyTidyEnv (scopedSort tvs)

    (_, bad_tvbs) = foldl add_one (mkVarSet tvs, []) tvbs

    add_one :: (TyVarSet, [TyConBinder])
            -> TyConBinder
            -> (TyVarSet, [TyConBinder])
    add_one (bad_bndrs, acc) tvb
      | fkvs `intersectsVarSet` bad_bndrs
      = (bad', tvb : acc)
      | otherwise
      = (bad', acc)
      where
        tv = binderVar tvb
        fkvs = tyCoVarsOfType (tyVarKind tv)
        bad' = bad_bndrs `delVarSet` tv

    extra
      | any isInvisibleTyConBinder tvbs
      = text "NB: Implicitly declared variables come before others."
      | otherwise
      = empty

{-
************************************************************************
*                                                                      *
\subsection{Auxiliary functions}
*                                                                      *
************************************************************************
-}

-- Free variables of a type, retaining repetitions, and expanding synonyms
-- This ignores coercions, as coercions aren't user-written
fvType :: Type -> [TyCoVar]
fvType ty | Just exp_ty <- tcView ty = fvType exp_ty
fvType (TyVarTy tv)          = [tv]
fvType (TyConApp _ tys)      = fvTypes tys
fvType (LitTy {})            = []
fvType (AppTy fun arg)       = fvType fun ++ fvType arg
fvType (FunTy _ arg res)     = fvType arg ++ fvType res
fvType (ForAllTy (Bndr tv _) ty)
  = fvType (tyVarKind tv) ++
    filter (/= tv) (fvType ty)
fvType (CastTy ty _)         = fvType ty
fvType (CoercionTy {})       = []

fvTypes :: [Type] -> [TyVar]
fvTypes tys                = concat (map fvType tys)

sizeType :: Type -> Int
-- Size of a type: the number of variables and constructors
sizeType ty | Just exp_ty <- tcView ty = sizeType exp_ty
sizeType (TyVarTy {})      = 1
sizeType (TyConApp tc tys) = 1 + sizeTyConAppArgs tc tys
sizeType (LitTy {})        = 1
sizeType (AppTy fun arg)   = sizeType fun + sizeType arg
sizeType (FunTy _ arg res) = sizeType arg + sizeType res + 1
sizeType (ForAllTy _ ty)   = sizeType ty
sizeType (CastTy ty _)     = sizeType ty
sizeType (CoercionTy _)    = 0

sizeTypes :: [Type] -> Int
sizeTypes = foldr ((+) . sizeType) 0

sizeTyConAppArgs :: TyCon -> [Type] -> Int
sizeTyConAppArgs _tc tys = sizeTypes tys -- (filterOutInvisibleTypes tc tys)
                           -- See Note [Invisible arguments and termination]

-- Size of a predicate
--
-- We are considering whether class constraints terminate.
-- Equality constraints and constraints for the implicit
-- parameter class always terminate so it is safe to say "size 0".
-- See Trac #4200.
sizePred :: PredType -> Int
sizePred ty = goClass ty
  where
    goClass p = go (classifyPredType p)

    go (ClassPred cls tys')
      | isTerminatingClass cls = 0
      | otherwise = sizeTypes (filterOutInvisibleTypes (classTyCon cls) tys')
                    -- The filtering looks bogus
                    -- See Note [Invisible arguments and termination]
    go (EqPred {})           = 0
    go (IrredPred ty)        = sizeType ty
    go (ForAllPred _ _ pred) = goClass pred

-- | When this says "True", ignore this class constraint during
-- a termination check
isTerminatingClass :: Class -> Bool
isTerminatingClass cls
  = isIPClass cls    -- Implicit parameter constraints always terminate because
                     -- there are no instances for them --- they are only solved
                     -- by "local instances" in expressions
    || cls `hasKey` typeableClassKey
    || cls `hasKey` coercibleTyConKey
    || cls `hasKey` eqTyConKey
    || cls `hasKey` heqTyConKey

-- | Tidy before printing a type
ppr_tidy :: TidyEnv -> Type -> SDoc
ppr_tidy env ty = pprType (tidyType env ty)

allDistinctTyVars :: TyVarSet -> [KindOrType] -> Bool
-- (allDistinctTyVars tvs tys) returns True if tys are
-- a) all tyvars
-- b) all distinct
-- c) disjoint from tvs
allDistinctTyVars _    [] = True
allDistinctTyVars tkvs (ty : tys)
  = case getTyVar_maybe ty of
      Nothing -> False
      Just tv | tv `elemVarSet` tkvs -> False
              | otherwise -> allDistinctTyVars (tkvs `extendVarSet` tv) tys
