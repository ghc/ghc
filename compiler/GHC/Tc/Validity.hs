{-# LANGUAGE CPP           #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Tc.Validity (
  Rank, UserTypeCtxt(..), checkValidType, checkValidMonoType,
  checkValidTheta,
  checkValidInstance, checkValidInstHead, validDerivPred,
  checkTySynRhs,
  checkValidCoAxiom, checkValidCoAxBranch,
  checkValidTyFamEqn, checkValidAssocTyFamDeflt, checkConsistentFamInst,
  badATErr, arityErr,
  checkTyConTelescope,
  allDistinctTyVars
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Data.Maybe

-- friends:
import GHC.Tc.Utils.Unify    ( tcSubTypeSigma )
import GHC.Tc.Solver         ( simplifyAmbiguityCheck )
import GHC.Tc.Instance.Class ( matchGlobalInst, ClsInstResult(..), InstanceWhat(..), AssocInstInfo(..) )
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr
import GHC.Tc.Utils.TcType hiding ( sizeType, sizeTypes )
import GHC.Builtin.Types ( heqTyConName, eqTyConName, coercibleTyConName, manyDataConTy )
import GHC.Builtin.Names
import GHC.Core.Type
import GHC.Core.Unify ( tcMatchTyX_BM, BindFlag(..) )
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.Predicate
import GHC.Tc.Types.Origin

-- others:
import GHC.Iface.Type    ( pprIfaceType, pprIfaceTypeApp )
import GHC.CoreToIface   ( toIfaceTyCon, toIfaceTcArgs, toIfaceType )
import GHC.Hs
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env  ( tcInitTidyEnv, tcInitOpenTidyEnv )
import GHC.Tc.Instance.FunDeps
import GHC.Core.FamInstEnv
   ( isDominatedBy, injectiveBranches, InjectivityCheckResult(..) )
import GHC.Tc.Instance.Family
import GHC.Types.Name
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Var     ( VarBndr(..), mkTyVar )
import GHC.Utils.FV
import GHC.Utils.Error
import GHC.Driver.Session
import GHC.Utils.Misc
import GHC.Data.List.SetOps
import GHC.Types.SrcLoc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Builtin.Uniques  ( mkAlphaTyVarUnique )
import GHC.Data.Bag      ( emptyBag )
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.Foldable
import Data.Function
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
the originals, so in the case of an ambiguous type it won't work.
Consider our earlier example f :: C a => Int.  Then in g's definition,
we'll instantiate to (C alpha) and try to deduce (C alpha) from (C a),
and fail.

So in fact we use this as our *definition* of ambiguity.  We use a
very similar test for *inferred* types, to ensure that they are
unambiguous. See Note [Impedance matching] in GHC.Tc.Gen.Bind.

This test is very conveniently implemented by calling
    tcSubType <type> <type>
This neatly takes account of the functional dependency stuff above,
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
    messages (see #10432).

To avoid this, we call checkAmbiguity once, at the top, in checkValidType.
(I'm still a bit worried about unbound skolems when the type mentions
in-scope type variables.)

In fact, because of the co/contra-variance implemented in tcSubType,
this *does* catch function f above. too.

Concerning (a) the ambiguity check is only used for *user* types, not
for types coming from interface files.  The latter can legitimately
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
"tau-tvs" stuff.  This is done in the function 'GHC.Tc.Instance.FunDeps.grow'.
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
                            tcSubTypeSigma ctxt ty ty
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
      GhciCtxt {}  -> False
      TySynCtxt {} -> False
      TypeAppCtxt  -> False
      StandaloneKindSigCtxt{} -> False
      _            -> True

checkUserTypeError :: Type -> TcM ()
-- Check to see if the type signature mentions "TypeError blah"
-- anywhere in it, and fail if so.
--
-- Very unsatisfactorily (#11144) we need to tidy the type
-- because it may have come from an /inferred/ signature, not a
-- user-supplied one.  This is really only a half-baked fix;
-- the other errors in checkValidType don't do tidying, and so
-- may give bad error messages when given an inferred type.
checkUserTypeError = check
  where
  check ty
    | Just msg     <- userTypeError_maybe ty      = fail_with msg
    | Just (_,ts)  <- splitTyConApp_maybe ty      = mapM_ check ts
    | Just (t1,t2) <- splitAppTy_maybe ty         = check t1 >> check t2
    | Just (_,t1)  <- splitForAllTyCoVar_maybe ty = check t1
    | otherwise                                   = return ()

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

  There is also an implementation reason (#11608).  In the RHS of
  a type synonym we don't (currently) instantiate 'a' and 'b' with
  TcTyVars before calling checkValidType, so we get assertion failures
  from doing an ambiguity check on a type with TyVars in it.  Fixing this
  would not be hard, but let's wait till there's a reason.

* TypeAppCtxt: visible type application
     f @ty
  No need to check ty for ambiguity

* StandaloneKindSigCtxt: type T :: ksig
  Kinds need a different ambiguity check than types, and the currently
  implemented check is only good for types. See #14419, in particular
  https://gitlab.haskell.org/ghc/ghc/issues/14419#note_160844

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
  = do { traceTc "checkValidType" (ppr ty <+> text "::" <+> ppr (tcTypeKind ty))
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
                 PatSigCtxt     -> rank0
                 RuleSigCtxt _  -> rank1
                 TySynCtxt _    -> rank0

                 ExprSigCtxt    -> rank1
                 KindSigCtxt    -> rank1
                 StandaloneKindSigCtxt{} -> rank1
                 TypeAppCtxt | impred_flag -> ArbitraryRank
                             | otherwise   -> tyConArgMonoType
                    -- Normally, ImpredicativeTypes is handled in check_arg_type,
                    -- but visible type applications don't go through there.
                    -- So we do this check here.

                 FunSigCtxt {}  -> rank1
                 InfSigCtxt {}  -> rank1 -- Inferred types should obey the
                                         -- same rules as declared ones

                 ConArgCtxt _   -> rank1 -- We are given the type of the entire
                                         -- constructor, hence rank 1
                 PatSynCtxt _   -> rank1

                 ForSigCtxt _   -> rank1
                 SpecInstCtxt   -> rank1
                 GhciCtxt {}    -> ArbitraryRank

                 TyVarBndrKindCtxt _ -> rank0
                 DataKindCtxt _      -> rank1
                 TySynKindCtxt _     -> rank1
                 TyFamResKindCtxt _  -> rank1

                 _              -> panic "checkValidType"
                                          -- Can't happen; not used for *user* sigs

       ; env <- tcInitOpenTidyEnv (tyCoVarsOfTypeList ty)
       ; expand <- initialExpandMode
       ; let ve = ValidityEnv{ ve_tidy_env = env, ve_ctxt = ctxt
                             , ve_rank = rank, ve_expand = expand }

       -- Check the internal validity of the type itself
       -- Fail if bad things happen, else we misleading
       -- (and more complicated) errors in checkAmbiguity
       ; checkNoErrs $
         do { check_type ve ty
            ; checkUserTypeError ty
            ; traceTc "done ct" (ppr ty) }

       -- Check for ambiguous types.  See Note [When to call checkAmbiguity]
       -- NB: this will happen even for monotypes, but that should be cheap;
       --     and there may be nested foralls for the subtype test to examine
       ; checkAmbiguity ctxt ty

       ; traceTc "checkValidType done" (ppr ty <+> text "::" <+> ppr (tcTypeKind ty)) }

checkValidMonoType :: Type -> TcM ()
-- Assumes argument is fully zonked
checkValidMonoType ty
  = do { env <- tcInitOpenTidyEnv (tyCoVarsOfTypeList ty)
       ; expand <- initialExpandMode
       ; let ve = ValidityEnv{ ve_tidy_env = env, ve_ctxt = SigmaCtxt
                             , ve_rank = MustBeMonoType, ve_expand = expand }
       ; check_type ve ty }

checkTySynRhs :: UserTypeCtxt -> TcType -> TcM ()
checkTySynRhs ctxt ty
  | tcReturnsConstraintKind actual_kind
  = do { ck <- xoptM LangExt.ConstraintKinds
       ; if ck
         then  when (tcIsConstraintKind actual_kind)
                    (do { dflags <- getDynFlags
                        ; expand <- initialExpandMode
                        ; check_pred_ty emptyTidyEnv dflags ctxt expand ty })
         else addErrTcM (constraintSynErr emptyTidyEnv actual_kind) }

  | otherwise
  = return ()
  where
    actual_kind = tcTypeKind ty

{-
Note [Higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~
Technically
            Int -> forall a. a->a
is still a rank-1 type, but it's not Haskell 98 (#5957).  So the
validity checker allow a forall after an arrow only if we allow it
before -- that is, with Rank2Types or RankNTypes
-}

data Rank = ArbitraryRank         -- Any rank ok

          | LimitedRank   -- Note [Higher rank types]
                 Bool     -- Forall ok at top
                 Rank     -- Use for function arguments

          | MonoType SDoc   -- Monotype, with a suggestion of how it could be a polytype

          | MustBeMonoType  -- Monotype regardless of flags

instance Outputable Rank where
  ppr ArbitraryRank  = text "ArbitraryRank"
  ppr (LimitedRank top_forall_ok r)
                     = text "LimitedRank" <+> ppr top_forall_ok
                                          <+> parens (ppr r)
  ppr (MonoType msg) = text "MonoType" <+> parens msg
  ppr MustBeMonoType = text "MustBeMonoType"

rankZeroMonoType, tyConArgMonoType, synArgMonoType, constraintMonoType :: Rank
rankZeroMonoType   = MonoType (text "Perhaps you intended to use RankNTypes")
tyConArgMonoType   = MonoType (text "Perhaps you intended to use ImpredicativeTypes")
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

-- | Indicates whether a 'UserTypeCtxt' represents type-level contexts,
-- kind-level contexts, or both.
data TypeOrKindCtxt
  = OnlyTypeCtxt
    -- ^ A 'UserTypeCtxt' that only represents type-level positions.
  | OnlyKindCtxt
    -- ^ A 'UserTypeCtxt' that only represents kind-level positions.
  | BothTypeAndKindCtxt
    -- ^ A 'UserTypeCtxt' that can represent both type- and kind-level positions.
  deriving Eq

instance Outputable TypeOrKindCtxt where
  ppr ctxt = text $ case ctxt of
    OnlyTypeCtxt        -> "OnlyTypeCtxt"
    OnlyKindCtxt        -> "OnlyKindCtxt"
    BothTypeAndKindCtxt -> "BothTypeAndKindCtxt"

-- | Determine whether a 'UserTypeCtxt' can represent type-level contexts,
-- kind-level contexts, or both.
typeOrKindCtxt :: UserTypeCtxt -> TypeOrKindCtxt
typeOrKindCtxt (FunSigCtxt {})      = OnlyTypeCtxt
typeOrKindCtxt (InfSigCtxt {})      = OnlyTypeCtxt
typeOrKindCtxt (ExprSigCtxt {})     = OnlyTypeCtxt
typeOrKindCtxt (TypeAppCtxt {})     = OnlyTypeCtxt
typeOrKindCtxt (PatSynCtxt {})      = OnlyTypeCtxt
typeOrKindCtxt (PatSigCtxt {})      = OnlyTypeCtxt
typeOrKindCtxt (RuleSigCtxt {})     = OnlyTypeCtxt
typeOrKindCtxt (ForSigCtxt {})      = OnlyTypeCtxt
typeOrKindCtxt (DefaultDeclCtxt {}) = OnlyTypeCtxt
typeOrKindCtxt (InstDeclCtxt {})    = OnlyTypeCtxt
typeOrKindCtxt (SpecInstCtxt {})    = OnlyTypeCtxt
typeOrKindCtxt (GenSigCtxt {})      = OnlyTypeCtxt
typeOrKindCtxt (ClassSCCtxt {})     = OnlyTypeCtxt
typeOrKindCtxt (SigmaCtxt {})       = OnlyTypeCtxt
typeOrKindCtxt (DataTyCtxt {})      = OnlyTypeCtxt
typeOrKindCtxt (DerivClauseCtxt {}) = OnlyTypeCtxt
typeOrKindCtxt (ConArgCtxt {})      = OnlyTypeCtxt
  -- Although data constructors can be promoted with DataKinds, we always
  -- validity-check them as though they are the types of terms. We may need
  -- to revisit this decision if we ever allow visible dependent quantification
  -- in the types of data constructors.

typeOrKindCtxt (KindSigCtxt {})           = OnlyKindCtxt
typeOrKindCtxt (StandaloneKindSigCtxt {}) = OnlyKindCtxt
typeOrKindCtxt (TyVarBndrKindCtxt {})     = OnlyKindCtxt
typeOrKindCtxt (DataKindCtxt {})          = OnlyKindCtxt
typeOrKindCtxt (TySynKindCtxt {})         = OnlyKindCtxt
typeOrKindCtxt (TyFamResKindCtxt {})      = OnlyKindCtxt

typeOrKindCtxt (TySynCtxt {}) = BothTypeAndKindCtxt
  -- Type synonyms can have types and kinds on their RHSs
typeOrKindCtxt (GhciCtxt {})  = BothTypeAndKindCtxt
  -- GHCi's :kind command accepts both types and kinds

-- | Returns 'True' if the supplied 'UserTypeCtxt' is unambiguously not the
-- context for a kind of a type.
-- If the 'UserTypeCtxt' can refer to both types and kinds, this function
-- conservatively returns 'True'.
--
-- An example of something that is unambiguously the kind of a type is the
-- @Show a => a -> a@ in @type Foo :: Show a => a -> a@. On the other hand, the
-- same type in @foo :: Show a => a -> a@ is unambiguously the type of a term,
-- not the kind of a type, so it is permitted.
typeLevelUserTypeCtxt :: UserTypeCtxt -> Bool
typeLevelUserTypeCtxt ctxt = case typeOrKindCtxt ctxt of
  OnlyTypeCtxt        -> True
  OnlyKindCtxt        -> False
  BothTypeAndKindCtxt -> True

-- | Returns 'True' if the supplied 'UserTypeCtxt' is unambiguously not the
-- context for a kind of a type, where the arbitrary use of constraints is
-- currently disallowed.
-- (See @Note [Constraints in kinds]@ in "GHC.Core.TyCo.Rep".)
allConstraintsAllowed :: UserTypeCtxt -> Bool
allConstraintsAllowed = typeLevelUserTypeCtxt

-- | Returns 'True' if the supplied 'UserTypeCtxt' is unambiguously not the
-- context for a kind of a type, where all function arrows currently
-- must be unrestricted.
linearityAllowed :: UserTypeCtxt -> Bool
linearityAllowed = typeLevelUserTypeCtxt

-- | Returns 'True' if the supplied 'UserTypeCtxt' is unambiguously not the
-- context for the type of a term, where visible, dependent quantification is
-- currently disallowed. If the 'UserTypeCtxt' can refer to both types and
-- kinds, this function conservatively returns 'True'.
--
-- An example of something that is unambiguously the type of a term is the
-- @forall a -> a -> a@ in @foo :: forall a -> a -> a@. On the other hand, the
-- same type in @type family Foo :: forall a -> a -> a@ is unambiguously the
-- kind of a type, not the type of a term, so it is permitted.
--
-- For more examples, see
-- @testsuite/tests/dependent/should_compile/T16326_Compile*.hs@ (for places
-- where VDQ is permitted) and
-- @testsuite/tests/dependent/should_fail/T16326_Fail*.hs@ (for places where
-- VDQ is disallowed).
vdqAllowed :: UserTypeCtxt -> Bool
vdqAllowed ctxt = case typeOrKindCtxt ctxt of
  OnlyTypeCtxt        -> False
  OnlyKindCtxt        -> True
  BothTypeAndKindCtxt -> True

{-
Note [Correctness and performance of type synonym validity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the type A arg1 arg2, where A is a type synonym. How should we check
this type for validity? We have three distinct choices, corresponding to the
three constructors of ExpandMode:

1. Expand the application of A, and check the resulting type (`Expand`).
2. Don't expand the application of A. Only check the arguments (`NoExpand`).
3. Check the arguments *and* check the expanded type (`Both`).

It's tempting to think that we could always just pick choice (3), but this
results in serious performance issues when checking a type like in the
signature for `f` below:

  type S = ...
  f :: S (S (S (S (S (S ....(S Int)...))))

When checking the type of `f`, we'll check the outer `S` application with and
without expansion, and in *each* of those checks, we'll check the next `S`
application with and without expansion... the result is exponential blowup! So
clearly we don't want to use `Both` 100% of the time.

On the other hand, neither is it correct to use exclusively `Expand` or
exclusively `NoExpand` 100% of the time:

* If one always expands, then one can miss erroneous programs like the one in
  the `tcfail129` test case:

    type Foo a = String -> Maybe a
    type Bar m = m Int
    blah = undefined :: Bar Foo

  If we expand `Bar Foo` immediately, we'll miss the fact that the `Foo` type
  synonyms is unsaturated.
* If one never expands and only checks the arguments, then one can miss
  erroneous programs like the one in #16059:

    type Foo b = Eq b => b
    f :: forall b (a :: Foo b). Int

  The kind of `a` contains a constraint, which is illegal, but this will only
  be caught if `Foo b` is expanded.

Therefore, it's impossible to have these validity checks be simultaneously
correct and performant if one sticks exclusively to a single `ExpandMode`. In
that case, the solution is to vary the `ExpandMode`s! In more detail:

1. When we start validity checking, we start with `Expand` if
   LiberalTypeSynonyms is enabled (see Note [Liberal type synonyms] for why we
   do this), and we start with `Both` otherwise. The `initialExpandMode`
   function is responsible for this.
2. When expanding an application of a type synonym (in `check_syn_tc_app`), we
   determine which things to check based on the current `ExpandMode` argument.
   Importantly, if the current mode is `Both`, then we check the arguments in
   `NoExpand` mode and check the expanded type in `Both` mode.

   Switching to `NoExpand` when checking the arguments is vital to avoid
   exponential blowup. One consequence of this choice is that if you have
   the following type synonym in one module (with RankNTypes enabled):

     {-# LANGUAGE RankNTypes #-}
     module A where
     type A = forall a. a

   And you define the following in a separate module *without* RankNTypes
   enabled:

     module B where

     import A

     type Const a b = a
     f :: Const Int A -> Int

   Then `f` will be accepted, even though `A` (which is technically a rank-n
   type) appears in its type. We view this as an acceptable compromise, since
   `A` never appears in the type of `f` post-expansion. If `A` _did_ appear in
   a type post-expansion, such as in the following variant:

     g :: Const A A -> Int

   Then that would be rejected unless RankNTypes were enabled.
-}

-- | When validity-checking an application of a type synonym, should we
-- check the arguments, check the expanded type, or both?
-- See Note [Correctness and performance of type synonym validity checking]
data ExpandMode
  = Expand   -- ^ Only check the expanded type.
  | NoExpand -- ^ Only check the arguments.
  | Both     -- ^ Check both the arguments and the expanded type.

instance Outputable ExpandMode where
  ppr e = text $ case e of
                   Expand   -> "Expand"
                   NoExpand -> "NoExpand"
                   Both     -> "Both"

-- | If @LiberalTypeSynonyms@ is enabled, we start in 'Expand' mode for the
-- reasons explained in @Note [Liberal type synonyms]@. Otherwise, we start
-- in 'Both' mode.
initialExpandMode :: TcM ExpandMode
initialExpandMode = do
  liberal_flag <- xoptM LangExt.LiberalTypeSynonyms
  pure $ if liberal_flag then Expand else Both

-- | Information about a type being validity-checked.
data ValidityEnv = ValidityEnv
  { ve_tidy_env :: TidyEnv
  , ve_ctxt     :: UserTypeCtxt
  , ve_rank     :: Rank
  , ve_expand   :: ExpandMode }

instance Outputable ValidityEnv where
  ppr (ValidityEnv{ ve_tidy_env = env, ve_ctxt = ctxt
                  , ve_rank = rank, ve_expand = expand }) =
    hang (text "ValidityEnv")
       2 (vcat [ text "ve_tidy_env" <+> ppr env
               , text "ve_ctxt"     <+> pprUserTypeCtxt ctxt
               , text "ve_rank"     <+> ppr rank
               , text "ve_expand"   <+> ppr expand ])

----------------------------------------
check_type :: ValidityEnv -> Type -> TcM ()
-- The args say what the *type context* requires, independent
-- of *flag* settings.  You test the flag settings at usage sites.
--
-- Rank is allowed rank for function args
-- Rank 0 means no for-alls anywhere

check_type _ (TyVarTy _) = return ()

check_type ve (AppTy ty1 ty2)
  = do  { check_type ve ty1
        ; check_arg_type False ve ty2 }

check_type ve ty@(TyConApp tc tys)
  | isTypeSynonymTyCon tc || isTypeFamilyTyCon tc
  = check_syn_tc_app ve ty tc tys
  | isUnboxedTupleTyCon tc = check_ubx_tuple ve ty tys
  | otherwise              = mapM_ (check_arg_type False ve) tys

check_type _ (LitTy {}) = return ()

check_type ve (CastTy ty _) = check_type ve ty

-- Check for rank-n types, such as (forall x. x -> x) or (Show x => x).
--
-- Critically, this case must come *after* the case for TyConApp.
-- See Note [Liberal type synonyms].
check_type ve@(ValidityEnv{ ve_tidy_env = env, ve_ctxt = ctxt
                          , ve_rank = rank, ve_expand = expand }) ty
  | not (null tvbs && null theta)
  = do  { traceTc "check_type" (ppr ty $$ ppr rank)
        ; checkTcM (forAllAllowed rank) (forAllTyErr env rank ty)
                -- Reject e.g. (Maybe (?x::Int => Int)),
                -- with a decent error message

        ; checkConstraintsOK ve theta ty
                -- Reject forall (a :: Eq b => b). blah
                -- In a kind signature we don't allow constraints

        ; checkTcM (all (isInvisibleArgFlag . binderArgFlag) tvbs
                         || vdqAllowed ctxt)
                   (illegalVDQTyErr env ty)
                -- Reject visible, dependent quantification in the type of a
                -- term (e.g., `f :: forall a -> a -> Maybe a`)

        ; check_valid_theta env' SigmaCtxt expand theta
                -- Allow     type T = ?x::Int => Int -> Int
                -- but not   type T = ?x::Int

        ; check_type (ve{ve_tidy_env = env'}) tau
                -- Allow foralls to right of arrow

        ; checkEscapingKind env' tvbs' theta tau }
  where
    (tvbs, phi)   = tcSplitForAllTyVarBinders ty
    (theta, tau)  = tcSplitPhiTy phi
    (env', tvbs') = tidyTyCoVarBinders env tvbs

check_type (ve@ValidityEnv{ ve_tidy_env = env, ve_ctxt = ctxt
                          , ve_rank = rank })
           ty@(FunTy _ mult arg_ty res_ty)
  = do  { failIfTcM (not (linearityAllowed ctxt) && not (isManyDataConTy mult))
                     (linearFunKindErr env ty)
        ; check_type (ve{ve_rank = arg_rank}) arg_ty
        ; check_type (ve{ve_rank = res_rank}) res_ty }
  where
    (arg_rank, res_rank) = funArgResRank rank

check_type _ ty = pprPanic "check_type" (ppr ty)

----------------------------------------
check_syn_tc_app :: ValidityEnv
                 -> KindOrType -> TyCon -> [KindOrType] -> TcM ()
-- Used for type synonyms and type synonym families,
-- which must be saturated,
-- but not data families, which need not be saturated
check_syn_tc_app (ve@ValidityEnv{ ve_ctxt = ctxt, ve_expand = expand })
                 ty tc tys
  | tys `lengthAtLeast` tc_arity   -- Saturated
       -- Check that the synonym has enough args
       -- This applies equally to open and closed synonyms
       -- It's OK to have an *over-applied* type synonym
       --      data Tree a b = ...
       --      type Foo a = Tree [a]
       --      f :: Foo a b -> ...
  = case expand of
      _ |  isTypeFamilyTyCon tc
        -> check_args_only expand
      -- See Note [Correctness and performance of type synonym validity
      --           checking]
      Expand   -> check_expansion_only expand
      NoExpand -> check_args_only expand
      Both     -> check_args_only NoExpand *> check_expansion_only Both

  | GhciCtxt True <- ctxt  -- Accept outermost under-saturated type synonym or
                           -- type family constructors in GHCi :kind commands.
                           -- See Note [Unsaturated type synonyms in GHCi]
  = check_args_only expand

  | otherwise
  = failWithTc (tyConArityErr tc tys)
  where
    tc_arity  = tyConArity tc

    check_arg :: ExpandMode -> KindOrType -> TcM ()
    check_arg expand =
      check_arg_type (isTypeSynonymTyCon tc) (ve{ve_expand = expand})

    check_args_only, check_expansion_only :: ExpandMode -> TcM ()
    check_args_only expand = mapM_ (check_arg expand) tys

    check_expansion_only expand
      = ASSERT2( isTypeSynonymTyCon tc, ppr tc )
        case tcView ty of
         Just ty' -> let err_ctxt = text "In the expansion of type synonym"
                                    <+> quotes (ppr tc)
                     in addErrCtxt err_ctxt $
                        check_type (ve{ve_expand = expand}) ty'
         Nothing  -> pprPanic "check_syn_tc_app" (ppr ty)

{-
Note [Unsaturated type synonyms in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking, GHC disallows unsaturated uses of type synonyms or type
families. For instance, if one defines `type Const a b = a`, then GHC will not
permit using `Const` unless it is applied to (at least) two arguments. There is
an exception to this rule, however: GHCi's :kind command. For instance, it
is quite common to look up the kind of a type constructor like so:

  λ> :kind Const
  Const :: j -> k -> j
  λ> :kind Const Int
  Const Int :: k -> Type

Strictly speaking, the two uses of `Const` above are unsaturated, but this
is an extremely benign (and useful) example of unsaturation, so we allow it
here as a special case.

That being said, we do not allow unsaturation carte blanche in GHCi. Otherwise,
this GHCi interaction would be possible:

  λ> newtype Fix f = MkFix (f (Fix f))
  λ> type Id a = a
  λ> :kind Fix Id
  Fix Id :: Type

This is rather dodgy, so we move to disallow this. We only permit unsaturated
synonyms in GHCi if they are *top-level*—that is, if the synonym is the
outermost type being applied. This allows `Const` and `Const Int` in the
first example, but not `Fix Id` in the second example, as `Id` is not the
outermost type being applied (`Fix` is).

We track this outermost property in the GhciCtxt constructor of UserTypeCtxt.
A field of True in GhciCtxt indicates that we're in an outermost position. Any
time we invoke `check_arg` to check the validity of an argument, we switch the
field to False.
-}

----------------------------------------
check_ubx_tuple :: ValidityEnv -> KindOrType -> [KindOrType] -> TcM ()
check_ubx_tuple (ve@ValidityEnv{ve_tidy_env = env}) ty tys
  = do  { ub_tuples_allowed <- xoptM LangExt.UnboxedTuples
        ; checkTcM ub_tuples_allowed (ubxArgTyErr env ty)

        ; impred <- xoptM LangExt.ImpredicativeTypes
        ; let rank' = if impred then ArbitraryRank else tyConArgMonoType
                -- c.f. check_arg_type
                -- However, args are allowed to be unlifted, or
                -- more unboxed tuples, so can't use check_arg_ty
        ; mapM_ (check_type (ve{ve_rank = rank'})) tys }

----------------------------------------
check_arg_type
  :: Bool -- ^ Is this the argument to a type synonym?
  -> ValidityEnv -> KindOrType -> TcM ()
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

check_arg_type _ _ (CoercionTy {}) = return ()

check_arg_type type_syn (ve@ValidityEnv{ve_ctxt = ctxt, ve_rank = rank}) ty
  = do  { impred <- xoptM LangExt.ImpredicativeTypes
        ; let rank' = case rank of          -- Predictive => must be monotype
                        -- Rank-n arguments to type synonyms are OK, provided
                        -- that LiberalTypeSynonyms is enabled.
                        _ | type_syn       -> synArgMonoType
                        MustBeMonoType     -> MustBeMonoType  -- Monotype, regardless
                        _other | impred    -> ArbitraryRank
                               | otherwise -> tyConArgMonoType
                        -- Make sure that MustBeMonoType is propagated,
                        -- so that we don't suggest -XImpredicativeTypes in
                        --    (Ord (forall a.a)) => a -> a
                        -- and so that if it Must be a monotype, we check that it is!
              ctxt' :: UserTypeCtxt
              ctxt'
                | GhciCtxt _ <- ctxt = GhciCtxt False
                    -- When checking an argument, set the field of GhciCtxt to
                    -- False to indicate that we are no longer in an outermost
                    -- position (and thus unsaturated synonyms are no longer
                    -- allowed).
                    -- See Note [Unsaturated type synonyms in GHCi]
                | otherwise          = ctxt

        ; check_type (ve{ve_ctxt = ctxt', ve_rank = rank'}) ty }

----------------------------------------
forAllTyErr :: TidyEnv -> Rank -> Type -> (TidyEnv, SDoc)
forAllTyErr env rank ty
   = ( env
     , vcat [ hang herald 2 (ppr_tidy env ty)
            , suggestion ] )
  where
    (tvs, _rho) = tcSplitForAllTyVars ty
    herald | null tvs  = text "Illegal qualified type:"
           | otherwise = text "Illegal polymorphic type:"
    suggestion = case rank of
                   LimitedRank {} -> text "Perhaps you intended to use RankNTypes"
                   MonoType d     -> d
                   _              -> Outputable.empty -- Polytype is always illegal

-- | Reject type variables that would escape their escape through a kind.
-- See @Note [Type variables escaping through kinds]@.
checkEscapingKind :: TidyEnv -> [TyVarBinder] -> ThetaType -> Type -> TcM ()
checkEscapingKind env tvbs theta tau =
  case occCheckExpand (binderVars tvbs) phi_kind of
    -- Ensure that none of the tvs occur in the kind of the forall
    -- /after/ expanding type synonyms.
    -- See Note [Phantom type variables in kinds] in GHC.Core.Type
    Nothing -> failWithTcM $ forAllEscapeErr env tvbs theta tau tau_kind
    Just _  -> pure ()
  where
    tau_kind              = tcTypeKind tau
    phi_kind | null theta = tau_kind
             | otherwise  = liftedTypeKind
        -- If there are any constraints, the kind is *. (#11405)

forAllEscapeErr :: TidyEnv -> [TyVarBinder] -> ThetaType -> Type -> Kind
                -> (TidyEnv, SDoc)
forAllEscapeErr env tvbs theta tau tau_kind
  = ( env
    , vcat [ hang (text "Quantified type's kind mentions quantified type variable")
                2 (text "type:" <+> quotes (ppr (mkSigmaTy tvbs theta tau)))
                -- NB: Don't tidy this type since the tvbs were already tidied
                -- previously, and re-tidying them will make the names of type
                -- variables different from tau_kind.
           , hang (text "where the body of the forall has this kind:")
                2 (quotes (ppr_tidy env tau_kind)) ] )

{-
Note [Type variables escaping through kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:

  type family T (r :: RuntimeRep) :: TYPE r
  foo :: forall r. T r

Something smells funny about the type of `foo`. If you spell out the kind
explicitly, it becomes clearer from where the smell originates:

  foo :: ((forall r. T r) :: TYPE r)

The type variable `r` appears in the result kind, which escapes the scope of
its binding site! This is not desirable, so we establish a validity check
(`checkEscapingKind`) to catch any type variables that might escape through
kinds in this way.
-}

ubxArgTyErr :: TidyEnv -> Type -> (TidyEnv, SDoc)
ubxArgTyErr env ty
  = ( env, vcat [ sep [ text "Illegal unboxed tuple type as function argument:"
                      , ppr_tidy env ty ]
                , text "Perhaps you intended to use UnboxedTuples" ] )

checkConstraintsOK :: ValidityEnv -> ThetaType -> Type -> TcM ()
checkConstraintsOK ve theta ty
  | null theta                         = return ()
  | allConstraintsAllowed (ve_ctxt ve) = return ()
  | otherwise
  = -- We are in a kind, where we allow only equality predicates
    -- See Note [Constraints in kinds] in GHC.Core.TyCo.Rep, and #16263
    checkTcM (all isEqPred theta) $
    constraintTyErr (ve_tidy_env ve) ty

constraintTyErr :: TidyEnv -> Type -> (TidyEnv, SDoc)
constraintTyErr env ty
  = (env, text "Illegal constraint in a kind:" <+> ppr_tidy env ty)

-- | Reject a use of visible, dependent quantification in the type of a term.
illegalVDQTyErr :: TidyEnv -> Type -> (TidyEnv, SDoc)
illegalVDQTyErr env ty =
  (env, vcat
  [ hang (text "Illegal visible, dependent quantification" <+>
          text "in the type of a term:")
       2 (ppr_tidy env ty)
  , text "(GHC does not yet support this)" ] )

-- | Reject uses of linear function arrows in kinds.
linearFunKindErr :: TidyEnv -> Type -> (TidyEnv, SDoc)
linearFunKindErr env ty =
  (env, text "Illegal linear function in a kind:" <+> ppr_tidy env ty)

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
checking on an application (T ty1 ty2)

        *either* before expansion (i.e. check ty1, ty2)
        *or* after expansion (i.e. expand T ty1 ty2, and then check)
        BUT NOT BOTH

If we do both, we get exponential behaviour!!

  data TIACons1 i r c = c i ::: r c
  type TIACons2 t x = TIACons1 t (TIACons1 t x)
  type TIACons3 t x = TIACons2 t (TIACons1 t x)
  type TIACons4 t x = TIACons2 t (TIACons2 t x)
  type TIACons7 t x = TIACons4 t (TIACons3 t x)

The order in which you do validity checking is also somewhat delicate. Consider
the `check_type` function, which drives the validity checking for unsaturated
uses of type synonyms. There is a special case for rank-n types, such as
(forall x. x -> x) or (Show x => x), since those require at least one language
extension to use. It used to be the case that this case came before every other
case, but this can lead to bugs. Imagine you have this scenario (from #15954):

  type A a = Int
  type B (a :: Type -> Type) = forall x. x -> x
  type C = B A

If the rank-n case came first, then in the process of checking for `forall`s
or contexts, we would expand away `B A` to `forall x. x -> x`. This is because
the functions that split apart `forall`s/contexts
(tcSplitForAllTyVarBinders/tcSplitPhiTy) expand type synonyms! If `B A` is expanded
away to `forall x. x -> x` before the actually validity checks occur, we will
have completely obfuscated the fact that we had an unsaturated application of
the `A` type synonym.

We have since learned from our mistakes and now put this rank-n case /after/
the case for TyConApp, which ensures that an unsaturated `A` TyConApp will be
caught properly. But be careful! We can't make the rank-n case /last/ either,
as the FunTy case must came after the rank-n case. Otherwise, something like
(Eq a => Int) would be treated as a function type (FunTy), which just
wouldn't do.

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
applying the instance decl would show up two uses of ?x.  #8912.
-}

checkValidTheta :: UserTypeCtxt -> ThetaType -> TcM ()
-- Assumes argument is fully zonked
checkValidTheta ctxt theta
  = addErrCtxtM (checkThetaCtxt ctxt theta) $
    do { env <- tcInitOpenTidyEnv (tyCoVarsOfTypesList theta)
       ; expand <- initialExpandMode
       ; check_valid_theta env ctxt expand theta }

-------------------------
check_valid_theta :: TidyEnv -> UserTypeCtxt -> ExpandMode
                  -> [PredType] -> TcM ()
check_valid_theta _ _ _ []
  = return ()
check_valid_theta env ctxt expand theta
  = do { dflags <- getDynFlags
       ; warnTcM (Reason Opt_WarnDuplicateConstraints)
                 (wopt Opt_WarnDuplicateConstraints dflags && notNull dups)
                 (dupPredWarn env dups)
       ; traceTc "check_valid_theta" (ppr theta)
       ; mapM_ (check_pred_ty env dflags ctxt expand) theta }
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
(#9838):

   {-# LANGUAGE ConstraintKinds #-}
   module A where
      type EqShow a = (Eq a, Show a)

   module B where
      import A
      foo :: EqShow a => a -> String

We don't want to require ConstraintKinds in module B.
-}

check_pred_ty :: TidyEnv -> DynFlags -> UserTypeCtxt -> ExpandMode
              -> PredType -> TcM ()
-- Check the validity of a predicate in a signature
-- See Note [Validity checking for constraints]
check_pred_ty env dflags ctxt expand pred
  = do { check_type ve pred
       ; check_pred_help False env dflags ctxt pred }
  where
    rank | xopt LangExt.QuantifiedConstraints dflags
         = ArbitraryRank
         | otherwise
         = constraintMonoType

    ve :: ValidityEnv
    ve = ValidityEnv{ ve_tidy_env = env
                    , ve_ctxt     = SigmaCtxt
                    , ve_rank     = rank
                    , ve_expand   = expand }

check_pred_help :: Bool    -- True <=> under a type synonym
                -> TidyEnv
                -> DynFlags -> UserTypeCtxt
                -> PredType -> TcM ()
check_pred_help under_syn env dflags ctxt pred
  | Just pred' <- tcView pred  -- Switch on under_syn when going under a
                                 -- synonym (#9838, yuk)
  = check_pred_help True env dflags ctxt pred'

  | otherwise  -- A bit like classifyPredType, but not the same
               -- E.g. we treat (~) like (~#); and we look inside tuples
  = case classifyPredType pred of
      ClassPred cls tys
        | isCTupleClass cls   -> check_tuple_pred under_syn env dflags ctxt pred tys
        | otherwise           -> check_class_pred env dflags ctxt pred cls tys

      EqPred _ _ _      -> pprPanic "check_pred_help" (ppr pred)
              -- EqPreds, such as (t1 ~ #t2) or (t1 ~R# t2), don't even have kind Constraint
              -- and should never appear before the '=>' of a type.  Thus
              --     f :: (a ~# b) => blah
              -- is wrong.  For user written signatures, it'll be rejected by kind-checking
              -- well before we get to validity checking.  For inferred types we are careful
              -- to box such constraints in GHC.Tc.Utils.TcType.pickQuantifiablePreds, as described
              -- in Note [Lift equality constraints when quantifying] in GHC.Tc.Utils.TcType

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
check_quant_pred env dflags ctxt pred theta head_pred
  = addErrCtxt (text "In the quantified constraint" <+> quotes (ppr pred)) $
    do { -- Check the instance head
         case classifyPredType head_pred of
                                 -- SigmaCtxt tells checkValidInstHead that
                                 -- this is the head of a quantified constraint
            ClassPred cls tys -> do { checkValidInstHead SigmaCtxt cls tys
                                    ; check_pred_help False env dflags ctxt head_pred }
                               -- need check_pred_help to do extra pred-only validity
                               -- checks, such as for (~). Otherwise, we get #17563
                               -- NB: checks for the context are covered by the check_type
                               -- in check_pred_ty
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
         -- at the definition site (#9838)
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
was done at the type synonym definition site; see #9838
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
  | isEqPredClass cls    -- (~) and (~~) are classified as classes,
                         -- but here we want to treat them as equalities
  = check_eq_pred env dflags pred

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
                 2 (ppr what)
            , hang (text "This makes type inference for inner bindings fragile;")
                 2 (text "either use MonoLocalBinds, or simplify it using the instance") ]

{- Note [Simplifiable given constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A type signature like
   f :: Eq [(a,b)] => a -> b
is very fragile, for reasons described at length in GHC.Tc.Solver.Interact
Note [Instance and Given overlap].  As that Note discusses, for the
most part the clever stuff in GHC.Tc.Solver.Interact means that we don't use a
top-level instance if a local Given might fire, so there is no
fragility. But if we /infer/ the type of a local let-binding, things
can go wrong (#11948 is an example, discussed in the Note).

So this warning is switched on only if we have NoMonoLocalBinds; in
that case the warning discourages users from writing simplifiable
class constraints.

The warning only fires if the constraint in the signature
matches the top-level instances in only one way, and with no
unifiers -- that is, under the same circumstances that
GHC.Tc.Solver.Interact.matchInstEnv fires an interaction with the top
level instances.  For example (#13526), consider

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
okIPCtxt GenSigCtxt             = True
okIPCtxt (ConArgCtxt {})        = True
okIPCtxt (ForSigCtxt {})        = True  -- ??
okIPCtxt (GhciCtxt {})          = True
okIPCtxt SigmaCtxt              = True
okIPCtxt (DataTyCtxt {})        = True
okIPCtxt (PatSynCtxt {})        = True
okIPCtxt (TySynCtxt {})         = True   -- e.g.   type Blah = ?x::Int
                                         -- #11466

okIPCtxt (KindSigCtxt {})       = False
okIPCtxt (StandaloneKindSigCtxt {}) = False
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
-- (e.g. #10516)
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
       ; check_special_inst_head dflags is_boot is_sig ctxt clas cls_args
       ; checkValidTypePats (classTyCon clas) cls_args
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
module that imports M.hsig: see #15379 and
Note [Fabricating Evidence for Literals in Backpack] in GHC.Tc.Instance.Class.

Hence, checkValidInstHead accepts a user-written instance declaration
in hsig files, where `is_sig` is True.

-}

check_special_inst_head :: DynFlags -> Bool -> Bool
                        -> UserTypeCtxt -> Class -> [Type] -> TcM ()
-- Wow!  There are a surprising number of ad-hoc special cases here.
check_special_inst_head dflags is_boot is_sig ctxt clas cls_args

  -- If not in an hs-boot file, abstract classes cannot have instances
  | isAbstractClass clas
  , not is_boot
  = failWithTc abstract_class_msg

  -- For Typeable, don't complain about instances for
  -- standalone deriving; they are no-ops, and we warn about
  -- it in GHC.Tc.Deriv.deriveStandalone.
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
  = pure ()
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
                -- even though it's a type synonym for FUN 'Many
        _ -> True

tcInstHeadTyAppAllTyVars :: Type -> Bool
-- Used in Haskell-98 mode, for the argument types of an instance head
-- These must be a constructor applied to type variable arguments
-- or a type-level literal.
-- But we allow
-- 1) kind instantiations
-- 2) the type (->) = FUN 'Many, even though it's not in this form.
tcInstHeadTyAppAllTyVars ty
  | Just (tc, tys) <- tcSplitTyConApp_maybe (dropCasts ty)
  = let tys' = filterOutInvisibleTypes tc tys  -- avoid kinds
        tys'' | tc == funTyCon, tys_h:tys_t <- tys', tys_h `eqType` manyDataConTy = tys_t
              | otherwise = tys'
    in ok tys''
  | LitTy _ <- ty = True  -- accept type literals (#13833)
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
dropCasts (CastTy ty _)       = dropCasts ty
dropCasts (AppTy t1 t2)       = mkAppTy (dropCasts t1) (dropCasts t2)
dropCasts ty@(FunTy _ w t1 t2)  = ty { ft_mult = dropCasts w, ft_arg = dropCasts t1, ft_res = dropCasts t2 }
dropCasts (TyConApp tc tys)   = mkTyConApp tc (map dropCasts tys)
dropCasts (ForAllTy b ty)     = ForAllTy (dropCastsB b) (dropCasts ty)
dropCasts ty                  = ty  -- LitTy, TyVarTy, CoercionTy

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
[HasField instances] in GHC.Tc.Solver.Interact).  However, we permit users to
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
derived instance contexts] in GHC.Tc.Deriv.  However the predicate is
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
    would count as a constructor! See #11833.

  * Also check for a bizarre corner case, when the derived instance decl
    would look like
       instance C a b => D (T a) where ...
    Note that 'b' isn't a parameter of T.  This gives rise to all sorts of
    problems; in particular, it's hard to compare solutions for equality
    when finding the fixpoint, and that means the inferContext loop does
    not converge.  See #5287.

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
Consider these (#13267):
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

checkValidInstance :: UserTypeCtxt -> LHsSigType GhcRn -> Type -> TcM ()
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
  = do  { setSrcSpanA head_loc $
          checkValidInstHead ctxt clas inst_tys

        ; traceTc "checkValidInstance {" (ppr ty)

        ; env0 <- tcInitTidyEnv
        ; expand <- initialExpandMode
        ; check_valid_theta env0 ctxt expand theta

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

        ; return () }
  where
    (_tvs, theta, tau)   = tcSplitSigmaTy ty
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
         EqPred {}    -> return ()  -- See #4200.
         IrredPred {} -> check2 foralld_tvs pred (sizeType pred)
         ClassPred cls tys
           | isTerminatingClass cls
           -> return ()

           | isCTupleClass cls  -- Look inside tuple predicates; #8359
           -> check_preds foralld_tvs tys

           | otherwise          -- Other ClassPreds
           -> check2 foralld_tvs pred bogus_size
           where
              bogus_size = 1 + sizeTypes (filterOutInvisibleTypes (classTyCon cls) tys)
                               -- See Note [Invisible arguments and termination]

         ForAllPred tvs _ head_pred'
           -> check (foralld_tvs `extendVarSetList` tvs) head_pred'
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
  instance C (F a) => C [[[a]]]     where ...

No: the type family in the instance head might blow up to an
arbitrarily large type, depending on how 'a' is instantiated.
So we require UndecidableInstances if we have a type family
in the instance head.  #15172.

Note [Invisible arguments and termination]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When checking the ​Paterson conditions for termination an instance
declaration, we check for the number of "constructors and variables"
in the instance head and constraints. Question: Do we look at

 * All the arguments, visible or invisible?
 * Just the visible arguments?

I think both will ensure termination, provided we are consistent.
Currently we are /not/ consistent, which is really a bug.  It's
described in #15177, which contains a number of examples.
The suspicious bits are the calls to filterOutInvisibleTypes.
-}


{-
************************************************************************
*                                                                      *
        Checking type instance well-formedness and termination
*                                                                      *
************************************************************************
-}

checkValidCoAxiom :: CoAxiom Branched -> TcM ()
checkValidCoAxiom ax@(CoAxiom { co_ax_tc = fam_tc, co_ax_branches = branches })
  = do { mapM_ (checkValidCoAxBranch fam_tc) branch_list
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
             inaccessibleCoAxBranch fam_tc cur_branch
           ; return prev_branches }
      | otherwise
      = do { check_injectivity prev_branches cur_branch
           ; return (cur_branch : prev_branches) }

     -- Injectivity check: check whether a new (CoAxBranch) can extend
     -- already checked equations without violating injectivity
     -- annotation supplied by the user.
     -- See Note [Verifying injectivity annotation] in GHC.Core.FamInstEnv
    check_injectivity prev_branches cur_branch
      | Injective inj <- injectivity
      = do { dflags <- getDynFlags
           ; let conflicts =
                     fst $ foldl' (gather_conflicts inj prev_branches cur_branch)
                                 ([], 0) prev_branches
           ; reportConflictingInjectivityErrs fam_tc conflicts cur_branch
           ; reportInjectivityErrors dflags ax cur_branch inj }
      | otherwise
      = return ()

    gather_conflicts inj prev_branches cur_branch (acc, n) branch
               -- n is 0-based index of branch in prev_branches
      = case injectiveBranches inj cur_branch branch of
           -- Case 1B2 in Note [Verifying injectivity annotation] in GHC.Core.FamInstEnv
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
checkValidCoAxBranch :: TyCon -> CoAxBranch -> TcM ()
checkValidCoAxBranch fam_tc
                    (CoAxBranch { cab_tvs = tvs, cab_cvs = cvs
                                , cab_lhs = typats
                                , cab_rhs = rhs, cab_loc = loc })
  = setSrcSpan loc $
    checkValidTyFamEqn fam_tc (tvs++cvs) typats rhs

-- | Do validity checks on a type family equation, including consistency
-- with any enclosing class instance head, termination, and lack of
-- polytypes.
checkValidTyFamEqn :: TyCon   -- ^ of the type family
                   -> [Var]   -- ^ Bound variables in the equation
                   -> [Type]  -- ^ Type patterns
                   -> Type    -- ^ Rhs
                   -> TcM ()
checkValidTyFamEqn fam_tc qvs typats rhs
  = do { checkValidTypePats fam_tc typats

         -- Check for things used on the right but not bound on the left
       ; checkFamPatBinders fam_tc qvs typats rhs

         -- Check for oversaturated visible kind arguments in a type family
         -- equation.
         -- See Note [Oversaturated type family equations]
       ; when (isTypeFamilyTyCon fam_tc) $
           case drop (tyConArity fam_tc) typats of
             [] -> pure ()
             spec_arg:_ ->
               addErr $ text "Illegal oversaturated visible kind argument:"
                    <+> quotes (char '@' <> pprParendType spec_arg)

         -- The argument patterns, and RHS, are all boxed tau types
         -- E.g  Reject type family F (a :: k1) :: k2
         --             type instance F (forall a. a->a) = ...
         --             type instance F Int#             = ...
         --             type instance F Int              = forall a. a->a
         --             type instance F Int              = Int#
         -- See #9357
       ; checkValidMonoType rhs

         -- We have a decidable instance unless otherwise permitted
       ; undecidable_ok <- xoptM LangExt.UndecidableInstances
       ; traceTc "checkVTFE" (ppr fam_tc $$ ppr rhs $$ ppr (tcTyFamInsts rhs))
       ; unless undecidable_ok $
         mapM_ addErrTc (checkFamInstRhs fam_tc typats (tcTyFamInsts rhs)) }

-- | Checks that an associated type family default:
--
-- 1. Only consists of arguments that are bare type variables, and
--
-- 2. Has a distinct type variable in each argument.
--
-- See @Note [Type-checking default assoc decls]@ in "GHC.Tc.TyCl".
checkValidAssocTyFamDeflt :: TyCon  -- ^ of the type family
                          -> [Type] -- ^ Type patterns
                          -> TcM ()
checkValidAssocTyFamDeflt fam_tc pats =
  do { cpt_tvs <- zipWithM extract_tv pats pats_vis
     ; check_all_distinct_tvs $ zip cpt_tvs pats_vis }
  where
    pats_vis :: [ArgFlag]
    pats_vis = tyConArgFlags fam_tc pats

    -- Checks that a pattern on the LHS of a default is a type
    -- variable. If so, return the underlying type variable, and if
    -- not, throw an error.
    -- See Note [Type-checking default assoc decls]
    extract_tv :: Type    -- The particular type pattern from which to extract
                          -- its underlying type variable
               -> ArgFlag -- The visibility of the type pattern
                          -- (only used for error message purposes)
               -> TcM TyVar
    extract_tv pat pat_vis =
      case getTyVar_maybe pat of
        Just tv -> pure tv
        Nothing -> failWithTc $
          pprWithExplicitKindsWhen (isInvisibleArgFlag pat_vis) $
          hang (text "Illegal argument" <+> quotes (ppr pat) <+> text "in:")
             2 (vcat [ppr_eqn, suggestion])

    -- Checks that no type variables in an associated default declaration are
    -- duplicated. If that is the case, throw an error.
    -- See Note [Type-checking default assoc decls]
    check_all_distinct_tvs ::
         [(TyVar, ArgFlag)] -- The type variable arguments in the associated
                            -- default declaration, along with their respective
                            -- visibilities (the latter are only used for error
                            -- message purposes)
      -> TcM ()
    check_all_distinct_tvs cpt_tvs_vis =
      let dups = findDupsEq ((==) `on` fst) cpt_tvs_vis in
      traverse_
        (\d -> let (pat_tv, pat_vis) = NE.head d in failWithTc $
               pprWithExplicitKindsWhen (isInvisibleArgFlag pat_vis) $
               hang (text "Illegal duplicate variable"
                       <+> quotes (ppr pat_tv) <+> text "in:")
                  2 (vcat [ppr_eqn, suggestion]))
        dups

    ppr_eqn :: SDoc
    ppr_eqn =
      quotes (text "type" <+> ppr (mkTyConApp fam_tc pats)
                <+> equals <+> text "...")

    suggestion :: SDoc
    suggestion = text "The arguments to" <+> quotes (ppr fam_tc)
             <+> text "must all be distinct type variables"

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

-----------------
checkFamPatBinders :: TyCon
                   -> [TcTyVar]   -- Bound on LHS of family instance
                   -> [TcType]    -- LHS patterns
                   -> Type        -- RHS
                   -> TcM ()
checkFamPatBinders fam_tc qtvs pats rhs
  = do { traceTc "checkFamPatBinders" $
         vcat [ debugPprType (mkTyConApp fam_tc pats)
              , ppr (mkTyConApp fam_tc pats)
              , text "qtvs:" <+> ppr qtvs
              , text "rhs_tvs:" <+> ppr (fvVarSet rhs_fvs)
              , text "cpt_tvs:" <+> ppr cpt_tvs
              , text "inj_cpt_tvs:" <+> ppr inj_cpt_tvs ]

         -- Check for implicitly-bound tyvars, mentioned on the
         -- RHS but not bound on the LHS
         --    data T            = MkT (forall (a::k). blah)
         --    data family D Int = MkD (forall (a::k). blah)
         -- In both cases, 'k' is not bound on the LHS, but is used on the RHS
         -- We catch the former in kcDeclHeader, and the latter right here
         -- See Note [Check type-family instance binders]
       ; check_tvs bad_rhs_tvs (text "mentioned in the RHS")
                               (text "bound on the LHS of")

         -- Check for explicitly forall'd variable that is not bound on LHS
         --    data instance forall a.  T Int = MkT Int
         -- See Note [Unused explicitly bound variables in a family pattern]
         -- See Note [Check type-family instance binders]
       ; check_tvs bad_qtvs (text "bound by a forall")
                            (text "used in")
       }
  where
    cpt_tvs     = tyCoVarsOfTypes pats
    inj_cpt_tvs = fvVarSet $ injectiveVarsOfTypes False pats
      -- The type variables that are in injective positions.
      -- See Note [Dodgy binding sites in type family instances]
      -- NB: The False above is irrelevant, as we never have type families in
      -- patterns.
      --
      -- NB: It's OK to use the nondeterministic `fvVarSet` function here,
      -- since the order of `inj_cpt_tvs` is never revealed in an error
      -- message.
    rhs_fvs     = tyCoFVsOfType rhs
    used_tvs    = cpt_tvs `unionVarSet` fvVarSet rhs_fvs
    bad_qtvs    = filterOut (`elemVarSet` used_tvs) qtvs
                  -- Bound but not used at all
    bad_rhs_tvs = filterOut (`elemVarSet` inj_cpt_tvs) (fvVarList rhs_fvs)
                  -- Used on RHS but not bound on LHS
    dodgy_tvs   = cpt_tvs `minusVarSet` inj_cpt_tvs

    check_tvs tvs what what2
      = unless (null tvs) $ addErrAt (getSrcSpan (head tvs)) $
        hang (text "Type variable" <> plural tvs <+> pprQuotedList tvs
              <+> isOrAre tvs <+> what <> comma)
           2 (vcat [ text "but not" <+> what2 <+> text "the family instance"
                   , mk_extra tvs ])

    -- mk_extra: #7536: give a decent error message for
    --         type T a = Int
    --         type instance F (T a) = a
    mk_extra tvs = ppWhen (any (`elemVarSet` dodgy_tvs) tvs) $
                   hang (text "The real LHS (expanding synonyms) is:")
                      2 (pprTypeApp fam_tc (map expandTypeSynonyms pats))


-- | Checks that a list of type patterns is valid in a matching (LHS)
-- position of a class instances or type/data family instance.
--
-- Specifically:
--    * All monotypes
--    * No type-family applications
checkValidTypePats :: TyCon -> [Type] -> TcM ()
checkValidTypePats tc pat_ty_args
  = do { -- Check that each of pat_ty_args is a monotype.
         -- One could imagine generalising to allow
         --      instance C (forall a. a->a)
         -- but we don't know what all the consequences might be.
         traverse_ checkValidMonoType pat_ty_args

       -- Ensure that no type family applications occur a type pattern
       ; case tcTyConAppTyFamInstsAndVis tc pat_ty_args of
            [] -> pure ()
            ((tf_is_invis_arg, tf_tc, tf_args):_) -> failWithTc $
               ty_fam_inst_illegal_err tf_is_invis_arg
                                       (mkTyConApp tf_tc tf_args) }
  where
    inst_ty = mkTyConApp tc pat_ty_args

    ty_fam_inst_illegal_err :: Bool -> Type -> SDoc
    ty_fam_inst_illegal_err invis_arg ty
      = pprWithExplicitKindsWhen invis_arg $
        hang (text "Illegal type synonym family application"
                <+> quotes (ppr ty) <+> text "in instance" <> colon)
           2 (ppr inst_ty)

-- Error messages

inaccessibleCoAxBranch :: TyCon -> CoAxBranch -> SDoc
inaccessibleCoAxBranch fam_tc cur_branch
  = text "Type family instance equation is overlapped:" $$
    nest 2 (pprCoAxBranchUser fam_tc cur_branch)

nestedMsg :: SDoc -> SDoc
nestedMsg what
  = sep [ text "Illegal nested" <+> what
        , parens undecidableMsg ]

badATErr :: Name -> Name -> SDoc
badATErr clas op
  = hsep [text "Class", quotes (ppr clas),
          text "does not have an associated type", quotes (ppr op)]


-------------------------
checkConsistentFamInst :: AssocInstInfo
                       -> TyCon     -- ^ Family tycon
                       -> CoAxBranch
                       -> TcM ()
-- See Note [Checking consistent instantiation]

checkConsistentFamInst NotAssociated _ _
  = return ()

checkConsistentFamInst (InClsInst { ai_class = clas
                                  , ai_tyvars = inst_tvs
                                  , ai_inst_env = mini_env })
                       fam_tc branch
  = do { traceTc "checkConsistentFamInst" (vcat [ ppr inst_tvs
                                                , ppr arg_triples
                                                , ppr mini_env
                                                , ppr ax_tvs
                                                , ppr ax_arg_tys
                                                , ppr arg_triples ])
       -- Check that the associated type indeed comes from this class
       -- See [Mismatched class methods and associated type families]
       -- in TcInstDecls.
       ; checkTc (Just (classTyCon clas) == tyConAssoc_maybe fam_tc)
                 (badATErr (className clas) (tyConName fam_tc))

       ; check_match arg_triples
       }
  where
    (ax_tvs, ax_arg_tys, _) = etaExpandCoAxBranch branch

    arg_triples :: [(Type,Type, ArgFlag)]
    arg_triples = [ (cls_arg_ty, at_arg_ty, vis)
                  | (fam_tc_tv, vis, at_arg_ty)
                       <- zip3 (tyConTyVars fam_tc)
                               (tyConArgFlags fam_tc ax_arg_tys)
                               ax_arg_tys
                  , Just cls_arg_ty <- [lookupVarEnv mini_env fam_tc_tv] ]

    pp_wrong_at_arg vis
      = pprWithExplicitKindsWhen (isInvisibleArgFlag vis) $
        vcat [ text "Type indexes must match class instance head"
             , text "Expected:" <+> pp_expected_ty
             , text "  Actual:" <+> pp_actual_ty ]

    -- Fiddling around to arrange that wildcards unconditionally print as "_"
    -- We only need to print the LHS, not the RHS at all
    -- See Note [Printing conflicts with class header]
    (tidy_env1, _) = tidyVarBndrs emptyTidyEnv inst_tvs
    (tidy_env2, _) = tidyCoAxBndrsForUser tidy_env1 (ax_tvs \\ inst_tvs)

    pp_expected_ty = pprIfaceTypeApp topPrec (toIfaceTyCon fam_tc) $
                     toIfaceTcArgs fam_tc $
                     [ case lookupVarEnv mini_env at_tv of
                         Just cls_arg_ty -> tidyType tidy_env2 cls_arg_ty
                         Nothing         -> mk_wildcard at_tv
                     | at_tv <- tyConTyVars fam_tc ]

    pp_actual_ty = pprIfaceTypeApp topPrec (toIfaceTyCon fam_tc) $
                   toIfaceTcArgs fam_tc $
                   tidyTypes tidy_env2 ax_arg_tys

    mk_wildcard at_tv = mkTyVarTy (mkTyVar tv_name (tyVarKind at_tv))
    tv_name = mkInternalName (mkAlphaTyVarUnique 1) (mkTyVarOcc "_") noSrcSpan

    -- For check_match, bind_me, see
    -- Note [Matching in the consistent-instantiation check]
    check_match :: [(Type,Type,ArgFlag)] -> TcM ()
    check_match triples = go emptyTCvSubst emptyTCvSubst triples

    go _ _ [] = return ()
    go lr_subst rl_subst ((ty1,ty2,vis):triples)
      | Just lr_subst1 <- tcMatchTyX_BM bind_me lr_subst ty1 ty2
      , Just rl_subst1 <- tcMatchTyX_BM bind_me rl_subst ty2 ty1
      = go lr_subst1 rl_subst1 triples
      | otherwise
      = addErrTc (pp_wrong_at_arg vis)

    -- The /scoped/ type variables from the class-instance header
    -- should not be alpha-renamed.  Inferred ones can be.
    no_bind_set = mkVarSet inst_tvs
    bind_me tv | tv `elemVarSet` no_bind_set = Skolem
               | otherwise                   = BindMe


{- Note [Check type-family instance binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type family instance, we require (of course), type variables
used on the RHS are matched on the LHS. This is checked by
checkFamPatBinders.  Here is an interesting example:

    type family   T :: k
    type instance T = (Nothing :: Maybe a)

Upon a cursory glance, it may appear that the kind variable `a` is unbound
since there are no (visible) LHS patterns in `T`. However, there is an
*invisible* pattern due to the return kind, so inside of GHC, the instance
looks closer to this:

    type family T @k :: k
    type instance T @(Maybe a) = (Nothing :: Maybe a)

Here, we can see that `a` really is bound by a LHS type pattern, so `a` is in
fact not unbound. Contrast that with this example (#13985)

    type instance T = Proxy (Nothing :: Maybe a)

This would looks like this inside of GHC:

    type instance T @(*) = Proxy (Nothing :: Maybe a)

So this time, `a` is neither bound by a visible nor invisible type pattern on
the LHS, so `a` would be reported as not in scope.

Finally, here's one more brain-teaser (from #9574). In the example below:

    class Funct f where
      type Codomain f :: *
    instance Funct ('KProxy :: KProxy o) where
      type Codomain 'KProxy = NatTr (Proxy :: o -> *)

As it turns out, `o` is in scope in this example. That is because `o` is
bound by the kind signature of the LHS type pattern 'KProxy. To make this more
obvious, one can also write the instance like so:

    instance Funct ('KProxy :: KProxy o) where
      type Codomain ('KProxy :: KProxy o) = NatTr (Proxy :: o -> *)

Note [Dodgy binding sites in type family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following example (from #7536):

  type T a = Int
  type instance F (T a) = a

This `F` instance is extremely fishy, since the RHS, `a`, purports to be
"bound" by the LHS pattern `T a`. "Bound" has scare quotes around it because
`T a` expands to `Int`, which doesn't mention at all, so it's as if one had
actually written:

  type instance F Int = a

That is clearly bogus, so to reject this, we check that every type variable
that is mentioned on the RHS is /actually/ bound on the LHS. In other words,
we need to do something slightly more sophisticated that just compute the free
variables of the LHS patterns.

It's tempting to just expand all type synonyms on the LHS and then compute
their free variables, but even that isn't sophisticated enough. After all,
an impish user could write the following (#17008):

  type family ConstType (a :: Type) :: Type where
    ConstType _ = Type

  type family F (x :: ConstType a) :: Type where
    F (x :: ConstType a) = a

Just like in the previous example, the `a` on the RHS isn't actually bound
on the LHS, but this time a type family is responsible for the deception, not
a type synonym.

We avoid both issues by requiring that all RHS type variables are mentioned
in injective positions on the left-hand side (by way of
`injectiveVarsOfTypes`). For instance, the `a` in `T a` is not in an injective
position, as `T` is not an injective type constructor, so we do not count that.
Similarly for the `a` in `ConstType a`.

Note [Matching in the consistent-instantiation check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Matching the class-instance header to family-instance tyvars is
tricker than it sounds.  Consider (#13972)
    class C (a :: k) where
      type T k :: Type
    instance C Left where
      type T (a -> Either a b) = Int

Here there are no lexically-scoped variables from (C Left).
Yet the real class-instance header is   C @(p -> Either @p @q)) (Left @p @q)
while the type-family instance is       T (a -> Either @a @b)
So we allow alpha-renaming of variables that don't come
from the class-instance header.

We track the lexically-scoped type variables from the
class-instance header in ai_tyvars.

Here's another example (#14045a)
    class C (a :: k) where
      data S (a :: k)
    instance C (z :: Bool) where
      data S :: Bool -> Type where

Again, there is no lexical connection, but we will get
   class-instance header:   C @Bool (z::Bool)
   family instance          S @Bool (a::Bool)

When looking for mis-matches, we check left-to-right,
kinds first.  If we look at types first, we'll fail to
suggest -fprint-explicit-kinds for a mis-match with
      T @k    vs    T @Type
somewhere deep inside the type

Note [Checking consistent instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #11450 for background discussion on this check.

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
  See #11450.

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
  to pursue that design (see also GHC #13398).

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

Note [Printing conflicts with class header]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's remarkably painful to give a decent error message for conflicts
with the class header.  Consider
   clase C b where
     type F a b c
   instance C [b] where
     type F x Int _ _ = ...

Here we want to report a conflict between
    Expected: F _ [b] _
    Actual:   F x Int _ _

But if the type instance shadows the class variable like this
(rename/should_fail/T15828):
   instance C [b] where
     type forall b. F x (Tree b) _ _ = ...

then we must use a fresh variable name
    Expected: F _ [b] _
    Actual:   F x [b1] _ _

Notice that:
  - We want to print an underscore in the "Expected" type in
    positions where the class header has no influence over the
    parameter.  Hence the fancy footwork in pp_expected_ty

  - Although the binders in the axiom are already tidy, we must
    re-tidy them to get a fresh variable name when we shadow

  - The (ax_tvs \\ inst_tvs) is to avoid tidying one of the
    class-instance variables a second time, from 'a' to 'a1' say.
    Remember, the ax_tvs of the axiom share identity with the
    class-instance variables, inst_tvs..

  - We use tidyCoAxBndrsForUser to get underscores rather than
    _1, _2, etc in the axiom tyvars; see the definition of
    tidyCoAxBndrsForUser

This all seems absurdly complicated.

Note [Unused explicitly bound variables in a family pattern]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Why is 'unusedExplicitForAllErr' not just a warning?

Consider the following examples:

  type instance F a = Maybe b
  type instance forall b. F a = Bool
  type instance forall b. F a = Maybe b

In every case, b is a type variable not determined by the LHS pattern. The
first is caught by the renamer, but we catch the last two here. Perhaps one
could argue that the second should be accepted, albeit with a warning, but
consider the fact that in a type family instance, there is no way to interact
with such a varable. At least with @x :: forall a. Int@ we can use visibile
type application, like @x \@Bool 1@. (Of course it does nothing, but it is
permissible.) In the type family case, the only sensible explanation is that
the user has made a mistake -- thus we throw an error.

Note [Oversaturated type family equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type family tycons have very rigid arities. We want to reject something like
this:

  type family Foo :: Type -> Type where
    Foo x = ...

Because Foo has arity zero (i.e., it doesn't bind anything to the left of the
double colon), we want to disallow any equation for Foo that has more than zero
arguments, such as `Foo x = ...`. The algorithm here is pretty simple: if an
equation has more arguments than the arity of the type family, reject.

Things get trickier when visible kind application enters the picture. Consider
the following example:

  type family Bar (x :: j) :: forall k. Either j k where
    Bar 5 @Symbol = ...

The arity of Bar is two, since it binds two variables, `j` and `x`. But even
though Bar's equation has two arguments, it's still invalid. Imagine the same
equation in Core:

    Bar Nat 5 Symbol = ...

Here, it becomes apparent that Bar is actually taking /three/ arguments! So
we can't just rely on a simple counting argument to reject
`Bar 5 @Symbol = ...`, since it only has two user-written arguments.
Moreover, there's one explicit argument (5) and one visible kind argument
(@Symbol), which matches up perfectly with the fact that Bar has one required
binder (x) and one specified binder (j), so that's not a valid way to detect
oversaturation either.

To solve this problem in a robust way, we do the following:

1. When kind-checking, we count the number of user-written *required*
   arguments and check if there is an equal number of required tycon binders.
   If not, reject. (See `wrongNumberOfParmsErr` in GHC.Tc.TyCl.)

   We perform this step during kind-checking, not during validity checking,
   since we can give better error messages if we catch it early.
2. When validity checking, take all of the (Core) type patterns from on
   equation, drop the first n of them (where n is the arity of the type family
   tycon), and check if there are any types leftover. If so, reject.

   Why does this work? We know that after dropping the first n type patterns,
   none of the leftover types can be required arguments, since step (1) would
   have already caught that. Moreover, the only places where visible kind
   applications should be allowed are in the first n types, since those are the
   only arguments that can correspond to binding forms. Therefore, the
   remaining arguments must correspond to oversaturated uses of visible kind
   applications, which are precisely what we want to reject.

Note that we only perform this check for type families, and not for data
families. This is because it is perfectly acceptable to oversaturate data
family instance equations: see Note [Arity of data families] in GHC.Core.FamInstEnv.

************************************************************************
*                                                                      *
   Telescope checking
*                                                                      *
************************************************************************

Note [Bad TyCon telescopes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

To catch these errors, we call checkTyConTelescope during kind-checking
datatype declarations.  This checks for

* Ill-scoped binders. From (1) and (2) above we can get putative
  kinds like
       T1 :: forall (a:k) (k:*) (b:k). SameKind a b -> *
  where 'k' is mentioned a's kind before k is bound

  This is easy to check for: just look for
  out-of-scope variables in the kind

* We should arguably also check for ambiguous binders
  but we don't.  See Note [Ambiguous kind vars].

See also
  * Note [Required, Specified, and Inferred for types] in GHC.Tc.TyCl.
  * Note [Checking telescopes] in GHC.Tc.Types.Constraint discusses how
    this check works for `forall x y z.` written in a type.

Note [Ambiguous kind vars]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to be concerned about ambiguous binders. Suppose we have the kind
     S1 :: forall k -> * -> *
     S2 :: forall k. * -> *
Here S1 is OK, because k is Required, and at a use of S1 we will
see (S1 *) or (S1 (*->*)) or whatever.

But S2 is /not/ OK because 'k' is Specfied (and hence invisible) and
we have no way (ever) to figure out how 'k' should be instantiated.
For example if we see (S2 Int), that tells us nothing about k's
instantiation.  (In this case we'll instantiate it to Any, but that
seems wrong.)  This is really the same test as we make for ambiguous
type in term type signatures.

Now, it's impossible for a Specified variable not to occur
at all in the kind -- after all, it is Specified so it must have
occurred.  (It /used/ to be possible; see tests T13983 and T7873.  But
with the advent of the forall-or-nothing rule for kind variables,
those strange cases went away. See Note [forall-or-nothing rule] in
GHC.Hs.Type.)

But one might worry about
    type v k = *
    S3 :: forall k. V k -> *
which appears to mention 'k' but doesn't really.  Or
    S4 :: forall k. F k -> *
where F is a type function.  But we simply don't check for
those cases of ambiguity, yet anyway.  The worst that can happen
is ambiguity at the call sites.

Historical note: this test used to be called reportFloatingKvs.
-}

-- | Check a list of binders to see if they make a valid telescope.
-- See Note [Bad TyCon telescopes]
type TelescopeAcc
      = ( TyVarSet   -- Bound earlier in the telescope
        , Bool       -- At least one binder occurred (in a kind) before
                     -- it was bound in the telescope.  E.g.
        )            --    T :: forall (a::k) k. blah

checkTyConTelescope :: TyCon -> TcM ()
checkTyConTelescope tc
  | bad_scope
  = -- See "Ill-scoped binders" in Note [Bad TyCon telescopes]
    addErr $
    vcat [ hang (text "The kind of" <+> quotes (ppr tc) <+> text "is ill-scoped")
              2 pp_tc_kind
         , extra
         , hang (text "Perhaps try this order instead:")
              2 (pprTyVars sorted_tvs) ]

  | otherwise
  = return ()
  where
    tcbs = tyConBinders tc
    tvs  = binderVars tcbs
    sorted_tvs = scopedSort tvs

    (_, bad_scope) = foldl add_one (emptyVarSet, False) tcbs

    add_one :: TelescopeAcc -> TyConBinder -> TelescopeAcc
    add_one (bound, bad_scope) tcb
      = ( bound `extendVarSet` tv
        , bad_scope || not (isEmptyVarSet (fkvs `minusVarSet` bound)) )
      where
        tv = binderVar tcb
        fkvs = tyCoVarsOfType (tyVarKind tv)

    inferred_tvs  = [ binderVar tcb
                    | tcb <- tcbs, Inferred == tyConBinderArgFlag tcb ]
    specified_tvs = [ binderVar tcb
                    | tcb <- tcbs, Specified == tyConBinderArgFlag tcb ]

    pp_inf  = parens (text "namely:" <+> pprTyVars inferred_tvs)
    pp_spec = parens (text "namely:" <+> pprTyVars specified_tvs)

    pp_tc_kind = text "Inferred kind:" <+> ppr tc <+> dcolon <+> ppr_untidy (tyConKind tc)
    ppr_untidy ty = pprIfaceType (toIfaceType ty)
      -- We need ppr_untidy here because pprType will tidy the type, which
      -- will turn the bogus kind we are trying to report
      --     T :: forall (a::k) k (b::k) -> blah
      -- into a misleadingly sanitised version
      --     T :: forall (a::k) k1 (b::k1) -> blah

    extra
      | null inferred_tvs && null specified_tvs
      = empty
      | null inferred_tvs
      = hang (text "NB: Specified variables")
           2 (sep [pp_spec, text "always come first"])
      | null specified_tvs
      = hang (text "NB: Inferred variables")
           2 (sep [pp_inf, text "always come first"])
      | otherwise
      = hang (text "NB: Inferred variables")
           2 (vcat [ sep [ pp_inf, text "always come first"]
                   , sep [text "then Specified variables", pp_spec]])

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
fvType (FunTy _ w arg res)   = fvType w ++ fvType arg ++ fvType res
fvType (ForAllTy (Bndr tv _) ty)
  = fvType (tyVarKind tv) ++
    filter (/= tv) (fvType ty)
fvType (CastTy ty _)         = fvType ty
fvType (CoercionTy {})       = []

fvTypes :: [Type] -> [TyVar]
fvTypes tys                = concatMap fvType tys

sizeType :: Type -> Int
-- Size of a type: the number of variables and constructors
sizeType ty | Just exp_ty <- tcView ty = sizeType exp_ty
sizeType (TyVarTy {})      = 1
sizeType (TyConApp tc tys) = 1 + sizeTyConAppArgs tc tys
sizeType (LitTy {})        = 1
sizeType (AppTy fun arg)   = sizeType fun + sizeType arg
sizeType (FunTy _ w arg res) = sizeType w + sizeType arg + sizeType res + 1
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
-- See #4200.
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
    || isEqPredClass cls
    || cls `hasKey` typeableClassKey
    || cls `hasKey` coercibleTyConKey

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
