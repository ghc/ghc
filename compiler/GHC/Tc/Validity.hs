{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Tc.Validity (
  Rank(..), UserTypeCtxt(..), checkValidType, checkValidMonoType,
  checkValidTheta,
  checkValidInstance, checkValidInstHead, validDerivPred,
  checkTySynRhs, checkEscapingKind,
  checkValidCoAxiom, checkValidCoAxBranch,
  checkFamPatBinders, checkTyFamEqnValidityInfo,
  checkValidTyFamEqn, checkValidAssocTyFamDeflt, checkConsistentFamInst,
  checkTyConTelescope,
  ) where

import GHC.Prelude
import GHC.Data.FastString

import GHC.Data.Maybe

-- friends:
import GHC.Tc.Utils.Unify    ( tcSubTypeAmbiguity )
import GHC.Tc.Solver         ( simplifyAmbiguityCheck )
import GHC.Tc.Instance.Class ( matchGlobalInst, ClsInstResult(..), AssocInstInfo(..) )
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Rank
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Instance.FunDeps
import GHC.Tc.Instance.Family
import GHC.Tc.Zonk.TcType

import GHC.Builtin.Types
import GHC.Builtin.Names
import GHC.Builtin.Uniques  ( mkAlphaTyVarUnique )

import GHC.Core.Type
import GHC.Core.Unify ( typesAreApart, tcMatchTyX_BM, BindFlag(..) )
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.Predicate
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr
import GHC.Core.TyCo.Tidy
import GHC.Core.FamInstEnv ( isDominatedBy, injectiveBranches
                           , InjectivityCheckResult(..) )

import GHC.Hs
import GHC.Driver.Session
import qualified GHC.LanguageExtensions as LangExt

import GHC.Types.Error
import GHC.Types.Basic   ( TypeOrKind(..), UnboxedTupleOrSum(..)
                         , unboxedTupleOrSumExtension )
import GHC.Types.Name
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Var     ( VarBndr(..), isInvisibleFunArg, mkTyVar, tyVarName )
import GHC.Types.SourceFile
import GHC.Types.SrcLoc
import GHC.Types.TyThing ( TyThing(..) )
import GHC.Types.Unique.Set( isEmptyUniqSet )

import GHC.Utils.FV
import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import GHC.Data.List.SetOps

import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import Control.Monad
import Data.Foldable
import Data.Function
import Data.List        ( (\\) )
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty(..) )

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

Note [The squishiness of the ambiguity check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What about this?
   g :: C [a] => Int
Is every call to 'g' ambiguous?  After all, we might have
   instance C [a] where ...
at the call site.  So maybe that type is ok!  Indeed even f's
quintessentially ambiguous type might, just possibly be callable:
with -XFlexibleInstances we could have
  instance C a where ...
and now a call could be legal after all!  Well, we'll reject this
unless the instance is available *here*.

But even that's not quite right. Even a function with an utterly-ambiguous
type like f :: Eq a => Int -> Int
is still callable if you are prepared to use visible type application,
thus (f @Bool x).

In short, the ambiguity check is a good-faith attempt to say "you are likely
to have trouble if your function has this type"; it is NOT the case that
"you can't call this function without giving a type error".

See also Note [Ambiguity check and deep subsumption] in GHC.Tc.Utils.Unify.

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
  = do { traceTc "Ambiguity check for {" (ppr ty)
         -- Solve the constraints eagerly because an ambiguous type
         -- can cause a cascade of further errors.  Since the free
         -- tyvars are skolemised, we can safely use tcSimplifyTop
       ; allow_ambiguous <- xoptM LangExt.AllowAmbiguousTypes
       ; unless allow_ambiguous $
         do { (_wrap, wanted) <- addErrCtxt (AmbiguityCheckCtxt ctxt allow_ambiguous) $
                                 captureConstraints $
                                 tcSubTypeAmbiguity ctxt ty ty
                                 -- See Note [Ambiguity check and deep subsumption]
                                 -- in GHC.Tc.Utils.Unify
            ; simplifyAmbiguityCheck ty wanted }

       ; traceTc "} Done ambiguity check for" (ppr ty) }

  | otherwise
  = return ()

wantAmbiguityCheck :: UserTypeCtxt -> Bool
wantAmbiguityCheck ctxt
  = case ctxt of  -- See Note [When we don't check for ambiguity]
      GhciCtxt {}  -> False
      TySynCtxt {} -> False
      TypeAppCtxt  -> False
      StandaloneKindSigCtxt{} -> False
      _            -> True

-- | Check whether the type signature contains custom type errors,
-- and fail if so.
--
-- Note that some custom type errors are acceptable:
--
--   - in the RHS of a type synonym, e.g. to allow users to define
--     type synonyms for custom type errors with large messages (#20181),
--   - inside a type family application, as a custom type error
--     might evaporate after performing type family reduction (#20241).
checkUserTypeError :: UserTypeCtxt -> Type -> TcM ()
-- Very unsatisfactorily (#11144) we need to tidy the type
-- because it may have come from an /inferred/ signature, not a
-- user-supplied one.  This is really only a half-baked fix;
-- the other errors in checkValidType don't do tidying, and so
-- may give bad error messages when given an inferred type.
checkUserTypeError ctxt ty
  | TySynCtxt {} <- ctxt  -- Do not complain about TypeError on the
  = return ()             -- RHS of type synonyms. See #20181

  | Just msg <- deepUserTypeError_maybe ty
  = do { env0 <- liftZonkM tcInitTidyEnv
       ; let (env1, tidy_msg) = tidyOpenTypeX env0 msg
       ; failWithTcM (env1, TcRnUserTypeError tidy_msg) }
  | otherwise
  = return ()


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
-- Assumes argument is well-kinded;
--    that is, checkValidType doesn't need to do kind checking
-- Not used for instance decls; checkValidInstance instead
checkValidType ctxt ty
  = do { traceTc "checkValidType" (ppr ty <+> text "::" <+> ppr (typeKind ty))
       ; rankn_flag  <- xoptM LangExt.RankNTypes
       ; impred_flag <- xoptM LangExt.ImpredicativeTypes
       ; let gen_rank :: Rank -> Rank
             gen_rank r | rankn_flag = ArbitraryRank
                        | otherwise  = r

             rank1 = gen_rank r1
             rank0 = gen_rank MonoTypeRankZero

             r1 = LimitedRank True MonoTypeRankZero

             rank
               = case ctxt of
                 DefaultDeclCtxt-> MustBeMonoType
                 PatSigCtxt     -> rank0
                 TySynCtxt _    -> rank0

                 ExprSigCtxt {} -> rank1
                 KindSigCtxt    -> rank1
                 StandaloneKindSigCtxt{} -> rank1
                 TypeAppCtxt | impred_flag -> ArbitraryRank
                             | otherwise   -> MonoTypeTyConArg
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

                 TyVarBndrKindCtxt {} -> rank0
                 RuleBndrTypeCtxt{}   -> rank1
                 DataKindCtxt _       -> rank1
                 TySynKindCtxt _      -> rank1
                 TyFamResKindCtxt _   -> rank1

                 _              -> panic "checkValidType"
                                          -- Can't happen; not used for *user* sigs

       ; env <- liftZonkM $ tcInitOpenTidyEnv (tyCoVarsOfTypeList ty)
       ; expand <- initialExpandMode
       ; let ve = ValidityEnv{ ve_tidy_env = env, ve_ctxt = ctxt
                             , ve_rank = rank, ve_expand = expand }

       -- Check the internal validity of the type itself
       -- Fail if bad things happen, else we misleading
       -- (and more complicated) errors in checkAmbiguity
       ; checkNoErrs $
         do { check_type ve ty
            ; checkUserTypeError ctxt ty
            ; traceTc "done ct" (ppr ty) }

       -- Check for ambiguous types.  See Note [When to call checkAmbiguity]
       -- NB: this will happen even for monotypes, but that should be cheap;
       --     and there may be nested foralls for the subtype test to examine
       ; checkAmbiguity ctxt ty

       ; traceTc "checkValidType done" (ppr ty <+> text "::" <+> ppr (typeKind ty)) }

checkValidMonoType :: Type -> TcM ()
-- Assumes argument is fully zonked
checkValidMonoType ty
  = do { env <- liftZonkM $ tcInitOpenTidyEnv (tyCoVarsOfTypeList ty)
       ; expand <- initialExpandMode
       ; let ve = ValidityEnv{ ve_tidy_env = env, ve_ctxt = SigmaCtxt
                             , ve_rank = MustBeMonoType, ve_expand = expand }
       ; check_type ve ty }

checkTySynRhs :: UserTypeCtxt -> TcType -> TcM ()
checkTySynRhs ctxt ty
  | returnsConstraintKind actual_kind
  = do { ck <- xoptM LangExt.ConstraintKinds
       ; if ck
         then  when (isConstraintLikeKind actual_kind)
                    (do { dflags <- getDynFlags
                        ; expand <- initialExpandMode
                        ; check_pred_ty emptyTidyEnv dflags ctxt expand ty })
         else addErrTcM ( emptyTidyEnv
                        , TcRnIllegalConstraintSynonymOfKind (tidyType emptyTidyEnv actual_kind)
                        ) }

  | otherwise
  = return ()
  where
    actual_kind = typeKind ty

{- Note [Check for escaping result kind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:
  type T :: TYPE (BoxedRep l)
  data T = MkT
This is not OK: we get
  MkT :: forall l. T @l :: TYPE (BoxedRep l)
which is ill-kinded.

For ordinary /user-written/ type signatures f :: blah, we make this
check as part of kind-checking the type signature in tcHsSigType; see
Note [Escaping kind in type signatures] in GHC.Tc.Gen.HsType.

But in two other places we need to check for an escaping result kind:

* For data constructors we check the type piecemeal, and there is no
  very convenient place to do it.  For example, note that it only
  applies for /nullary/ constructors.  If we had
    data T = MkT Int
  then the type of MkT would be MkT :: forall l. Int -> T @l, which is fine.

  So we make the check in checkValidDataCon.

* When inferring the type of a function, there is no user-written type
  that we are checking.  Forgetting this led to #22743.  Now we call
  checkEscapingKind in GHC.Tc.Gen.Bind.mkInferredPolyId

Historical note: we used to do the escaping-kind check in
checkValidType (#20929 discusses), but that is now redundant.
-}

checkEscapingKind :: Type -> TcM ()
-- Give a sigma-type (forall a1 .. an. ty), where (ty :: ki),
-- check that `ki` does not mention any of the binders a1..an.
-- Otherwise the type is ill-kinded
-- See Note [Check for escaping result kind]
checkEscapingKind poly_ty
  | (tvs, tau) <- splitForAllTyVars poly_ty
  , let tau_kind = typeKind tau
  , Nothing <- occCheckExpand tvs tau_kind
    -- Ensure that none of the tvs occur in the kind of the forall
    -- /after/ expanding type synonyms.
    -- See Note [Phantom type variables in kinds] in GHC.Core.Type
  = failWithTc $ TcRnForAllEscapeError poly_ty tau_kind
  | otherwise
  = return ()

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
typeOrKindCtxt (RuleBndrTypeCtxt {})= OnlyTypeCtxt
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

-- These cases are also described in Note [No constraints in kinds], so any
-- change here should be reflected in that note.
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

check_type _ (TyVarTy _)
  = return ()

check_type ve (AppTy ty1 ty2)
  = do  { check_type ve ty1
        ; check_arg_type False ve ty2 }

check_type ve ty@(TyConApp tc tys)
  | isTypeSynonymTyCon tc || isTypeFamilyTyCon tc
  = check_syn_tc_app ve ty tc tys

  -- Check for unboxed tuples and unboxed sums: these
  -- require the corresponding extension to be enabled.
  | isUnboxedTupleTyCon tc
  = check_ubx_tuple_or_sum UnboxedTupleType ve ty tys
  | isUnboxedSumTyCon tc
  = check_ubx_tuple_or_sum UnboxedSumType   ve ty tys

  | otherwise
  = do { -- We require DataKinds to use a type constructor in a kind, unless it
         -- is exempted (e.g., Type, TYPE, etc., which is checked by
         -- isKindTyCon) or a `type data` type constructor.
         -- See Note [Checking for DataKinds].
         unless (isKindTyCon tc || isTypeDataTyCon tc) $
         checkDataKinds ve ty
       ; mapM_ (check_arg_type False ve) tys }

check_type ve ty@(LitTy {}) =
  -- Type-level literals are forbidden from appearing in kinds unless DataKinds
  -- is enabled. See Note [Checking for DataKinds].
  checkDataKinds ve ty

check_type ve (CastTy ty _) = check_type ve ty

-- Check for rank-n types, such as (forall x. x -> x) or (Show x => x).
--
-- Critically, this case must come *after* the case for TyConApp.
-- See Note [Liberal type synonyms].
check_type ve@(ValidityEnv{ ve_tidy_env = env
                          , ve_rank = rank, ve_expand = expand }) ty
  | not (null tvbs && null theta)
  = do  { traceTc "check_type" (ppr ty $$ ppr rank)
        ; checkTcM (forAllAllowed rank) $
          let (env1, tidy_ty) = tidyOpenTypeX env ty
          in  (env1, TcRnForAllRankErr rank tidy_ty)
                -- Reject e.g. (Maybe (?x::Int => Int)),
                -- with a decent error message

        ; checkConstraintsOK ve theta ty
                -- Reject forall (a :: Eq b => b). blah
                -- In a kind signature we don't allow constraints

        ; checkVdqOK ve tvbs ty
                -- Reject visible, dependent quantification in the type of a
                -- term (e.g., `f :: forall a -> a -> Maybe a`)

        ; check_valid_theta env' SigmaCtxt expand theta
                -- Allow     type T = ?x::Int => Int -> Int
                -- but not   type T = ?x::Int

        ; check_type (ve{ve_tidy_env = env'}) tau
                -- Allow foralls to right of arrow

        -- Note: skolem-escape in types (e.g. forall r (a::r). a) is handled
        --       by tcHsSigType and the constraint solver, so no need to
        --       check it here; c.f. #20929
        }
  where
    (tvbs, phi)   = tcSplitForAllTyVarBinders ty
    (theta, tau)  = tcSplitPhiTy phi
    (env', _)     = tidyForAllTyBinders env tvbs

check_type (ve@ValidityEnv{ ve_tidy_env = env, ve_ctxt = ctxt
                          , ve_rank = rank })
           ty@(FunTy _ mult arg_ty res_ty)
  = do  { failIfTcM (not (linearityAllowed ctxt) && not (isManyTy mult))
                     (env, TcRnLinearFuncInKind (tidyType env ty))
        ; check_type (ve{ve_rank = arg_rank}) arg_ty
        ; check_type (ve{ve_rank = res_rank}) res_ty }
  where
    (arg_rank, res_rank) = funArgResRank rank

check_type _ ty@(ForAllTy {}) = pprPanic "check_type" (ppr ty)
check_type _ ty@(CoercionTy {}) = pprPanic "check_type" (ppr ty)

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
      = assertPpr (isTypeSynonymTyCon tc) (ppr tc) $
        case coreView ty of
         Just ty' -> addErrCtxt (TySynErrCtxt tc) $
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
check_ubx_tuple_or_sum :: UnboxedTupleOrSum -> ValidityEnv -> KindOrType -> [KindOrType] -> TcM ()
check_ubx_tuple_or_sum tup_or_sum (ve@ValidityEnv{ve_tidy_env = env}) ty tys
  = do  { ub_thing_allowed <- xoptM $ unboxedTupleOrSumExtension tup_or_sum
        ; checkTcM ub_thing_allowed
            (env, TcRnUnboxedTupleOrSumTypeFuncArg tup_or_sum (tidyType env ty))

          -- Unboxed tuples and sums are forbidden from appearing in kinds
          -- unless DataKinds is enabled. See Note [Checking for DataKinds].
        ; checkDataKinds ve ty

        ; impred <- xoptM LangExt.ImpredicativeTypes
        ; let rank' = if impred then ArbitraryRank else MonoTypeTyConArg
                -- c.f. check_arg_type
                -- However, args are allowed to be unlifted, or
                -- more unboxed tuples or sums, so can't use check_arg_ty
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
                        _ | type_syn       -> MonoTypeSynArg
                        MustBeMonoType     -> MustBeMonoType  -- Monotype, regardless
                        _other | impred    -> ArbitraryRank
                               | otherwise -> MonoTypeTyConArg
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
checkConstraintsOK :: ValidityEnv -> ThetaType -> Type -> TcM ()
checkConstraintsOK ve theta ty
  | null theta                         = return ()
  | allConstraintsAllowed (ve_ctxt ve) = return ()
  | otherwise -- We are unambiguously in a kind; see
              -- Note [No constraints in kinds]
  = failWithTcM (env, TcRnConstraintInKind (tidyType env ty))
  where env = ve_tidy_env ve

checkVdqOK :: ValidityEnv -> [TyVarBinder] -> Type -> TcM ()
checkVdqOK ve tvbs ty = do
  required_type_arguments <- xoptM LangExt.RequiredTypeArguments
  checkTcM (required_type_arguments || vdqAllowed ctxt || no_vdq)
           (env, TcRnVDQInTermType (Just (tidyType env ty)))
  where
    no_vdq = all (isInvisibleForAllTyFlag . binderFlag) tvbs
    ValidityEnv{ve_tidy_env = env, ve_ctxt = ctxt} = ve

-- | Check for a DataKinds violation in a kind context.
-- See @Note [Checking for DataKinds]@.
--
-- Note that emitting DataKinds errors from the typechecker is a fairly recent
-- addition to GHC (introduced in GHC 9.10), and in order to prevent these new
-- errors from breaking users' code, we temporarily downgrade these errors to
-- warnings. (This is why we use 'diagnosticTcM' below.) See
-- @Note [Checking for DataKinds] (Wrinkle: Migration story for DataKinds
-- typechecker errors)@.
checkDataKinds :: ValidityEnv -> Type -> TcM ()
checkDataKinds (ValidityEnv{ ve_ctxt = ctxt, ve_tidy_env = env }) ty = do
  data_kinds <- xoptM LangExt.DataKinds
  diagnosticTcM
    (not (data_kinds || typeLevelUserTypeCtxt ctxt)) $
    (env, TcRnDataKindsError KindLevel (Right (tidyType env ty)))

{- Note [No constraints in kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC does not allow constraints in kinds. Equality constraints
in kinds were allowed from GHC 8.0, but this "feature" was removed
as part of Proposal #547 (https://github.com/ghc-proposals/ghc-proposals/pull/547),
which contains further context and motivation for the removal.

The lack of constraints in kinds is enforced by checkConstraintsOK, which
uses the UserTypeCtxt to determine if we are unambiguously checking a kind.
There are two ambiguous contexts (constructor BothTypeAndKindCtxt of TypeOrKindCtxt)
as written in typeOfKindCtxt:
  - TySynCtxt: this is the RHS of a type synonym. We check the expansion of type
    synonyms for constraints, so this is handled at the usage site of the synonym.
  - GhciCtxt: This is the type in a :kind command. A constraint here does not cause
    any trouble, because the type cannot be used to classify a type.

Beyond these two cases, we also promote data constructors. We check for constraints
in data constructor types in GHC.Tc.Gen.HsType.tcTyVar.

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

Note [Checking for DataKinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Checking whether a piece of code requires -XDataKinds or not is surprisingly
complicated, so here is a specification (adapted from #22141) for what
-XDataKinds does and does not allow. First, some definitions:

* A user-written type (i.e. part of the source text of a program) is in a
  /kind context/ if it follows a `::` in:
  * A standalone kind signature, e.g.,
      type T :: Nat -> Type
  * A kind signature in a type, e.g.:
    - forall (a :: Nat -> Type). blah
    - type F = G :: Nat -> Type
    - etc.
  * A result kind signature in a type declaration, e.g.:
    - data T a :: Nat -> Type where ...
    - type family Fam :: Nat -> Type
    - etc.

* All other contexts where types can appear are referred to as /type contexts/.

* The /kind type constructors/ are (see GHC.Core.TyCon.isKindTyCon):
  * TYPE and Type
  * CONSTRAINT and Constraint
  * LiftedRep
  * RuntimeRep, Levity, and their data constructors
  * Multiplicity and its data construtors
  * VecCount, VecElem, and their data constructors

* A `type data` type constructor is defined using the -XTypeData extension, such
  as the T in `type data T = A | B`.

* The following are rejected in type contexts unless -XDataKinds is enabled:
  * Promoted data constructors (e.g., 'Just), except for those data constructors
    listed under /kind type constructors/
  * Promoted list or tuple syntax (e.g., '[Int, Bool] or '(Int, Bool))
  * Type-level literals (e.g., 42, "hello", or 'a' at the type level)

* The following are rejected in kind contexts unless -XDataKinds is enabled:
  * Everything that is rejected in a type context.
  * Any type constructor that is not a kind type constructor or a `type data`
    type constructor (e.g., Maybe, [], Char, Nat, Symbol, etc.)

    Note that this includes rejecting occurrences of non-kind type construtors
    in type synomym (or type family) applications, even it the expansion would
    be legal. For example:

      type T a = Type
      f :: forall (x :: T Int). blah

    Here the `Int` in `T Int` is rejected even though the expansion is just
    `Type`. This is consistent with, for example, rejecting `T (forall a. a->a)`
    without -XImpredicativeTypes.

    This check only occurs in kind contexts. It is always permissible to mention
    type synonyms in a type context without enabling -XDataKinds, even if the
    type synonym expands to something that would otherwise require -XDataKinds.

Because checking for DataKinds in a kind context requires looking beneath type
synonyms, it is natural to implement these checks in checkValidType, which has
the necessary machinery to check for language extensions in the presence of
type synonyms. For the exact same reason, checkValidType is *not* a good place
to check for DataKinds in a type context, since we deliberately do not want to
look beneath type synonyms there. As a result, we check for DataKinds in two
different places in the code:

* We check for DataKinds violations in kind contexts in the typechecker. See
  checkDataKinds in this module.
* We check for DataKinds violations in type contexts in the renamer. See
  checkDataKinds in GHC.Rename.HsType and check_data_kinds in GHC.Rename.Pat.

  Note that the renamer can also catch "obvious" kind-level violations such as
  `data Dat :: Proxy 42 -> Type` (where 42 is not hidden beneath a type
  synonym), so we also catch a subset of kind-level violations in the renamer
  to allow for earlier reporting of these errors.

-----
-- Wrinkle: Migration story for DataKinds typechecker errors
-----

As mentioned above, DataKinds is checked in two different places: the renamer
and the typechecker. The checks in the renamer have been around since DataKinds
was introduced. The checks in the typechecker, on the other hand, are a fairly
recent addition, having been introduced in GHC 9.10. As such, it is possible
that there are some programs in the wild that (1) do not enable DataKinds, and
(2) were accepted by a previous GHC version, but would now be rejected by the
new DataKinds checks in the typechecker.

To prevent the new DataKinds checks in the typechecker from breaking users'
code, we temporarily allow programs to compile if they violate a DataKinds
check in the typechecker, but GHC will emit a warning if such a violation
occurs. Users can then silence the warning by enabling DataKinds in the module
where the affected code lives. It is fairly straightforward to distinguish
between DataKinds violations arising from the renamer versus the typechecker,
as TcRnDataKindsError (the error message type classifying all DataKinds errors)
stores an Either field that is Left when the error comes from the renamer and
Right when the error comes from the typechecker.

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
    do { env <- liftZonkM $ tcInitOpenTidyEnv (tyCoVarsOfTypesList theta)
       ; expand <- initialExpandMode
       ; check_valid_theta env ctxt expand theta }

-------------------------
check_valid_theta :: TidyEnv -> UserTypeCtxt -> ExpandMode
                  -> [PredType] -> TcM ()
check_valid_theta _ _ _ []
  = return ()
check_valid_theta env ctxt expand theta
  = do { dflags <- getDynFlags
       ; traceTc "check_valid_theta" (ppr theta)
       ; mapM_ (check_pred_ty env dflags ctxt expand) theta }

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
         = MonoTypeConstraint

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
  | Just pred' <- coreView pred  -- Switch on under_syn when going under a
                                 -- synonym (#9838, yuk)
  = check_pred_help True env dflags ctxt pred'

  | otherwise  -- A bit like classifyPredType, but not the same
               -- E.g. we treat (~) like (~#); and we look inside tuples
  = case classifyPredType pred of
      ClassPred cls tys
        | isCTupleClass cls   -> check_tuple_pred under_syn env dflags ctxt pred tys
        | otherwise           -> check_class_pred env dflags ctxt pred cls tys

      EqPred _ _ _      -> pprPanic "check_pred_help" (ppr pred)
              -- EqPreds, such as (t1 ~# t2) or (t1 ~R# t2), don't even have kind Constraint
              -- and should never appear before the '=>' of a type.  Thus
              --     f :: (a ~# b) => blah
              -- is wrong.  For user written signatures, it'll be rejected by kind-checking
              -- well before we get to validity checking.  For inferred types we are careful
              -- to box such constraints in GHC.Tc.Utils.TcType.pickQuantifiablePreds, as described
              -- in Note [Lift equality constraints when quantifying] in GHC.Tc.Solver

      ForAllPred _ theta head -> check_quant_pred env dflags ctxt pred theta head
      _                       -> return ()

check_quant_pred :: TidyEnv -> DynFlags -> UserTypeCtxt
                 -> PredType -> ThetaType -> PredType -> TcM ()
check_quant_pred env dflags ctxt pred theta head_pred
  = addErrCtxt (QuantifiedCtCtxt pred) $
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
            _                 -> failWithTcM (env, TcRnBadQuantPredHead (tidyType env pred))

         -- Check for termination
       ; unless (xopt LangExt.UndecidableInstances dflags) $
         checkInstTermination theta head_pred
    }

check_tuple_pred :: Bool -> TidyEnv -> DynFlags -> UserTypeCtxt -> PredType -> [PredType] -> TcM ()
check_tuple_pred under_syn env dflags ctxt pred ts
  = do { -- See Note [ConstraintKinds in predicates]
         checkTcM (under_syn || xopt LangExt.ConstraintKinds dflags)
                  (env, TcRnIllegalTupleConstraint (tidyType env pred))
       ; mapM_ (check_pred_help under_syn env dflags ctxt) ts }
    -- This case will not normally be executed because without
    -- -XConstraintKinds tuple types are only kind-checked as *

{- Note [ConstraintKinds in predicates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Don't check for -XConstraintKinds under a type synonym, because that
was done at the type synonym definition site; see #9838
e.g.   module A where
          type C a = (Eq a, Ix a)   -- Needs -XConstraintKinds
       module B where
          import A
          f :: C a => a -> a        -- Does *not* need -XConstraintKinds
-}

-------------------------
check_class_pred :: TidyEnv -> DynFlags -> UserTypeCtxt
                 -> PredType -> Class -> [TcType] -> TcM ()
check_class_pred env dflags ctxt pred cls tys
  | isEqualityClass cls  -- (~) and (~~) and Coercible are classified as classes,
                         -- but here we want to treat them as equalities
  = -- Equational constraints are valid in all contexts, and
    -- we do not need to check e.g. for FlexibleContexts here, so just do nothing
    -- We used to require TypeFamilies/GADTs for equality constraints,
    -- but not anymore (GHC Proposal #371)
   return ()

  | isIPClass cls
  = do { check_arity
       ; checkTcM (okIPCtxt ctxt) (env, TcRnIllegalImplicitParam (tidyType env pred)) }

  | otherwise     -- Includes Coercible
  = do { check_arity
       ; checkSimplifiableClassConstraint env dflags ctxt cls tys
       ; checkTcM arg_tys_ok (env, TcRnNonTypeVarArgInConstraint (tidyType env pred)) }
  where
    check_arity = checkTc (tys `lengthIs` classArity cls)
                          (tyConArityErr (classTyCon cls) tys)

    -- Check the arguments of a class constraint
    flexible_contexts = xopt LangExt.FlexibleContexts     dflags
    arg_tys_ok = case ctxt of
        SpecInstCtxt -> True    -- {-# SPECIALISE instance Eq (T Int) #-} is fine
        InstDeclCtxt {} -> checkValidClsArgs flexible_contexts cls tys
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
  = do { result <- matchGlobalInst dflags False cls tys Nothing
       ; case result of
           OneInst { cir_what = what }
              -> addDiagnosticTc (TcRnSimplifiableConstraint pred what)
           _  -> return () }
  where
    pred = tidyType env (mkClassPred cls tys)

{- Note [Simplifiable given constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A type signature like
   f :: Eq [(a,b)] => a -> b
is very fragile, for reasons described at length in GHC.Tc.Solver.Dict
Note [Instance and Given overlap].  As that Note discusses, for the
most part the clever stuff in GHC.Tc.Solver.Dict means that we don't use a
top-level instance if a local Given might fire, so there is no
fragility. But if we /infer/ the type of a local let-binding, things
can go wrong (#11948 is an example, discussed in the Note).

So this warning is switched on only if we have NoMonoLocalBinds; in
that case the warning discourages users from writing simplifiable
class constraints.

The warning only fires if the constraint in the signature
matches the top-level instances in only one way, and with no
unifiers -- that is, under the same circumstances that
GHC.Tc.Instance.Class.matchInstEnv fires an interaction with the top
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
okIPCtxt (ExprSigCtxt {})       = True
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
okIPCtxt (RuleBndrTypeCtxt {})  = False
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

checkThetaCtxt :: UserTypeCtxt -> ThetaType -> TidyEnv -> ZonkM (TidyEnv, ErrCtxtMsg)
checkThetaCtxt ctxt theta env
  = return (env, ThetaCtxt ctxt (tidyTypes env theta))

tyConArityErr :: TyCon -> [TcType] -> TcRnMessage
-- For type-constructor arity errors, be careful to report
-- the number of /visible/ arguments required and supplied,
-- ignoring the /invisible/ arguments, which the user does not see.
-- (e.g. #10516)
tyConArityErr tc tks
  = TcRnArityMismatch (ATyCon tc) tc_type_arity tc_type_args
  where
    vis_tks = filterOutInvisibleTypes tc tks

    -- tc_type_arity = number of *type* args expected
    -- tc_type_args  = number of *type* args encountered
    tc_type_arity = count isVisibleTyConBinder (tyConBinders tc)
    tc_type_args  = length vis_tks

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
       ; hsc_src  <- tcHscSource
       ; check_special_inst_head dflags hsc_src ctxt clas cls_args
       ; checkValidTypePats (classTyCon clas) cls_args
       }

{-

Note [Instances of built-in classes in signature files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

User defined instances for KnownNat, KnownSymbol, KnownChar,
and Typeable are disallowed
  -- they are generated when needed by GHC itself, on-the-fly.

However, if they occur in a Backpack signature file, they have an
entirely different meaning. To illustrate, suppose in M.hsig we see

  signature M where
    data T :: Nat
    instance KnownNat T

That says that any module satisfying M.hsig must provide a KnownNat
instance for T.  We absolutely need that instance when compiling a
module that imports M.hsig: see #15379 and
Note [Fabricating Evidence for Literals in Backpack] in GHC.Tc.Instance.Class.

Hence, checkValidInstHead accepts a user-written instance declaration
in hsig files, where `is_sig` is True.

-}

check_special_inst_head :: DynFlags -> HscSource -> UserTypeCtxt
                        -> Class -> [Type] -> TcM ()
-- Wow!  There are a surprising number of ad-hoc special cases here.
-- TODO: common up the logic for special typeclasses (see GHC ticket #20441).
check_special_inst_head dflags hs_src ctxt clas cls_args

  -- Abstract classes cannot have instances, except in hs-boot or signature files.
  | isAbstractClass clas
  , hs_src == HsSrcFile
  = fail_with_inst_err $ IllegalInstanceHead
                       $ InstHeadAbstractClass (className clas)

  -- Complain about hand-written instances of built-in classes
  -- Typeable, KnownNat, KnownSymbol, Coercible, HasField.

  -- Disallow hand-written Typeable instances, except that we
  -- allow a standalone deriving declaration: they are no-ops,
  -- and we warn about them in GHC.Tc.Deriv.deriveStandalone.
  | clas_nm == typeableClassName
  , not (hs_src == HsigFile)
    -- Note [Instances of built-in classes in signature files]
  , hand_written_bindings
  = fail_with_inst_err $ IllegalSpecialClassInstance clas False

  -- Handwritten instances of KnownNat/KnownChar/KnownSymbol
  -- are forbidden outside of signature files (#12837).
  -- Derived instances are forbidden completely (#21087).
     -- FIXME: DataToTag instances in signature files don't actually work yet
  | clas_nm `elem` [ knownNatClassName, knownSymbolClassName
                   , knownCharClassName, dataToTagClassName ]
  , (not (hs_src == HsigFile) && hand_written_bindings) || derived_instance
    -- Note [Instances of built-in classes in signature files]
  = fail_with_inst_err $ IllegalSpecialClassInstance clas False

  -- For the most part we don't allow
  -- instances for (~), (~~), or Coercible;
  -- but we DO want to allow them in quantified constraints:
  --   f :: (forall a b. Coercible a b => Coercible (m a) (m b)) => ...m...
  | clas_nm `elem`
    [ heqTyConName, eqTyConName, coercibleTyConName
    , withDictClassName, unsatisfiableClassName ]
  , not quantified_constraint
  = fail_with_inst_err $ IllegalSpecialClassInstance clas False

  -- Check for hand-written Generic instances (disallowed in Safe Haskell)
  | clas_nm `elem` genericClassNames
  , hand_written_bindings
  =  do { when (safeLanguageOn dflags) $
           fail_with_inst_err $ IllegalSpecialClassInstance clas True
        ; when (safeInferOn dflags) (recordUnsafeInfer emptyMessages) }

  | clas_nm == hasFieldClassName
  , not quantified_constraint
  -- Don't do any validity checking for HasField contexts
  -- inside quantified constraints (#20989): the validity checks
  -- only apply to user-written instances.
  = checkHasFieldInst clas cls_args

  | isCTupleClass clas
  = do
    -- Since we're now declaring instances for constraint tuples in
    -- GHC.Classes, this check must exclude that file.
    this_mod <- fmap tcg_mod getGblEnv
    when (this_mod /= gHC_CLASSES) (failWithTc (TcRnTupleConstraintInst clas))

  -- Check language restrictions on the args to the class
  | check_h98_arg_shape
  , Just illegal_head <- mb_ty_args_type
  = fail_with_inst_err $ IllegalInstanceHead illegal_head

  | otherwise
  = pure ()
  where

    fail_with_inst_err err =
      failWithTc $ TcRnIllegalInstance
                 $ IllegalClassInstance
                    (TypeThing $ mkClassPred clas cls_args)
                 $ err

    clas_nm = getName clas
    ty_args = filterOutInvisibleTypes (classTyCon clas) cls_args

    hand_written_bindings
      = case ctxt of
          InstDeclCtxt standalone -> not standalone
          SpecInstCtxt            -> False
          DerivClauseCtxt         -> False
          SigmaCtxt               -> False
          _                       -> True

    derived_instance
      = case ctxt of
            InstDeclCtxt standalone -> standalone
            DerivClauseCtxt         -> True
            _                       -> False

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

    mb_ty_args_type
      | not (xopt LangExt.TypeSynonymInstances dflags)
      , not (all tcInstHeadTyNotSynonym ty_args)
      = Just InstHeadTySynArgs

      | not (xopt LangExt.FlexibleInstances dflags)
      , not (all tcInstHeadTyAppAllTyVars ty_args)
      = Just InstHeadNonTyVarArgs

      | length ty_args /= 1
      , not (xopt LangExt.MultiParamTypeClasses dflags)
      , not (xopt LangExt.NullaryTypeClasses dflags && null ty_args)
      = Just InstHeadMultiParam

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
        tys'' | tc `hasKey` fUNTyConKey
              , ManyTy : tys_t <- tys'
              = tys_t
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
             tvs = mapMaybe getTyVar_maybe tys

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

-- | See Note [Validity checking of HasField instances]
checkHasFieldInst :: Class -> [Type] -> TcM ()
checkHasFieldInst cls tys@[_k_ty, _r_rep, _a_rep, lbl_ty, r_ty, _a_ty] =
  case splitTyConApp_maybe r_ty of
    Nothing -> add_err IllegalHasFieldInstanceNotATyCon
    Just (tc, _)
      | isFamilyTyCon tc
                  -> add_err IllegalHasFieldInstanceFamilyTyCon
      | otherwise -> case isStrLitTy lbl_ty of
       Just lbl
         | let lbl_str = FieldLabelString lbl
         , isJust (lookupTyConFieldLabel lbl_str tc)
         -> add_err $ IllegalHasFieldInstanceTyConHasField tc lbl_str
         | otherwise
         -> return ()
       Nothing
         -- If the label is not a literal, we need to ensure it can't unify
         -- with any of the field labels of the TyCon.
         | null (tyConFieldLabels tc)
           -- No field labels at all: nothing to check.
         || typesAreApart (typeKind lbl_ty) typeSymbolKind
           -- If the label has a type whose kind can't unify with Symbol,
           -- then it definitely can't unify with any of the field labels.
         -> return ()
         | otherwise
         -> add_err $ IllegalHasFieldInstanceTyConHasFields tc lbl_ty
  where
    add_err err = addErrTc $ TcRnIllegalInstance
                           $ IllegalClassInstance
                               (TypeThing $ mkClassPred cls tys)
                           $ IllegalHasFieldInstance err

checkHasFieldInst _ tys = pprPanic "checkHasFieldInst" (ppr tys)

{- Note [Casts during validity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the (bogus)
     instance Eq Char#
We elaborate to  'Eq (Char# |> UnivCo(hole))'  where the hole is an
insoluble equality constraint for Type ~ TYPE WordRep.  We'll report the insoluble
constraint separately, but we don't want to *also* complain that Eq is
not applied to a type constructor.  So we look gaily look through
CastTys here.

Another example:  Eq (Either a).  Then we actually get a cast in
the middle:
   Eq ((Either |> g) a)


Note [Validity checking of HasField instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The HasField class has magic constraint solving behaviour (see Note
[HasField instances] in GHC.Tc.Instance.Class).  However, we permit users to
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
In a 'derived' instance declaration, we *infer* the context. It's a bit unclear
what rules we should apply for this; the Haskell report is silent. Obviously,
constraints like `Eq a` are fine, but what about

  data T f a = MkT (f a) deriving Eq

where we'd get an `Eq (f a)` constraint. That's probably fine too.

On the other hand, we don't want to allow *every* inferred constraint, as there
are some particularly complex constraints that are tricky to handle. If GHC
encounters a constraint that is too complex, it will reject it, and you will
have to use StandaloneDeriving to manually specify the instance context that
you want.

There are two criteria for a constraint inferred by a `deriving` clause to be
considered valid, which are described below as (VD1) and (VD2). (Here, "VD"
stands for "valid deriving".) `validDerivPred` implements these checks. While
`validDerivPred` is similar to other things defined in GHC.Tc.Deriv.Infer, we
define it here in GHC.Tc.Validity because it is quite similar to
`checkInstTermination`.

-----------------------------------
-- (VD1) The Paterson conditions --
-----------------------------------

Constraints must satisfy the Paterson conditions (see Note [Paterson
conditions]) to be valid. Not only does this check for termination (of course),
but it also deals with a nasty corner case:

  instance C a b => D (T a) where ...

Note that `b` isn't a parameter of T.  This gives rise to all sorts of
problems; in particular, it's hard to compare solutions for equality when
finding the fixpoint, and that means the
GHC.Tc.Deriv.Infer.simplifyInstanceContexts loop does not converge.
See #5287 and #21302.

Another common situation in which a derived instance's context fails to meet
the Paterson conditions is when a constraint mentions a type variable more
often than the instance head, e.g.,

  data Fix f = In (f (Fix f)) deriving Eq

This would result in the following derived `Eq` instance:

  instance Eq (f (Fix f)) => Eq (Fix f)

Because `f` is mentioned more often in the `Eq (f (Fix f))` constraint than in
the instance head `Eq (Fix f)`, GHC rejects this instance.

This is a somewhat contentious restriction, and some have suggested that
instances like this one should be accepted if UndecidableInstances is enabled
(see #15868 for one such discussion). If we *do* lift this restriction in the
future, we should make sure to still meet the termination conditions. If not,
the deriving mechanism generates larger and larger constraints. Example:

  data Succ a = S a
  data Seq a = Cons a (Seq (Succ a)) | Nil deriving Show

Note the lack of a Show instance for Succ.  First we'll generate:

  instance (Show (Succ a), Show a) => Show (Seq a)

and then:

  instance (Show (Succ (Succ a)), Show (Succ a), Show a) => Show (Seq a)

and so on. Instead we want to complain of no instance for (Show (Succ a)).

---------------------------------
-- (VD2) No exotic constraints --
---------------------------------

A constraint must satisfy one of the following properties in order to be valid:

* It is a `ClassPred` of the form `C a_1 ... a_n`, where C is a type class
  constructor and a_1, ..., a_n are either raw type variables or applications
  of type variables (e.g., `f a`).
* It is an `IrredPred` of the form `c a_1 ... a_n`, where `c` is a raw type
  variable and a_1, ..., a_n are either raw type variables or applications of
  type variables (e.g., `f a`).

If a constraint does not meet either of these properties, it is considered
*exotic*. A constraint will be exotic if it contains:

* Other type constructors (besides the class in a `ClassPred`),
* Foralls, or
* Equalities

A common form of exotic constraint is one that mentions another type
constructor. For example, given the following:

  data NotAShowInstance
  data Foo = MkFoo Int NotAShowInstance deriving Show

GHC would attempt to generate the following derived `Show` instance:

  instance (Show Int, Show NotAShowInstance) => Show Foo

Note that because there is a top-level `Show Int` instance, GHC is able to
simplify away the inferred `Show Int` constraint. However, it cannot do the
same for the `Show NotAShowInstance` constraint. One possibility would be to
generate this instance:

  instance Show NotAShowInstance => Show Foo

But this is almost surely not what we want most of the time. For this reason,
we reject the constraint above as being exotic.

Here are some other interesting examples:

* Derived instances whose instance context would mention TypeError, such as the
  code from the deriving/should_fail/T14339 test case, are exotic. For example:

    newtype Foo = Foo Int

    class Bar a where
      bar :: a

    instance (TypeError (Text "Boo")) => Bar Foo where
      bar = undefined

    newtype Baz = Baz Foo
      deriving Bar

  The `deriving Bar` clause would generate this instance:

    instance TypeError (Text "Boo") => Bar Baz

  The instance context is exotic, as `TypeError` is not a type constructor, and
  `Text "Boo"` is not an application of type variables. As such, GHC rejects
  it. This has the desirable side effect of causing the TypeError to fire in
  the resulting error message.

* The following `IrredPred`s are not exotic:

    instance c => C (T c a)
    instance c a => C (T c a)

  This `IrredPred`, however, *is* exotic:

    instance c NotAShowInstance => C (T c)

  This is rejected for the same reasons that we do not permit a `ClassPred`
  with a type constructor argument, such as the `Show NotAShowInstance` example
  above.

As part of implementing this check, GHC calls `tyConsOfType` on the arguments
of the constraint, ensuring that there are no other type constructors.
Wrinkle: for `ClassPred`s, we look only at the /visible/ arguments of the class
type constructor. Including the non-visible arguments can cause the following,
perfectly valid instance to be rejected:

  class Category (cat :: k -> k -> Type) where ...
  newtype T (c :: Type -> Type -> Type) a b = MkT (c a b)
  instance Category c => Category (T c) where ...

since the first argument to `Category` is a non-visible `Type`, which has a type
constructor! See #11833.

Note [Equality class instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can't have users writing instances for the equality classes. But we
still need to be able to write instances for them ourselves. So we allow
instances only in the defining module.
-}

validDerivPred :: PatersonSize -> PredType -> Bool
-- See Note [Valid 'deriving' predicate]
validDerivPred head_size pred
  = case classifyPredType pred of
            EqPred {}     -> False  -- Reject equality constraints (VD2)
            ForAllPred {} -> False  -- Rejects quantified predicates (VD2)

            ClassPred cls tys -> check_size (pSizeClassPred cls tys)        -- (VD1)
                              && isEmptyUniqSet (tyConsOfTypes visible_tys) -- (VD2)
                where
                  -- See the wrinkle about visible arguments in (VD2)
                  visible_tys = filterOutInvisibleTypes (classTyCon cls) tys

            IrredPred {} -> check_size (pSizeType pred)        -- (VD1)
                         && isEmptyUniqSet (tyConsOfType pred) -- (VD2)

  where
    check_size pred_size = isNothing (pred_size `ltPatersonSize` head_size)
        -- Check (VD1)

{-
************************************************************************
*                                                                      *
\subsection{Checking instance for termination}
*                                                                      *
************************************************************************
-}

{- Note [Paterson conditions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Paterson Conditions ensure termination of instance resolution.
Given an instance declaration
   instance (..., C t1.. tn, ...) => D s1 .. sm

we check that each constraint in the context of the instance is
"Paterson-smaller" than the instance head.  The underlying idea of
Paterson-smaller is that

    For any ground substitution S, for each constraint P in the
    context, S(P) has fewer type constructors, counting repetitions,
    than the head S(H)

We implement this check by checking the following syntactic conditions:

(PC1) No type variable has more (shallow) occurrences in P than in H.

      (If not, a substitution that replaces that variable with a big type
      would make P have many more type constructors than H. Side note: we
      could in principle skip this test for a variable of kind Bool,
      since there are no big ground types we can substitute for it.)

(PC2) The constraint P has fewer constructors and variables (taken
      together and counting repetitions) than the head H.  This size
      metric is computed by sizeType.

      (A substitution that replaces each variable with Int demonstrates
      the need.)

(PC3) The constraint P mentions no type functions.

      (A type function application can in principle expand to a type of
      arbitrary size, and so are rejected out of hand.  See #15172.)

(See Section 5 of "Understanding functional dependencies via Constraint
Handling Rules", JFP Jan 2007; and the user manual section "Instance
termination rules".)

We measure "size" with the data type PatersonSize, in GHC.Tc.Utils.TcType.
  data PatersonSize
    = PS_TyFam TyCon
    | PS_Vanilla { ps_tvs :: [TyVar]  -- Free tyvars, including repetitions;
                 , ps_size :: Int}    -- Number of type constructors and variables

* ps_tvs deals with (PC1)
* ps_size deals with (PC2)
* PS_TyFam deals with (PC3)

Note [Tuples in checkInstTermination]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider these two ways of giving the same instance decl (#8359):

   instance (Eq a, Num a) => C (T a)

   type X a = (Eq a, Num a)
   instance X a => C (T a)

In the former, `checkInstTermination` will check the size of two predicates:
(Eq a) and (Num a). In the latter, it will see only one: (X a). But we want
to treat the latter like the former.

So the `check` function in `checkInstTermination`, we simply recurse
if we find a constraint tuple.
-}


checkValidInstance :: UserTypeCtxt -> LHsSigType GhcRn -> Type -> TcM ()
checkValidInstance ctxt hs_type ty = case tau of
  -- See Note [Instances and constraint synonyms]
  TyConApp tc inst_tys
    | Just clas <- tyConClass_maybe tc
    -> do
        { setSrcSpanA head_loc $
          checkValidInstHead ctxt clas inst_tys

        ; traceTc "checkValidInstance {" (ppr ty)

        ; env0 <- liftZonkM $ tcInitTidyEnv
        ; expand <- initialExpandMode
        ; check_valid_theta env0 ctxt expand theta

        -- The Termination and Coverage Conditions
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

        ; case checkInstCoverage undecidable_ok clas theta inst_tys of
            IsValid
              -> return ()   -- Check succeeded
            NotValid coverageInstErr
              -> addErrTc $ mk_err
                          $ IllegalInstanceFailsCoverageCondition clas coverageInstErr

        ; traceTc "End checkValidInstance }" empty }
    | otherwise
    -> failWithTc $ mk_err $ IllegalInstanceHead
                           $ InstHeadNonClassHead
                           $ InstNonClassTyCon (tyConName tc) (fmap tyConName $ tyConFlavour tc)
  _ -> failWithTc $ mk_err $ IllegalInstanceHead
                           $ InstHeadNonClassHead InstNonTyCon
  where
    (theta, tau) = splitInstTyForValidity ty

    mk_err err = TcRnIllegalInstance
               $ IllegalClassInstance (TypeThing tau) err

        -- The location of the "head" of the instance
    head_loc = getLoc (getLHsInstDeclHead hs_type)

-- | Split an instance type of the form @forall tvbs. inst_ctxt => inst_head@
-- and return @(inst_ctxt, inst_head)@. This function makes no attempt to look
-- through type synonyms. See @Note [Instances and constraint synonyms]@.
splitInstTyForValidity :: Type -> (ThetaType, Type)
splitInstTyForValidity = split_context [] . drop_foralls
  where
    -- This is like 'dropForAlls', except that it does not look through type
    -- synonyms.
    drop_foralls :: Type -> Type
    drop_foralls (ForAllTy (Bndr _tv argf) ty)
      | isInvisibleForAllTyFlag argf = drop_foralls ty
    drop_foralls ty = ty

    -- This is like 'tcSplitPhiTy', except that it does not look through type
    -- synonyms.
    split_context :: ThetaType -> Type -> (ThetaType, Type)
    split_context preds (FunTy { ft_af = af, ft_arg = pred, ft_res = tau })
      | isInvisibleFunArg af = split_context (pred:preds) tau
    split_context preds ty = (reverse preds, ty)

checkInstTermination :: ThetaType -> TcPredType -> TcM ()
-- See Note [Paterson conditions]
checkInstTermination theta head_pred
  = check_preds emptyVarSet theta
  where
   head_size = pSizeType head_pred
   -- This is inconsistent and probably wrong.  pSizeType does not filter out
   -- invisible type args (making the instance head look big), whereas the use of
   -- pSizeClassPredX below /does/ filter them out (making the tested constraints
   -- look smaller). I'm sure there is non termination lurking here, but see #15177
   -- for why I didn't change it. See Note [Invisible arguments and termination]
   -- in GHC.Tc.Utils.TcType

   check_preds :: VarSet -> [PredType] -> TcM ()
   check_preds foralld_tvs preds = mapM_ (check foralld_tvs) preds

   check :: VarSet -> PredType -> TcM ()
   check foralld_tvs pred
     = case classifyPredType pred of
         EqPred {}      -> return ()  -- See #4200.
         IrredPred {}   -> check2 (pSizeTypeX foralld_tvs pred)

         ClassPred cls tys
           | isCTupleClass cls  -- See Note [Tuples in checkInstTermination]
           -> check_preds foralld_tvs tys

           | otherwise          -- Other ClassPreds
           -> check2 (pSizeClassPredX foralld_tvs cls tys)

         ForAllPred tvs _ head_pred'
           -> check (foralld_tvs `extendVarSetList` tvs) head_pred'
              -- Termination of the quantified predicate itself is checked
              -- when the predicates are individually checked for validity

      where
        check2 pred_size
          = case pred_size `ltPatersonSize` head_size of
              Just pc_failure -> failWithTc $ TcRnPatersonCondFailure pc_failure InInstanceDecl pred head_pred
              Nothing         -> return ()

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

For similar reasons, we do not use tcSplitSigmaTy when decomposing the instance
context, as the looks through type synonyms. If we looked through type
synonyms, then it could be possible to write an instance for a type synonym
involving a quantified constraint (see #22570). Instead, we define
splitInstTyForValidity, a specialized version of tcSplitSigmaTy (local to
GHC.Tc.Validity) that does not expand type synonyms.
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
      = do { let dia = TcRnInaccessibleCoAxBranch fam_tc cur_branch
           ; addDiagnosticAt (coAxBranchSpan cur_branch) dia
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
-- See also the separate 'checkFamPatBinders' which performs scoping checks
-- on a type family equation.
-- (It's separate because it expects 'TyFamEqnValidityInfo', which comes from a
-- separate place e.g. for associated type defaults.)
checkValidCoAxBranch :: TyCon -> CoAxBranch -> TcM ()
checkValidCoAxBranch fam_tc
                    (CoAxBranch { cab_lhs = typats
                                , cab_rhs = rhs, cab_loc = loc })
  = setSrcSpan loc $
    checkValidTyFamEqn fam_tc typats rhs

-- | Do validity checks on a type family equation, including consistency
-- with any enclosing class instance head, termination, and lack of
-- polytypes.
--
-- See also the separate 'checkFamPatBinders' which performs scoping checks
-- on a type family equation.
-- (It's separate because it expects 'TyFamEqnValidityInfo', which comes from a
-- separate place e.g. for associated type defaults.)
checkValidTyFamEqn :: TyCon    -- ^ of the type family
                   -> [Type]   -- ^ Type patterns
                   -> Type     -- ^ Rhs
                   -> TcM ()
checkValidTyFamEqn fam_tc typats rhs
  = do { checkValidTypePats fam_tc typats

         -- Check for oversaturated visible kind arguments in a type family
         -- equation.
         -- See Note [Oversaturated type family equations]
       ; when (isTypeFamilyTyCon fam_tc) $
           case drop (tyConArity fam_tc) typats of
             [] -> pure ()
             spec_arg:_ ->
               addErr (TcRnOversaturatedVisibleKindArg spec_arg)

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
    pats_vis :: [ForAllTyFlag]
    pats_vis = tyConForAllTyFlags fam_tc pats

    -- Checks that a pattern on the LHS of a default is a type
    -- variable. If so, return the underlying type variable, and if
    -- not, throw an error.
    -- See Note [Type-checking default assoc decls]
    extract_tv :: Type          -- The particular type pattern from which to
                                -- extrace its underlying type variable
               -> ForAllTyFlag  -- The visibility of the type pattern
                                -- (only used for error message purposes)
               -> TcM TyVar
    extract_tv pat pat_vis =
      case getTyVar_maybe pat of
        Just tv -> pure tv
        Nothing -> failWithTc $ TcRnIllegalInstance
                              $ IllegalFamilyInstance
                              $ InvalidAssoc $ InvalidAssocDefault
                              $ AssocDefaultBadArgs fam_tc pats
                              $ AssocDefaultNonTyVarArg (pat, pat_vis)

    -- Checks that no type variables in an associated default declaration are
    -- duplicated. If that is the case, throw an error.
    -- See Note [Type-checking default assoc decls]
    check_all_distinct_tvs ::
         [(TyVar, ForAllTyFlag)] -- The type variable arguments in the associated
                            -- default declaration, along with their respective
                            -- visibilities (the latter are only used for error
                            -- message purposes)
      -> TcM ()
    check_all_distinct_tvs cpt_tvs_vis =
      let dups = findDupsEq ((==) `on` fst) cpt_tvs_vis in
      traverse_
        (\d -> failWithTc $ TcRnIllegalInstance
                          $ IllegalFamilyInstance
                          $ InvalidAssoc $ InvalidAssocDefault
                          $ AssocDefaultBadArgs fam_tc pats
                          $ AssocDefaultDuplicateTyVars d)
        dups

checkFamInstRhs :: TyCon -> [Type]         -- LHS
                -> [(TyCon, [Type])]       -- type family calls in RHS
                -> [TcRnMessage]
-- Ensure that the type family instance terminates. Specifically:
-- ensure that each type family application in the RHS is
--    (TF1) a call to a stuck family like (TypeError ...) or Any
--          See Note [Stuck type families] in GHC.Tc.Utils.TcType
-- or (TF2) obeys the Paterson conditions, namely:
--          - strictly smaller than the lhs,
--          - mentions no type variable more often than the lhs, and
--          - does not contain any further type family applications
checkFamInstRhs lhs_tc lhs_tys famInsts
  = mapMaybe check famInsts
  where
   lhs_size = pSizeTypes lhs_tys
   check (tc, tys)
      | not (isStuckTypeFamily tc)                                   -- (TF1)
      , Just pc_failure <- pSizeTypes tys `ltPatersonSize` lhs_size  -- (TF2)
      = Just $ TcRnPatersonCondFailure pc_failure InTyFamEquation (TyConApp lhs_tc lhs_tys) (TyConApp tc tys)
      | otherwise
      = Nothing

-----------------

-- | Perform scoping check on a type family equation.
--
-- See 'TyFamEqnValidityInfo'.
checkTyFamEqnValidityInfo :: TyCon -> TyFamEqnValidityInfo -> TcM ()
checkTyFamEqnValidityInfo fam_tc = \ case
  NoVI -> return ()
  VI { vi_loc          = loc
     , vi_qtvs         = qtvs
     , vi_non_user_tvs = non_user_tvs
     , vi_pats         = pats
     , vi_rhs          = rhs } ->
    setSrcSpan loc $
    checkFamPatBinders fam_tc qtvs non_user_tvs pats rhs

-- | Check binders for a type or data family declaration.
--
-- Specifically, this function checks for:
--
--  - type variables used on the RHS but not bound (explicitly or implicitly)
--    in the LHS,
--  - variables bound by a forall in the LHS but not used in the RHS.
--
-- See Note [Check type family instance binders].
checkFamPatBinders :: TyCon
                   -> [TcTyVar]   -- ^ Bound on LHS of family instance
                   -> TyVarSet    -- ^ non-user-written tyvars
                   -> [TcType]    -- ^ LHS patterns
                   -> TcType      -- ^ RHS
                   -> TcM ()
checkFamPatBinders fam_tc qtvs non_user_tvs pats rhs
  = do { traceTc "checkFamPatBinders" $
         vcat [ debugPprType (mkTyConApp fam_tc pats)
              , ppr (mkTyConApp fam_tc pats)
              , text "rhs:" <+> ppr rhs
              , text "qtvs:" <+> ppr qtvs
              , text "rhs_fvs:" <+> ppr (fvVarSet rhs_fvs)
              , text "cpt_tvs:" <+> ppr cpt_tvs
              , text "inj_cpt_tvs:" <+> ppr inj_cpt_tvs
              , text "bad_rhs_tvs:" <+> ppr bad_rhs_tvs
              , text "bad_qtvs:" <+> ppr (map ifiqtv bad_qtvs) ]

         -- Check for implicitly-bound tyvars, mentioned on the
         -- RHS but not bound on the LHS
         --    data T            = MkT (forall (a::k). blah)
         --    data family D Int = MkD (forall (a::k). blah)
         -- In both cases, 'k' is not bound on the LHS, but is used on the RHS
         -- We catch the former in kcDeclHeader, and the latter right here
         -- See Note [Check type family instance binders]
       ; check_tvs (FamInstRHSOutOfScopeTyVars (Just (fam_tc, pats, dodgy_tvs)))
                   (map tyVarName bad_rhs_tvs)

         -- Check for explicitly forall'd variable that is not bound on LHS
         --    data instance forall a.  T Int = MkT Int
         -- See Note [Unused explicitly bound variables in a family pattern]
         -- See Note [Check type family instance binders]
       ; check_tvs FamInstLHSUnusedBoundTyVars bad_qtvs
       }
  where
    rhs_fvs = tyCoFVsOfType rhs

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

    -- Bound but not used at all
    bad_qtvs    = mapMaybe bad_qtv_maybe qtvs

    bad_qtv_maybe qtv
      | not_bound_in_pats
      = let reason
              | dodgy
              = InvalidFamInstQTvDodgy
              | used_in_rhs
              = InvalidFamInstQTvNotBoundInPats
              | otherwise
              = InvalidFamInstQTvNotUsedInRHS
        in Just $ InvalidFamInstQTv
                    { ifiqtv = qtv
                    , ifiqtv_user_written = not $ qtv `elemVarSet` non_user_tvs
                    , ifiqtv_reason = reason
                    }
      | otherwise
      = Nothing
        where
          not_bound_in_pats = not $ qtv `elemVarSet` inj_cpt_tvs
          dodgy             = not_bound_in_pats && qtv `elemVarSet` cpt_tvs
          used_in_rhs       = qtv `elemVarSet` fvVarSet rhs_fvs

    -- Used on RHS but not bound on LHS
    bad_rhs_tvs = filterOut ((`elemVarSet` inj_cpt_tvs) <||> (`elem` qtvs)) (fvVarList rhs_fvs)

    dodgy_tvs   = cpt_tvs `minusVarSet` inj_cpt_tvs

    check_tvs mk_err tvs
      = for_ (NE.nonEmpty tvs) $ \ ne_tvs@(tv0 :| _) ->
        addErrAt (getSrcSpan tv0) $
        TcRnIllegalInstance $ IllegalFamilyInstance $
        mk_err ne_tvs

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
            ((tf_is_invis_arg, tf_tc, tf_args):_) ->
              failWithTc $ TcRnIllegalInstance $
                IllegalFamilyApplicationInInstance inst_ty
                  tf_is_invis_arg tf_tc tf_args }
  where
    inst_ty = mkTyConApp tc pat_ty_args

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
       ; checkTc (Just (classTyCon clas) == tyConAssoc_maybe fam_tc) $
          TcRnIllegalInstance $ IllegalFamilyInstance $
            InvalidAssoc $ InvalidAssocInstance $
            AssocNotInThisClass clas fam_tc

       ; check_match arg_triples
       }
  where
    (ax_tvs, ax_arg_tys, _) = etaExpandCoAxBranch branch

    arg_triples :: [(Type,Type, ForAllTyFlag)]
    arg_triples = [ (cls_arg_ty, at_arg_ty, vis)
                  | (fam_tc_tv, vis, at_arg_ty)
                       <- zip3 (tyConTyVars fam_tc)
                               (tyConForAllTyFlags fam_tc ax_arg_tys)
                               ax_arg_tys
                  , Just cls_arg_ty <- [lookupVarEnv mini_env fam_tc_tv] ]

    -- Fiddling around to arrange that wildcards unconditionally print as "_"
    -- We only need to print the LHS, not the RHS at all
    -- See Note [Printing conflicts with class header]
    (tidy_env1, _) = tidyVarBndrs emptyTidyEnv inst_tvs
    (tidy_env2, _) = tidyCoAxBndrsForUser tidy_env1 (ax_tvs \\ inst_tvs)

    mk_wildcard at_tv = mkTyVarTy (mkTyVar tv_name (tyVarKind at_tv))
    tv_name = mkInternalName (mkAlphaTyVarUnique 1) (mkTyVarOccFS (fsLit "_")) noSrcSpan

    -- For check_match, bind_me, see
    -- Note [Matching in the consistent-instantiation check]
    check_match :: [(Type,Type,ForAllTyFlag)] -> TcM ()
    check_match triples = go emptySubst emptySubst triples

    go _ _ [] = return ()
    go lr_subst rl_subst ((ty1,ty2,vis):triples)
      | Just lr_subst1 <- tcMatchTyX_BM bind_me lr_subst ty1 ty2
      , Just rl_subst1 <- tcMatchTyX_BM bind_me rl_subst ty2 ty1
      = go lr_subst1 rl_subst1 triples
      | otherwise
      = addErrTc $ TcRnIllegalInstance $ IllegalFamilyInstance
                 $ InvalidAssoc $ InvalidAssocInstance
                 $ AssocTyVarsDontMatch vis fam_tc exp_tys act_tys

    -- Expected/actual family argument types (for error messages)
    exp_tys =
      [ case lookupVarEnv mini_env at_tv of
          Just cls_arg_ty -> tidyType tidy_env2 cls_arg_ty
          Nothing         -> mk_wildcard at_tv
      | at_tv <- tyConTyVars fam_tc ]
    act_tys = tidyTypes tidy_env2 ax_arg_tys

    -- The /scoped/ type variables from the class-instance header
    -- should not be alpha-renamed.  Inferred ones can be.
    no_bind_set = mkVarSet inst_tvs
    bind_me tv _ty | tv `elemVarSet` no_bind_set = DontBindMe
                   | otherwise                   = BindMe


{- Note [Check type family instance binders]
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
   class C b where
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
with such a variable. At least with @x :: forall a. Int@ we can use visible
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
    addErr $ TcRnBadTyConTelescope tc

  | otherwise
  = return ()
  where
    tcbs = tyConBinders tc

    (_, bad_scope) = foldl add_one (emptyVarSet, False) tcbs

    add_one :: TelescopeAcc -> TyConBinder -> TelescopeAcc
    add_one (bound, bad_scope) tcb
      = ( bound `extendVarSet` tv
        , bad_scope || not (isEmptyVarSet (fkvs `minusVarSet` bound)) )
      where
        tv = binderVar tcb
        fkvs = tyCoVarsOfType (tyVarKind tv)
