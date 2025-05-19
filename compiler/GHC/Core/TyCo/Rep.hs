{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_HADDOCK not-home #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998
\section[GHC.Core.TyCo.Rep]{Type and Coercion - friends' interface}

Note [The Type-related module hierarchy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  GHC.Core.Class
  GHC.Core.Coercion.Axiom
  GHC.Core.TyCon           imports GHC.Core.{Class, Coercion.Axiom}
  GHC.Core.TyCo.Rep        imports GHC.Core.{Class, Coercion.Axiom, TyCon}
  GHC.Core.TyCo.Ppr        imports GHC.Core.TyCo.Rep
  GHC.Core.TyCo.FVs        imports GHC.Core.TyCo.Rep
  GHC.Core.TyCo.Subst      imports GHC.Core.TyCo.{Rep, FVs, Ppr}
  GHC.Core.TyCo.Tidy       imports GHC.Core.TyCo.{Rep, FVs}
  GHC.Builtin.Types.Prim   imports GHC.Core.TyCo.Rep ( including mkTyConTy )
  GHC.Core.Coercion        imports GHC.Core.Type
-}

-- We expose the relevant stuff from this module via the Type module
module GHC.Core.TyCo.Rep (

        -- * Types
        Type(..),

        TyLit(..),
        KindOrType, Kind,
        RuntimeRepType, LevityType,
        KnotTied,
        PredType, ThetaType, FRRType,     -- Synonyms
        ForAllTyFlag(..), FunTyFlag(..),

        -- * Coercions
        Coercion(..), CoSel(..), FunSel(..),
        UnivCoProvenance(..),
        CoercionHole(..), coHoleCoVar, setCoHoleCoVar,
        CoercionN, CoercionR, CoercionP, KindCoercion,
        MCoercion(..), MCoercionR, MCoercionN,

        -- * Functions over types
        mkNakedTyConTy, mkTyVarTy, mkTyVarTys,
        mkTyCoVarTy, mkTyCoVarTys,
        mkFunTy, mkNakedFunTy,
        mkVisFunTy, mkScaledFunTys,
        mkInvisFunTy, mkInvisFunTys,
        tcMkVisFunTy, tcMkInvisFunTy, tcMkScaledFunTy, tcMkScaledFunTys,
        mkForAllTy, mkForAllTys, mkInvisForAllTys,
        mkPiTy, mkPiTys,
        mkVisFunTyMany, mkVisFunTysMany,
        nonDetCmpTyLit, cmpTyLit,

        -- * Functions over coercions
        pickLR,

        -- ** Analyzing types
        TyCoFolder(..), foldTyCo, noView,

        -- * Sizes
        typeSize, typesSize, coercionSize,

        -- * Multiplicities
        Scaled(..), scaledMult, scaledThing, mapScaledType, Mult
    ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Core.TyCo.Ppr ( pprType, pprCo, pprTyLit )
import {-# SOURCE #-} GHC.Builtin.Types
import {-# SOURCE #-} GHC.Core.TyCo.FVs( tyCoVarsOfType ) -- Use in assertions
import {-# SOURCE #-} GHC.Core.Type( chooseFunTyFlag, typeKind, typeTypeOrConstraint )

   -- Transitively pulls in a LOT of stuff, better to break the loop

-- friends:
import GHC.Types.Var
import GHC.Types.Var.Set( elemVarSet )
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom

-- others
import GHC.Builtin.Names

import GHC.Types.Basic ( LeftOrRight(..), pickLR )
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Binary

-- libraries
import qualified Data.Data as Data hiding ( TyCon )
import Data.IORef ( IORef )   -- for CoercionHole
import Control.DeepSeq

{- **********************************************************************
*                                                                       *
                        Type
*                                                                       *
********************************************************************** -}

-- | The key representation of types within the compiler

type KindOrType = Type -- See Note [Arguments to type constructors]

-- | The key type representing kinds in the compiler.
type Kind = Type

-- | Type synonym used for types of kind RuntimeRep.
type RuntimeRepType = Type

-- | Type synonym used for types of kind Levity.
type LevityType = Type

-- A type with a syntactically fixed RuntimeRep, in the sense
-- of Note [Fixed RuntimeRep] in GHC.Tc.Utils.Concrete.
type FRRType = Type

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in GHC.Core.Lint
data Type
  -- See Note [Non-trivial definitional equality]
  = TyVarTy Var -- ^ Vanilla type or kind variable (*never* a coercion variable)

  | AppTy
        Type
        Type            -- ^ Type application to something other than a 'TyCon'. Parameters:
                        --
                        --  1) Function: must /not/ be a 'TyConApp' or 'CastTy',
                        --     must be another 'AppTy', or 'TyVarTy'
                        --     See Note [Respecting definitional equality] \(EQ1) about the
                        --     no 'CastTy' requirement
                        --
                        --  2) Argument type

  | TyConApp
        TyCon
        [KindOrType]    -- ^ Application of a 'TyCon', including newtypes /and/ synonyms.
                        -- Invariant: saturated applications of 'FunTyCon' must
                        -- use 'FunTy' and saturated synonyms must use their own
                        -- constructors. However, /unsaturated/ 'FunTyCon's
                        -- do appear as 'TyConApp's.
                        -- Parameters:
                        --
                        -- 1) Type constructor being applied to.
                        --
                        -- 2) Type arguments. Might not have enough type arguments
                        --    here to saturate the constructor.
                        --    Even type synonyms are not necessarily saturated;
                        --    for example unsaturated type synonyms
                        --    can appear as the right hand side of a type synonym.

  | ForAllTy  -- See Note [ForAllTy]
        {-# UNPACK #-} !ForAllTyBinder
           -- ForAllTyBinder: see GHC.Types.Var
           --    Note [VarBndrs, ForAllTyBinders, TyConBinders, and visibility]
        Type
           -- INVARIANT: If the binder is a coercion variable, it must
           --            be mentioned in the Type.
           --            See Note [Unused coercion variable in ForAllTy]
           -- See Note [Why ForAllTy can quantify over a coercion variable]

  | FunTy      -- ^ FUN m t1 t2   Very common, so an important special case
                -- See Note [Function types]
     { ft_af   :: FunTyFlag    -- Is this (->/FUN) or (=>) or (==>)?
                                 -- This info is fully specified by the kinds in
                                 --      ft_arg and ft_res
                                 -- Note [FunTyFlag] in GHC.Types.Var

     , ft_mult :: Mult           -- Multiplicity; always Many for (=>) and (==>)
     , ft_arg  :: Type           -- Argument type
     , ft_res  :: Type }         -- Result type

  | LitTy TyLit     -- ^ Type literals are similar to type constructors.

  | CastTy
        Type
        KindCoercion  -- ^ A kind cast. The coercion is always nominal.
                      -- INVARIANT: The cast is never reflexive \(EQ2)
                      -- INVARIANT: The Type is not a CastTy (use TransCo instead) \(EQ3)
                      -- INVARIANT: The Type is not a ForAllTy over a tyvar \(EQ4)
                      -- See Note [Respecting definitional equality]

  | CoercionTy
        Coercion    -- ^ Injection of a Coercion into a type
                    -- This should only ever be used in the RHS of an AppTy,
                    -- in the list of a TyConApp, when applying a promoted
                    -- GADT data constructor

  deriving Data.Data

instance Outputable Type where
  ppr = pprType

-- NOTE:  Other parts of the code assume that type literals do not contain
-- types or type variables.
data TyLit
  = NumTyLit Integer
  | StrTyLit FastString
  | CharTyLit Char
  deriving (Eq, Data.Data)

-- Non-determinism arises due to uniqCompareFS
nonDetCmpTyLit :: TyLit -> TyLit -> Ordering
nonDetCmpTyLit = cmpTyLitWith NonDetFastString

-- Slower than nonDetCmpTyLit but deterministic
cmpTyLit :: TyLit -> TyLit -> Ordering
cmpTyLit = cmpTyLitWith LexicalFastString

{-# INLINE cmpTyLitWith #-}
cmpTyLitWith :: Ord r => (FastString -> r) -> TyLit -> TyLit -> Ordering
cmpTyLitWith _ (NumTyLit  x) (NumTyLit  y) = compare x y
cmpTyLitWith w (StrTyLit  x) (StrTyLit  y) = compare (w x) (w y)
cmpTyLitWith _ (CharTyLit x) (CharTyLit y) = compare x y
cmpTyLitWith _ a b = compare (tag a) (tag b)
  where
    tag :: TyLit -> Int
    tag NumTyLit{}  = 0
    tag StrTyLit{}  = 1
    tag CharTyLit{} = 2

instance Outputable TyLit where
   ppr = pprTyLit

{- Note [Function types]
~~~~~~~~~~~~~~~~~~~~~~~~
FunTy is the constructor for a function type.  Here are the details:

* The primitive function type constructor FUN has kind
     FUN :: forall (m :: Multiplicity) ->
            forall {r1 :: RuntimeRep} {r2 :: RuntimeRep}.
            TYPE r1 ->
            TYPE r2 ->
            Type
  mkTyConApp ensures that we convert a saturated application
    TyConApp FUN [m,r1,r2,t1,t2] into FunTy FTF_T_T m t1 t2
  dropping the 'r1' and 'r2' arguments; they are easily recovered
  from 't1' and 't2'. The FunTyFlag is always FTF_T_T, because
  we build constraint arrows (=>) with e.g. mkPhiTy and friends,
  never `mkTyConApp funTyCon args`.

* For the time being its RuntimeRep quantifiers are left
  inferred. This is to allow for it to evolve.

* Because the RuntimeRep args came first historically (that is,
  the arrow type constructor gained these arguments before gaining
  the Multiplicity argument), we wanted to be able to say
    type (->) = FUN Many
  which we do in library module GHC.Types. This means that the
  Multiplicity argument must precede the RuntimeRep arguments --
  and it means changing the name of the primitive constructor from
  (->) to FUN.

* The multiplicity argument is dependent, because Typeable does not
  support a type such as `Multiplicity -> forall {r1 r2 :: RuntimeRep}. ...`.
  There is a plan to change the argument order and make the
  multiplicity argument nondependent in #20164.

* Re the ft_af field: see Note [FunTyFlag] in GHC.Types.Var
  See Note [Types for coercions, predicates, and evidence]
  This visibility info makes no difference in Core; it matters
  only when we regard the type as a Haskell source type.

Note [Types for coercions, predicates, and evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat differently:

  (a) Predicate types
        Test: isPredTy
        Binders: DictIds
        Kind: Constraint
        Examples: (Eq a), and (a ~ b)

  (b) Coercion types are primitive, unboxed equalities
        Test: isCoVarTy
        Binders: CoVars (can appear in coercions)
        Kind: TYPE (TupleRep [])
        Examples: (t1 ~# t2) or (t1 ~R# t2)

  (c) Evidence types is the type of evidence manipulated by
      the type constraint solver.
        Test: isEvVarType
        Binders: EvVars
        Kind: Constraint or TYPE (TupleRep [])
        Examples: all coercion types and predicate types

Coercion types and predicate types are mutually exclusive,
but evidence types are a superset of both.

When treated as a user type,

  - Predicates (of kind Constraint) are invisible and are
    implicitly instantiated

  - Coercion types, and non-pred evidence types (i.e. not
    of kind Constraint), are just regular old types, are
    visible, and are not implicitly instantiated.

In a FunTy { ft_af = af } and af = FTF_C_T or FTF_C_C, the argument
type is always a Predicate type.

Note [Weird typing rule for ForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here are the typing rules for ForAllTy:

tyvar : Type
inner : TYPE r
tyvar does not occur in r
------------------------------------
ForAllTy (Bndr tyvar vis) inner : TYPE r

inner : TYPE r
------------------------------------
ForAllTy (Bndr covar vis) inner : Type

Note that the kind of the result depends on whether the binder is a
tyvar or a covar. The kind of a forall-over-tyvar is the same as
the kind of the inner type. This is because quantification over types
is erased before runtime. By contrast, the kind of a forall-over-covar
is always Type, because a forall-over-covar is compiled into a function
taking a 0-bit-wide erased coercion argument.

Because the tyvar form above includes r in its result, we must
be careful not to let any variables escape -- thus the last premise
of the rule above.

Note [Arguments to type constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because of kind polymorphism, in addition to type application we now
have kind instantiation. We reuse the same notations to do so.

For example:

  Just (* -> *) Maybe
  Right * Nat Zero

are represented by:

  TyConApp (PromotedDataCon Just) [* -> *, Maybe]
  TyConApp (PromotedDataCon Right) [*, Nat, (PromotedDataCon Zero)]

Important note: Nat is used as a *kind* and not as a type. This can be
confusing, since type-level Nat and kind-level Nat are identical. We
use the kind of (PromotedDataCon Right) to know if its arguments are
kinds or types.

This kind instantiation only happens in TyConApp currently.

Note [Non-trivial definitional equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is ((IO |> co1) Int |> co2) equal to (IO Int)?
Assume
   co1 :: (Type->Type) ~ (Type->Wombat)
   co2 :: Wombat ~ Type
Well, yes.  The casts are just getting in the way.
See also Note [Respecting definitional equality].

So we do this:

(EQTYPE)
  The `eqType` function, which defines Core's type equality relation,
  - /ignores/ casts, and
  - /ignores/ coercion arguments
  - /provided/ two types have the same kind

This allows us to be a little sloppier in keeping track of coercions, which is a
good thing. It also means that eqType does not depend on eqCoercion, which is
also a good thing.

Why is this sensible? That is, why is something different than α-equivalence
appropriate for the implementation of eqType?

Anything smaller than ~ and homogeneous is an appropriate definition for
equality. The type safety of FC depends only on ~. Let's say η : τ ~ σ. Any
expression of type τ can be transmuted to one of type σ at any point by
casting. The same is true of expressions of type σ. So in some sense, τ and σ
are interchangeable.

But let's be more precise. If we examine the typing rules of FC (say, those in
https://richarde.dev/papers/2015/equalities/equalities.pdf)
there are several places where the same metavariable is used in two different
premises to a rule. (For example, see Ty_App.) There is an implicit equality
check here. What definition of equality should we use? By convention, we use
α-equivalence. Take any rule with one (or more) of these implicit equality
checks. Then there is an admissible rule that uses ~ instead of the implicit
check, adding in casts as appropriate.

The only problem here is that ~ is heterogeneous. To make the kinds work out
in the admissible rule that uses ~, it is necessary to homogenize the
coercions. That is, if we have η : (τ : κ1) ~ (σ : κ2), then we don't use η;
we use η |> kind η, which is homogeneous.

The effect of this all is that eqType, the implementation of the implicit
equality check, can use any homogeneous relation that is smaller than ~, as
those rules must also be admissible.

A more drawn out argument around all of this is presented in Section 7.2 of
Richard E's thesis (http://richarde.dev/papers/2016/thesis/eisenberg-thesis.pdf).

What would go wrong if we insisted on the casts matching? See the beginning of
Section 8 in the unpublished paper above. Theoretically, nothing at all goes
wrong. But in practical terms, getting the coercions right proved to be
nightmarish. And types would explode: during kind-checking, we often produce
reflexive kind coercions. When we try to cast by these, mkCastTy just discards
them. But if we used an eqType that distinguished between Int and Int |> <*>,
then we couldn't discard -- the output of kind-checking would be enormous,
and we would need enormous casts with lots of CoherenceCo's to straighten
them out.

Would anything go wrong if eqType looked through type families? No, not at
all. But that makes eqType rather hard to implement.

Thus, the guideline for eqType is that it should be the largest
easy-to-implement relation that is still smaller than ~ and homogeneous. The
precise choice of relation is somewhat incidental, as long as the smart
constructors and destructors in Type respect whatever relation is chosen.

Another helpful principle with eqType is this:

 (EQ) If (t1 `eqType` t2) then I can replace t1 by t2 anywhere.

This principle also tells us that eqType must relate only types with the
same kinds.

Interestingly, it must be the case that the free variables of t1 and t2
might be different, even if t1 `eqType` t2. A simple example of this is
if we have both cv1 :: k1 ~ k2 and cv2 :: k1 ~ k2 in the environment.
Then t1 = t |> cv1 and t2 = t |> cv2 are eqType; yet cv1 is in the free
vars of t1 and cv2 is in the free vars of t2. Unless we choose to implement
eqType to be just α-equivalence, this wrinkle around free variables
remains.

Yet not all is lost: we can say that any two equal types share the same
*relevant* free variables. Here, a relevant variable is a shallow
free variable (see Note [Shallow and deep free variables] in GHC.Core.TyCo.FVs)
that does not appear within a coercion. Note that type variables can
appear within coercions (in, say, a Refl node), but that coercion variables
cannot appear outside a coercion. We do not (yet) have a function to
extract relevant free variables, but it would not be hard to write if
the need arises.

Note [Respecting definitional equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note [Non-trivial definitional equality] introduces the property (EQ).
How is this upheld?

Any function that pattern matches on all the constructors will have to
consider the possibility of CastTy. Presumably, those functions will handle
CastTy appropriately and we'll be OK.

More dangerous are the splitXXX functions. Let's focus on splitTyConApp.
We don't want it to fail on (T a b c |> co). Happily, if we have
  (T a b c |> co) `eqType` (T d e f)
then co must be reflexive. Why? eqType checks that the kinds are equal, as
well as checking that (a `eqType` d), (b `eqType` e), and (c `eqType` f).
By the kind check, we know that (T a b c |> co) and (T d e f) have the same
kind. So the only way that co could be non-reflexive is for (T a b c) to have
a different kind than (T d e f). But because T's kind is closed (all tycon kinds
are closed), the only way for this to happen is that one of the arguments has
to differ, leading to a contradiction. Thus, co is reflexive.

Accordingly, by eliminating reflexive casts, splitTyConApp need not worry
about outermost casts to uphold (EQ). Eliminating reflexive casts is done
in mkCastTy. This is (EQ2) below.

Unfortunately, that's not the end of the story. Consider comparing
  (T a b c)      =?       (T a b |> (co -> <Type>)) (c |> co)
These two types have the same kind (Type), but the left type is a TyConApp
while the right type is not. To handle this case, we say that the right-hand
type is ill-formed, requiring an AppTy never to have a casted TyConApp
on its left. It is easy enough to pull around the coercions to maintain
this invariant, as done in Type.mkAppTy. In the example above, trying to
form the right-hand type will instead yield (T a b (c |> co |> sym co) |> <Type>).
Both the casts there are reflexive and will be dropped. Huzzah.

This idea of pulling coercions to the right works for splitAppTy as well.

However, there is one hiccup: it's possible that a coercion doesn't relate two
Pi-types. For example, if we have @type family Fun a b where Fun a b = a -> b@,
then we might have (T :: Fun Type Type) and (T |> axFun) Int. That axFun can't
be pulled to the right. But we don't need to pull it: (T |> axFun) Int is not
`eqType` to any proper TyConApp -- thus, leaving it where it is doesn't violate
our (EQ) property.

In order to detect reflexive casts reliably, we must make sure not
to have nested casts: we update (t |> co1 |> co2) to (t |> (co1 `TransCo` co2)).
This is (EQ3) below.

One other troublesome case is ForAllTy. See Note [Weird typing rule for ForAllTy].
The kind of the body is the same as the kind of the ForAllTy. Accordingly,

  ForAllTy tv (ty |> co)     and     (ForAllTy tv ty) |> co

are `eqType`. But only the first can be split by splitForAllTy. So we forbid
the second form, instead pushing the coercion inside to get the first form.
This is done in mkCastTy.

In sum, in order to uphold (EQ), we need the following invariants:

  (EQ1) No decomposable CastTy to the left of an AppTy,
        where a "decomposable cast" is one that relates
        either a FunTy to a FunTy, or a ForAllTy to a ForAllTy.
  (EQ2) No reflexive casts in CastTy.
  (EQ3) No nested CastTys.
  (EQ4) No CastTy over (ForAllTy (Bndr tyvar vis) body).
        See Note [Weird typing rule for ForAllTy]

These invariants are all documented above, in the declaration for Type.

Note [Equality on FunTys]
~~~~~~~~~~~~~~~~~~~~~~~~~
A (FunTy vis mult arg res) is just an abbreviation for a
  TyConApp funTyCon [mult, arg_rep, res_rep, arg, res]
where
  arg :: TYPE arg_rep
  res :: TYPE res_rep
Note that the vis field of a FunTy appears nowhere in the
equivalent TyConApp. In Core, this is OK, because we no longer
care about the visibility of the argument in a FunTy
(the vis distinguishes between arg -> res and arg => res).
In the type-checker, we are careful not to decompose FunTys
with an invisible argument. See also Note [Decomposing fat arrow c=>t]
in GHC.Core.Type.

In order to compare FunTys while respecting how they could
expand into TyConApps, we must check
the kinds of the arg and the res.

Note [ForAllTy]
~~~~~~~~~~~~~~~
A (ForAllTy (Bndr tcv vis) ty) can quantify over a TyVar or, less commonly, a CoVar.
See Note [Why ForAllTy can quantify over a coercion variable] for why we need the latter.

(FT1) Invariant: See Note [Weird typing rule for ForAllTy]

(FT2) Invariant: in (ForAllTy (Bndr tcv vis) ty),
      if tcv is a CoVar, then vis = coreTyLamForAllTyFlag.
   Visibility is not important for coercion abstractions,
   because they are not user-visible.

(FT3) Invariant: see Note [Unused coercion variable in ForAllTy]

Note [Why ForAllTy can quantify over a coercion variable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ForAllTyBinder in a ForAllTy can be (most often) a TyVar or (rarely)
a CoVar. We support quantifying over a CoVar here in order to support
a homogeneous (~#) relation (someday -- not yet implemented). Here is
the example:

  type (:~~:) :: forall k1 k2. k1 -> k2 -> Type
  data a :~~: b where
    HRefl :: a :~~: a

Assuming homogeneous equality (that is, with
  (~#) :: forall k. k -> k -> TYPE (TupleRep '[])
) after rejigging to make equalities explicit, we get a constructor that
looks like

  HRefl :: forall k1 k2 (a :: k1) (b :: k2).
           forall (cv :: k1 ~# k2). (a |> cv) ~# b
        => (:~~:) k1 k2 a b

Note that we must cast `a` by a cv bound in the same type in order to
make this work out.

See also https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell/phase2
which gives a general road map that covers this space.  Having this feature in
Core does *not* mean we have it in source Haskell.  See #15710 about that.

Note [Unused coercion variable in ForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  \(co:t1 ~# t2). e

What type should we give to the above expression?
  (1) forall (co:t1 ~# t2) -> t
  (2) (t1 ~# t2) -> t

If co is used in t, (1) should be the right choice.
if co is not used in t, we would like to have (1) and (2) equivalent.

However, we want to keep eqType simple and don't want eqType (1) (2) to return
True in any case.

We decide to always construct (2) if co is not used in t.

Thus in mkLamType, we check whether the variable is a coercion
variable (of type (t1 ~# t2), and whether it is un-used in the
body. If so, it returns a FunTy instead of a ForAllTy.

There are cases we want to skip the check. For example, the check is
unnecessary when it is known from the context that the input variable
is a type variable.  In those cases, we use mkForAllTy.

Note [Weird typing rule for ForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is the (truncated) typing rule for the dependent ForAllTy:

  inner : TYPE r
  tyvar is not free in r
  ----------------------------------------
  ForAllTy (Bndr tyvar vis) inner : TYPE r

Note that the kind of `inner` is the kind of the overall ForAllTy. This is
necessary because every ForAllTy over a type variable is erased at runtime.
Thus the runtime representation of a ForAllTy (as encoded, via TYPE rep, in
the kind) must be the same as the representation of the body. We must check
for skolem-escape, though. The skolem-escape would prevent a definition like

  undefined :: forall (r :: RuntimeRep) (a :: TYPE r). a

because the type's kind (TYPE r) mentions the out-of-scope r. Luckily, the real
type of undefined is

  undefined :: forall (r :: RuntimeRep) (a :: TYPE r). HasCallStack => a

and that HasCallStack constraint neatly sidesteps the potential skolem-escape
problem.

If the bound variable is a coercion variable:

  inner : TYPE r
  covar is free in inner
  ------------------------------------
  ForAllTy (Bndr covar vis) inner : Type

Here, the kind of the ForAllTy is just Type, because coercion abstractions
are *not* erased. The "covar is free in inner" premise is solely to maintain
the representation invariant documented in
Note [Unused coercion variable in ForAllTy]. Though there is surface similarity
between this free-var check and the one in the tyvar rule, these two restrictions
are truly unrelated.

-}

-- | A type labeled 'KnotTied' might have knot-tied tycons in it. See
-- Note [Type checking recursive type and class declarations] in
-- "GHC.Tc.TyCl"
type KnotTied ty = ty

{- **********************************************************************
*                                                                       *
                        PredType
*                                                                       *
********************************************************************** -}


-- | A type of the form @p@ of constraint kind represents a value whose type is
-- the Haskell predicate @p@, where a predicate is what occurs before
-- the @=>@ in a Haskell type.
--
-- We use 'PredType' as documentation to mark those types that we guarantee to
-- have this kind.
--
-- It can be expanded into its representation, but:
--
-- * The type checker must treat it as opaque
--
-- * The rest of the compiler treats it as transparent
--
-- Consider these examples:
--
-- > f :: (Eq a) => a -> Int
-- > g :: (?x :: Int -> Int) => a -> Int
-- > h :: (r\l) => {r} => {l::Int | r}
--
-- Here the @Eq a@ and @?x :: Int -> Int@ and @r\l@ are all called \"predicates\"
type PredType = Type

-- | A collection of 'PredType's
type ThetaType = [PredType]

{-
(We don't support TREX records yet, but the setup is designed
to expand to allow them.)

A Haskell qualified type, such as that for f,g,h above, is
represented using
        * a FunTy for the double arrow
        * with a type of kind Constraint as the function argument

The predicate really does turn into a real extra argument to the
function.  If the argument has type (p :: Constraint) then the predicate p is
represented by evidence of type p.


%************************************************************************
%*                                                                      *
            Simple constructors
%*                                                                      *
%************************************************************************

These functions are here so that they can be used by GHC.Builtin.Types.Prim,
which in turn is imported by Type
-}

mkTyVarTy  :: TyVar   -> Type
mkTyVarTy v = assertPpr (isTyVar v) (ppr v <+> dcolon <+> ppr (tyVarKind v)) $
              TyVarTy v

mkTyVarTys :: [TyVar] -> [Type]
mkTyVarTys = map mkTyVarTy -- a common use of mkTyVarTy

mkTyCoVarTy :: TyCoVar -> Type
mkTyCoVarTy v
  | isTyVar v
  = TyVarTy v
  | otherwise
  = CoercionTy (CoVarCo v)

mkTyCoVarTys :: [TyCoVar] -> [Type]
mkTyCoVarTys = map mkTyCoVarTy

infixr 3 `mkFunTy`, `mkInvisFunTy`, `mkVisFunTyMany`

mkNakedFunTy :: FunTyFlag -> Kind -> Kind -> Kind
-- See Note [Naked FunTy] in GHC.Builtin.Types
-- Always Many multiplicity; kinds have no linearity
mkNakedFunTy af arg res
 =  FunTy { ft_af   = af, ft_mult = manyDataConTy
          , ft_arg  = arg, ft_res  = res }

mkFunTy :: HasDebugCallStack => FunTyFlag -> Mult -> Type -> Type -> Type
mkFunTy af mult arg res
  = assertPpr (af == chooseFunTyFlag arg res) (vcat
      [ text "af" <+> ppr af
      , text "chooseAAF" <+> ppr (chooseFunTyFlag arg res)
      , text "arg" <+> ppr arg <+> dcolon <+> ppr (typeKind arg)
      , text "res" <+> ppr res <+> dcolon <+> ppr (typeKind res) ]) $
    FunTy { ft_af   = af
          , ft_mult = mult
          , ft_arg  = arg
          , ft_res  = res }

mkInvisFunTy :: HasDebugCallStack => Type -> Type -> Type
mkInvisFunTy arg res
  = mkFunTy (invisArg (typeTypeOrConstraint res)) manyDataConTy arg res

mkInvisFunTys :: HasDebugCallStack => [Type] -> Type -> Type
mkInvisFunTys args res
  = foldr (mkFunTy af manyDataConTy) res args
  where
    af = invisArg (typeTypeOrConstraint res)

mkVisFunTy :: HasDebugCallStack => Mult -> Type -> Type -> Type
-- Always TypeLike, user-specified multiplicity.
mkVisFunTy = mkFunTy visArgTypeLike

-- | Make nested arrow types
-- | Special, common, case: Arrow type with mult Many
mkVisFunTyMany :: HasDebugCallStack => Type -> Type -> Type
-- Always TypeLike, multiplicity Many
mkVisFunTyMany = mkVisFunTy manyDataConTy

mkVisFunTysMany :: [Type] -> Type -> Type
-- Always TypeLike, multiplicity Many
mkVisFunTysMany tys ty = foldr mkVisFunTyMany ty tys

---------------
mkScaledFunTy :: HasDebugCallStack => FunTyFlag -> Scaled Type -> Type -> Type
mkScaledFunTy af (Scaled mult arg) res = mkFunTy af mult arg res

mkScaledFunTys :: HasDebugCallStack => [Scaled Type] -> Type -> Type
-- All visible args
-- Result type can be TypeLike or ConstraintLike
-- Example of the latter: dataConWrapperType for the data con of a class
mkScaledFunTys tys ty = foldr (mkScaledFunTy af) ty tys
  where
    af = visArg (typeTypeOrConstraint ty)

---------------
-- | Like 'mkTyCoForAllTy', but does not check the occurrence of the binder
-- See Note [Unused coercion variable in ForAllTy]
mkForAllTy :: ForAllTyBinder -> Type -> Type
mkForAllTy bndr body
  = assertPpr (good_bndr bndr) (ppr bndr <+> ppr body) $
    ForAllTy bndr body
  where
    -- Check ForAllTy invariants
    good_bndr (Bndr cv vis)
      | isCoVar cv = vis == coreTyLamForAllTyFlag
                     -- See (FT2) in Note [ForAllTy]
                  && (cv `elemVarSet` tyCoVarsOfType body)
                     -- See (FT3) in Note [ForAllTy]
      | otherwise = True

-- | Wraps foralls over the type using the provided 'TyCoVar's from left to right
mkForAllTys :: [ForAllTyBinder] -> Type -> Type
mkForAllTys tyvars ty = foldr ForAllTy ty tyvars

-- | Wraps foralls over the type using the provided 'InvisTVBinder's from left to right
mkInvisForAllTys :: [InvisTVBinder] -> Type -> Type
mkInvisForAllTys tyvars = mkForAllTys (tyVarSpecToBinders tyvars)

mkPiTy :: HasDebugCallStack => PiTyBinder -> Type -> Type
mkPiTy (Anon ty1 af) ty2  = mkScaledFunTy af ty1 ty2
mkPiTy (Named bndr) ty    = mkForAllTy bndr ty

mkPiTys :: HasDebugCallStack => [PiTyBinder] -> Type -> Type
mkPiTys tbs ty = foldr mkPiTy ty tbs

-- | 'mkNakedTyConTy' creates a nullary 'TyConApp'. In general you
-- should rather use 'GHC.Core.Type.mkTyConTy', which picks the shared
-- nullary TyConApp from inside the TyCon (via tyConNullaryTy.  But
-- we have to build the TyConApp tc [] in that TyCon field; that's
-- what 'mkNakedTyConTy' is for.
mkNakedTyConTy :: TyCon -> Type
mkNakedTyConTy tycon = TyConApp tycon []

tcMkVisFunTy :: Mult -> Type -> Type -> Type
-- Always TypeLike result, user-specified multiplicity.
-- Does not have the assert-checking in mkFunTy: used by the typechecker
-- to avoid looking at the result kind, which may not be zonked
tcMkVisFunTy mult arg res
  = FunTy { ft_af = visArgTypeLike, ft_mult = mult
          , ft_arg = arg, ft_res = res }

tcMkInvisFunTy :: TypeOrConstraint -> Type -> Type -> Type
-- Always invisible (constraint) argument, result specified by res_torc
-- Does not have the assert-checking in mkFunTy: used by the typechecker
-- to avoid looking at the result kind, which may not be zonked
tcMkInvisFunTy res_torc arg res
  = FunTy { ft_af = invisArg res_torc, ft_mult = manyDataConTy
          , ft_arg = arg, ft_res = res }

tcMkScaledFunTys :: [Scaled Type] -> Type -> Type
-- All visible args
-- Result type must be TypeLike
-- No mkFunTy assert checking; result kind may not be zonked
tcMkScaledFunTys tys ty = foldr tcMkScaledFunTy ty tys

tcMkScaledFunTy :: Scaled Type -> Type -> Type
tcMkScaledFunTy (Scaled mult arg) res = tcMkVisFunTy mult arg res

{-
%************************************************************************
%*                                                                      *
            Coercions
%*                                                                      *
%************************************************************************
-}

-- | A 'Coercion' is concrete evidence of the equality/convertibility
-- of two types.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in GHC.Core.Lint
data Coercion
  -- Each constructor has a "role signature", indicating the way roles are
  -- propagated through coercions.
  --    -  P, N, and R stand for coercions of the given role
  --    -  e stands for a coercion of a specific unknown role
  --           (think "role polymorphism")
  --    -  "e" stands for an explicit role parameter indicating role e.
  --    -   _ stands for a parameter that is not a Role or Coercion.

  -- These ones mirror the shape of types
  = -- Refl :: _ -> N
    -- A special case reflexivity for a very common case: Nominal reflexivity
    -- If you need Representational, use (GRefl Representational ty MRefl)
    --                               not (SubCo (Refl ty))
    Refl Type  -- See Note [Refl invariant]

  -- GRefl :: "e" -> _ -> Maybe N -> e
  -- See Note [Generalized reflexive coercion]
  | GRefl Role Type MCoercionN  -- See Note [Refl invariant]
          -- Use (Refl ty), not (GRefl Nominal ty MRefl)
          -- Use (GRefl Representational _ _), not (SubCo (GRefl Nominal _ _))

  -- These ones simply lift the correspondingly-named
  -- Type constructors into Coercions

  -- TyConAppCo :: "e" -> _ -> ?? -> e
  -- See Note [TyConAppCo roles]
  | TyConAppCo Role TyCon [Coercion]    -- lift TyConApp
               -- The TyCon is never a synonym;
               -- we expand synonyms eagerly
               -- But it can be a type function
               -- TyCon is never a saturated (->); use FunCo instead

  | AppCo Coercion CoercionN             -- lift AppTy
          -- AppCo :: e -> N -> e

  -- See Note [ForAllCo]
  | ForAllCo
      { fco_tcv  :: TyCoVar
      , fco_visL :: !ForAllTyFlag -- Visibility of coercionLKind
      , fco_visR :: !ForAllTyFlag -- Visibility of coercionRKind
                                  -- See (FC7) of Note [ForAllCo]
      , fco_kind :: KindCoercion
      , fco_body :: Coercion }
         -- ForAllCo :: _ -> N -> e -> e

  | FunCo  -- FunCo :: "e" -> N/P -> e -> e -> e
           -- See Note [FunCo] for fco_afl, fco_afr
        { fco_role         :: Role
        , fco_afl          :: FunTyFlag   -- Arrow for coercionLKind
        , fco_afr          :: FunTyFlag   -- Arrow for coercionRKind
        , fco_mult         :: CoercionN
        , fco_arg, fco_res :: Coercion }
       -- (if the role "e" is Phantom, the first coercion is, too)
       -- the first coercion is for the multiplicity

  -- These are special
  | CoVarCo CoVar      -- :: _ -> (N or R)
                       -- result role depends on the tycon of the variable's type

  | AxiomCo CoAxiomRule [Coercion]
     -- The coercion arguments always *precisely* saturate
     -- arity of (that branch of) the CoAxiom. If there are
     -- any left over, we use AppCo.
     -- See [Coercion axioms applied to coercions]
     -- The roles of the argument coercions are determined
     -- by the cab_roles field of the relevant branch of the CoAxiom

  | UnivCo  -- See Note [UnivCo]
            -- Of kind (lty ~role rty)
      { uco_prov         :: UnivCoProvenance
      , uco_role         :: Role
      , uco_lty, uco_rty :: Type
      , uco_deps         :: [Coercion]  -- Coercions on which it depends
               --   See Note [The importance of tracking UnivCo dependencies]
      }

  | SymCo Coercion             -- :: e -> e
  | TransCo Coercion Coercion  -- :: e -> e -> e

  | SelCo CoSel Coercion  -- See Note [SelCo]

  | LRCo   LeftOrRight CoercionN     -- Decomposes (t_left t_right)
    -- :: _ -> N -> N
  | InstCo Coercion CoercionN
    -- :: e -> N -> e
    -- See Note [InstCo roles]

  -- Extract a kind coercion from a (heterogeneous) type coercion
  -- NB: all kind coercions are Nominal
  | KindCo Coercion
     -- :: e -> N

  | SubCo CoercionN                  -- Turns a ~N into a ~R
    -- :: N -> R

  | HoleCo CoercionHole              -- ^ See Note [Coercion holes]
                                     -- Only present during typechecking
  deriving Data.Data

data CoSel  -- See Note [SelCo]
  = SelTyCon Int Role  -- Decomposes (T co1 ... con); zero-indexed
                       -- Invariant: Given: SelCo (SelTyCon i r) co
                       --            we have r == tyConRole (coercionRole co) tc
                       --                and tc1 == tc2
                       --            where T tc1 _ = coercionLKind co
                       --                  T tc2 _ = coercionRKind co
                       -- See Note [SelCo]

  | SelFun FunSel      -- Decomposes (co1 -> co2)

  | SelForAll          -- Decomposes (forall a. co)

  deriving( Eq, Data.Data, Ord )

data FunSel  -- See Note [SelCo]
  = SelMult  -- Multiplicity
  | SelArg   -- Argument of function
  | SelRes   -- Result of function
  deriving( Eq, Data.Data, Ord )

type CoercionN = Coercion       -- always nominal
type CoercionR = Coercion       -- always representational
type CoercionP = Coercion       -- always phantom
type KindCoercion = CoercionN   -- always nominal

instance Outputable Coercion where
  ppr = pprCo

instance Outputable CoSel where
  ppr (SelTyCon n r) = text "Tc" <> parens (int n <> comma <> pprOneCharRole r)
  ppr SelForAll      = text "All"
  ppr (SelFun fs)    = text "Fun" <> parens (ppr fs)


pprOneCharRole :: Role -> SDoc
pprOneCharRole Nominal          = char 'N'
pprOneCharRole Representational = char 'R'
pprOneCharRole Phantom          = char 'P'

instance Outputable FunSel where
  ppr SelMult = text "mult"
  ppr SelArg  = text "arg"
  ppr SelRes  = text "res"

instance NFData FunSel where
  rnf SelMult = ()
  rnf SelArg  = ()
  rnf SelRes  = ()

instance Binary CoSel where
   put_ bh (SelTyCon n r)   = do { putByte bh 0; put_ bh n; put_ bh r }
   put_ bh SelForAll        = putByte bh 1
   put_ bh (SelFun SelMult) = putByte bh 2
   put_ bh (SelFun SelArg)  = putByte bh 3
   put_ bh (SelFun SelRes)  = putByte bh 4

   get bh = do { h <- getByte bh
               ; case h of
                   0 -> do { n <- get bh; r <- get bh; return (SelTyCon n r) }
                   1 -> return SelForAll
                   2 -> return (SelFun SelMult)
                   3 -> return (SelFun SelArg)
                   _ -> return (SelFun SelRes) }

instance NFData CoSel where
  rnf (SelTyCon n r) = rnf n `seq` rnf r `seq` ()
  rnf SelForAll      = ()
  rnf (SelFun fs)    = rnf fs `seq` ()

-- | A semantically more meaningful type to represent what may or may not be a
-- useful 'Coercion'.
data MCoercion
  = MRefl
    -- A trivial Reflexivity coercion
  | MCo Coercion
    -- Other coercions
  deriving Data.Data
type MCoercionR = MCoercion
type MCoercionN = MCoercion

instance Outputable MCoercion where
  ppr MRefl    = text "MRefl"
  ppr (MCo co) = text "MCo" <+> ppr co

{- Note [Refl invariant]
~~~~~~~~~~~~~~~~~~~~~~~~
Invariant 1: Refl lifting
        Refl (similar for GRefl r ty MRefl) is always lifted as far as possible.
    For example
        (Refl T) (Refl a) (Refl b) is normalised (by mkAppCo) to  (Refl (T a b)).

    You might think that a consequences is:
         Every identity coercion has Refl at the root

    But that's not quite true because of coercion variables.  Consider
         g         where g :: Int~Int
         Left h    where h :: Maybe Int ~ Maybe Int
    etc.  So the consequence is only true of coercions that
    have no coercion variables.

Invariant 2: TyConAppCo
   An application of (Refl T) to some coercions, at least one of which is
   NOT the identity, is normalised to TyConAppCo.  (They may not be
   fully saturated however.)  TyConAppCo coercions (like all coercions
   other than Refl) are NEVER the identity.

Note [Generalized reflexive coercion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GRefl is a generalized reflexive coercion (see #15192). It wraps a kind
coercion, which might be reflexive (MRefl) or any coercion (MCo co). The typing
rules for GRefl:

  ty : k1
  ------------------------------------
  GRefl r ty MRefl: ty ~r ty

  ty : k1       co :: k1 ~ k2
  ------------------------------------
  GRefl r ty (MCo co) : ty ~r ty |> co

Consider we have

   g1 :: s ~r t
   s  :: k1
   g2 :: k1 ~ k2

and we want to construct a coercions co which has type

   (s |> g2) ~r t

We can define

   co = Sym (GRefl r s g2) ; g1

It is easy to see that

   Refl == GRefl Nominal ty MRefl :: ty ~n ty

A nominal reflexive coercion is quite common, so we keep the special form Refl to
save allocation.

Note [SelCo]
~~~~~~~~~~~~
The Coercion form SelCo allows us to decompose a structural coercion, one
between ForallTys, or TyConApps, or FunTys.

There are three forms, split by the CoSel field inside the SelCo:
SelTyCon, SelForAll, and SelFun.

* SelTyCon:

      co : (T s1..sn) ~r0 (T t1..tn)
      T is a data type, not a newtype, nor an arrow type
      r = tyConRole tc r0 i
      i < n    (i is zero-indexed)
      ----------------------------------
      SelCo (SelTyCon i r) co : si ~r ti

  "Not a newtype": see Note [SelCo and newtypes]
  "Not an arrow type": see SelFun below

   See Note [SelCo Cached Roles]

* SelForAll:
      co : forall (a:k1).t1 ~r0 forall (a:k2).t2
      ----------------------------------
      SelCo SelForAll co : k1 ~N k2

  NB: SelForAll always gives a Nominal coercion.

* The SelFun form, for functions, has three sub-forms for the three
  components of the function type (multiplicity, argument, result).

      co : (s1 %{m1}-> t1) ~r0 (s2 %{m2}-> t2)
      r = funRole r0 SelMult
      ----------------------------------
      SelCo (SelFun SelMult) co : m1 ~r m2

      co : (s1 %{m1}-> t1) ~r0 (s2 %{m2}-> t2)
      r = funRole r0 SelArg
      ----------------------------------
      SelCo (SelFun SelArg) co : s1 ~r s2

      co : (s1 %{m1}-> t1) ~r0 (s2 %{m2}-> t2)
      r = funRole r0 SelRes
      ----------------------------------
      SelCo (SelFun SelRes) co : t1 ~r t2

Note [FunCo]
~~~~~~~~~~~~
Just as FunTy has a ft_af :: FunTyFlag field, FunCo (which connects
two function types) has two FunTyFlag fields:
     funco_afl, funco_afr :: FunTyFlag
In all cases, the FunTyFlag is recoverable from the kinds of the argument
and result types/coercions; but experiments show that it's better to
cache it.

Why does FunCo need /two/ flags? If we have a single method class,
implemented as a newtype
   class C a where { op :: [a] -> a }
then we can have a coercion
   co :: C Int ~R ([Int]->Int)
So now we can define
   FunCo co <Bool> : (C Int => Bool) ~R (([Int]->Int) -> Bool)
Notice that the left and right arrows are different!  Hence two flags,
one for coercionLKind and one for coercionRKind.

Note [Coercion axioms applied to coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The reason coercion axioms can be applied to coercions and not just
types is to allow for better optimization.  There are some cases where
we need to be able to "push transitivity inside" an axiom in order to
expose further opportunities for optimization.

For example, suppose we have

  C a : t[a] ~ F a
  g   : b ~ c

and we want to optimize

  sym (C b) ; t[g] ; C c

which has the kind

  F b ~ F c

(stopping through t[b] and t[c] along the way).

We'd like to optimize this to just F g -- but how?  The key is
that we need to allow axioms to be instantiated by *coercions*,
not just by types.  Then we can (in certain cases) push
transitivity inside the axiom instantiations, and then react
opposite-polarity instantiations of the same axiom.  In this
case, e.g., we match t[g] against the LHS of (C c)'s kind, to
obtain the substitution  a |-> g  (note this operation is sort
of the dual of lifting!) and hence end up with

  C g : t[b] ~ F c

which indeed has the same kind as  t[g] ; C c.

Now we have

  sym (C b) ; C g

which can be optimized to F g.

Note [Required foralls in Core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the CoreExpr (Lam a e) where `a` is a TyVar, and (e::e_ty).
It has type
   forall a. e_ty
Note the Specified visibility of (forall a. e_ty); the Core type just isn't able
to express more than one visiblity, and we pick `Specified`.  See `exprType` and
`mkLamType` in GHC.Core.Utils, and `GHC.Type.Var.coreLamForAllTyFlag`.

So how can we ever get a term of type (forall a -> e_ty)?  Answer: /only/ via a
cast built with ForAllCo.  See `GHC.Tc.Types.Evidence.mkWpForAllCast`.  This does
not seem very satisfying, but it does the job.

An alternative would be to put a visibility flag into `Lam` (a huge change),
or into a `TyVar` (a more plausible change), but we leave that for the future.

See also Note [ForAllTy and type equality] in GHC.Core.TyCo.Compare.

Note [ForAllCo]
~~~~~~~~~~~~~~~
See also Note [ForAllTy and type equality] in GHC.Core.TyCo.Compare.

Constructing coercions between forall-types can be a bit tricky,
because the kinds of the bound tyvars can be different.

The typing rule is:

  G |- kind_co : k1 ~N k2
  tv1 \not\in fv(typeKind(t1),typeKind(t2))  -- Skolem escape
  G, tv1:k1 |- co : t1 ~r t2
  if r=N, then vis1=vis2
  ------------------------------------
  G |- ForAllCo (tv1:k1) vis1 vis2 kind_co co
         : forall (tv1:k1) <vis1>. t1
              ~r
           forall (tv1:k2) <vis2>. (t2[tv1 |-> (tv1:k2) |> sym kind_co])

Several things to note here

(FC1) First, the TyCoVar stored in a ForAllCo is really just a convenience: this
  field should be a Name, as its kind is redundant. Thinking of the field as a
  Name is helpful in understanding what a ForAllCo means.  The kind of TyCoVar
  always matches the left-hand kind of the coercion.

  * The idea is that kind_co gives the two kinds of the tyvar. See how, in the
    conclusion, tv1 is assigned kind k1 on the left but kind k2 on the right.

  * Of course, a type variable can't have different kinds at the same time.
    So, in `co` itself we use (tv1 : k1); hence the premise
          tv1:k1 |- co : t1 ~r t2

  * The last wrinkle is that we need to fix the kinds in the conclusion. In
    t2, tv1 is assumed to have kind k1, but it has kind k2 in the conclusion of
     the rule. So we do a kind-fixing substitution, replacing (tv1:k1) with
     (tv1:k2) |> sym kind_co. This substitution is slightly bizarre, because it
    mentions the same name with different kinds, but it *is* well-kinded, noting
     that `(tv1:k2) |> sym kind_co` has kind k1.

  We could instead store just a Name in the ForAllCo, and it might even be
  more efficient to do so. But we can't add Names to, e.g., VarSets, and
  there generally is just an impedance mismatch in a bunch of places. So we
  use tv1. When we need tv2, we can use setTyVarKind.

(FC2) Note that the kind coercion must be Nominal; and that the role `r` of
  the final coercion is the same as that of the body coercion.

(FC3) A ForAllCo allows casting between visibilities.  For example:
         ForAllCo a Required Specified (SubCo (Refl ty))
           : (forall a -> ty) ~R (forall a. ty)
  But you can only cast between visiblities at Representational role;
  Hence the premise
      if r=N, then vis1=vis2
  in the typing rule.  See also Note [ForAllTy and type equality] in
  GHC.Core.TyCo.Compare.

(FC4) See Note [Required foralls in Core].

(FC5) In a /type/, in (ForAllTy cv ty) where cv is a CoVar, we insist that
  `cv` must appear free in `ty`; see Note [Unused coercion variable in ForAllTy]
  in GHC.Core.TyCo.Rep for the motivation.  If it does not appear free,
  use FunTy.

  However we do /not/ impose the same restriction on ForAllCo in /coercions/.
  Instead, in coercionLKind and coercionRKind, we use mkTyCoForAllTy to perform
  the check and construct a FunTy when necessary.  Why?
    * For a coercion, all that matters is its kind, So ForAllCo vs FunCo does not
       make a difference.
    * Even if cv occurs in body_co, it is possible that cv does not occur in the kind
      of body_co. Therefore the check in coercionKind is inevitable.

(FC6) Invariant: in a ForAllCo where fco_tcv is a coercion variable, `cv`,
  we insist that `cv` appears only in positions that are erased. In fact we use
  a conservative approximation of this: we require that
       (almostDevoidCoVarOfCo cv fco_body)
  holds.  This function checks that `cv` appers only within the type in a Refl
  node and under a GRefl node (including in the Coercion stored in a GRefl).
  It's possible other places are OK, too, but this is a safe approximation.

  Why all this fuss?  See Section 5.8.5.2 of Richard's thesis. The idea is that
  we cannot prove that the type system is consistent with unrestricted use of this
  cv; the consistency proof uses an untyped rewrite relation that works over types
  with all coercions and casts removed. So, we can allow the cv to appear only in
  positions that are erased.

  Sadly, with heterogeneous equality, this restriction might be able to be
  violated; Richard's thesis is unable to prove that it isn't. Specifically, the
  liftCoSubst function might create an invalid coercion. Because a violation of
  the restriction might lead to a program that "goes wrong", it is checked all
  the time, even in a production compiler and without -dcore-lint. We *have*
  proved that the problem does not occur with homogeneous equality, so this
  check can be dropped once ~# is made to be homogeneous.

(FC7) Invariant: in a ForAllCo, if fco_tcv is a CoVar, then
         fco_visL = fco_visR = coreTyLamForAllTyFlag
  c.f. (FT2) in Note [ForAllTy]

Note [Predicate coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   g :: a~b
How can we coerce between types
   ([c]~a) => [a] -> c
and
   ([c]~b) => [b] -> c
where the equality predicate *itself* differs?

Answer: we simply treat (~) as an ordinary type constructor, so these
types really look like

   ((~) [c] a) -> [a] -> c
   ((~) [c] b) -> [b] -> c

So the coercion between the two is obviously

   ((~) [c] g) -> [g] -> c

Another way to see this to say that we simply collapse predicates to
their representation type (see Type.coreView and Type.predTypeRep).

This collapse is done by mkPredCo; there is no PredCo constructor
in Coercion.  This is important because we need Nth to work on
predicates too:
    SelCo (SelTyCon 1) ((~) [c] g) = g
See Simplify.simplCoercionF, which generates such selections.

Note [Roles]
~~~~~~~~~~~~
Roles are a solution to the GeneralizedNewtypeDeriving problem, articulated
in #1496. The full story is in docs/core-spec/core-spec.pdf. Also, see
https://gitlab.haskell.org/ghc/ghc/wikis/roles-implementation

Here is one way to phrase the problem:

Given:
newtype Age = MkAge Int
type family F x
type instance F Age = Bool
type instance F Int = Char

This compiles down to:
axAge :: Age ~ Int
axF1 :: F Age ~ Bool
axF2 :: F Int ~ Char

Then, we can make:
(sym (axF1) ; F axAge ; axF2) :: Bool ~ Char

Yikes!

The solution is _roles_, as articulated in "Generative Type Abstraction and
Type-level Computation" (POPL 2010), available at
http://www.seas.upenn.edu/~sweirich/papers/popl163af-weirich.pdf

The specification for roles has evolved somewhat since that paper. For the
current full details, see the documentation in docs/core-spec. Here are some
highlights.

We label every equality with a notion of type equivalence, of which there are
three options: Nominal, Representational, and Phantom. A ground type is
nominally equivalent only with itself. A newtype (which is considered a ground
type in Haskell) is representationally equivalent to its representation.
Anything is "phantomly" equivalent to anything else. We use "N", "R", and "P"
to denote the equivalences.

The axioms above would be:
axAge :: Age ~R Int
axF1 :: F Age ~N Bool
axF2 :: F Age ~N Char

Then, because transitivity applies only to coercions proving the same notion
of equivalence, the above construction is impossible.

However, there is still an escape hatch: we know that any two types that are
nominally equivalent are representationally equivalent as well. This is what
the form SubCo proves -- it "demotes" a nominal equivalence into a
representational equivalence. So, it would seem the following is possible:

sub (sym axF1) ; F axAge ; sub axF2 :: Bool ~R Char   -- WRONG

What saves us here is that the arguments to a type function F, lifted into a
coercion, *must* prove nominal equivalence. So, (F axAge) is ill-formed, and
we are safe.

Roles are attached to parameters to TyCons. When lifting a TyCon into a
coercion (through TyConAppCo), we need to ensure that the arguments to the
TyCon respect their roles. For example:

data T a b = MkT a (F b)

If we know that a1 ~R a2, then we know (T a1 b) ~R (T a2 b). But, if we know
that b1 ~R b2, we know nothing about (T a b1) and (T a b2)! This is because
the type function F branches on b's *name*, not representation. So, we say
that 'a' has role Representational and 'b' has role Nominal. The third role,
Phantom, is for parameters not used in the type's definition. Given the
following definition

data Q a = MkQ Int

the Phantom role allows us to say that (Q Bool) ~R (Q Char), because we
can construct the coercion Bool ~P Char (using UnivCo).

See the paper cited above for more examples and information.

Note [TyConAppCo roles]
~~~~~~~~~~~~~~~~~~~~~~~
The TyConAppCo constructor has a role parameter, indicating the role at
which the coercion proves equality. The choice of this parameter affects
the required roles of the arguments of the TyConAppCo. To help explain
it, assume the following definition:

  type instance F Int = Bool   -- Axiom axF : F Int ~N Bool
  newtype Age = MkAge Int      -- Axiom axAge : Age ~R Int
  data Foo a = MkFoo a         -- Role on Foo's parameter is Representational

TyConAppCo Nominal Foo axF : Foo (F Int) ~N Foo Bool
  For (TyConAppCo Nominal) all arguments must have role Nominal. Why?
  So that Foo Age ~N Foo Int does *not* hold.

TyConAppCo Representational Foo (SubCo axF) : Foo (F Int) ~R Foo Bool
TyConAppCo Representational Foo axAge       : Foo Age     ~R Foo Int
  For (TyConAppCo Representational), all arguments must have the roles
  corresponding to the result of tyConRoles on the TyCon. This is the
  whole point of having roles on the TyCon to begin with. So, we can
  have Foo Age ~R Foo Int, if Foo's parameter has role R.

  If a Representational TyConAppCo is over-saturated (which is otherwise fine),
  the spill-over arguments must all be at Nominal. This corresponds to the
  behavior for AppCo.

TyConAppCo Phantom Foo (UnivCo Phantom Int Bool) : Foo Int ~P Foo Bool
  All arguments must have role Phantom. This one isn't strictly
  necessary for soundness, but this choice removes ambiguity.

The rules here dictate the roles of the parameters to mkTyConAppCo
(should be checked by Lint).

Note [SelCo and newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

  newtype N a = MkN Int
  type role N representational

This yields axiom

  NTCo:N :: forall a. N a ~R Int

We can then build

  co :: forall a b. N a ~R N b
  co = NTCo:N a ; sym (NTCo:N b)

for any `a` and `b`. Because of the role annotation on N, if we use
SelCo, we'll get out a representational coercion. That is:

  SelCo (SelTyCon 0 r) co :: forall a b. a ~r b

Yikes! Clearly, this is terrible. The solution is simple: forbid
SelCo to be used on newtypes if the internal coercion is representational.
See the SelCo equation for GHC.Core.Lint.lintCoercion.

This is not just some corner case discovered by a segfault somewhere;
it was discovered in the proof of soundness of roles and described
in the "Safe Coercions" paper (ICFP '14).

Note [SelCo Cached Roles]
~~~~~~~~~~~~~~~~~~~~~~~~~
Why do we cache the role of SelCo in the SelCo constructor?
Because computing role(Nth i co) involves figuring out that

  co :: T tys1 ~ T tys2

using coercionKind, and finding (coercionRole co), and then looking
at the tyConRoles of T. Avoiding bad asymptotic behaviour here means
we have to compute the kind and role of a coercion simultaneously,
which makes the code complicated and inefficient.

This only happens for SelCo. Caching the role solves the problem, and
allows coercionKind and coercionRole to be simple.

See #11735

Note [InstCo roles]
~~~~~~~~~~~~~~~~~~~
Here is (essentially) the typing rule for InstCo:

g :: (forall a. t1) ~r (forall a. t2)
w :: s1 ~N s2
------------------------------- InstCo
InstCo g w :: (t1 [a |-> s1]) ~r (t2 [a |-> s2])

Note that the Coercion w *must* be nominal. This is necessary
because the variable a might be used in a "nominal position"
(that is, a place where role inference would require a nominal
role) in t1 or t2. If we allowed w to be representational, we
could get bogus equalities.

A more nuanced treatment might be able to relax this condition
somewhat, by checking if t1 and/or t2 use their bound variables
in nominal ways. If not, having w be representational is OK.


%************************************************************************
%*                                                                      *
                UnivCo
%*                                                                      *
%************************************************************************

Note [UnivCo]
~~~~~~~~~~~~~
A UnivCo is a coercion whose proof does not directly express its role
and kind (indeed for some UnivCos, like PluginProv, there /is/ no proof).

The different kinds of UnivCo are described by UnivCoProvenance.  Really each
is entirely separate, but they all share the need to represent these fields:

  UnivCo
      { uco_prov         :: UnivCoProvenance
      , uco_role         :: Role
      , uco_lty, uco_rty :: Type
      , uco_deps         :: [Coercion]  -- Coercions on which it depends

Here,
 * uco_role, uco_lty, uco_rty express the type of the coercion
 * uco_prov says where it came from
 * uco_deps specifies the coercions on which this proof (which is not
   explicity given) depends. See
   Note [The importance of tracking UnivCo dependencies]
-}

-- | For simplicity, we have just one UnivCo that represents a coercion from
-- some type to some other type, with (in general) no restrictions on the
-- type. The UnivCoProvenance specifies more exactly what the coercion really
-- is and why a program should (or shouldn't!) trust the coercion.
-- It is reasonable to consider each constructor of 'UnivCoProvenance'
-- as a totally independent coercion form; their only commonality is
-- that they don't tell you what types they coercion between. (That info
-- is in the 'UnivCo' constructor of 'Coercion'.
data UnivCoProvenance
  = PhantomProv    -- ^ See Note [Phantom coercions]. Only in Phantom
                   -- roled coercions

  | ProofIrrelProv -- ^ From the fact that any two coercions are
                   --   considered equivalent. See Note [ProofIrrelProv].
                   -- Can be used in Nominal or Representational coercions

  | PluginProv String
      -- ^ From a plugin, which asserts that this coercion is sound.
      --   The string and the variable set are for the use by the plugin.

  deriving (Eq, Ord, Data.Data)
  -- Why Ord?  See Note [Ord instance of IfaceType] in GHC.Iface.Type

instance Outputable UnivCoProvenance where
  ppr PhantomProv      = text "(phantom)"
  ppr ProofIrrelProv   = text "(proof irrel)"
  ppr (PluginProv str) = parens (text "plugin" <+> brackets (text str))

instance NFData UnivCoProvenance where
  rnf p = p `seq` ()

instance Binary UnivCoProvenance where
  put_ bh PhantomProv    = putByte bh 1
  put_ bh ProofIrrelProv = putByte bh 2
  put_ bh (PluginProv a) = putByte bh 3 >> put_ bh a
  get bh = do
      tag <- getByte bh
      case tag of
           1 -> return PhantomProv
           2 -> return ProofIrrelProv
           3 -> do a <- get bh
                   return $ PluginProv a
           _ -> panic ("get UnivCoProvenance " ++ show tag)


{- Note [Phantom coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
     data T a = T1 | T2
Then we have
     T s ~R T t
for any old s,t. The witness for this is (TyConAppCo T Rep co),
where (co :: s ~P t) is a phantom coercion built with PhantomProv.
The role of the UnivCo is always Phantom.  The Coercion stored is the
(nominal) kind coercion between the types
   kind(s) ~N kind (t)

Note [ProofIrrelProv]
~~~~~~~~~~~~~~~~~~~~~
A ProofIrrelProv is a coercion between coercions. For example:

  data G a where
    MkG :: G Bool

In core, we get

  G :: * -> *
  MkG :: forall (a :: *). (a ~# Bool) -> G a

Now, consider 'MkG -- that is, MkG used in a type -- and suppose we want
a proof that ('MkG a1 co1) ~ ('MkG a2 co2). This will have to be

  TyConAppCo Nominal MkG [co3, co4]
  where
    co3 :: co1 ~ co2
    co4 :: a1 ~ a2

Note that
  co1 :: a1 ~ Bool
  co2 :: a2 ~ Bool

Here,
  co3 = UnivCo ProofIrrelProv Nominal (CoercionTy co1) (CoercionTy co2) [co5]
  where
    co5 :: (a1 ~# Bool) ~# (a2 ~# Bool)
    co5 = TyConAppCo Nominal (~#) [<Consraint#>, <Constraint#>, co4, <Bool>]


Note [The importance of tracking UnivCo dependencies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is vital that `UnivCo` (a coercion that lacks a proper proof)
tracks the coercions on which it depends. To see why, consider this program:

    type S :: Nat -> Nat

    data T (a::Nat) where
      T1 :: T 0
      T2 :: ...

    f :: T a -> S (a+1) -> S 1
    f = /\a (x:T a) (y:a).
        case x of
          T1 (gco : a ~# 0) -> y |> wco

For this to typecheck we need `wco :: S (a+1) ~# S 1`, given that `gco : a ~# 0`.
To prove that we need to know that `a+1 = 1` if `a=0`, which a plugin might know.
So it solves `wco` by providing a `UnivCo (PluginProv "my-plugin") (a+1) 1 [gco]`.

    But the `uco_deps` in `PluginProv` must mention `gco`!

Why? Otherwise we might float the entire expression (y |> wco) out of the
the case alternative for `T1` which brings `gco` into scope. If this
happens then we aren't far from a segmentation fault or much worse.
See #23923 for a real-world example of this happening.

So it is /crucial/ for the `UnivCo` to mention, in `uco_deps`, the coercion
variables used by the plugin to justify the `UnivCo` that it builds.  You
should think of it like `TyConAppCo`: the `UnivCo` proof constructor is
applied to a list of coercions, just as `TyConAppCo` is

It's very convenient to record a full coercion, not just a set of free coercion
variables, because during typechecking those coercions might contain coercion
holes `HoleCo`, which get filled in later.
-}

{- **********************************************************************
%*                                                                      *
                Coercion holes
%*                                                                      *
%********************************************************************* -}

-- | A coercion to be filled in by the type-checker. See Note [Coercion holes]
data CoercionHole
  = CoercionHole { ch_co_var  :: CoVar
                       -- See Note [CoercionHoles and coercion free variables]

                 , ch_ref :: IORef (Maybe Coercion)
                 }

coHoleCoVar :: CoercionHole -> CoVar
coHoleCoVar = ch_co_var

setCoHoleCoVar :: CoercionHole -> CoVar -> CoercionHole
setCoHoleCoVar h cv = h { ch_co_var = cv }

instance Data.Data CoercionHole where
  -- don't traverse?
  toConstr _   = abstractConstr "CoercionHole"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "CoercionHole"

instance Outputable CoercionHole where
  ppr (CoercionHole { ch_co_var = cv }) = braces (ppr cv)

instance Uniquable CoercionHole where
  getUnique (CoercionHole { ch_co_var = cv }) = getUnique cv

{- Note [Coercion holes]
~~~~~~~~~~~~~~~~~~~~~~~~
During typechecking, constraint solving for type classes works by
  - Generate an evidence Id,  d7 :: Num a
  - Wrap it in a Wanted constraint, [W] d7 :: Num a
  - Use the evidence Id where the evidence is needed
  - Solve the constraint later
  - When solved, add an enclosing let-binding  let d7 = .... in ....
    which actually binds d7 to the (Num a) evidence

For equality constraints we use a different strategy.  See Note [The
equality types story] in GHC.Builtin.Types.Prim for background on equality constraints.
  - For /boxed/ equality constraints, (t1 ~N t2) and (t1 ~R t2), it's just
    like type classes above. (Indeed, boxed equality constraints *are* classes.)
  - But for /unboxed/ equality constraints (t1 ~R# t2) and (t1 ~N# t2)
    we use a different plan

For unboxed equalities:
  - Generate a CoercionHole, a mutable variable just like a unification
    variable
  - Wrap the CoercionHole in a Wanted constraint; see GHC.Tc.Utils.TcEvDest
  - Use the CoercionHole in a Coercion, via HoleCo
  - Solve the constraint later
  - When solved, fill in the CoercionHole by side effect, instead of
    doing the let-binding thing

The main reason for all this is that there may be no good place to let-bind
the evidence for unboxed equalities:

  - We emit constraints for kind coercions, to be used to cast a
    type's kind. These coercions then must be used in types. Because
    they might appear in a top-level type, there is no place to bind
    these (unlifted) coercions in the usual way.

  - A coercion for (forall a. t1) ~ (forall a. t2) will look like
       forall a. (coercion for t1~t2)
    But the coercion for (t1~t2) may mention 'a', and we don't have
    let-bindings within coercions.  We could add them, but coercion
    holes are easier.

  - Moreover, nothing is lost from the lack of let-bindings. For
    dictionaries want to achieve sharing to avoid recomputing the
    dictionary.  But coercions are entirely erased, so there's little
    benefit to sharing. Indeed, even if we had a let-binding, we
    always inline types and coercions at every use site and drop the
    binding.

Other notes about HoleCo:

 * INVARIANT: CoercionHole and HoleCo are used only during type checking,
   and should never appear in Core. Just like unification variables; a Type
   can contain a TcTyVar, but only during type checking. If, one day, we
   use type-level information to separate out forms that can appear during
   type-checking vs forms that can appear in core proper, holes in Core will
   be ruled out.

 * See Note [CoercionHoles and coercion free variables]

 * Coercion holes can be compared for equality like other coercions:
   by looking at the types coerced.


Note [CoercionHoles and coercion free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Why does a CoercionHole contain a CoVar, as well as reference to
fill in?  Because we want to treat that CoVar as a free variable of
the coercion.  See #14584, and Note [What prevents a
constraint from floating] in GHC.Tc.Solver, item (4):

        forall k. [W] co1 :: t1 ~# t2 |> co2
                  [W] co2 :: k ~# *

Here co2 is a CoercionHole. But we /must/ know that it is free in
co1, because that's all that stops it floating outside the
implication.
-}



{- *********************************************************************
*                                                                      *
                foldType  and   foldCoercion
*                                                                      *
********************************************************************* -}

{- Note [foldType]
~~~~~~~~~~~~~~~~~~
foldType is a bit more powerful than perhaps it looks:

* You can fold with an accumulating parameter, via
     TyCoFolder env (Endo a)
  Recall newtype Endo a = Endo (a->a)

* You can fold monadically with a monad M, via
     TyCoFolder env (M a)
  provided you have
     instance ..  => Monoid (M a)

Note [mapType vs foldType]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We define foldType here, but mapType in module Type. Why?

* foldType is used in GHC.Core.TyCo.FVs for finding free variables.
  It's a very simple function that analyses a type,
  but does not construct one.

* mapType constructs new types, and so it needs to call
  the "smart constructors", mkAppTy, mkCastTy, and so on.
  These are sophisticated functions, and can't be defined
  here in GHC.Core.TyCo.Rep.

Note [Specialising foldType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We inline foldType at every call site (there are not many), so that it
becomes specialised for the particular monoid *and* TyCoFolder at
that site.  This is just for efficiency, but walking over types is
done a *lot* in GHC, so worth optimising.

We were worried that
    TyCoFolder env (Endo a)
might not eta-expand.  Recall newtype Endo a = Endo (a->a).

In particular, given
   fvs :: Type -> TyCoVarSet
   fvs ty = appEndo (foldType tcf emptyVarSet ty) emptyVarSet

   tcf :: TyCoFolder enf (Endo a)
   tcf = TyCoFolder { tcf_tyvar = do_tv, ... }
      where
        do_tvs is tv = Endo do_it
           where
             do_it acc | tv `elemVarSet` is  = acc
                       | tv `elemVarSet` acc = acc
                       | otherwise = acc `extendVarSet` tv

we want to end up with
   fvs ty = go emptyVarSet ty emptyVarSet
     where
       go env (TyVarTy tv) acc = acc `extendVarSet` tv
       ..etc..

And indeed this happens.
  - Selections from 'tcf' are done at compile time
  - 'go' is nicely eta-expanded.

We were also worried about
   deep_fvs :: Type -> TyCoVarSet
   deep_fvs ty = appEndo (foldType deep_tcf emptyVarSet ty) emptyVarSet

   deep_tcf :: TyCoFolder enf (Endo a)
   deep_tcf = TyCoFolder { tcf_tyvar = do_tv, ... }
      where
        do_tvs is tv = Endo do_it
           where
             do_it acc | tv `elemVarSet` is  = acc
                       | tv `elemVarSet` acc = acc
                       | otherwise = deep_fvs (varType tv)
                                     `unionVarSet` acc
                                     `extendVarSet` tv

Here deep_fvs and deep_tcf are mutually recursive, unlike fvs and tcf.
But, amazingly, we get good code here too. GHC is careful not to mark
TyCoFolder data constructor for deep_tcf as a loop breaker, so the
record selections still cancel.  And eta expansion still happens too.

Note [Use explicit recursion in foldTyCo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In foldTyCo you'll see things like:
    go_tys _   []     = mempty
    go_tys env (t:ts) = go_ty env t `mappend` go_tys env ts
where we use /explicit recursion/.  You might wonder about using foldl instead:
    go_tys env = foldl (\t acc -> go_ty env t `mappend` acc) mempty
Or maybe foldl', or foldr.

But don't do that for two reasons (see #24591)

* We sometimes instantiate `a` to (Endo VarSet). Remembering
     newtype Endo a = Endo (a->a)
  after inlining `foldTyCo` bodily, the explicit recursion looks like
    go_tys _   []     = \acc -> acc
    go_tys env (t:ts) = \acc -> go_ty env t (go_tys env ts acc)
  The strictness analyser has no problem spotting that this function is
  strict in `acc`, provided `go_ty` is.

  But in the foldl form that is /much/ less obvious, and the strictness
  analyser fails utterly.  Result: lots and lots of thunks get built.  In
  !12037, Mikolaj found that GHC allocated /six times/ as much heap
  on test perf/compiler/T9198 as a result of this single problem!

* Second, while I think that using `foldr` would be fine (simple experiments in
  #24591 suggest as much), it builds a local loop (with env free) and I'm not 100%
  confident it'll be lambda lifted in the end. It seems more direct just to write
  the code we want.

  On the other hand in `go_cvs` we might hope that the `foldr` will fuse with the
  `dVarSetElems` so I have used `foldr`.
-}

data TyCoFolder env a
  = TyCoFolder
      { tcf_view  :: Type -> Maybe Type   -- Optional "view" function
                                          -- E.g. expand synonyms
      , tcf_tyvar :: env -> TyVar -> a    -- Does not automatically recur
      , tcf_covar :: env -> CoVar -> a    -- into kinds of variables
      , tcf_hole  :: env -> CoercionHole -> a
          -- ^ What to do with coercion holes.
          -- See Note [Coercion holes] in "GHC.Core.TyCo.Rep".

      , tcf_tycobinder :: env -> TyCoVar -> ForAllTyFlag -> env
          -- ^ The returned env is used in the extended scope
      }

{-# INLINE foldTyCo  #-}  -- See Note [Specialising foldType]
foldTyCo :: Monoid a => TyCoFolder env a -> env
         -> (Type -> a, [Type] -> a, Coercion -> a, [Coercion] -> a)
foldTyCo (TyCoFolder { tcf_view       = view
                     , tcf_tyvar      = tyvar
                     , tcf_tycobinder = tycobinder
                     , tcf_covar      = covar
                     , tcf_hole       = cohole }) env
  = (go_ty env, go_tys env, go_co env, go_cos env)
  where
    go_ty env ty | Just ty' <- view ty = go_ty env ty'
    go_ty env (TyVarTy tv)      = tyvar env tv
    go_ty env (AppTy t1 t2)     = go_ty env t1 `mappend` go_ty env t2
    go_ty _   (LitTy {})        = mempty
    go_ty env (CastTy ty co)    = go_ty env ty `mappend` go_co env co
    go_ty env (CoercionTy co)   = go_co env co
    go_ty env (FunTy _ w arg res) = go_ty env w `mappend` go_ty env arg `mappend` go_ty env res
    go_ty env (TyConApp _ tys)  = go_tys env tys
    go_ty env (ForAllTy (Bndr tv vis) inner)
      = let !env' = tycobinder env tv vis  -- Avoid building a thunk here
        in go_ty env (varType tv) `mappend` go_ty env' inner

    -- See Note [Use explicit recursion in foldTyCo]
    go_tys _   []     = mempty
    go_tys env (t:ts) = go_ty env t `mappend` go_tys env ts

    -- See Note [Use explicit recursion in foldTyCo]
    go_cos _   []     = mempty
    go_cos env (c:cs) = go_co env c `mappend` go_cos env cs

    go_co env (Refl ty)                = go_ty env ty
    go_co env (GRefl _ ty MRefl)       = go_ty env ty
    go_co env (GRefl _ ty (MCo co))    = go_ty env ty `mappend` go_co env co
    go_co env (TyConAppCo _ _ args)    = go_cos env args
    go_co env (AppCo c1 c2)            = go_co env c1 `mappend` go_co env c2
    go_co env (CoVarCo cv)             = covar env cv
    go_co env (AxiomCo _ cos)          = go_cos env cos
    go_co env (HoleCo hole)            = cohole env hole
    go_co env (UnivCo { uco_lty = t1, uco_rty = t2, uco_deps = deps })
                                       = go_ty env t1 `mappend` go_ty env t2
                                         `mappend` go_cos env deps
    go_co env (SymCo co)               = go_co env co
    go_co env (TransCo c1 c2)          = go_co env c1 `mappend` go_co env c2
    go_co env (SelCo _ co)             = go_co env co
    go_co env (LRCo _ co)              = go_co env co
    go_co env (InstCo co arg)          = go_co env co `mappend` go_co env arg
    go_co env (KindCo co)              = go_co env co
    go_co env (SubCo co)               = go_co env co

    go_co env (FunCo { fco_mult = cw, fco_arg = c1, fco_res = c2 })
       = go_co env cw `mappend` go_co env c1 `mappend` go_co env c2

    go_co env (ForAllCo tv _vis1 _vis2 kind_co co)
      = go_co env kind_co `mappend` go_ty env (varType tv)
                          `mappend` go_co env' co
      where
        env' = tycobinder env tv Inferred

-- | A view function that looks through nothing.
noView :: Type -> Maybe Type
noView _ = Nothing

{- *********************************************************************
*                                                                      *
                   typeSize, coercionSize
*                                                                      *
********************************************************************* -}

-- NB: We put typeSize/coercionSize here because they are mutually
--     recursive, and have the CPR property.  If we have mutual
--     recursion across a hi-boot file, we don't get the CPR property
--     and these functions allocate a tremendous amount of rubbish.
--     It's not critical (because typeSize is really only used in
--     debug mode, but I tripped over an example (T5642) in which
--     typeSize was one of the biggest single allocators in all of GHC.
--     And it's easy to fix, so I did.

-- NB: typeSize does not respect `eqType`, in that two types that
--     are `eqType` may return different sizes. This is OK, because this
--     function is used only in reporting, not decision-making.

typeSize :: Type -> Int
-- The size of the syntax tree of a type.  No special treatment
-- for type synonyms or type families.
typeSize (LitTy {})                 = 1
typeSize (TyVarTy {})               = 1
typeSize (AppTy t1 t2)              = typeSize t1 + typeSize t2
typeSize (FunTy _ _ t1 t2)          = typeSize t1 + typeSize t2
typeSize (ForAllTy (Bndr tv _) t)   = typeSize (varType tv) + typeSize t
typeSize (TyConApp _ ts)            = 1 + typesSize ts
typeSize (CastTy ty co)             = typeSize ty + coercionSize co
typeSize (CoercionTy co)            = coercionSize co

typesSize :: [Type] -> Int
typesSize tys = foldr ((+) . typeSize) 0 tys

coercionSize :: Coercion -> Int
coercionSize (Refl ty)             = typeSize ty
coercionSize (GRefl _ ty MRefl)    = typeSize ty
coercionSize (GRefl _ ty (MCo co)) = 1 + typeSize ty + coercionSize co
coercionSize (TyConAppCo _ _ args) = 1 + sum (map coercionSize args)
coercionSize (AppCo co arg)        = coercionSize co + coercionSize arg
coercionSize (ForAllCo { fco_kind = h, fco_body = co })
                                   = 1 + coercionSize co + coercionSize h
coercionSize (FunCo _ _ _ w c1 c2) = 1 + coercionSize c1 + coercionSize c2
                                                         + coercionSize w
coercionSize (CoVarCo _)         = 1
coercionSize (HoleCo _)          = 1
coercionSize (AxiomCo _ cs)      = 1 + sum (map coercionSize cs)
coercionSize (UnivCo { uco_lty = t1, uco_rty = t2 })  = 1 + typeSize t1 + typeSize t2
coercionSize (SymCo co)          = 1 + coercionSize co
coercionSize (TransCo co1 co2)   = 1 + coercionSize co1 + coercionSize co2
coercionSize (SelCo _ co)        = 1 + coercionSize co
coercionSize (LRCo  _ co)        = 1 + coercionSize co
coercionSize (InstCo co arg)     = 1 + coercionSize co + coercionSize arg
coercionSize (KindCo co)         = 1 + coercionSize co
coercionSize (SubCo co)          = 1 + coercionSize co

{-
************************************************************************
*                                                                      *
                    Multiplicities
*                                                                      *
************************************************************************

These definitions are here to avoid module loops, and to keep
GHC.Core.Multiplicity above this module.

-}

-- | A shorthand for data with an attached 'Mult' element (the multiplicity).
data Scaled a = Scaled !Mult a
  deriving (Data.Data)
  -- You might think that this would be a natural candidate for
  -- Functor, Traversable but Krzysztof says (!3674) "it was too easy
  -- to accidentally lift functions (substitutions, zonking etc.) from
  -- Type -> Type to Scaled Type -> Scaled Type, ignoring
  -- multiplicities and causing bugs".  So we don't.
  --
  -- Being strict in a is worse for performance, so we are only strict on the
  -- Mult part of scaled.


instance (Outputable a) => Outputable (Scaled a) where
   ppr (Scaled _cnt t) = ppr t
     -- Do not print the multiplicity here because it tends to be too verbose

scaledMult :: Scaled a -> Mult
scaledMult (Scaled m _) = m

scaledThing :: Scaled a -> a
scaledThing (Scaled _ t) = t

-- | Apply a function to both the Mult and the Type in a 'Scaled Type'
mapScaledType :: (Type -> Type) -> Scaled Type -> Scaled Type
mapScaledType f (Scaled m t) = Scaled (f m) (f t)

{- |
Mult is a type alias for Type.

Mult must contain Type because multiplicity variables are mere type variables
(of kind Multiplicity) in Haskell. So the simplest implementation is to make
Mult be Type.

Multiplicities can be formed with:
- One: GHC.Types.One (= oneDataCon)
- Many: GHC.Types.Many (= manyDataCon)
- Multiplication: GHC.Types.MultMul (= multMulTyCon)

So that Mult feels a bit more structured, we provide pattern synonyms and smart
constructors for these.
-}
type Mult = Type
