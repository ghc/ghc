{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998
\section[TyCoRep]{Type and Coercion - friends' interface}

Note [The Type-related module hierarchy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Class
  CoAxiom
  TyCon    imports Class, CoAxiom
  TyCoRep  imports Class, CoAxiom, TyCon
  TysPrim  imports TyCoRep ( including mkTyConTy )
  Kind     imports TysPrim ( mainly for primitive kinds )
  Type     imports Kind
  Coercion imports Type
-}

-- We expose the relevant stuff from this module via the Type module
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP, DeriveDataTypeable, MultiWayIf #-}

module TyCoRep (
        TyThing(..), tyThingCategory, pprTyThingCategory, pprShortTyThing,

        -- * Types
        Type(..),
        TyLit(..),
        KindOrType, Kind,
        KnotTied,
        PredType, ThetaType,      -- Synonyms
        ArgFlag(..),

        -- * Coercions
        Coercion(..),
        UnivCoProvenance(..),
        CoercionHole(..), coHoleCoVar, setCoHoleCoVar,
        CoercionN, CoercionR, CoercionP, KindCoercion,
        MCoercion(..), MCoercionR, MCoercionN,

        -- * Functions over types
        mkTyConTy, mkTyVarTy, mkTyVarTys,
        mkTyCoVarTy, mkTyCoVarTys,
        mkFunTy, mkFunTys, mkTyCoForAllTy, mkForAllTys,
        mkForAllTy,
        mkTyCoPiTy, mkTyCoPiTys,
        mkPiTys,

        kindRep_maybe, kindRep,
        isLiftedTypeKind, isUnliftedTypeKind,
        isLiftedRuntimeRep, isUnliftedRuntimeRep,
        isRuntimeRepTy, isRuntimeRepVar,
        sameVis,

        -- * Functions over binders
        TyCoBinder(..), TyCoVarBinder, TyBinder,
        binderVar, binderVars, binderType, binderArgFlag,
        delBinderVar,
        isInvisibleArgFlag, isVisibleArgFlag,
        isInvisibleBinder, isVisibleBinder,
        isTyBinder, isNamedBinder,
        tyCoBinderArgFlag,

        -- * Functions over coercions
        pickLR,

        -- * Pretty-printing
        pprType, pprParendType, pprPrecType, pprPrecTypeX,
        pprTypeApp, pprTCvBndr, pprTCvBndrs,
        pprSigmaType,
        pprTheta, pprParendTheta, pprForAll, pprUserForAll,
        pprTyVar, pprTyVars,
        pprThetaArrowTy, pprClassPred,
        pprKind, pprParendKind, pprTyLit,
        PprPrec(..), topPrec, sigPrec, opPrec, funPrec, appPrec, maybeParen,
        pprDataCons, pprWithExplicitKindsWhen,

        pprCo, pprParendCo,

        debugPprType,

        -- * Free variables
        tyCoVarsOfType, tyCoVarsOfTypeDSet, tyCoVarsOfTypes, tyCoVarsOfTypesDSet,
        tyCoFVsBndr, tyCoFVsVarBndr, tyCoFVsVarBndrs,
        tyCoFVsOfType, tyCoVarsOfTypeList,
        tyCoFVsOfTypes, tyCoVarsOfTypesList,
        coVarsOfType, coVarsOfTypes,
        coVarsOfCo, coVarsOfCos,
        tyCoVarsOfCo, tyCoVarsOfCos,
        tyCoVarsOfCoDSet,
        tyCoFVsOfCo, tyCoFVsOfCos,
        tyCoVarsOfCoList, tyCoVarsOfProv,
        almostDevoidCoVarOfCo,
        injectiveVarsOfType, tyConAppNeedsKindSig,

        noFreeVarsOfType, noFreeVarsOfCo,

        -- * Substitutions
        TCvSubst(..), TvSubstEnv, CvSubstEnv,
        emptyTvSubstEnv, emptyCvSubstEnv, composeTCvSubstEnv, composeTCvSubst,
        emptyTCvSubst, mkEmptyTCvSubst, isEmptyTCvSubst,
        mkTCvSubst, mkTvSubst, mkCvSubst,
        getTvSubstEnv,
        getCvSubstEnv, getTCvInScope, getTCvSubstRangeFVs,
        isInScope, notElemTCvSubst,
        setTvSubstEnv, setCvSubstEnv, zapTCvSubst,
        extendTCvInScope, extendTCvInScopeList, extendTCvInScopeSet,
        extendTCvSubst, extendTCvSubstWithClone,
        extendCvSubst, extendCvSubstWithClone,
        extendTvSubst, extendTvSubstBinderAndInScope, extendTvSubstWithClone,
        extendTvSubstList, extendTvSubstAndInScope,
        extendTCvSubstList,
        unionTCvSubst, zipTyEnv, zipCoEnv, mkTyCoInScopeSet,
        zipTvSubst, zipCvSubst,
        zipTCvSubst,
        mkTvSubstPrs,

        substTyWith, substTyWithCoVars, substTysWith, substTysWithCoVars,
        substCoWith,
        substTy, substTyAddInScope,
        substTyUnchecked, substTysUnchecked, substThetaUnchecked,
        substTyWithUnchecked,
        substCoUnchecked, substCoWithUnchecked,
        substTyWithInScope,
        substTys, substTheta,
        lookupTyVar,
        substCo, substCos, substCoVar, substCoVars, lookupCoVar,
        cloneTyVarBndr, cloneTyVarBndrs,
        substVarBndr, substVarBndrs,
        substTyVarBndr, substTyVarBndrs,
        substCoVarBndr,
        substTyVar, substTyVars, substTyCoVars,
        substForAllCoBndr,
        substVarBndrUsing, substForAllCoBndrUsing,
        checkValidSubst, isValidTCvSubst,

        -- * Tidying type related things up for printing
        tidyType,      tidyTypes,
        tidyOpenType,  tidyOpenTypes,
        tidyOpenKind,
        tidyVarBndr, tidyVarBndrs, tidyFreeTyCoVars, avoidNameClashes,
        tidyOpenTyCoVar, tidyOpenTyCoVars,
        tidyTyCoVarOcc,
        tidyTopType,
        tidyKind,
        tidyCo, tidyCos,
        tidyTyCoVarBinder, tidyTyCoVarBinders,

        -- * Sizes
        typeSize, coercionSize, provSize
    ) where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-} DataCon( dataConFullSig
                             , dataConUserTyVarBinders
                             , DataCon )
import {-# SOURCE #-} Type( isPredTy, isCoercionTy, mkAppTy, mkCastTy
                          , tyCoVarsOfTypeWellScoped
                          , tyCoVarsOfTypesWellScoped
                          , scopedSort
                          , coreView )
   -- Transitively pulls in a LOT of stuff, better to break the loop

import {-# SOURCE #-} Coercion
import {-# SOURCE #-} ConLike ( ConLike(..), conLikeName )
import {-# SOURCE #-} ToIface( toIfaceTypeX, toIfaceTyLit, toIfaceForAllBndr
                             , toIfaceTyCon, toIfaceTcArgs, toIfaceCoercionX )

-- friends:
import IfaceType
import Var
import VarEnv
import VarSet
import Name hiding ( varName )
import TyCon
import Class
import CoAxiom
import FV

-- others
import BasicTypes ( LeftOrRight(..), PprPrec(..), topPrec, sigPrec, opPrec
                  , funPrec, appPrec, maybeParen, pickLR )
import PrelNames
import Outputable
import DynFlags
import FastString
import Pair
import UniqSupply
import Util
import UniqFM
import UniqSet

-- libraries
import qualified Data.Data as Data hiding ( TyCon )
import Data.List
import Data.IORef ( IORef )   -- for CoercionHole

{-
%************************************************************************
%*                                                                      *
                        TyThing
%*                                                                      *
%************************************************************************

Despite the fact that DataCon has to be imported via a hi-boot route,
this module seems the right place for TyThing, because it's needed for
funTyCon and all the types in TysPrim.

It is also SOURCE-imported into Name.hs


Note [ATyCon for classes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Both classes and type constructors are represented in the type environment
as ATyCon.  You can tell the difference, and get to the class, with
   isClassTyCon :: TyCon -> Bool
   tyConClass_maybe :: TyCon -> Maybe Class
The Class and its associated TyCon have the same Name.
-}

-- | A global typecheckable-thing, essentially anything that has a name.
-- Not to be confused with a 'TcTyThing', which is also a typecheckable
-- thing but in the *local* context.  See 'TcEnv' for how to retrieve
-- a 'TyThing' given a 'Name'.
data TyThing
  = AnId     Id
  | AConLike ConLike
  | ATyCon   TyCon       -- TyCons and classes; see Note [ATyCon for classes]
  | ACoAxiom (CoAxiom Branched)

instance Outputable TyThing where
  ppr = pprShortTyThing

instance NamedThing TyThing where       -- Can't put this with the type
  getName (AnId id)     = getName id    -- decl, because the DataCon instance
  getName (ATyCon tc)   = getName tc    -- isn't visible there
  getName (ACoAxiom cc) = getName cc
  getName (AConLike cl) = conLikeName cl

pprShortTyThing :: TyThing -> SDoc
-- c.f. PprTyThing.pprTyThing, which prints all the details
pprShortTyThing thing
  = pprTyThingCategory thing <+> quotes (ppr (getName thing))

pprTyThingCategory :: TyThing -> SDoc
pprTyThingCategory = text . capitalise . tyThingCategory

tyThingCategory :: TyThing -> String
tyThingCategory (ATyCon tc)
  | isClassTyCon tc = "class"
  | otherwise       = "type constructor"
tyThingCategory (ACoAxiom _) = "coercion axiom"
tyThingCategory (AnId   _)   = "identifier"
tyThingCategory (AConLike (RealDataCon _)) = "data constructor"
tyThingCategory (AConLike (PatSynCon _))  = "pattern synonym"


{- **********************************************************************
*                                                                       *
                        Type
*                                                                       *
********************************************************************** -}

-- | The key representation of types within the compiler

type KindOrType = Type -- See Note [Arguments to type constructors]

-- | The key type representing kinds in the compiler.
type Kind = Type

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
data Type
  -- See Note [Non-trivial definitional equality]
  = TyVarTy Var -- ^ Vanilla type or kind variable (*never* a coercion variable)

  | AppTy
        Type
        Type            -- ^ Type application to something other than a 'TyCon'. Parameters:
                        --
                        --  1) Function: must /not/ be a 'TyConApp' or 'CastTy',
                        --     must be another 'AppTy', or 'TyVarTy'
                        --     See Note [Respecting definitional equality] (EQ1) about the
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

  | ForAllTy
        {-# UNPACK #-} !TyCoVarBinder
        Type            -- ^ A Π type.

  | FunTy Type Type     -- ^ t1 -> t2   Very common, so an important special case

  | LitTy TyLit     -- ^ Type literals are similar to type constructors.

  | CastTy
        Type
        KindCoercion  -- ^ A kind cast. The coercion is always nominal.
                      -- INVARIANT: The cast is never refl.
                      -- INVARIANT: The Type is not a CastTy (use TransCo instead)
                      -- See Note [Respecting definitional equality] (EQ2) and (EQ3)

  | CoercionTy
        Coercion    -- ^ Injection of a Coercion into a type
                    -- This should only ever be used in the RHS of an AppTy,
                    -- in the list of a TyConApp, when applying a promoted
                    -- GADT data constructor

  deriving Data.Data


-- NOTE:  Other parts of the code assume that type literals do not contain
-- types or type variables.
data TyLit
  = NumTyLit Integer
  | StrTyLit FastString
  deriving (Eq, Ord, Data.Data)

{- Note [Arguments to type constructors]
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
Is Int |> <*> the same as Int? YES! In order to reduce headaches,
we decide that any reflexive casts in types are just ignored.
(Indeed they must be. See Note [Respecting definitional equality].)
More generally, the `eqType` function, which defines Core's type equality
relation, ignores casts and coercion arguments, as long as the
two types have the same kind. This allows us to be a little sloppier
in keeping track of coercions, which is a good thing. It also means
that eqType does not depend on eqCoercion, which is also a good thing.

Why is this sensible? That is, why is something different than α-equivalence
appropriate for the implementation of eqType?

Anything smaller than ~ and homogeneous is an appropriate definition for
equality. The type safety of FC depends only on ~. Let's say η : τ ~ σ. Any
expression of type τ can be transmuted to one of type σ at any point by
casting. The same is true of expressions of type σ. So in some sense, τ and σ
are interchangeable.

But let's be more precise. If we examine the typing rules of FC (say, those in
https://cs.brynmawr.edu/~rae/papers/2015/equalities/equalities.pdf)
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
Richard E's thesis (http://cs.brynmawr.edu/~rae/papers/2016/thesis/eisenberg-thesis.pdf).

What would go wrong if we insisted on the casts matching? See the beginning of
Section 8 in the unpublished paper above. Theoretically, nothing at all goes
wrong. But in practical terms, getting the coercions right proved to be
nightmarish. And types would explode: during kind-checking, we often produce
reflexive kind coercions. When we try to cast by these, mkCastTy just discards
them. But if we used an eqType that distinguished between Int and Int |> <*>,
then we couldn't discard -- the output of kind-checking would be enormous,
and we would need enormous casts with lots of CoherenceCo's to straighten
them out.

Would anything go wrong if eqType respected type families? No, not at all. But
that makes eqType rather hard to implement.

Thus, the guideline for eqType is that it should be the largest
easy-to-implement relation that is still smaller than ~ and homogeneous. The
precise choice of relation is somewhat incidental, as long as the smart
constructors and destructors in Type respect whatever relation is chosen.

Another helpful principle with eqType is this:

 (EQ) If (t1 `eqType` t2) then I can replace t1 by t2 anywhere.

This principle also tells us that eqType must relate only types with the
same kinds.

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
in mkCastTy.

Unforunately, that's not the end of the story. Consider comparing
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

Lastly, in order to detect reflexive casts reliably, we must make sure not
to have nested casts: we update (t |> co1 |> co2) to (t |> (co1 `TransCo` co2)).

In sum, in order to uphold (EQ), we need the following three invariants:

  (EQ1) No decomposable CastTy to the left of an AppTy, where a decomposable
        cast is one that relates either a FunTy to a FunTy or a
        ForAllTy to a ForAllTy.
  (EQ2) No reflexive casts in CastTy.
  (EQ3) No nested CastTys.
  (EQ4) No CastTy over (ForAllTy (Bndr tyvar vis) body).
        See Note [Weird typing rule for ForAllTy] in Type.

These invariants are all documented above, in the declaration for Type.

Note [Unused coercion variable in ForAllTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  \(co:t1 ~ t2). e

What type should we give to this expression?
  (1) forall (co:t1 ~ t2) -> t
  (2) (t1 ~ t2) -> t

If co is used in t, (1) should be the right choice.
if co is not used in t, we would like to have (1) and (2) equivalent.

However, we want to keep eqType simple and don't want eqType (1) (2) to return
True in any case.

We decide to always construct (2) if co is not used in t.

Thus in mkTyCoForAllTy, we check whether the variable is a coercion
variable and whether it is used in the body. If so, it returns a FunTy
instead of a ForAllTy.

There are cases we want to skip the check. For example, the check is unnecessary
when it is known from the context that the input variable is a type variable.
In those cases, we use mkForAllTy.
-}

-- | A type labeled 'KnotTied' might have knot-tied tycons in it. See
-- Note [Type checking recursive type and class declarations] in
-- TcTyClsDecls
type KnotTied ty = ty

{- **********************************************************************
*                                                                       *
                  TyCoBinder and ArgFlag
*                                                                       *
********************************************************************** -}

-- | A 'TyCoBinder' represents an argument to a function. TyCoBinders can be
-- dependent ('Named') or nondependent ('Anon'). They may also be visible or
-- not. See Note [TyCoBinders]
data TyCoBinder
  = Named TyCoVarBinder -- A type-lambda binder
  | Anon Type           -- A term-lambda binder. Type here can be CoercionTy.
                        -- Visibility is determined by the type (Constraint vs. *)
  deriving Data.Data

-- | 'TyBinder' is like 'TyCoBinder', but there can only be 'TyVarBinder'
-- in the 'Named' field.
type TyBinder = TyCoBinder

-- | Remove the binder's variable from the set, if the binder has
-- a variable.
delBinderVar :: VarSet -> TyCoVarBinder -> VarSet
delBinderVar vars (Bndr tv _) = vars `delVarSet` tv

-- | Does this binder bind an invisible argument?
isInvisibleBinder :: TyCoBinder -> Bool
isInvisibleBinder (Named (Bndr _ vis)) = isInvisibleArgFlag vis
isInvisibleBinder (Anon ty)            = isPredTy ty

-- | Does this binder bind a visible argument?
isVisibleBinder :: TyCoBinder -> Bool
isVisibleBinder = not . isInvisibleBinder

isNamedBinder :: TyCoBinder -> Bool
isNamedBinder (Named {}) = True
isNamedBinder (Anon {})  = False

-- | If its a named binder, is the binder a tyvar?
-- Returns True for nondependent binder.
isTyBinder :: TyCoBinder -> Bool
isTyBinder (Named bnd) = isTyVarBinder bnd
isTyBinder _ = True

tyCoBinderArgFlag :: TyCoBinder -> ArgFlag
tyCoBinderArgFlag (Named (Bndr _ flag)) = flag
tyCoBinderArgFlag (Anon ty)
 | isPredTy ty = Inferred
 | otherwise = Required

{- Note [TyCoBinders]
~~~~~~~~~~~~~~~~~~~
A ForAllTy contains a TyCoVarBinder.  But a type can be decomposed
to a telescope consisting of a [TyCoBinder]

A TyCoBinder represents the type of binders -- that is, the type of an
argument to a Pi-type. GHC Core currently supports two different
Pi-types:

 * A non-dependent function type,
   written with ->, e.g. ty1 -> ty2
   represented as FunTy ty1 ty2. These are
   lifted to Coercions with the corresponding FunCo.

 * A dependent compile-time-only polytype,
   written with forall, e.g.  forall (a:*). ty
   represented as ForAllTy (Bndr a v) ty

Both Pi-types classify terms/types that take an argument. In other
words, if `x` is either a function or a polytype, `x arg` makes sense
(for an appropriate `arg`).


Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* A ForAllTy (used for both types and kinds) contains a TyCoVarBinder.
  Each TyCoVarBinder
      Bndr a tvis
  is equipped with tvis::ArgFlag, which says whether or not arguments
  for this binder should be visible (explicit) in source Haskell.

* A TyCon contains a list of TyConBinders.  Each TyConBinder
      Bndr a cvis
  is equipped with cvis::TyConBndrVis, which says whether or not type
  and kind arguments for this TyCon should be visible (explicit) in
  source Haskell.

This table summarises the visibility rules:
---------------------------------------------------------------------------------------
|                                                      Occurrences look like this
|                             GHC displays type as     in Haskell source code
|--------------------------------------------------------------------------------------
| Bndr a tvis :: TyCoVarBinder, in the binder of ForAllTy for a term
|  tvis :: ArgFlag
|  tvis = Inferred:            f :: forall {a}. type    Arg not allowed:  f
                               f :: forall {co}. type   Arg not allowed:  f
|  tvis = Specified:           f :: forall a. type      Arg optional:     f  or  f @Int
|  tvis = Required:            T :: forall k -> type    Arg required:     T *
|    This last form is illegal in terms: See Note [No Required TyCoBinder in terms]
|
| Bndr k cvis :: TyConBinder, in the TyConBinders of a TyCon
|  cvis :: TyConBndrVis
|  cvis = AnonTCB:             T :: kind -> kind        Required:            T *
|  cvis = NamedTCB Inferred:   T :: forall {k}. kind    Arg not allowed:     T
|                              T :: forall {co}. kind   Arg not allowed:     T
|  cvis = NamedTCB Specified:  T :: forall k. kind      Arg not allowed[1]:  T
|  cvis = NamedTCB Required:   T :: forall k -> kind    Required:            T *
---------------------------------------------------------------------------------------

[1] In types, in the Specified case, it would make sense to allow
    optional kind applications, thus (T @*), but we have not
    yet implemented that

---- In term declarations ----

* Inferred.  Function defn, with no signature:  f1 x = x
  We infer f1 :: forall {a}. a -> a, with 'a' Inferred
  It's Inferred because it doesn't appear in any
  user-written signature for f1

* Specified.  Function defn, with signature (implicit forall):
     f2 :: a -> a; f2 x = x
  So f2 gets the type f2 :: forall a. a -> a, with 'a' Specified
  even though 'a' is not bound in the source code by an explicit forall

* Specified.  Function defn, with signature (explicit forall):
     f3 :: forall a. a -> a; f3 x = x
  So f3 gets the type f3 :: forall a. a -> a, with 'a' Specified

* Inferred/Specified.  Function signature with inferred kind polymorphism.
     f4 :: a b -> Int
  So 'f4' gets the type f4 :: forall {k} (a:k->*) (b:k). a b -> Int
  Here 'k' is Inferred (it's not mentioned in the type),
  but 'a' and 'b' are Specified.

* Specified.  Function signature with explicit kind polymorphism
     f5 :: a (b :: k) -> Int
  This time 'k' is Specified, because it is mentioned explicitly,
  so we get f5 :: forall (k:*) (a:k->*) (b:k). a b -> Int

* Similarly pattern synonyms:
  Inferred - from inferred types (e.g. no pattern type signature)
           - or from inferred kind polymorphism

---- In type declarations ----

* Inferred (k)
     data T1 a b = MkT1 (a b)
  Here T1's kind is  T1 :: forall {k:*}. (k->*) -> k -> *
  The kind variable 'k' is Inferred, since it is not mentioned

  Note that 'a' and 'b' correspond to /Anon/ TyCoBinders in T1's kind,
  and Anon binders don't have a visibility flag. (Or you could think
  of Anon having an implicit Required flag.)

* Specified (k)
     data T2 (a::k->*) b = MkT (a b)
  Here T's kind is  T :: forall (k:*). (k->*) -> k -> *
  The kind variable 'k' is Specified, since it is mentioned in
  the signature.

* Required (k)
     data T k (a::k->*) b = MkT (a b)
  Here T's kind is  T :: forall k:* -> (k->*) -> k -> *
  The kind is Required, since it bound in a positional way in T's declaration
  Every use of T must be explicitly applied to a kind

* Inferred (k1), Specified (k)
     data T a b (c :: k) = MkT (a b) (Proxy c)
  Here T's kind is  T :: forall {k1:*} (k:*). (k1->*) -> k1 -> k -> *
  So 'k' is Specified, because it appears explicitly,
  but 'k1' is Inferred, because it does not

Generally, in the list of TyConBinders for a TyCon,

* Inferred arguments always come first
* Specified, Anon and Required can be mixed

e.g.
  data Foo (a :: Type) :: forall b. (a -> b -> Type) -> Type where ...

Here Foo's TyConBinders are
   [Required 'a', Specified 'b', Anon]
and its kind prints as
   Foo :: forall a -> forall b. (a -> b -> Type) -> Type

See also Note [Required, Specified, and Inferred for types] in TcTyClsDecls

---- Printing -----

 We print forall types with enough syntax to tell you their visibility
 flag.  But this is not source Haskell, and these types may not all
 be parsable.

 Specified: a list of Specified binders is written between `forall` and `.`:
               const :: forall a b. a -> b -> a

 Inferred:  with -fprint-explicit-foralls, Inferred binders are written
            in braces:
               f :: forall {k} (a:k). S k a -> Int
            Otherwise, they are printed like Specified binders.

 Required: binders are put between `forall` and `->`:
              T :: forall k -> *

---- Other points -----

* In classic Haskell, all named binders (that is, the type variables in
  a polymorphic function type f :: forall a. a -> a) have been Inferred.

* Inferred variables correspond to "generalized" variables from the
  Visible Type Applications paper (ESOP'16).

Note [No Required TyCoBinder in terms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't allow Required foralls for term variables, including pattern
synonyms and data constructors.  Why?  Because then an application
would need a /compulsory/ type argument (possibly without an "@"?),
thus (f Int); and we don't have concrete syntax for that.

We could change this decision, but Required, Named TyCoBinders are rare
anyway.  (Most are Anons.)

However the type of a term can (just about) have a required quantifier;
see Note [Required quantifiers in the type of a term] in TcExpr.
-}


{- **********************************************************************
*                                                                       *
                        PredType
*                                                                       *
********************************************************************** -}


-- | A type of the form @p@ of kind @Constraint@ represents a value whose type is
-- the Haskell predicate @p@, where a predicate is what occurs before
-- the @=>@ in a Haskell type.
--
-- We use 'PredType' as documentation to mark those types that we guarantee to have
-- this kind.
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

These functions are here so that they can be used by TysPrim,
which in turn is imported by Type
-}

mkTyVarTy  :: TyVar   -> Type
mkTyVarTy v = ASSERT2( isTyVar v, ppr v <+> dcolon <+> ppr (tyVarKind v) )
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

infixr 3 `mkFunTy`      -- Associates to the right
-- | Make an arrow type
mkFunTy :: Type -> Type -> Type
mkFunTy arg res = FunTy arg res

-- | Make nested arrow types
mkFunTys :: [Type] -> Type -> Type
mkFunTys tys ty = foldr mkFunTy ty tys

-- | If tv is a coercion variable and it is not used in the body, returns
-- a FunTy, otherwise makes a forall type.
-- See Note [Unused coercion variable in ForAllTy]
mkTyCoForAllTy :: TyCoVar -> ArgFlag -> Type -> Type
mkTyCoForAllTy tv vis ty
  | isCoVar tv
  , not (tv `elemVarSet` tyCoVarsOfType ty)
  = ASSERT( vis == Inferred )
    mkFunTy (varType tv) ty
  | otherwise
  = ForAllTy (Bndr tv vis) ty

-- | Like 'mkTyCoForAllTy', but does not check the occurrence of the binder
-- See Note [Unused coercion variable in ForAllTy]
mkForAllTy :: TyCoVar -> ArgFlag -> Type -> Type
mkForAllTy tv vis ty = ForAllTy (Bndr tv vis) ty

-- | Wraps foralls over the type using the provided 'TyCoVar's from left to right
mkForAllTys :: [TyCoVarBinder] -> Type -> Type
mkForAllTys tyvars ty = foldr ForAllTy ty tyvars

mkTyCoPiTy :: TyCoBinder -> Type -> Type
mkTyCoPiTy (Anon ty1) ty2           = FunTy ty1 ty2
mkTyCoPiTy (Named (Bndr tv vis)) ty = mkTyCoForAllTy tv vis ty

-- | Like 'mkTyCoPiTy', but does not check the occurrence of the binder
mkPiTy:: TyCoBinder -> Type -> Type
mkPiTy (Anon ty1) ty2           = FunTy ty1 ty2
mkPiTy (Named (Bndr tv vis)) ty = mkForAllTy tv vis ty

mkTyCoPiTys :: [TyCoBinder] -> Type -> Type
mkTyCoPiTys tbs ty = foldr mkTyCoPiTy ty tbs

-- | Like 'mkTyCoPiTys', but does not check the occurrence of the binder
mkPiTys :: [TyCoBinder] -> Type -> Type
mkPiTys tbs ty = foldr mkPiTy ty tbs

-- | Create the plain type constructor type which has been applied to no type arguments at all.
mkTyConTy :: TyCon -> Type
mkTyConTy tycon = TyConApp tycon []

{-
Some basic functions, put here to break loops eg with the pretty printer
-}

-- | Extract the RuntimeRep classifier of a type from its kind. For example,
-- @kindRep * = LiftedRep@; Panics if this is not possible.
-- Treats * and Constraint as the same
kindRep :: HasDebugCallStack => Kind -> Type
kindRep k = case kindRep_maybe k of
              Just r  -> r
              Nothing -> pprPanic "kindRep" (ppr k)

-- | Given a kind (TYPE rr), extract its RuntimeRep classifier rr.
-- For example, @kindRep_maybe * = Just LiftedRep@
-- Returns 'Nothing' if the kind is not of form (TYPE rr)
-- Treats * and Constraint as the same
kindRep_maybe :: HasDebugCallStack => Kind -> Maybe Type
kindRep_maybe kind
  | Just kind' <- coreView kind = kindRep_maybe kind'
  | TyConApp tc [arg] <- kind
  , tc `hasKey` tYPETyConKey    = Just arg
  | otherwise                   = Nothing

-- | This version considers Constraint to be the same as *. Returns True
-- if the argument is equivalent to Type/Constraint and False otherwise.
-- See Note [Kind Constraint and kind Type]
isLiftedTypeKind :: Kind -> Bool
isLiftedTypeKind kind
  = case kindRep_maybe kind of
      Just rep -> isLiftedRuntimeRep rep
      Nothing  -> False

-- | Returns True if the kind classifies unlifted types and False otherwise.
-- Note that this returns False for levity-polymorphic kinds, which may
-- be specialized to a kind that classifies unlifted types.
isUnliftedTypeKind :: Kind -> Bool
isUnliftedTypeKind kind
  = case kindRep_maybe kind of
      Just rep -> isUnliftedRuntimeRep rep
      Nothing  -> False

isLiftedRuntimeRep, isUnliftedRuntimeRep :: Type -> Bool
-- isLiftedRuntimeRep is true of LiftedRep :: RuntimeRep
-- Similarly isUnliftedRuntimeRep
isLiftedRuntimeRep rep
  | Just rep' <- coreView rep          = isLiftedRuntimeRep rep'
  | TyConApp rr_tc args <- rep
  , rr_tc `hasKey` liftedRepDataConKey = ASSERT( null args ) True
  | otherwise                          = False

isUnliftedRuntimeRep rep
  | Just rep' <- coreView rep          = isUnliftedRuntimeRep rep'
  | TyConApp rr_tc args <- rep
  , isUnliftedRuntimeRepTyCon rr_tc    = ASSERT( null args ) True
  | otherwise                          = False

isUnliftedRuntimeRepTyCon :: TyCon -> Bool
isUnliftedRuntimeRepTyCon rr_tc
  = elem (getUnique rr_tc) unliftedRepDataConKeys

-- | Is this the type 'RuntimeRep'?
isRuntimeRepTy :: Type -> Bool
isRuntimeRepTy ty | Just ty' <- coreView ty = isRuntimeRepTy ty'
isRuntimeRepTy (TyConApp tc []) = tc `hasKey` runtimeRepTyConKey
isRuntimeRepTy _ = False

-- | Is a tyvar of type 'RuntimeRep'?
isRuntimeRepVar :: TyVar -> Bool
isRuntimeRepVar = isRuntimeRepTy . tyVarKind

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
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
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
    Refl Type  -- See Note [Refl invariant]
          -- Invariant: applications of (Refl T) to a bunch of identity coercions
          --            always show up as Refl.
          -- For example  (Refl T) (Refl a) (Refl b) shows up as (Refl (T a b)).

          -- Applications of (Refl T) to some coercions, at least one of
          -- which is NOT the identity, show up as TyConAppCo.
          -- (They may not be fully saturated however.)
          -- ConAppCo coercions (like all coercions other than Refl)
          -- are NEVER the identity.

          -- Use (GRefl Representational ty MRefl), not (SubCo (Refl ty))

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

  | AppCo Coercion CoercionN             -- lift AppTy
          -- AppCo :: e -> N -> e

  -- See Note [Forall coercions]
  | ForAllCo TyCoVar KindCoercion Coercion
         -- ForAllCo :: _ -> N -> e -> e

  | FunCo Role Coercion Coercion         -- lift FunTy
         -- FunCo :: "e" -> e -> e -> e

  -- These are special
  | CoVarCo CoVar      -- :: _ -> (N or R)
                       -- result role depends on the tycon of the variable's type

    -- AxiomInstCo :: e -> _ -> [N] -> e
  | AxiomInstCo (CoAxiom Branched) BranchIndex [Coercion]
     -- See also [CoAxiom index]
     -- The coercion arguments always *precisely* saturate
     -- arity of (that branch of) the CoAxiom. If there are
     -- any left over, we use AppCo.
     -- See [Coercion axioms applied to coercions]

  | AxiomRuleCo CoAxiomRule [Coercion]
    -- AxiomRuleCo is very like AxiomInstCo, but for a CoAxiomRule
    -- The number coercions should match exactly the expectations
    -- of the CoAxiomRule (i.e., the rule is fully saturated).

  | UnivCo UnivCoProvenance Role Type Type
      -- :: _ -> "e" -> _ -> _ -> e

  | SymCo Coercion             -- :: e -> e
  | TransCo Coercion Coercion  -- :: e -> e -> e

  | NthCo  Role Int Coercion     -- Zero-indexed; decomposes (T t0 ... tn)
    -- :: "e" -> _ -> e0 -> e (inverse of TyConAppCo, see Note [TyConAppCo roles])
    -- Using NthCo on a ForAllCo gives an N coercion always
    -- See Note [NthCo and newtypes]
    --
    -- Invariant:  (NthCo r i co), it is always the case that r = role of (Nth i co)
    -- That is: the role of the entire coercion is redundantly cached here.
    -- See Note [NthCo Cached Roles]

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

type CoercionN = Coercion       -- always nominal
type CoercionR = Coercion       -- always representational
type CoercionP = Coercion       -- always phantom
type KindCoercion = CoercionN   -- always nominal

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

{-
Note [Refl invariant]
~~~~~~~~~~~~~~~~~~~~~
Invariant 1:

Coercions have the following invariant
     Refl (similar for GRefl r ty MRefl) is always lifted as far as possible.

You might think that a consequencs is:
     Every identity coercions has Refl at the root

But that's not quite true because of coercion variables.  Consider
     g         where g :: Int~Int
     Left h    where h :: Maybe Int ~ Maybe Int
etc.  So the consequence is only true of coercions that
have no coercion variables.

Note [Generalized reflexive coercion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GRefl is a generalized reflexive coercion (see Trac #15192). It wraps a kind
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

Note [CoAxiom index]
~~~~~~~~~~~~~~~~~~~~
A CoAxiom has 1 or more branches. Each branch has contains a list
of the free type variables in that branch, the LHS type patterns,
and the RHS type for that branch. When we apply an axiom to a list
of coercions, we must choose which branch of the axiom we wish to
use, as the different branches may have different numbers of free
type variables. (The number of type patterns is always the same
among branches, but that doesn't quite concern us here.)

The Int in the AxiomInstCo constructor is the 0-indexed number
of the chosen branch.

Note [Forall coercions]
~~~~~~~~~~~~~~~~~~~~~~~
Constructing coercions between forall-types can be a bit tricky,
because the kinds of the bound tyvars can be different.

The typing rule is:


  kind_co : k1 ~ k2
  tv1:k1 |- co : t1 ~ t2
  -------------------------------------------------------------------
  ForAllCo tv1 kind_co co : all tv1:k1. t1  ~
                            all tv1:k2. (t2[tv1 |-> tv1 |> sym kind_co])

First, the TyCoVar stored in a ForAllCo is really an optimisation: this field
should be a Name, as its kind is redundant. Thinking of the field as a Name
is helpful in understanding what a ForAllCo means.
The kind of TyCoVar always matches the left-hand kind of the coercion.

The idea is that kind_co gives the two kinds of the tyvar. See how, in the
conclusion, tv1 is assigned kind k1 on the left but kind k2 on the right.

Of course, a type variable can't have different kinds at the same time. So,
we arbitrarily prefer the first kind when using tv1 in the inner coercion
co, which shows that t1 equals t2.

The last wrinkle is that we need to fix the kinds in the conclusion. In
t2, tv1 is assumed to have kind k1, but it has kind k2 in the conclusion of
the rule. So we do a kind-fixing substitution, replacing (tv1:k1) with
(tv1:k2) |> sym kind_co. This substitution is slightly bizarre, because it
mentions the same name with different kinds, but it *is* well-kinded, noting
that `(tv1:k2) |> sym kind_co` has kind k1.

This all really would work storing just a Name in the ForAllCo. But we can't
add Names to, e.g., VarSets, and there generally is just an impedance mismatch
in a bunch of places. So we use tv1. When we need tv2, we can use
setTyVarKind.

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
    Nth 1 ((~) [c] g) = g
See Simplify.simplCoercionF, which generates such selections.

Note [Roles]
~~~~~~~~~~~~
Roles are a solution to the GeneralizedNewtypeDeriving problem, articulated
in Trac #1496. The full story is in docs/core-spec/core-spec.pdf. Also, see
http://ghc.haskell.org/trac/ghc/wiki/RolesImplementation

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

Note [NthCo and newtypes]
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
NthCo, we'll get out a representational coercion. That is:

  NthCo r 0 co :: forall a b. a ~R b

Yikes! Clearly, this is terrible. The solution is simple: forbid
NthCo to be used on newtypes if the internal coercion is representational.

This is not just some corner case discovered by a segfault somewhere;
it was discovered in the proof of soundness of roles and described
in the "Safe Coercions" paper (ICFP '14).

Note [NthCo Cached Roles]
~~~~~~~~~~~~~~~~~~~~~~~~~
Why do we cache the role of NthCo in the NthCo constructor?
Because computing role(Nth i co) involves figuring out that

  co :: T tys1 ~ T tys2

using coercionKind, and finding (coercionRole co), and then looking
at the tyConRoles of T. Avoiding bad asymptotic behaviour here means
we have to compute the kind and role of a coercion simultaneously,
which makes the code complicated and inefficient.

This only happens for NthCo. Caching the role solves the problem, and
allows coercionKind and coercionRole to be simple.

See Trac #11735

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
                UnivCoProvenance
%*                                                                      *
%************************************************************************

A UnivCo is a coercion whose proof does not directly express its role
and kind (indeed for some UnivCos, like UnsafeCoerceProv, there /is/
no proof).

The different kinds of UnivCo are described by UnivCoProvenance.  Really
each is entirely separate, but they all share the need to represent their
role and kind, which is done in the UnivCo constructor.

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
  = UnsafeCoerceProv   -- ^ From @unsafeCoerce#@. These are unsound.

  | PhantomProv KindCoercion -- ^ See Note [Phantom coercions]. Only in Phantom
                             -- roled coercions

  | ProofIrrelProv KindCoercion  -- ^ From the fact that any two coercions are
                                 --   considered equivalent. See Note [ProofIrrelProv].
                                 -- Can be used in Nominal or Representational coercions

  | PluginProv String  -- ^ From a plugin, which asserts that this coercion
                       --   is sound. The string is for the use of the plugin.

  deriving Data.Data

instance Outputable UnivCoProvenance where
  ppr UnsafeCoerceProv   = text "(unsafeCoerce#)"
  ppr (PhantomProv _)    = text "(phantom)"
  ppr (ProofIrrelProv _) = text "(proof irrel.)"
  ppr (PluginProv str)   = parens (text "plugin" <+> brackets (text str))

-- | A coercion to be filled in by the type-checker. See Note [Coercion holes]
data CoercionHole
  = CoercionHole { ch_co_var :: CoVar
                       -- See Note [CoercionHoles and coercion free variables]

                 , ch_ref    :: IORef (Maybe Coercion)
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

Note [Coercion holes]
~~~~~~~~~~~~~~~~~~~~~~~~
During typechecking, constraint solving for type classes works by
  - Generate an evidence Id,  d7 :: Num a
  - Wrap it in a Wanted constraint, [W] d7 :: Num a
  - Use the evidence Id where the evidence is needed
  - Solve the constraint later
  - When solved, add an enclosing let-binding  let d7 = .... in ....
    which actually binds d7 to the (Num a) evidence

For equality constraints we use a different strategy.  See Note [The
equality types story] in TysPrim for background on equality constraints.
  - For /boxed/ equality constraints, (t1 ~N t2) and (t1 ~R t2), it's just
    like type classes above. (Indeed, boxed equality constraints *are* classes.)
  - But for /unboxed/ equality constraints (t1 ~R# t2) and (t1 ~N# t2)
    we use a different plan

For unboxed equalities:
  - Generate a CoercionHole, a mutable variable just like a unification
    variable
  - Wrap the CoercionHole in a Wanted constraint; see TcRnTypes.TcEvDest
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
    dicionaries want to achieve sharing to avoid recomoputing the
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
the coercion.  See Trac #14584, and Note [What prevents a
constraint from floating] in TcSimplify, item (4):

        forall k. [W] co1 :: t1 ~# t2 |> co2
                  [W] co2 :: k ~# *

Here co2 is a CoercionHole. But we /must/ know that it is free in
co1, because that's all that stops it floating outside the
implication.


Note [ProofIrrelProv]
~~~~~~~~~~~~~~~~~~~~~
A ProofIrrelProv is a coercion between coercions. For example:

  data G a where
    MkG :: G Bool

In core, we get

  G :: * -> *
  MkG :: forall (a :: *). (a ~ Bool) -> G a

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
  co3 = UnivCo (ProofIrrelProv co5) Nominal (CoercionTy co1) (CoercionTy co2)
  where
    co5 :: (a1 ~ Bool) ~ (a2 ~ Bool)
    co5 = TyConAppCo Nominal (~#) [<*>, <*>, co4, <Bool>]


%************************************************************************
%*                                                                      *
                 Free variables of types and coercions
%*                                                                      *
%************************************************************************
-}

{- Note [Free variables of types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The family of functions tyCoVarsOfType, tyCoVarsOfTypes etc, returns
a VarSet that is closed over the types of its variables.  More precisely,
  if    S = tyCoVarsOfType( t )
  and   (a:k) is in S
  then  tyCoVarsOftype( k ) is a subset of S

Example: The tyCoVars of this ((a:* -> k) Int) is {a, k}.

We could /not/ close over the kinds of the variable occurrences, and
instead do so at call sites, but it seems that we always want to do
so, so it's easiest to do it here.

It turns out that getting the free variables of types is performance critical,
so we profiled several versions, exploring different implementation strategies.

1. Baseline version: uses FV naively. Essentially:

   tyCoVarsOfType ty = fvVarSet $ tyCoFVsOfType ty

   This is not nice, because FV introduces some overhead to implement
   determinism, and throught its "interesting var" function, neither of which
   we need here, so they are a complete waste.

2. UnionVarSet version: instead of reusing the FV-based code, we simply used
   VarSets directly, trying to avoid the overhead of FV. E.g.:

   -- FV version:
   tyCoFVsOfType (AppTy fun arg)    a b c = (tyCoFVsOfType fun `unionFV` tyCoFVsOfType arg) a b c

   -- UnionVarSet version:
   tyCoVarsOfType (AppTy fun arg)    = (tyCoVarsOfType fun `unionVarSet` tyCoVarsOfType arg)

   This looks deceptively similar, but while FV internally builds a list- and
   set-generating function, the VarSet functions manipulate sets directly, and
   the latter peforms a lot worse than the naive FV version.

3. Accumulator-style VarSet version: this is what we use now. We do use VarSet
   as our data structure, but delegate the actual work to a new
   ty_co_vars_of_...  family of functions, which use accumulator style and the
   "in-scope set" filter found in the internals of FV, but without the
   determinism overhead.

See Trac #14880.

Note [Closing over free variable kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tyCoVarsOfType and tyCoFVsOfType, while traversing a type, will also close over
free variable kinds. In previous GHC versions, this happened naively: whenever
we would encounter an occurrence of a free type variable, we would close over
its kind. This, however is wrong for two reasons (see Trac #14880):

1. Efficiency. If we have Proxy (a::k) -> Proxy (a::k) -> Proxy (a::k), then
   we don't want to have to traverse k more than once.

2. Correctness. Imagine we have forall k. b -> k, where b has
   kind k, for some k bound in an outer scope. If we look at b's kind inside
   the forall, we'll collect that k is free and then remove k from the set of
   free variables. This is plain wrong. We must instead compute that b is free
   and then conclude that b's kind is free.

An obvious first approach is to move the closing-over-kinds from the
occurrences of a type variable to after finding the free vars - however, this
turns out to introduce performance regressions, and isn't even entirely
correct.

In fact, it isn't even important *when* we close over kinds; what matters is
that we handle each type var exactly once, and that we do it in the right
context.

So the next approach we tried was to use the "in-scope set" part of FV or the
equivalent argument in the accumulator-style `ty_co_vars_of_type` function, to
say "don't bother with variables we have already closed over". This should work
fine in theory, but the code is complicated and doesn't perform well.

But there is a simpler way, which is implemented here. Consider the two points
above:

1. Efficiency: we now have an accumulator, so the second time we encounter 'a',
   we'll ignore it, certainly not looking at its kind - this is why
   pre-checking set membership before inserting ends up not only being faster,
   but also being correct.

2. Correctness: we have an "in-scope set" (I think we should call it it a
  "bound-var set"), specifying variables that are bound by a forall in the type
  we are traversing; we simply ignore these variables, certainly not looking at
  their kind.

So now consider:

    forall k. b -> k

where b :: k->Type is free; but of course, it's a different k! When looking at
b -> k we'll have k in the bound-var set. So we'll ignore the k. But suppose
this is our first encounter with b; we want the free vars of its kind. But we
want to behave as if we took the free vars of its kind at the end; that is,
with no bound vars in scope.

So the solution is easy. The old code was this:

  ty_co_vars_of_type (TyVarTy v) is acc
    | v `elemVarSet` is  = acc
    | v `elemVarSet` acc = acc
    | otherwise          = ty_co_vars_of_type (tyVarKind v) is (extendVarSet acc v)

Now all we need to do is take the free vars of tyVarKind v *with an empty
bound-var set*, thus:

ty_co_vars_of_type (TyVarTy v) is acc
  | v `elemVarSet` is  = acc
  | v `elemVarSet` acc = acc
  | otherwise          = ty_co_vars_of_type (tyVarKind v) emptyVarSet (extendVarSet acc v)
                                                          ^^^^^^^^^^^

And that's it.

-}

tyCoVarsOfType :: Type -> TyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfType ty = ty_co_vars_of_type ty emptyVarSet emptyVarSet

tyCoVarsOfTypes :: [Type] -> TyCoVarSet
tyCoVarsOfTypes tys = ty_co_vars_of_types tys emptyVarSet emptyVarSet

ty_co_vars_of_type :: Type -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_type (TyVarTy v) is acc
  | v `elemVarSet` is  = acc
  | v `elemVarSet` acc = acc
  | otherwise          = ty_co_vars_of_type (tyVarKind v)
                            emptyVarSet  -- See Note [Closing over free variable kinds]
                            (extendVarSet acc v)

ty_co_vars_of_type (TyConApp _ tys)   is acc = ty_co_vars_of_types tys is acc
ty_co_vars_of_type (LitTy {})         _  acc = acc
ty_co_vars_of_type (AppTy fun arg)    is acc = ty_co_vars_of_type fun is (ty_co_vars_of_type arg is acc)
ty_co_vars_of_type (FunTy arg res)    is acc = ty_co_vars_of_type arg is (ty_co_vars_of_type res is acc)
ty_co_vars_of_type (ForAllTy (Bndr tv _) ty) is acc = ty_co_vars_of_type (varType tv) is $
                                                      ty_co_vars_of_type ty (extendVarSet is tv) acc
ty_co_vars_of_type (CastTy ty co)     is acc = ty_co_vars_of_type ty is (ty_co_vars_of_co co is acc)
ty_co_vars_of_type (CoercionTy co)    is acc = ty_co_vars_of_co co is acc

ty_co_vars_of_types :: [Type] -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_types []       _  acc = acc
ty_co_vars_of_types (ty:tys) is acc = ty_co_vars_of_type ty is (ty_co_vars_of_types tys is acc)

tyCoVarsOfCo :: Coercion -> TyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfCo co = ty_co_vars_of_co co emptyVarSet emptyVarSet

tyCoVarsOfCos :: [Coercion] -> TyCoVarSet
tyCoVarsOfCos cos = ty_co_vars_of_cos cos emptyVarSet emptyVarSet


ty_co_vars_of_co :: Coercion -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_co (Refl ty)            is acc = ty_co_vars_of_type ty is acc
ty_co_vars_of_co (GRefl _ ty mco)     is acc = ty_co_vars_of_type ty is $
                                               ty_co_vars_of_mco mco is acc
ty_co_vars_of_co (TyConAppCo _ _ cos) is acc = ty_co_vars_of_cos cos is acc
ty_co_vars_of_co (AppCo co arg)       is acc = ty_co_vars_of_co co is $
                                               ty_co_vars_of_co arg is acc
ty_co_vars_of_co (ForAllCo tv kind_co co) is acc = ty_co_vars_of_co kind_co is $
                                                   ty_co_vars_of_co co (extendVarSet is tv) acc
ty_co_vars_of_co (FunCo _ co1 co2)    is acc = ty_co_vars_of_co co1 is $
                                               ty_co_vars_of_co co2 is acc
ty_co_vars_of_co (CoVarCo v)          is acc = ty_co_vars_of_co_var v is acc
ty_co_vars_of_co (HoleCo h)           is acc = ty_co_vars_of_co_var (coHoleCoVar h) is acc
    -- See Note [CoercionHoles and coercion free variables]
ty_co_vars_of_co (AxiomInstCo _ _ cos) is acc = ty_co_vars_of_cos cos is acc
ty_co_vars_of_co (UnivCo p _ t1 t2)    is acc = ty_co_vars_of_prov p is $
                                                ty_co_vars_of_type t1 is $
                                                ty_co_vars_of_type t2 is acc
ty_co_vars_of_co (SymCo co)          is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_co (TransCo co1 co2)   is acc = ty_co_vars_of_co co1 is $
                                              ty_co_vars_of_co co2 is acc
ty_co_vars_of_co (NthCo _ _ co)      is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_co (LRCo _ co)         is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_co (InstCo co arg)     is acc = ty_co_vars_of_co co is $
                                              ty_co_vars_of_co arg is acc
ty_co_vars_of_co (KindCo co)         is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_co (SubCo co)          is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_co (AxiomRuleCo _ cs)  is acc = ty_co_vars_of_cos cs is acc

ty_co_vars_of_mco :: MCoercion -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_mco MRefl    _is acc = acc
ty_co_vars_of_mco (MCo co) is  acc = ty_co_vars_of_co co is acc

ty_co_vars_of_co_var :: CoVar -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_co_var v is acc
  | v `elemVarSet` is  = acc
  | v `elemVarSet` acc = acc
  | otherwise          = ty_co_vars_of_type (varType v)
                            emptyVarSet  -- See Note [Closing over free variable kinds]
                            (extendVarSet acc v)

ty_co_vars_of_cos :: [Coercion] -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_cos []       _  acc = acc
ty_co_vars_of_cos (co:cos) is acc = ty_co_vars_of_co co is (ty_co_vars_of_cos cos is acc)

tyCoVarsOfProv :: UnivCoProvenance -> TyCoVarSet
tyCoVarsOfProv prov = ty_co_vars_of_prov prov emptyVarSet emptyVarSet

ty_co_vars_of_prov :: UnivCoProvenance -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_prov (PhantomProv co)    is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_prov (ProofIrrelProv co) is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_prov UnsafeCoerceProv    _  acc = acc
ty_co_vars_of_prov (PluginProv _)      _  acc = acc

-- | Generates an in-scope set from the free variables in a list of types
-- and a list of coercions
mkTyCoInScopeSet :: [Type] -> [Coercion] -> InScopeSet
mkTyCoInScopeSet tys cos
  = mkInScopeSet (ty_co_vars_of_types tys emptyVarSet $
                  ty_co_vars_of_cos   cos emptyVarSet emptyVarSet)

-- | `tyCoFVsOfType` that returns free variables of a type in a deterministic
-- set. For explanation of why using `VarSet` is not deterministic see
-- Note [Deterministic FV] in FV.
tyCoVarsOfTypeDSet :: Type -> DTyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfTypeDSet ty = fvDVarSet $ tyCoFVsOfType ty

-- | `tyCoFVsOfType` that returns free variables of a type in deterministic
-- order. For explanation of why using `VarSet` is not deterministic see
-- Note [Deterministic FV] in FV.
tyCoVarsOfTypeList :: Type -> [TyCoVar]
-- See Note [Free variables of types]
tyCoVarsOfTypeList ty = fvVarList $ tyCoFVsOfType ty

-- | Returns free variables of types, including kind variables as
-- a non-deterministic set. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypesSet :: TyVarEnv Type -> TyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfTypesSet tys = tyCoVarsOfTypes $ nonDetEltsUFM tys
  -- It's OK to use nonDetEltsUFM here because we immediately forget the
  -- ordering by returning a set

-- | Returns free variables of types, including kind variables as
-- a deterministic set. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypesDSet :: [Type] -> DTyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfTypesDSet tys = fvDVarSet $ tyCoFVsOfTypes tys

-- | Returns free variables of types, including kind variables as
-- a deterministically ordered list. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypesList :: [Type] -> [TyCoVar]
-- See Note [Free variables of types]
tyCoVarsOfTypesList tys = fvVarList $ tyCoFVsOfTypes tys

-- | The worker for `tyCoFVsOfType` and `tyCoFVsOfTypeList`.
-- The previous implementation used `unionVarSet` which is O(n+m) and can
-- make the function quadratic.
-- It's exported, so that it can be composed with
-- other functions that compute free variables.
-- See Note [FV naming conventions] in FV.
--
-- Eta-expanded because that makes it run faster (apparently)
-- See Note [FV eta expansion] in FV for explanation.
tyCoFVsOfType :: Type -> FV
-- See Note [Free variables of types]
tyCoFVsOfType (TyVarTy v)        f bound_vars (acc_list, acc_set)
  | not (f v) = (acc_list, acc_set)
  | v `elemVarSet` bound_vars = (acc_list, acc_set)
  | v `elemVarSet` acc_set = (acc_list, acc_set)
  | otherwise = tyCoFVsOfType (tyVarKind v) f
                               emptyVarSet   -- See Note [Closing over free variable kinds]
                               (v:acc_list, extendVarSet acc_set v)
tyCoFVsOfType (TyConApp _ tys)   f bound_vars acc = tyCoFVsOfTypes tys f bound_vars acc
tyCoFVsOfType (LitTy {})         f bound_vars acc = emptyFV f bound_vars acc
tyCoFVsOfType (AppTy fun arg)    f bound_vars acc = (tyCoFVsOfType fun `unionFV` tyCoFVsOfType arg) f bound_vars acc
tyCoFVsOfType (FunTy arg res)    f bound_vars acc = (tyCoFVsOfType arg `unionFV` tyCoFVsOfType res) f bound_vars acc
tyCoFVsOfType (ForAllTy bndr ty) f bound_vars acc = tyCoFVsBndr bndr (tyCoFVsOfType ty)  f bound_vars acc
tyCoFVsOfType (CastTy ty co)     f bound_vars acc = (tyCoFVsOfType ty `unionFV` tyCoFVsOfCo co) f bound_vars acc
tyCoFVsOfType (CoercionTy co)    f bound_vars acc = tyCoFVsOfCo co f bound_vars acc

tyCoFVsBndr :: TyCoVarBinder -> FV -> FV
-- Free vars of (forall b. <thing with fvs>)
tyCoFVsBndr (Bndr tv _) fvs = tyCoFVsVarBndr tv fvs

tyCoFVsVarBndrs :: [Var] -> FV -> FV
tyCoFVsVarBndrs vars fvs = foldr tyCoFVsVarBndr fvs vars

tyCoFVsVarBndr :: Var -> FV -> FV
tyCoFVsVarBndr var fvs
  = tyCoFVsOfType (varType var)   -- Free vars of its type/kind
    `unionFV` delFV var fvs       -- Delete it from the thing-inside

tyCoFVsOfTypes :: [Type] -> FV
-- See Note [Free variables of types]
tyCoFVsOfTypes (ty:tys) fv_cand in_scope acc = (tyCoFVsOfType ty `unionFV` tyCoFVsOfTypes tys) fv_cand in_scope acc
tyCoFVsOfTypes []       fv_cand in_scope acc = emptyFV fv_cand in_scope acc

-- | Get a deterministic set of the vars free in a coercion
tyCoVarsOfCoDSet :: Coercion -> DTyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfCoDSet co = fvDVarSet $ tyCoFVsOfCo co

tyCoVarsOfCoList :: Coercion -> [TyCoVar]
-- See Note [Free variables of types]
tyCoVarsOfCoList co = fvVarList $ tyCoFVsOfCo co

tyCoFVsOfMCo :: MCoercion -> FV
tyCoFVsOfMCo MRefl    = emptyFV
tyCoFVsOfMCo (MCo co) = tyCoFVsOfCo co

tyCoVarsOfCosSet :: CoVarEnv Coercion -> TyCoVarSet
tyCoVarsOfCosSet cos = tyCoVarsOfCos $ nonDetEltsUFM cos
  -- It's OK to use nonDetEltsUFM here because we immediately forget the
  -- ordering by returning a set

tyCoFVsOfCo :: Coercion -> FV
-- Extracts type and coercion variables from a coercion
-- See Note [Free variables of types]
tyCoFVsOfCo (Refl ty) fv_cand in_scope acc
  = tyCoFVsOfType ty fv_cand in_scope acc
tyCoFVsOfCo (GRefl _ ty mco) fv_cand in_scope acc
  = (tyCoFVsOfType ty `unionFV` tyCoFVsOfMCo mco) fv_cand in_scope acc
tyCoFVsOfCo (TyConAppCo _ _ cos) fv_cand in_scope acc = tyCoFVsOfCos cos fv_cand in_scope acc
tyCoFVsOfCo (AppCo co arg) fv_cand in_scope acc
  = (tyCoFVsOfCo co `unionFV` tyCoFVsOfCo arg) fv_cand in_scope acc
tyCoFVsOfCo (ForAllCo tv kind_co co) fv_cand in_scope acc
  = (tyCoFVsVarBndr tv (tyCoFVsOfCo co) `unionFV` tyCoFVsOfCo kind_co) fv_cand in_scope acc
tyCoFVsOfCo (FunCo _ co1 co2)    fv_cand in_scope acc
  = (tyCoFVsOfCo co1 `unionFV` tyCoFVsOfCo co2) fv_cand in_scope acc
tyCoFVsOfCo (CoVarCo v) fv_cand in_scope acc
  = tyCoFVsOfCoVar v fv_cand in_scope acc
tyCoFVsOfCo (HoleCo h) fv_cand in_scope acc
  = tyCoFVsOfCoVar (coHoleCoVar h) fv_cand in_scope acc
    -- See Note [CoercionHoles and coercion free variables]
tyCoFVsOfCo (AxiomInstCo _ _ cos) fv_cand in_scope acc = tyCoFVsOfCos cos fv_cand in_scope acc
tyCoFVsOfCo (UnivCo p _ t1 t2) fv_cand in_scope acc
  = (tyCoFVsOfProv p `unionFV` tyCoFVsOfType t1
                     `unionFV` tyCoFVsOfType t2) fv_cand in_scope acc
tyCoFVsOfCo (SymCo co)          fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (TransCo co1 co2)   fv_cand in_scope acc = (tyCoFVsOfCo co1 `unionFV` tyCoFVsOfCo co2) fv_cand in_scope acc
tyCoFVsOfCo (NthCo _ _ co)      fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (LRCo _ co)         fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (InstCo co arg)     fv_cand in_scope acc = (tyCoFVsOfCo co `unionFV` tyCoFVsOfCo arg) fv_cand in_scope acc
tyCoFVsOfCo (KindCo co)         fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (SubCo co)          fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (AxiomRuleCo _ cs)  fv_cand in_scope acc = tyCoFVsOfCos cs fv_cand in_scope acc

tyCoFVsOfCoVar :: CoVar -> FV
tyCoFVsOfCoVar v fv_cand in_scope acc
  = (unitFV v `unionFV` tyCoFVsOfType (varType v)) fv_cand in_scope acc

tyCoFVsOfProv :: UnivCoProvenance -> FV
tyCoFVsOfProv UnsafeCoerceProv    fv_cand in_scope acc = emptyFV fv_cand in_scope acc
tyCoFVsOfProv (PhantomProv co)    fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfProv (ProofIrrelProv co) fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfProv (PluginProv _)      fv_cand in_scope acc = emptyFV fv_cand in_scope acc

tyCoFVsOfCos :: [Coercion] -> FV
tyCoFVsOfCos []       fv_cand in_scope acc = emptyFV fv_cand in_scope acc
tyCoFVsOfCos (co:cos) fv_cand in_scope acc = (tyCoFVsOfCo co `unionFV` tyCoFVsOfCos cos) fv_cand in_scope acc


------------- Extracting the CoVars of a type or coercion -----------

{-

Note [CoVarsOfX and the InterestingVarFun]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The coVarsOfType, coVarsOfTypes, coVarsOfCo, and coVarsOfCos functions are
implemented in terms of the respective FV equivalents (tyCoFVsOf...), rather
than the VarSet-based flavors (tyCoVarsOf...), despite the performance
considerations outlined in Note [Free variables of types].

This is because FV includes the InterestingVarFun, which is useful here,
because we can cleverly use it to restrict our calculations to CoVars - this
is what getCoVarSet achieves.

See Trac #14880.

-}

getCoVarSet :: FV -> CoVarSet
getCoVarSet fv = snd (fv isCoVar emptyVarSet ([], emptyVarSet))

coVarsOfType :: Type -> CoVarSet
coVarsOfType ty = getCoVarSet (tyCoFVsOfType ty)

coVarsOfTypes :: [Type] -> TyCoVarSet
coVarsOfTypes tys = getCoVarSet (tyCoFVsOfTypes tys)

coVarsOfCo :: Coercion -> CoVarSet
coVarsOfCo co = getCoVarSet (tyCoFVsOfCo co)

coVarsOfCos :: [Coercion] -> CoVarSet
coVarsOfCos cos = getCoVarSet (tyCoFVsOfCos cos)

----- Whether a covar is /Almost Devoid/ in a type or coercion ----

-- | Given a covar and a coercion, returns True if covar is almost devoid in
-- the coercion. That is, covar can only appear in Refl and GRefl.
-- See last wrinkle in Note [Unused coercion variable in ForAllCo] in Coercion
almostDevoidCoVarOfCo :: CoVar -> Coercion -> Bool
almostDevoidCoVarOfCo cv co =
  almost_devoid_co_var_of_co co cv

almost_devoid_co_var_of_co :: Coercion -> CoVar -> Bool
almost_devoid_co_var_of_co (Refl {}) _ = True   -- covar is allowed in Refl and
almost_devoid_co_var_of_co (GRefl {}) _ = True  -- GRefl, so we don't look into
                                                -- the coercions
almost_devoid_co_var_of_co (TyConAppCo _ _ cos) cv
  = almost_devoid_co_var_of_cos cos cv
almost_devoid_co_var_of_co (AppCo co arg) cv
  = almost_devoid_co_var_of_co co cv
  && almost_devoid_co_var_of_co arg cv
almost_devoid_co_var_of_co (ForAllCo v kind_co co) cv
  = almost_devoid_co_var_of_co kind_co cv
  && (v == cv || almost_devoid_co_var_of_co co cv)
almost_devoid_co_var_of_co (FunCo _ co1 co2) cv
  = almost_devoid_co_var_of_co co1 cv
  && almost_devoid_co_var_of_co co2 cv
almost_devoid_co_var_of_co (CoVarCo v) cv = v /= cv
almost_devoid_co_var_of_co (HoleCo h)  cv = (coHoleCoVar h) /= cv
almost_devoid_co_var_of_co (AxiomInstCo _ _ cos) cv
  = almost_devoid_co_var_of_cos cos cv
almost_devoid_co_var_of_co (UnivCo p _ t1 t2) cv
  = almost_devoid_co_var_of_prov p cv
  && almost_devoid_co_var_of_type t1 cv
  && almost_devoid_co_var_of_type t2 cv
almost_devoid_co_var_of_co (SymCo co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_co (TransCo co1 co2) cv
  = almost_devoid_co_var_of_co co1 cv
  && almost_devoid_co_var_of_co co2 cv
almost_devoid_co_var_of_co (NthCo _ _ co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_co (LRCo _ co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_co (InstCo co arg) cv
  = almost_devoid_co_var_of_co co cv
  && almost_devoid_co_var_of_co arg cv
almost_devoid_co_var_of_co (KindCo co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_co (SubCo co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_co (AxiomRuleCo _ cs) cv
  = almost_devoid_co_var_of_cos cs cv

almost_devoid_co_var_of_cos :: [Coercion] -> CoVar -> Bool
almost_devoid_co_var_of_cos [] _ = True
almost_devoid_co_var_of_cos (co:cos) cv
  = almost_devoid_co_var_of_co co cv
  && almost_devoid_co_var_of_cos cos cv

almost_devoid_co_var_of_prov :: UnivCoProvenance -> CoVar -> Bool
almost_devoid_co_var_of_prov (PhantomProv co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_prov (ProofIrrelProv co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_prov UnsafeCoerceProv _ = True
almost_devoid_co_var_of_prov (PluginProv _) _ = True

almost_devoid_co_var_of_type :: Type -> CoVar -> Bool
almost_devoid_co_var_of_type (TyVarTy _) _ = True
almost_devoid_co_var_of_type (TyConApp _ tys) cv
  = almost_devoid_co_var_of_types tys cv
almost_devoid_co_var_of_type (LitTy {}) _ = True
almost_devoid_co_var_of_type (AppTy fun arg) cv
  = almost_devoid_co_var_of_type fun cv
  && almost_devoid_co_var_of_type arg cv
almost_devoid_co_var_of_type (FunTy arg res) cv
  = almost_devoid_co_var_of_type arg cv
  && almost_devoid_co_var_of_type res cv
almost_devoid_co_var_of_type (ForAllTy (Bndr v _) ty) cv
  = almost_devoid_co_var_of_type (varType v) cv
  && (v == cv || almost_devoid_co_var_of_type ty cv)
almost_devoid_co_var_of_type (CastTy ty co) cv
  = almost_devoid_co_var_of_type ty cv
  && almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_type (CoercionTy co) cv
  = almost_devoid_co_var_of_co co cv

almost_devoid_co_var_of_types :: [Type] -> CoVar -> Bool
almost_devoid_co_var_of_types [] _ = True
almost_devoid_co_var_of_types (ty:tys) cv
  = almost_devoid_co_var_of_type ty cv
  && almost_devoid_co_var_of_types tys cv

------------- Injective free vars -----------------

-- | Returns the free variables of a 'Type' that are in injective positions.
-- For example, if @F@ is a non-injective type family, then:
--
-- @
-- injectiveTyVarsOf( Either c (Maybe (a, F b c)) ) = {a,c}
-- @
--
-- If @'injectiveVarsOfType' ty = itvs@, then knowing @ty@ fixes @itvs@.
-- More formally, if
-- @a@ is in @'injectiveVarsOfType' ty@
-- and  @S1(ty) ~ S2(ty)@,
-- then @S1(a)  ~ S2(a)@,
-- where @S1@ and @S2@ are arbitrary substitutions.
--
-- See @Note [When does a tycon application need an explicit kind signature?]@.
injectiveVarsOfType :: Type -> FV
injectiveVarsOfType = go
  where
    go ty                | Just ty' <- coreView ty
                         = go ty'
    go (TyVarTy v)       = unitFV v `unionFV` go (tyVarKind v)
    go (AppTy f a)       = go f `unionFV` go a
    go (FunTy ty1 ty2)   = go ty1 `unionFV` go ty2
    go (TyConApp tc tys) =
      case tyConInjectivityInfo tc of
        NotInjective  -> emptyFV
        Injective inj -> mapUnionFV go $
                         filterByList (inj ++ repeat True) tys
                         -- Oversaturated arguments to a tycon are
                         -- always injective, hence the repeat True
    go (ForAllTy tvb ty) = tyCoFVsBndr tvb $ go ty
    go LitTy{}           = emptyFV
    go (CastTy ty _)     = go ty
    go CoercionTy{}      = emptyFV

-- | Does a 'TyCon' (that is applied to some number of arguments) need to be
-- ascribed with an explicit kind signature to resolve ambiguity if rendered as
-- a source-syntax type?
-- (See @Note [When does a tycon application need an explicit kind signature?]@
-- for a full explanation of what this function checks for.)

-- Morally, this function ought to belong in TyCon.hs, not TyCoRep.hs, but
-- accomplishing this requires a fair deal of futzing aruond with .hs-boot
-- files.
tyConAppNeedsKindSig
  :: Bool  -- ^ Should specified binders count towards injective positions in
           --   the kind of the TyCon?
  -> TyCon
  -> Int   -- ^ The number of args the 'TyCon' is applied to.
  -> Bool  -- ^ Does @T t_1 ... t_n@ need a kind signature? (Where @n@ is the
           --   number of arguments)
tyConAppNeedsKindSig spec_inj_pos tc n_args
  | LT <- listLengthCmp tc_binders n_args
  = False
  | otherwise
  = let (dropped_binders, remaining_binders)
          = splitAt n_args tc_binders
        result_kind  = mkTyConKind remaining_binders tc_res_kind
        result_vars  = tyCoVarsOfType result_kind
        dropped_vars = fvVarSet $
                       mapUnionFV (injective_vars_of_binder spec_inj_pos)
                                  dropped_binders

    in not (subVarSet result_vars dropped_vars)
  where
    tc_binders  = tyConBinders tc
    tc_res_kind = tyConResKind tc

    -- Returns the variables that would be fixed by knowing a TyConBinder. See
    -- Note [When does a tycon application need an explicit kind signature?]
    -- for a more detailed explanation of what this function does.
    injective_vars_of_binder
      :: Bool -- Should specified binders count towards injective positions?
              -- (If you're using visible kind applications, then you want True
              -- here.)
      -> TyConBinder -> FV
    injective_vars_of_binder spec_inj_pos (Bndr tv vis) =
      case vis of
        AnonTCB -> injectiveVarsOfType (varType tv)
        NamedTCB argf
          |     (argf == Required)
             || (spec_inj_pos && (argf == Specified))
          -> unitFV tv `unionFV` injectiveVarsOfType (varType tv)
          |  otherwise
          -> emptyFV

{-
Note [When does a tycon application need an explicit kind signature?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are a couple of places in GHC where we convert Core Types into forms that
more closely resemble user-written syntax. These include:

1. Template Haskell Type reification (see, for instance, TcSplice.reify_tc_app)
2. Converting Types to LHsTypes (in HsUtils.typeToLHsType, or in Haddock)

This conversion presents a challenge: how do we ensure that the resulting type
has enough kind information so as not to be ambiguous? To better motivate this
question, consider the following Core type:

  -- Foo :: Type -> Type
  type Foo = Proxy Type

There is nothing ambiguous about the RHS of Foo in Core. But if we were to,
say, reify it into a TH Type, then it's tempting to just drop the invisible
Type argument and simply return `Proxy`. But now we've lost crucial kind
information: we don't know if we're dealing with `Proxy Type` or `Proxy Bool`
or `Proxy Int` or something else! We've inadvertently introduced ambiguity.

Unlike in other situations in GHC, we can't just turn on
-fprint-explicit-kinds, as we need to produce something which has the same
structure as a source-syntax type. Moreover, we can't rely on visible kind
application, since the first kind argument to Proxy is inferred, not specified.
Our solution is to annotate certain tycons with their kinds whenever they
appear in applied form in order to resolve the ambiguity. For instance, we
would reify the RHS of Foo like so:

  type Foo = (Proxy :: Type -> Type)

We need to devise an algorithm that determines precisely which tycons need
these explicit kind signatures. We certainly don't want to annotate _every_
tycon with a kind signature, or else we might end up with horribly bloated
types like the following:

  (Either :: Type -> Type -> Type) (Int :: Type) (Char :: Type)

We only want to annotate tycons that absolutely require kind signatures in
order to resolve some sort of ambiguity, and nothing more.

Suppose we have a tycon application (T ty_1 ... ty_n). Why might this type
require a kind signature? It might require it when we need to fill in any of
T's omitted arguments. By "omitted argument", we mean one that is dropped when
reifying ty_1 ... ty_n. Sometimes, the omitted arguments are inferred and
specified arguments (e.g., TH reification in TcSplice), and sometimes the
omitted arguments are only the inferred ones (e.g., in HsUtils.typeToLHsType,
which reifies specified arguments through visible kind application).
Regardless, the key idea is that _some_ arguments are going to be omitted after
reification, and the only mechanism we have at our disposal for filling them in
is through explicit kind signatures.

What do we mean by "fill in"? Let's consider this small example:

  T :: forall {k}. Type -> (k -> Type) -> k

Moreover, we have this application of T:

  T @{j} Int aty

When we reify this type, we omit the inferred argument @{j}. Is it fixed by the
other (non-inferred) arguments? Yes! If we know the kind of (aty :: blah), then
we'll generate an equality constraint (kappa -> Type) and, assuming we can
solve it, that will fix `kappa`. (Here, `kappa` is the unification variable
that we instantiate `k` with.)

Therefore, for any application of a tycon T to some arguments, the Question We
Must Answer is:

* Given the first n arguments of T, do the kinds of the non-omitted arguments
  fill in the omitted arguments?

(This is still a bit hand-wavey, but we'll refine this question incrementally
as we explain more of the machinery underlying this process.)

Answering this question is precisely the role that the `injectiveVarsOfType`
and `injective_vars_of_binder` functions exist to serve. If an omitted argument
`a` appears in the set returned by `injectiveVarsOfType ty`, then knowing
`ty` determines (i.e., fills in) `a`. (More on `injective_vars_of_binder` in a
bit.)

More formally, if
`a` is in `injectiveVarsOfType ty`
and  S1(ty) ~ S2(ty),
then S1(a)  ~ S2(a),
where S1 and S2 are arbitrary substitutions.

For example, is `F` is a non-injective type family, then

  injectiveVarsOfType(Either c (Maybe (a, F b c))) = {a, c}

Now that we know what this function does, here is a second attempt at the
Question We Must Answer:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. Do the injective
  variables of these binders fill in the remainder of T's kind?

Alright, we're getting closer. Next, we need to clarify what the injective
variables of a tycon binder are. This the role that the
`injective_vars_of_binder` function serves. Here is what this function does for
each form of tycon binder:

* Anonymous binders are injective positions. For example, in the promoted data
  constructor '(:):

    '(:) :: forall a. a -> [a] -> [a]

  The second and third tyvar binders (of kinds `a` and `[a]`) are both
  anonymous, so if we had '(:) 'True '[], then the kinds of 'True and
  '[] would contribute to the kind of '(:) 'True '[]. Therefore,
  injective_vars_of_binder(_ :: a) = injectiveVarsOfType(a) = {a}.
  (Similarly, injective_vars_of_binder(_ :: [a]) = {a}.)
* Named binders:
  - Inferred binders are never injective positions. For example, in this data
    type:

      data Proxy a
      Proxy :: forall {k}. k -> Type

    If we had Proxy 'True, then the kind of 'True would not contribute to the
    kind of Proxy 'True. Therefore,
    injective_vars_of_binder(forall {k}. ...) = {}.
  - Required binders are injective positions. For example, in this data type:

      data Wurble k (a :: k) :: k
      Wurble :: forall k -> k -> k

  The first tyvar binder (of kind `forall k`) has required visibility, so if
  we had Wurble (Maybe a) Nothing, then the kind of Maybe a would
  contribute to the kind of Wurble (Maybe a) Nothing. Hence,
  injective_vars_of_binder(forall a -> ...) = {a}.
  - Specified binders /might/ be injective positions, depending on how you
    approach things. Continuing the '(:) example:

      '(:) :: forall a. a -> [a] -> [a]

    Normally, the (forall a. ...) tyvar binder wouldn't contribute to the kind
    of '(:) 'True '[], since it's not explicitly instantiated by the user. But
    if visible kind application is enabled, then this is possible, since the
    user can write '(:) @Bool 'True '[]. (In that case,
    injective_vars_of_binder(forall a. ...) = {a}.)

    There are some situations where using visible kind application is appropriate
    (e.g., HsUtils.typeToLHsType) and others where it is not (e.g., TH
    reification), so the `injective_vars_of_binder` function is parametrized by
    a Bool which decides if specified binders should be counted towards
    injective positions or not.

Now that we've defined injective_vars_of_binder, we can refine the Question We
Must Answer once more:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. For each such binder
  b_i, take the union of all injective_vars_of_binder(b_i). Is this set a
  superset of the free variables of the remainder of T's kind?

If the answer to this question is "no", then (T ty_1 ... ty_n) needs an
explicit kind signature, since T's kind has kind variables leftover that
aren't fixed by the non-omitted arguments.

One last sticking point: what does "the remainder of T's kind" mean? You might
be tempted to think that it corresponds to all of the arguments in the kind of
T that would normally be instantiated by omitted arguments. But this isn't
quite right, strictly speaking. Consider the following (silly) example:

  S :: forall {k}. Type -> Type

And suppose we have this application of S:

  S Int Bool

The Int argument would be omitted, and
injective_vars_of_binder(_ :: Type) = {}. This is not a superset of {k}, which
might suggest that (S Bool) needs an explicit kind signature. But
(S Bool :: Type) doesn't actually fix `k`! This is because the kind signature
only affects the /result/ of the application, not all of the individual
arguments. So adding a kind signature here won't make a difference. Therefore,
the fourth (and final) iteration of the Question We Must Answer is:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. For each such binder
  b_i, take the union of all injective_vars_of_binder(b_i). Is this set a
  superset of the free variables of the kind of (T ty_1 ... ty_n)?

Phew, that was a lot of work!

How can be sure that this is correct? That is, how can we be sure that in the
event that we leave off a kind annotation, that one could infer the kind of the
tycon application from its arguments? It's essentially a proof by induction: if
we can infer the kinds of every subtree of a type, then the whole tycon
application will have an inferrable kind--unless, of course, the remainder of
the tycon application's kind has uninstantiated kind variables.

What happens if T is oversaturated? That is, if T's kind has fewer than n
arguments, in the case that the concrete application instantiates a result
kind variable with an arrow kind? If we run out of arguments, we do not attach
a kind annotation. This should be a rare case, indeed. Here is an example:

   data T1 :: k1 -> k2 -> *
   data T2 :: k1 -> k2 -> *

   type family G (a :: k) :: k
   type instance G T1 = T2

   type instance F Char = (G T1 Bool :: (* -> *) -> *)   -- F from above

Here G's kind is (forall k. k -> k), and the desugared RHS of that last
instance of F is (G (* -> (* -> *) -> *) (T1 * (* -> *)) Bool). According to
the algorithm above, there are 3 arguments to G so we should peel off 3
arguments in G's kind. But G's kind has only two arguments. This is the
rare special case, and we choose not to annotate the application of G with
a kind signature. After all, we needn't do this, since that instance would
be reified as:

   type instance F Char = G (T1 :: * -> (* -> *) -> *) Bool

So the kind of G isn't ambiguous anymore due to the explicit kind annotation
on its argument. See #8953 and test th/T8953.
-}

------------- No free vars -----------------

-- | Returns True if this type has no free variables. Should be the same as
-- isEmptyVarSet . tyCoVarsOfType, but faster in the non-forall case.
noFreeVarsOfType :: Type -> Bool
noFreeVarsOfType (TyVarTy _)      = False
noFreeVarsOfType (AppTy t1 t2)    = noFreeVarsOfType t1 && noFreeVarsOfType t2
noFreeVarsOfType (TyConApp _ tys) = all noFreeVarsOfType tys
noFreeVarsOfType ty@(ForAllTy {}) = isEmptyVarSet (tyCoVarsOfType ty)
noFreeVarsOfType (FunTy t1 t2)    = noFreeVarsOfType t1 && noFreeVarsOfType t2
noFreeVarsOfType (LitTy _)        = True
noFreeVarsOfType (CastTy ty co)   = noFreeVarsOfType ty && noFreeVarsOfCo co
noFreeVarsOfType (CoercionTy co)  = noFreeVarsOfCo co

noFreeVarsOfMCo :: MCoercion -> Bool
noFreeVarsOfMCo MRefl    = True
noFreeVarsOfMCo (MCo co) = noFreeVarsOfCo co

noFreeVarsOfTypes :: [Type] -> Bool
noFreeVarsOfTypes = all noFreeVarsOfType

-- | Returns True if this coercion has no free variables. Should be the same as
-- isEmptyVarSet . tyCoVarsOfCo, but faster in the non-forall case.
noFreeVarsOfCo :: Coercion -> Bool
noFreeVarsOfCo (Refl ty)              = noFreeVarsOfType ty
noFreeVarsOfCo (GRefl _ ty co)        = noFreeVarsOfType ty && noFreeVarsOfMCo co
noFreeVarsOfCo (TyConAppCo _ _ args)  = all noFreeVarsOfCo args
noFreeVarsOfCo (AppCo c1 c2)          = noFreeVarsOfCo c1 && noFreeVarsOfCo c2
noFreeVarsOfCo co@(ForAllCo {})       = isEmptyVarSet (tyCoVarsOfCo co)
noFreeVarsOfCo (FunCo _ c1 c2)        = noFreeVarsOfCo c1 && noFreeVarsOfCo c2
noFreeVarsOfCo (CoVarCo _)            = False
noFreeVarsOfCo (HoleCo {})            = True    -- I'm unsure; probably never happens
noFreeVarsOfCo (AxiomInstCo _ _ args) = all noFreeVarsOfCo args
noFreeVarsOfCo (UnivCo p _ t1 t2)     = noFreeVarsOfProv p &&
                                        noFreeVarsOfType t1 &&
                                        noFreeVarsOfType t2
noFreeVarsOfCo (SymCo co)             = noFreeVarsOfCo co
noFreeVarsOfCo (TransCo co1 co2)      = noFreeVarsOfCo co1 && noFreeVarsOfCo co2
noFreeVarsOfCo (NthCo _ _ co)         = noFreeVarsOfCo co
noFreeVarsOfCo (LRCo _ co)            = noFreeVarsOfCo co
noFreeVarsOfCo (InstCo co1 co2)       = noFreeVarsOfCo co1 && noFreeVarsOfCo co2
noFreeVarsOfCo (KindCo co)            = noFreeVarsOfCo co
noFreeVarsOfCo (SubCo co)             = noFreeVarsOfCo co
noFreeVarsOfCo (AxiomRuleCo _ cs)     = all noFreeVarsOfCo cs

-- | Returns True if this UnivCoProv has no free variables. Should be the same as
-- isEmptyVarSet . tyCoVarsOfProv, but faster in the non-forall case.
noFreeVarsOfProv :: UnivCoProvenance -> Bool
noFreeVarsOfProv UnsafeCoerceProv    = True
noFreeVarsOfProv (PhantomProv co)    = noFreeVarsOfCo co
noFreeVarsOfProv (ProofIrrelProv co) = noFreeVarsOfCo co
noFreeVarsOfProv (PluginProv {})     = True

{-
%************************************************************************
%*                                                                      *
                        Substitutions
      Data type defined here to avoid unnecessary mutual recursion
%*                                                                      *
%************************************************************************
-}

-- | Type & coercion substitution
--
-- #tcvsubst_invariant#
-- The following invariants must hold of a 'TCvSubst':
--
-- 1. The in-scope set is needed /only/ to
-- guide the generation of fresh uniques
--
-- 2. In particular, the /kind/ of the type variables in
-- the in-scope set is not relevant
--
-- 3. The substitution is only applied ONCE! This is because
-- in general such application will not reach a fixed point.
data TCvSubst
  = TCvSubst InScopeSet -- The in-scope type and kind variables
             TvSubstEnv -- Substitutes both type and kind variables
             CvSubstEnv -- Substitutes coercion variables
        -- See Note [Substitutions apply only once]
        -- and Note [Extending the TvSubstEnv]
        -- and Note [Substituting types and coercions]
        -- and Note [The substitution invariant]

-- | A substitution of 'Type's for 'TyVar's
--                 and 'Kind's for 'KindVar's
type TvSubstEnv = TyVarEnv Type
  -- NB: A TvSubstEnv is used
  --   both inside a TCvSubst (with the apply-once invariant
  --        discussed in Note [Substitutions apply only once],
  --   and  also independently in the middle of matching,
  --        and unification (see Types.Unify).
  -- So you have to look at the context to know if it's idempotent or
  -- apply-once or whatever

-- | A substitution of 'Coercion's for 'CoVar's
type CvSubstEnv = CoVarEnv Coercion

{- Note [The substitution invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When calling (substTy subst ty) it should be the case that
the in-scope set in the substitution is a superset of both:

  (SIa) The free vars of the range of the substitution
  (SIb) The free vars of ty minus the domain of the substitution

The same rules apply to other substitutions (notably CoreSubst.Subst)

* Reason for (SIa). Consider
      substTy [a :-> Maybe b] (forall b. b->a)
  we must rename the forall b, to get
      forall b2. b2 -> Maybe b
  Making 'b' part of the in-scope set forces this renaming to
  take place.

* Reason for (SIb). Consider
     substTy [a :-> Maybe b] (forall b. (a,b,x))
  Then if we use the in-scope set {b}, satisfying (SIa), there is
  a danger we will rename the forall'd variable to 'x' by mistake,
  getting this:
      forall x. (Maybe b, x, x)
  Breaking (SIb) caused the bug from #11371.

Note: if the free vars of the range of the substitution are freshly created,
then the problems of (SIa) can't happen, and so it would be sound to
ignore (SIa).

Note [Substitutions apply only once]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use TCvSubsts to instantiate things, and we might instantiate
        forall a b. ty
with the types
        [a, b], or [b, a].
So the substitution might go [a->b, b->a].  A similar situation arises in Core
when we find a beta redex like
        (/\ a /\ b -> e) b a
Then we also end up with a substitution that permutes type variables. Other
variations happen to; for example [a -> (a, b)].

        ********************************************************
        *** So a substitution must be applied precisely once ***
        ********************************************************

A TCvSubst is not idempotent, but, unlike the non-idempotent substitution
we use during unifications, it must not be repeatedly applied.

Note [Extending the TvSubstEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #tcvsubst_invariant# for the invariants that must hold.

This invariant allows a short-cut when the subst envs are empty:
if the TvSubstEnv and CvSubstEnv are empty --- i.e. (isEmptyTCvSubst subst)
holds --- then (substTy subst ty) does nothing.

For example, consider:
        (/\a. /\b:(a~Int). ...b..) Int
We substitute Int for 'a'.  The Unique of 'b' does not change, but
nevertheless we add 'b' to the TvSubstEnv, because b's kind does change

This invariant has several crucial consequences:

* In substVarBndr, we need extend the TvSubstEnv
        - if the unique has changed
        - or if the kind has changed

* In substTyVar, we do not need to consult the in-scope set;
  the TvSubstEnv is enough

* In substTy, substTheta, we can short-circuit when the TvSubstEnv is empty

Note [Substituting types and coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Types and coercions are mutually recursive, and either may have variables
"belonging" to the other. Thus, every time we wish to substitute in a
type, we may also need to substitute in a coercion, and vice versa.
However, the constructor used to create type variables is distinct from
that of coercion variables, so we carry two VarEnvs in a TCvSubst. Note
that it would be possible to use the CoercionTy constructor to combine
these environments, but that seems like a false economy.

Note that the TvSubstEnv should *never* map a CoVar (built with the Id
constructor) and the CvSubstEnv should *never* map a TyVar. Furthermore,
the range of the TvSubstEnv should *never* include a type headed with
CoercionTy.
-}

emptyTvSubstEnv :: TvSubstEnv
emptyTvSubstEnv = emptyVarEnv

emptyCvSubstEnv :: CvSubstEnv
emptyCvSubstEnv = emptyVarEnv

composeTCvSubstEnv :: InScopeSet
                   -> (TvSubstEnv, CvSubstEnv)
                   -> (TvSubstEnv, CvSubstEnv)
                   -> (TvSubstEnv, CvSubstEnv)
-- ^ @(compose env1 env2)(x)@ is @env1(env2(x))@; i.e. apply @env2@ then @env1@.
-- It assumes that both are idempotent.
-- Typically, @env1@ is the refinement to a base substitution @env2@
composeTCvSubstEnv in_scope (tenv1, cenv1) (tenv2, cenv2)
  = ( tenv1 `plusVarEnv` mapVarEnv (substTy subst1) tenv2
    , cenv1 `plusVarEnv` mapVarEnv (substCo subst1) cenv2 )
        -- First apply env1 to the range of env2
        -- Then combine the two, making sure that env1 loses if
        -- both bind the same variable; that's why env1 is the
        --  *left* argument to plusVarEnv, because the right arg wins
  where
    subst1 = TCvSubst in_scope tenv1 cenv1

-- | Composes two substitutions, applying the second one provided first,
-- like in function composition.
composeTCvSubst :: TCvSubst -> TCvSubst -> TCvSubst
composeTCvSubst (TCvSubst is1 tenv1 cenv1) (TCvSubst is2 tenv2 cenv2)
  = TCvSubst is3 tenv3 cenv3
  where
    is3 = is1 `unionInScope` is2
    (tenv3, cenv3) = composeTCvSubstEnv is3 (tenv1, cenv1) (tenv2, cenv2)

emptyTCvSubst :: TCvSubst
emptyTCvSubst = TCvSubst emptyInScopeSet emptyTvSubstEnv emptyCvSubstEnv

mkEmptyTCvSubst :: InScopeSet -> TCvSubst
mkEmptyTCvSubst is = TCvSubst is emptyTvSubstEnv emptyCvSubstEnv

isEmptyTCvSubst :: TCvSubst -> Bool
         -- See Note [Extending the TvSubstEnv]
isEmptyTCvSubst (TCvSubst _ tenv cenv) = isEmptyVarEnv tenv && isEmptyVarEnv cenv

mkTCvSubst :: InScopeSet -> (TvSubstEnv, CvSubstEnv) -> TCvSubst
mkTCvSubst in_scope (tenv, cenv) = TCvSubst in_scope tenv cenv

mkTvSubst :: InScopeSet -> TvSubstEnv -> TCvSubst
-- ^ Make a TCvSubst with specified tyvar subst and empty covar subst
mkTvSubst in_scope tenv = TCvSubst in_scope tenv emptyCvSubstEnv

mkCvSubst :: InScopeSet -> CvSubstEnv -> TCvSubst
-- ^ Make a TCvSubst with specified covar subst and empty tyvar subst
mkCvSubst in_scope cenv = TCvSubst in_scope emptyTvSubstEnv cenv

getTvSubstEnv :: TCvSubst -> TvSubstEnv
getTvSubstEnv (TCvSubst _ env _) = env

getCvSubstEnv :: TCvSubst -> CvSubstEnv
getCvSubstEnv (TCvSubst _ _ env) = env

getTCvInScope :: TCvSubst -> InScopeSet
getTCvInScope (TCvSubst in_scope _ _) = in_scope

-- | Returns the free variables of the types in the range of a substitution as
-- a non-deterministic set.
getTCvSubstRangeFVs :: TCvSubst -> VarSet
getTCvSubstRangeFVs (TCvSubst _ tenv cenv)
    = unionVarSet tenvFVs cenvFVs
  where
    tenvFVs = tyCoVarsOfTypesSet tenv
    cenvFVs = tyCoVarsOfCosSet cenv

isInScope :: Var -> TCvSubst -> Bool
isInScope v (TCvSubst in_scope _ _) = v `elemInScopeSet` in_scope

notElemTCvSubst :: Var -> TCvSubst -> Bool
notElemTCvSubst v (TCvSubst _ tenv cenv)
  | isTyVar v
  = not (v `elemVarEnv` tenv)
  | otherwise
  = not (v `elemVarEnv` cenv)

setTvSubstEnv :: TCvSubst -> TvSubstEnv -> TCvSubst
setTvSubstEnv (TCvSubst in_scope _ cenv) tenv = TCvSubst in_scope tenv cenv

setCvSubstEnv :: TCvSubst -> CvSubstEnv -> TCvSubst
setCvSubstEnv (TCvSubst in_scope tenv _) cenv = TCvSubst in_scope tenv cenv

zapTCvSubst :: TCvSubst -> TCvSubst
zapTCvSubst (TCvSubst in_scope _ _) = TCvSubst in_scope emptyVarEnv emptyVarEnv

extendTCvInScope :: TCvSubst -> Var -> TCvSubst
extendTCvInScope (TCvSubst in_scope tenv cenv) var
  = TCvSubst (extendInScopeSet in_scope var) tenv cenv

extendTCvInScopeList :: TCvSubst -> [Var] -> TCvSubst
extendTCvInScopeList (TCvSubst in_scope tenv cenv) vars
  = TCvSubst (extendInScopeSetList in_scope vars) tenv cenv

extendTCvInScopeSet :: TCvSubst -> VarSet -> TCvSubst
extendTCvInScopeSet (TCvSubst in_scope tenv cenv) vars
  = TCvSubst (extendInScopeSetSet in_scope vars) tenv cenv

extendTCvSubst :: TCvSubst -> TyCoVar -> Type -> TCvSubst
extendTCvSubst subst v ty
  | isTyVar v
  = extendTvSubst subst v ty
  | CoercionTy co <- ty
  = extendCvSubst subst v co
  | otherwise
  = pprPanic "extendTCvSubst" (ppr v <+> text "|->" <+> ppr ty)

extendTCvSubstWithClone :: TCvSubst -> TyCoVar -> TyCoVar -> TCvSubst
extendTCvSubstWithClone subst tcv
  | isTyVar tcv = extendTvSubstWithClone subst tcv
  | otherwise   = extendCvSubstWithClone subst tcv

extendTvSubst :: TCvSubst -> TyVar -> Type -> TCvSubst
extendTvSubst (TCvSubst in_scope tenv cenv) tv ty
  = TCvSubst in_scope (extendVarEnv tenv tv ty) cenv

extendTvSubstBinderAndInScope :: TCvSubst -> TyCoBinder -> Type -> TCvSubst
extendTvSubstBinderAndInScope subst (Named (Bndr v _)) ty
  = ASSERT( isTyVar v )
    extendTvSubstAndInScope subst v ty
extendTvSubstBinderAndInScope subst (Anon _)     _
  = subst

extendTvSubstWithClone :: TCvSubst -> TyVar -> TyVar -> TCvSubst
-- Adds a new tv -> tv mapping, /and/ extends the in-scope set
extendTvSubstWithClone (TCvSubst in_scope tenv cenv) tv tv'
  = TCvSubst (extendInScopeSetSet in_scope new_in_scope)
             (extendVarEnv tenv tv (mkTyVarTy tv'))
             cenv
  where
    new_in_scope = tyCoVarsOfType (tyVarKind tv') `extendVarSet` tv'

extendCvSubst :: TCvSubst -> CoVar -> Coercion -> TCvSubst
extendCvSubst (TCvSubst in_scope tenv cenv) v co
  = TCvSubst in_scope tenv (extendVarEnv cenv v co)

extendCvSubstWithClone :: TCvSubst -> CoVar -> CoVar -> TCvSubst
extendCvSubstWithClone (TCvSubst in_scope tenv cenv) cv cv'
  = TCvSubst (extendInScopeSetSet in_scope new_in_scope)
             tenv
             (extendVarEnv cenv cv (mkCoVarCo cv'))
  where
    new_in_scope = tyCoVarsOfType (varType cv') `extendVarSet` cv'

extendTvSubstAndInScope :: TCvSubst -> TyVar -> Type -> TCvSubst
-- Also extends the in-scope set
extendTvSubstAndInScope (TCvSubst in_scope tenv cenv) tv ty
  = TCvSubst (in_scope `extendInScopeSetSet` tyCoVarsOfType ty)
             (extendVarEnv tenv tv ty)
             cenv

extendTvSubstList :: TCvSubst -> [Var] -> [Type] -> TCvSubst
extendTvSubstList subst tvs tys
  = foldl2 extendTvSubst subst tvs tys

extendTCvSubstList :: TCvSubst -> [Var] -> [Type] -> TCvSubst
extendTCvSubstList subst tvs tys
  = foldl2 extendTCvSubst subst tvs tys

unionTCvSubst :: TCvSubst -> TCvSubst -> TCvSubst
-- Works when the ranges are disjoint
unionTCvSubst (TCvSubst in_scope1 tenv1 cenv1) (TCvSubst in_scope2 tenv2 cenv2)
  = ASSERT( not (tenv1 `intersectsVarEnv` tenv2)
         && not (cenv1 `intersectsVarEnv` cenv2) )
    TCvSubst (in_scope1 `unionInScope` in_scope2)
             (tenv1     `plusVarEnv`   tenv2)
             (cenv1     `plusVarEnv`   cenv2)

-- mkTvSubstPrs and zipTvSubst generate the in-scope set from
-- the types given; but it's just a thunk so with a bit of luck
-- it'll never be evaluated

-- | Generates the in-scope set for the 'TCvSubst' from the types in the incoming
-- environment. No CoVars, please!
zipTvSubst :: HasDebugCallStack => [TyVar] -> [Type] -> TCvSubst
zipTvSubst tvs tys
  = mkTvSubst (mkInScopeSet (tyCoVarsOfTypes tys)) tenv
  where
    tenv = zipTyEnv tvs tys

-- | Generates the in-scope set for the 'TCvSubst' from the types in the incoming
-- environment.  No TyVars, please!
zipCvSubst :: HasDebugCallStack => [CoVar] -> [Coercion] -> TCvSubst
zipCvSubst cvs cos
  = TCvSubst (mkInScopeSet (tyCoVarsOfCos cos)) emptyTvSubstEnv cenv
  where
    cenv = zipCoEnv cvs cos

zipTCvSubst :: HasDebugCallStack => [TyCoVar] -> [Type] -> TCvSubst
zipTCvSubst tcvs tys
  = zip_tcvsubst tcvs tys (mkEmptyTCvSubst $ mkInScopeSet (tyCoVarsOfTypes tys))
  where zip_tcvsubst :: [TyCoVar] -> [Type] -> TCvSubst -> TCvSubst
        zip_tcvsubst (tv:tvs) (ty:tys) subst
          = zip_tcvsubst tvs tys (extendTCvSubst subst tv ty)
        zip_tcvsubst [] [] subst = subst -- empty case
        zip_tcvsubst _  _  _     = pprPanic "zipTCvSubst: length mismatch"
                                            (ppr tcvs <+> ppr tys)

-- | Generates the in-scope set for the 'TCvSubst' from the types in the
-- incoming environment. No CoVars, please!
mkTvSubstPrs :: [(TyVar, Type)] -> TCvSubst
mkTvSubstPrs prs =
    ASSERT2( onlyTyVarsAndNoCoercionTy, text "prs" <+> ppr prs )
    mkTvSubst in_scope tenv
  where tenv = mkVarEnv prs
        in_scope = mkInScopeSet $ tyCoVarsOfTypes $ map snd prs
        onlyTyVarsAndNoCoercionTy =
          and [ isTyVar tv && not (isCoercionTy ty)
              | (tv, ty) <- prs ]

zipTyEnv :: HasDebugCallStack => [TyVar] -> [Type] -> TvSubstEnv
zipTyEnv tyvars tys
  | debugIsOn
  , not (all isTyVar tyvars)
  = pprPanic "zipTyEnv" (ppr tyvars <+> ppr tys)
  | otherwise
  = ASSERT( all (not . isCoercionTy) tys )
    mkVarEnv (zipEqual "zipTyEnv" tyvars tys)
        -- There used to be a special case for when
        --      ty == TyVarTy tv
        -- (a not-uncommon case) in which case the substitution was dropped.
        -- But the type-tidier changes the print-name of a type variable without
        -- changing the unique, and that led to a bug.   Why?  Pre-tidying, we had
        -- a type {Foo t}, where Foo is a one-method class.  So Foo is really a newtype.
        -- And it happened that t was the type variable of the class.  Post-tiding,
        -- it got turned into {Foo t2}.  The ext-core printer expanded this using
        -- sourceTypeRep, but that said "Oh, t == t2" because they have the same unique,
        -- and so generated a rep type mentioning t not t2.
        --
        -- Simplest fix is to nuke the "optimisation"

zipCoEnv :: HasDebugCallStack => [CoVar] -> [Coercion] -> CvSubstEnv
zipCoEnv cvs cos
  | debugIsOn
  , not (all isCoVar cvs)
  = pprPanic "zipCoEnv" (ppr cvs <+> ppr cos)
  | otherwise
  = mkVarEnv (zipEqual "zipCoEnv" cvs cos)

instance Outputable TCvSubst where
  ppr (TCvSubst ins tenv cenv)
    = brackets $ sep[ text "TCvSubst",
                      nest 2 (text "In scope:" <+> ppr ins),
                      nest 2 (text "Type env:" <+> ppr tenv),
                      nest 2 (text "Co env:" <+> ppr cenv) ]

{-
%************************************************************************
%*                                                                      *
                Performing type or kind substitutions
%*                                                                      *
%************************************************************************

Note [Sym and ForAllCo]
~~~~~~~~~~~~~~~~~~~~~~~
In OptCoercion, we try to push "sym" out to the leaves of a coercion. But,
how do we push sym into a ForAllCo? It's a little ugly.

Here is the typing rule:

h : k1 ~# k2
(tv : k1) |- g : ty1 ~# ty2
----------------------------
ForAllCo tv h g : (ForAllTy (tv : k1) ty1) ~#
                  (ForAllTy (tv : k2) (ty2[tv |-> tv |> sym h]))

Here is what we want:

ForAllCo tv h' g' : (ForAllTy (tv : k2) (ty2[tv |-> tv |> sym h])) ~#
                    (ForAllTy (tv : k1) ty1)


Because the kinds of the type variables to the right of the colon are the kinds
coerced by h', we know (h' : k2 ~# k1). Thus, (h' = sym h).

Now, we can rewrite ty1 to be (ty1[tv |-> tv |> sym h' |> h']). We thus want

ForAllCo tv h' g' :
  (ForAllTy (tv : k2) (ty2[tv |-> tv |> h'])) ~#
  (ForAllTy (tv : k1) (ty1[tv |-> tv |> h'][tv |-> tv |> sym h']))

We thus see that we want

g' : ty2[tv |-> tv |> h'] ~# ty1[tv |-> tv |> h']

and thus g' = sym (g[tv |-> tv |> h']).

Putting it all together, we get this:

sym (ForAllCo tv h g)
==>
ForAllCo tv (sym h) (sym g[tv |-> tv |> sym h])

Note [Substituting in a coercion hole]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It seems highly suspicious to be substituting in a coercion that still
has coercion holes. Yet, this can happen in a situation like this:

  f :: forall k. k :~: Type -> ()
  f Refl = let x :: forall (a :: k). [a] -> ...
               x = ...

When we check x's type signature, we require that k ~ Type. We indeed
know this due to the Refl pattern match, but the eager unifier can't
make use of givens. So, when we're done looking at x's type, a coercion
hole will remain. Then, when we're checking x's definition, we skolemise
x's type (in order to, e.g., bring the scoped type variable `a` into scope).
This requires performing a substitution for the fresh skolem variables.

This subsitution needs to affect the kind of the coercion hole, too --
otherwise, the kind will have an out-of-scope variable in it. More problematically
in practice (we won't actually notice the out-of-scope variable ever), skolems
in the kind might have too high a level, triggering a failure to uphold the
invariant that no free variables in a type have a higher level than the
ambient level in the type checker. In the event of having free variables in the
hole's kind, I'm pretty sure we'll always have an erroneous program, so we
don't need to worry what will happen when the hole gets filled in. After all,
a hole relating a locally-bound type variable will be unable to be solved. This
is why it's OK not to look through the IORef of a coercion hole during
substitution.

-}

-- | Type substitution, see 'zipTvSubst'
substTyWith :: HasCallStack => [TyVar] -> [Type] -> Type -> Type
-- Works only if the domain of the substitution is a
-- superset of the type being substituted into
substTyWith tvs tys = {-#SCC "substTyWith" #-}
                      ASSERT( tvs `equalLength` tys )
                      substTy (zipTvSubst tvs tys)

-- | Type substitution, see 'zipTvSubst'. Disables sanity checks.
-- The problems that the sanity checks in substTy catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTyUnchecked to
-- substTy and remove this function. Please don't use in new code.
substTyWithUnchecked :: [TyVar] -> [Type] -> Type -> Type
substTyWithUnchecked tvs tys
  = ASSERT( tvs `equalLength` tys )
    substTyUnchecked (zipTvSubst tvs tys)

-- | Substitute tyvars within a type using a known 'InScopeSet'.
-- Pre-condition: the 'in_scope' set should satisfy Note [The substitution
-- invariant]; specifically it should include the free vars of 'tys',
-- and of 'ty' minus the domain of the subst.
substTyWithInScope :: InScopeSet -> [TyVar] -> [Type] -> Type -> Type
substTyWithInScope in_scope tvs tys ty =
  ASSERT( tvs `equalLength` tys )
  substTy (mkTvSubst in_scope tenv) ty
  where tenv = zipTyEnv tvs tys

-- | Coercion substitution, see 'zipTvSubst'
substCoWith :: HasCallStack => [TyVar] -> [Type] -> Coercion -> Coercion
substCoWith tvs tys = ASSERT( tvs `equalLength` tys )
                      substCo (zipTvSubst tvs tys)

-- | Coercion substitution, see 'zipTvSubst'. Disables sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substCoWithUnchecked :: [TyVar] -> [Type] -> Coercion -> Coercion
substCoWithUnchecked tvs tys
  = ASSERT( tvs `equalLength` tys )
    substCoUnchecked (zipTvSubst tvs tys)



-- | Substitute covars within a type
substTyWithCoVars :: [CoVar] -> [Coercion] -> Type -> Type
substTyWithCoVars cvs cos = substTy (zipCvSubst cvs cos)

-- | Type substitution, see 'zipTvSubst'
substTysWith :: [TyVar] -> [Type] -> [Type] -> [Type]
substTysWith tvs tys = ASSERT( tvs `equalLength` tys )
                       substTys (zipTvSubst tvs tys)

-- | Type substitution, see 'zipTvSubst'
substTysWithCoVars :: [CoVar] -> [Coercion] -> [Type] -> [Type]
substTysWithCoVars cvs cos = ASSERT( cvs `equalLength` cos )
                             substTys (zipCvSubst cvs cos)

-- | Substitute within a 'Type' after adding the free variables of the type
-- to the in-scope set. This is useful for the case when the free variables
-- aren't already in the in-scope set or easily available.
-- See also Note [The substitution invariant].
substTyAddInScope :: TCvSubst -> Type -> Type
substTyAddInScope subst ty =
  substTy (extendTCvInScopeSet subst $ tyCoVarsOfType ty) ty

-- | When calling `substTy` it should be the case that the in-scope set in
-- the substitution is a superset of the free vars of the range of the
-- substitution.
-- See also Note [The substitution invariant].
isValidTCvSubst :: TCvSubst -> Bool
isValidTCvSubst (TCvSubst in_scope tenv cenv) =
  (tenvFVs `varSetInScope` in_scope) &&
  (cenvFVs `varSetInScope` in_scope)
  where
  tenvFVs = tyCoVarsOfTypesSet tenv
  cenvFVs = tyCoVarsOfCosSet cenv

-- | This checks if the substitution satisfies the invariant from
-- Note [The substitution invariant].
checkValidSubst :: HasCallStack => TCvSubst -> [Type] -> [Coercion] -> a -> a
checkValidSubst subst@(TCvSubst in_scope tenv cenv) tys cos a
-- TODO (RAE): Change back to ASSERT
  = WARN( not (isValidTCvSubst subst),
             text "in_scope" <+> ppr in_scope $$
             text "tenv" <+> ppr tenv $$
             text "tenvFVs" <+> ppr (tyCoVarsOfTypesSet tenv) $$
             text "cenv" <+> ppr cenv $$
             text "cenvFVs" <+> ppr (tyCoVarsOfCosSet cenv) $$
             text "tys" <+> ppr tys $$
             text "cos" <+> ppr cos )
    WARN( not tysCosFVsInScope,
             text "in_scope" <+> ppr in_scope $$
             text "tenv" <+> ppr tenv $$
             text "cenv" <+> ppr cenv $$
             text "tys" <+> ppr tys $$
             text "cos" <+> ppr cos $$
             text "needInScope" <+> ppr needInScope )
    a
  where
  substDomain = nonDetKeysUFM tenv ++ nonDetKeysUFM cenv
    -- It's OK to use nonDetKeysUFM here, because we only use this list to
    -- remove some elements from a set
  needInScope = (tyCoVarsOfTypes tys `unionVarSet` tyCoVarsOfCos cos)
                  `delListFromUniqSet_Directly` substDomain
  tysCosFVsInScope = needInScope `varSetInScope` in_scope


-- | Substitute within a 'Type'
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substTy :: HasCallStack => TCvSubst -> Type  -> Type
substTy subst ty
  | isEmptyTCvSubst subst = ty
  | otherwise             = checkValidSubst subst [ty] [] $
                            subst_ty subst ty

-- | Substitute within a 'Type' disabling the sanity checks.
-- The problems that the sanity checks in substTy catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTyUnchecked to
-- substTy and remove this function. Please don't use in new code.
substTyUnchecked :: TCvSubst -> Type -> Type
substTyUnchecked subst ty
                 | isEmptyTCvSubst subst = ty
                 | otherwise             = subst_ty subst ty

-- | Substitute within several 'Type's
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substTys :: HasCallStack => TCvSubst -> [Type] -> [Type]
substTys subst tys
  | isEmptyTCvSubst subst = tys
  | otherwise = checkValidSubst subst tys [] $ map (subst_ty subst) tys

-- | Substitute within several 'Type's disabling the sanity checks.
-- The problems that the sanity checks in substTys catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTysUnchecked to
-- substTys and remove this function. Please don't use in new code.
substTysUnchecked :: TCvSubst -> [Type] -> [Type]
substTysUnchecked subst tys
                 | isEmptyTCvSubst subst = tys
                 | otherwise             = map (subst_ty subst) tys

-- | Substitute within a 'ThetaType'
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substTheta :: HasCallStack => TCvSubst -> ThetaType -> ThetaType
substTheta = substTys

-- | Substitute within a 'ThetaType' disabling the sanity checks.
-- The problems that the sanity checks in substTys catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substThetaUnchecked to
-- substTheta and remove this function. Please don't use in new code.
substThetaUnchecked :: TCvSubst -> ThetaType -> ThetaType
substThetaUnchecked = substTysUnchecked


subst_ty :: TCvSubst -> Type -> Type
-- subst_ty is the main workhorse for type substitution
--
-- Note that the in_scope set is poked only if we hit a forall
-- so it may often never be fully computed
subst_ty subst ty
   = go ty
  where
    go (TyVarTy tv)      = substTyVar subst tv
    go (AppTy fun arg)   = mkAppTy (go fun) $! (go arg)
                -- The mkAppTy smart constructor is important
                -- we might be replacing (a Int), represented with App
                -- by [Int], represented with TyConApp
    go (TyConApp tc tys) = let args = map go tys
                           in  args `seqList` TyConApp tc args
    go (FunTy arg res)   = (FunTy $! go arg) $! go res
    go (ForAllTy (Bndr tv vis) ty)
                         = case substVarBndrUnchecked subst tv of
                             (subst', tv') ->
                               (ForAllTy $! ((Bndr $! tv') vis)) $!
                                            (subst_ty subst' ty)
    go (LitTy n)         = LitTy $! n
    go (CastTy ty co)    = (mkCastTy $! (go ty)) $! (subst_co subst co)
    go (CoercionTy co)   = CoercionTy $! (subst_co subst co)

substTyVar :: TCvSubst -> TyVar -> Type
substTyVar (TCvSubst _ tenv _) tv
  = ASSERT( isTyVar tv )
    case lookupVarEnv tenv tv of
      Just ty -> ty
      Nothing -> TyVarTy tv

substTyVars :: TCvSubst -> [TyVar] -> [Type]
substTyVars subst = map $ substTyVar subst

substTyCoVars :: TCvSubst -> [TyCoVar] -> [Type]
substTyCoVars subst = map $ substTyCoVar subst

substTyCoVar :: TCvSubst -> TyCoVar -> Type
substTyCoVar subst tv
  | isTyVar tv = substTyVar subst tv
  | otherwise = CoercionTy $ substCoVar subst tv

lookupTyVar :: TCvSubst -> TyVar  -> Maybe Type
        -- See Note [Extending the TCvSubst]
lookupTyVar (TCvSubst _ tenv _) tv
  = ASSERT( isTyVar tv )
    lookupVarEnv tenv tv

-- | Substitute within a 'Coercion'
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substCo :: HasCallStack => TCvSubst -> Coercion -> Coercion
substCo subst co
  | isEmptyTCvSubst subst = co
  | otherwise = checkValidSubst subst [] [co] $ subst_co subst co

-- | Substitute within a 'Coercion' disabling sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substCoUnchecked :: TCvSubst -> Coercion -> Coercion
substCoUnchecked subst co
  | isEmptyTCvSubst subst = co
  | otherwise = subst_co subst co

-- | Substitute within several 'Coercion's
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substCos :: HasCallStack => TCvSubst -> [Coercion] -> [Coercion]
substCos subst cos
  | isEmptyTCvSubst subst = cos
  | otherwise = checkValidSubst subst [] cos $ map (subst_co subst) cos

subst_co :: TCvSubst -> Coercion -> Coercion
subst_co subst co
  = go co
  where
    go_ty :: Type -> Type
    go_ty = subst_ty subst

    go_mco :: MCoercion -> MCoercion
    go_mco MRefl    = MRefl
    go_mco (MCo co) = MCo (go co)

    go :: Coercion -> Coercion
    go (Refl ty)             = mkNomReflCo $! (go_ty ty)
    go (GRefl r ty mco)      = (mkGReflCo r $! (go_ty ty)) $! (go_mco mco)
    go (TyConAppCo r tc args)= let args' = map go args
                               in  args' `seqList` mkTyConAppCo r tc args'
    go (AppCo co arg)        = (mkAppCo $! go co) $! go arg
    go (ForAllCo tv kind_co co)
      = case substForAllCoBndrUnchecked subst tv kind_co of
         (subst', tv', kind_co') ->
          ((mkForAllCo $! tv') $! kind_co') $! subst_co subst' co
    go (FunCo r co1 co2)     = (mkFunCo r $! go co1) $! go co2
    go (CoVarCo cv)          = substCoVar subst cv
    go (AxiomInstCo con ind cos) = mkAxiomInstCo con ind $! map go cos
    go (UnivCo p r t1 t2)    = (((mkUnivCo $! go_prov p) $! r) $!
                                (go_ty t1)) $! (go_ty t2)
    go (SymCo co)            = mkSymCo $! (go co)
    go (TransCo co1 co2)     = (mkTransCo $! (go co1)) $! (go co2)
    go (NthCo r d co)        = mkNthCo r d $! (go co)
    go (LRCo lr co)          = mkLRCo lr $! (go co)
    go (InstCo co arg)       = (mkInstCo $! (go co)) $! go arg
    go (KindCo co)           = mkKindCo $! (go co)
    go (SubCo co)            = mkSubCo $! (go co)
    go (AxiomRuleCo c cs)    = let cs1 = map go cs
                                in cs1 `seqList` AxiomRuleCo c cs1
    go (HoleCo h)            = HoleCo $! go_hole h

    go_prov UnsafeCoerceProv     = UnsafeCoerceProv
    go_prov (PhantomProv kco)    = PhantomProv (go kco)
    go_prov (ProofIrrelProv kco) = ProofIrrelProv (go kco)
    go_prov p@(PluginProv _)     = p

    -- See Note [Substituting in a coercion hole]
    go_hole h@(CoercionHole { ch_co_var = cv })
      = h { ch_co_var = updateVarType go_ty cv }

substForAllCoBndr :: TCvSubst -> TyCoVar -> KindCoercion
                  -> (TCvSubst, TyCoVar, Coercion)
substForAllCoBndr subst
  = substForAllCoBndrUsing False (substCo subst) subst

-- | Like 'substForAllCoBndr', but disables sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substForAllCoBndrUnchecked :: TCvSubst -> TyCoVar -> KindCoercion
                           -> (TCvSubst, TyCoVar, Coercion)
substForAllCoBndrUnchecked subst
  = substForAllCoBndrUsing False (substCoUnchecked subst) subst

-- See Note [Sym and ForAllCo]
substForAllCoBndrUsing :: Bool  -- apply sym to binder?
                       -> (Coercion -> Coercion)  -- transformation to kind co
                       -> TCvSubst -> TyCoVar -> KindCoercion
                       -> (TCvSubst, TyCoVar, KindCoercion)
substForAllCoBndrUsing sym sco subst old_var
  | isTyVar old_var = substForAllCoTyVarBndrUsing sym sco subst old_var
  | otherwise       = substForAllCoCoVarBndrUsing sym sco subst old_var

substForAllCoTyVarBndrUsing :: Bool  -- apply sym to binder?
                            -> (Coercion -> Coercion)  -- transformation to kind co
                            -> TCvSubst -> TyVar -> KindCoercion
                            -> (TCvSubst, TyVar, KindCoercion)
substForAllCoTyVarBndrUsing sym sco (TCvSubst in_scope tenv cenv) old_var old_kind_co
  = ASSERT( isTyVar old_var )
    ( TCvSubst (in_scope `extendInScopeSet` new_var) new_env cenv
    , new_var, new_kind_co )
  where
    new_env | no_change && not sym = delVarEnv tenv old_var
            | sym       = extendVarEnv tenv old_var $
                          TyVarTy new_var `CastTy` new_kind_co
            | otherwise = extendVarEnv tenv old_var (TyVarTy new_var)

    no_kind_change = noFreeVarsOfCo old_kind_co
    no_change = no_kind_change && (new_var == old_var)

    new_kind_co | no_kind_change = old_kind_co
                | otherwise      = sco old_kind_co

    Pair new_ki1 _ = coercionKind new_kind_co
    -- We could do substitution to (tyVarKind old_var). We don't do so because
    -- we already substituted new_kind_co, which contains the kind information
    -- we want. We don't want to do substitution once more. Also, in most cases,
    -- new_kind_co is a Refl, in which case coercionKind is really fast.

    new_var  = uniqAway in_scope (setTyVarKind old_var new_ki1)

substForAllCoCoVarBndrUsing :: Bool  -- apply sym to binder?
                            -> (Coercion -> Coercion)  -- transformation to kind co
                            -> TCvSubst -> CoVar -> KindCoercion
                            -> (TCvSubst, CoVar, KindCoercion)
substForAllCoCoVarBndrUsing sym sco (TCvSubst in_scope tenv cenv)
                            old_var old_kind_co
  = ASSERT( isCoVar old_var )
    ( TCvSubst (in_scope `extendInScopeSet` new_var) tenv new_cenv
    , new_var, new_kind_co )
  where
    new_cenv | no_change && not sym = delVarEnv cenv old_var
             | otherwise = extendVarEnv cenv old_var (mkCoVarCo new_var)

    no_kind_change = noFreeVarsOfCo old_kind_co
    no_change = no_kind_change && (new_var == old_var)

    new_kind_co | no_kind_change = old_kind_co
                | otherwise      = sco old_kind_co

    Pair h1 h2 = coercionKind new_kind_co

    new_var       = uniqAway in_scope $ mkCoVar (varName old_var) new_var_type
    new_var_type  | sym       = h2
                  | otherwise = h1

substCoVar :: TCvSubst -> CoVar -> Coercion
substCoVar (TCvSubst _ _ cenv) cv
  = case lookupVarEnv cenv cv of
      Just co -> co
      Nothing -> CoVarCo cv

substCoVars :: TCvSubst -> [CoVar] -> [Coercion]
substCoVars subst cvs = map (substCoVar subst) cvs

lookupCoVar :: TCvSubst -> Var -> Maybe Coercion
lookupCoVar (TCvSubst _ _ cenv) v = lookupVarEnv cenv v

substTyVarBndr :: HasCallStack => TCvSubst -> TyVar -> (TCvSubst, TyVar)
substTyVarBndr = substTyVarBndrUsing substTy

substTyVarBndrs :: HasCallStack => TCvSubst -> [TyVar] -> (TCvSubst, [TyVar])
substTyVarBndrs = mapAccumL substTyVarBndr

substVarBndr :: HasCallStack => TCvSubst -> TyCoVar -> (TCvSubst, TyCoVar)
substVarBndr = substVarBndrUsing substTy

substVarBndrs :: HasCallStack => TCvSubst -> [TyCoVar] -> (TCvSubst, [TyCoVar])
substVarBndrs = mapAccumL substVarBndr

substCoVarBndr :: HasCallStack => TCvSubst -> CoVar -> (TCvSubst, CoVar)
substCoVarBndr = substCoVarBndrUsing substTy

-- | Like 'substVarBndr', but disables sanity checks.
-- The problems that the sanity checks in substTy catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTyUnchecked to
-- substTy and remove this function. Please don't use in new code.
substVarBndrUnchecked :: TCvSubst -> TyCoVar -> (TCvSubst, TyCoVar)
substVarBndrUnchecked = substVarBndrUsing substTyUnchecked

substVarBndrUsing :: (TCvSubst -> Type -> Type)
                  -> TCvSubst -> TyCoVar -> (TCvSubst, TyCoVar)
substVarBndrUsing subst_fn subst v
  | isTyVar v = substTyVarBndrUsing subst_fn subst v
  | otherwise = substCoVarBndrUsing subst_fn subst v

-- | Substitute a tyvar in a binding position, returning an
-- extended subst and a new tyvar.
-- Use the supplied function to substitute in the kind
substTyVarBndrUsing
  :: (TCvSubst -> Type -> Type)  -- ^ Use this to substitute in the kind
  -> TCvSubst -> TyVar -> (TCvSubst, TyVar)
substTyVarBndrUsing subst_fn subst@(TCvSubst in_scope tenv cenv) old_var
  = ASSERT2( _no_capture, pprTyVar old_var $$ pprTyVar new_var $$ ppr subst )
    ASSERT( isTyVar old_var )
    (TCvSubst (in_scope `extendInScopeSet` new_var) new_env cenv, new_var)
  where
    new_env | no_change = delVarEnv tenv old_var
            | otherwise = extendVarEnv tenv old_var (TyVarTy new_var)

    _no_capture = not (new_var `elemVarSet` tyCoVarsOfTypesSet tenv)
    -- Assertion check that we are not capturing something in the substitution

    old_ki = tyVarKind old_var
    no_kind_change = noFreeVarsOfType old_ki -- verify that kind is closed
    no_change = no_kind_change && (new_var == old_var)
        -- no_change means that the new_var is identical in
        -- all respects to the old_var (same unique, same kind)
        -- See Note [Extending the TCvSubst]
        --
        -- In that case we don't need to extend the substitution
        -- to map old to new.  But instead we must zap any
        -- current substitution for the variable. For example:
        --      (\x.e) with id_subst = [x |-> e']
        -- Here we must simply zap the substitution for x

    new_var | no_kind_change = uniqAway in_scope old_var
            | otherwise = uniqAway in_scope $
                          setTyVarKind old_var (subst_fn subst old_ki)
        -- The uniqAway part makes sure the new variable is not already in scope

-- | Substitute a covar in a binding position, returning an
-- extended subst and a new covar.
-- Use the supplied function to substitute in the kind
substCoVarBndrUsing
  :: (TCvSubst -> Type -> Type)
  -> TCvSubst -> CoVar -> (TCvSubst, CoVar)
substCoVarBndrUsing subst_fn subst@(TCvSubst in_scope tenv cenv) old_var
  = ASSERT( isCoVar old_var )
    (TCvSubst (in_scope `extendInScopeSet` new_var) tenv new_cenv, new_var)
  where
    new_co         = mkCoVarCo new_var
    no_kind_change = noFreeVarsOfTypes [t1, t2]
    no_change      = new_var == old_var && no_kind_change

    new_cenv | no_change = delVarEnv cenv old_var
             | otherwise = extendVarEnv cenv old_var new_co

    new_var = uniqAway in_scope subst_old_var
    subst_old_var = mkCoVar (varName old_var) new_var_type

    (_, _, t1, t2, role) = coVarKindsTypesRole old_var
    t1' = subst_fn subst t1
    t2' = subst_fn subst t2
    new_var_type = mkCoercionType role t1' t2'
                  -- It's important to do the substitution for coercions,
                  -- because they can have free type variables

cloneTyVarBndr :: TCvSubst -> TyVar -> Unique -> (TCvSubst, TyVar)
cloneTyVarBndr subst@(TCvSubst in_scope tv_env cv_env) tv uniq
  = ASSERT2( isTyVar tv, ppr tv )   -- I think it's only called on TyVars
    (TCvSubst (extendInScopeSet in_scope tv')
              (extendVarEnv tv_env tv (mkTyVarTy tv')) cv_env, tv')
  where
    old_ki = tyVarKind tv
    no_kind_change = noFreeVarsOfType old_ki -- verify that kind is closed

    tv1 | no_kind_change = tv
        | otherwise      = setTyVarKind tv (substTy subst old_ki)

    tv' = setVarUnique tv1 uniq

cloneTyVarBndrs :: TCvSubst -> [TyVar] -> UniqSupply -> (TCvSubst, [TyVar])
cloneTyVarBndrs subst []     _usupply = (subst, [])
cloneTyVarBndrs subst (t:ts)  usupply = (subst'', tv:tvs)
  where
    (uniq, usupply') = takeUniqFromSupply usupply
    (subst' , tv )   = cloneTyVarBndr subst t uniq
    (subst'', tvs)   = cloneTyVarBndrs subst' ts usupply'

{-
%************************************************************************
%*                                                                      *
                   Pretty-printing types

       Defined very early because of debug printing in assertions
%*                                                                      *
%************************************************************************

@pprType@ is the standard @Type@ printer; the overloaded @ppr@ function is
defined to use this.  @pprParendType@ is the same, except it puts
parens around the type, except for the atomic cases.  @pprParendType@
works just by setting the initial context precedence very high.

Note that any function which pretty-prints a @Type@ first converts the @Type@
to an @IfaceType@. See Note [IfaceType and pretty-printing] in IfaceType.

See Note [Precedence in types] in BasicTypes.
-}

--------------------------------------------------------
-- When pretty-printing types, we convert to IfaceType,
--   and pretty-print that.
-- See Note [Pretty printing via IfaceSyn] in PprTyThing
--------------------------------------------------------

pprType, pprParendType :: Type -> SDoc
pprType       = pprPrecType topPrec
pprParendType = pprPrecType appPrec

pprPrecType :: PprPrec -> Type -> SDoc
pprPrecType = pprPrecTypeX emptyTidyEnv

pprPrecTypeX :: TidyEnv -> PprPrec -> Type -> SDoc
pprPrecTypeX env prec ty
  = getPprStyle $ \sty ->
    if debugStyle sty           -- Use debugPprType when in
    then debug_ppr_ty prec ty   -- when in debug-style
    else pprPrecIfaceType prec (tidyToIfaceTypeStyX env ty sty)

pprTyLit :: TyLit -> SDoc
pprTyLit = pprIfaceTyLit . toIfaceTyLit

pprKind, pprParendKind :: Kind -> SDoc
pprKind       = pprType
pprParendKind = pprParendType

tidyToIfaceTypeStyX :: TidyEnv -> Type -> PprStyle -> IfaceType
tidyToIfaceTypeStyX env ty sty
  | userStyle sty = tidyToIfaceTypeX env ty
  | otherwise     = toIfaceTypeX (tyCoVarsOfType ty) ty
     -- in latter case, don't tidy, as we'll be printing uniques.

tidyToIfaceType :: Type -> IfaceType
tidyToIfaceType = tidyToIfaceTypeX emptyTidyEnv

tidyToIfaceTypeX :: TidyEnv -> Type -> IfaceType
-- It's vital to tidy before converting to an IfaceType
-- or nested binders will become indistinguishable!
--
-- Also for the free type variables, tell toIfaceTypeX to
-- leave them as IfaceFreeTyVar.  This is super-important
-- for debug printing.
tidyToIfaceTypeX env ty = toIfaceTypeX (mkVarSet free_tcvs) (tidyType env' ty)
  where
    env'      = tidyFreeTyCoVars env free_tcvs
    free_tcvs = tyCoVarsOfTypeWellScoped ty

------------
pprCo, pprParendCo :: Coercion -> SDoc
pprCo       co = getPprStyle $ \ sty -> pprIfaceCoercion (tidyToIfaceCoSty co sty)
pprParendCo co = getPprStyle $ \ sty -> pprParendIfaceCoercion (tidyToIfaceCoSty co sty)

tidyToIfaceCoSty :: Coercion -> PprStyle -> IfaceCoercion
tidyToIfaceCoSty co sty
  | userStyle sty = tidyToIfaceCo co
  | otherwise     = toIfaceCoercionX (tyCoVarsOfCo co) co
     -- in latter case, don't tidy, as we'll be printing uniques.

tidyToIfaceCo :: Coercion -> IfaceCoercion
-- It's vital to tidy before converting to an IfaceType
-- or nested binders will become indistinguishable!
--
-- Also for the free type variables, tell toIfaceCoercionX to
-- leave them as IfaceFreeCoVar.  This is super-important
-- for debug printing.
tidyToIfaceCo co = toIfaceCoercionX (mkVarSet free_tcvs) (tidyCo env co)
  where
    env       = tidyFreeTyCoVars emptyTidyEnv free_tcvs
    free_tcvs = scopedSort $ tyCoVarsOfCoList co
------------
pprClassPred :: Class -> [Type] -> SDoc
pprClassPred clas tys = pprTypeApp (classTyCon clas) tys

------------
pprTheta :: ThetaType -> SDoc
pprTheta = pprIfaceContext topPrec . map tidyToIfaceType

pprParendTheta :: ThetaType -> SDoc
pprParendTheta = pprIfaceContext appPrec . map tidyToIfaceType

pprThetaArrowTy :: ThetaType -> SDoc
pprThetaArrowTy = pprIfaceContextArr . map tidyToIfaceType

------------------
instance Outputable Type where
    ppr ty = pprType ty

instance Outputable TyLit where
   ppr = pprTyLit

------------------
pprSigmaType :: Type -> SDoc
pprSigmaType = pprIfaceSigmaType ShowForAllWhen . tidyToIfaceType

pprForAll :: [TyCoVarBinder] -> SDoc
pprForAll tvs = pprIfaceForAll (map toIfaceForAllBndr tvs)

-- | Print a user-level forall; see Note [When to print foralls]
pprUserForAll :: [TyCoVarBinder] -> SDoc
pprUserForAll = pprUserIfaceForAll . map toIfaceForAllBndr

pprTCvBndrs :: [TyCoVarBinder] -> SDoc
pprTCvBndrs tvs = sep (map pprTCvBndr tvs)

pprTCvBndr :: TyCoVarBinder -> SDoc
pprTCvBndr = pprTyVar . binderVar

pprTyVars :: [TyVar] -> SDoc
pprTyVars tvs = sep (map pprTyVar tvs)

pprTyVar :: TyVar -> SDoc
-- Print a type variable binder with its kind (but not if *)
-- Here we do not go via IfaceType, because the duplication with
-- pprIfaceTvBndr is minimal, and the loss of uniques etc in
-- debug printing is disastrous
pprTyVar tv
  | isLiftedTypeKind kind = ppr tv
  | otherwise             = parens (ppr tv <+> dcolon <+> ppr kind)
  where
    kind = tyVarKind tv

instance Outputable TyCoBinder where
  ppr (Anon ty) = text "[anon]" <+> ppr ty
  ppr (Named (Bndr v Required))  = ppr v
  ppr (Named (Bndr v Specified)) = char '@' <> ppr v
  ppr (Named (Bndr v Inferred))  = braces (ppr v)

-----------------
instance Outputable Coercion where -- defined here to avoid orphans
  ppr = pprCo

debugPprType :: Type -> SDoc
-- ^ debugPprType is a simple pretty printer that prints a type
-- without going through IfaceType.  It does not format as prettily
-- as the normal route, but it's much more direct, and that can
-- be useful for debugging.  E.g. with -dppr-debug it prints the
-- kind on type-variable /occurrences/ which the normal route
-- fundamentally cannot do.
debugPprType ty = debug_ppr_ty topPrec ty

debug_ppr_ty :: PprPrec -> Type -> SDoc
debug_ppr_ty _ (LitTy l)
  = ppr l

debug_ppr_ty _ (TyVarTy tv)
  = ppr tv  -- With -dppr-debug we get (tv :: kind)

debug_ppr_ty prec (FunTy arg res)
  = maybeParen prec funPrec $
    sep [debug_ppr_ty funPrec arg, arrow <+> debug_ppr_ty prec res]

debug_ppr_ty prec (TyConApp tc tys)
  | null tys  = ppr tc
  | otherwise = maybeParen prec appPrec $
                hang (ppr tc) 2 (sep (map (debug_ppr_ty appPrec) tys))

debug_ppr_ty _ (AppTy t1 t2)
  = hang (debug_ppr_ty appPrec t1)  -- Print parens so we see ((a b) c)
       2 (debug_ppr_ty appPrec t2)  -- so that we can distinguish
                                    -- TyConApp from AppTy

debug_ppr_ty prec (CastTy ty co)
  = maybeParen prec topPrec $
    hang (debug_ppr_ty topPrec ty)
       2 (text "|>" <+> ppr co)

debug_ppr_ty _ (CoercionTy co)
  = parens (text "CO" <+> ppr co)

debug_ppr_ty prec ty@(ForAllTy {})
  | (tvs, body) <- split ty
  = maybeParen prec funPrec $
    hang (text "forall" <+> fsep (map ppr tvs) <> dot)
         -- The (map ppr tvs) will print kind-annotated
         -- tvs, because we are (usually) in debug-style
       2 (ppr body)
  where
    split ty | ForAllTy tv ty' <- ty
             , (tvs, body) <- split ty'
             = (tv:tvs, body)
             | otherwise
             = ([], ty)

{-
Note [When to print foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Mostly we want to print top-level foralls when (and only when) the user specifies
-fprint-explicit-foralls.  But when kind polymorphism is at work, that suppresses
too much information; see Trac #9018.

So I'm trying out this rule: print explicit foralls if
  a) User specifies -fprint-explicit-foralls, or
  b) Any of the quantified type variables has a kind
     that mentions a kind variable

This catches common situations, such as a type siguature
     f :: m a
which means
      f :: forall k. forall (m :: k->*) (a :: k). m a
We really want to see both the "forall k" and the kind signatures
on m and a.  The latter comes from pprTCvBndr.

Note [Infix type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
With TypeOperators you can say

   f :: (a ~> b) -> b

and the (~>) is considered a type variable.  However, the type
pretty-printer in this module will just see (a ~> b) as

   App (App (TyVarTy "~>") (TyVarTy "a")) (TyVarTy "b")

So it'll print the type in prefix form.  To avoid confusion we must
remember to parenthesise the operator, thus

   (~>) a b -> b

See Trac #2766.
-}

pprDataCons :: TyCon -> SDoc
pprDataCons = sepWithVBars . fmap pprDataConWithArgs . tyConDataCons
  where
    sepWithVBars [] = empty
    sepWithVBars docs = sep (punctuate (space <> vbar) docs)

pprDataConWithArgs :: DataCon -> SDoc
pprDataConWithArgs dc = sep [forAllDoc, thetaDoc, ppr dc <+> argsDoc]
  where
    (_univ_tvs, _ex_tvs, _eq_spec, theta, arg_tys, _res_ty) = dataConFullSig dc
    user_bndrs = dataConUserTyVarBinders dc
    forAllDoc  = pprUserForAll user_bndrs
    thetaDoc   = pprThetaArrowTy theta
    argsDoc    = hsep (fmap pprParendType arg_tys)


pprTypeApp :: TyCon -> [Type] -> SDoc
pprTypeApp tc tys
  = pprIfaceTypeApp topPrec (toIfaceTyCon tc)
                            (toIfaceTcArgs tc tys)
    -- TODO: toIfaceTcArgs seems rather wasteful here

------------------
-- | Display all kind information (with @-fprint-explicit-kinds@) when the
-- provided 'Bool' argument is 'True'.
-- See @Note [Kind arguments in error messages]@ in "TcErrors".
pprWithExplicitKindsWhen :: Bool -> SDoc -> SDoc
pprWithExplicitKindsWhen b
  = updSDocDynFlags $ \dflags ->
      if b then gopt_set dflags Opt_PrintExplicitKinds
           else dflags

{-
%************************************************************************
%*                                                                      *
\subsection{TidyType}
%*                                                                      *
%************************************************************************
-}

-- | This tidies up a type for printing in an error message, or in
-- an interface file.
--
-- It doesn't change the uniques at all, just the print names.
tidyVarBndrs :: TidyEnv -> [TyCoVar] -> (TidyEnv, [TyCoVar])
tidyVarBndrs tidy_env tvs
  = mapAccumL tidyVarBndr (avoidNameClashes tvs tidy_env) tvs

tidyVarBndr :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
tidyVarBndr tidy_env@(occ_env, subst) var
  = case tidyOccName occ_env (getHelpfulOccName var) of
      (occ_env', occ') -> ((occ_env', subst'), var')
        where
          subst' = extendVarEnv subst var var'
          var'   = setVarType (setVarName var name') type'
          type'  = tidyType tidy_env (varType var)
          name'  = tidyNameOcc name occ'
          name   = varName var

avoidNameClashes :: [TyCoVar] -> TidyEnv -> TidyEnv
-- Seed the occ_env with clashes among the names, see
-- Node [Tidying multiple names at once] in OccName
avoidNameClashes tvs (occ_env, subst)
  = (avoidClashesOccEnv occ_env occs, subst)
  where
    occs = map getHelpfulOccName tvs

getHelpfulOccName :: TyCoVar -> OccName
-- A TcTyVar with a System Name is probably a
-- unification variable; when we tidy them we give them a trailing
-- "0" (or 1 etc) so that they don't take precedence for the
-- un-modified name. Plus, indicating a unification variable in
-- this way is a helpful clue for users
getHelpfulOccName tv
  | isSystemName name, isTcTyVar tv
  = mkTyVarOcc (occNameString occ ++ "0")
  | otherwise
  = occ
  where
   name = varName tv
   occ  = getOccName name

tidyTyCoVarBinder :: TidyEnv -> VarBndr TyCoVar vis
                  -> (TidyEnv, VarBndr TyCoVar vis)
tidyTyCoVarBinder tidy_env (Bndr tv vis)
  = (tidy_env', Bndr tv' vis)
  where
    (tidy_env', tv') = tidyVarBndr tidy_env tv

tidyTyCoVarBinders :: TidyEnv -> [VarBndr TyCoVar vis]
                   -> (TidyEnv, [VarBndr TyCoVar vis])
tidyTyCoVarBinders = mapAccumL tidyTyCoVarBinder

---------------
tidyFreeTyCoVars :: TidyEnv -> [TyCoVar] -> TidyEnv
-- ^ Add the free 'TyVar's to the env in tidy form,
-- so that we can tidy the type they are free in
tidyFreeTyCoVars (full_occ_env, var_env) tyvars
  = fst (tidyOpenTyCoVars (full_occ_env, var_env) tyvars)

---------------
tidyOpenTyCoVars :: TidyEnv -> [TyCoVar] -> (TidyEnv, [TyCoVar])
tidyOpenTyCoVars env tyvars = mapAccumL tidyOpenTyCoVar env tyvars

---------------
tidyOpenTyCoVar :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
-- ^ Treat a new 'TyCoVar' as a binder, and give it a fresh tidy name
-- using the environment if one has not already been allocated. See
-- also 'tidyVarBndr'
tidyOpenTyCoVar env@(_, subst) tyvar
  = case lookupVarEnv subst tyvar of
        Just tyvar' -> (env, tyvar')              -- Already substituted
        Nothing     ->
          let env' = tidyFreeTyCoVars env (tyCoVarsOfTypeList (tyVarKind tyvar))
          in tidyVarBndr env' tyvar  -- Treat it as a binder

---------------
tidyTyCoVarOcc :: TidyEnv -> TyCoVar -> TyCoVar
tidyTyCoVarOcc env@(_, subst) tv
  = case lookupVarEnv subst tv of
        Nothing  -> updateVarType (tidyType env) tv
        Just tv' -> tv'

---------------
tidyTypes :: TidyEnv -> [Type] -> [Type]
tidyTypes env tys = map (tidyType env) tys

---------------
tidyType :: TidyEnv -> Type -> Type
tidyType _   (LitTy n)            = LitTy n
tidyType env (TyVarTy tv)         = TyVarTy (tidyTyCoVarOcc env tv)
tidyType env (TyConApp tycon tys) = let args = tidyTypes env tys
                                    in args `seqList` TyConApp tycon args
tidyType env (AppTy fun arg)      = (AppTy $! (tidyType env fun)) $! (tidyType env arg)
tidyType env (FunTy fun arg)      = (FunTy $! (tidyType env fun)) $! (tidyType env arg)
tidyType env (ty@(ForAllTy{}))    = mkForAllTys' (zip tvs' vis) $! tidyType env' body_ty
  where
    (tvs, vis, body_ty) = splitForAllTys' ty
    (env', tvs') = tidyVarBndrs env tvs
tidyType env (CastTy ty co)       = (CastTy $! tidyType env ty) $! (tidyCo env co)
tidyType env (CoercionTy co)      = CoercionTy $! (tidyCo env co)


-- The following two functions differ from mkForAllTys and splitForAllTys in that
-- they expect/preserve the ArgFlag argument. Thes belong to types/Type.hs, but
-- how should they be named?
mkForAllTys' :: [(TyCoVar, ArgFlag)] -> Type -> Type
mkForAllTys' tvvs ty = foldr strictMkForAllTy ty tvvs
  where
    strictMkForAllTy (tv,vis) ty = (ForAllTy $! ((Bndr $! tv) $! vis)) $! ty

splitForAllTys' :: Type -> ([TyCoVar], [ArgFlag], Type)
splitForAllTys' ty = go ty [] []
  where
    go (ForAllTy (Bndr tv vis) ty) tvs viss = go ty (tv:tvs) (vis:viss)
    go ty                          tvs viss = (reverse tvs, reverse viss, ty)


---------------
-- | Grabs the free type variables, tidies them
-- and then uses 'tidyType' to work over the type itself
tidyOpenTypes :: TidyEnv -> [Type] -> (TidyEnv, [Type])
tidyOpenTypes env tys
  = (env', tidyTypes (trimmed_occ_env, var_env) tys)
  where
    (env'@(_, var_env), tvs') = tidyOpenTyCoVars env $
                                tyCoVarsOfTypesWellScoped tys
    trimmed_occ_env = initTidyOccEnv (map getOccName tvs')
      -- The idea here was that we restrict the new TidyEnv to the
      -- _free_ vars of the types, so that we don't gratuitously rename
      -- the _bound_ variables of the types.

---------------
tidyOpenType :: TidyEnv -> Type -> (TidyEnv, Type)
tidyOpenType env ty = let (env', [ty']) = tidyOpenTypes env [ty] in
                      (env', ty')

---------------
-- | Calls 'tidyType' on a top-level type (i.e. with an empty tidying environment)
tidyTopType :: Type -> Type
tidyTopType ty = tidyType emptyTidyEnv ty

---------------
tidyOpenKind :: TidyEnv -> Kind -> (TidyEnv, Kind)
tidyOpenKind = tidyOpenType

tidyKind :: TidyEnv -> Kind -> Kind
tidyKind = tidyType

----------------
tidyCo :: TidyEnv -> Coercion -> Coercion
tidyCo env@(_, subst) co
  = go co
  where
    go_mco MRefl    = MRefl
    go_mco (MCo co) = MCo (go co)

    go (Refl ty)             = Refl (tidyType env ty)
    go (GRefl r ty mco)      = GRefl r (tidyType env ty) $! go_mco mco
    go (TyConAppCo r tc cos) = let args = map go cos
                               in args `seqList` TyConAppCo r tc args
    go (AppCo co1 co2)       = (AppCo $! go co1) $! go co2
    go (ForAllCo tv h co)    = ((ForAllCo $! tvp) $! (go h)) $! (tidyCo envp co)
                               where (envp, tvp) = tidyVarBndr env tv
            -- the case above duplicates a bit of work in tidying h and the kind
            -- of tv. But the alternative is to use coercionKind, which seems worse.
    go (FunCo r co1 co2)     = (FunCo r $! go co1) $! go co2
    go (CoVarCo cv)          = case lookupVarEnv subst cv of
                                 Nothing  -> CoVarCo cv
                                 Just cv' -> CoVarCo cv'
    go (HoleCo h)            = HoleCo h
    go (AxiomInstCo con ind cos) = let args = map go cos
                               in  args `seqList` AxiomInstCo con ind args
    go (UnivCo p r t1 t2)    = (((UnivCo $! (go_prov p)) $! r) $!
                                tidyType env t1) $! tidyType env t2
    go (SymCo co)            = SymCo $! go co
    go (TransCo co1 co2)     = (TransCo $! go co1) $! go co2
    go (NthCo r d co)        = NthCo r d $! go co
    go (LRCo lr co)          = LRCo lr $! go co
    go (InstCo co ty)        = (InstCo $! go co) $! go ty
    go (KindCo co)           = KindCo $! go co
    go (SubCo co)            = SubCo $! go co
    go (AxiomRuleCo ax cos)  = let cos1 = tidyCos env cos
                               in cos1 `seqList` AxiomRuleCo ax cos1

    go_prov UnsafeCoerceProv    = UnsafeCoerceProv
    go_prov (PhantomProv co)    = PhantomProv (go co)
    go_prov (ProofIrrelProv co) = ProofIrrelProv (go co)
    go_prov p@(PluginProv _)    = p

tidyCos :: TidyEnv -> [Coercion] -> [Coercion]
tidyCos env = map (tidyCo env)


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
typeSize (LitTy {})                 = 1
typeSize (TyVarTy {})               = 1
typeSize (AppTy t1 t2)              = typeSize t1 + typeSize t2
typeSize (FunTy t1 t2)              = typeSize t1 + typeSize t2
typeSize (ForAllTy (Bndr tv _) t)   = typeSize (varType tv) + typeSize t
typeSize (TyConApp _ ts)            = 1 + sum (map typeSize ts)
typeSize (CastTy ty co)             = typeSize ty + coercionSize co
typeSize (CoercionTy co)            = coercionSize co

coercionSize :: Coercion -> Int
coercionSize (Refl ty)             = typeSize ty
coercionSize (GRefl _ ty MRefl)    = typeSize ty
coercionSize (GRefl _ ty (MCo co)) = 1 + typeSize ty + coercionSize co
coercionSize (TyConAppCo _ _ args) = 1 + sum (map coercionSize args)
coercionSize (AppCo co arg)      = coercionSize co + coercionSize arg
coercionSize (ForAllCo _ h co)   = 1 + coercionSize co + coercionSize h
coercionSize (FunCo _ co1 co2)   = 1 + coercionSize co1 + coercionSize co2
coercionSize (CoVarCo _)         = 1
coercionSize (HoleCo _)          = 1
coercionSize (AxiomInstCo _ _ args) = 1 + sum (map coercionSize args)
coercionSize (UnivCo p _ t1 t2)  = 1 + provSize p + typeSize t1 + typeSize t2
coercionSize (SymCo co)          = 1 + coercionSize co
coercionSize (TransCo co1 co2)   = 1 + coercionSize co1 + coercionSize co2
coercionSize (NthCo _ _ co)      = 1 + coercionSize co
coercionSize (LRCo  _ co)        = 1 + coercionSize co
coercionSize (InstCo co arg)     = 1 + coercionSize co + coercionSize arg
coercionSize (KindCo co)         = 1 + coercionSize co
coercionSize (SubCo co)          = 1 + coercionSize co
coercionSize (AxiomRuleCo _ cs)  = 1 + sum (map coercionSize cs)

provSize :: UnivCoProvenance -> Int
provSize UnsafeCoerceProv    = 1
provSize (PhantomProv co)    = 1 + coercionSize co
provSize (ProofIrrelProv co) = 1 + coercionSize co
provSize (PluginProv _)      = 1
