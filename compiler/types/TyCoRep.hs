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
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP, DeriveDataTypeable, MultiWayIf #-}
{-# LANGUAGE ImplicitParams #-}

module TyCoRep (
        TyThing(..), tyThingCategory, pprTyThingCategory, pprShortTyThing,

        -- * Types
        Type(..),
        TyLit(..),
        KindOrType, Kind,
        PredType, ThetaType,      -- Synonyms
        ArgFlag(..),

        -- * Coercions
        Coercion(..),
        UnivCoProvenance(..), CoercionHole(..),
        CoercionN, CoercionR, CoercionP, KindCoercion,

        -- * Functions over types
        mkTyConTy, mkTyVarTy, mkTyVarTys,
        mkFunTy, mkFunTys, mkForAllTy, mkForAllTys,
        mkPiTy, mkPiTys,
        isLiftedTypeKind, isUnliftedTypeKind,
        isCoercionType, isRuntimeRepTy, isRuntimeRepVar,
        isRuntimeRepKindedTy, dropRuntimeRepArgs,
        sameVis,

        -- * Functions over binders
        TyBinder(..), TyVarBinder,
        binderVar, binderVars, binderKind, binderArgFlag,
        delBinderVar,
        isInvisibleArgFlag, isVisibleArgFlag,
        isInvisibleBinder, isVisibleBinder,

        -- * Functions over coercions
        pickLR,

        -- * Pretty-printing
        pprType, pprParendType, pprPrecType,
        pprTypeApp, pprTvBndr, pprTvBndrs,
        pprSigmaType,
        pprTheta, pprParendTheta, pprForAll, pprUserForAll,
        pprTyVar, pprTyVars,
        pprThetaArrowTy, pprClassPred,
        pprKind, pprParendKind, pprTyLit,
        TyPrec(..), maybeParen,
        pprPrefixApp, pprArrowChain,
        pprDataCons, ppSuggestExplicitKinds,

        pprCo, pprParendCo,

        -- * Free variables
        tyCoVarsOfType, tyCoVarsOfTypeDSet, tyCoVarsOfTypes, tyCoVarsOfTypesDSet,
        tyCoFVsBndr, tyCoFVsOfType, tyCoVarsOfTypeList,
        tyCoFVsOfTypes, tyCoVarsOfTypesList,
        closeOverKindsDSet, closeOverKindsFV, closeOverKindsList,
        coVarsOfType, coVarsOfTypes,
        coVarsOfCo, coVarsOfCos,
        tyCoVarsOfCo, tyCoVarsOfCos,
        tyCoVarsOfCoDSet,
        tyCoFVsOfCo, tyCoFVsOfCos,
        tyCoVarsOfCoList, tyCoVarsOfProv,
        closeOverKinds,

        noFreeVarsOfType, noFreeVarsOfCo,

        -- * Substitutions
        TCvSubst(..), TvSubstEnv, CvSubstEnv,
        emptyTvSubstEnv, emptyCvSubstEnv, composeTCvSubstEnv, composeTCvSubst,
        emptyTCvSubst, mkEmptyTCvSubst, isEmptyTCvSubst,
        mkTCvSubst, mkTvSubst,
        getTvSubstEnv,
        getCvSubstEnv, getTCvInScope, getTCvSubstRangeFVs,
        isInScope, notElemTCvSubst,
        setTvSubstEnv, setCvSubstEnv, zapTCvSubst,
        extendTCvInScope, extendTCvInScopeList, extendTCvInScopeSet,
        extendTCvSubst,
        extendCvSubst, extendCvSubstWithClone,
        extendTvSubst, extendTvSubstBinderAndInScope, extendTvSubstWithClone,
        extendTvSubstList, extendTvSubstAndInScope,
        unionTCvSubst, zipTyEnv, zipCoEnv, mkTyCoInScopeSet,
        zipTvSubst, zipCvSubst,
        mkTvSubstPrs,

        substTyWith, substTyWithCoVars, substTysWith, substTysWithCoVars,
        substCoWith,
        substTy, substTyAddInScope,
        substTyUnchecked, substTysUnchecked, substThetaUnchecked,
        substTyWithUnchecked,
        substCoUnchecked, substCoWithUnchecked,
        substTyWithInScope,
        substTys, substTheta,
        lookupTyVar, substTyVarBndr,
        substCo, substCos, substCoVar, substCoVars, lookupCoVar,
        substCoVarBndr, cloneTyVarBndr, cloneTyVarBndrs,
        substTyVar, substTyVars,
        substForAllCoBndr,
        substTyVarBndrCallback, substForAllCoBndrCallback,
        checkValidSubst, isValidTCvSubst,

        -- * Tidying type related things up for printing
        tidyType,      tidyTypes,
        tidyOpenType,  tidyOpenTypes,
        tidyOpenKind,
        tidyTyCoVarBndr, tidyTyCoVarBndrs, tidyFreeTyCoVars,
        tidyOpenTyCoVar, tidyOpenTyCoVars,
        tidyTyVarOcc,
        tidyTopType,
        tidyKind,
        tidyCo, tidyCos,
        tidyTyVarBinder, tidyTyVarBinders,

        -- * Sizes
        typeSize, coercionSize, provSize
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} DataCon( dataConFullSig
                             , dataConUnivTyVarBinders, dataConExTyVarBinders
                             , DataCon, filterEqSpec )
import {-# SOURCE #-} Type( isPredTy, isCoercionTy, mkAppTy, mkCastTy
                          , tyCoVarsOfTypeWellScoped
                          , tyCoVarsOfTypesWellScoped
                          , toposortTyVars
                          , coreView, typeKind )
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
import BasicTypes ( LeftOrRight(..), TyPrec(..), maybeParen, pickLR )
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
                        --  1) Function: must /not/ be a 'TyConApp',
                        --     must be another 'AppTy', or 'TyVarTy'
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
        {-# UNPACK #-} !TyVarBinder
        Type            -- ^ A Π type.

  | FunTy Type Type     -- ^ t1 -> t2   Very common, so an important special case

  | LitTy TyLit     -- ^ Type literals are similar to type constructors.

  | CastTy
        Type
        KindCoercion  -- ^ A kind cast. The coercion is always nominal.
                      -- INVARIANT: The cast is never refl.
                      -- INVARIANT: The cast is "pushed down" as far as it
                      -- can go. See Note [Pushing down casts]

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

Note [Pushing down casts]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have (a :: k1 -> *), (b :: k1), and (co :: * ~ q).
The type (a b |> co) is `eqType` to ((a |> co') b), where
co' = (->) <k1> co. Thus, to make this visible to functions
that inspect types, we always push down coercions, preferring
the second form. Note that this also applies to TyConApps!

Note [Non-trivial definitional equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Is Int |> <*> the same as Int? YES! In order to reduce headaches,
we decide that any reflexive casts in types are just ignored. More
generally, the `eqType` function, which defines Core's type equality
relation, ignores casts and coercion arguments, as long as the
two types have the same kind. This allows us to be a little sloppier
in keeping track of coercions, which is a good thing. It also means
that eqType does not depend on eqCoercion, which is also a good thing.

Why is this sensible? That is, why is something different than α-equivalence
appropriate for the implementation of eqType?

Anything smaller than ~ and homogeneous is an appropriate definition for
equality. The type safety of FC depends only on ~. Let's say η : τ ~ σ. Any
expression of type τ can be transmuted to one of type σ at any point by
casting. The same is true of types of type τ. So in some sense, τ and σ are
interchangeable.

But let's be more precise. If we examine the typing rules of FC (say, those in
http://www.cis.upenn.edu/~eir/papers/2015/equalities/equalities-extended.pdf)
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

 ** If (t1 eqType t2) then I can replace t1 by t2 anywhere. **

This principle also tells us that eqType must relate only types with the
same kinds.
-}

{- **********************************************************************
*                                                                       *
                  TyBinder and ArgFlag
*                                                                       *
********************************************************************** -}

-- | A 'TyBinder' represents an argument to a function. TyBinders can be dependent
-- ('Named') or nondependent ('Anon'). They may also be visible or not.
-- See Note [TyBinders]
data TyBinder
  = Named TyVarBinder   -- A type-lambda binder
  | Anon Type           -- A term-lambda binder
                        -- Visibility is determined by the type (Constraint vs. *)
  deriving Data.Data

-- | Remove the binder's variable from the set, if the binder has
-- a variable.
delBinderVar :: VarSet -> TyVarBinder -> VarSet
delBinderVar vars (TvBndr tv _) = vars `delVarSet` tv

-- | Does this binder bind an invisible argument?
isInvisibleBinder :: TyBinder -> Bool
isInvisibleBinder (Named (TvBndr _ vis)) = isInvisibleArgFlag vis
isInvisibleBinder (Anon ty)              = isPredTy ty

-- | Does this binder bind a visible argument?
isVisibleBinder :: TyBinder -> Bool
isVisibleBinder = not . isInvisibleBinder


{- Note [TyBinders]
~~~~~~~~~~~~~~~~~~~
A ForAllTy contains a TyVarBinder.  But a type can be decomposed
to a telescope consisting of a [TyBinder]

A TyBinder represents the type of binders -- that is, the type of an
argument to a Pi-type. GHC Core currently supports two different
Pi-types:

 * A non-dependent function type,
   written with ->, e.g. ty1 -> ty2
   represented as FunTy ty1 ty2. These are
   lifted to Coercions with the corresponding FunCo.

 * A dependent compile-time-only polytype,
   written with forall, e.g.  forall (a:*). ty
   represented as ForAllTy (TvBndr a v) ty

Both Pi-types classify terms/types that take an argument. In other
words, if `x` is either a function or a polytype, `x arg` makes sense
(for an appropriate `arg`).


Note [TyVarBndrs, TyVarBinders, TyConBinders, and visiblity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* A ForAllTy (used for both types and kinds) contains a TyVarBinder.
  Each TyVarBinder
      TvBndr a tvis
  is equipped with tvis::ArgFlag, which says whether or not arguments
  for this binder should be visible (explicit) in source Haskell.

* A TyCon contains a list of TyConBinders.  Each TyConBinder
      TvBndr a cvis
  is equipped with cvis::TyConBndrVis, which says whether or not type
  and kind arguments for this TyCon should be visible (explicit) in
  source Haskell.

This table summarises the visiblity rules:
---------------------------------------------------------------------------------------
|                                                      Occurrences look like this
|                             GHC displays type as     in Haskell souce code
|-----------------------------------------------------------------------
| TvBndr a tvis :: TyVarBinder, in the binder of ForAllTy for a term
|  tvis :: ArgFlag
|  tvis = Inferred:            f :: forall {a}. type    Arg not allowed:  f
|  tvis = Specified:           f :: forall a. type      Arg optional:     f  or  f @Int
|  tvis = Required:   Illegal: See Note [No Required TyBinder in terms]
|
| TvBndr k cvis :: TyConBinder, in the TyConBinders of a TyCon
|  cvis :: TyConBndrVis
|  cvis = AnonTCB:             T :: kind -> kind        Required:            T *
|  cvis = NamedTCB Inferred:   T :: forall {k}. kind    Arg not allowed:     T
|  cvis = NamedTCB Specified:  T :: forall k. kind      Arg not allowed[1]:  T
|  cvis = NamedTCB Required:   T :: forall k -> kind    Required:            T *
---------------------------------------------------------------------------------------

[1] In types, in the Specified case, it would make sense to allow
    optional kind applications, thus (T @*), but we have not
    yet implemented that

---- Examples of where the different visibilities come from -----

In term declarations:

* Inferred.  Function defn, with no signature:  f1 x = x
  We infer f1 :: forall {a}. a -> a, with 'a' Inferred
  It's Inferred because it doesn't appear in any
  user-written signature for f1

* Specified.  Function defn, with signature (implicit forall):
     f2 :: a -> a; f2 x = x
  So f2 gets the type f2 :: forall a. a->a, with 'a' Specified
  even though 'a' is not bound in the source code by an explicit forall

* Specified.  Function defn, with signature (explicit forall):
     f3 :: forall a. a -> a; f3 x = x
  So f3 gets the type f3 :: forall a. a->a, with 'a' Specified

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

In type declarations:

* Inferred (k)
     data T1 a b = MkT1 (a b)
  Here T1's kind is  T1 :: forall {k:*}. (k->*) -> k -> *
  The kind variable 'k' is Inferred, since it is not mentioned

  Note that 'a' and 'b' correspond to /Anon/ TyBinders in T1's kind,
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

Note [No Required TyBinder in terms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't allow Required foralls for term variables, including pattern
synonyms and data constructors.  Why?  Because then an application
would need a /compulsory/ type argument (possibly without an "@"?),
thus (f Int); and we don't have concrete syntax for that.

We could change this decision, but Required, Named TyBinders are rare
anyway.  (Most are Anons.)
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

-- named with "Only" to prevent naive use of mkTyVarTy
mkTyVarTy  :: TyVar   -> Type
mkTyVarTy v = ASSERT2( isTyVar v, ppr v <+> dcolon <+> ppr (tyVarKind v) )
                  TyVarTy v

mkTyVarTys :: [TyVar] -> [Type]
mkTyVarTys = map mkTyVarTy -- a common use of mkTyVarTy

infixr 3 `mkFunTy`      -- Associates to the right
-- | Make an arrow type
mkFunTy :: Type -> Type -> Type
mkFunTy arg res = FunTy arg res

-- | Make nested arrow types
mkFunTys :: [Type] -> Type -> Type
mkFunTys tys ty = foldr mkFunTy ty tys

mkForAllTy :: TyVar -> ArgFlag -> Type -> Type
mkForAllTy tv vis ty = ForAllTy (TvBndr tv vis) ty

-- | Wraps foralls over the type using the provided 'TyVar's from left to right
mkForAllTys :: [TyVarBinder] -> Type -> Type
mkForAllTys tyvars ty = foldr ForAllTy ty tyvars

mkPiTy :: TyBinder -> Type -> Type
mkPiTy (Anon ty1) ty2 = FunTy ty1 ty2
mkPiTy (Named tvb) ty = ForAllTy tvb ty

mkPiTys :: [TyBinder] -> Type -> Type
mkPiTys tbs ty = foldr mkPiTy ty tbs

-- | Does this type classify a core (unlifted) Coercion?
-- At either role nominal or representational
--    (t1 ~# t2) or (t1 ~R# t2)
isCoercionType :: Type -> Bool
isCoercionType (TyConApp tc tys)
  | (tc `hasKey` eqPrimTyConKey) || (tc `hasKey` eqReprPrimTyConKey)
  , tys `lengthIs` 4
  = True
isCoercionType _ = False


-- | Create the plain type constructor type which has been applied to no type arguments at all.
mkTyConTy :: TyCon -> Type
mkTyConTy tycon = TyConApp tycon []

{-
Some basic functions, put here to break loops eg with the pretty printer
-}

is_TYPE :: (   Type    -- the single argument to TYPE; not a synonym
            -> Bool )  -- what to return
        -> Kind -> Bool
is_TYPE f ki | Just ki' <- coreView ki = is_TYPE f ki'
is_TYPE f (TyConApp tc [arg])
  | tc `hasKey` tYPETyConKey
  = go arg
    where
      go ty | Just ty' <- coreView ty = go ty'
      go ty = f ty
is_TYPE _ _ = False

-- | This version considers Constraint to be distinct from *. Returns True
-- if the argument is equivalent to Type and False otherwise.
isLiftedTypeKind :: Kind -> Bool
isLiftedTypeKind = is_TYPE is_lifted
  where
    is_lifted (TyConApp lifted_rep []) = lifted_rep `hasKey` liftedRepDataConKey
    is_lifted _                        = False

-- | Returns True if the kind classifies unlifted types and False otherwise.
-- Note that this returns False for levity-polymorphic kinds, which may
-- be specialized to a kind that classifies unlifted types.
isUnliftedTypeKind :: Kind -> Bool
isUnliftedTypeKind = is_TYPE is_unlifted
  where
    is_unlifted (TyConApp rr _args) = not (rr `hasKey` liftedRepDataConKey)
    is_unlifted _                   = False

-- | Is this the type 'RuntimeRep'?
isRuntimeRepTy :: Type -> Bool
isRuntimeRepTy ty | Just ty' <- coreView ty = isRuntimeRepTy ty'
isRuntimeRepTy (TyConApp tc []) = tc `hasKey` runtimeRepTyConKey
isRuntimeRepTy _ = False

-- | Is this a type of kind RuntimeRep? (e.g. LiftedRep)
isRuntimeRepKindedTy :: Type -> Bool
isRuntimeRepKindedTy = isRuntimeRepTy . typeKind

-- | Is a tyvar of type 'RuntimeRep'?
isRuntimeRepVar :: TyVar -> Bool
isRuntimeRepVar = isRuntimeRepTy . tyVarKind

-- | Drops prefix of RuntimeRep constructors in 'TyConApp's. Useful for e.g.
-- dropping 'LiftedRep arguments of unboxed tuple TyCon applications:
--
--   dropRuntimeRepArgs [ 'LiftedRep, 'IntRep
--                      , String, Int# ] == [String, Int#]
--
dropRuntimeRepArgs :: [Type] -> [Type]
dropRuntimeRepArgs = dropWhile isRuntimeRepKindedTy

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
  = -- Refl :: "e" -> _ -> e
    Refl Role Type  -- See Note [Refl invariant]
          -- Invariant: applications of (Refl T) to a bunch of identity coercions
          --            always show up as Refl.
          -- For example  (Refl T) (Refl a) (Refl b) shows up as (Refl (T a b)).

          -- Applications of (Refl T) to some coercions, at least one of
          -- which is NOT the identity, show up as TyConAppCo.
          -- (They may not be fully saturated however.)
          -- ConAppCo coercions (like all coercions other than Refl)
          -- are NEVER the identity.

          -- Use (Refl Representational _), not (SubCo (Refl Nominal _))

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
  | ForAllCo TyVar KindCoercion Coercion
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

  | UnivCo UnivCoProvenance Role Type Type
      -- :: _ -> "e" -> _ -> _ -> e

  | SymCo Coercion             -- :: e -> e
  | TransCo Coercion Coercion  -- :: e -> e -> e

    -- The number coercions should match exactly the expectations
    -- of the CoAxiomRule (i.e., the rule is fully saturated).
  | AxiomRuleCo CoAxiomRule [Coercion]

  | NthCo  Int         Coercion     -- Zero-indexed; decomposes (T t0 ... tn)
    -- :: _ -> e -> ?? (inverse of TyConAppCo, see Note [TyConAppCo roles])
    -- Using NthCo on a ForAllCo gives an N coercion always
    -- See Note [NthCo and newtypes]

  | LRCo   LeftOrRight CoercionN     -- Decomposes (t_left t_right)
    -- :: _ -> N -> N
  | InstCo Coercion CoercionN
    -- :: e -> N -> e
    -- See Note [InstCo roles]

  -- Coherence applies a coercion to the left-hand type of another coercion
  -- See Note [Coherence]
  | CoherenceCo Coercion KindCoercion
     -- :: e -> N -> e

  -- Extract a kind coercion from a (heterogeneous) type coercion
  -- NB: all kind coercions are Nominal
  | KindCo Coercion
     -- :: e -> N

  | SubCo CoercionN                  -- Turns a ~N into a ~R
    -- :: N -> R

  deriving Data.Data

type CoercionN = Coercion       -- always nominal
type CoercionR = Coercion       -- always representational
type CoercionP = Coercion       -- always phantom
type KindCoercion = CoercionN   -- always nominal

{-
Note [Refl invariant]
~~~~~~~~~~~~~~~~~~~~~
Invariant 1:

Coercions have the following invariant
     Refl is always lifted as far as possible.

You might think that a consequencs is:
     Every identity coercions has Refl at the root

But that's not quite true because of coercion variables.  Consider
     g         where g :: Int~Int
     Left h    where h :: Maybe Int ~ Maybe Int
etc.  So the consequence is only true of coercions that
have no coercion variables.

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

First, the TyVar stored in a ForAllCo is really an optimisation: this field
should be a Name, as its kind is redundant. Thinking of the field as a Name
is helpful in understanding what a ForAllCo means.

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

Note [Coherence]
~~~~~~~~~~~~~~~~
The Coherence typing rule is thus:

  g1 : s ~ t    s : k1    g2 : k1 ~ k2
  ------------------------------------
  CoherenceCo g1 g2 : (s |> g2) ~ t

While this looks (and is) unsymmetric, a combination of other coercion
combinators can make the symmetric version.

For role information, see Note [Roles and kind coercions].

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

  NthCo 0 co :: forall a b. a ~R b

Yikes! Clearly, this is terrible. The solution is simple: forbid
NthCo to be used on newtypes if the internal coercion is representational.

This is not just some corner case discovered by a segfault somewhere;
it was discovered in the proof of soundness of roles and described
in the "Safe Coercions" paper (ICFP '14).

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

  | HoleProv CoercionHole  -- ^ See Note [Coercion holes]
  deriving Data.Data

instance Outputable UnivCoProvenance where
  ppr UnsafeCoerceProv   = text "(unsafeCoerce#)"
  ppr (PhantomProv _)    = text "(phantom)"
  ppr (ProofIrrelProv _) = text "(proof irrel.)"
  ppr (PluginProv str)   = parens (text "plugin" <+> brackets (text str))
  ppr (HoleProv hole)    = parens (text "hole" <> ppr hole)

-- | A coercion to be filled in by the type-checker. See Note [Coercion holes]
data CoercionHole
  = CoercionHole { chUnique   :: Unique   -- ^ used only for debugging
                 , chCoercion :: IORef (Maybe Coercion)
                 }

instance Data.Data CoercionHole where
  -- don't traverse?
  toConstr _   = abstractConstr "CoercionHole"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "CoercionHole"

instance Outputable CoercionHole where
  ppr (CoercionHole u _) = braces (ppr u)


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
  - For boxed equality constraints, (t1 ~N t2) and (t1 ~R t2), it's just
    like type classes above. (Indeed, boxed equality constraints *are* classes.)
  - But for /unboxed/ equality constraints (t1 ~R# t2) and (t1 ~N# t2)
    we use a different plan

For unboxed equalities:
  - Generate a CoercionHole, a mutable variable just like a unification
    variable
  - Wrap the CoercionHole in a Wanted constraint; see TcRnTypes.TcEvDest
  - Use the CoercionHole in a Coercion, via HoleProv
  - Solve the constraint later
  - When solved, fill in the CoercionHole by side effect, instead of
    doing the let-binding thing

The main reason for all this is that there may be no good place to let-bind
the evidence for unboxed equalities:
  - We emit constraints for kind coercions, to be used
    to cast a type's kind. These coercions then must be used in types. Because
    they might appear in a top-level type, there is no place to bind these
   (unlifted) coercions in the usual way.

  - A coercion for (forall a. t1) ~ forall a. t2) will look like
       forall a. (coercion for t1~t2)
    But the coercion for (t1~t2) may mention 'a', and we don't have let-bindings
    within coercions.  We could add them, but coercion holes are easier.

Other notes about HoleCo:

 * INVARIANT: CoercionHole and HoleProv are used only during type checking,
   and should never appear in Core. Just like unification variables; a Type
   can contain a TcTyVar, but only during type checking. If, one day, we
   use type-level information to separate out forms that can appear during
   type-checking vs forms that can appear in core proper, holes in Core will
   be ruled out.

 * The Unique carried with a coercion hole is used solely for debugging.

 * Coercion holes can be compared for equality only like other coercions:
   only by looking at the types coerced.

 * We don't use holes for other evidence because other evidence wants to
   be /shared/. But coercions are entirely erased, so there's little
   benefit to sharing.

Note [ProofIrrelProv]
~~~~~~~~~~~~~~~~~~~~~
A ProofIrrelProv is a coercion between coercions. For example:

  data G a where
    MkG :: G Bool

In core, we get

  G :: * -> *
  MkG :: forall (a :: *). (a ~ Bool) -> G a

Now, consider 'MkG -- that is, MkG used in a type -- and suppose we want
a proof that ('MkG co1 a1) ~ ('MkG co2 a2). This will have to be

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
    co5 = TyConAppCo Nominal (~) [<*>, <*>, co4, <Bool>]


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
-}


-- | Returns free variables of a type, including kind variables as
-- a non-deterministic set. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfType :: Type -> TyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfType ty = fvVarSet $ tyCoFVsOfType ty

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
tyCoFVsOfType (TyVarTy v)        a b c = (unitFV v `unionFV` tyCoFVsOfType (tyVarKind v)) a b c
tyCoFVsOfType (TyConApp _ tys)   a b c = tyCoFVsOfTypes tys a b c
tyCoFVsOfType (LitTy {})         a b c = emptyFV a b c
tyCoFVsOfType (AppTy fun arg)    a b c = (tyCoFVsOfType fun `unionFV` tyCoFVsOfType arg) a b c
tyCoFVsOfType (FunTy arg res)    a b c = (tyCoFVsOfType arg `unionFV` tyCoFVsOfType res) a b c
tyCoFVsOfType (ForAllTy bndr ty) a b c = tyCoFVsBndr bndr (tyCoFVsOfType ty)  a b c
tyCoFVsOfType (CastTy ty co)     a b c = (tyCoFVsOfType ty `unionFV` tyCoFVsOfCo co) a b c
tyCoFVsOfType (CoercionTy co)    a b c = tyCoFVsOfCo co a b c

tyCoFVsBndr :: TyVarBinder -> FV -> FV
-- Free vars of (forall b. <thing with fvs>)
tyCoFVsBndr (TvBndr tv _) fvs = (delFV tv fvs)
                                `unionFV` tyCoFVsOfType (tyVarKind tv)

-- | Returns free variables of types, including kind variables as
-- a non-deterministic set. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypes :: [Type] -> TyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfTypes tys = fvVarSet $ tyCoFVsOfTypes tys

-- | Returns free variables of types, including kind variables as
-- a non-deterministic set. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypesSet :: TyVarEnv Type -> TyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfTypesSet tys = fvVarSet $ tyCoFVsOfTypes $ nonDetEltsUFM tys
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

tyCoFVsOfTypes :: [Type] -> FV
-- See Note [Free variables of types]
tyCoFVsOfTypes (ty:tys) fv_cand in_scope acc = (tyCoFVsOfType ty `unionFV` tyCoFVsOfTypes tys) fv_cand in_scope acc
tyCoFVsOfTypes []       fv_cand in_scope acc = emptyFV fv_cand in_scope acc

tyCoVarsOfCo :: Coercion -> TyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfCo co = fvVarSet $ tyCoFVsOfCo co

-- | Get a deterministic set of the vars free in a coercion
tyCoVarsOfCoDSet :: Coercion -> DTyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfCoDSet co = fvDVarSet $ tyCoFVsOfCo co

tyCoVarsOfCoList :: Coercion -> [TyCoVar]
-- See Note [Free variables of types]
tyCoVarsOfCoList co = fvVarList $ tyCoFVsOfCo co

tyCoFVsOfCo :: Coercion -> FV
-- Extracts type and coercion variables from a coercion
-- See Note [Free variables of types]
tyCoFVsOfCo (Refl _ ty)         fv_cand in_scope acc = tyCoFVsOfType ty fv_cand in_scope acc
tyCoFVsOfCo (TyConAppCo _ _ cos) fv_cand in_scope acc = tyCoFVsOfCos cos fv_cand in_scope acc
tyCoFVsOfCo (AppCo co arg) fv_cand in_scope acc
  = (tyCoFVsOfCo co `unionFV` tyCoFVsOfCo arg) fv_cand in_scope acc
tyCoFVsOfCo (ForAllCo tv kind_co co) fv_cand in_scope acc
  = (delFV tv (tyCoFVsOfCo co) `unionFV` tyCoFVsOfCo kind_co) fv_cand in_scope acc
tyCoFVsOfCo (FunCo _ co1 co2)    fv_cand in_scope acc
  = (tyCoFVsOfCo co1 `unionFV` tyCoFVsOfCo co2) fv_cand in_scope acc
tyCoFVsOfCo (CoVarCo v) fv_cand in_scope acc
  = (unitFV v `unionFV` tyCoFVsOfType (varType v)) fv_cand in_scope acc
tyCoFVsOfCo (AxiomInstCo _ _ cos) fv_cand in_scope acc = tyCoFVsOfCos cos fv_cand in_scope acc
tyCoFVsOfCo (UnivCo p _ t1 t2) fv_cand in_scope acc
  = (tyCoFVsOfProv p `unionFV` tyCoFVsOfType t1
                         `unionFV` tyCoFVsOfType t2) fv_cand in_scope acc
tyCoFVsOfCo (SymCo co)          fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (TransCo co1 co2)   fv_cand in_scope acc = (tyCoFVsOfCo co1 `unionFV` tyCoFVsOfCo co2) fv_cand in_scope acc
tyCoFVsOfCo (NthCo _ co)        fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (LRCo _ co)         fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (InstCo co arg)     fv_cand in_scope acc = (tyCoFVsOfCo co `unionFV` tyCoFVsOfCo arg) fv_cand in_scope acc
tyCoFVsOfCo (CoherenceCo c1 c2) fv_cand in_scope acc = (tyCoFVsOfCo c1 `unionFV` tyCoFVsOfCo c2) fv_cand in_scope acc
tyCoFVsOfCo (KindCo co)         fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (SubCo co)          fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (AxiomRuleCo _ cs)  fv_cand in_scope acc = tyCoFVsOfCos cs fv_cand in_scope acc

tyCoVarsOfProv :: UnivCoProvenance -> TyCoVarSet
tyCoVarsOfProv prov = fvVarSet $ tyCoFVsOfProv prov

tyCoFVsOfProv :: UnivCoProvenance -> FV
tyCoFVsOfProv UnsafeCoerceProv    fv_cand in_scope acc = emptyFV fv_cand in_scope acc
tyCoFVsOfProv (PhantomProv co)    fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfProv (ProofIrrelProv co) fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfProv (PluginProv _)      fv_cand in_scope acc = emptyFV fv_cand in_scope acc
tyCoFVsOfProv (HoleProv _)        fv_cand in_scope acc = emptyFV fv_cand in_scope acc

tyCoVarsOfCos :: [Coercion] -> TyCoVarSet
tyCoVarsOfCos cos = fvVarSet $ tyCoFVsOfCos cos

tyCoVarsOfCosSet :: CoVarEnv Coercion -> TyCoVarSet
tyCoVarsOfCosSet cos = fvVarSet $ tyCoFVsOfCos $ nonDetEltsUFM cos
  -- It's OK to use nonDetEltsUFM here because we immediately forget the
  -- ordering by returning a set

tyCoFVsOfCos :: [Coercion] -> FV
tyCoFVsOfCos []       fv_cand in_scope acc = emptyFV fv_cand in_scope acc
tyCoFVsOfCos (co:cos) fv_cand in_scope acc = (tyCoFVsOfCo co `unionFV` tyCoFVsOfCos cos) fv_cand in_scope acc

coVarsOfType :: Type -> CoVarSet
coVarsOfType (TyVarTy v)         = coVarsOfType (tyVarKind v)
coVarsOfType (TyConApp _ tys)    = coVarsOfTypes tys
coVarsOfType (LitTy {})          = emptyVarSet
coVarsOfType (AppTy fun arg)     = coVarsOfType fun `unionVarSet` coVarsOfType arg
coVarsOfType (FunTy arg res)     = coVarsOfType arg `unionVarSet` coVarsOfType res
coVarsOfType (ForAllTy (TvBndr tv _) ty)
  = (coVarsOfType ty `delVarSet` tv)
    `unionVarSet` coVarsOfType (tyVarKind tv)
coVarsOfType (CastTy ty co)      = coVarsOfType ty `unionVarSet` coVarsOfCo co
coVarsOfType (CoercionTy co)     = coVarsOfCo co

coVarsOfTypes :: [Type] -> TyCoVarSet
coVarsOfTypes tys = mapUnionVarSet coVarsOfType tys

coVarsOfCo :: Coercion -> CoVarSet
-- Extract *coercion* variables only.  Tiresome to repeat the code, but easy.
coVarsOfCo (Refl _ ty)         = coVarsOfType ty
coVarsOfCo (TyConAppCo _ _ args) = coVarsOfCos args
coVarsOfCo (AppCo co arg)      = coVarsOfCo co `unionVarSet` coVarsOfCo arg
coVarsOfCo (ForAllCo tv kind_co co)
  = coVarsOfCo co `delVarSet` tv `unionVarSet` coVarsOfCo kind_co
coVarsOfCo (FunCo _ co1 co2)   = coVarsOfCo co1 `unionVarSet` coVarsOfCo co2
coVarsOfCo (CoVarCo v)         = unitVarSet v `unionVarSet` coVarsOfType (varType v)
coVarsOfCo (AxiomInstCo _ _ args) = coVarsOfCos args
coVarsOfCo (UnivCo p _ t1 t2)  = coVarsOfProv p `unionVarSet` coVarsOfTypes [t1, t2]
coVarsOfCo (SymCo co)          = coVarsOfCo co
coVarsOfCo (TransCo co1 co2)   = coVarsOfCo co1 `unionVarSet` coVarsOfCo co2
coVarsOfCo (NthCo _ co)        = coVarsOfCo co
coVarsOfCo (LRCo _ co)         = coVarsOfCo co
coVarsOfCo (InstCo co arg)     = coVarsOfCo co `unionVarSet` coVarsOfCo arg
coVarsOfCo (CoherenceCo c1 c2) = coVarsOfCos [c1, c2]
coVarsOfCo (KindCo co)         = coVarsOfCo co
coVarsOfCo (SubCo co)          = coVarsOfCo co
coVarsOfCo (AxiomRuleCo _ cs)  = coVarsOfCos cs

coVarsOfProv :: UnivCoProvenance -> CoVarSet
coVarsOfProv UnsafeCoerceProv    = emptyVarSet
coVarsOfProv (PhantomProv co)    = coVarsOfCo co
coVarsOfProv (ProofIrrelProv co) = coVarsOfCo co
coVarsOfProv (PluginProv _)      = emptyVarSet
coVarsOfProv (HoleProv _)        = emptyVarSet

coVarsOfCos :: [Coercion] -> CoVarSet
coVarsOfCos cos = mapUnionVarSet coVarsOfCo cos

-- | Add the kind variables free in the kinds of the tyvars in the given set.
-- Returns a non-deterministic set.
closeOverKinds :: TyVarSet -> TyVarSet
closeOverKinds = fvVarSet . closeOverKindsFV . nonDetEltsUniqSet
  -- It's OK to use nonDetEltsUniqSet here because we immediately forget
  -- about the ordering by returning a set.

-- | Given a list of tyvars returns a deterministic FV computation that
-- returns the given tyvars with the kind variables free in the kinds of the
-- given tyvars.
closeOverKindsFV :: [TyVar] -> FV
closeOverKindsFV tvs =
  mapUnionFV (tyCoFVsOfType . tyVarKind) tvs `unionFV` mkFVs tvs

-- | Add the kind variables free in the kinds of the tyvars in the given set.
-- Returns a deterministically ordered list.
closeOverKindsList :: [TyVar] -> [TyVar]
closeOverKindsList tvs = fvVarList $ closeOverKindsFV tvs

-- | Add the kind variables free in the kinds of the tyvars in the given set.
-- Returns a deterministic set.
closeOverKindsDSet :: DTyVarSet -> DTyVarSet
closeOverKindsDSet = fvDVarSet . closeOverKindsFV . dVarSetElems

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

-- | Returns True if this coercion has no free variables. Should be the same as
-- isEmptyVarSet . tyCoVarsOfCo, but faster in the non-forall case.
noFreeVarsOfCo :: Coercion -> Bool
noFreeVarsOfCo (Refl _ ty)            = noFreeVarsOfType ty
noFreeVarsOfCo (TyConAppCo _ _ args)  = all noFreeVarsOfCo args
noFreeVarsOfCo (AppCo c1 c2)          = noFreeVarsOfCo c1 && noFreeVarsOfCo c2
noFreeVarsOfCo co@(ForAllCo {})       = isEmptyVarSet (tyCoVarsOfCo co)
noFreeVarsOfCo (FunCo _ c1 c2)        = noFreeVarsOfCo c1 && noFreeVarsOfCo c2
noFreeVarsOfCo (CoVarCo _)            = False
noFreeVarsOfCo (AxiomInstCo _ _ args) = all noFreeVarsOfCo args
noFreeVarsOfCo (UnivCo p _ t1 t2)     = noFreeVarsOfProv p &&
                                        noFreeVarsOfType t1 &&
                                        noFreeVarsOfType t2
noFreeVarsOfCo (SymCo co)             = noFreeVarsOfCo co
noFreeVarsOfCo (TransCo co1 co2)      = noFreeVarsOfCo co1 && noFreeVarsOfCo co2
noFreeVarsOfCo (NthCo _ co)           = noFreeVarsOfCo co
noFreeVarsOfCo (LRCo _ co)            = noFreeVarsOfCo co
noFreeVarsOfCo (InstCo co1 co2)       = noFreeVarsOfCo co1 && noFreeVarsOfCo co2
noFreeVarsOfCo (CoherenceCo co1 co2)  = noFreeVarsOfCo co1 && noFreeVarsOfCo co2
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
noFreeVarsOfProv (HoleProv {})       = True -- matches with coVarsOfProv, but I'm unsure

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
        -- See Note [Apply Once]
        -- and Note [Extending the TvSubstEnv]
        -- and Note [Substituting types and coercions]
        -- and Note [The substitution invariant]

-- | A substitution of 'Type's for 'TyVar's
--                 and 'Kind's for 'KindVar's
type TvSubstEnv = TyVarEnv Type
        -- A TvSubstEnv is used both inside a TCvSubst (with the apply-once
        -- invariant discussed in Note [Apply Once]), and also independently
        -- in the middle of matching, and unification (see Types.Unify)
        -- So you have to look at the context to know if it's idempotent or
        -- apply-once or whatever

-- | A substitution of 'Coercion's for 'CoVar's
type CvSubstEnv = CoVarEnv Coercion

{-
Note [Apply Once]
~~~~~~~~~~~~~~~~~
We use TCvSubsts to instantiate things, and we might instantiate
        forall a b. ty
\with the types
        [a, b], or [b, a].
So the substitution might go [a->b, b->a].  A similar situation arises in Core
when we find a beta redex like
        (/\ a /\ b -> e) b a
Then we also end up with a substitution that permutes type variables. Other
variations happen to; for example [a -> (a, b)].

        ****************************************************
        *** So a TCvSubst must be applied precisely once ***
        ****************************************************

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

* In substTyVarBndr, we need extend the TvSubstEnv
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

Note [The substitution invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When calling (substTy subst ty) it should be the case that
the in-scope set in the substitution is a superset of both:

  * The free vars of the range of the substitution
  * The free vars of ty minus the domain of the substitution

If we want to substitute [a -> ty1, b -> ty2] I used to
think it was enough to generate an in-scope set that includes
fv(ty1,ty2).  But that's not enough; we really should also take the
free vars of the type we are substituting into!  Example:
     (forall b. (a,b,x)) [a -> List b]
Then if we use the in-scope set {b}, there is a danger we will rename
the forall'd variable to 'x' by mistake, getting this:
     (forall x. (List b, x, x))

Breaking this invariant caused the bug from #11371.
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

extendTvSubst :: TCvSubst -> TyVar -> Type -> TCvSubst
extendTvSubst (TCvSubst in_scope tenv cenv) tv ty
  = TCvSubst in_scope (extendVarEnv tenv tv ty) cenv

extendTvSubstBinderAndInScope :: TCvSubst -> TyBinder -> Type -> TCvSubst
extendTvSubstBinderAndInScope subst (Named bndr) ty
  = extendTvSubstAndInScope subst (binderVar bndr) ty
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

-- | Generates an in-scope set from the free variables in a list of types
-- and a list of coercions
mkTyCoInScopeSet :: [Type] -> [Coercion] -> InScopeSet
mkTyCoInScopeSet tys cos
  = mkInScopeSet (tyCoVarsOfTypes tys `unionVarSet` tyCoVarsOfCos cos)

-- | Generates the in-scope set for the 'TCvSubst' from the types in the incoming
-- environment. No CoVars, please!
zipTvSubst :: [TyVar] -> [Type] -> TCvSubst
zipTvSubst tvs tys
  | debugIsOn
  , not (all isTyVar tvs) || neLength tvs tys
  = pprTrace "zipTvSubst" (ppr tvs $$ ppr tys) emptyTCvSubst
  | otherwise
  = mkTvSubst (mkInScopeSet (tyCoVarsOfTypes tys)) tenv
  where
    tenv = zipTyEnv tvs tys

-- | Generates the in-scope set for the 'TCvSubst' from the types in the incoming
-- environment.  No TyVars, please!
zipCvSubst :: [CoVar] -> [Coercion] -> TCvSubst
zipCvSubst cvs cos
  | debugIsOn
  , not (all isCoVar cvs) || neLength cvs cos
  = pprTrace "zipCvSubst" (ppr cvs $$ ppr cos) emptyTCvSubst
  | otherwise
  = TCvSubst (mkInScopeSet (tyCoVarsOfCos cos)) emptyTvSubstEnv cenv
  where
    cenv = zipCoEnv cvs cos

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

zipTyEnv :: [TyVar] -> [Type] -> TvSubstEnv
zipTyEnv tyvars tys
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

zipCoEnv :: [CoVar] -> [Coercion] -> CvSubstEnv
zipCoEnv cvs cos = mkVarEnv (zipEqual "zipCoEnv" cvs cos)

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

-}

-- | Type substitution, see 'zipTvSubst'
substTyWith :: HasCallStack => [TyVar] -> [Type] -> Type -> Type
-- Works only if the domain of the substitution is a
-- superset of the type being substituted into
substTyWith tvs tys = ASSERT( tvs `equalLength` tys )
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
  = ASSERT2( isValidTCvSubst subst,
             text "in_scope" <+> ppr in_scope $$
             text "tenv" <+> ppr tenv $$
             text "tenvFVs"
               <+> ppr (tyCoVarsOfTypesSet tenv) $$
             text "cenv" <+> ppr cenv $$
             text "cenvFVs"
               <+> ppr (tyCoVarsOfCosSet cenv) $$
             text "tys" <+> ppr tys $$
             text "cos" <+> ppr cos )
    ASSERT2( tysCosFVsInScope,
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
    go (ForAllTy (TvBndr tv vis) ty)
                         = case substTyVarBndrUnchecked subst tv of
                             (subst', tv') ->
                               (ForAllTy $! ((TvBndr $! tv') vis)) $!
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

    go :: Coercion -> Coercion
    go (Refl r ty)           = mkReflCo r $! go_ty ty
    go (TyConAppCo r tc args)= let args' = map go args
                               in  args' `seqList` mkTyConAppCo r tc args'
    go (AppCo co arg)        = (mkAppCo $! go co) $! go arg
    go (ForAllCo tv kind_co co)
      = case substForAllCoBndrUnchecked subst tv kind_co of { (subst', tv', kind_co') ->
          ((mkForAllCo $! tv') $! kind_co') $! subst_co subst' co }
    go (FunCo r co1 co2)     = (mkFunCo r $! go co1) $! go co2
    go (CoVarCo cv)          = substCoVar subst cv
    go (AxiomInstCo con ind cos) = mkAxiomInstCo con ind $! map go cos
    go (UnivCo p r t1 t2)    = (((mkUnivCo $! go_prov p) $! r) $!
                                (go_ty t1)) $! (go_ty t2)
    go (SymCo co)            = mkSymCo $! (go co)
    go (TransCo co1 co2)     = (mkTransCo $! (go co1)) $! (go co2)
    go (NthCo d co)          = mkNthCo d $! (go co)
    go (LRCo lr co)          = mkLRCo lr $! (go co)
    go (InstCo co arg)       = (mkInstCo $! (go co)) $! go arg
    go (CoherenceCo co1 co2) = (mkCoherenceCo $! (go co1)) $! (go co2)
    go (KindCo co)           = mkKindCo $! (go co)
    go (SubCo co)            = mkSubCo $! (go co)
    go (AxiomRuleCo c cs)    = let cs1 = map go cs
                                in cs1 `seqList` AxiomRuleCo c cs1

    go_prov UnsafeCoerceProv     = UnsafeCoerceProv
    go_prov (PhantomProv kco)    = PhantomProv (go kco)
    go_prov (ProofIrrelProv kco) = ProofIrrelProv (go kco)
    go_prov p@(PluginProv _)     = p
    go_prov p@(HoleProv _)       = p
      -- NB: this last case is a little suspicious, but we need it. Originally,
      -- there was a panic here, but it triggered from deeplySkolemise. Because
      -- we only skolemise tyvars that are manually bound, this operation makes
      -- sense, even over a coercion with holes.

substForAllCoBndr :: TCvSubst -> TyVar -> Coercion -> (TCvSubst, TyVar, Coercion)
substForAllCoBndr subst
  = substForAllCoBndrCallback False (substCo subst) subst

-- | Like 'substForAllCoBndr', but disables sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substForAllCoBndrUnchecked :: TCvSubst -> TyVar -> Coercion -> (TCvSubst, TyVar, Coercion)
substForAllCoBndrUnchecked subst
  = substForAllCoBndrCallback False (substCoUnchecked subst) subst

-- See Note [Sym and ForAllCo]
substForAllCoBndrCallback :: Bool  -- apply sym to binder?
                          -> (Coercion -> Coercion)  -- transformation to kind co
                          -> TCvSubst -> TyVar -> Coercion
                          -> (TCvSubst, TyVar, Coercion)
substForAllCoBndrCallback sym sco (TCvSubst in_scope tenv cenv)
                          old_var old_kind_co
  = ( TCvSubst (in_scope `extendInScopeSet` new_var) new_env cenv
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

    new_var  = uniqAway in_scope (setTyVarKind old_var new_ki1)

substCoVar :: TCvSubst -> CoVar -> Coercion
substCoVar (TCvSubst _ _ cenv) cv
  = case lookupVarEnv cenv cv of
      Just co -> co
      Nothing -> CoVarCo cv

substCoVars :: TCvSubst -> [CoVar] -> [Coercion]
substCoVars subst cvs = map (substCoVar subst) cvs

lookupCoVar :: TCvSubst -> Var  -> Maybe Coercion
lookupCoVar (TCvSubst _ _ cenv) v = lookupVarEnv cenv v

substTyVarBndr :: HasCallStack => TCvSubst -> TyVar -> (TCvSubst, TyVar)
substTyVarBndr = substTyVarBndrCallback substTy

-- | Like 'substTyVarBndr' but disables sanity checks.
-- The problems that the sanity checks in substTy catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTyUnchecked to
-- substTy and remove this function. Please don't use in new code.
substTyVarBndrUnchecked :: TCvSubst -> TyVar -> (TCvSubst, TyVar)
substTyVarBndrUnchecked = substTyVarBndrCallback substTyUnchecked

-- | Substitute a tyvar in a binding position, returning an
-- extended subst and a new tyvar.
substTyVarBndrCallback :: (TCvSubst -> Type -> Type)  -- ^ the subst function
                       -> TCvSubst -> TyVar -> (TCvSubst, TyVar)
substTyVarBndrCallback subst_fn subst@(TCvSubst in_scope tenv cenv) old_var
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

substCoVarBndr :: TCvSubst -> CoVar -> (TCvSubst, CoVar)
substCoVarBndr subst@(TCvSubst in_scope tenv cenv) old_var
  = ASSERT( isCoVar old_var )
    (TCvSubst (in_scope `extendInScopeSet` new_var) tenv new_cenv, new_var)
  where
    new_co         = mkCoVarCo new_var
    no_kind_change = all noFreeVarsOfType [t1, t2]
    no_change      = new_var == old_var && no_kind_change

    new_cenv | no_change = delVarEnv cenv old_var
             | otherwise = extendVarEnv cenv old_var new_co

    new_var = uniqAway in_scope subst_old_var
    subst_old_var = mkCoVar (varName old_var) new_var_type

    (_, _, t1, t2, role) = coVarKindsTypesRole old_var
    t1' = substTy subst t1
    t2' = substTy subst t2
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

See Note [Precedence in types] in BasicTypes.
-}

------------------

pprType, pprParendType :: Type -> SDoc
pprType       = pprPrecType TopPrec
pprParendType = pprPrecType TyConPrec

pprPrecType :: TyPrec -> Type -> SDoc
pprPrecType prec ty = getPprStyle $ \sty -> pprPrecIfaceType prec (tidyToIfaceTypeSty ty sty)

pprTyLit :: TyLit -> SDoc
pprTyLit = pprIfaceTyLit . toIfaceTyLit

pprKind, pprParendKind :: Kind -> SDoc
pprKind       = pprType
pprParendKind = pprParendType

tidyToIfaceTypeSty :: Type -> PprStyle -> IfaceType
tidyToIfaceTypeSty ty sty
  | userStyle sty = tidyToIfaceType ty
  | otherwise     = toIfaceTypeX (tyCoVarsOfType ty) ty
     -- in latter case, don't tidy, as we'll be printing uniques.

tidyToIfaceType :: Type -> IfaceType
-- It's vital to tidy before converting to an IfaceType
-- or nested binders will become indistinguishable!
--
-- Also for the free type variables, tell toIfaceTypeX to
-- leave them as IfaceFreeTyVar.  This is super-important
-- for debug printing.
tidyToIfaceType ty = toIfaceTypeX (mkVarSet free_tcvs) (tidyType env ty)
  where
    env       = tidyFreeTyCoVars emptyTidyEnv free_tcvs
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
    free_tcvs = toposortTyVars $ tyCoVarsOfCoList co

------------
pprClassPred :: Class -> [Type] -> SDoc
pprClassPred clas tys = pprTypeApp (classTyCon clas) tys

------------
pprTheta :: ThetaType -> SDoc
pprTheta = pprIfaceContext TopPrec . map tidyToIfaceType

pprParendTheta :: ThetaType -> SDoc
pprParendTheta = pprIfaceContext TyConPrec . map tidyToIfaceType

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

pprForAll :: [TyVarBinder] -> SDoc
pprForAll tvs = pprIfaceForAll (map toIfaceForAllBndr tvs)

-- | Print a user-level forall; see Note [When to print foralls]
pprUserForAll :: [TyVarBinder] -> SDoc
pprUserForAll = pprUserIfaceForAll . map toIfaceForAllBndr

pprTvBndrs :: [TyVarBinder] -> SDoc
pprTvBndrs tvs = sep (map pprTvBndr tvs)

pprTvBndr :: TyVarBinder -> SDoc
pprTvBndr = pprTyVar . binderVar

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

instance Outputable TyBinder where
  ppr (Anon ty) = text "[anon]" <+> ppr ty
  ppr (Named (TvBndr v Required))  = ppr v
  ppr (Named (TvBndr v Specified)) = char '@' <> ppr v
  ppr (Named (TvBndr v Inferred))  = braces (ppr v)

-----------------
instance Outputable Coercion where -- defined here to avoid orphans
  ppr = pprCo

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
on m and a.  The latter comes from pprTvBndr.

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
    (_univ_tvs, _ex_tvs, eq_spec, theta, arg_tys, _res_ty) = dataConFullSig dc
    univ_bndrs = dataConUnivTyVarBinders dc
    ex_bndrs   = dataConExTyVarBinders dc
    forAllDoc = pprUserForAll $ (filterEqSpec eq_spec univ_bndrs ++ ex_bndrs)
    thetaDoc  = pprThetaArrowTy theta
    argsDoc   = hsep (fmap pprParendType arg_tys)


pprTypeApp :: TyCon -> [Type] -> SDoc
pprTypeApp tc tys
  = pprIfaceTypeApp TopPrec (toIfaceTyCon tc)
                            (toIfaceTcArgs tc tys)
    -- TODO: toIfaceTcArgs seems rather wasteful here

------------------

pprPrefixApp :: TyPrec -> SDoc -> [SDoc] -> SDoc
pprPrefixApp = pprIfacePrefixApp

----------------
pprArrowChain :: TyPrec -> [SDoc] -> SDoc
-- pprArrowChain p [a,b,c]  generates   a -> b -> c
pprArrowChain _ []         = empty
pprArrowChain p (arg:args) = maybeParen p FunPrec $
                             sep [arg, sep (map (arrow <+>) args)]

ppSuggestExplicitKinds :: SDoc
-- Print a helpful suggstion about -fprint-explicit-kinds,
-- if it is not already on
ppSuggestExplicitKinds
  = sdocWithDynFlags $ \ dflags ->
    ppUnless (gopt Opt_PrintExplicitKinds dflags) $
    text "Use -fprint-explicit-kinds to see the kind arguments"

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
tidyTyCoVarBndrs :: TidyEnv -> [TyCoVar] -> (TidyEnv, [TyCoVar])
tidyTyCoVarBndrs (occ_env, subst) tvs
    = mapAccumL tidyTyCoVarBndr tidy_env' tvs
  where
    -- Seed the occ_env with clashes among the names, see
    -- Node [Tidying multiple names at once] in OccName
    -- Se still go through tidyTyCoVarBndr so that each kind variable is tidied
    -- with the correct tidy_env
    occs = map getHelpfulOccName tvs
    tidy_env' = (avoidClashesOccEnv occ_env occs, subst)

tidyTyCoVarBndr :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
tidyTyCoVarBndr tidy_env@(occ_env, subst) tyvar
  = case tidyOccName occ_env (getHelpfulOccName tyvar) of
      (occ_env', occ') -> ((occ_env', subst'), tyvar')
        where
          subst' = extendVarEnv subst tyvar tyvar'
          tyvar' = setTyVarKind (setTyVarName tyvar name') kind'
          kind'  = tidyKind tidy_env (tyVarKind tyvar)
          name'  = tidyNameOcc name occ'
          name   = tyVarName tyvar

getHelpfulOccName :: TyCoVar -> OccName
getHelpfulOccName tyvar = occ1
  where
    name = tyVarName tyvar
    occ  = getOccName name
    -- A TcTyVar with a System Name is probably a unification variable;
    -- when we tidy them we give them a trailing "0" (or 1 etc)
    -- so that they don't take precedence for the un-modified name
    -- Plus, indicating a unification variable in this way is a
    -- helpful clue for users
    occ1 | isSystemName name
         , isTcTyVar tyvar
         = mkTyVarOcc (occNameString occ ++ "0")
         | otherwise
         = occ

tidyTyVarBinder :: TidyEnv -> TyVarBndr TyVar vis
                -> (TidyEnv, TyVarBndr TyVar vis)
tidyTyVarBinder tidy_env (TvBndr tv vis)
  = (tidy_env', TvBndr tv' vis)
  where
    (tidy_env', tv') = tidyTyCoVarBndr tidy_env tv

tidyTyVarBinders :: TidyEnv -> [TyVarBndr TyVar vis]
                 -> (TidyEnv, [TyVarBndr TyVar vis])
tidyTyVarBinders = mapAccumL tidyTyVarBinder

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
-- also 'tidyTyCoVarBndr'
tidyOpenTyCoVar env@(_, subst) tyvar
  = case lookupVarEnv subst tyvar of
        Just tyvar' -> (env, tyvar')              -- Already substituted
        Nothing     ->
          let env' = tidyFreeTyCoVars env (tyCoVarsOfTypeList (tyVarKind tyvar))
          in tidyTyCoVarBndr env' tyvar  -- Treat it as a binder

---------------
tidyTyVarOcc :: TidyEnv -> TyVar -> TyVar
tidyTyVarOcc env@(_, subst) tv
  = case lookupVarEnv subst tv of
        Nothing  -> updateTyVarKind (tidyType env) tv
        Just tv' -> tv'

---------------
tidyTypes :: TidyEnv -> [Type] -> [Type]
tidyTypes env tys = map (tidyType env) tys

---------------
tidyType :: TidyEnv -> Type -> Type
tidyType _   (LitTy n)            = LitTy n
tidyType env (TyVarTy tv)         = TyVarTy (tidyTyVarOcc env tv)
tidyType env (TyConApp tycon tys) = let args = tidyTypes env tys
                                    in args `seqList` TyConApp tycon args
tidyType env (AppTy fun arg)      = (AppTy $! (tidyType env fun)) $! (tidyType env arg)
tidyType env (FunTy fun arg)      = (FunTy $! (tidyType env fun)) $! (tidyType env arg)
tidyType env (ty@(ForAllTy{}))    = mkForAllTys' (zip tvs' vis) $! tidyType env' body_ty
  where
    (tvs, vis, body_ty) = splitForAllTys' ty
    (env', tvs') = tidyTyCoVarBndrs env tvs
tidyType env (CastTy ty co)       = (CastTy $! tidyType env ty) $! (tidyCo env co)
tidyType env (CoercionTy co)      = CoercionTy $! (tidyCo env co)


-- The following two functions differ from mkForAllTys and splitForAllTys in that
-- they expect/preserve the ArgFlag argument. Thes belong to types/Type.hs, but
-- how should they be named?
mkForAllTys' :: [(TyVar, ArgFlag)] -> Type -> Type
mkForAllTys' tvvs ty = foldr strictMkForAllTy ty tvvs
  where
    strictMkForAllTy (tv,vis) ty = (ForAllTy $! ((TvBndr $! tv) $! vis)) $! ty

splitForAllTys' :: Type -> ([TyVar], [ArgFlag], Type)
splitForAllTys' ty = go ty [] []
  where
    go (ForAllTy (TvBndr tv vis) ty) tvs viss = go ty (tv:tvs) (vis:viss)
    go ty                            tvs viss = (reverse tvs, reverse viss, ty)


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
    go (Refl r ty)           = Refl r (tidyType env ty)
    go (TyConAppCo r tc cos) = let args = map go cos
                               in args `seqList` TyConAppCo r tc args
    go (AppCo co1 co2)       = (AppCo $! go co1) $! go co2
    go (ForAllCo tv h co)    = ((ForAllCo $! tvp) $! (go h)) $! (tidyCo envp co)
                               where (envp, tvp) = tidyTyCoVarBndr env tv
            -- the case above duplicates a bit of work in tidying h and the kind
            -- of tv. But the alternative is to use coercionKind, which seems worse.
    go (FunCo r co1 co2)     = (FunCo r $! go co1) $! go co2
    go (CoVarCo cv)          = case lookupVarEnv subst cv of
                                 Nothing  -> CoVarCo cv
                                 Just cv' -> CoVarCo cv'
    go (AxiomInstCo con ind cos) = let args = map go cos
                               in  args `seqList` AxiomInstCo con ind args
    go (UnivCo p r t1 t2)    = (((UnivCo $! (go_prov p)) $! r) $!
                                tidyType env t1) $! tidyType env t2
    go (SymCo co)            = SymCo $! go co
    go (TransCo co1 co2)     = (TransCo $! go co1) $! go co2
    go (NthCo d co)          = NthCo d $! go co
    go (LRCo lr co)          = LRCo lr $! go co
    go (InstCo co ty)        = (InstCo $! go co) $! go ty
    go (CoherenceCo co1 co2) = (CoherenceCo $! go co1) $! go co2
    go (KindCo co)           = KindCo $! go co
    go (SubCo co)            = SubCo $! go co
    go (AxiomRuleCo ax cos)  = let cos1 = tidyCos env cos
                               in cos1 `seqList` AxiomRuleCo ax cos1

    go_prov UnsafeCoerceProv    = UnsafeCoerceProv
    go_prov (PhantomProv co)    = PhantomProv (go co)
    go_prov (ProofIrrelProv co) = ProofIrrelProv (go co)
    go_prov p@(PluginProv _)    = p
    go_prov p@(HoleProv _)      = p

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
typeSize (ForAllTy (TvBndr tv _) t) = typeSize (tyVarKind tv) + typeSize t
typeSize (TyConApp _ ts)            = 1 + sum (map typeSize ts)
typeSize (CastTy ty co)             = typeSize ty + coercionSize co
typeSize (CoercionTy co)            = coercionSize co

coercionSize :: Coercion -> Int
coercionSize (Refl _ ty)         = typeSize ty
coercionSize (TyConAppCo _ _ args) = 1 + sum (map coercionSize args)
coercionSize (AppCo co arg)      = coercionSize co + coercionSize arg
coercionSize (ForAllCo _ h co)   = 1 + coercionSize co + coercionSize h
coercionSize (FunCo _ co1 co2)   = 1 + coercionSize co1 + coercionSize co2
coercionSize (CoVarCo _)         = 1
coercionSize (AxiomInstCo _ _ args) = 1 + sum (map coercionSize args)
coercionSize (UnivCo p _ t1 t2)  = 1 + provSize p + typeSize t1 + typeSize t2
coercionSize (SymCo co)          = 1 + coercionSize co
coercionSize (TransCo co1 co2)   = 1 + coercionSize co1 + coercionSize co2
coercionSize (NthCo _ co)        = 1 + coercionSize co
coercionSize (LRCo  _ co)        = 1 + coercionSize co
coercionSize (InstCo co arg)     = 1 + coercionSize co + coercionSize arg
coercionSize (CoherenceCo c1 c2) = 1 + coercionSize c1 + coercionSize c2
coercionSize (KindCo co)         = 1 + coercionSize co
coercionSize (SubCo co)          = 1 + coercionSize co
coercionSize (AxiomRuleCo _ cs)  = 1 + sum (map coercionSize cs)

provSize :: UnivCoProvenance -> Int
provSize UnsafeCoerceProv    = 1
provSize (PhantomProv co)    = 1 + coercionSize co
provSize (ProofIrrelProv co) = 1 + coercionSize co
provSize (PluginProv _)      = 1
provSize (HoleProv h)        = pprPanic "provSize hits a hole" (ppr h)
