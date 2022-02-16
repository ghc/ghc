{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998


This module defines interface types and binders
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
  -- FlexibleInstances for Binary (DefMethSpec IfaceType)
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- For NFData instance
{-# LANGUAGE UndecidableInstances #-}

-- | Printing Iface's Type
module GHC.Iface.Type.Ppr (

        IfaceTypePpr, IfaceKindPpr,
        IfaceContextPpr, IfacePredTypePpr, 
        IfaceCoercionPpr,IfaceMCoercionPpr,
        IfaceUnivCoProvPpr,
        IfaceMultPpr,
        IfaceAppArgsPpr,
        IfaceBndrPpr,
        IfaceTvBndrPpr, IfaceIdBndrPpr,
        IfaceTyConBinderPpr, IfaceForAllBndrPpr, IfaceForAllSpecBndrPpr,

        ShowForAllFlag(..),

        SuppressBndrSig(..),
        UseBndrParens(..),
        PrintExplicitKinds(..),
        pprIfaceType, pprParendIfaceType, pprPrecIfaceType,
        pprIfaceContext, pprIfaceContextArr,
        pprIfaceIdBndr, pprIfaceLamBndr, pprIfaceTvBndr, pprIfaceTyConBinders,
        pprIfaceBndrs, pprIfaceAppArgs, pprParendIfaceAppArgs,
        pprIfaceForAllPart, pprIfaceForAllPartMust, pprIfaceForAll,
        pprIfaceSigmaType, pprIfaceTyLit,
        pprIfaceCoercion, pprParendIfaceCoercion,
        pprIfaceTypeApp, pprUserIfaceForAll,
        pprIfaceCoTcApp, pprTyTcApp, pprIfacePrefixApp,
        ppr_fun_arrow,
        isIfaceTauType,
    ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Builtin.Types
                                 ( coercibleTyCon, heqTyCon
                                 , tupleTyConName
                                 , manyDataConTyCon, oneDataConTyCon
                                 , liftedRepTyCon, liftedDataConTyCon )
import {-# SOURCE #-} GHC.Core.Type ( isRuntimeRepTy, isMultiplicityTy, isLevityTy )

import GHC.Core.TyCon hiding ( pprPromotionQuote )
import GHC.Types.Var
import GHC.Builtin.Names
import {-# SOURCE #-} GHC.Builtin.Types ( liftedTypeKindTyConName )
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Utils.Outputable
import {-# SOURCE #-} GHC.Tc.Utils.TcType ( isMetaTyVar, isTyConableTyVar )
import GHC.Iface.Type

import Control.DeepSeq

{-
************************************************************************
*                                                                      *
                Local (nested) binders
*                                                                      *
************************************************************************
-}

type IfaceIdBndrPpr = IfaceIdBndr' TtgIfacePpr
type IfaceTvBndrPpr = IfaceTvBndr' TtgIfacePpr

type IfaceBndrPpr = IfaceBndr' TtgIfacePpr

type IfaceLamBndrPpr = (IfaceBndrPpr, IfaceOneShot)

{-
%************************************************************************
%*                                                                      *
                IfaceType
%*                                                                      *
%************************************************************************
-}

type IfaceKindPpr = IfaceKind' TtgIfacePpr
type IfaceMultPpr = IfaceMult' TtgIfacePpr

-- See Note [Free tyvars in IfaceType]
newtype IfaceFreeTyVar = IfaceFreeTyVar    TyVar

data IfaceFreeCoVar
  = IfaceFreeCoVar    CoVar    -- See Note [Free tyvars in IfaceType]
  | IfaceHoleCo       CoVar    -- ^ See Note [Holes in (IfaceCoercion' p)]

instance NFData IfaceFreeTyVar where
  rnf = \case
    IfaceFreeTyVar f1 -> f1 `seq` ()

instance NFData IfaceFreeCoVar where
  rnf = \case
    IfaceFreeCoVar f1 -> f1 `seq` ()
    IfaceHoleCo f1 -> f1 `seq` ()

data TtgIfacePpr

type instance XXIfaceType TtgIfacePpr = IfaceFreeTyVar
type IfaceTypePpr = IfaceType' TtgIfacePpr

type instance XXIfaceCoercion TtgIfacePpr = IfaceFreeCoVar

type IfacePredTypePpr = IfacePredType' TtgIfacePpr
type IfaceContextPpr = IfaceContext' TtgIfacePpr

type IfaceTyConBinderPpr = IfaceTyConBinder' TtgIfacePpr
type IfaceForAllBndrPpr  = IfaceForAllBndr' TtgIfacePpr
type IfaceForAllSpecBndrPpr = IfaceForAllSpecBndr' TtgIfacePpr

type IfaceAppArgsPpr = IfaceAppArgs' TtgIfacePpr

{- Note [Free tyvars in IfaceType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nowadays (since Nov 16, 2016) we pretty-print a Type by converting to
an IfaceType and pretty printing that.  This eliminates a lot of
pretty-print duplication, and it matches what we do with pretty-
printing TyThings. See Note [Pretty printing via Iface syntax] in GHC.Types.TyThing.Ppr.

It works fine for closed types, but when printing debug traces (e.g.
when using -ddump-tc-trace) we print a lot of /open/ types.  These
types are full of TcTyVars, and it's absolutely crucial to print them
in their full glory, with their unique, TcTyVarDetails etc.

So we simply embed a TyVar in IfaceType with the IfaceFreeTyVar constructor.
Note that:

* We never expect to serialise an IfaceFreeTyVar into an interface file, nor
  to deserialise one.  IfaceFreeTyVar is used only in the "convert to IfaceType
  and then pretty-print" pipeline.

We do the same for covars, naturally.

Note [Equality predicates in IfaceType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC has several varieties of type equality (see Note [The equality types story]
in GHC.Builtin.Types.Prim for details).  In an effort to avoid confusing users, we suppress
the differences during pretty printing unless certain flags are enabled.
Here is how each equality predicate* is printed in homogeneous and
heterogeneous contexts, depending on which combination of the
-fprint-explicit-kinds and -fprint-equality-relations flags is used:

--------------------------------------------------------------------------------------------
|         Predicate             |        Neither flag        |    -fprint-explicit-kinds   |
|-------------------------------|----------------------------|-----------------------------|
| a ~ b         (homogeneous)   |        a ~ b               | (a :: Type) ~  (b :: Type)  |
| a ~~ b,       homogeneously   |        a ~ b               | (a :: Type) ~  (b :: Type)  |
| a ~~ b,       heterogeneously |        a ~~ c              | (a :: Type) ~~ (c :: k)     |
| a ~# b,       homogeneously   |        a ~ b               | (a :: Type) ~  (b :: Type)  |
| a ~# b,       heterogeneously |        a ~~ c              | (a :: Type) ~~ (c :: k)     |
| Coercible a b (homogeneous)   |        Coercible a b       | Coercible @Type a b         |
| a ~R# b,      homogeneously   |        Coercible a b       | Coercible @Type a b         |
| a ~R# b,      heterogeneously |        a ~R# b             | (a :: Type) ~R# (c :: k)    |
|-------------------------------|----------------------------|-----------------------------|
|         Predicate             | -fprint-equality-relations |          Both flags         |
|-------------------------------|----------------------------|-----------------------------|
| a ~ b         (homogeneous)   |        a ~  b              | (a :: Type) ~  (b :: Type)  |
| a ~~ b,       homogeneously   |        a ~~ b              | (a :: Type) ~~ (b :: Type)  |
| a ~~ b,       heterogeneously |        a ~~ c              | (a :: Type) ~~ (c :: k)     |
| a ~# b,       homogeneously   |        a ~# b              | (a :: Type) ~# (b :: Type)  |
| a ~# b,       heterogeneously |        a ~# c              | (a :: Type) ~# (c :: k)     |
| Coercible a b (homogeneous)   |        Coercible a b       | Coercible @Type a b         |
| a ~R# b,      homogeneously   |        a ~R# b             | (a :: Type) ~R# (b :: Type) |
| a ~R# b,      heterogeneously |        a ~R# b             | (a :: Type) ~R# (c :: k)    |
--------------------------------------------------------------------------------------------

(* There is no heterogeneous, representational, lifted equality counterpart
to (~~). There could be, but there seems to be no use for it.)

This table adheres to the following rules:

A. With -fprint-equality-relations, print the true equality relation.
B. Without -fprint-equality-relations:
     i. If the equality is representational and homogeneous, use Coercible.
    ii. Otherwise, if the equality is representational, use ~R#.
   iii. If the equality is nominal and homogeneous, use ~.
    iv. Otherwise, if the equality is nominal, use ~~.
C. With -fprint-explicit-kinds, print kinds on both sides of an infix operator,
   as above; or print the kind with Coercible.
D. Without -fprint-explicit-kinds, don't print kinds.

A hetero-kinded equality is used homogeneously when it is applied to two
identical kinds. Unfortunately, determining this from an IfaceType isn't
possible since we can't see through type synonyms. Consequently, we need to
record whether this particular application is homogeneous in IfaceTyConSort
for the purposes of pretty-printing.

See Note [The equality types story] in GHC.Builtin.Types.Prim.
-}

type IfaceMCoercionPpr = IfaceMCoercion' TtgIfacePpr
type IfaceCoercionPpr = IfaceCoercion' TtgIfacePpr
type IfaceUnivCoProvPpr = IfaceUnivCoProv' TtgIfacePpr

{-
%************************************************************************
%*                                                                      *
                Functions over IFaceTypes
*                                                                      *
************************************************************************
-}

ifTypeIsVarFree :: IfaceTypePpr -> Bool
-- Returns True if the type definitely has no variables at all
-- Just used to control pretty printing
ifTypeIsVarFree ty = go ty
  where
    go (IfaceTyVar {})         = False
    go (XIfaceType _)          = False
    go (IfaceAppTy fun args)   = go fun && go_args args
    go (IfaceFunTy _ w arg res) = go w && go arg && go res
    go (IfaceForAllTy {})      = False
    go (IfaceTyConApp _ args)  = go_args args
    go (IfaceTupleTy _ _ args) = go_args args
    go (IfaceLitTy _)          = True
    go (IfaceCastTy {})        = False -- Safe
    go (IfaceCoercionTy {})    = False -- Safe

    go_args IA_Nil = True
    go_args (IA_Arg arg _ args) = go arg && go_args args

{-
Note [Suppressing invisible arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use the IfaceAppArgs data type to specify which of the arguments to a type
should be displayed when pretty-printing, under the control of
-fprint-explicit-kinds.
See also Type.filterOutInvisibleTypes.
For example, given

    T :: forall k. (k->*) -> k -> *    -- Ordinary kind polymorphism
    'Just :: forall k. k -> 'Maybe k   -- Promoted

we want

    T * Tree Int    prints as    T Tree Int
    'Just *         prints as    Just *

For type constructors (IfaceTyConApp), IfaceAppArgs is a quite natural fit,
since the corresponding Core constructor:

    data Type
      = ...
      | TyConApp TyCon [Type]

Already puts all of its arguments into a list. So when converting a Type to an
IfaceType (see toIfaceAppArgsX in GHC.Core.ToIface), we simply use the kind of
the TyCon (which is cached) to guide the process of converting the argument
Types into an IfaceAppArgs list.

We also want this behavior for IfaceAppTy, since given:

    data Proxy (a :: k)
    f :: forall (t :: forall a. a -> Type). Proxy Type (t Bool True)

We want to print the return type as `Proxy (t True)` without the use of
-fprint-explicit-kinds (#15330). Accomplishing this is trickier than in the
tycon case, because the corresponding Core constructor for IfaceAppTy:

    data Type
      = ...
      | AppTy Type Type

Only stores one argument at a time. Therefore, when converting an AppTy to an
IfaceAppTy (in toIfaceTypeX in GHC.CoreToIface), we:

1. Flatten the chain of AppTys down as much as possible
2. Use typeKind to determine the function Type's kind
3. Use this kind to guide the process of converting the argument Types into an
   IfaceAppArgs list.

By flattening the arguments like this, we obtain two benefits:

(a) We can reuse the same machinery to pretty-print IfaceTyConApp arguments as
    we do IfaceTyApp arguments, which means that we only need to implement the
    logic to filter out invisible arguments once.
(b) Unlike for tycons, finding the kind of a type in general (through typeKind)
    is not a constant-time operation, so by flattening the arguments first, we
    decrease the number of times we have to call typeKind.

Note [Pretty-printing invisible arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note [Suppressing invisible arguments] is all about how to avoid printing
invisible arguments when the -fprint-explicit-kinds flag is disables. Well,
what about when it's enabled? Then we can and should print invisible kind
arguments, and this Note explains how we do it.

As two running examples, consider the following code:

  {-# LANGUAGE PolyKinds #-}
  data T1 a
  data T2 (a :: k)

When displaying these types (with -fprint-explicit-kinds on), we could just
do the following:

  T1 k a
  T2 k a

That certainly gets the job done. But it lacks a crucial piece of information:
is the `k` argument inferred or specified? To communicate this, we use visible
kind application syntax to distinguish the two cases:

  T1 @{k} a
  T2 @k   a

Here, @{k} indicates that `k` is an inferred argument, and @k indicates that
`k` is a specified argument. (See
Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility] in GHC.Core.TyCo.Rep for
a lengthier explanation on what "inferred" and "specified" mean.)

************************************************************************
*                                                                      *
                Pretty-printing
*                                                                      *
************************************************************************
-}

if_print_coercions :: SDoc  -- ^ if printing coercions
                   -> SDoc  -- ^ otherwise
                   -> SDoc
if_print_coercions yes no
  = sdocOption sdocPrintExplicitCoercions $ \print_co ->
    getPprStyle $ \style ->
    getPprDebug $ \debug ->
    if print_co || dumpStyle style || debug
    then yes
    else no

pprIfaceInfixApp :: PprPrec -> SDoc -> SDoc -> SDoc -> SDoc
pprIfaceInfixApp ctxt_prec pp_tc pp_ty1 pp_ty2
  = maybeParen ctxt_prec opPrec $
    sep [pp_ty1, pp_tc <+> pp_ty2]

pprIfacePrefixApp :: PprPrec -> SDoc -> [SDoc] -> SDoc
pprIfacePrefixApp ctxt_prec pp_fun pp_tys
  | null pp_tys = pp_fun
  | otherwise   = maybeParen ctxt_prec appPrec $
                  hang pp_fun 2 (sep pp_tys)

isIfaceTauType :: IfaceType -> Bool
isIfaceTauType (IfaceForAllTy _ _) = False
isIfaceTauType (IfaceFunTy InvisArg _ _ _) = False
isIfaceTauType _ = True

-- ----------------------------- Printing binders ------------------------------------

instance Outputable IfaceBndrPpr where
    ppr (IfaceIdBndr bndr) = pprIfaceIdBndr bndr
    ppr (IfaceTvBndr bndr) = char '@' <> pprIfaceTvBndr bndr (SuppressBndrSig False)
                                                             (UseBndrParens False)

pprIfaceBndrs :: [IfaceBndrPpr] -> SDoc
pprIfaceBndrs bs = sep (map ppr bs)

pprIfaceLamBndr :: IfaceLamBndrPpr -> SDoc
pprIfaceLamBndr (b, IfaceNoOneShot) = ppr b
pprIfaceLamBndr (b, IfaceOneShot)   = ppr b <> text "[OneShot]"

pprIfaceIdBndr :: IfaceIdBndrPpr -> SDoc
pprIfaceIdBndr (w, name, ty) = parens (ppr name <> brackets (ppr w) <+> dcolon <+> ppr ty)

{- Note [Suppressing binder signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When printing the binders in a 'forall', we want to keep the kind annotations:

    forall (a :: k). blah
              ^^^^
              good

On the other hand, when we print the binders of a data declaration in :info,
the kind information would be redundant due to the standalone kind signature:

   type F :: Symbol -> Type
   type F (s :: Symbol) = blah
             ^^^^^^^^^
             redundant

Here we'd like to omit the kind annotation:

   type F :: Symbol -> Type
   type F s = blah

Note [Printing type abbreviations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally, we pretty-print `TYPE 'LiftedRep` as `Type` (or `*`) and
`FUN 'Many` as `(->)`.
This way, error messages don't refer to representation polymorphism
or linearity if it is not necessary.

However, when printing the definition of Type or (->) with :info,
this would give confusing output: `type (->) = (->)` (#18594).
Solution: detect when we are in :info and disable displaying the synonym
with the SDoc option sdocPrintTypeAbbreviations.

If there will be a need, in the future we could expose it as a flag
-fprint-type-abbreviations or even two separate flags controlling
TYPE 'LiftedRep and FUN 'Many.
-}

-- | Do we want to suppress kind annotations on binders?
-- See Note [Suppressing binder signatures]
newtype SuppressBndrSig = SuppressBndrSig Bool

newtype UseBndrParens      = UseBndrParens Bool

pprIfaceTvBndr :: IfaceTvBndrPpr -> SuppressBndrSig -> UseBndrParens -> SDoc
pprIfaceTvBndr (tv, ki) (SuppressBndrSig suppress_sig) (UseBndrParens use_parens)
  | suppress_sig             = ppr tv
  | isIfaceLiftedTypeKind ki = ppr tv
  | otherwise                = maybe_parens (ppr tv <+> dcolon <+> ppr ki)
  where
    maybe_parens | use_parens = parens
                 | otherwise  = id

pprIfaceTyConBinders :: SuppressBndrSig -> [IfaceTyConBinderPpr] -> SDoc
pprIfaceTyConBinders suppress_sig = sep . map go
  where
    go :: IfaceTyConBinderPpr -> SDoc
    go (Bndr (IfaceIdBndr bndr) _) = pprIfaceIdBndr bndr
    go (Bndr (IfaceTvBndr bndr) vis) =
      -- See Note [Pretty-printing invisible arguments]
      case vis of
        AnonTCB  VisArg    -> ppr_bndr (UseBndrParens True)
        AnonTCB  InvisArg  -> char '@' <> braces (ppr_bndr (UseBndrParens False))
          -- The above case is rare. (See Note [AnonTCB InvisArg] in GHC.Core.TyCon.)
          -- Should we print these differently?
        NamedTCB Required  -> ppr_bndr (UseBndrParens True)
        NamedTCB Specified -> char '@' <> ppr_bndr (UseBndrParens True)
        NamedTCB Inferred  -> char '@' <> braces (ppr_bndr (UseBndrParens False))
      where
        ppr_bndr = pprIfaceTvBndr bndr suppress_sig

-- ----------------------------- Printing IfaceType ------------------------------------

---------------------------------
instance Outputable IfaceTypePpr where
  ppr ty = pprIfaceType ty

pprIfaceType, pprParendIfaceType :: IfaceTypePpr -> SDoc
pprIfaceType       = pprPrecIfaceType topPrec
pprParendIfaceType = pprPrecIfaceType appPrec

pprPrecIfaceType :: PprPrec -> IfaceTypePpr -> SDoc
-- We still need `hideNonStandardTypes`, since the `pprPrecIfaceType` may be
-- called from other places, besides `:type` and `:info`.
pprPrecIfaceType prec ty =
  hideNonStandardTypes (ppr_ty prec) ty

ppr_fun_arrow :: IfaceMultPpr -> SDoc
ppr_fun_arrow w
  | (IfaceTyConApp tc _) <- w
  , tc `ifaceTyConHasKey` (getUnique manyDataConTyCon) = arrow
  | (IfaceTyConApp tc _) <- w
  , tc `ifaceTyConHasKey` (getUnique oneDataConTyCon) = lollipop
  | otherwise = mulArrow (pprIfaceType w)

ppr_sigma :: PprPrec -> IfaceTypePpr -> SDoc
ppr_sigma ctxt_prec ty
  = maybeParen ctxt_prec funPrec (pprIfaceSigmaType ShowForAllMust ty)

ppr_ty :: PprPrec -> IfaceTypePpr -> SDoc
ppr_ty ctxt_prec ty@(IfaceForAllTy {})          = ppr_sigma ctxt_prec ty
ppr_ty ctxt_prec ty@(IfaceFunTy InvisArg _ _ _) = ppr_sigma ctxt_prec ty

ppr_ty _         (XIfaceType (IfaceFreeTyVar tyvar)) = ppr tyvar  -- This is the main reason for XIfaceFreeTyVar!
ppr_ty _         (IfaceTyVar tyvar)     = ppr tyvar  -- See Note [Free tyvars in IfaceType]
ppr_ty ctxt_prec (IfaceTyConApp tc tys) = pprTyTcApp ctxt_prec tc tys
ppr_ty ctxt_prec (IfaceTupleTy i p tys) = pprTuple ctxt_prec i p tys -- always fully saturated
ppr_ty _         (IfaceLitTy n)         = pprIfaceTyLit n
        -- Function types
ppr_ty ctxt_prec (IfaceFunTy _ w ty1 ty2)  -- Should be VisArg
  = -- We don't want to lose synonyms, so we mustn't use splitFunTys here.
    maybeParen ctxt_prec funPrec $
    sep [ppr_ty funPrec ty1, sep (ppr_fun_tail w ty2)]
  where
    ppr_fun_tail wthis (IfaceFunTy VisArg wnext ty1 ty2)
      = (ppr_fun_arrow wthis <+> ppr_ty funPrec ty1) : ppr_fun_tail wnext ty2
    ppr_fun_tail wthis other_ty
      = [ppr_fun_arrow wthis <+> pprIfaceType other_ty]

ppr_ty ctxt_prec (IfaceAppTy t ts)
  = if_print_coercions
      ppr_app_ty
      ppr_app_ty_no_casts
  where
    ppr_app_ty =
        sdocOption sdocPrintExplicitKinds $ \print_kinds ->
        let tys_wo_kinds = appArgsIfaceTypesArgFlags $ stripInvisArgs
                              (PrintExplicitKinds print_kinds) ts
        in pprIfacePrefixApp ctxt_prec
                             (ppr_ty funPrec t)
                             (map (ppr_app_arg appPrec) tys_wo_kinds)


    -- Strip any casts from the head of the application
    ppr_app_ty_no_casts =
        case t of
          IfaceCastTy head _ -> ppr_ty ctxt_prec (mk_app_tys head ts)
          _                  -> ppr_app_ty

    mk_app_tys :: IfaceTypePpr -> IfaceAppArgsPpr -> IfaceTypePpr
    mk_app_tys (IfaceTyConApp tc tys1) tys2 =
        IfaceTyConApp tc (tys1 `mappend` tys2)
    mk_app_tys t1 tys2 = IfaceAppTy t1 tys2

ppr_ty ctxt_prec (IfaceCastTy ty co)
  = if_print_coercions
      (parens (ppr_ty topPrec ty <+> text "|>" <+> ppr co))
      (ppr_ty ctxt_prec ty)

ppr_ty ctxt_prec (IfaceCoercionTy co)
  = if_print_coercions
      (ppr_co ctxt_prec co)
      (text "<>")

{- Note [Defaulting RuntimeRep variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RuntimeRep variables are considered by many (most?) users to be little
more than syntactic noise. When the notion was introduced there was a
significant and understandable push-back from those with pedagogy in
mind, which argued that RuntimeRep variables would throw a wrench into
nearly any teach approach since they appear in even the lowly ($)
function's type,

    ($) :: forall (w :: RuntimeRep) a (b :: TYPE w). (a -> b) -> a -> b

which is significantly less readable than its non RuntimeRep-polymorphic type of

    ($) :: (a -> b) -> a -> b

Moreover, unboxed types don't appear all that often in run-of-the-mill
Haskell programs, so it makes little sense to make all users pay this
syntactic overhead.

For this reason it was decided that we would hide RuntimeRep variables
for now (see #11549). We do this by defaulting all type variables of
kind RuntimeRep to LiftedRep.
Likewise, we default all Multiplicity variables to Many.

This is done in a pass right before pretty-printing
(defaultIfaceTyVarsOfKind, controlled by
-fprint-explicit-runtime-reps and -XLinearTypes)

This applies to /quantified/ variables like 'w' above.  What about
variables that are /free/ in the type being printed, which certainly
happens in error messages.  Suppose (#16074, #19361) we are reporting a
mismatch between skolems
          (a :: RuntimeRep) ~ (b :: RuntimeRep)
        or
          (m :: Multiplicity) ~ Many
We certainly don't want to say "Can't match LiftedRep with LiftedRep" or
"Can't match Many with Many"!

But if we are printing the type
    (forall (a :: TYPE r). blah)
we do want to turn that (free) r into LiftedRep, so it prints as
    (forall a. blah)

We use isMetaTyVar to distinguish between those two situations:
metavariables are converted, skolem variables are not.

There's one exception though: TyVarTv metavariables should not be defaulted,
as they appear during kind-checking of "newtype T :: TYPE r where..."
(test T18357a). Therefore, we additionally test for isTyConableTyVar.
-}

-- | Default 'RuntimeRep' variables to 'LiftedRep',
--   'Levity' variables to 'Lifted', and 'Multiplicity'
--   variables to 'Many'. For example:
--
-- @
-- ($) :: forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r).
--        (a -> b) -> a -> b
-- Just :: forall (k :: Multiplicity) a. a % k -> Maybe a
-- @
--
-- turns in to,
--
-- @ ($) :: forall a (b :: *). (a -> b) -> a -> b @
-- @ Just :: forall a . a -> Maybe a @
--
-- We do this to prevent RuntimeRep, Levity and Multiplicity variables from
-- incurring a significant syntactic overhead in otherwise simple
-- type signatures (e.g. ($)). See Note [Defaulting RuntimeRep variables]
-- and #11549 for further discussion.
defaultIfaceTyVarsOfKind :: Bool -- ^ default 'RuntimeRep'/'Levity' variables?
                         -> Bool -- ^ default 'Multiplicity' variables?
                         -> IfaceTypePpr -> IfaceTypePpr
defaultIfaceTyVarsOfKind def_rep def_mult ty = go emptyFsEnv ty
  where
    go :: FastStringEnv IfaceTypePpr -- Set of enclosing forall-ed RuntimeRep/Levity/Multiplicity variables
       -> IfaceTypePpr
       -> IfaceTypePpr
    go subs (IfaceForAllTy (Bndr (IfaceTvBndr (var, var_kind)) argf) ty)
     | isInvisibleArgFlag argf  -- Don't default *visible* quantification
                                -- or we get the mess in #13963
     , Just substituted_ty <- check_substitution var_kind
      = let subs' = extendFsEnv subs var substituted_ty
            -- Record that we should replace it with LiftedRep/Lifted/Many,
            -- and recurse, discarding the forall
        in go subs' ty

    go subs (IfaceForAllTy bndr ty)
      = IfaceForAllTy (go_ifacebndr subs bndr) (go subs ty)

    go subs ty@(IfaceTyVar tv) = case lookupFsEnv subs tv of
      Just s -> s
      Nothing -> ty

    go _ ty@(XIfaceType (IfaceFreeTyVar tv))
      -- See Note [Defaulting RuntimeRep variables], about free vars
      | def_rep
      , GHC.Core.Type.isRuntimeRepTy (tyVarKind tv)
      , isMetaTyVar tv
      , isTyConableTyVar tv
      = liftedRep_ty
      | def_rep
      , GHC.Core.Type.isLevityTy (tyVarKind tv)
      , isMetaTyVar tv
      , isTyConableTyVar tv
      = lifted_ty
      | def_mult
      , GHC.Core.Type.isMultiplicityTy (tyVarKind tv)
      , isMetaTyVar tv
      , isTyConableTyVar tv
      = many_ty
      | otherwise
      = ty

    go subs (IfaceTyConApp tc tc_args)
      = IfaceTyConApp tc (go_args subs tc_args)

    go subs (IfaceTupleTy sort is_prom tc_args)
      = IfaceTupleTy sort is_prom (go_args subs tc_args)

    go subs (IfaceFunTy af w arg res)
      = IfaceFunTy af (go subs w) (go subs arg) (go subs res)

    go subs (IfaceAppTy t ts)
      = IfaceAppTy (go subs t) (go_args subs ts)

    go subs (IfaceCastTy x co)
      = IfaceCastTy (go subs x) co

    go _ ty@(IfaceLitTy {}) = ty
    go _ ty@(IfaceCoercionTy {}) = ty

    go_ifacebndr :: FastStringEnv IfaceTypePpr -> IfaceForAllBndrPpr -> IfaceForAllBndrPpr
    go_ifacebndr subs (Bndr (IfaceIdBndr (w, n, t)) argf)
      = Bndr (IfaceIdBndr (w, n, go subs t)) argf
    go_ifacebndr subs (Bndr (IfaceTvBndr (n, t)) argf)
      = Bndr (IfaceTvBndr (n, go subs t)) argf

    go_args :: FastStringEnv IfaceTypePpr -> IfaceAppArgsPpr -> IfaceAppArgsPpr
    go_args _ IA_Nil = IA_Nil
    go_args subs (IA_Arg ty argf args)
      = IA_Arg (go subs ty) argf (go_args subs args)

    check_substitution :: IfaceTypePpr -> Maybe IfaceTypePpr
    check_substitution (IfaceTyConApp tc _)
        | def_rep
        , tc `ifaceTyConHasKey` runtimeRepTyConKey
        = Just liftedRep_ty
        | def_rep
        , tc `ifaceTyConHasKey` levityTyConKey
        = Just lifted_ty
        | def_mult
        , tc `ifaceTyConHasKey` multiplicityTyConKey
        = Just many_ty
    check_substitution _ = Nothing

-- | The type ('BoxedRep 'Lifted), also known as LiftedRep.
liftedRep_ty :: IfaceType' p
liftedRep_ty =
  IfaceTyConApp liftedRep IA_Nil
  where
    liftedRep :: IfaceTyCon
    liftedRep = IfaceTyCon tc_name (mkIfaceTyConInfo NotPromoted IfaceNormalTyCon)
      where tc_name = getName liftedRepTyCon

-- | The type 'Lifted :: Levity'.
lifted_ty :: IfaceType' p
lifted_ty =
    IfaceTyConApp (IfaceTyCon dc_name (mkIfaceTyConInfo IsPromoted IfaceNormalTyCon))
                  IA_Nil
  where dc_name = getName liftedDataConTyCon

hideNonStandardTypes :: (IfaceTypePpr -> SDoc) -> IfaceTypePpr -> SDoc
hideNonStandardTypes f ty
  = sdocOption sdocPrintExplicitRuntimeReps $ \printExplicitRuntimeReps ->
    sdocOption sdocLinearTypes $ \linearTypes ->
    getPprStyle      $ \sty    ->
    let def_rep  = not printExplicitRuntimeReps
        def_mult = not linearTypes
    in if userStyle sty
       then f (defaultIfaceTyVarsOfKind def_rep def_mult ty)
       else f ty

instance Outputable IfaceAppArgsPpr where
  ppr tca = pprIfaceAppArgs tca

pprIfaceAppArgs, pprParendIfaceAppArgs :: IfaceAppArgsPpr -> SDoc
pprIfaceAppArgs  = ppr_app_args topPrec
pprParendIfaceAppArgs = ppr_app_args appPrec

ppr_app_args :: PprPrec -> IfaceAppArgsPpr -> SDoc
ppr_app_args ctx_prec = go
  where
    go :: IfaceAppArgsPpr -> SDoc
    go IA_Nil             = empty
    go (IA_Arg t argf ts) = ppr_app_arg ctx_prec (t, argf) <+> go ts

-- See Note [Pretty-printing invisible arguments]
ppr_app_arg :: PprPrec -> (IfaceTypePpr, ArgFlag) -> SDoc
ppr_app_arg ctx_prec (t, argf) =
  sdocOption sdocPrintExplicitKinds $ \print_kinds ->
  case argf of
       Required  -> ppr_ty ctx_prec t
       Specified |  print_kinds
                 -> char '@' <> ppr_ty appPrec t
       Inferred  |  print_kinds
                 -> char '@' <> braces (ppr_ty topPrec t)
       _         -> empty

-------------------
pprIfaceForAllPart :: [IfaceForAllBndrPpr] -> [IfacePredTypePpr] -> SDoc -> SDoc
pprIfaceForAllPart tvs ctxt sdoc
  = ppr_iface_forall_part ShowForAllWhen tvs ctxt sdoc

-- | Like 'pprIfaceForAllPart', but always uses an explicit @forall@.
pprIfaceForAllPartMust :: [IfaceForAllBndrPpr] -> [IfacePredTypePpr] -> SDoc -> SDoc
pprIfaceForAllPartMust tvs ctxt sdoc
  = ppr_iface_forall_part ShowForAllMust tvs ctxt sdoc

pprIfaceForAllCoPart :: [(IfLclName, IfaceCoercionPpr)] -> SDoc -> SDoc
pprIfaceForAllCoPart tvs sdoc
  = sep [ pprIfaceForAllCo tvs, sdoc ]

ppr_iface_forall_part :: ShowForAllFlag
                      -> [IfaceForAllBndrPpr] -> [IfacePredTypePpr] -> SDoc -> SDoc
ppr_iface_forall_part show_forall tvs ctxt sdoc
  = sep [ case show_forall of
            ShowForAllMust -> pprIfaceForAll tvs
            ShowForAllWhen -> pprUserIfaceForAll tvs
        , pprIfaceContextArr ctxt
        , sdoc]

-- | Render the "forall ... ." or "forall ... ->" bit of a type.
pprIfaceForAll :: [IfaceForAllBndrPpr] -> SDoc
pprIfaceForAll [] = empty
pprIfaceForAll bndrs@(Bndr _ vis : _)
  = sep [ add_separator (forAllLit <+> fsep docs)
        , pprIfaceForAll bndrs' ]
  where
    (bndrs', docs) = ppr_itv_bndrs bndrs vis

    add_separator stuff = case vis of
                            Required -> stuff <+> arrow
                            _inv     -> stuff <>  dot


-- | Render the ... in @(forall ... .)@ or @(forall ... ->)@.
-- Returns both the list of not-yet-rendered binders and the doc.
-- No anonymous binders here!
ppr_itv_bndrs :: [IfaceForAllBndrPpr]
             -> ArgFlag  -- ^ visibility of the first binder in the list
             -> ([IfaceForAllBndrPpr], [SDoc])
ppr_itv_bndrs all_bndrs@(bndr@(Bndr _ vis) : bndrs) vis1
  | vis `sameVis` vis1 = let (bndrs', doc) = ppr_itv_bndrs bndrs vis1 in
                         (bndrs', pprIfaceForAllBndr bndr : doc)
  | otherwise   = (all_bndrs, [])
ppr_itv_bndrs [] _ = ([], [])

pprIfaceForAllCo :: [(IfLclName, IfaceCoercionPpr)] -> SDoc
pprIfaceForAllCo []  = empty
pprIfaceForAllCo tvs = text "forall" <+> pprIfaceForAllCoBndrs tvs <> dot

pprIfaceForAllCoBndrs :: [(IfLclName, IfaceCoercionPpr)] -> SDoc
pprIfaceForAllCoBndrs bndrs = hsep $ map pprIfaceForAllCoBndr bndrs

pprIfaceForAllBndr :: IfaceForAllBndrPpr -> SDoc
pprIfaceForAllBndr bndr =
  case bndr of
    Bndr (IfaceTvBndr tv) Inferred ->
      braces $ pprIfaceTvBndr tv suppress_sig (UseBndrParens False)
    Bndr (IfaceTvBndr tv) _ ->
      pprIfaceTvBndr tv suppress_sig (UseBndrParens True)
    Bndr (IfaceIdBndr idv) _ -> pprIfaceIdBndr idv
  where
    -- See Note [Suppressing binder signatures]
    suppress_sig = SuppressBndrSig False

pprIfaceForAllCoBndr :: (IfLclName, IfaceCoercionPpr) -> SDoc
pprIfaceForAllCoBndr (tv, kind_co)
  = parens (ppr tv <+> dcolon <+> pprIfaceCoercion kind_co)

-- | Show forall flag
--
-- Unconditionally show the forall quantifier with ('ShowForAllMust')
-- or when ('ShowForAllWhen') the names used are free in the binder
-- or when compiling with -fprint-explicit-foralls.
data ShowForAllFlag = ShowForAllMust | ShowForAllWhen

pprIfaceSigmaType :: ShowForAllFlag -> IfaceTypePpr -> SDoc
pprIfaceSigmaType show_forall ty
  = hideNonStandardTypes ppr_fn ty
  where
    ppr_fn iface_ty =
      let (invis_tvs, theta, tau) = splitIfaceSigmaTy iface_ty
          (req_tvs, tau') = splitIfaceReqForallTy tau
          -- splitIfaceSigmaTy is recursive, so it will gather the binders after
          -- the theta, i.e.  forall a. theta => forall b. tau
          -- will give you    ([a,b], theta, tau).
          --
          -- This isn't right when it comes to visible forall (see
          --  testsuite/tests/polykinds/T18522-ppr),
          -- so we split off required binders separately,
          -- using splitIfaceReqForallTy.
          --
          -- An alternative solution would be to make splitIfaceSigmaTy
          -- non-recursive (see #18458).
          -- Then it could handle both invisible and required binders, and
          -- splitIfaceReqForallTy wouldn't be necessary here.
       in ppr_iface_forall_part show_forall invis_tvs theta $
          sep [pprIfaceForAll req_tvs, ppr tau']

pprUserIfaceForAll :: [IfaceForAllBndrPpr] -> SDoc
pprUserIfaceForAll tvs
   = sdocOption sdocPrintExplicitForalls $ \print_foralls ->
     -- See Note [When to print foralls] in this module.
     ppWhen (any tv_has_kind_var tvs
             || any tv_is_required tvs
             || print_foralls) $
     pprIfaceForAll tvs
   where
     tv_has_kind_var (Bndr (IfaceTvBndr (_,kind)) _)
       = not (ifTypeIsVarFree kind)
     tv_has_kind_var _ = False

     tv_is_required = isVisibleArgFlag . binderArgFlag

{-
Note [When to print foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We opt to explicitly pretty-print `forall`s if any of the following
criteria are met:

1. -fprint-explicit-foralls is on.

2. A bound type variable has a polymorphic kind. E.g.,

     forall k (a::k). Proxy a -> Proxy a

   Since a's kind mentions a variable k, we print the foralls.

3. A bound type variable is a visible argument (#14238).
   Suppose we are printing the kind of:

     T :: forall k -> k -> Type

   The "forall k ->" notation means that this kind argument is required.
   That is, it must be supplied at uses of T. E.g.,

     f :: T (Type->Type)  Monad -> Int

   So we print an explicit "T :: forall k -> k -> Type",
   because omitting it and printing "T :: k -> Type" would be
   utterly misleading.

   See Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility]
   in GHC.Core.TyCo.Rep.

N.B. Until now (Aug 2018) we didn't check anything for coercion variables.

Note [Printing foralls in type family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use the same criteria as in Note [When to print foralls] to determine
whether a type family instance should be pretty-printed with an explicit
`forall`. Example:

  type family Foo (a :: k) :: k where
    Foo Maybe       = []
    Foo (a :: Type) = Int
    Foo a           = a

Without -fprint-explicit-foralls enabled, this will be pretty-printed as:

type family Foo (a :: k) :: k where
  Foo Maybe = []
  Foo a = Int
  forall k (a :: k). Foo a = a

Note that only the third equation has an explicit forall, since it has a type
variable with a non-Type kind. (If -fprint-explicit-foralls were enabled, then
the second equation would be preceded with `forall a.`.)

There is one tricky point in the implementation: what visibility
do we give the type variables in a type family instance? Type family instances
only store type *variables*, not type variable *binders*, and only the latter
has visibility information. We opt to default the visibility of each of these
type variables to Specified because users can't ever instantiate these
variables manually, so the choice of visibility is only relevant to
pretty-printing. (This is why the `k` in `forall k (a :: k). ...` above is
printed the way it is, even though it wasn't written explicitly in the
original source code.)

We adopt the same strategy for data family instances. Example:

  data family DF (a :: k)
  data instance DF '[a, b] = DFList

That data family instance is pretty-printed as:

  data instance forall j (a :: j) (b :: j). DF '[a, b] = DFList

This is despite that the representation tycon for this data instance (call it
$DF:List) actually has different visibilities for its binders.
However, the visibilities of these /binders are utterly irrelevant to the
programmer, who cares only about the specificity of variables in `DF`'s type,
not $DF:List's type. Therefore, we opt to pretty-print all variables in data
family instances as Specified.

Note [Printing promoted type constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this GHCi session (#14343)
    > _ :: Proxy '[ 'True ]
    error:
      Found hole: _ :: Proxy '['True]

This would be bad, because the '[' looks like a character literal.
Solution: in type-level lists and tuples, add a leading space
if the first type is itself promoted.  See pprSpaceIfPromotedTyCon.
-}


-------------------

-- | Prefix a space if the given 'IfaceType' is a promoted 'TyCon'.
-- See Note [Printing promoted type constructors]
pprSpaceIfPromotedTyCon :: IfaceTypePpr -> SDoc -> SDoc
pprSpaceIfPromotedTyCon (IfaceTyConApp tyCon _)
  = case ifaceTyConIsPromoted (ifaceTyConInfo tyCon) of
      IsPromoted -> (space <>)
      _ -> id
pprSpaceIfPromotedTyCon _
  = id

-- See equivalent function in "GHC.Core.TyCo.Rep"
pprIfaceTyList :: PprPrec -> IfaceTypePpr -> IfaceTypePpr -> SDoc
-- Given a type-level list (t1 ': t2), see if we can print
-- it in list notation [t1, ...].
-- Precondition: Opt_PrintExplicitKinds is off
pprIfaceTyList ctxt_prec ty1 ty2
  = case gather ty2 of
      (arg_tys, Nothing)
        -> char '\'' <> brackets (pprSpaceIfPromotedTyCon ty1 (fsep
                        (punctuate comma (map (ppr_ty topPrec) (ty1:arg_tys)))))
      (arg_tys, Just tl)
        -> maybeParen ctxt_prec funPrec $ hang (ppr_ty funPrec ty1)
           2 (fsep [ colon <+> ppr_ty funPrec ty | ty <- arg_tys ++ [tl]])
  where
    gather :: IfaceTypePpr -> ([IfaceTypePpr], Maybe IfaceTypePpr)
     -- (gather ty) = (tys, Nothing) means ty is a list [t1, .., tn]
     --             = (tys, Just tl) means ty is of form t1:t2:...tn:tl
    gather (IfaceTyConApp tc tys)
      | tc `ifaceTyConHasKey` consDataConKey
      , IA_Arg _ argf (IA_Arg ty1 Required (IA_Arg ty2 Required IA_Nil)) <- tys
      , isInvisibleArgFlag argf
      , (args, tl) <- gather ty2
      = (ty1:args, tl)
      | tc `ifaceTyConHasKey` nilDataConKey
      = ([], Nothing)
    gather ty = ([], Just ty)

pprIfaceTypeApp :: PprPrec -> IfaceTyCon -> IfaceAppArgsPpr -> SDoc
pprIfaceTypeApp prec tc args = pprTyTcApp prec tc args

pprTyTcApp :: PprPrec -> IfaceTyCon -> IfaceAppArgsPpr -> SDoc
pprTyTcApp ctxt_prec tc tys =
    sdocOption sdocPrintExplicitKinds $ \print_kinds ->
    sdocOption sdocPrintTypeAbbreviations $ \print_type_abbreviations ->
    getPprDebug $ \debug ->

    if | ifaceTyConName tc `hasKey` ipClassKey
       , IA_Arg (IfaceLitTy (IfaceStrTyLit n))
                Required (IA_Arg ty Required IA_Nil) <- tys
       -> maybeParen ctxt_prec funPrec
         $ char '?' <> ftext n <> text "::" <> ppr_ty topPrec ty

       | IfaceTupleTyCon arity sort <- ifaceTyConSort info
       , not debug
       , arity == ifaceVisAppArgsLength tys
       -> pprTuple ctxt_prec sort (ifaceTyConIsPromoted info) tys
           -- NB: pprTuple requires a saturated tuple.

       | IfaceSumTyCon arity <- ifaceTyConSort info
       , not debug
       , arity == ifaceVisAppArgsLength tys
       -> pprSum (ifaceTyConIsPromoted info) tys
           -- NB: pprSum requires a saturated unboxed sum.

       | tc `ifaceTyConHasKey` consDataConKey
       , False <- print_kinds
       , IA_Arg _ argf (IA_Arg ty1 Required (IA_Arg ty2 Required IA_Nil)) <- tys
       , isInvisibleArgFlag argf
       -> pprIfaceTyList ctxt_prec ty1 ty2

       | isIfaceTyConAppLiftedTypeKind tc tys
       , print_type_abbreviations  -- See Note [Printing type abbreviations]
       -> ppr_kind_type ctxt_prec

       | tc `ifaceTyConHasKey` funTyConKey
       , IA_Arg (IfaceTyConApp rep IA_Nil) Required args <- tys
       , rep `ifaceTyConHasKey` manyDataConKey
       , print_type_abbreviations  -- See Note [Printing type abbreviations]
       -> pprIfacePrefixApp ctxt_prec (parens arrow) (map (ppr_app_arg appPrec) $
          appArgsIfaceTypesArgFlags $ stripInvisArgs (PrintExplicitKinds print_kinds) args)
          -- Use appArgsIfaceTypesArgFlags to print invisible arguments
          -- correctly (#19310)

       | tc `ifaceTyConHasKey` errorMessageTypeErrorFamKey
       , not debug
         -- Suppress detail unless you _really_ want to see
       -> text "(TypeError ...)"

       | Just doc <- ppr_equality ctxt_prec tc (appArgsIfaceTypes tys)
       -> doc

       | otherwise
       -> ppr_iface_tc_app ppr_app_arg ctxt_prec tc $
          appArgsIfaceTypesArgFlags $ stripInvisArgs (PrintExplicitKinds print_kinds) tys
  where
    info = ifaceTyConInfo tc

ppr_kind_type :: PprPrec -> SDoc
ppr_kind_type ctxt_prec = sdocOption sdocStarIsType $ \case
   False -> pprPrefixOcc liftedTypeKindTyConName
   True  -> maybeParen ctxt_prec starPrec $
              unicodeSyntax (char '★') (char '*')

-- | Pretty-print a type-level equality.
-- Returns (Just doc) if the argument is a /saturated/ application
-- of   eqTyCon          (~)
--      eqPrimTyCon      (~#)
--      eqReprPrimTyCon  (~R#)
--      heqTyCon         (~~)
--
-- See Note [Equality predicates in IfaceType]
-- and Note [The equality types story] in GHC.Builtin.Types.Prim
ppr_equality :: PprPrec -> IfaceTyCon -> [IfaceTypePpr] -> Maybe SDoc
ppr_equality ctxt_prec tc args
  | hetero_eq_tc
  , [k1, k2, t1, t2] <- args
  = Just $ print_equality (k1, k2, t1, t2)

  | hom_eq_tc
  , [k, t1, t2] <- args
  = Just $ print_equality (k, k, t1, t2)

  | otherwise
  = Nothing
  where
    homogeneous = tc_name `hasKey` eqTyConKey -- (~)
               || hetero_tc_used_homogeneously
      where
        hetero_tc_used_homogeneously
          = case ifaceTyConSort $ ifaceTyConInfo tc of
                          IfaceEqualityTyCon -> True
                          _other             -> False
             -- True <=> a heterogeneous equality whose arguments
             --          are (in this case) of the same kind

    tc_name = ifaceTyConName tc
    pp = ppr_ty
    hom_eq_tc = tc_name `hasKey` eqTyConKey            -- (~)
    hetero_eq_tc = tc_name `hasKey` eqPrimTyConKey     -- (~#)
                || tc_name `hasKey` eqReprPrimTyConKey -- (~R#)
                || tc_name `hasKey` heqTyConKey        -- (~~)
    nominal_eq_tc = tc_name `hasKey` heqTyConKey       -- (~~)
                 || tc_name `hasKey` eqPrimTyConKey    -- (~#)
    print_equality args =
        sdocOption sdocPrintExplicitKinds $ \print_kinds ->
        sdocOption sdocPrintEqualityRelations $ \print_eqs ->
        getPprStyle      $ \style  ->
        getPprDebug      $ \debug  ->
        print_equality' args print_kinds
          (print_eqs || dumpStyle style || debug)

    print_equality' (ki1, ki2, ty1, ty2) print_kinds print_eqs
      | -- If -fprint-equality-relations is on, just print the original TyCon
        print_eqs
      = ppr_infix_eq (ppr tc)

      | -- Homogeneous use of heterogeneous equality (ty1 ~~ ty2)
        --                 or unlifted equality      (ty1 ~# ty2)
        nominal_eq_tc, homogeneous
      = ppr_infix_eq (text "~")

      | -- Heterogeneous use of unlifted equality (ty1 ~# ty2)
        not homogeneous
      = ppr_infix_eq (ppr heqTyCon)

      | -- Homogeneous use of representational unlifted equality (ty1 ~R# ty2)
        tc_name `hasKey` eqReprPrimTyConKey, homogeneous
      = let ki | print_kinds = [pp appPrec ki1]
               | otherwise   = []
        in pprIfacePrefixApp ctxt_prec (ppr coercibleTyCon)
                            (ki ++ [pp appPrec ty1, pp appPrec ty2])

        -- The other cases work as you'd expect
      | otherwise
      = ppr_infix_eq (ppr tc)
      where
        ppr_infix_eq :: SDoc -> SDoc
        ppr_infix_eq eq_op = pprIfaceInfixApp ctxt_prec eq_op
                               (pp_ty_ki ty1 ki1) (pp_ty_ki ty2 ki2)
          where
            pp_ty_ki ty ki
              | print_kinds
              = parens (pp topPrec ty <+> dcolon <+> pp opPrec ki)
              | otherwise
              = pp opPrec ty


pprIfaceCoTcApp :: PprPrec -> IfaceTyCon -> [IfaceCoercionPpr] -> SDoc
pprIfaceCoTcApp ctxt_prec tc tys =
  ppr_iface_tc_app (\prec (co, _) -> ppr_co prec co) ctxt_prec tc
    (map (, Required) tys)
    -- We are trying to re-use ppr_iface_tc_app here, which requires its
    -- arguments to be accompanied by visibilities. But visibility is
    -- irrelevant when printing coercions, so just default everything to
    -- Required.

-- | Pretty-prints an application of a type constructor to some arguments
-- (whose visibilities are known). This is polymorphic (over @a@) since we use
-- this function to pretty-print two different things:
--
-- 1. Types (from `pprTyTcApp'`)
--
-- 2. Coercions (from 'pprIfaceCoTcApp')
ppr_iface_tc_app :: (PprPrec -> (a, ArgFlag) -> SDoc)
                 -> PprPrec -> IfaceTyCon -> [(a, ArgFlag)] -> SDoc
ppr_iface_tc_app pp _ tc [ty]
  | tc `ifaceTyConHasKey` listTyConKey = pprPromotionQuote tc <> brackets (pp topPrec ty)

ppr_iface_tc_app pp ctxt_prec tc tys
  | tc `ifaceTyConHasKey` liftedTypeKindTyConKey
  = ppr_kind_type ctxt_prec

  | not (isSymOcc (nameOccName (ifaceTyConName tc)))
  = pprIfacePrefixApp ctxt_prec (ppr tc) (map (pp appPrec) tys)

  | [ ty1@(_, Required)
    , ty2@(_, Required) ] <- tys
      -- Infix, two visible arguments (we know nothing of precedence though).
      -- Don't apply this special case if one of the arguments is invisible,
      -- lest we print something like (@LiftedRep -> @LiftedRep) (#15941).
  = pprIfaceInfixApp ctxt_prec (ppr tc)
                     (pp opPrec ty1) (pp opPrec ty2)

  | otherwise
  = pprIfacePrefixApp ctxt_prec (parens (ppr tc)) (map (pp appPrec) tys)

-- | Pretty-print an unboxed sum type. The sum should be saturated:
-- as many visible arguments as the arity of the sum.
--
-- NB: this always strips off the invisible 'RuntimeRep' arguments,
-- even with `-fprint-explicit-runtime-reps` and `-fprint-explicit-kinds`.
pprSum :: PromotionFlag -> IfaceAppArgsPpr -> SDoc
pprSum is_promoted args
  =   -- drop the RuntimeRep vars.
      -- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
    let tys   = appArgsIfaceTypes args
        args' = drop (length tys `div` 2) tys
    in pprPromotionQuoteI is_promoted
       <> sumParens (pprWithBars (ppr_ty topPrec) args')

-- | Pretty-print a tuple type (boxed tuple, constraint tuple, unboxed tuple).
-- The tuple should be saturated: as many visible arguments as the arity of
-- the tuple.
--
-- NB: this always strips off the invisible 'RuntimeRep' arguments,
-- even with `-fprint-explicit-runtime-reps` and `-fprint-explicit-kinds`.
pprTuple :: PprPrec -> TupleSort -> PromotionFlag -> IfaceAppArgsPpr -> SDoc
pprTuple ctxt_prec sort promoted args =
  case promoted of
    IsPromoted
      -> let tys = appArgsIfaceTypes args
             args' = drop (length tys `div` 2) tys
             spaceIfPromoted = case args' of
               arg0:_ -> pprSpaceIfPromotedTyCon arg0
               _ -> id
         in ppr_tuple_app args' $
            pprPromotionQuoteI IsPromoted <>
            tupleParens sort (spaceIfPromoted (pprWithCommas pprIfaceType args'))

    NotPromoted
      |  ConstraintTuple <- sort
      ,  IA_Nil <- args
      -> maybeParen ctxt_prec sigPrec $
         text "() :: Constraint"

      | otherwise
      ->   -- drop the RuntimeRep vars.
           -- See Note [Unboxed tuple RuntimeRep vars] in GHC.Core.TyCon
         let tys   = appArgsIfaceTypes args
             args' = case sort of
                       UnboxedTuple -> drop (length tys `div` 2) tys
                       _            -> tys
         in
         ppr_tuple_app args' $
         pprPromotionQuoteI promoted <>
         tupleParens sort (pprWithCommas pprIfaceType args')
  where
    ppr_tuple_app :: [IfaceTypePpr] -> SDoc -> SDoc
    ppr_tuple_app args_wo_runtime_reps ppr_args_w_parens
        -- Special-case unary boxed tuples so that they are pretty-printed as
        -- `Solo x`, not `(x)`
      | [_] <- args_wo_runtime_reps
      , BoxedTuple <- sort
      = let unit_tc_info = mkIfaceTyConInfo promoted IfaceNormalTyCon
            unit_tc = IfaceTyCon (tupleTyConName sort 1) unit_tc_info in
        pprPrecIfaceType ctxt_prec $ IfaceTyConApp unit_tc args
      | otherwise
      = ppr_args_w_parens

pprIfaceCoercion, pprParendIfaceCoercion :: IfaceCoercionPpr -> SDoc
pprIfaceCoercion = ppr_co topPrec
pprParendIfaceCoercion = ppr_co appPrec

ppr_co :: PprPrec -> IfaceCoercionPpr -> SDoc
ppr_co _         (IfaceReflCo ty) = angleBrackets (ppr ty) <> ppr_role Nominal
ppr_co _         (IfaceGReflCo r ty IfaceMRefl)
  = angleBrackets (ppr ty) <> ppr_role r
ppr_co ctxt_prec (IfaceGReflCo r ty (IfaceMCo co))
  = ppr_special_co ctxt_prec
    (text "GRefl" <+> ppr r <+> pprParendIfaceType ty) [co]
ppr_co ctxt_prec (IfaceFunCo r cow co1 co2)
  = maybeParen ctxt_prec funPrec $
    sep (ppr_co funPrec co1 : ppr_fun_tail cow co2)
  where
    ppr_fun_tail cow' (IfaceFunCo r cow co1 co2)
      = (coercionArrow cow' <> ppr_role r <+> ppr_co funPrec co1) : ppr_fun_tail cow co2
    ppr_fun_tail cow' other_co
      = [coercionArrow cow' <> ppr_role r <+> pprIfaceCoercion other_co]
    coercionArrow w = mulArrow (ppr_co topPrec w)

ppr_co _         (IfaceTyConAppCo r tc cos)
  = parens (pprIfaceCoTcApp topPrec tc cos) <> ppr_role r
ppr_co ctxt_prec (IfaceAppCo co1 co2)
  = maybeParen ctxt_prec appPrec $
    ppr_co funPrec co1 <+> pprParendIfaceCoercion co2
ppr_co ctxt_prec co@(IfaceForAllCo {})
  = maybeParen ctxt_prec funPrec $
    pprIfaceForAllCoPart tvs (pprIfaceCoercion inner_co)
  where
    (tvs, inner_co) = split_co co

    split_co (IfaceForAllCo (IfaceTvBndr (name, _)) kind_co co')
      = let (tvs, co'') = split_co co' in ((name,kind_co):tvs,co'')
    split_co (IfaceForAllCo (IfaceIdBndr (_, name, _)) kind_co co')
      = let (tvs, co'') = split_co co' in ((name,kind_co):tvs,co'')
    split_co co' = ([], co')

-- Why these three? See Note [Free tyvars in IfaceType]
ppr_co _ (IfaceCoVarCo covar)   = ppr covar
ppr_co _ (XIfaceCoercion (IfaceFreeCoVar covar)) = ppr covar
ppr_co _ (XIfaceCoercion (IfaceHoleCo covar))    = braces (ppr covar)

ppr_co _ (IfaceUnivCo prov role ty1 ty2)
  = text "Univ" <> (parens $
      sep [ ppr role <+> pprIfaceUnivCoProv prov
          , dcolon <+>  ppr ty1 <> comma <+> ppr ty2 ])

ppr_co ctxt_prec (IfaceInstCo co ty)
  = maybeParen ctxt_prec appPrec $
    text "Inst" <+> pprParendIfaceCoercion co
                        <+> pprParendIfaceCoercion ty

ppr_co ctxt_prec (IfaceAxiomRuleCo tc cos)
  = maybeParen ctxt_prec appPrec $ ppr tc <+> parens (interpp'SP cos)

ppr_co ctxt_prec (IfaceAxiomInstCo n i cos)
  = ppr_special_co ctxt_prec (ppr n <> brackets (ppr i)) cos
ppr_co ctxt_prec (IfaceSymCo co)
  = ppr_special_co ctxt_prec (text "Sym") [co]
ppr_co ctxt_prec (IfaceTransCo co1 co2)
    -- chain nested TransCo
  = let ppr_trans (IfaceTransCo c1 c2) = semi <+> ppr_co topPrec c1 : ppr_trans c2
        ppr_trans c                    = [semi <+> ppr_co opPrec c]
    in maybeParen ctxt_prec opPrec $
        vcat (ppr_co topPrec co1 : ppr_trans co2)
ppr_co ctxt_prec (IfaceNthCo d co)
  = ppr_special_co ctxt_prec (text "Nth:" <> int d) [co]
ppr_co ctxt_prec (IfaceLRCo lr co)
  = ppr_special_co ctxt_prec (ppr lr) [co]
ppr_co ctxt_prec (IfaceSubCo co)
  = ppr_special_co ctxt_prec (text "Sub") [co]
ppr_co ctxt_prec (IfaceKindCo co)
  = ppr_special_co ctxt_prec (text "Kind") [co]

ppr_special_co :: PprPrec -> SDoc -> [IfaceCoercionPpr] -> SDoc
ppr_special_co ctxt_prec doc cos
  = maybeParen ctxt_prec appPrec
               (sep [doc, nest 4 (sep (map pprParendIfaceCoercion cos))])

ppr_role :: Role -> SDoc
ppr_role r = underscore <> pp_role
  where pp_role = case r of
                    Nominal          -> char 'N'
                    Representational -> char 'R'
                    Phantom          -> char 'P'

------------------
pprIfaceUnivCoProv :: IfaceUnivCoProvPpr -> SDoc
pprIfaceUnivCoProv (IfacePhantomProv co)
  = text "phantom" <+> pprParendIfaceCoercion co
pprIfaceUnivCoProv (IfaceProofIrrelProv co)
  = text "irrel" <+> pprParendIfaceCoercion co
pprIfaceUnivCoProv (IfacePluginProv s)
  = text "plugin" <+> doubleQuotes (text s)
pprIfaceUnivCoProv (IfaceCorePrepProv _)
  = text "CorePrep"

-------------------

instance Outputable IfaceCoercionPpr where
  ppr = pprIfaceCoercion

-------------------

-- Some notes about printing contexts
--
-- In the event that we are printing a singleton context (e.g. @Eq a@) we can
-- omit parentheses. However, we must take care to set the precedence correctly
-- to opPrec, since something like @a :~: b@ must be parenthesized (see
-- #9658).
--
-- When printing a larger context we use 'fsep' instead of 'sep' so that
-- the context doesn't get displayed as a giant column. Rather than,
--  instance (Eq a,
--            Eq b,
--            Eq c,
--            Eq d,
--            Eq e,
--            Eq f,
--            Eq g,
--            Eq h,
--            Eq i,
--            Eq j,
--            Eq k,
--            Eq l) =>
--           Eq (a, b, c, d, e, f, g, h, i, j, k, l)
--
-- we want
--
--  instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--            Eq j, Eq k, Eq l) =>
--           Eq (a, b, c, d, e, f, g, h, i, j, k, l)



-- | Prints "(C a, D b) =>", including the arrow.
-- Used when we want to print a context in a type, so we
-- use 'funPrec' to decide whether to parenthesise a singleton
-- predicate; e.g.   Num a => a -> a
pprIfaceContextArr :: [IfacePredTypePpr] -> SDoc
pprIfaceContextArr []     = empty
pprIfaceContextArr [pred] = ppr_ty funPrec pred <+> darrow
pprIfaceContextArr preds  = ppr_parend_preds preds <+> darrow

-- | Prints a context or @()@ if empty
-- You give it the context precedence
pprIfaceContext :: PprPrec -> [IfacePredTypePpr] -> SDoc
pprIfaceContext _    []     = text "()"
pprIfaceContext prec [pred] = ppr_ty prec pred
pprIfaceContext _    preds  = ppr_parend_preds preds

ppr_parend_preds :: [IfacePredTypePpr] -> SDoc
ppr_parend_preds preds = parens (fsep (punctuate comma (map ppr preds)))
