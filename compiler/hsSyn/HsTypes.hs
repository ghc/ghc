{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


HsTypes: Abstract syntax: user-defined types
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}

module HsTypes (
        HsType(..), LHsType, HsKind, LHsKind,
        HsTyVarBndr(..), LHsTyVarBndr,
        LHsQTyVars(..),
        HsImplicitBndrs(..),
        HsWildCardBndrs(..),
        LHsSigType, LHsSigWcType, LHsWcType,
        HsTupleSort(..),
        HsContext, LHsContext,
        HsTyLit(..),
        HsIPName(..), hsIPNameFS,
        HsAppType(..),

        LBangType, BangType,
        HsSrcBang(..), HsImplBang(..),
        SrcStrictness(..), SrcUnpackedness(..),
        getBangType, getBangStrictness,

        ConDeclField(..), LConDeclField, pprConDeclFields, updateGadtResult,

        HsConDetails(..),

        FieldOcc(..), LFieldOcc, mkFieldOcc,
        AmbiguousFieldOcc(..), mkAmbiguousFieldOcc,
        rdrNameAmbiguousFieldOcc, selectorAmbiguousFieldOcc,
        unambiguousFieldOcc, ambiguousFieldOcc,

        HsWildCardInfo(..), mkAnonWildCardTy, mkNamedWildCardTy,
        wildCardName, sameWildCard, sameNamedWildCard,
        isAnonWildCard, isNamedWildCard,

        mkHsImplicitBndrs, mkHsWildCardBndrs, hsImplicitBody,
        mkEmptyImplicitBndrs, mkEmptyWildCardBndrs,
        mkHsQTvs, hsQTvExplicit, emptyLHsQTvs, isEmptyLHsQTvs,
        isHsKindedTyVar, hsTvbAllKinded,
        hsScopedTvs, hsWcScopedTvs, dropWildCards,
        hsTyVarName, hsAllLTyVarNames, hsLTyVarLocNames,
        hsLTyVarName, hsLTyVarLocName, hsExplicitLTyVarNames,
        splitLHsInstDeclTy,
        splitLHsPatSynTy,
        splitLHsForAllTy, splitLHsQualTy, splitLHsSigmaTy,
        splitHsFunType, splitHsAppTys,
        mkHsOpTy,
        ignoreParens, hsSigType, hsSigWcType,
        hsLTyVarBndrToType, hsLTyVarBndrsToTypes,

        -- Printing
        pprParendHsType, pprHsForAll, pprHsForAllTvs, pprHsForAllExtra,
        pprHsContext, pprHsContextNoArrow, pprHsContextMaybe
    ) where

import {-# SOURCE #-} HsExpr ( HsSplice, pprSplice )

import PlaceHolder ( PostTc,PostRn,DataId,PlaceHolder(..) )

import Id ( Id )
import Name( Name )
import RdrName ( RdrName )
import DataCon( HsSrcBang(..), HsImplBang(..),
                SrcStrictness(..), SrcUnpackedness(..) )
import TysPrim( funTyConName )
import Type
import HsDoc
import BasicTypes
import SrcLoc
import StaticFlags
import Outputable
import FastString
import Maybes( isJust )

import Data.Data hiding ( Fixity )
import Data.Maybe ( fromMaybe )
import Control.Monad ( unless )
#if __GLASGOW_HASKELL > 710
import Data.Semigroup   ( Semigroup )
import qualified Data.Semigroup as Semigroup
#endif

{-
************************************************************************
*                                                                      *
\subsection{Bang annotations}
*                                                                      *
************************************************************************
-}

type LBangType name = Located (BangType name)
type BangType name  = HsType name       -- Bangs are in the HsType data type

getBangType :: LHsType a -> LHsType a
getBangType (L _ (HsBangTy _ ty)) = ty
getBangType ty                    = ty

getBangStrictness :: LHsType a -> HsSrcBang
getBangStrictness (L _ (HsBangTy s _)) = s
getBangStrictness _ = (HsSrcBang Nothing NoSrcUnpack NoSrcStrict)

{-
************************************************************************
*                                                                      *
\subsection{Data types}
*                                                                      *
************************************************************************

This is the syntax for types as seen in type signatures.

Note [HsBSig binder lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a binder (or pattern) decoarated with a type or kind,
   \ (x :: a -> a). blah
   forall (a :: k -> *) (b :: k). blah
Then we use a LHsBndrSig on the binder, so that the
renamer can decorate it with the variables bound
by the pattern ('a' in the first example, 'k' in the second),
assuming that neither of them is in scope already
See also Note [Kind and type-variable binders] in RnTypes

Note [HsType binders]
~~~~~~~~~~~~~~~~~~~~~
The system for recording type and kind-variable binders in HsTypes
is a bit complicated.  Here's how it works.

* In a HsType,
     HsForAllTy   represents an /explicit, user-written/ 'forall'
                   e.g.   forall a b. ...
     HsQualTy     reprsents an /explicit, user-written/ context
                   e.g.   (Eq a, Show a) => ...
                  The context can be empty if that's what the user wrote
  These constructors represent what the user wrote, no more
  and no less.

* HsTyVarBndr describes a quantified type variable written by the
  user.  For example
     f :: forall a (b :: *).  blah
  here 'a' and '(b::*)' are each a HsTyVarBndr.  A HsForAllTy has
  a list of LHsTyVarBndrs.

* HsImplicitBndrs is a wrapper that gives the implicitly-quantified
  kind and type variables of the wrapped thing.  It is filled in by
  the renamer.   For example, if the
  user writes
     f :: a -> a
  the HsImplicitBinders binds the 'a' (not a HsForAllTy!).
  NB: this implicit quantification is purely lexical: we bind any
      type or kind variables that are not in scope. The type checker
      may subsequently quantify over further kind variables.

* HsWildCardBndrs is a wrapper that binds the wildcard variables
  of the wrapped thing.  It is filled in by the renamer
     f :: _a -> _
  The enclosing HsWildCardBndrs binds the wildcards _a and _.

* The explicit presence of these wrappers specifies, in the HsSyn,
  exactly where implicit quantification is allowed, and where
  wildcards are allowed.

* LHsQTyVars is used in data/class declarations, where the user gives
  explicit *type* variable bindings, but we need to implicitly bind
  *kind* variables.  For example
      class C (a :: k -> *) where ...
  The 'k' is implicitly bound in the hsq_tvs field of LHsQTyVars

-}

type LHsContext name = Located (HsContext name)
      -- ^ 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnUnit'

      -- For details on above see note [Api annotations] in ApiAnnotation

type HsContext name = [LHsType name]

type LHsType name = Located (HsType name)
      -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma' when
      --   in a list

      -- For details on above see note [Api annotations] in ApiAnnotation
type HsKind name = HsType name
type LHsKind name = Located (HsKind name)
      -- ^ 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

      -- For details on above see note [Api annotations] in ApiAnnotation

--------------------------------------------------
--             LHsQTyVars
--  The explicitly-quantified binders in a data/type declaration

type LHsTyVarBndr name = Located (HsTyVarBndr name)
                         -- See Note [HsType binders]

data LHsQTyVars name   -- See Note [HsType binders]
  = HsQTvs { hsq_implicit :: PostRn name [Name]      -- implicit (dependent) variables
           , hsq_explicit :: [LHsTyVarBndr name]     -- explicit variables
             -- See Note [HsForAllTy tyvar binders]
    }
  deriving( Typeable )

deriving instance (DataId name) => Data (LHsQTyVars name)

mkHsQTvs :: [LHsTyVarBndr RdrName] -> LHsQTyVars RdrName
mkHsQTvs tvs = HsQTvs { hsq_implicit = PlaceHolder, hsq_explicit = tvs }

hsQTvExplicit :: LHsQTyVars name -> [LHsTyVarBndr name]
hsQTvExplicit = hsq_explicit

emptyLHsQTvs :: LHsQTyVars Name
emptyLHsQTvs = HsQTvs [] []

isEmptyLHsQTvs :: LHsQTyVars Name -> Bool
isEmptyLHsQTvs (HsQTvs [] []) = True
isEmptyLHsQTvs _              = False

------------------------------------------------
--            HsImplicitBndrs
-- Used to quantify the binders of a type in cases
-- when a HsForAll isn't appropriate:
--    * Patterns in a type/data family instance (HsTyPats)
--    * Type of a rule binder (RuleBndr)
--    * Pattern type signatures (SigPatIn)
-- In the last of these, wildcards can happen, so we must accommodate them

data HsImplicitBndrs name thing   -- See Note [HsType binders]
  = HsIB { hsib_vars :: PostRn name [Name] -- Implicitly-bound kind & type vars
         , hsib_body :: thing              -- Main payload (type or list of types)
    }
  deriving (Typeable)

data HsWildCardBndrs name thing   -- See Note [HsType binders]
  = HsWC { hswc_wcs :: PostRn name [Name]
                -- Wild cards, both named and anonymous

         , hswc_ctx :: Maybe SrcSpan
                -- Indicates whether hswc_body has an
                -- extra-constraint wildcard, and if so where
                --    e.g.  (Eq a, _) => a -> a
                -- NB: the wildcard stays in HsQualTy inside the type!
                -- So for pretty printing purposes you can ignore
                -- hswc_ctx

         , hswc_body :: thing  -- Main payload (type or list of types)
    }
  deriving( Typeable )

deriving instance (Data name, Data thing, Data (PostRn name [Name]))
  => Data (HsImplicitBndrs name thing)

deriving instance (Data name, Data thing, Data (PostRn name [Name]))
  => Data (HsWildCardBndrs name thing)

type LHsSigType   name = HsImplicitBndrs name (LHsType name)    -- Implicit only
type LHsWcType    name = HsWildCardBndrs name (LHsType name)    -- Wildcard only
type LHsSigWcType name = HsImplicitBndrs name (LHsWcType name)  -- Both

-- See Note [Representing type signatures]

hsImplicitBody :: HsImplicitBndrs name thing -> thing
hsImplicitBody (HsIB { hsib_body = body }) = body

hsSigType :: LHsSigType name -> LHsType name
hsSigType = hsImplicitBody

hsSigWcType :: LHsSigWcType name -> LHsType name
hsSigWcType sig_ty = hswc_body (hsib_body sig_ty)

dropWildCards :: LHsSigWcType name -> LHsSigType name
-- Drop the wildcard part of a LHsSigWcType
dropWildCards sig_ty = sig_ty { hsib_body = hsSigWcType sig_ty }

{- Note [Representing type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HsSigType is used to represent an explicit user type signature
such as   f :: a -> a
     or   g (x :: a -> a) = x

A HsSigType is just a HsImplicitBndrs wrapping a LHsType.
 * The HsImplicitBndrs binds the /implicitly/ quantified tyvars
 * The LHsType binds the /explictly/ quantified tyvars

E.g. For a signature like
   f :: forall (a::k). blah
we get
   HsIB { hsib_vars = [k]
        , hsib_body = HsForAllTy { hst_bndrs = [(a::*)]
                                 , hst_body = blah }
The implicit kind variable 'k' is bound by the HsIB;
the explictly forall'd tyvar 'a' is bounnd by the HsForAllTy
-}

mkHsImplicitBndrs :: thing -> HsImplicitBndrs RdrName thing
mkHsImplicitBndrs x = HsIB { hsib_body = x
                           , hsib_vars = PlaceHolder }

mkHsWildCardBndrs :: thing -> HsWildCardBndrs RdrName thing
mkHsWildCardBndrs x = HsWC { hswc_body = x
                           , hswc_wcs  = PlaceHolder
                           , hswc_ctx  = Nothing }

-- Add empty binders.  This is a bit suspicious; what if
-- the wrapped thing had free type variables?
mkEmptyImplicitBndrs :: thing -> HsImplicitBndrs Name thing
mkEmptyImplicitBndrs x = HsIB { hsib_body = x
                              , hsib_vars = [] }

mkEmptyWildCardBndrs :: thing -> HsWildCardBndrs Name thing
mkEmptyWildCardBndrs x = HsWC { hswc_body = x
                              , hswc_wcs  = []
                              , hswc_ctx  = Nothing }


--------------------------------------------------
-- | These names are used early on to store the names of implicit
-- parameters.  They completely disappear after type-checking.
newtype HsIPName = HsIPName FastString
  deriving( Eq, Data, Typeable )

hsIPNameFS :: HsIPName -> FastString
hsIPNameFS (HsIPName n) = n

instance Outputable HsIPName where
    ppr (HsIPName n) = char '?' <> ftext n -- Ordinary implicit parameters

instance OutputableBndr HsIPName where
    pprBndr _ n   = ppr n         -- Simple for now
    pprInfixOcc  n = ppr n
    pprPrefixOcc n = ppr n

--------------------------------------------------
data HsTyVarBndr name
  = UserTyVar        -- no explicit kinding
         (Located name)
        -- See Note [Located RdrNames] in HsExpr
  | KindedTyVar
         (Located name)
         (LHsKind name)  -- The user-supplied kind signature
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --          'ApiAnnotation.AnnDcolon', 'ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId name) => Data (HsTyVarBndr name)

-- | Does this 'HsTyVarBndr' come with an explicit kind annotation?
isHsKindedTyVar :: HsTyVarBndr name -> Bool
isHsKindedTyVar (UserTyVar {})   = False
isHsKindedTyVar (KindedTyVar {}) = True

-- | Do all type variables in this 'LHsQTyVars' come with kind annotations?
hsTvbAllKinded :: LHsQTyVars name -> Bool
hsTvbAllKinded = all (isHsKindedTyVar . unLoc) . hsQTvExplicit

data HsType name
  = HsForAllTy   -- See Note [HsType binders]
      { hst_bndrs :: [LHsTyVarBndr name]   -- Explicit, user-supplied 'forall a b c'
      , hst_body  :: LHsType name          -- body type
      }
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnForall',
      --         'ApiAnnotation.AnnDot','ApiAnnotation.AnnDarrow'
      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsQualTy   -- See Note [HsType binders]
      { hst_ctxt :: LHsContext name       -- Context C => blah
      , hst_body :: LHsType name }

  | HsTyVar    (Located name)
                  -- Type variable, type constructor, or data constructor
                  -- see Note [Promotions (HsTyVar)]
                  -- See Note [Located RdrNames] in HsExpr
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsAppsTy            [HsAppType name]  -- Used only before renaming,
                                          -- Note [HsAppsTy]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

  | HsAppTy             (LHsType name)
                        (LHsType name)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsFunTy             (LHsType name)   -- function type
                        (LHsType name)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRarrow',

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsListTy            (LHsType name)  -- Element type
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
      --         'ApiAnnotation.AnnClose' @']'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsPArrTy            (LHsType name)  -- Elem. type of parallel array: [:t:]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
      --         'ApiAnnotation.AnnClose' @':]'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsTupleTy           HsTupleSort
                        [LHsType name]  -- Element types (length gives arity)
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'(' or '(#'@,
    --         'ApiAnnotation.AnnClose' @')' or '#)'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsOpTy              (LHsType name) (Located name) (LHsType name)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsParTy             (LHsType name)   -- See Note [Parens in HsSyn] in HsExpr
        -- Parenthesis preserved for the precedence re-arrangement in RnTypes
        -- It's important that a * (b + c) doesn't get rearranged to (a*b) + c!
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
      --         'ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsIParamTy          HsIPName         -- (?x :: ty)
                        (LHsType name)   -- Implicit parameters as they occur in contexts
      -- ^
      -- > (?x :: ty)
      --
      -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsEqTy              (LHsType name)   -- ty1 ~ ty2
                        (LHsType name)   -- Always allowed even without TypeOperators, and has special kinding rule
      -- ^
      -- > ty1 ~ ty2
      --
      -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsKindSig           (LHsType name)  -- (ty :: kind)
                        (LHsKind name)  -- A type with a kind signature
      -- ^
      -- > (ty :: kind)
      --
      -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
      --         'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsSpliceTy          (HsSplice name)   -- Includes quasi-quotes
                        (PostTc name Kind)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'$('@,
      --         'ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsDocTy             (LHsType name) LHsDocString -- A documented type
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsBangTy    HsSrcBang (LHsType name)   -- Bang-style type annotations
      -- ^ - 'ApiAnnotation.AnnKeywordId' :
      --         'ApiAnnotation.AnnOpen' @'{-\# UNPACK' or '{-\# NOUNPACK'@,
      --         'ApiAnnotation.AnnClose' @'#-}'@
      --         'ApiAnnotation.AnnBang' @\'!\'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsRecTy     [LConDeclField name]    -- Only in data type declarations
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{'@,
      --         'ApiAnnotation.AnnClose' @'}'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCoreTy Type       -- An escape hatch for tunnelling a *closed*
                        -- Core Type through HsSyn.
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsExplicitListTy       -- A promoted explicit list
        (PostTc name Kind) -- See Note [Promoted lists and tuples]
        [LHsType name]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @"'["@,
      --         'ApiAnnotation.AnnClose' @']'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsExplicitTupleTy      -- A promoted explicit tuple
        [PostTc name Kind] -- See Note [Promoted lists and tuples]
        [LHsType name]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @"'("@,
      --         'ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsTyLit HsTyLit      -- A promoted numeric literal.
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsWildCardTy (HsWildCardInfo name)  -- A type wildcard
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId name) => Data (HsType name)

-- Note [Literal source text] in BasicTypes for SourceText fields in
-- the following
data HsTyLit
  = HsNumTy SourceText Integer
  | HsStrTy SourceText FastString
    deriving (Data, Typeable)

mkHsOpTy :: LHsType name -> Located name -> LHsType name -> HsType name
mkHsOpTy ty1 op ty2 = HsOpTy ty1 op ty2

data HsWildCardInfo name
    = AnonWildCard (PostRn name (Located Name))
      -- A anonymous wild card ('_'). A fresh Name is generated for
      -- each individual anonymous wildcard during renaming
    | NamedWildCard (Located name)
      -- A named wild card ('_a').
    deriving (Typeable)
deriving instance (DataId name) => Data (HsWildCardInfo name)

data HsAppType name
  = HsAppInfix (Located name)       -- either a symbol or an id in backticks
  | HsAppPrefix (LHsType name)      -- anything else, including things like (+)
  deriving (Typeable)
deriving instance (DataId name) => Data (HsAppType name)

instance OutputableBndr name => Outputable (HsAppType name) where
  ppr = ppr_app_ty TopPrec

{-
Note [HsForAllTy tyvar binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After parsing:
  * Implicit => empty
    Explicit => the variables the user wrote

After renaming
  * Implicit => the *type* variables free in the type
    Explicit => the variables the user wrote (renamed)

Qualified currently behaves exactly as Implicit,
but it is deprecated to use it for implicit quantification.
In this case, GHC 7.10 gives a warning; see
Note [Context quantification] in RnTypes, and Trac #4426.
In GHC 7.12, Qualified will no longer bind variables
and this will become an error.

The kind variables bound in the hsq_implicit field come both
  a) from the kind signatures on the kind vars (eg k1)
  b) from the scope of the forall (eg k2)
Example:   f :: forall (a::k1) b. T a (b::k2)


Note [Unit tuples]
~~~~~~~~~~~~~~~~~~
Consider the type
    type instance F Int = ()
We want to parse that "()"
    as HsTupleTy HsBoxedOrConstraintTuple [],
NOT as HsTyVar unitTyCon

Why? Because F might have kind (* -> Constraint), so we when parsing we
don't know if that tuple is going to be a constraint tuple or an ordinary
unit tuple.  The HsTupleSort flag is specifically designed to deal with
that, but it has to work for unit tuples too.

Note [Promotions (HsTyVar)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
HsTyVar: A name in a type or kind.
  Here are the allowed namespaces for the name.
    In a type:
      Var: not allowed
      Data: promoted data constructor
      Tv: type variable
      TcCls before renamer: type constructor, class constructor, or promoted data constructor
      TcCls after renamer: type constructor or class constructor
    In a kind:
      Var, Data: not allowed
      Tv: kind variable
      TcCls: kind constructor or promoted type constructor

Note [HsAppsTy]
~~~~~~~~~~~~~~~
How to parse

  Foo * Int

? Is it `(*) Foo Int` or `Foo GHC.Types.* Int`? There's no way to know until renaming.
So we just take type expressions like this and put each component in a list, so be
sorted out in the renamer. The sorting out is done by RnTypes.mkHsOpTyRn. This means
that the parser should never produce HsAppTy or HsOpTy.

Note [Promoted lists and tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice the difference between
   HsListTy    HsExplicitListTy
   HsTupleTy   HsExplicitListTupleTy

E.g.    f :: [Int]                      HsListTy

        g3  :: T '[]                   All these use
        g2  :: T '[True]                  HsExplicitListTy
        g1  :: T '[True,False]
        g1a :: T [True,False]             (can omit ' where unambiguous)

  kind of T :: [Bool] -> *        This kind uses HsListTy!

E.g.    h :: (Int,Bool)                 HsTupleTy; f is a pair
        k :: S '(True,False)            HsExplicitTypleTy; S is indexed by
                                           a type-level pair of booleans
        kind of S :: (Bool,Bool) -> *   This kind uses HsExplicitTupleTy

Note [Distinguishing tuple kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Apart from promotion, tuples can have one of three different kinds:

        x :: (Int, Bool)                -- Regular boxed tuples
        f :: Int# -> (# Int#, Int# #)   -- Unboxed tuples
        g :: (Eq a, Ord a) => a         -- Constraint tuples

For convenience, internally we use a single constructor for all of these,
namely HsTupleTy, but keep track of the tuple kind (in the first argument to
HsTupleTy, a HsTupleSort). We can tell if a tuple is unboxed while parsing,
because of the #. However, with -XConstraintKinds we can only distinguish
between constraint and boxed tuples during type checking, in general. Hence the
four constructors of HsTupleSort:

        HsUnboxedTuple                  -> Produced by the parser
        HsBoxedTuple                    -> Certainly a boxed tuple
        HsConstraintTuple               -> Certainly a constraint tuple
        HsBoxedOrConstraintTuple        -> Could be a boxed or a constraint
                                        tuple. Produced by the parser only,
                                        disappears after type checking
-}

data HsTupleSort = HsUnboxedTuple
                 | HsBoxedTuple
                 | HsConstraintTuple
                 | HsBoxedOrConstraintTuple
                 deriving (Data, Typeable)

type LConDeclField name = Located (ConDeclField name)
      -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma' when
      --   in a list

      -- For details on above see note [Api annotations] in ApiAnnotation
data ConDeclField name  -- Record fields have Haddoc docs on them
  = ConDeclField { cd_fld_names :: [LFieldOcc name],
                                   -- ^ See Note [ConDeclField names]
                   cd_fld_type :: LBangType name,
                   cd_fld_doc  :: Maybe LHsDocString }
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

      -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId name) => Data (ConDeclField name)

instance (OutputableBndr name) => Outputable (ConDeclField name) where
  ppr (ConDeclField fld_n fld_ty _) = ppr fld_n <+> dcolon <+> ppr fld_ty

-- HsConDetails is used for patterns/expressions *and* for data type
-- declarations
data HsConDetails arg rec
  = PrefixCon [arg]             -- C p1 p2 p3
  | RecCon    rec               -- C { x = p1, y = p2 }
  | InfixCon  arg arg           -- p1 `C` p2
  deriving (Data, Typeable)

instance (Outputable arg, Outputable rec)
         => Outputable (HsConDetails arg rec) where
  ppr (PrefixCon args) = text "PrefixCon" <+> ppr args
  ppr (RecCon rec)     = text "RecCon:" <+> ppr rec
  ppr (InfixCon l r)   = text "InfixCon:" <+> ppr [l, r]

type LFieldOcc name = Located (FieldOcc name)

-- | Represents an *occurrence* of an unambiguous field.  We store
-- both the 'RdrName' the user originally wrote, and after the
-- renamer, the selector function.
data FieldOcc name = FieldOcc { rdrNameFieldOcc  :: Located RdrName
                                 -- ^ See Note [Located RdrNames] in HsExpr
                              , selectorFieldOcc :: PostRn name name
                              }
  deriving Typeable
deriving instance Eq (PostRn name name) => Eq (FieldOcc name)
deriving instance Ord (PostRn name name) => Ord (FieldOcc name)
deriving instance (Data name, Data (PostRn name name)) => Data (FieldOcc name)

instance Outputable (FieldOcc name) where
  ppr = ppr . rdrNameFieldOcc

mkFieldOcc :: Located RdrName -> FieldOcc RdrName
mkFieldOcc rdr = FieldOcc rdr PlaceHolder


-- | Represents an *occurrence* of a field that is potentially
-- ambiguous after the renamer, with the ambiguity resolved by the
-- typechecker.  We always store the 'RdrName' that the user
-- originally wrote, and store the selector function after the renamer
-- (for unambiguous occurrences) or the typechecker (for ambiguous
-- occurrences).
--
-- See Note [HsRecField and HsRecUpdField] in HsPat and
-- Note [Disambiguating record fields] in TcExpr.
-- See Note [Located RdrNames] in HsExpr
data AmbiguousFieldOcc name
  = Unambiguous (Located RdrName) (PostRn name name)
  | Ambiguous   (Located RdrName) (PostTc name name)
  deriving (Typeable)
deriving instance ( Data name
                  , Data (PostRn name name)
                  , Data (PostTc name name))
                  => Data (AmbiguousFieldOcc name)

instance Outputable (AmbiguousFieldOcc name) where
  ppr = ppr . rdrNameAmbiguousFieldOcc

instance OutputableBndr (AmbiguousFieldOcc name) where
  pprInfixOcc  = pprInfixOcc . rdrNameAmbiguousFieldOcc
  pprPrefixOcc = pprPrefixOcc . rdrNameAmbiguousFieldOcc

mkAmbiguousFieldOcc :: Located RdrName -> AmbiguousFieldOcc RdrName
mkAmbiguousFieldOcc rdr = Unambiguous rdr PlaceHolder

rdrNameAmbiguousFieldOcc :: AmbiguousFieldOcc name -> RdrName
rdrNameAmbiguousFieldOcc (Unambiguous (L _ rdr) _) = rdr
rdrNameAmbiguousFieldOcc (Ambiguous   (L _ rdr) _) = rdr

selectorAmbiguousFieldOcc :: AmbiguousFieldOcc Id -> Id
selectorAmbiguousFieldOcc (Unambiguous _ sel) = sel
selectorAmbiguousFieldOcc (Ambiguous   _ sel) = sel

unambiguousFieldOcc :: AmbiguousFieldOcc Id -> FieldOcc Id
unambiguousFieldOcc (Unambiguous rdr sel) = FieldOcc rdr sel
unambiguousFieldOcc (Ambiguous   rdr sel) = FieldOcc rdr sel

ambiguousFieldOcc :: FieldOcc name -> AmbiguousFieldOcc name
ambiguousFieldOcc (FieldOcc rdr sel) = Unambiguous rdr sel

-- Takes details and result type of a GADT data constructor as created by the
-- parser and rejigs them using information about fixities from the renamer.
-- See Note [Sorting out the result type] in RdrHsSyn
updateGadtResult
  :: (Monad m)
     => (SDoc -> m ())
     -> SDoc
     -> HsConDetails (LHsType Name) (Located [LConDeclField Name])
                     -- ^ Original details
     -> LHsType Name -- ^ Original result type
     -> m (HsConDetails (LHsType Name) (Located [LConDeclField Name]),
           LHsType Name)
updateGadtResult failWith doc details ty
  = do { let (arg_tys, res_ty) = splitHsFunType ty
             badConSig         = text "Malformed constructor signature"
       ; case details of
           InfixCon {}  -> pprPanic "updateGadtResult" (ppr ty)

           RecCon {}    -> do { unless (null arg_tys)
                                       (failWith (doc <+> badConSig))
                              ; return (details, res_ty) }

           PrefixCon {} -> return (PrefixCon arg_tys, res_ty)}

{-
Note [ConDeclField names]
~~~~~~~~~~~~~~~~~~~~~~~~~

A ConDeclField contains a list of field occurrences: these always
include the field label as the user wrote it.  After the renamer, it
will additionally contain the identity of the selector function in the
second component.

Due to DuplicateRecordFields, the OccName of the selector function
may have been mangled, which is why we keep the original field label
separately.  For example, when DuplicateRecordFields is enabled

    data T = MkT { x :: Int }

gives

    ConDeclField { cd_fld_names = [L _ (FieldOcc "x" $sel:x:MkT)], ... }.
-}

-----------------------
-- A valid type must have a for-all at the top of the type, or of the fn arg
-- types

---------------------
hsWcScopedTvs :: LHsSigWcType Name -> [Name]
-- Get the lexically-scoped type variables of a HsSigType
--  - the explicitly-given forall'd type variables
--  - the implicitly-bound kind variables
--  - the named wildcars; see Note [Scoping of named wildcards]
-- because they scope in the same way
hsWcScopedTvs sig_ty
  | HsIB { hsib_vars = vars, hsib_body = sig_ty1 } <- sig_ty
  , HsWC { hswc_wcs = nwcs, hswc_body = sig_ty2 } <- sig_ty1
  = case sig_ty2 of
      L _ (HsForAllTy { hst_bndrs = tvs }) -> vars ++ nwcs ++
                                              map hsLTyVarName tvs
               -- include kind variables only if the type is headed by forall
               -- (this is consistent with GHC 7 behaviour)
      _                                    -> nwcs

hsScopedTvs :: LHsSigType Name -> [Name]
-- Same as hsWcScopedTvs, but for a LHsSigType
hsScopedTvs sig_ty
  | HsIB { hsib_vars = vars,  hsib_body = sig_ty2 } <- sig_ty
  , L _ (HsForAllTy { hst_bndrs = tvs }) <- sig_ty2
  = vars ++ map hsLTyVarName tvs
  | otherwise
  = []

{- Note [Scoping of named wildcards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f :: _a -> _a
  f x = let g :: _a -> _a
            g = ...
        in ...

Currently, for better or worse, the "_a" variables are all the same. So
although there is no explicit forall, the "_a" scopes over the definition.
I don't know if this is a good idea, but there it is.
-}

---------------------
hsTyVarName :: HsTyVarBndr name -> name
hsTyVarName (UserTyVar (L _ n))     = n
hsTyVarName (KindedTyVar (L _ n) _) = n

hsLTyVarName :: LHsTyVarBndr name -> name
hsLTyVarName = hsTyVarName . unLoc

hsExplicitLTyVarNames :: LHsQTyVars name -> [name]
-- Explicit variables only
hsExplicitLTyVarNames qtvs = map hsLTyVarName (hsQTvExplicit qtvs)

hsAllLTyVarNames :: LHsQTyVars Name -> [Name]
-- All variables
hsAllLTyVarNames (HsQTvs { hsq_implicit = kvs, hsq_explicit = tvs })
  = kvs ++ map hsLTyVarName tvs

hsLTyVarLocName :: LHsTyVarBndr name -> Located name
hsLTyVarLocName = fmap hsTyVarName

hsLTyVarLocNames :: LHsQTyVars name -> [Located name]
hsLTyVarLocNames qtvs = map hsLTyVarLocName (hsQTvExplicit qtvs)

-- | Convert a LHsTyVarBndr to an equivalent LHsType.
hsLTyVarBndrToType :: LHsTyVarBndr name -> LHsType name
hsLTyVarBndrToType = fmap cvt
  where cvt (UserTyVar n)                     = HsTyVar n
        cvt (KindedTyVar (L name_loc n) kind)
                   = HsKindSig (L name_loc (HsTyVar (L name_loc n))) kind

-- | Convert a LHsTyVarBndrs to a list of types.
-- Works on *type* variable only, no kind vars.
hsLTyVarBndrsToTypes :: LHsQTyVars name -> [LHsType name]
hsLTyVarBndrsToTypes (HsQTvs { hsq_explicit = tvbs }) = map hsLTyVarBndrToType tvbs

---------------------
mkAnonWildCardTy :: HsType RdrName
mkAnonWildCardTy = HsWildCardTy (AnonWildCard PlaceHolder)

mkNamedWildCardTy :: Located n -> HsType n
mkNamedWildCardTy = HsWildCardTy . NamedWildCard

isAnonWildCard :: HsWildCardInfo name -> Bool
isAnonWildCard (AnonWildCard _) = True
isAnonWildCard _                = False

isNamedWildCard :: HsWildCardInfo name -> Bool
isNamedWildCard = not . isAnonWildCard

wildCardName :: HsWildCardInfo Name -> Name
wildCardName (NamedWildCard (L _ n)) = n
wildCardName (AnonWildCard  (L _ n)) = n

-- Two wild cards are the same when: they're both named and have the same
-- name, or they're both anonymous and have the same location.
sameWildCard :: Eq name
             => Located (HsWildCardInfo name)
             -> Located (HsWildCardInfo name) -> Bool
sameWildCard (L l1 (AnonWildCard _))   (L l2 (AnonWildCard _))   = l1 == l2
sameWildCard (L _  (NamedWildCard (L _ n1)))
             (L _  (NamedWildCard (L _ n2))) = n1 == n2
sameWildCard _ _ = False

sameNamedWildCard :: Eq name
                  => Located (HsWildCardInfo name)
                  -> Located (HsWildCardInfo name) -> Bool
sameNamedWildCard (L _  (NamedWildCard (L _ n1)))
                  (L _  (NamedWildCard (L _ n2))) = n1 == n2
sameNamedWildCard _ _ = False

splitHsAppTys :: LHsType Name -> [LHsType Name] -> (LHsType Name, [LHsType Name])
  -- no need to worry about HsAppsTy here
splitHsAppTys (L _ (HsAppTy f a)) as = splitHsAppTys f (a:as)
splitHsAppTys (L _ (HsParTy f))   as = splitHsAppTys f as
splitHsAppTys f                   as = (f,as)

splitLHsPatSynTy :: LHsType name
                 -> ( [LHsTyVarBndr name]
                    , LHsContext name        -- Required
                    , LHsContext name        -- Provided
                    , LHsType name)          -- Body
splitLHsPatSynTy ty
  | L _ (HsQualTy { hst_ctxt = req, hst_body = ty2 }) <- ty1
  , L _ (HsQualTy { hst_ctxt = prov,  hst_body = ty3 }) <- ty2
  = (tvs, req, prov, ty3)

  | L _ (HsQualTy { hst_ctxt = req, hst_body = ty2 }) <- ty1
  = (tvs, req, noLoc [], ty2)

  | otherwise
  = (tvs, noLoc [], noLoc [], ty1)
  where
    (tvs, ty1) = splitLHsForAllTy ty

splitLHsSigmaTy :: LHsType name -> ([LHsTyVarBndr name], LHsContext name, LHsType name)
splitLHsSigmaTy ty
  | (tvs, ty1)  <- splitLHsForAllTy ty
  , (ctxt, ty2) <- splitLHsQualTy ty1
  = (tvs, ctxt, ty2)

splitLHsForAllTy :: LHsType name -> ([LHsTyVarBndr name], LHsType name)
splitLHsForAllTy (L _ (HsForAllTy { hst_bndrs = tvs, hst_body = body })) = (tvs, body)
splitLHsForAllTy body                                                    = ([], body)

splitLHsQualTy :: LHsType name -> (LHsContext name, LHsType name)
splitLHsQualTy (L _ (HsQualTy { hst_ctxt = ctxt, hst_body = body })) = (ctxt,     body)
splitLHsQualTy body                                                  = (noLoc [], body)

splitLHsInstDeclTy
    :: LHsSigType Name
    -> ([Name], LHsContext Name, LHsType Name)
        -- Split up an instance decl type, returning the pieces
splitLHsInstDeclTy (HsIB { hsib_vars = itkvs
                         , hsib_body = inst_ty })
  = (itkvs, cxt, body_ty)
         -- Return implicitly bound type and kind vars
         -- For an instance decl, all of them are in scope
  where
    (cxt, body_ty) = splitLHsQualTy inst_ty

-- splitHsFunType decomposes a type (t1 -> t2 ... -> tn)
-- Breaks up any parens in the result type:
--      splitHsFunType (a -> (b -> c)) = ([a,b], c)
-- Also deals with (->) t1 t2; that is why it only works on LHsType Name
--   (see Trac #9096)
splitHsFunType :: LHsType Name -> ([LHsType Name], LHsType Name)
splitHsFunType (L _ (HsParTy ty))
  = splitHsFunType ty

splitHsFunType (L _ (HsFunTy x y))
  | (args, res) <- splitHsFunType y
  = (x:args, res)

splitHsFunType orig_ty@(L _ (HsAppTy t1 t2))
  = go t1 [t2]
  where  -- Look for (->) t1 t2, possibly with parenthesisation
    go (L _ (HsTyVar (L _ fn))) tys | fn == funTyConName
                                 , [t1,t2] <- tys
                                 , (args, res) <- splitHsFunType t2
                                 = (t1:args, res)
    go (L _ (HsAppTy t1 t2)) tys = go t1 (t2:tys)
    go (L _ (HsParTy ty))    tys = go ty tys
    go _                     _   = ([], orig_ty)  -- Failure to match

splitHsFunType other = ([], other)

ignoreParens :: LHsType name -> LHsType name
ignoreParens (L _ (HsParTy ty))                = ignoreParens ty
ignoreParens (L _ (HsAppsTy [HsAppPrefix ty])) = ignoreParens ty
ignoreParens ty                                = ty

{-
************************************************************************
*                                                                      *
\subsection{Pretty printing}
*                                                                      *
************************************************************************
-}

instance (OutputableBndr name) => Outputable (HsType name) where
    ppr ty = pprHsType ty

instance Outputable HsTyLit where
    ppr = ppr_tylit

instance (OutputableBndr name) => Outputable (LHsQTyVars name) where
    ppr (HsQTvs { hsq_explicit = tvs }) = interppSP tvs

instance (OutputableBndr name) => Outputable (HsTyVarBndr name) where
    ppr (UserTyVar n)     = ppr n
    ppr (KindedTyVar n k) = parens $ hsep [ppr n, dcolon, ppr k]

instance (Outputable thing) => Outputable (HsImplicitBndrs name thing) where
    ppr (HsIB { hsib_body = ty }) = ppr ty

instance (Outputable thing) => Outputable (HsWildCardBndrs name thing) where
    ppr (HsWC { hswc_body = ty }) = ppr ty

instance (Outputable name) => Outputable (HsWildCardInfo name) where
    ppr (AnonWildCard _)  = char '_'
    ppr (NamedWildCard n) = ppr n

pprHsForAll :: OutputableBndr name => [LHsTyVarBndr name] -> LHsContext name -> SDoc
pprHsForAll = pprHsForAllExtra Nothing

-- | Version of 'pprHsForAll' that can also print an extra-constraints
-- wildcard, e.g. @_ => a -> Bool@ or @(Show a, _) => a -> String@. This
-- underscore will be printed when the 'Maybe SrcSpan' argument is a 'Just'
-- containing the location of the extra-constraints wildcard. A special
-- function for this is needed, as the extra-constraints wildcard is removed
-- from the actual context and type, and stored in a separate field, thus just
-- printing the type will not print the extra-constraints wildcard.
pprHsForAllExtra :: OutputableBndr name => Maybe SrcSpan -> [LHsTyVarBndr name] -> LHsContext name -> SDoc
pprHsForAllExtra extra qtvs cxt
  = pprHsForAllTvs qtvs <+> pprHsContextExtra show_extra (unLoc cxt)
  where
    show_extra = isJust extra

pprHsForAllTvs :: OutputableBndr name => [LHsTyVarBndr name] -> SDoc
pprHsForAllTvs qtvs
  | show_forall = forAllLit <+> interppSP qtvs <> dot
  | otherwise   = empty
  where
    show_forall = opt_PprStyle_Debug || not (null qtvs)

pprHsContext :: (OutputableBndr name) => HsContext name -> SDoc
pprHsContext = maybe empty (<+> darrow) . pprHsContextMaybe

pprHsContextNoArrow :: (OutputableBndr name) => HsContext name -> SDoc
pprHsContextNoArrow = fromMaybe empty . pprHsContextMaybe

pprHsContextMaybe :: (OutputableBndr name) => HsContext name -> Maybe SDoc
pprHsContextMaybe []         = Nothing
pprHsContextMaybe [L _ pred] = Just $ ppr_mono_ty FunPrec pred
pprHsContextMaybe cxt        = Just $ parens (interpp'SP cxt)

-- True <=> print an extra-constraints wildcard, e.g. @(Show a, _) =>@
pprHsContextExtra :: (OutputableBndr name) => Bool -> HsContext name -> SDoc
pprHsContextExtra show_extra ctxt
  | not show_extra
  = pprHsContext ctxt
  | null ctxt
  = char '_' <+> darrow
  | otherwise
  = parens (sep (punctuate comma ctxt')) <+> darrow
  where
    ctxt' = map ppr ctxt ++ [char '_']

pprConDeclFields :: OutputableBndr name => [LConDeclField name] -> SDoc
pprConDeclFields fields = braces (sep (punctuate comma (map ppr_fld fields)))
  where
    ppr_fld (L _ (ConDeclField { cd_fld_names = ns, cd_fld_type = ty,
                                 cd_fld_doc = doc }))
        = ppr_names ns <+> dcolon <+> ppr ty <+> ppr_mbDoc doc
    ppr_names [n] = ppr n
    ppr_names ns = sep (punctuate comma (map ppr ns))

{-
Note [Printing KindedTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Trac #3830 reminded me that we should really only print the kind
signature on a KindedTyVar if the kind signature was put there by the
programmer.  During kind inference GHC now adds a PostTcKind to UserTyVars,
rather than converting to KindedTyVars as before.

(As it happens, the message in #3830 comes out a different way now,
and the problem doesn't show up; but having the flag on a KindedTyVar
seems like the Right Thing anyway.)
-}

-- Printing works more-or-less as for Types

pprHsType, pprParendHsType :: (OutputableBndr name) => HsType name -> SDoc

pprHsType ty       = ppr_mono_ty TopPrec (prepare ty)
pprParendHsType ty = ppr_mono_ty TyConPrec ty

-- Before printing a type, remove outermost HsParTy parens
prepare :: HsType name -> HsType name
prepare (HsParTy ty)                      = prepare (unLoc ty)
prepare (HsAppsTy [HsAppPrefix (L _ ty)]) = prepare ty
prepare ty                                = ty

ppr_mono_lty :: (OutputableBndr name) => TyPrec -> LHsType name -> SDoc
ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)

ppr_mono_ty :: (OutputableBndr name) => TyPrec -> HsType name -> SDoc
ppr_mono_ty ctxt_prec (HsForAllTy { hst_bndrs = tvs, hst_body = ty })
  = maybeParen ctxt_prec FunPrec $
    sep [pprHsForAllTvs tvs, ppr_mono_lty TopPrec ty]

ppr_mono_ty ctxt_prec (HsQualTy { hst_ctxt = L _ ctxt, hst_body = ty })
  = maybeParen ctxt_prec FunPrec $
    sep [pprHsContext ctxt, ppr_mono_lty TopPrec ty]

ppr_mono_ty _    (HsBangTy b ty)     = ppr b <> ppr_mono_lty TyConPrec ty
ppr_mono_ty _    (HsRecTy flds)      = pprConDeclFields flds
ppr_mono_ty _    (HsTyVar (L _ name))= pprPrefixOcc name
ppr_mono_ty prec (HsFunTy ty1 ty2)   = ppr_fun_ty prec ty1 ty2
ppr_mono_ty _    (HsTupleTy con tys) = tupleParens std_con (pprWithCommas ppr tys)
  where std_con = case con of
                    HsUnboxedTuple -> UnboxedTuple
                    _              -> BoxedTuple
ppr_mono_ty _    (HsKindSig ty kind) = parens (ppr_mono_lty TopPrec ty <+> dcolon <+> ppr kind)
ppr_mono_ty _    (HsListTy ty)       = brackets (ppr_mono_lty TopPrec ty)
ppr_mono_ty _    (HsPArrTy ty)       = paBrackets (ppr_mono_lty TopPrec ty)
ppr_mono_ty prec (HsIParamTy n ty)   = maybeParen prec FunPrec (ppr n <+> dcolon <+> ppr_mono_lty TopPrec ty)
ppr_mono_ty _    (HsSpliceTy s _)    = pprSplice s
ppr_mono_ty _    (HsCoreTy ty)       = ppr ty
ppr_mono_ty _    (HsExplicitListTy _ tys) = quote $ brackets (interpp'SP tys)
ppr_mono_ty _    (HsExplicitTupleTy _ tys) = quote $ parens (interpp'SP tys)
ppr_mono_ty _    (HsTyLit t)         = ppr_tylit t
ppr_mono_ty _    (HsWildCardTy (AnonWildCard _))     = char '_'
ppr_mono_ty _    (HsWildCardTy (NamedWildCard name)) = ppr name

ppr_mono_ty ctxt_prec (HsEqTy ty1 ty2)
  = maybeParen ctxt_prec TyOpPrec $
    ppr_mono_lty TyOpPrec ty1 <+> char '~' <+> ppr_mono_lty TyOpPrec ty2

ppr_mono_ty ctxt_prec (HsAppsTy tys)
  = maybeParen ctxt_prec TyConPrec $
    hsep (map (ppr_app_ty TopPrec) tys)

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec TyConPrec $
    hsep [ppr_mono_lty FunPrec fun_ty, ppr_mono_lty TyConPrec arg_ty]

ppr_mono_ty ctxt_prec (HsOpTy ty1 (L _ op) ty2)
  = maybeParen ctxt_prec TyOpPrec $
    sep [ ppr_mono_lty TyOpPrec ty1
        , sep [pprInfixOcc op, ppr_mono_lty TyOpPrec ty2 ] ]

ppr_mono_ty _         (HsParTy ty)
  = parens (ppr_mono_lty TopPrec ty)
  -- Put the parens in where the user did
  -- But we still use the precedence stuff to add parens because
  --    toHsType doesn't put in any HsParTys, so we may still need them

ppr_mono_ty ctxt_prec (HsDocTy ty doc)
  = maybeParen ctxt_prec TyOpPrec $
    ppr_mono_lty TyOpPrec ty <+> ppr (unLoc doc)
  -- we pretty print Haddock comments on types as if they were
  -- postfix operators

--------------------------
ppr_fun_ty :: (OutputableBndr name) => TyPrec -> LHsType name -> LHsType name -> SDoc
ppr_fun_ty ctxt_prec ty1 ty2
  = let p1 = ppr_mono_lty FunPrec ty1
        p2 = ppr_mono_lty TopPrec ty2
    in
    maybeParen ctxt_prec FunPrec $
    sep [p1, ptext (sLit "->") <+> p2]

--------------------------
ppr_app_ty :: OutputableBndr name => TyPrec -> HsAppType name -> SDoc
ppr_app_ty _    (HsAppInfix (L _ n))                  = pprInfixOcc n
ppr_app_ty _    (HsAppPrefix (L _ (HsTyVar (L _ n)))) = pprPrefixOcc n
ppr_app_ty ctxt (HsAppPrefix ty)                      = ppr_mono_lty ctxt ty

--------------------------
ppr_tylit :: HsTyLit -> SDoc
ppr_tylit (HsNumTy _ i) = integer i
ppr_tylit (HsStrTy _ s) = text (show s)
