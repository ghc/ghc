{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


HsTypes: Abstract syntax: user-defined types
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module HsTypes (
        HsType(..), NewHsTypeX(..), LHsType, HsKind, LHsKind,
        HsTyVarBndr(..), LHsTyVarBndr,
        LHsQTyVars(..), HsQTvsRn(..),
        HsImplicitBndrs(..), HsIBRn(..),
        HsWildCardBndrs(..),
        LHsSigType, LHsSigWcType, LHsWcType,
        HsTupleSort(..),
        Promoted(..),
        HsContext, LHsContext,
        HsTyLit(..),
        HsIPName(..), hsIPNameFS,
        HsAppType(..),LHsAppType,

        LBangType, BangType,
        HsSrcBang(..), HsImplBang(..),
        SrcStrictness(..), SrcUnpackedness(..),
        getBangType, getBangStrictness,

        ConDeclField(..), LConDeclField, pprConDeclFields,

        HsConDetails(..),

        FieldOcc(..), LFieldOcc, mkFieldOcc,
        AmbiguousFieldOcc(..), mkAmbiguousFieldOcc,
        rdrNameAmbiguousFieldOcc, selectorAmbiguousFieldOcc,
        unambiguousFieldOcc, ambiguousFieldOcc,

        HsWildCardInfo(..), mkAnonWildCardTy, pprAnonWildCard,
        wildCardName, sameWildCard,

        mkHsImplicitBndrs, mkHsWildCardBndrs, hsImplicitBody,
        mkEmptyImplicitBndrs, mkEmptyWildCardBndrs,
        mkHsQTvs, hsQTvExplicit, emptyLHsQTvs, isEmptyLHsQTvs,
        isHsKindedTyVar, hsTvbAllKinded, isLHsForAllTy,
        hsScopedTvs, hsWcScopedTvs, dropWildCards,
        hsTyVarName, hsAllLTyVarNames, hsLTyVarLocNames,
        hsLTyVarName, hsLTyVarLocName, hsExplicitLTyVarNames,
        splitLHsInstDeclTy, getLHsInstDeclHead, getLHsInstDeclClass_maybe,
        splitLHsPatSynTy,
        splitLHsForAllTy, splitLHsQualTy, splitLHsSigmaTy,
        splitHsFunType, splitHsAppsTy,
        splitHsAppTys, getAppsTyHead_maybe, hsTyGetAppHead_maybe,
        mkHsOpTy, mkHsAppTy, mkHsAppTys, mkHsAppsTy,
        ignoreParens, hsSigType, hsSigWcType,
        hsLTyVarBndrToType, hsLTyVarBndrsToTypes,

        -- Printing
        pprHsType, pprHsForAll, pprHsForAllTvs, pprHsForAllExtra,
        pprHsContext, pprHsContextNoArrow, pprHsContextMaybe,
        hsTypeNeedsParens, parenthesizeHsType
    ) where

import GhcPrelude

import {-# SOURCE #-} HsExpr ( HsSplice, pprSplice )

import HsExtension
import HsLit () -- for instances

import Id ( Id )
import Name( Name )
import RdrName ( RdrName )
import NameSet ( NameSet, emptyNameSet )
import DataCon( HsSrcBang(..), HsImplBang(..),
                SrcStrictness(..), SrcUnpackedness(..) )
import TysPrim( funTyConName )
import Type
import HsDoc
import BasicTypes
import SrcLoc
import Outputable
import FastString
import Maybes( isJust )

import Data.Data hiding ( Fixity, Prefix, Infix )
import Data.Maybe ( fromMaybe )

{-
************************************************************************
*                                                                      *
\subsection{Bang annotations}
*                                                                      *
************************************************************************
-}

-- | Located Bang Type
type LBangType pass = Located (BangType pass)

-- | Bang Type
type BangType pass  = HsType pass       -- Bangs are in the HsType data type

getBangType :: LHsType a -> LHsType a
getBangType (L _ (HsBangTy _ _ ty)) = ty
getBangType ty                      = ty

getBangStrictness :: LHsType a -> HsSrcBang
getBangStrictness (L _ (HsBangTy _ s _)) = s
getBangStrictness _ = (HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict)

{-
************************************************************************
*                                                                      *
\subsection{Data types}
*                                                                      *
************************************************************************

This is the syntax for types as seen in type signatures.

Note [HsBSig binder lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a binder (or pattern) decorated with a type or kind,
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
     HsQualTy     represents an /explicit, user-written/ context
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
  the renamer. For example, if the user writes
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

Note [The wildcard story for types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Types can have wildcards in them, to support partial type signatures,
like       f :: Int -> (_ , _a) -> _a

A wildcard in a type can be

  * An anonymous wildcard,
        written '_'
    In HsType this is represented by HsWildCardTy.
    After the renamer, this contains a Name which uniquely
    identifies this particular occurrence.

  * A named wildcard,
        written '_a', '_foo', etc
    In HsType this is represented by (HsTyVar "_a")
    i.e. a perfectly ordinary type variable that happens
         to start with an underscore

Note carefully:

* When NamedWildCards is off, type variables that start with an
  underscore really /are/ ordinary type variables.  And indeed, even
  when NamedWildCards is on you can bind _a explicitly as an ordinary
  type variable:
        data T _a _b = MkT _b _a
  Or even:
        f :: forall _a. _a -> _b
  Here _a is an ordinary forall'd binder, but (With NamedWildCards)
  _b is a named wildcard.  (See the comments in Trac #10982)

* All wildcards, whether named or anonymous, are bound by the
  HsWildCardBndrs construct, which wraps types that are allowed
  to have wildcards.

* After type checking is done, we report what types the wildcards
  got unified with.

-}

-- | Located Haskell Context
type LHsContext pass = Located (HsContext pass)
      -- ^ 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnUnit'

      -- For details on above see note [Api annotations] in ApiAnnotation

-- | Haskell Context
type HsContext pass = [LHsType pass]

-- | Located Haskell Type
type LHsType pass = Located (HsType pass)
      -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma' when
      --   in a list

      -- For details on above see note [Api annotations] in ApiAnnotation

-- | Haskell Kind
type HsKind pass = HsType pass

-- | Located Haskell Kind
type LHsKind pass = Located (HsKind pass)
      -- ^ 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

      -- For details on above see note [Api annotations] in ApiAnnotation

--------------------------------------------------
--             LHsQTyVars
--  The explicitly-quantified binders in a data/type declaration

-- | Located Haskell Type Variable Binder
type LHsTyVarBndr pass = Located (HsTyVarBndr pass)
                         -- See Note [HsType binders]

-- | Located Haskell Quantified Type Variables
data LHsQTyVars pass   -- See Note [HsType binders]
  = HsQTvs { hsq_ext :: XHsQTvs pass

           , hsq_explicit :: [LHsTyVarBndr pass]
                -- Explicit variables, written by the user
                -- See Note [HsForAllTy tyvar binders]
    }
  | XLHsQTyVars (XXLHsQTyVars pass)

data HsQTvsRn
  = HsQTvsRn
           { hsq_implicit :: [Name]
                -- Implicit (dependent) variables

           , hsq_dependent :: NameSet
               -- Which members of hsq_explicit are dependent; that is,
               -- mentioned in the kind of a later hsq_explicit,
               -- or mentioned in a kind in the scope of this HsQTvs
               -- See Note [Dependent LHsQTyVars] in TcHsType
           } deriving Data

type instance XHsQTvs       GhcPs = NoExt
type instance XHsQTvs       GhcRn = HsQTvsRn
type instance XHsQTvs       GhcTc = HsQTvsRn

type instance XXLHsQTyVars  (GhcPass _) = NoExt

mkHsQTvs :: [LHsTyVarBndr GhcPs] -> LHsQTyVars GhcPs
mkHsQTvs tvs = HsQTvs { hsq_ext = noExt, hsq_explicit = tvs }

hsQTvExplicit :: LHsQTyVars pass -> [LHsTyVarBndr pass]
hsQTvExplicit = hsq_explicit

emptyLHsQTvs :: LHsQTyVars GhcRn
emptyLHsQTvs = HsQTvs (HsQTvsRn [] emptyNameSet) []

isEmptyLHsQTvs :: LHsQTyVars GhcRn -> Bool
isEmptyLHsQTvs (HsQTvs (HsQTvsRn [] _) []) = True
isEmptyLHsQTvs _                = False

------------------------------------------------
--            HsImplicitBndrs
-- Used to quantify the implicit binders of a type
--    * Implicit binders of a type signature (LHsSigType/LHsSigWcType)
--    * Patterns in a type/data family instance (HsTyPats)

-- | Haskell Implicit Binders
data HsImplicitBndrs pass thing   -- See Note [HsType binders]
  = HsIB { hsib_ext  :: XHsIB pass thing
         , hsib_body :: thing            -- Main payload (type or list of types)
    }
  | XHsImplicitBndrs (XXHsImplicitBndrs pass thing)

data HsIBRn
  = HsIBRn { hsib_vars :: [Name] -- Implicitly-bound kind & type vars
           , hsib_closed :: Bool -- Taking the hsib_vars into account,
                                 -- is the payload closed? Used in
                                 -- TcHsType.decideKindGeneralisationPlan
    } deriving Data

type instance XHsIB              GhcPs _ = NoExt
type instance XHsIB              GhcRn _ = HsIBRn
type instance XHsIB              GhcTc _ = HsIBRn

type instance XXHsImplicitBndrs  (GhcPass _) _ = NoExt

-- | Haskell Wildcard Binders
data HsWildCardBndrs pass thing
    -- See Note [HsType binders]
    -- See Note [The wildcard story for types]
  = HsWC { hswc_ext :: XHsWC pass thing
                -- after the renamer
                -- Wild cards, both named and anonymous

         , hswc_body :: thing
                -- Main payload (type or list of types)
                -- If there is an extra-constraints wildcard,
                -- it's still there in the hsc_body.
    }
  | XHsWildCardBndrs (XXHsWildCardBndrs pass thing)

type instance XHsWC              GhcPs b = NoExt
type instance XHsWC              GhcRn b = [Name]
type instance XHsWC              GhcTc b = [Name]

type instance XXHsWildCardBndrs  (GhcPass _) b = NoExt

-- | Located Haskell Signature Type
type LHsSigType   pass = HsImplicitBndrs pass (LHsType pass)    -- Implicit only

-- | Located Haskell Wildcard Type
type LHsWcType    pass = HsWildCardBndrs pass (LHsType pass)    -- Wildcard only

-- | Located Haskell Signature Wildcard Type
type LHsSigWcType pass = HsWildCardBndrs pass (LHsSigType pass) -- Both

-- See Note [Representing type signatures]

hsImplicitBody :: HsImplicitBndrs pass thing -> thing
hsImplicitBody (HsIB { hsib_body = body }) = body
hsImplicitBody (XHsImplicitBndrs _) = panic "hsImplicitBody"

hsSigType :: LHsSigType pass -> LHsType pass
hsSigType = hsImplicitBody

hsSigWcType :: LHsSigWcType pass -> LHsType pass
hsSigWcType sig_ty = hsib_body (hswc_body sig_ty)

dropWildCards :: LHsSigWcType pass -> LHsSigType pass
-- Drop the wildcard part of a LHsSigWcType
dropWildCards sig_ty = hswc_body sig_ty

{- Note [Representing type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HsSigType is used to represent an explicit user type signature
such as   f :: a -> a
     or   g (x :: a -> a) = x

A HsSigType is just a HsImplicitBndrs wrapping a LHsType.
 * The HsImplicitBndrs binds the /implicitly/ quantified tyvars
 * The LHsType binds the /explicitly/ quantified tyvars

E.g. For a signature like
   f :: forall (a::k). blah
we get
   HsIB { hsib_vars = [k]
        , hsib_body = HsForAllTy { hst_bndrs = [(a::*)]
                                 , hst_body = blah }
The implicit kind variable 'k' is bound by the HsIB;
the explicitly forall'd tyvar 'a' is bound by the HsForAllTy
-}

mkHsImplicitBndrs :: thing -> HsImplicitBndrs GhcPs thing
mkHsImplicitBndrs x = HsIB { hsib_ext  = noExt
                           , hsib_body = x }

mkHsWildCardBndrs :: thing -> HsWildCardBndrs GhcPs thing
mkHsWildCardBndrs x = HsWC { hswc_body = x
                           , hswc_ext  = noExt }

-- Add empty binders.  This is a bit suspicious; what if
-- the wrapped thing had free type variables?
mkEmptyImplicitBndrs :: thing -> HsImplicitBndrs GhcRn thing
mkEmptyImplicitBndrs x = HsIB { hsib_ext = HsIBRn
                                  { hsib_vars   = []
                                  , hsib_closed = False }
                              , hsib_body = x }

mkEmptyWildCardBndrs :: thing -> HsWildCardBndrs GhcRn thing
mkEmptyWildCardBndrs x = HsWC { hswc_body = x
                              , hswc_ext  = [] }


--------------------------------------------------
-- | These names are used early on to store the names of implicit
-- parameters.  They completely disappear after type-checking.
newtype HsIPName = HsIPName FastString
  deriving( Eq, Data )

hsIPNameFS :: HsIPName -> FastString
hsIPNameFS (HsIPName n) = n

instance Outputable HsIPName where
    ppr (HsIPName n) = char '?' <> ftext n -- Ordinary implicit parameters

instance OutputableBndr HsIPName where
    pprBndr _ n   = ppr n         -- Simple for now
    pprInfixOcc  n = ppr n
    pprPrefixOcc n = ppr n

--------------------------------------------------

-- | Haskell Type Variable Binder
data HsTyVarBndr pass
  = UserTyVar        -- no explicit kinding
         (XUserTyVar pass)
         (Located (IdP pass))
        -- See Note [Located RdrNames] in HsExpr
  | KindedTyVar
         (XKindedTyVar pass)
         (Located (IdP pass))
         (LHsKind pass)  -- The user-supplied kind signature
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --          'ApiAnnotation.AnnDcolon', 'ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation

  | XTyVarBndr
      (XXTyVarBndr pass)

type instance XUserTyVar    (GhcPass _) = NoExt
type instance XKindedTyVar  (GhcPass _) = NoExt
type instance XXTyVarBndr   (GhcPass _) = NoExt

-- | Does this 'HsTyVarBndr' come with an explicit kind annotation?
isHsKindedTyVar :: HsTyVarBndr pass -> Bool
isHsKindedTyVar (UserTyVar {})   = False
isHsKindedTyVar (KindedTyVar {}) = True
isHsKindedTyVar (XTyVarBndr{})   = panic "isHsKindedTyVar"

-- | Do all type variables in this 'LHsQTyVars' come with kind annotations?
hsTvbAllKinded :: LHsQTyVars pass -> Bool
hsTvbAllKinded = all (isHsKindedTyVar . unLoc) . hsQTvExplicit

-- | Haskell Type
data HsType pass
  = HsForAllTy   -- See Note [HsType binders]
      { hst_xforall :: XForAllTy pass,
        hst_bndrs   :: [LHsTyVarBndr pass]
                                       -- Explicit, user-supplied 'forall a b c'
      , hst_body    :: LHsType pass      -- body type
      }
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnForall',
      --         'ApiAnnotation.AnnDot','ApiAnnotation.AnnDarrow'
      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsQualTy   -- See Note [HsType binders]
      { hst_xqual :: XQualTy pass
      , hst_ctxt  :: LHsContext pass       -- Context C => blah
      , hst_body  :: LHsType pass }

  | HsTyVar             (XTyVar pass)
                        Promoted -- whether explicitly promoted, for the pretty
                                 -- printer
                        (Located (IdP pass))
                  -- Type variable, type constructor, or data constructor
                  -- see Note [Promotions (HsTyVar)]
                  -- See Note [Located RdrNames] in HsExpr
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsAppsTy            (XAppsTy pass)
                        [LHsAppType pass] -- Used only before renaming,
                                          -- Note [HsAppsTy]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

  | HsAppTy             (XAppTy pass)
                        (LHsType pass)
                        (LHsType pass)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsFunTy             (XFunTy pass)
                        (LHsType pass)   -- function type
                        (LHsType pass)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRarrow',

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsListTy            (XListTy pass)
                        (LHsType pass)  -- Element type
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
      --         'ApiAnnotation.AnnClose' @']'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsPArrTy            (XPArrTy pass)
                        (LHsType pass)  -- Elem. type of parallel array: [:t:]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
      --         'ApiAnnotation.AnnClose' @':]'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsTupleTy           (XTupleTy pass)
                        HsTupleSort
                        [LHsType pass]  -- Element types (length gives arity)
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'(' or '(#'@,
    --         'ApiAnnotation.AnnClose' @')' or '#)'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsSumTy             (XSumTy pass)
                        [LHsType pass]  -- Element types (length gives arity)
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'(#'@,
    --         'ApiAnnotation.AnnClose' '#)'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsOpTy              (XOpTy pass)
                        (LHsType pass) (Located (IdP pass)) (LHsType pass)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsParTy             (XParTy pass)
                        (LHsType pass)   -- See Note [Parens in HsSyn] in HsExpr
        -- Parenthesis preserved for the precedence re-arrangement in RnTypes
        -- It's important that a * (b + c) doesn't get rearranged to (a*b) + c!
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
      --         'ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsIParamTy          (XIParamTy pass)
                        (Located HsIPName) -- (?x :: ty)
                        (LHsType pass)   -- Implicit parameters as they occur in
                                         -- contexts
      -- ^
      -- > (?x :: ty)
      --
      -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsEqTy              (XEqTy pass)
                        (LHsType pass)   -- ty1 ~ ty2
                        (LHsType pass)   -- Always allowed even without
                                         -- TypeOperators, and has special
                                         -- kinding rule
      -- ^
      -- > ty1 ~ ty2
      --
      -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsKindSig           (XKindSig pass)
                        (LHsType pass)  -- (ty :: kind)
                        (LHsKind pass)  -- A type with a kind signature
      -- ^
      -- > (ty :: kind)
      --
      -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
      --         'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsSpliceTy          (XSpliceTy pass)
                        (HsSplice pass)   -- Includes quasi-quotes
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'$('@,
      --         'ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsDocTy             (XDocTy pass)
                        (LHsType pass) LHsDocString -- A documented type
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsBangTy    (XBangTy pass)
                HsSrcBang (LHsType pass)   -- Bang-style type annotations
      -- ^ - 'ApiAnnotation.AnnKeywordId' :
      --         'ApiAnnotation.AnnOpen' @'{-\# UNPACK' or '{-\# NOUNPACK'@,
      --         'ApiAnnotation.AnnClose' @'#-}'@
      --         'ApiAnnotation.AnnBang' @\'!\'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsRecTy     (XRecTy pass)
                [LConDeclField pass]    -- Only in data type declarations
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{'@,
      --         'ApiAnnotation.AnnClose' @'}'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  -- | HsCoreTy (XCoreTy pass) Type -- An escape hatch for tunnelling a *closed*
  --                                -- Core Type through HsSyn.
  --     -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsExplicitListTy       -- A promoted explicit list
        (XExplicitListTy pass)
        Promoted           -- whether explcitly promoted, for pretty printer
        [LHsType pass]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @"'["@,
      --         'ApiAnnotation.AnnClose' @']'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsExplicitTupleTy      -- A promoted explicit tuple
        (XExplicitTupleTy pass)
        [LHsType pass]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @"'("@,
      --         'ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsTyLit (XTyLit pass) HsTyLit      -- A promoted numeric literal.
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsWildCardTy (XWildCardTy pass)  -- A type wildcard
      -- See Note [The wildcard story for types]
      -- A anonymous wild card ('_'). A fresh Name is generated for
      -- each individual anonymous wildcard during renaming
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  -- For adding new constructors via Trees that Grow
  | XHsType
      (XXType pass)

data NewHsTypeX
  = NHsCoreTy Type -- An escape hatch for tunnelling a *closed*
                   -- Core Type through HsSyn.
    deriving Data
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

instance Outputable NewHsTypeX where
  ppr (NHsCoreTy ty) = ppr ty

type instance XForAllTy        (GhcPass _) = NoExt
type instance XQualTy          (GhcPass _) = NoExt
type instance XTyVar           (GhcPass _) = NoExt
type instance XAppsTy          (GhcPass _) = NoExt
type instance XAppTy           (GhcPass _) = NoExt
type instance XFunTy           (GhcPass _) = NoExt
type instance XListTy          (GhcPass _) = NoExt
type instance XPArrTy          (GhcPass _) = NoExt
type instance XTupleTy         (GhcPass _) = NoExt
type instance XSumTy           (GhcPass _) = NoExt
type instance XOpTy            (GhcPass _) = NoExt
type instance XParTy           (GhcPass _) = NoExt
type instance XIParamTy        (GhcPass _) = NoExt
type instance XEqTy            (GhcPass _) = NoExt
type instance XKindSig         (GhcPass _) = NoExt

type instance XSpliceTy        GhcPs = NoExt
type instance XSpliceTy        GhcRn = NoExt
type instance XSpliceTy        GhcTc = Kind

type instance XDocTy           (GhcPass _) = NoExt
type instance XBangTy          (GhcPass _) = NoExt
type instance XRecTy           (GhcPass _) = NoExt

type instance XExplicitListTy  GhcPs = NoExt
type instance XExplicitListTy  GhcRn = NoExt
type instance XExplicitListTy  GhcTc = Kind

type instance XExplicitTupleTy GhcPs = NoExt
type instance XExplicitTupleTy GhcRn = NoExt
type instance XExplicitTupleTy GhcTc = [Kind]

type instance XTyLit           (GhcPass _) = NoExt

type instance XWildCardTy      GhcPs = NoExt
type instance XWildCardTy      GhcRn = HsWildCardInfo
type instance XWildCardTy      GhcTc = HsWildCardInfo

type instance XXType         (GhcPass _) = NewHsTypeX


-- Note [Literal source text] in BasicTypes for SourceText fields in
-- the following
-- | Haskell Type Literal
data HsTyLit
  = HsNumTy SourceText Integer
  | HsStrTy SourceText FastString
    deriving Data

newtype HsWildCardInfo        -- See Note [The wildcard story for types]
    = AnonWildCard (Located Name)
      deriving Data
      -- A anonymous wild card ('_'). A fresh Name is generated for
      -- each individual anonymous wildcard during renaming

-- | Located Haskell Application Type
type LHsAppType pass = Located (HsAppType pass)
      -- ^ 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSimpleQuote'

-- | Haskell Application Type
data HsAppType pass
  = HsAppInfix (XAppInfix pass)
               (Located (IdP pass)) -- either a symbol or an id in backticks
  | HsAppPrefix (XAppPrefix pass)
                (LHsType pass)      -- anything else, including things like (+)
  | XAppType
      (XXAppType pass)

type instance XAppInfix   (GhcPass _) = NoExt
type instance XAppPrefix  (GhcPass _) = NoExt
type instance XXAppType   (GhcPass _) = NoExt

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (HsAppType p) where
  ppr = ppr_app_ty

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
In GHC 8.0, Qualified will no longer bind variables
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

  The 'Promoted' field in an HsTyVar captures whether the type was promoted in
  the source code by prefixing an apostrophe.

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

-- | Haskell Tuple Sort
data HsTupleSort = HsUnboxedTuple
                 | HsBoxedTuple
                 | HsConstraintTuple
                 | HsBoxedOrConstraintTuple
                 deriving Data


-- | Promoted data types.
data Promoted = Promoted
              | NotPromoted
              deriving (Data, Eq, Show)

-- | Located Constructor Declaration Field
type LConDeclField pass = Located (ConDeclField pass)
      -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma' when
      --   in a list

      -- For details on above see note [Api annotations] in ApiAnnotation

-- | Constructor Declaration Field
data ConDeclField pass  -- Record fields have Haddoc docs on them
  = ConDeclField { cd_fld_ext  :: XConDeclField pass,
                   cd_fld_names :: [LFieldOcc pass],
                                   -- ^ See Note [ConDeclField passs]
                   cd_fld_type :: LBangType pass,
                   cd_fld_doc  :: Maybe LHsDocString }
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

      -- For details on above see note [Api annotations] in ApiAnnotation
  | XConDeclField (XXConDeclField pass)

type instance XConDeclField  (GhcPass _) = NoExt
type instance XXConDeclField (GhcPass _) = NoExt

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (ConDeclField p) where
  ppr (ConDeclField _ fld_n fld_ty _) = ppr fld_n <+> dcolon <+> ppr fld_ty
  ppr (XConDeclField x) = ppr x

-- HsConDetails is used for patterns/expressions *and* for data type
-- declarations
-- | Haskell Constructor Details
data HsConDetails arg rec
  = PrefixCon [arg]             -- C p1 p2 p3
  | RecCon    rec               -- C { x = p1, y = p2 }
  | InfixCon  arg arg           -- p1 `C` p2
  deriving Data

instance (Outputable arg, Outputable rec)
         => Outputable (HsConDetails arg rec) where
  ppr (PrefixCon args) = text "PrefixCon" <+> ppr args
  ppr (RecCon rec)     = text "RecCon:" <+> ppr rec
  ppr (InfixCon l r)   = text "InfixCon:" <+> ppr [l, r]

{-
Note [ConDeclField passs]
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
hsWcScopedTvs :: LHsSigWcType GhcRn -> [Name]
-- Get the lexically-scoped type variables of a HsSigType
--  - the explicitly-given forall'd type variables
--  - the implicitly-bound kind variables
--  - the named wildcars; see Note [Scoping of named wildcards]
-- because they scope in the same way
hsWcScopedTvs sig_ty
  | HsWC { hswc_ext = nwcs, hswc_body = sig_ty1 }  <- sig_ty
  , HsIB { hsib_ext = HsIBRn { hsib_vars = vars}
         , hsib_body = sig_ty2 } <- sig_ty1
  = case sig_ty2 of
      L _ (HsForAllTy { hst_bndrs = tvs }) -> vars ++ nwcs ++
                                              map hsLTyVarName tvs
               -- include kind variables only if the type is headed by forall
               -- (this is consistent with GHC 7 behaviour)
      _                                    -> nwcs
hsWcScopedTvs (HsWC _ (XHsImplicitBndrs _)) = panic "hsWcScopedTvs"
hsWcScopedTvs (XHsWildCardBndrs _) = panic "hsWcScopedTvs"

hsScopedTvs :: LHsSigType GhcRn -> [Name]
-- Same as hsWcScopedTvs, but for a LHsSigType
hsScopedTvs sig_ty
  | HsIB { hsib_ext = HsIBRn { hsib_vars = vars }
         , hsib_body = sig_ty2 } <- sig_ty
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
hsTyVarName :: HsTyVarBndr pass -> IdP pass
hsTyVarName (UserTyVar _ (L _ n))     = n
hsTyVarName (KindedTyVar _ (L _ n) _) = n
hsTyVarName (XTyVarBndr{}) = panic "hsTyVarName"

hsLTyVarName :: LHsTyVarBndr pass -> IdP pass
hsLTyVarName = hsTyVarName . unLoc

hsExplicitLTyVarNames :: LHsQTyVars pass -> [IdP pass]
-- Explicit variables only
hsExplicitLTyVarNames qtvs = map hsLTyVarName (hsQTvExplicit qtvs)

hsAllLTyVarNames :: LHsQTyVars GhcRn -> [Name]
-- All variables
hsAllLTyVarNames (HsQTvs { hsq_ext = HsQTvsRn { hsq_implicit = kvs }
                         , hsq_explicit = tvs })
  = kvs ++ map hsLTyVarName tvs
hsAllLTyVarNames (XLHsQTyVars _) = panic "hsAllLTyVarNames"

hsLTyVarLocName :: LHsTyVarBndr pass -> Located (IdP pass)
hsLTyVarLocName = fmap hsTyVarName

hsLTyVarLocNames :: LHsQTyVars pass -> [Located (IdP pass)]
hsLTyVarLocNames qtvs = map hsLTyVarLocName (hsQTvExplicit qtvs)

-- | Convert a LHsTyVarBndr to an equivalent LHsType.
hsLTyVarBndrToType :: LHsTyVarBndr (GhcPass p) -> LHsType (GhcPass p)
hsLTyVarBndrToType = fmap cvt
  where cvt (UserTyVar _ n) = HsTyVar noExt NotPromoted n
        cvt (KindedTyVar _ (L name_loc n) kind)
          = HsKindSig noExt
                   (L name_loc (HsTyVar noExt NotPromoted (L name_loc n))) kind
        cvt (XTyVarBndr{}) = panic "hsLTyVarBndrToType"

-- | Convert a LHsTyVarBndrs to a list of types.
-- Works on *type* variable only, no kind vars.
hsLTyVarBndrsToTypes :: LHsQTyVars (GhcPass p) -> [LHsType (GhcPass p)]
hsLTyVarBndrsToTypes (HsQTvs { hsq_explicit = tvbs }) = map hsLTyVarBndrToType tvbs
hsLTyVarBndrsToTypes (XLHsQTyVars _) = panic "hsLTyVarBndrsToTypes"

---------------------
wildCardName :: HsWildCardInfo -> Name
wildCardName (AnonWildCard  (L _ n)) = n

-- Two wild cards are the same when they have the same location
sameWildCard :: Located HsWildCardInfo -> Located HsWildCardInfo -> Bool
sameWildCard (L l1 (AnonWildCard _))   (L l2 (AnonWildCard _))   = l1 == l2

ignoreParens :: LHsType pass -> LHsType pass
ignoreParens (L _ (HsParTy _ ty)) = ignoreParens ty
ignoreParens ty                   = ty

isLHsForAllTy :: LHsType p -> Bool
isLHsForAllTy (L _ (HsForAllTy {})) = True
isLHsForAllTy _                     = False

{-
************************************************************************
*                                                                      *
                Building types
*                                                                      *
************************************************************************
-}

mkAnonWildCardTy :: HsType GhcPs
mkAnonWildCardTy = HsWildCardTy noExt

mkHsOpTy :: LHsType (GhcPass p) -> Located (IdP (GhcPass p))
         -> LHsType (GhcPass p) -> HsType (GhcPass p)
mkHsOpTy ty1 op ty2 = HsOpTy noExt ty1 op ty2

mkHsAppTy :: LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
mkHsAppTy t1 t2
  = addCLoc t1 t2 (HsAppTy noExt t1 (parenthesizeHsType appPrec t2))

mkHsAppTys :: LHsType (GhcPass p) -> [LHsType (GhcPass p)]
           -> LHsType (GhcPass p)
mkHsAppTys = foldl mkHsAppTy

mkHsAppsTy :: [LHsAppType GhcPs] -> HsType GhcPs
-- In the common case of a singleton non-operator,
-- avoid the clutter of wrapping in a HsAppsTy
mkHsAppsTy [L _ (HsAppPrefix _ (L _ ty))] = ty
mkHsAppsTy app_tys                        = HsAppsTy NoExt app_tys

{-
************************************************************************
*                                                                      *
                Decomposing HsTypes
*                                                                      *
************************************************************************
-}

---------------------------------
-- splitHsFunType decomposes a type (t1 -> t2 ... -> tn)
-- Breaks up any parens in the result type:
--      splitHsFunType (a -> (b -> c)) = ([a,b], c)
-- Also deals with (->) t1 t2; that is why it only works on LHsType Name
--   (see Trac #9096)
splitHsFunType :: LHsType GhcRn -> ([LHsType GhcRn], LHsType GhcRn)
splitHsFunType (L _ (HsParTy _ ty))
  = splitHsFunType ty

splitHsFunType (L _ (HsFunTy _ x y))
  | (args, res) <- splitHsFunType y
  = (x:args, res)

splitHsFunType orig_ty@(L _ (HsAppTy _ t1 t2))
  = go t1 [t2]
  where  -- Look for (->) t1 t2, possibly with parenthesisation
    go (L _ (HsTyVar _ _ (L _ fn))) tys | fn == funTyConName
                                 , [t1,t2] <- tys
                                 , (args, res) <- splitHsFunType t2
                                 = (t1:args, res)
    go (L _ (HsAppTy _ t1 t2)) tys = go t1 (t2:tys)
    go (L _ (HsParTy _ ty))    tys = go ty tys
    go _                       _   = ([], orig_ty)  -- Failure to match

splitHsFunType other = ([], other)

--------------------------------
-- | Retrieves the head of an HsAppsTy, if this can be done unambiguously,
-- without consulting fixities.
getAppsTyHead_maybe :: [LHsAppType (GhcPass p)]
                    -> Maybe ( LHsType (GhcPass p)
                             , [LHsType (GhcPass p)], LexicalFixity)
getAppsTyHead_maybe tys = case splitHsAppsTy tys of
  ([app1:apps], []) ->  -- no symbols, some normal types
    Just (mkHsAppTys app1 apps, [], Prefix)
  ([app1l:appsl, app1r:appsr], [L loc op]) ->  -- one operator
    Just ( L loc (HsTyVar noExt NotPromoted (L loc op))
         , [mkHsAppTys app1l appsl, mkHsAppTys app1r appsr], Infix)
  _ -> -- can't figure it out
    Nothing

-- | Splits a [HsAppType pass] (the payload of an HsAppsTy) into regions of
-- prefix types (normal types) and infix operators.
-- If @splitHsAppsTy tys = (non_syms, syms)@, then @tys@ starts with the first
-- element of @non_syms@ followed by the first element of @syms@ followed by
-- the next element of @non_syms@, etc. It is guaranteed that the non_syms list
-- has one more element than the syms list.
splitHsAppsTy :: [LHsAppType pass] -> ([[LHsType pass]], [Located (IdP pass)])
splitHsAppsTy = go [] [] []
  where
    go acc acc_non acc_sym [] = (reverse (reverse acc : acc_non), reverse acc_sym)
    go acc acc_non acc_sym (L _ (HsAppPrefix _ ty) : rest)
      = go (ty : acc) acc_non acc_sym rest
    go acc acc_non acc_sym (L _ (HsAppInfix _ op) : rest)
      = go [] (reverse acc : acc_non) (op : acc_sym) rest
    go _ _ _ (L _ (XAppType _):_) = panic "splitHsAppsTy"

-- Retrieve the name of the "head" of a nested type application
-- somewhat like splitHsAppTys, but a little more thorough
-- used to examine the result of a GADT-like datacon, so it doesn't handle
-- *all* cases (like lists, tuples, (~), etc.)
hsTyGetAppHead_maybe :: LHsType (GhcPass p)
                     -> Maybe (Located (IdP (GhcPass p)), [LHsType (GhcPass p)])
hsTyGetAppHead_maybe = go []
  where
    go tys (L _ (HsTyVar _ _ ln))          = Just (ln, tys)
    go tys (L _ (HsAppsTy _ apps))
      | Just (head, args, _) <- getAppsTyHead_maybe apps
                                           = go (args ++ tys) head
    go tys (L _ (HsAppTy _ l r))           = go (r : tys) l
    go tys (L _ (HsOpTy _ l (L loc n) r))  = Just (L loc n, l : r : tys)
    go tys (L _ (HsParTy _ t))             = go tys t
    go tys (L _ (HsKindSig _ t _))         = go tys t
    go _   _                             = Nothing

splitHsAppTys :: LHsType GhcRn -> [LHsType GhcRn]
              -> (LHsType GhcRn, [LHsType GhcRn])
  -- no need to worry about HsAppsTy here
splitHsAppTys (L _ (HsAppTy _ f a)) as = splitHsAppTys f (a:as)
splitHsAppTys (L _ (HsParTy _ f))   as = splitHsAppTys f as
splitHsAppTys f                     as = (f,as)

--------------------------------
splitLHsPatSynTy :: LHsType pass
                 -> ( [LHsTyVarBndr pass]    -- universals
                    , LHsContext pass        -- required constraints
                    , [LHsTyVarBndr pass]    -- existentials
                    , LHsContext pass        -- provided constraints
                    , LHsType pass)          -- body type
splitLHsPatSynTy ty = (univs, reqs, exis, provs, ty4)
  where
    (univs, ty1) = splitLHsForAllTy ty
    (reqs,  ty2) = splitLHsQualTy ty1
    (exis,  ty3) = splitLHsForAllTy ty2
    (provs, ty4) = splitLHsQualTy ty3

splitLHsSigmaTy :: LHsType pass
                -> ([LHsTyVarBndr pass], LHsContext pass, LHsType pass)
splitLHsSigmaTy ty
  | (tvs, ty1)  <- splitLHsForAllTy ty
  , (ctxt, ty2) <- splitLHsQualTy ty1
  = (tvs, ctxt, ty2)

splitLHsForAllTy :: LHsType pass -> ([LHsTyVarBndr pass], LHsType pass)
splitLHsForAllTy (L _ (HsParTy _ ty)) = splitLHsForAllTy ty
splitLHsForAllTy (L _ (HsForAllTy { hst_bndrs = tvs, hst_body = body })) = (tvs, body)
splitLHsForAllTy body              = ([], body)

splitLHsQualTy :: LHsType pass -> (LHsContext pass, LHsType pass)
splitLHsQualTy (L _ (HsParTy _ ty)) = splitLHsQualTy ty
splitLHsQualTy (L _ (HsQualTy { hst_ctxt = ctxt, hst_body = body })) = (ctxt,     body)
splitLHsQualTy body              = (noLoc [], body)

splitLHsInstDeclTy :: LHsSigType GhcRn
                   -> ([Name], LHsContext GhcRn, LHsType GhcRn)
-- Split up an instance decl type, returning the pieces
splitLHsInstDeclTy (HsIB { hsib_ext = HsIBRn { hsib_vars = itkvs }
                         , hsib_body = inst_ty })
  | (tvs, cxt, body_ty) <- splitLHsSigmaTy inst_ty
  = (itkvs ++ map hsLTyVarName tvs, cxt, body_ty)
         -- Return implicitly bound type and kind vars
         -- For an instance decl, all of them are in scope
splitLHsInstDeclTy (XHsImplicitBndrs _) = panic "splitLHsInstDeclTy"

getLHsInstDeclHead :: LHsSigType pass -> LHsType pass
getLHsInstDeclHead inst_ty
  | (_tvs, _cxt, body_ty) <- splitLHsSigmaTy (hsSigType inst_ty)
  = body_ty

getLHsInstDeclClass_maybe :: LHsSigType (GhcPass p)
                          -> Maybe (Located (IdP (GhcPass p)))
-- Works on (HsSigType RdrName)
getLHsInstDeclClass_maybe inst_ty
  = do { let head_ty = getLHsInstDeclHead inst_ty
       ; (cls, _) <- hsTyGetAppHead_maybe head_ty
       ; return cls }

{-
************************************************************************
*                                                                      *
                FieldOcc
*                                                                      *
************************************************************************
-}

-- | Located Field Occurrence
type LFieldOcc pass = Located (FieldOcc pass)

-- | Field Occurrence
--
-- Represents an *occurrence* of an unambiguous field.  We store
-- both the 'RdrName' the user originally wrote, and after the
-- renamer, the selector function.
data FieldOcc pass = FieldOcc { extFieldOcc     :: XFieldOcc pass
                              , rdrNameFieldOcc :: Located RdrName
                                 -- ^ See Note [Located RdrNames] in HsExpr
                              }

  | XFieldOcc
      (XXFieldOcc pass)
deriving instance (p ~ GhcPass pass, Eq (XFieldOcc p)) => Eq  (FieldOcc p)
deriving instance (p ~ GhcPass pass, Ord (XFieldOcc p)) => Ord (FieldOcc p)

type instance XFieldOcc GhcPs = NoExt
type instance XFieldOcc GhcRn = Name
type instance XFieldOcc GhcTc = Id

type instance XXFieldOcc (GhcPass _) = NoExt

instance Outputable (FieldOcc pass) where
  ppr = ppr . rdrNameFieldOcc

mkFieldOcc :: Located RdrName -> FieldOcc GhcPs
mkFieldOcc rdr = FieldOcc noExt rdr


-- | Ambiguous Field Occurrence
--
-- Represents an *occurrence* of a field that is potentially
-- ambiguous after the renamer, with the ambiguity resolved by the
-- typechecker.  We always store the 'RdrName' that the user
-- originally wrote, and store the selector function after the renamer
-- (for unambiguous occurrences) or the typechecker (for ambiguous
-- occurrences).
--
-- See Note [HsRecField and HsRecUpdField] in HsPat and
-- Note [Disambiguating record fields] in TcExpr.
-- See Note [Located RdrNames] in HsExpr
data AmbiguousFieldOcc pass
  = Unambiguous (XUnambiguous pass) (Located RdrName)
  | Ambiguous   (XAmbiguous pass)   (Located RdrName)
  | XAmbiguousFieldOcc (XXAmbiguousFieldOcc pass)

type instance XUnambiguous GhcPs = NoExt
type instance XUnambiguous GhcRn = Name
type instance XUnambiguous GhcTc = Id

type instance XAmbiguous GhcPs = NoExt
type instance XAmbiguous GhcRn = NoExt
type instance XAmbiguous GhcTc = Id

type instance XXAmbiguousFieldOcc (GhcPass _) = NoExt

instance p ~ GhcPass pass => Outputable (AmbiguousFieldOcc p) where
  ppr = ppr . rdrNameAmbiguousFieldOcc

instance p ~ GhcPass pass => OutputableBndr (AmbiguousFieldOcc p) where
  pprInfixOcc  = pprInfixOcc . rdrNameAmbiguousFieldOcc
  pprPrefixOcc = pprPrefixOcc . rdrNameAmbiguousFieldOcc

mkAmbiguousFieldOcc :: Located RdrName -> AmbiguousFieldOcc GhcPs
mkAmbiguousFieldOcc rdr = Unambiguous noExt rdr

rdrNameAmbiguousFieldOcc :: AmbiguousFieldOcc (GhcPass p) -> RdrName
rdrNameAmbiguousFieldOcc (Unambiguous _ (L _ rdr)) = rdr
rdrNameAmbiguousFieldOcc (Ambiguous   _ (L _ rdr)) = rdr
rdrNameAmbiguousFieldOcc (XAmbiguousFieldOcc _)
  = panic "rdrNameAmbiguousFieldOcc"

selectorAmbiguousFieldOcc :: AmbiguousFieldOcc GhcTc -> Id
selectorAmbiguousFieldOcc (Unambiguous sel _) = sel
selectorAmbiguousFieldOcc (Ambiguous   sel _) = sel
selectorAmbiguousFieldOcc (XAmbiguousFieldOcc _)
  = panic "selectorAmbiguousFieldOcc"

unambiguousFieldOcc :: AmbiguousFieldOcc GhcTc -> FieldOcc GhcTc
unambiguousFieldOcc (Unambiguous rdr sel) = FieldOcc rdr sel
unambiguousFieldOcc (Ambiguous   rdr sel) = FieldOcc rdr sel
unambiguousFieldOcc (XAmbiguousFieldOcc _) = panic "unambiguousFieldOcc"

ambiguousFieldOcc :: FieldOcc GhcTc -> AmbiguousFieldOcc GhcTc
ambiguousFieldOcc (FieldOcc sel rdr) = Unambiguous sel rdr
ambiguousFieldOcc (XFieldOcc _) = panic "ambiguousFieldOcc"

{-
************************************************************************
*                                                                      *
\subsection{Pretty printing}
*                                                                      *
************************************************************************
-}

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (HsType p) where
    ppr ty = pprHsType ty

instance Outputable HsTyLit where
    ppr = ppr_tylit

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (LHsQTyVars p) where
    ppr (HsQTvs { hsq_explicit = tvs }) = interppSP tvs
    ppr (XLHsQTyVars x) = ppr x

instance (p ~ GhcPass pass, OutputableBndrId p)
       => Outputable (HsTyVarBndr p) where
    ppr (UserTyVar _ n)     = ppr n
    ppr (KindedTyVar _ n k) = parens $ hsep [ppr n, dcolon, ppr k]
    ppr (XTyVarBndr n)      = ppr n

instance (p ~ GhcPass pass,Outputable thing)
       => Outputable (HsImplicitBndrs p thing) where
    ppr (HsIB { hsib_body = ty }) = ppr ty
    ppr (XHsImplicitBndrs x) = ppr x

instance (p ~ GhcPass pass,Outputable thing)
       => Outputable (HsWildCardBndrs p thing) where
    ppr (HsWC { hswc_body = ty }) = ppr ty
    ppr (XHsWildCardBndrs x) = ppr x

instance Outputable HsWildCardInfo where
    ppr (AnonWildCard _)  = char '_'

pprAnonWildCard :: SDoc
pprAnonWildCard = char '_'

pprHsForAll :: (OutputableBndrId (GhcPass p))
            => [LHsTyVarBndr (GhcPass p)] -> LHsContext (GhcPass p) -> SDoc
pprHsForAll = pprHsForAllExtra Nothing

-- | Version of 'pprHsForAll' that can also print an extra-constraints
-- wildcard, e.g. @_ => a -> Bool@ or @(Show a, _) => a -> String@. This
-- underscore will be printed when the 'Maybe SrcSpan' argument is a 'Just'
-- containing the location of the extra-constraints wildcard. A special
-- function for this is needed, as the extra-constraints wildcard is removed
-- from the actual context and type, and stored in a separate field, thus just
-- printing the type will not print the extra-constraints wildcard.
pprHsForAllExtra :: (OutputableBndrId (GhcPass p))
                 => Maybe SrcSpan -> [LHsTyVarBndr (GhcPass p)]
                 -> LHsContext (GhcPass p) -> SDoc
pprHsForAllExtra extra qtvs cxt
  = pprHsForAllTvs qtvs <+> pprHsContextExtra show_extra (unLoc cxt)
  where
    show_extra = isJust extra

pprHsForAllTvs :: (OutputableBndrId (GhcPass p))
               => [LHsTyVarBndr (GhcPass p)] -> SDoc
pprHsForAllTvs qtvs
  | null qtvs = whenPprDebug (forAllLit <+> dot)
  | otherwise = forAllLit <+> interppSP qtvs <> dot

pprHsContext :: (OutputableBndrId (GhcPass p)) => HsContext (GhcPass p) -> SDoc
pprHsContext = maybe empty (<+> darrow) . pprHsContextMaybe

pprHsContextNoArrow :: (OutputableBndrId (GhcPass p))
                    => HsContext (GhcPass p) -> SDoc
pprHsContextNoArrow = fromMaybe empty . pprHsContextMaybe

pprHsContextMaybe :: (OutputableBndrId (GhcPass p))
                  => HsContext (GhcPass p) -> Maybe SDoc
pprHsContextMaybe []         = Nothing
pprHsContextMaybe [L _ pred] = Just $ ppr_mono_ty pred
pprHsContextMaybe cxt        = Just $ parens (interpp'SP cxt)

-- For use in a HsQualTy, which always gets printed if it exists.
pprHsContextAlways :: (OutputableBndrId (GhcPass p))
                   => HsContext (GhcPass p) -> SDoc
pprHsContextAlways []  = parens empty <+> darrow
pprHsContextAlways [L _ ty] = ppr_mono_ty ty <+> darrow
pprHsContextAlways cxt = parens (interpp'SP cxt) <+> darrow

-- True <=> print an extra-constraints wildcard, e.g. @(Show a, _) =>@
pprHsContextExtra :: (OutputableBndrId (GhcPass p))
                  => Bool -> HsContext (GhcPass p) -> SDoc
pprHsContextExtra show_extra ctxt
  | not show_extra
  = pprHsContext ctxt
  | null ctxt
  = char '_' <+> darrow
  | otherwise
  = parens (sep (punctuate comma ctxt')) <+> darrow
  where
    ctxt' = map ppr ctxt ++ [char '_']

pprConDeclFields :: (OutputableBndrId (GhcPass p))
                 => [LConDeclField (GhcPass p)] -> SDoc
pprConDeclFields fields = braces (sep (punctuate comma (map ppr_fld fields)))
  where
    ppr_fld (L _ (ConDeclField { cd_fld_names = ns, cd_fld_type = ty,
                                 cd_fld_doc = doc }))
        = ppr_names ns <+> dcolon <+> ppr ty <+> ppr_mbDoc doc
    ppr_fld (L _ (XConDeclField x)) = ppr x
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

pprHsType :: (OutputableBndrId (GhcPass p)) => HsType (GhcPass p) -> SDoc
pprHsType ty = ppr_mono_ty ty

ppr_mono_lty :: (OutputableBndrId (GhcPass p)) => LHsType (GhcPass p) -> SDoc
ppr_mono_lty ty = ppr_mono_ty (unLoc ty)

ppr_mono_ty :: (OutputableBndrId (GhcPass p)) => HsType (GhcPass p) -> SDoc
ppr_mono_ty (HsForAllTy { hst_bndrs = tvs, hst_body = ty })
  = sep [pprHsForAllTvs tvs, ppr_mono_lty ty]

ppr_mono_ty (HsQualTy { hst_ctxt = L _ ctxt, hst_body = ty })
  = sep [pprHsContextAlways ctxt, ppr_mono_lty ty]

ppr_mono_ty (HsBangTy _ b ty)   = ppr b <> ppr_mono_lty ty
ppr_mono_ty (HsRecTy _ flds)      = pprConDeclFields flds
ppr_mono_ty (HsTyVar _ NotPromoted (L _ name))= pprPrefixOcc name
ppr_mono_ty (HsTyVar _ Promoted (L _ name))
  = space <> quote (pprPrefixOcc name)
                         -- We need a space before the ' above, so the parser
                         -- does not attach it to the previous symbol
ppr_mono_ty (HsFunTy _ ty1 ty2)   = ppr_fun_ty ty1 ty2
ppr_mono_ty (HsTupleTy _ con tys) = tupleParens std_con (pprWithCommas ppr tys)
  where std_con = case con of
                    HsUnboxedTuple -> UnboxedTuple
                    _              -> BoxedTuple
ppr_mono_ty (HsSumTy _ tys)
  = tupleParens UnboxedTuple (pprWithBars ppr tys)
ppr_mono_ty (HsKindSig _ ty kind)
  = parens (ppr_mono_lty ty <+> dcolon <+> ppr kind)
ppr_mono_ty (HsListTy _ ty)       = brackets (ppr_mono_lty ty)
ppr_mono_ty (HsPArrTy _ ty)       = paBrackets (ppr_mono_lty ty)
ppr_mono_ty (HsIParamTy _ n ty)   = (ppr n <+> dcolon <+> ppr_mono_lty ty)
ppr_mono_ty (HsSpliceTy _ s)      = pprSplice s
ppr_mono_ty (HsExplicitListTy _ Promoted tys)
  = quote $ brackets (interpp'SP tys)
ppr_mono_ty (HsExplicitListTy _ NotPromoted tys)
  = brackets (interpp'SP tys)
ppr_mono_ty (HsExplicitTupleTy _ tys) = quote $ parens (interpp'SP tys)
ppr_mono_ty (HsTyLit _ t)       = ppr_tylit t
ppr_mono_ty (HsWildCardTy {})   = char '_'

ppr_mono_ty (HsEqTy _ ty1 ty2)
  = ppr_mono_lty ty1 <+> char '~' <+> ppr_mono_lty ty2

ppr_mono_ty (HsAppsTy _ tys)
  = hsep (map (ppr_app_ty . unLoc) tys)

ppr_mono_ty (HsAppTy _ fun_ty arg_ty)
  = hsep [ppr_mono_lty fun_ty, ppr_mono_lty arg_ty]

ppr_mono_ty (HsOpTy _ ty1 (L _ op) ty2)
  = sep [ ppr_mono_lty ty1
        , sep [pprInfixOcc op, ppr_mono_lty ty2 ] ]

ppr_mono_ty (HsParTy _ ty)
  = parens (ppr_mono_lty ty)
  -- Put the parens in where the user did
  -- But we still use the precedence stuff to add parens because
  --    toHsType doesn't put in any HsParTys, so we may still need them

ppr_mono_ty (HsDocTy _ ty doc)
  -- AZ: Should we add parens?  Should we introduce "-- ^"?
  = ppr_mono_lty ty <+> ppr (unLoc doc)
  -- we pretty print Haddock comments on types as if they were
  -- postfix operators

ppr_mono_ty (XHsType t) = ppr t

--------------------------
ppr_fun_ty :: (OutputableBndrId (GhcPass p))
           => LHsType (GhcPass p) -> LHsType (GhcPass p) -> SDoc
ppr_fun_ty ty1 ty2
  = let p1 = ppr_mono_lty ty1
        p2 = ppr_mono_lty ty2
    in
    sep [p1, text "->" <+> p2]

--------------------------
ppr_app_ty :: (OutputableBndrId (GhcPass p)) => HsAppType (GhcPass p) -> SDoc
ppr_app_ty (HsAppInfix _ (L _ n)) = pprInfixOcc n
ppr_app_ty (HsAppPrefix _ (L _ (HsTyVar _ NotPromoted (L _ n))))
  = pprPrefixOcc n
ppr_app_ty (HsAppPrefix _ (L _ (HsTyVar _ Promoted  (L _ n))))
  = space <> quote (pprPrefixOcc n) -- We need a space before the ' above, so
                                    -- the parser does not attach it to the
                                    -- previous symbol
ppr_app_ty (HsAppPrefix _ ty) = ppr_mono_lty ty

ppr_app_ty (XAppType ty)      = ppr ty

--------------------------
ppr_tylit :: HsTyLit -> SDoc
ppr_tylit (HsNumTy _ i) = integer i
ppr_tylit (HsStrTy _ s) = text (show s)


-- | @'hsTypeNeedsParens' p t@ returns 'True' if the type @t@ needs parentheses
-- under precedence @p@.
hsTypeNeedsParens :: PprPrec -> HsType pass -> Bool
hsTypeNeedsParens p = go
  where
    go (HsForAllTy{})        = False
    go (HsQualTy{})          = False
    go (HsBangTy{})          = p > topPrec
    go (HsRecTy{})           = False
    go (HsTyVar{})           = False
    go (HsFunTy{})           = p >= funPrec
    go (HsTupleTy{})         = False
    go (HsSumTy{})           = False
    go (HsKindSig{})         = False
    go (HsListTy{})          = False
    go (HsPArrTy{})          = False
    go (HsIParamTy{})        = p > topPrec
    go (HsSpliceTy{})        = False
    go (HsExplicitListTy{})  = False
    go (HsExplicitTupleTy{}) = False
    go (HsTyLit{})           = False
    go (HsWildCardTy{})      = False
    go (HsEqTy{})            = p >= opPrec
    go (HsAppsTy _ args)     = p >= appPrec && not (null args)
    go (HsAppTy{})           = p >= appPrec
    go (HsOpTy{})            = p >= opPrec
    go (HsParTy{})           = False
    go (HsDocTy _ (L _ t) _) = go t
    go (XHsType{})           = False

-- | @'parenthesizeHsType' p ty@ checks if @'hsTypeNeedsParens' p ty@ is
-- true, and if so, surrounds @ty@ with an 'HsParTy'. Otherwise, it simply
-- returns @ty@.
parenthesizeHsType :: PprPrec -> LHsType (GhcPass p) -> LHsType (GhcPass p)
parenthesizeHsType p lty@(L loc ty)
  | hsTypeNeedsParens p ty = L loc (HsParTy NoExt lty)
  | otherwise              = lty
