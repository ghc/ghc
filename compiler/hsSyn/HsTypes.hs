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
{-# LANGUAGE TypeFamilies #-}

module HsTypes (
        HsType(..), NewHsTypeX(..), LHsType, HsKind, LHsKind,
        HsTyVarBndr(..), LHsTyVarBndr,
        LHsQTyVars(..), HsQTvsRn(..),
        HsImplicitBndrs(..),
        HsWildCardBndrs(..),
        LHsSigType, LHsSigWcType, LHsWcType,
        HsTupleSort(..),
        HsContext, LHsContext, noLHsContext,
        HsTyLit(..),
        HsIPName(..), hsIPNameFS,
        HsArg(..), numVisibleArgs,
        LHsTypeArg,

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

        mkAnonWildCardTy, pprAnonWildCard,

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
        splitHsFunType,
        splitHsAppTys, hsTyGetAppHead_maybe,
        mkHsOpTy, mkHsAppTy, mkHsAppTys, mkHsAppKindTy,
        ignoreParens, hsSigType, hsSigWcType,
        hsLTyVarBndrToType, hsLTyVarBndrsToTypes,

        -- Printing
        pprHsType, pprHsForAll, pprHsForAllExtra, pprHsExplicitForAll,
        pprLHsContext,
        hsTypeNeedsParens, parenthesizeHsType, parenthesizeHsContext
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
import Util ( count )

import Data.Data hiding ( Fixity, Prefix, Infix )

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
    The renamer leaves it untouched, and it is later given fresh meta tyvars in
    the typechecker.

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

* Named wildcards are bound by the HsWildCardBndrs construct, which wraps
  types that are allowed to have wildcards. Unnamed wildcards however are left
  unchanged until typechecking, where we give them fresh wild tyavrs and
  determine whether or not to emit hole constraints on each wildcard
  (we don't if it's a visible type/kind argument or a type family pattern).
  See related notes Note [Wildcards in visible kind application]
  and Note [Wildcards in visible type application] in TcHsType.hs

* After type checking is done, we report what types the wildcards
  got unified with.

Note [Ordering of implicit variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since the advent of -XTypeApplications, GHC makes promises about the ordering
of implicit variable quantification. Specifically, we offer that implicitly
quantified variables (such as those in const :: a -> b -> a, without a `forall`)
will occur in left-to-right order of first occurrence. Here are a few examples:

  const :: a -> b -> a       -- forall a b. ...
  f :: Eq a => b -> a -> a   -- forall a b. ...  contexts are included

  type a <-< b = b -> a
  g :: a <-< b               -- forall a b. ...  type synonyms matter

  class Functor f where
    fmap :: (a -> b) -> f a -> f b   -- forall f a b. ...
    -- The f is quantified by the class, so only a and b are considered in fmap

This simple story is complicated by the possibility of dependency: all variables
must come after any variables mentioned in their kinds.

  typeRep :: Typeable a => TypeRep (a :: k)   -- forall k a. ...

The k comes first because a depends on k, even though the k appears later than
the a in the code. Thus, GHC does a *stable topological sort* on the variables.
By "stable", we mean that any two variables who do not depend on each other
preserve their existing left-to-right ordering.

Implicitly bound variables are collected by the extract- family of functions
(extractHsTysRdrTyVars, extractHsTyVarBndrsKVs, etc.) in RnTypes.
These functions thus promise to keep left-to-right ordering.
Look for pointers to this note to see the places where the action happens.

Note that we also maintain this ordering in kind signatures. Even though
there's no visible kind application (yet), having implicit variables be
quantified in left-to-right order in kind signatures is nice since:

* It's consistent with the treatment for type signatures.
* It can affect how types are displayed with -fprint-explicit-kinds (see
  #15568 for an example), which is a situation where knowing the order in
  which implicit variables are quantified can be useful.
* In the event that visible kind application is implemented, the order in
  which we would expect implicit variables to be ordered in kinds will have
  already been established.
-}

-- | Located Haskell Context
type LHsContext pass = Located (HsContext pass)
      -- ^ 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnUnit'
      -- For details on above see note [Api annotations] in ApiAnnotation

noLHsContext :: LHsContext pass
-- Use this when there is no context in the original program
-- It would really be more kosher to use a Maybe, to distinguish
--     class () => C a where ...
-- from
--     class C a where ...
noLHsContext = noLoc []

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
  = HsIB { hsib_ext  :: XHsIB pass thing -- after renamer: [Name]
                                         -- Implicitly-bound kind & type vars
                                         -- Order is important; see
                                         -- Note [Ordering of implicit variables]
                                         -- in RnTypes

         , hsib_body :: thing            -- Main payload (type or list of types)
    }
  | XHsImplicitBndrs (XXHsImplicitBndrs pass thing)

type instance XHsIB              GhcPs _ = NoExt
type instance XHsIB              GhcRn _ = [Name]
type instance XHsIB              GhcTc _ = [Name]

type instance XXHsImplicitBndrs  (GhcPass _) _ = NoExt

-- | Haskell Wildcard Binders
data HsWildCardBndrs pass thing
    -- See Note [HsType binders]
    -- See Note [The wildcard story for types]
  = HsWC { hswc_ext :: XHsWC pass thing
                -- after the renamer
                -- Wild cards, only named
                -- See Note [Wildcards in visible kind application]

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
mkEmptyImplicitBndrs x = HsIB { hsib_ext = []
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

  | HsTyVar  (XTyVar pass)
              PromotionFlag    -- Whether explicitly promoted,
                               -- for the pretty printer
             (Located (IdP pass))
                  -- Type variable, type constructor, or data constructor
                  -- see Note [Promotions (HsTyVar)]
                  -- See Note [Located RdrNames] in HsExpr
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsAppTy             (XAppTy pass)
                        (LHsType pass)
                        (LHsType pass)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsAppKindTy         (XAppKindTy pass) -- type level type app
                        (LHsType pass)
                        (LHsKind pass)

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

  | HsStarTy            (XStarTy pass)
                        Bool             -- Is this the Unicode variant?
                                         -- Note [HsStarTy]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

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
        PromotionFlag      -- whether explcitly promoted, for pretty printer
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
type instance XAppTy           (GhcPass _) = NoExt
type instance XFunTy           (GhcPass _) = NoExt
type instance XListTy          (GhcPass _) = NoExt
type instance XTupleTy         (GhcPass _) = NoExt
type instance XSumTy           (GhcPass _) = NoExt
type instance XOpTy            (GhcPass _) = NoExt
type instance XParTy           (GhcPass _) = NoExt
type instance XIParamTy        (GhcPass _) = NoExt
type instance XStarTy          (GhcPass _) = NoExt
type instance XKindSig         (GhcPass _) = NoExt

type instance XAppKindTy       (GhcPass _) = SrcSpan -- Where the `@` lives

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

type instance XWildCardTy      (GhcPass _) = NoExt

type instance XXType         (GhcPass _) = NewHsTypeX


-- Note [Literal source text] in BasicTypes for SourceText fields in
-- the following
-- | Haskell Type Literal
data HsTyLit
  = HsNumTy SourceText Integer
  | HsStrTy SourceText FastString
    deriving Data


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

Note [HsStarTy]
~~~~~~~~~~~~~~~
When the StarIsType extension is enabled, we want to treat '*' and its Unicode
variant identically to 'Data.Kind.Type'. Unfortunately, doing so in the parser
would mean that when we pretty-print it back, we don't know whether the user
wrote '*' or 'Type', and lose the parse/ppr roundtrip property.

As a workaround, we parse '*' as HsStarTy (if it stands for 'Data.Kind.Type')
and then desugar it to 'Data.Kind.Type' in the typechecker (see tc_hs_type).
When '*' is a regular type operator (StarIsType is disabled), HsStarTy is not
involved.


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
  , HsIB { hsib_ext = vars
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
  | HsIB { hsib_ext = vars
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
hsLTyVarLocName = onHasSrcSpan hsTyVarName

hsLTyVarLocNames :: LHsQTyVars pass -> [Located (IdP pass)]
hsLTyVarLocNames qtvs = map hsLTyVarLocName (hsQTvExplicit qtvs)

-- | Convert a LHsTyVarBndr to an equivalent LHsType.
hsLTyVarBndrToType :: LHsTyVarBndr (GhcPass p) -> LHsType (GhcPass p)
hsLTyVarBndrToType = onHasSrcSpan cvt
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
mkHsAppTys = foldl' mkHsAppTy

mkHsAppKindTy :: XAppKindTy (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
              -> LHsType (GhcPass p)
mkHsAppKindTy ext ty k
  = addCLoc ty k (HsAppKindTy ext ty k)

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
{- This is not so correct, because it won't work with visible kind app, in case
  someone wants to write '(->) @k1 @k2 t1 t2'. Fixing this would require changing
  ConDeclGADT abstract syntax -}
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

-- retrieve the name of the "head" of a nested type application
-- somewhat like splitHsAppTys, but a little more thorough
-- used to examine the result of a GADT-like datacon, so it doesn't handle
-- *all* cases (like lists, tuples, (~), etc.)
hsTyGetAppHead_maybe :: LHsType (GhcPass p)
                     -> Maybe (Located (IdP (GhcPass p)))
hsTyGetAppHead_maybe = go
  where
    go (L _ (HsTyVar _ _ ln))          = Just ln
    go (L _ (HsAppTy _ l _))           = go l
    go (L _ (HsAppKindTy _ t _))       = go t
    go (L _ (HsOpTy _ _ (L loc n) _))  = Just (L loc n)
    go (L _ (HsParTy _ t))             = go t
    go (L _ (HsKindSig _ t _))         = go t
    go _                               = Nothing

------------------------------------------------------------
-- Arguments in an expression/type after splitting
data HsArg tm ty
  = HsValArg tm   -- Argument is an ordinary expression     (f arg)
  | HsTypeArg SrcSpan ty -- Argument is a visible type application (f @ty)
                         -- SrcSpan is location of the `@`
  | HsArgPar SrcSpan -- See Note [HsArgPar]

numVisibleArgs :: [HsArg tm ty] -> Arity
numVisibleArgs = count is_vis
  where is_vis (HsValArg _) = True
        is_vis _            = False

-- type level equivalent
type LHsTypeArg p = HsArg (LHsType p) (LHsKind p)

instance (Outputable tm, Outputable ty) => Outputable (HsArg tm ty) where
  ppr (HsValArg tm)    = ppr tm
  ppr (HsTypeArg _ ty) = char '@' <> ppr ty
  ppr (HsArgPar sp)    = text "HsArgPar"  <+> ppr sp
{-
Note [HsArgPar]
A HsArgPar indicates that everything to the left of this in the argument list is
enclosed in parentheses together with the function itself. It is necessary so
that we can recreate the parenthesis structure in the original source after
typechecking the arguments.

The SrcSpan is the span of the original HsPar

((f arg1) arg2 arg3) results in an input argument list of
[HsValArg arg1, HsArgPar span1, HsValArg arg2, HsValArg arg3, HsArgPar span2]

-}

splitHsAppTys :: HsType GhcRn -> (LHsType GhcRn, [LHsTypeArg GhcRn])
splitHsAppTys e = go (noLoc e) []
  where
    go :: LHsType GhcRn -> [LHsTypeArg GhcRn]
       -> (LHsType GhcRn, [LHsTypeArg GhcRn])
    go (L _ (HsAppTy _ f a))      as = go f (HsValArg a : as)
    go (L _ (HsAppKindTy l ty k)) as = go ty (HsTypeArg l k : as)
    go (L sp (HsParTy _ f))       as = go f (HsArgPar sp : as)
    go f                          as = (f,as)
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
splitLHsQualTy body              = (noLHsContext, body)

splitLHsInstDeclTy :: LHsSigType GhcRn
                   -> ([Name], LHsContext GhcRn, LHsType GhcRn)
-- Split up an instance decl type, returning the pieces
splitLHsInstDeclTy (HsIB { hsib_ext = itkvs
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
       ; cls <- hsTyGetAppHead_maybe head_ty
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
data FieldOcc pass = FieldOcc { extFieldOcc     :: XCFieldOcc pass
                              , rdrNameFieldOcc :: Located RdrName
                                 -- ^ See Note [Located RdrNames] in HsExpr
                              }

  | XFieldOcc
      (XXFieldOcc pass)
deriving instance (p ~ GhcPass pass, Eq (XCFieldOcc p)) => Eq  (FieldOcc p)
deriving instance (p ~ GhcPass pass, Ord (XCFieldOcc p)) => Ord (FieldOcc p)

type instance XCFieldOcc GhcPs = NoExt
type instance XCFieldOcc GhcRn = Name
type instance XCFieldOcc GhcTc = Id

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

pprAnonWildCard :: SDoc
pprAnonWildCard = char '_'

-- | Prints a forall; When passed an empty list, prints @forall.@ only when
-- @-dppr-debug@
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
  = pp_forall <+> pprLHsContextExtra (isJust extra) cxt
  where
    pp_forall | null qtvs = whenPprDebug (forAllLit <> dot)
              | otherwise = forAllLit <+> interppSP qtvs <> dot

-- | Version of 'pprHsForall' or 'pprHsForallExtra' that will always print
-- @forall.@ when passed @Just []@. Prints nothing if passed 'Nothing'
pprHsExplicitForAll :: (OutputableBndrId (GhcPass p))
               => Maybe [LHsTyVarBndr (GhcPass p)] -> SDoc
pprHsExplicitForAll (Just qtvs) = forAllLit <+> interppSP qtvs <> dot
pprHsExplicitForAll Nothing     = empty

pprLHsContext :: (OutputableBndrId (GhcPass p))
              => LHsContext (GhcPass p) -> SDoc
pprLHsContext lctxt
  | null (unLoc lctxt) = empty
  | otherwise          = pprLHsContextAlways lctxt

-- For use in a HsQualTy, which always gets printed if it exists.
pprLHsContextAlways :: (OutputableBndrId (GhcPass p))
                    => LHsContext (GhcPass p) -> SDoc
pprLHsContextAlways (L _ ctxt)
  = case ctxt of
      []       -> parens empty             <+> darrow
      [L _ ty] -> ppr_mono_ty ty           <+> darrow
      _        -> parens (interpp'SP ctxt) <+> darrow

-- True <=> print an extra-constraints wildcard, e.g. @(Show a, _) =>@
pprLHsContextExtra :: (OutputableBndrId (GhcPass p))
                   => Bool -> LHsContext (GhcPass p) -> SDoc
pprLHsContextExtra show_extra lctxt@(L _ ctxt)
  | not show_extra = pprLHsContext lctxt
  | null ctxt      = char '_' <+> darrow
  | otherwise      = parens (sep (punctuate comma ctxt')) <+> darrow
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
  = sep [pprHsForAll tvs noLHsContext, ppr_mono_lty ty]

ppr_mono_ty (HsQualTy { hst_ctxt = ctxt, hst_body = ty })
  = sep [pprLHsContextAlways ctxt, ppr_mono_lty ty]

ppr_mono_ty (HsBangTy _ b ty)   = ppr b <> ppr_mono_lty ty
ppr_mono_ty (HsRecTy _ flds)      = pprConDeclFields flds
ppr_mono_ty (HsTyVar _ prom (L _ name))
  | isPromoted prom = quote (pprPrefixOcc name)
  | otherwise       = pprPrefixOcc name
ppr_mono_ty (HsFunTy _ ty1 ty2)   = ppr_fun_ty ty1 ty2
ppr_mono_ty (HsTupleTy _ con tys) = tupleParens std_con (pprWithCommas ppr tys)
  where std_con = case con of
                    HsUnboxedTuple -> UnboxedTuple
                    _              -> BoxedTuple
ppr_mono_ty (HsSumTy _ tys)
  = tupleParens UnboxedTuple (pprWithBars ppr tys)
ppr_mono_ty (HsKindSig _ ty kind)
  = ppr_mono_lty ty <+> dcolon <+> ppr kind
ppr_mono_ty (HsListTy _ ty)       = brackets (ppr_mono_lty ty)
ppr_mono_ty (HsIParamTy _ n ty)   = (ppr n <+> dcolon <+> ppr_mono_lty ty)
ppr_mono_ty (HsSpliceTy _ s)      = pprSplice s
ppr_mono_ty (HsExplicitListTy _ prom tys)
  | isPromoted prom = quote $ brackets (maybeAddSpace tys $ interpp'SP tys)
  | otherwise       = brackets (interpp'SP tys)
ppr_mono_ty (HsExplicitTupleTy _ tys)
  = quote $ parens (maybeAddSpace tys $ interpp'SP tys)
ppr_mono_ty (HsTyLit _ t)       = ppr_tylit t
ppr_mono_ty (HsWildCardTy {})   = char '_'

ppr_mono_ty (HsStarTy _ isUni)  = char (if isUni then '★' else '*')

ppr_mono_ty (HsAppTy _ fun_ty arg_ty)
  = hsep [ppr_mono_lty fun_ty, ppr_mono_lty arg_ty]
ppr_mono_ty (HsAppKindTy _ ty k)
  = ppr_mono_lty ty <+> char '@' <> ppr_mono_lty k
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
    sep [p1, arrow <+> p2]

--------------------------
ppr_tylit :: HsTyLit -> SDoc
ppr_tylit (HsNumTy _ i) = integer i
ppr_tylit (HsStrTy _ s) = text (show s)


-- | @'hsTypeNeedsParens' p t@ returns 'True' if the type @t@ needs parentheses
-- under precedence @p@.
hsTypeNeedsParens :: PprPrec -> HsType pass -> Bool
hsTypeNeedsParens p = go
  where
    go (HsForAllTy{})        = p >= funPrec
    go (HsQualTy{})          = p >= funPrec
    go (HsBangTy{})          = p > topPrec
    go (HsRecTy{})           = False
    go (HsTyVar{})           = False
    go (HsFunTy{})           = p >= funPrec
    go (HsTupleTy{})         = False
    go (HsSumTy{})           = False
    go (HsKindSig{})         = p >= sigPrec
    go (HsListTy{})          = False
    go (HsIParamTy{})        = p > topPrec
    go (HsSpliceTy{})        = False
    go (HsExplicitListTy{})  = False
    go (HsExplicitTupleTy{}) = False
    go (HsTyLit{})           = False
    go (HsWildCardTy{})      = False
    go (HsStarTy{})          = False
    go (HsAppTy{})           = p >= appPrec
    go (HsAppKindTy{})       = p >= appPrec
    go (HsOpTy{})            = p >= opPrec
    go (HsParTy{})           = False
    go (HsDocTy _ (L _ t) _) = go t
    go (XHsType{})           = False

maybeAddSpace :: [LHsType pass] -> SDoc -> SDoc
-- See Note [Printing promoted type constructors]
-- in IfaceType.  This code implements the same
-- logic for printing HsType
maybeAddSpace tys doc
  | (ty : _) <- tys
  , lhsTypeHasLeadingPromotionQuote ty = space <> doc
  | otherwise                          = doc

lhsTypeHasLeadingPromotionQuote :: LHsType pass -> Bool
lhsTypeHasLeadingPromotionQuote ty
  = goL ty
  where
    goL (L _ ty) = go ty

    go (HsForAllTy{})        = False
    go (HsQualTy{ hst_ctxt = ctxt, hst_body = body})
      | L _ (c:_) <- ctxt    = goL c
      | otherwise            = goL body
    go (HsBangTy{})          = False
    go (HsRecTy{})           = False
    go (HsTyVar _ p _)       = isPromoted p
    go (HsFunTy _ arg _)     = goL arg
    go (HsListTy{})          = False
    go (HsTupleTy{})         = False
    go (HsSumTy{})           = False
    go (HsOpTy _ t1 _ _)     = goL t1
    go (HsKindSig _ t _)     = goL t
    go (HsIParamTy{})        = False
    go (HsSpliceTy{})        = False
    go (HsExplicitListTy _ p _) = isPromoted p
    go (HsExplicitTupleTy{}) = True
    go (HsTyLit{})           = False
    go (HsWildCardTy{})      = False
    go (HsStarTy{})          = False
    go (HsAppTy _ t _)       = goL t
    go (HsAppKindTy _ t _)   = goL t
    go (HsParTy{})           = False
    go (HsDocTy _ t _)       = goL t
    go (XHsType{})           = False

-- | @'parenthesizeHsType' p ty@ checks if @'hsTypeNeedsParens' p ty@ is
-- true, and if so, surrounds @ty@ with an 'HsParTy'. Otherwise, it simply
-- returns @ty@.
parenthesizeHsType :: PprPrec -> LHsType (GhcPass p) -> LHsType (GhcPass p)
parenthesizeHsType p lty@(L loc ty)
  | hsTypeNeedsParens p ty = L loc (HsParTy NoExt lty)
  | otherwise              = lty

-- | @'parenthesizeHsContext' p ctxt@ checks if @ctxt@ is a single constraint
-- @c@ such that @'hsTypeNeedsParens' p c@ is true, and if so, surrounds @c@
-- with an 'HsParTy' to form a parenthesized @ctxt@. Otherwise, it simply
-- returns @ctxt@ unchanged.
parenthesizeHsContext :: PprPrec
                      -> LHsContext (GhcPass p) -> LHsContext (GhcPass p)
parenthesizeHsContext p lctxt@(L loc ctxt) =
  case ctxt of
    [c] -> L loc [parenthesizeHsType p c]
    _   -> lctxt -- Other contexts are already "parenthesized" by virtue of
                 -- being tuples.
