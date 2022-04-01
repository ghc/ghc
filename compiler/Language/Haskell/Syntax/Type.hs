
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


GHC.Hs.Type: Abstract syntax: user-defined types
-}

-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*
module Language.Haskell.Syntax.Type (
        Mult, HsScaled(..),
        hsMult, hsScaledThing,
        HsArrow(..),
        HsLinearArrowTokens(..),

        HsType(..), HsCoreTy, LHsType, HsKind, LHsKind,
        HsForAllTelescope(..), HsTyVarBndr(..), LHsTyVarBndr,
        LHsQTyVars(..),
        HsOuterTyVarBndrs(..), HsOuterFamEqnTyVarBndrs, HsOuterSigTyVarBndrs,
        HsWildCardBndrs(..),
        HsPatSigType(..), HsPSRn(..),
        HsSigType(..), LHsSigType, LHsSigWcType, LHsWcType,
        HsTupleSort(..),
        HsContext, LHsContext,
        HsTyLit(..),
        HsIPName(..), hsIPNameFS,
        HsArg(..), numVisibleArgs, pprHsArgsApp,
        LHsTypeArg,

        LBangType, BangType,
        HsSrcBang(..), HsImplBang(..),
        SrcStrictness(..), SrcUnpackedness(..),

        ConDeclField(..), LConDeclField,

        HsConDetails(..), noTypeArgs,

        FieldOcc(..), LFieldOcc,
        AmbiguousFieldOcc(..), LAmbiguousFieldOcc,

        mapHsOuterImplicit,
        hsQTvExplicit,
        isHsKindedTyVar,
        hsPatSigType,
    ) where

import GHC.Prelude

import {-# SOURCE #-} Language.Haskell.Syntax.Expr ( HsSplice )

import Language.Haskell.Syntax.Extension

import GHC.Types.SourceText
import GHC.Types.Name( Name )
import GHC.Types.Name.Reader ( RdrName )
import GHC.Core.DataCon( HsSrcBang(..), HsImplBang(..),
                         SrcStrictness(..), SrcUnpackedness(..) )
import GHC.Core.Type
import GHC.Hs.Doc
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Utils.Misc ( count )
import GHC.Parser.Annotation

import Data.Data hiding ( Fixity, Prefix, Infix )
import Data.Void

{-
************************************************************************
*                                                                      *
\subsection{Bang annotations}
*                                                                      *
************************************************************************
-}

-- | Located Bang Type
type LBangType pass = XRec pass (BangType pass)

-- | Bang Type
--
-- In the parser, strictness and packedness annotations bind more tightly
-- than docstrings. This means that when consuming a 'BangType' (and looking
-- for 'HsBangTy') we must be ready to peer behind a potential layer of
-- 'HsDocTy'. See #15206 for motivation and 'getBangType' for an example.
type BangType pass  = HsType pass       -- Bangs are in the HsType data type

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
See also Note [Kind and type-variable binders] in GHC.Rename.HsType

Note [HsType binders]
~~~~~~~~~~~~~~~~~~~~~
The system for recording type and kind-variable binders in HsTypes
is a bit complicated.  Here's how it works.

* In a HsType,
     HsForAllTy   represents an /explicit, user-written/ 'forall' that
                  is nested within another HsType
                   e.g.   forall a b.   {...} or
                          forall a b -> {...}

                  Note that top-level 'forall's are represented with a
                  different AST form. See the description of HsOuterTyVarBndrs
                  below.
     HsQualTy     represents an /explicit, user-written/ context
                   e.g.   (Eq a, Show a) => ...
                  The context can be empty if that's what the user wrote
  These constructors represent what the user wrote, no more
  and no less.

* The ForAllTelescope field of HsForAllTy represents whether a forall is
  invisible (e.g., forall a b. {...}, with a dot) or visible
  (e.g., forall a b -> {...}, with an arrow).

* HsTyVarBndr describes a quantified type variable written by the
  user.  For example
     f :: forall a (b :: *).  blah
  here 'a' and '(b::*)' are each a HsTyVarBndr.  A HsForAllTy has
  a list of LHsTyVarBndrs.

* HsOuterTyVarBndrs is used to represent the outermost quantified type
  variables in a type that obeys the forall-or-nothing rule. An
  HsOuterTyVarBndrs can be one of the following:

    HsOuterImplicit (implicit quantification, added by renamer)
          f :: a -> a     -- Desugars to f :: forall {a}. a -> a
    HsOuterExplicit (explicit user quantifiation):
          f :: forall a. a -> a

  See Note [forall-or-nothing rule].

* An HsSigType is an LHsType with an accompanying HsOuterTyVarBndrs that
  represents the presence (or absence) of its outermost 'forall'.
  See Note [Representing type signatures].

* HsWildCardBndrs is a wrapper that binds the wildcard variables
  of the wrapped thing.  It is filled in by the renamer
     f :: _a -> _
  The enclosing HsWildCardBndrs binds the wildcards _a and _.

* HsSigPatType describes types that appear in pattern signatures and
  the signatures of term-level binders in RULES. Like
  HsWildCardBndrs/HsOuterTyVarBndrs, they track the names of wildcard
  variables and implicitly bound type variables. Unlike
  HsOuterTyVarBndrs, however, HsSigPatTypes do not obey the
  forall-or-nothing rule. See Note [Pattern signature binders and scoping].

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
    The renamer leaves it untouched, and it is later given a fresh
    meta tyvar in the typechecker.

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
  _b is a named wildcard.  (See the comments in #10982)

* Named wildcards are bound by the HsWildCardBndrs (for types that obey the
  forall-or-nothing rule) and HsPatSigType (for type signatures in patterns
  and term-level binders in RULES), which wrap types that are allowed to have
  wildcards. Unnamed wildcards, however are left unchanged until typechecking,
  where we give them fresh wild tyvars and determine whether or not to emit
  hole constraints on each wildcard (we don't if it's a visible type/kind
  argument or a type family pattern). See related notes
  Note [Wildcards in visible kind application] and
  Note [Wildcards in visible type application] in GHC.Tc.Gen.HsType.

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
(extractHsTysRdrTyVars, extractHsTyVarBndrsKVs, etc.) in GHC.Rename.HsType.
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
type LHsContext pass = XRec pass (HsContext pass)
      -- ^ 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnUnit'
      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

-- | Haskell Context
type HsContext pass = [LHsType pass]

-- | Located Haskell Type
type LHsType pass = XRec pass (HsType pass)
      -- ^ May have 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnComma' when
      --   in a list

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

-- | Haskell Kind
type HsKind pass = HsType pass

-- | Located Haskell Kind
type LHsKind pass = XRec pass (HsKind pass)
      -- ^ 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDcolon'

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

--------------------------------------------------
--             LHsQTyVars
--  The explicitly-quantified binders in a data/type declaration

-- | The type variable binders in an 'HsForAllTy'.
-- See also @Note [Variable Specificity and Forall Visibility]@ in
-- "GHC.Tc.Gen.HsType".
data HsForAllTelescope pass
  = HsForAllVis -- ^ A visible @forall@ (e.g., @forall a -> {...}@).
                --   These do not have any notion of specificity, so we use
                --   '()' as a placeholder value.
    { hsf_xvis      :: XHsForAllVis pass
    , hsf_vis_bndrs :: [LHsTyVarBndr () pass]
    }
  | HsForAllInvis -- ^ An invisible @forall@ (e.g., @forall a {b} c. {...}@),
                  --   where each binder has a 'Specificity'.
    { hsf_xinvis       :: XHsForAllInvis pass
    , hsf_invis_bndrs  :: [LHsTyVarBndr Specificity pass]
    }
  | XHsForAllTelescope !(XXHsForAllTelescope pass)

-- | Located Haskell Type Variable Binder
type LHsTyVarBndr flag pass = XRec pass (HsTyVarBndr flag pass)
                         -- See Note [HsType binders]

-- | Located Haskell Quantified Type Variables
data LHsQTyVars pass   -- See Note [HsType binders]
  = HsQTvs { hsq_ext :: XHsQTvs pass

           , hsq_explicit :: [LHsTyVarBndr () pass]
                -- Explicit variables, written by the user
    }
  | XLHsQTyVars !(XXLHsQTyVars pass)

hsQTvExplicit :: LHsQTyVars pass -> [LHsTyVarBndr () pass]
hsQTvExplicit = hsq_explicit

------------------------------------------------
--            HsOuterTyVarBndrs
-- Used to quantify the outermost type variable binders of a type that obeys
-- the forall-or-nothing rule. These are used to represent the outermost
-- quantification in:
--    * Type signatures (LHsSigType/LHsSigWcType)
--    * Patterns in a type/data family instance (HsTyPats)
--
-- We support two forms:
--   HsOuterImplicit (implicit quantification, added by renamer)
--         f :: a -> a     -- Desugars to f :: forall {a}. a -> a
--         type instance F (a,b) = a->b
--   HsOuterExplicit (explicit user quantifiation):
--         f :: forall a. a -> a
--         type instance forall a b. F (a,b) = a->b
--
-- In constrast, when the user writes /visible/ quanitification
--         T :: forall k -> k -> Type
-- we use use HsOuterImplicit, wrapped around a HsForAllTy
-- for the visible quantification
--
-- See Note [forall-or-nothing rule]

-- | The outermost type variables in a type that obeys the @forall@-or-nothing
-- rule. See @Note [forall-or-nothing rule]@.
data HsOuterTyVarBndrs flag pass
  = HsOuterImplicit -- ^ Implicit forall, e.g.,
                    --    @f :: a -> b -> b@
    { hso_ximplicit :: XHsOuterImplicit pass
    }
  | HsOuterExplicit -- ^ Explicit forall, e.g.,
                    --    @f :: forall a b. a -> b -> b@
    { hso_xexplicit :: XHsOuterExplicit pass flag
    , hso_bndrs     :: [LHsTyVarBndr flag (NoGhcTc pass)]
    }
  | XHsOuterTyVarBndrs !(XXHsOuterTyVarBndrs pass)

-- | Used for signatures, e.g.,
--
-- @
-- f :: forall a {b}. blah
-- @
--
-- We use 'Specificity' for the 'HsOuterTyVarBndrs' @flag@ to allow
-- distinguishing between specified and inferred type variables.
type HsOuterSigTyVarBndrs = HsOuterTyVarBndrs Specificity

-- | Used for type-family instance equations, e.g.,
--
-- @
-- type instance forall a. F [a] = Tree a
-- @
--
-- The notion of specificity is irrelevant in type family equations, so we use
-- @()@ for the 'HsOuterTyVarBndrs' @flag@.
type HsOuterFamEqnTyVarBndrs = HsOuterTyVarBndrs ()

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
  | XHsWildCardBndrs !(XXHsWildCardBndrs pass thing)

-- | Types that can appear in pattern signatures, as well as the signatures for
-- term-level binders in RULES.
-- See @Note [Pattern signature binders and scoping]@.
--
-- This is very similar to 'HsSigWcType', but with
-- slightly different semantics: see @Note [HsType binders]@.
-- See also @Note [The wildcard story for types]@.
data HsPatSigType pass
  = HsPS { hsps_ext  :: XHsPS pass   -- ^ After renamer: 'HsPSRn'
         , hsps_body :: LHsType pass -- ^ Main payload (the type itself)
    }
  | XHsPatSigType !(XXHsPatSigType pass)

-- | The extension field for 'HsPatSigType', which is only used in the
-- renamer onwards. See @Note [Pattern signature binders and scoping]@.
data HsPSRn = HsPSRn
  { hsps_nwcs    :: [Name] -- ^ Wildcard names
  , hsps_imp_tvs :: [Name] -- ^ Implicitly bound variable names
  }
  deriving Data

-- | Located Haskell Signature Type
type LHsSigType   pass = XRec pass (HsSigType pass)               -- Implicit only

-- | Located Haskell Wildcard Type
type LHsWcType    pass = HsWildCardBndrs pass (LHsType pass)    -- Wildcard only

-- | Located Haskell Signature Wildcard Type
type LHsSigWcType pass = HsWildCardBndrs pass (LHsSigType pass) -- Both

-- | A type signature that obeys the @forall@-or-nothing rule. In other
-- words, an 'LHsType' that uses an 'HsOuterSigTyVarBndrs' to represent its
-- outermost type variable quantification.
-- See @Note [Representing type signatures]@.
data HsSigType pass
  = HsSig { sig_ext   :: XHsSig pass
          , sig_bndrs :: HsOuterSigTyVarBndrs pass
          , sig_body  :: LHsType pass
          }
  | XHsSigType !(XXHsSigType pass)

hsPatSigType :: HsPatSigType pass -> LHsType pass
hsPatSigType = hsps_body

{-
Note [forall-or-nothing rule]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Free variables in signatures are usually bound in an implicit 'forall' at the
beginning of user-written signatures. However, if the signature has an
explicit, invisible forall at the beginning, this is disabled. This is referred
to as the forall-or-nothing rule.

The idea is nested foralls express something which is only expressible
explicitly, while a top level forall could (usually) be replaced with an
implicit binding. Top-level foralls alone ("forall.") are therefore an
indication that the user is trying to be fastidious, so we don't implicitly
bind any variables.

Note that this rule only applies to outermost /in/visible 'forall's, and not
outermost visible 'forall's. See #18660 for more on this point.

Here are some concrete examples to demonstrate the forall-or-nothing rule in
action:

  type F1 :: a -> b -> b                    -- Legal; a,b are implicitly quantified.
                                            -- Equivalently: forall a b. a -> b -> b

  type F2 :: forall a b. a -> b -> b        -- Legal; explicitly quantified

  type F3 :: forall a. a -> b -> b          -- Illegal; the forall-or-nothing rule says that
                                            -- if you quantify a, you must also quantify b

  type F4 :: forall a -> b -> b             -- Legal; the top quantifier (forall a) is a /visible/
                                            -- quantifer, so the "nothing" part of the forall-or-nothing
                                            -- rule applies, and b is therefore implicitly quantified.
                                            -- Equivalently: forall b. forall a -> b -> b

  type F5 :: forall b. forall a -> b -> c   -- Illegal; the forall-or-nothing rule says that
                                            -- if you quantify b, you must also quantify c

  type F6 :: forall a -> forall b. b -> c   -- Legal: just like F4.

For a complete list of all places where the forall-or-nothing rule applies, see
"The `forall`-or-nothing rule" section of the GHC User's Guide.

Any type that obeys the forall-or-nothing rule is represented in the AST with
an HsOuterTyVarBndrs:

* If the type has an outermost, invisible 'forall', it uses HsOuterExplicit,
  which contains a list of the explicitly quantified type variable binders in
  `hso_bndrs`. After typechecking, HsOuterExplicit also stores a list of the
  explicitly quantified `InvisTVBinder`s in
  `hso_xexplicit :: XHsOuterExplicit GhcTc`.

* Otherwise, it uses HsOuterImplicit. HsOuterImplicit is used for different
  things depending on the phase:

  * After parsing, it does not store anything in particular.
  * After renaming, it stores the implicitly bound type variable `Name`s in
    `hso_ximplicit :: XHsOuterImplicit GhcRn`.
  * After typechecking, it stores the implicitly bound `TyVar`s in
    `hso_ximplicit :: XHsOuterImplicit GhcTc`.

  NB: this implicit quantification is purely lexical: we bind any
      type or kind variables that are not in scope. The type checker
      may subsequently quantify over further kind variables.
      See Note [Binding scoped type variables] in GHC.Tc.Gen.Sig.

HsOuterTyVarBndrs GhcTc is used in the typechecker as an intermediate data type
for storing the outermost TyVars/InvisTVBinders in a type.
See GHC.Tc.Gen.HsType.bindOuterTKBndrsX for an example of this.

Note [Representing type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HsSigType is used to represent an explicit user type signature. These are
used in a variety of places. Some examples include:

* Type signatures (e.g., f :: a -> a)
* Standalone kind signatures (e.g., type G :: a -> a)
* GADT constructor types (e.g., data T where MkT :: a -> T)

A HsSigType is the combination of an HsOuterSigTyVarBndrs and an LHsType:

* The HsOuterSigTyVarBndrs binds the /explicitly/ quantified type variables
  when the type signature has an outermost, user-written 'forall' (i.e,
  the HsOuterExplicit constructor is used). If there is no outermost 'forall',
  then it binds the /implicitly/ quantified type variables instead (i.e.,
  the HsOuterImplicit constructor is used).
* The LHsType represents the rest of the type.

E.g. For a signature like
   f :: forall k (a::k). blah
we get
   HsSig { sig_bndrs = HsOuterExplicit { hso_bndrs = [k, (a :: k)] }
         , sig_body  = blah }

Note [Pattern signature binders and scoping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the pattern signatures like those on `t` and `g` in:

   f = let h = \(t :: (b, b) ->
               \(g :: forall a. a -> b) ->
               ...(t :: (Int,Int))...
       in woggle

* The `b` in t's pattern signature is implicitly bound and scopes over
  the signature and the body of the lambda.  It stands for a type (any type);
  indeed we subsequently discover that b=Int.
  (See Note [TyVarTv] in GHC.Tc.Utils.TcMType for more on this point.)
* The `b` in g's pattern signature is an /occurrence/ of the `b` bound by
  t's pattern signature.
* The `a` in `forall a` scopes only over the type `a -> b`, not over the body
  of the lambda.
* There is no forall-or-nothing rule for pattern signatures, which is why the
  type `forall a. a -> b` is permitted in `g`'s pattern signature, even though
  `b` is not explicitly bound. See Note [forall-or-nothing rule].

Similar scoping rules apply to term variable binders in RULES, like in the
following example:

   {-# RULES "h" forall (t :: (b, b)) (g :: forall a. a -> b). h t g = ... #-}

Just like in pattern signatures, the `b` in t's signature is implicitly bound
and scopes over the remainder of the RULE. As a result, the `b` in g's
signature is an occurrence. Moreover, the `a` in `forall a` scopes only over
the type `a -> b`, and the forall-or-nothing rule does not apply.

While quite similar, RULE term binder signatures behave slightly differently
from pattern signatures in two ways:

1. Unlike in pattern signatures, where type variables can stand for any type,
   type variables in RULE term binder signatures are skolems.
   See Note [Typechecking pattern signature binders] in GHC.Tc.Gen.HsType for
   more on this point.

   In this sense, type variables in pattern signatures are quite similar to
   named wildcards, as both can refer to arbitrary types. The main difference
   lies in error reporting: if a named wildcard `_a` in a pattern signature
   stands for Int, then by default GHC will emit a warning stating as much.
   Changing `_a` to `a`, on the other hand, will cause it not to be reported.
2. In the `h` RULE above, only term variables are explicitly bound, so any free
   type variables in the term variables' signatures are implicitly bound.
   This is just like how the free type variables in pattern signatures are
   implicitly bound. If a RULE explicitly binds both term and type variables,
   however, then free type variables in term signatures are /not/ implicitly
   bound. For example, this RULE would be ill scoped:

     {-# RULES "h2" forall b. forall (t :: (b, c)) (g :: forall a. a -> b).
                    h2 t g = ... #-}

   This is because `b` and `c` occur free in the signature for `t`, but only
   `b` was explicitly bound, leaving `c` out of scope. If the RULE had started
   with `forall b c.`, then it would have been accepted.

The types in pattern signatures and RULE term binder signatures are represented
in the AST by HsSigPatType. From the renamer onward, the hsps_ext field (of
type HsPSRn) tracks the names of named wildcards and implicitly bound type
variables so that they can be brought into scope during renaming and
typechecking.

Note [Lexically scoped type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ScopedTypeVariables extension does two things:

* It allows the use of type signatures in patterns
  (e.g., `f (x :: a -> a) = ...`). See
  Note [Pattern signature binders and scoping] for more on this point.
* It brings lexically scoped type variables into scope for certain type
  signatures with outermost invisible 'forall's.

This Note concerns the latter bullet point. Per the
"Lexically scoped type variables" section of the GHC User's Guide, the
following forms of type signatures can have lexically scoped type variables:

* In declarations with type signatures, e.g.,

    f :: forall a. a -> a
    f x = e @a

  Here, the 'forall a' brings 'a' into scope over the body of 'f'.

  Note that ScopedTypeVariables does /not/ interact with standalone kind
  signatures, only type signatures.

* In explicit type annotations in expressions, e.g.,

    id @a :: forall a. a -> a

* In instance declarations, e.g.,

    instance forall a. C [a] where
      m = e @a

  Note that unlike the examples above, the use of an outermost 'forall' isn't
  required to bring 'a' into scope. That is, the following would also work:

    instance forall a. C [a] where
      m = e @a

Note that all of the types above obey the forall-or-nothing rule. As a result,
the places in the AST that can have lexically scoped type variables are a
subset of the places that use HsOuterTyVarBndrs
(See Note [forall-or-nothing rule].)

Some other observations about lexically scoped type variables:

* Only type variables bound by an /invisible/ forall can be lexically scoped.
  See Note [hsScopedTvs and visible foralls].
* The lexically scoped type variables may be a strict subset of the type
  variables brought into scope by a type signature.
  See Note [Binding scoped type variables] in GHC.Tc.Gen.Sig.
-}

mapHsOuterImplicit :: (XHsOuterImplicit pass -> XHsOuterImplicit pass)
                   -> HsOuterTyVarBndrs flag pass
                   -> HsOuterTyVarBndrs flag pass
mapHsOuterImplicit f (HsOuterImplicit{hso_ximplicit = imp}) =
  HsOuterImplicit{hso_ximplicit = f imp}
mapHsOuterImplicit _ hso@(HsOuterExplicit{})    = hso
mapHsOuterImplicit _ hso@(XHsOuterTyVarBndrs{}) = hso


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
-- The flag annotates the binder. It is 'Specificity' in places where
-- explicit specificity is allowed (e.g. x :: forall {a} b. ...) or
-- '()' in other places.
data HsTyVarBndr flag pass
  = UserTyVar        -- no explicit kinding
         (XUserTyVar pass)
         flag
         (LIdP pass)
        -- See Note [Located RdrNames] in GHC.Hs.Expr

  | KindedTyVar
         (XKindedTyVar pass)
         flag
         (LIdP pass)
         (LHsKind pass)  -- The user-supplied kind signature
        -- ^
        --  - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen',
        --          'GHC.Parser.Annotation.AnnDcolon', 'GHC.Parser.Annotation.AnnClose'

        -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | XTyVarBndr
      !(XXTyVarBndr pass)

-- | Does this 'HsTyVarBndr' come with an explicit kind annotation?
isHsKindedTyVar :: HsTyVarBndr flag pass -> Bool
isHsKindedTyVar (UserTyVar {})   = False
isHsKindedTyVar (KindedTyVar {}) = True
isHsKindedTyVar (XTyVarBndr {})  = False

-- | Haskell Type
data HsType pass
  = HsForAllTy   -- See Note [HsType binders]
      { hst_xforall :: XForAllTy pass
      , hst_tele    :: HsForAllTelescope pass
                                     -- Explicit, user-supplied 'forall a {b} c'
      , hst_body    :: LHsType pass  -- body type
      }
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnForall',
      --         'GHC.Parser.Annotation.AnnDot','GHC.Parser.Annotation.AnnDarrow'
      -- For details on above see Note [exact print annotations] in "GHC.Parser.Annotation"

  | HsQualTy   -- See Note [HsType binders]
      { hst_xqual :: XQualTy pass
      , hst_ctxt  :: LHsContext pass  -- Context C => blah
      , hst_body  :: LHsType pass }

  | HsTyVar  (XTyVar pass)
              PromotionFlag    -- Whether explicitly promoted,
                               -- for the pretty printer
             (LIdP pass)
                  -- Type variable, type constructor, or data constructor
                  -- see Note [Promotions (HsTyVar)]
                  -- See Note [Located RdrNames] in GHC.Hs.Expr
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsAppTy             (XAppTy pass)
                        (LHsType pass)
                        (LHsType pass)
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsAppKindTy         (XAppKindTy pass) -- type level type app
                        (LHsType pass)
                        (LHsKind pass)

  | HsFunTy             (XFunTy pass)
                        (HsArrow pass)
                        (LHsType pass)   -- function type
                        (LHsType pass)
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnRarrow',

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsListTy            (XListTy pass)
                        (LHsType pass)  -- Element type
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'['@,
      --         'GHC.Parser.Annotation.AnnClose' @']'@

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsTupleTy           (XTupleTy pass)
                        HsTupleSort
                        [LHsType pass]  -- Element types (length gives arity)
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'(' or '(#'@,
    --         'GHC.Parser.Annotation.AnnClose' @')' or '#)'@

    -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsSumTy             (XSumTy pass)
                        [LHsType pass]  -- Element types (length gives arity)
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'(#'@,
    --         'GHC.Parser.Annotation.AnnClose' '#)'@

    -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsOpTy              (XOpTy pass)
                        PromotionFlag    -- Whether explicitly promoted,
                                         -- for the pretty printer
                        (LHsType pass) (LIdP pass) (LHsType pass)
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsParTy             (XParTy pass)
                        (LHsType pass)   -- See Note [Parens in HsSyn] in GHC.Hs.Expr
        -- Parenthesis preserved for the precedence re-arrangement in
        -- GHC.Rename.HsType
        -- It's important that a * (b + c) doesn't get rearranged to (a*b) + c!
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'('@,
      --         'GHC.Parser.Annotation.AnnClose' @')'@

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsIParamTy          (XIParamTy pass)
                        (XRec pass HsIPName) -- (?x :: ty)
                        (LHsType pass)   -- Implicit parameters as they occur in
                                         -- contexts
      -- ^
      -- > (?x :: ty)
      --
      -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDcolon'

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsStarTy            (XStarTy pass)
                        Bool             -- Is this the Unicode variant?
                                         -- Note [HsStarTy]
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

  | HsKindSig           (XKindSig pass)
                        (LHsType pass)  -- (ty :: kind)
                        (LHsKind pass)  -- A type with a kind signature
      -- ^
      -- > (ty :: kind)
      --
      -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'('@,
      --         'GHC.Parser.Annotation.AnnDcolon','GHC.Parser.Annotation.AnnClose' @')'@

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsSpliceTy          (XSpliceTy pass)
                        (HsSplice pass)   -- Includes quasi-quotes
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'$('@,
      --         'GHC.Parser.Annotation.AnnClose' @')'@

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsDocTy             (XDocTy pass)
                        (LHsType pass) (LHsDoc pass) -- A documented type
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsBangTy    (XBangTy pass)
                HsSrcBang (LHsType pass)   -- Bang-style type annotations
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' :
      --         'GHC.Parser.Annotation.AnnOpen' @'{-\# UNPACK' or '{-\# NOUNPACK'@,
      --         'GHC.Parser.Annotation.AnnClose' @'#-}'@
      --         'GHC.Parser.Annotation.AnnBang' @\'!\'@

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsRecTy     (XRecTy pass)
                [LConDeclField pass]    -- Only in data type declarations
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'{'@,
      --         'GHC.Parser.Annotation.AnnClose' @'}'@

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsExplicitListTy       -- A promoted explicit list
        (XExplicitListTy pass)
        PromotionFlag      -- whether explicitly promoted, for pretty printer
        [LHsType pass]
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @"'["@,
      --         'GHC.Parser.Annotation.AnnClose' @']'@

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsExplicitTupleTy      -- A promoted explicit tuple
        (XExplicitTupleTy pass)
        [LHsType pass]
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @"'("@,
      --         'GHC.Parser.Annotation.AnnClose' @')'@

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsTyLit (XTyLit pass) HsTyLit      -- A promoted numeric literal.
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  | HsWildCardTy (XWildCardTy pass)  -- A type wildcard
      -- See Note [The wildcard story for types]
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : None

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

  -- Extension point; see Note [Trees That Grow] in Language.Haskell.Syntax.Extension
  | XHsType
      !(XXType pass)

-- An escape hatch for tunnelling a Core 'Type' through 'HsType'.
-- For more details on how this works, see:
--
-- * @Note [Renaming HsCoreTys]@ in "GHC.Rename.HsType"
--
-- * @Note [Typechecking HsCoreTys]@ in "GHC.Tc.Gen.HsType"
type HsCoreTy = Type


-- Note [Literal source text] in GHC.Types.Basic for SourceText fields in
-- the following
-- | Haskell Type Literal
data HsTyLit
  = HsNumTy SourceText Integer
  | HsStrTy SourceText FastString
  | HsCharTy SourceText Char
    deriving Data

-- | Denotes the type of arrows in the surface language
data HsArrow pass
  = HsUnrestrictedArrow !(LHsUniToken "->" "→" pass)
    -- ^ a -> b or a → b
  | HsLinearArrow !(HsLinearArrowTokens pass)
    -- ^ a %1 -> b or a %1 → b, or a ⊸ b
  | HsExplicitMult !(LHsToken "%" pass) !(LHsType pass) !(LHsUniToken "->" "→" pass)
    -- ^ a %m -> b or a %m → b (very much including `a %Many -> b`!
    -- This is how the programmer wrote it). It is stored as an
    -- `HsType` so as to preserve the syntax as written in the
    -- program.

data HsLinearArrowTokens pass
  = HsPct1 !(LHsToken "%1" pass) !(LHsUniToken "->" "→" pass)
  | HsLolly !(LHsToken "⊸" pass)

-- | This is used in the syntax. In constructor declaration. It must keep the
-- arrow representation.
data HsScaled pass a = HsScaled (HsArrow pass) a

hsMult :: HsScaled pass a -> HsArrow pass
hsMult (HsScaled m _) = m

hsScaledThing :: HsScaled pass a -> a
hsScaledThing (HsScaled _ t) = t

{-
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
two constructors of HsTupleSort:

        HsUnboxedTuple                  -> Produced by the parser
        HsBoxedOrConstraintTuple        -> Could be a boxed or a constraint
                                        tuple. Produced by the parser only,
                                        disappears after type checking

After typechecking, we use TupleSort (which clearly distinguishes between
constraint tuples and boxed tuples) rather than HsTupleSort.
-}

-- | Haskell Tuple Sort
data HsTupleSort = HsUnboxedTuple
                 | HsBoxedOrConstraintTuple
                 deriving Data

-- | Located Constructor Declaration Field
type LConDeclField pass = XRec pass (ConDeclField pass)
      -- ^ May have 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnComma' when
      --   in a list

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation

-- | Constructor Declaration Field
data ConDeclField pass  -- Record fields have Haddock docs on them
  = ConDeclField { cd_fld_ext  :: XConDeclField pass,
                   cd_fld_names :: [LFieldOcc pass],
                                   -- ^ See Note [ConDeclField passs]
                   cd_fld_type :: LBangType pass,
                   cd_fld_doc  :: Maybe (LHsDoc pass)}
      -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDcolon'

      -- For details on above see Note [exact print annotations] in GHC.Parser.Annotation
  | XConDeclField !(XXConDeclField pass)

-- | Describes the arguments to a data constructor. This is a common
-- representation for several constructor-related concepts, including:
--
-- * The arguments in a Haskell98-style constructor declaration
--   (see 'HsConDeclH98Details' in "GHC.Hs.Decls").
--
-- * The arguments in constructor patterns in @case@/function definitions
--   (see 'HsConPatDetails' in "GHC.Hs.Pat").
--
-- * The left-hand side arguments in a pattern synonym binding
--   (see 'HsPatSynDetails' in "GHC.Hs.Binds").
--
-- One notable exception is the arguments in a GADT constructor, which uses
-- a separate data type entirely (see 'HsConDeclGADTDetails' in
-- "GHC.Hs.Decls"). This is because GADT constructors cannot be declared with
-- infix syntax, unlike the concepts above (#18844).
data HsConDetails tyarg arg rec
  = PrefixCon [tyarg] [arg]     -- C @t1 @t2 p1 p2 p3
  | RecCon    rec               -- C { x = p1, y = p2 }
  | InfixCon  arg arg           -- p1 `C` p2
  deriving Data

-- | An empty list that can be used to indicate that there are no
-- type arguments allowed in cases where HsConDetails is applied to Void.
noTypeArgs :: [Void]
noTypeArgs = []

instance (Outputable tyarg, Outputable arg, Outputable rec)
         => Outputable (HsConDetails tyarg arg rec) where
  ppr (PrefixCon tyargs args) = text "PrefixCon:" <+> hsep (map (\t -> text "@" <> ppr t) tyargs) <+> ppr args
  ppr (RecCon rec)            = text "RecCon:" <+> ppr rec
  ppr (InfixCon l r)          = text "InfixCon:" <+> ppr [l, r]

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

{- Note [hsScopedTvs and visible foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-XScopedTypeVariables can be defined in terms of a desugaring to
-XTypeAbstractions (GHC Proposal #50):

    fn :: forall a b c. tau(a,b,c)            fn :: forall a b c. tau(a,b,c)
    fn = defn(a,b,c)                   ==>    fn @x @y @z = defn(x,y,z)

That is, for every type variable of the leading 'forall' in the type signature,
we add an invisible binder at term level.

This model does not extend to visible forall, as discussed here:

* https://gitlab.haskell.org/ghc/ghc/issues/16734#note_203412
* https://github.com/ghc-proposals/ghc-proposals/pull/238

The conclusion of these discussions can be summarized as follows:

  > Assuming support for visible 'forall' in terms, consider this example:
  >
  >     vfn :: forall x y -> tau(x,y)
  >     vfn = \a b -> ...
  >
  > The user has written their own binders 'a' and 'b' to stand for 'x' and
  > 'y', and we definitely should not desugar this into:
  >
  >     vfn :: forall x y -> tau(x,y)
  >     vfn x y = \a b -> ...         -- bad!

This design choice is reflected in the design of HsOuterSigTyVarBndrs, which are
used in every place that ScopedTypeVariables takes effect:

  data HsOuterTyVarBndrs flag pass
    = HsOuterImplicit { ... }
    | HsOuterExplicit { ..., hso_bndrs :: [LHsTyVarBndr flag pass] }
    | ...
  type HsOuterSigTyVarBndrs = HsOuterTyVarBndrs Specificity

The HsOuterExplicit constructor is only used in type signatures with outermost,
/invisible/ 'forall's. Any other type—including those with outermost,
/visible/ 'forall's—will use HsOuterImplicit. Therefore, when we determine
which type variables to bring into scope over the body of a function
(in hsScopedTvs), we /only/ bring the type variables bound by the hso_bndrs in
an HsOuterExplicit into scope. If we have an HsOuterImplicit instead, then we
do not bring any type variables into scope over the body of a function at all.

At the moment, GHC does not support visible 'forall' in terms. Nevertheless,
it is still possible to write erroneous programs that use visible 'forall's in
terms, such as this example:

    x :: forall a -> a -> a
    x = x

Previous versions of GHC would bring `a` into scope over the body of `x` in the
hopes that the typechecker would error out later
(see `GHC.Tc.Validity.vdqAllowed`). However, this can wreak havoc in the
renamer before GHC gets to that point (see #17687 for an example of this).
Bottom line: nip problems in the bud by refraining from bringing any type
variables in an HsOuterImplicit into scope over the body of a function, even
if they correspond to a visible 'forall'.
-}

{-
************************************************************************
*                                                                      *
                Decomposing HsTypes
*                                                                      *
************************************************************************
-}

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

-- | @'pprHsArgsApp' id fixity args@ pretty-prints an application of @id@
-- to @args@, using the @fixity@ to tell whether @id@ should be printed prefix
-- or infix. Examples:
--
-- @
-- pprHsArgsApp T Prefix [HsTypeArg Bool, HsValArg Int]                        = T \@Bool Int
-- pprHsArgsApp T Prefix [HsTypeArg Bool, HsArgPar, HsValArg Int]              = (T \@Bool) Int
-- pprHsArgsApp (++) Infix [HsValArg Char, HsValArg Double]                    = Char ++ Double
-- pprHsArgsApp (++) Infix [HsValArg Char, HsValArg Double, HsVarArg Ordering] = (Char ++ Double) Ordering
-- @
pprHsArgsApp :: (OutputableBndr id, Outputable tm, Outputable ty)
             => id -> LexicalFixity -> [HsArg tm ty] -> SDoc
pprHsArgsApp thing fixity (argl:argr:args)
  | Infix <- fixity
  = let pp_op_app = hsep [ ppr_single_hs_arg argl
                         , pprInfixOcc thing
                         , ppr_single_hs_arg argr ] in
    case args of
      [] -> pp_op_app
      _  -> ppr_hs_args_prefix_app (parens pp_op_app) args

pprHsArgsApp thing _fixity args
  = ppr_hs_args_prefix_app (pprPrefixOcc thing) args

-- | Pretty-print a prefix identifier to a list of 'HsArg's.
ppr_hs_args_prefix_app :: (Outputable tm, Outputable ty)
                        => SDoc -> [HsArg tm ty] -> SDoc
ppr_hs_args_prefix_app acc []         = acc
ppr_hs_args_prefix_app acc (arg:args) =
  case arg of
    HsValArg{}  -> ppr_hs_args_prefix_app (acc <+> ppr_single_hs_arg arg) args
    HsTypeArg{} -> ppr_hs_args_prefix_app (acc <+> ppr_single_hs_arg arg) args
    HsArgPar{}  -> ppr_hs_args_prefix_app (parens acc) args

-- | Pretty-print an 'HsArg' in isolation.
ppr_single_hs_arg :: (Outputable tm, Outputable ty)
                  => HsArg tm ty -> SDoc
ppr_single_hs_arg (HsValArg tm)    = ppr tm
ppr_single_hs_arg (HsTypeArg _ ty) = char '@' <> ppr ty
-- GHC shouldn't be constructing ASTs such that this case is ever reached.
-- Still, it's possible some wily user might construct their own AST that
-- allows this to be reachable, so don't fail here.
ppr_single_hs_arg (HsArgPar{})     = empty

-- | This instance is meant for debug-printing purposes. If you wish to
-- pretty-print an application of 'HsArg's, use 'pprHsArgsApp' instead.
instance (Outputable tm, Outputable ty) => Outputable (HsArg tm ty) where
  ppr (HsValArg tm)     = text "HsValArg"  <+> ppr tm
  ppr (HsTypeArg sp ty) = text "HsTypeArg" <+> ppr sp <+> ppr ty
  ppr (HsArgPar sp)     = text "HsArgPar"  <+> ppr sp
{-
Note [HsArgPar]
~~~~~~~~~~~~~~~
A HsArgPar indicates that everything to the left of this in the argument list is
enclosed in parentheses together with the function itself. It is necessary so
that we can recreate the parenthesis structure in the original source after
typechecking the arguments.

The SrcSpan is the span of the original HsPar

((f arg1) arg2 arg3) results in an input argument list of
[HsValArg arg1, HsArgPar span1, HsValArg arg2, HsValArg arg3, HsArgPar span2]

-}

--------------------------------


{-
************************************************************************
*                                                                      *
                FieldOcc
*                                                                      *
************************************************************************
-}

-- | Located Field Occurrence
type LFieldOcc pass = XRec pass (FieldOcc pass)

-- | Field Occurrence
--
-- Represents an *occurrence* of a field. This may or may not be a
-- binding occurrence (e.g. this type is used in 'ConDeclField' and
-- 'RecordPatSynField' which bind their fields, but also in
-- 'HsRecField' for record construction and patterns, which do not).
--
-- We store both the 'RdrName' the user originally wrote, and after
-- the renamer we use the extension field to store the selector
-- function.
data FieldOcc pass
  = FieldOcc {
        foExt :: XCFieldOcc pass
      , foLabel :: XRec pass RdrName -- See Note [Located RdrNames] in Language.Haskell.Syntax.Expr
      }
  | XFieldOcc !(XXFieldOcc pass)
deriving instance (
    Eq (XRec pass RdrName)
  , Eq (XCFieldOcc pass)
  , Eq (XXFieldOcc pass)
  ) => Eq (FieldOcc pass)

instance Outputable (XRec pass RdrName) => Outputable (FieldOcc pass) where
  ppr = ppr . foLabel

instance (UnXRec pass, OutputableBndr (XRec pass RdrName)) => OutputableBndr (FieldOcc pass) where
  pprInfixOcc  = pprInfixOcc . unXRec @pass . foLabel
  pprPrefixOcc = pprPrefixOcc . unXRec @pass . foLabel

instance (UnXRec pass, OutputableBndr (XRec pass RdrName)) => OutputableBndr (GenLocated SrcSpan (FieldOcc pass)) where
  pprInfixOcc  = pprInfixOcc . unLoc
  pprPrefixOcc = pprPrefixOcc . unLoc

-- | Located Ambiguous Field Occurence
type LAmbiguousFieldOcc pass = XRec pass (AmbiguousFieldOcc pass)

-- | Ambiguous Field Occurrence
--
-- Represents an *occurrence* of a field that is potentially
-- ambiguous after the renamer, with the ambiguity resolved by the
-- typechecker.  We always store the 'RdrName' that the user
-- originally wrote, and store the selector function after the renamer
-- (for unambiguous occurrences) or the typechecker (for ambiguous
-- occurrences).
--
-- See Note [HsRecField and HsRecUpdField] in "GHC.Hs.Pat".
-- See Note [Located RdrNames] in "GHC.Hs.Expr".
data AmbiguousFieldOcc pass
  = Unambiguous (XUnambiguous pass) (LocatedN RdrName)
  | Ambiguous   (XAmbiguous pass)   (LocatedN RdrName)
  | XAmbiguousFieldOcc !(XXAmbiguousFieldOcc pass)


{-
************************************************************************
*                                                                      *
\subsection{Pretty printing}
*                                                                      *
************************************************************************
-}

instance Outputable HsTyLit where
    ppr = ppr_tylit
--------------------------
ppr_tylit :: HsTyLit -> SDoc
ppr_tylit (HsNumTy source i) = pprWithSourceText source (integer i)
ppr_tylit (HsStrTy source s) = pprWithSourceText source (text (show s))
ppr_tylit (HsCharTy source c) = pprWithSourceText source (text (show c))
