{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Extension
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- Haskell language dialects and extensions

module Language.Haskell.Extension (
        Language(..),
        knownLanguages,
        classifyLanguage,

        Extension(..),
        KnownExtension(..),
        knownExtensions,
        deprecatedExtensions,
        classifyExtension,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Data.Array (Array, accumArray, bounds, Ix(inRange), (!))

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text

import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp

-- ------------------------------------------------------------
-- * Language
-- ------------------------------------------------------------

-- | This represents a Haskell language dialect.
--
-- Language 'Extension's are interpreted relative to one of these base
-- languages.
--
data Language =

  -- | The Haskell 98 language as defined by the Haskell 98 report.
  -- <http://haskell.org/onlinereport/>
     Haskell98

  -- | The Haskell 2010 language as defined by the Haskell 2010 report.
  -- <http://www.haskell.org/onlinereport/haskell2010>
  | Haskell2010

  -- | An unknown language, identified by its name.
  | UnknownLanguage String
  deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary Language

knownLanguages :: [Language]
knownLanguages = [Haskell98, Haskell2010]

instance Pretty Language where
  pretty (UnknownLanguage other) = Disp.text other
  pretty other                   = Disp.text (show other)

instance Parsec Language where
  parsec = classifyLanguage <$> P.munch1 isAlphaNum

instance Text Language where
  parse = do
    lang <- Parse.munch1 isAlphaNum
    return (classifyLanguage lang)

classifyLanguage :: String -> Language
classifyLanguage = \str -> case lookup str langTable of
    Just lang -> lang
    Nothing   -> UnknownLanguage str
  where
    langTable = [ (show lang, lang)
                | lang <- knownLanguages ]

-- ------------------------------------------------------------
-- * Extension
-- ------------------------------------------------------------

-- Note: if you add a new 'KnownExtension':
--
-- * also add it to the Distribution.Simple.X.languageExtensions lists
--   (where X is each compiler: GHC, JHC, LHC, UHC, HaskellSuite)
--
-- | This represents language extensions beyond a base 'Language' definition
-- (such as 'Haskell98') that are supported by some implementations, usually
-- in some special mode.
--
-- Where applicable, references are given to an implementation's
-- official documentation.

data Extension =
  -- | Enable a known extension
    EnableExtension KnownExtension

  -- | Disable a known extension
  | DisableExtension KnownExtension

  -- | An unknown extension, identified by the name of its @LANGUAGE@
  -- pragma.
  | UnknownExtension String

  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance Binary Extension

data KnownExtension =

  -- | Allow overlapping class instances, provided there is a unique
  -- most specific instance for each use.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XOverlappingInstances>
    OverlappingInstances

  -- | Ignore structural rules guaranteeing the termination of class
  -- instance resolution.  Termination is guaranteed by a fixed-depth
  -- recursion stack, and compilation may fail if this depth is
  -- exceeded.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XUndecidableInstances>
  | UndecidableInstances

  -- | Implies 'OverlappingInstances'.  Allow the implementation to
  -- choose an instance even when it is possible that further
  -- instantiation of types will lead to a more specific instance
  -- being applicable.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XIncoherentInstances>
  | IncoherentInstances

  -- | /(deprecated)/ Deprecated in favour of 'RecursiveDo'.
  --
  -- Old description: Allow recursive bindings in @do@ blocks, using
  -- the @rec@ keyword. See also 'RecursiveDo'.
  | DoRec

  -- | Allow recursive bindings in @do@ blocks, using the @rec@
  -- keyword, or @mdo@, a variant of @do@.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XRecursiveDo>
  | RecursiveDo

  -- | Provide syntax for writing list comprehensions which iterate
  -- over several lists together, like the 'zipWith' family of
  -- functions.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XParallelListComp>
  | ParallelListComp

  -- | Allow multiple parameters in a type class.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XMultiParamTypeClasses>
  | MultiParamTypeClasses

  -- | Enable the dreaded monomorphism restriction.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XNoMonomorphismRestriction>
  | MonomorphismRestriction

  -- | Allow a specification attached to a multi-parameter type class
  -- which indicates that some parameters are entirely determined by
  -- others. The implementation will check that this property holds
  -- for the declared instances, and will use this property to reduce
  -- ambiguity in instance resolution.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XFunctionalDependencies>
  | FunctionalDependencies

  -- | /(deprecated)/ A synonym for 'RankNTypes'.
  --
  -- Old description: Like 'RankNTypes' but does not allow a
  -- higher-rank type to itself appear on the left of a function
  -- arrow.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XRank2Types>
  | Rank2Types

  -- | Allow a universally-quantified type to occur on the left of a
  -- function arrow.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XRankNTypes>
  | RankNTypes

  -- | /(deprecated)/ A synonym for 'RankNTypes'.
  --
  -- Old description: Allow data constructors to have polymorphic
  -- arguments.  Unlike 'RankNTypes', does not allow this for ordinary
  -- functions.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#arbitrary-rank-polymorphism>
  | PolymorphicComponents

  -- | Allow existentially-quantified data constructors.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XExistentialQuantification>
  | ExistentialQuantification

  -- | Cause a type variable in a signature, which has an explicit
  -- @forall@ quantifier, to scope over the definition of the
  -- accompanying value declaration.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XScopedTypeVariables>
  | ScopedTypeVariables

  -- | Deprecated, use 'ScopedTypeVariables' instead.
  | PatternSignatures

  -- | Enable implicit function parameters with dynamic scope.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XImplicitParams>
  | ImplicitParams

  -- | Relax some restrictions on the form of the context of a type
  -- signature.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XFlexibleContexts>
  | FlexibleContexts

  -- | Relax some restrictions on the form of the context of an
  -- instance declaration.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XFlexibleInstances>
  | FlexibleInstances

  -- | Allow data type declarations with no constructors.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XEmptyDataDecls>
  | EmptyDataDecls

  -- | Run the C preprocessor on Haskell source code.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#language-pragma>
  | CPP

  -- | Allow an explicit kind signature giving the kind of types over
  -- which a type variable ranges.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XKindSignatures>
  | KindSignatures

  -- | Enable a form of pattern which forces evaluation before an
  -- attempted match, and a form of strict @let@/@where@ binding.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XBangPatterns>
  | BangPatterns

  -- | Allow type synonyms in instance heads.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XTypeSynonymInstances>
  | TypeSynonymInstances

  -- | Enable Template Haskell, a system for compile-time
  -- metaprogramming.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XTemplateHaskell>
  | TemplateHaskell

  -- | Enable the Foreign Function Interface.  In GHC, implements the
  -- standard Haskell 98 Foreign Function Interface Addendum, plus
  -- some GHC-specific extensions.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#language-pragma>
  | ForeignFunctionInterface

  -- | Enable arrow notation.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XArrows>
  | Arrows

  -- | /(deprecated)/ Enable generic type classes, with default instances defined in
  -- terms of the algebraic structure of a type.
  --
  -- * <https://haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#generic-classes>
  | Generics

  -- | Enable the implicit importing of the module "Prelude".  When
  -- disabled, when desugaring certain built-in syntax into ordinary
  -- identifiers, use whatever is in scope rather than the "Prelude"
  -- -- version.
  --
  -- * <https://haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#rebindable-syntax-and-the-implicit-prelude-import>
  | ImplicitPrelude

  -- | Enable syntax for implicitly binding local names corresponding
  -- to the field names of a record.  Puns bind specific names, unlike
  -- 'RecordWildCards'.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XNamedFieldPuns>
  | NamedFieldPuns

  -- | Enable a form of guard which matches a pattern and binds
  -- variables.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XPatternGuards>
  | PatternGuards

  -- | Allow a type declared with @newtype@ to use @deriving@ for any
  -- class with an instance for the underlying type.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XGeneralizedNewtypeDeriving>
  | GeneralizedNewtypeDeriving

  -- | Enable the \"Trex\" extensible records system.
  --
  -- * <http://haskell.org/hugs/pages/users_guide/hugs-only.html#TREX>
  | ExtensibleRecords

  -- | Enable type synonyms which are transparent in some definitions
  -- and opaque elsewhere, as a way of implementing abstract
  -- datatypes.
  --
  -- * <http://haskell.org/hugs/pages/users_guide/restricted-synonyms.html>
  | RestrictedTypeSynonyms

  -- | Enable an alternate syntax for string literals,
  -- with string templating.
  --
  -- * <http://haskell.org/hugs/pages/users_guide/here-documents.html>
  | HereDocuments

  -- | Allow the character @#@ as a postfix modifier on identifiers.
  -- Also enables literal syntax for unboxed values.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XMagicHash>
  | MagicHash

  -- | Allow data types and type synonyms which are indexed by types,
  -- i.e. ad-hoc polymorphism for types.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XTypeFamilies>
  | TypeFamilies

  -- | Allow a standalone declaration which invokes the type class
  -- @deriving@ mechanism.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XStandaloneDeriving>
  | StandaloneDeriving

  -- | Allow certain Unicode characters to stand for certain ASCII
  -- character sequences, e.g. keywords and punctuation.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XUnicodeSyntax>
  | UnicodeSyntax

  -- | Allow the use of unboxed types as foreign types, e.g. in
  -- @foreign import@ and @foreign export@.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#language-options>
  | UnliftedFFITypes

  -- | Enable interruptible FFI.
  --
  -- * <https://haskell.org/ghc/docs/latest/html/users_guide/ffi-chap.html#interruptible-foreign-calls>
  | InterruptibleFFI

  -- | Allow use of CAPI FFI calling convention (@foreign import capi@).
  --
  -- * <https://haskell.org/ghc/docs/latest/html/users_guide/ffi-chap.html#the-capi-calling-convention>
  | CApiFFI

  -- | Defer validity checking of types until after expanding type
  -- synonyms, relaxing the constraints on how synonyms may be used.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XLiberalTypeSynonyms>
  | LiberalTypeSynonyms

  -- | Allow the name of a type constructor, type class, or type
  -- variable to be an infix operator.
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XTypeOperators>
  | TypeOperators

  -- | Enable syntax for implicitly binding local names corresponding
  -- to the field names of a record.  A wildcard binds all unmentioned
  -- names, unlike 'NamedFieldPuns'.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XRecordWildCards>
  | RecordWildCards

  -- | Deprecated, use 'NamedFieldPuns' instead.
  | RecordPuns

  -- | Allow a record field name to be disambiguated by the type of
  -- the record it's in.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XDisambiguateRecordFields>
  | DisambiguateRecordFields

  -- | Enable traditional record syntax (as supported by Haskell 98)
  --
  -- * <https://haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#traditional-record-syntax>
  | TraditionalRecordSyntax

  -- | Enable overloading of string literals using a type class, much
  -- like integer literals.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XOverloadedStrings>
  | OverloadedStrings

  -- | Enable generalized algebraic data types, in which type
  -- variables may be instantiated on a per-constructor basis. Implies
  -- 'GADTSyntax'.
  --
  -- * <https://haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#generalised-algebraic-data-types-gadts>
  | GADTs

  -- | Enable GADT syntax for declaring ordinary algebraic datatypes.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XGADTSyntax>
  | GADTSyntax

  -- | /(deprecated)/ Has no effect.
  --
  -- Old description: Make pattern bindings monomorphic.
  --
  -- * <https://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/monomorphism.html>
  | MonoPatBinds

  -- | Relax the requirements on mutually-recursive polymorphic
  -- functions.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XRelaxedPolyRec>
  | RelaxedPolyRec

  -- | Allow default instantiation of polymorphic types in more
  -- situations.
  --
  -- * <http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#type-defaulting-in-ghci>
  | ExtendedDefaultRules

  -- | Enable unboxed tuples.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XUnboxedTuples>
  | UnboxedTuples

  -- | Enable @deriving@ for classes 'Data.Typeable.Typeable' and
  -- 'Data.Generics.Data'.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XDeriveDataTypeable>
  | DeriveDataTypeable

  -- | Enable @deriving@ for 'GHC.Generics.Generic' and 'GHC.Generics.Generic1'.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XDeriveGeneric>
  | DeriveGeneric

  -- | Enable support for default signatures.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XDefaultSignatures>
  | DefaultSignatures

  -- | Allow type signatures to be specified in instance declarations.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XInstanceSigs>
  | InstanceSigs

  -- | Allow a class method's type to place additional constraints on
  -- a class type variable.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XConstrainedClassMethods>
  | ConstrainedClassMethods

  -- | Allow imports to be qualified by the package name the module is
  -- intended to be imported from, e.g.
  --
  -- > import "network" Network.Socket
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XPackageImports>
  | PackageImports

  -- | /(deprecated)/ Allow a type variable to be instantiated at a
  -- polymorphic type.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XImpredicativeTypes>
  | ImpredicativeTypes

  -- | /(deprecated)/ Change the syntax for qualified infix operators.
  --
  -- * <http://www.haskell.org/ghc/docs/6.12.3/html/users_guide/syntax-extns.html#new-qualified-operators>
  | NewQualifiedOperators

  -- | Relax the interpretation of left operator sections to allow
  -- unary postfix operators.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XPostfixOperators>
  | PostfixOperators

  -- | Enable quasi-quotation, a mechanism for defining new concrete
  -- syntax for expressions and patterns.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XQuasiQuotes>
  | QuasiQuotes

  -- | Enable generalized list comprehensions, supporting operations
  -- such as sorting and grouping.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XTransformListComp>
  | TransformListComp

  -- | Enable monad comprehensions, which generalise the list
  -- comprehension syntax to work for any monad.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XMonadComprehensions>
  | MonadComprehensions

  -- | Enable view patterns, which match a value by applying a
  -- function and matching on the result.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XViewPatterns>
  | ViewPatterns

  -- | Allow concrete XML syntax to be used in expressions and patterns,
  -- as per the Haskell Server Pages extension language:
  -- <http://www.haskell.org/haskellwiki/HSP>. The ideas behind it are
  -- discussed in the paper \"Haskell Server Pages through Dynamic Loading\"
  -- by Niklas Broberg, from Haskell Workshop '05.
  | XmlSyntax

  -- | Allow regular pattern matching over lists, as discussed in the
  -- paper \"Regular Expression Patterns\" by Niklas Broberg, Andreas Farre
  -- and Josef Svenningsson, from ICFP '04.
  | RegularPatterns

  -- | Enable the use of tuple sections, e.g. @(, True)@ desugars into
  -- @\x -> (x, True)@.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XTupleSections>
  | TupleSections

  -- | Allow GHC primops, written in C--, to be imported into a Haskell
  -- file.
  | GHCForeignImportPrim

  -- | Support for patterns of the form @n + k@, where @k@ is an
  -- integer literal.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XNPlusKPatterns>
  | NPlusKPatterns

  -- | Improve the layout rule when @if@ expressions are used in a @do@
  -- block.
  | DoAndIfThenElse

  -- | Enable support for multi-way @if@-expressions.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XMultiWayIf>
  | MultiWayIf

  -- | Enable support lambda-@case@ expressions.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XLambdaCase>
  | LambdaCase

  -- | Makes much of the Haskell sugar be desugared into calls to the
  -- function with a particular name that is in scope.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XRebindableSyntax>
  | RebindableSyntax

  -- | Make @forall@ a keyword in types, which can be used to give the
  -- generalisation explicitly.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XExplicitForAll>
  | ExplicitForAll

  -- | Allow contexts to be put on datatypes, e.g. the @Eq a@ in
  -- @data Eq a => Set a = NilSet | ConsSet a (Set a)@.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XDatatypeContexts>
  | DatatypeContexts

  -- | Local (@let@ and @where@) bindings are monomorphic.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XMonoLocalBinds>
  | MonoLocalBinds

  -- | Enable @deriving@ for the 'Data.Functor.Functor' class.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XDeriveFunctor>
  | DeriveFunctor

  -- | Enable @deriving@ for the 'Data.Traversable.Traversable' class.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XDeriveTraversable>
  | DeriveTraversable

  -- | Enable @deriving@ for the 'Data.Foldable.Foldable' class.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XDeriveFoldable>
  | DeriveFoldable

  -- | Enable non-decreasing indentation for @do@ blocks.
  --
  -- * <https://haskell.org/ghc/docs/latest/html/users_guide/bugs.html#context-free-syntax>
  | NondecreasingIndentation

  -- | Allow imports to be qualified with a safe keyword that requires
  -- the imported module be trusted as according to the Safe Haskell
  -- definition of trust.
  --
  -- > import safe Network.Socket
  --
  -- * <https://haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#safe-imports>
  | SafeImports

  -- | Compile a module in the Safe, Safe Haskell mode -- a restricted
  -- form of the Haskell language to ensure type safety.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/safe_haskell.html#ghc-flag--XSafe>
  | Safe

  -- | Compile a module in the Trustworthy, Safe Haskell mode -- no
  -- restrictions apply but the module is marked as trusted as long as
  -- the package the module resides in is trusted.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/safe_haskell.html#ghc-flag--XTrustworthy>
  | Trustworthy

  -- | Compile a module in the Unsafe, Safe Haskell mode so that
  -- modules compiled using Safe, Safe Haskell mode can't import it.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/safe_haskell.html#ghc-flag--XUnsafe>
  | Unsafe

  -- | Allow type class/implicit parameter/equality constraints to be
  -- used as types with the special kind constraint.  Also generalise
  -- the @(ctxt => ty)@ syntax so that any type of kind constraint can
  -- occur before the arrow.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XConstraintKinds>
  | ConstraintKinds

  -- | Enable kind polymorphism.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XPolyKinds>
  | PolyKinds

  -- | Enable datatype promotion.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XDataKinds>
  | DataKinds

  -- | Enable parallel arrays syntax (@[:@, @:]@) for /Data Parallel Haskell/.
  --
  -- * <http://www.haskell.org/haskellwiki/GHC/Data_Parallel_Haskell>
  | ParallelArrays

  -- | Enable explicit role annotations, like in (@type role Foo representational representational@).
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XRoleAnnotations>
  | RoleAnnotations

  -- | Enable overloading of list literals, arithmetic sequences and
  -- list patterns using the 'IsList' type class.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XOverloadedLists>
  | OverloadedLists

  -- | Enable case expressions that have no alternatives. Also applies to lambda-case expressions if they are enabled.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XEmptyCase>
  | EmptyCase

  -- | /(deprecated)/ Deprecated in favour of 'DeriveDataTypeable'.
  --
  -- Old description: Triggers the generation of derived 'Typeable'
  -- instances for every datatype and type class declaration.
  --
  -- * <https://haskell.org/ghc/docs/7.8.4/html/users_guide/deriving.html#auto-derive-typeable>
  | AutoDeriveTypeable

  -- | Desugars negative literals directly (without using negate).
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XNegativeLiterals>
  | NegativeLiterals

  -- | Allow the use of binary integer literal syntax (e.g. @0b11001001@ to denote @201@).
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XBinaryLiterals>
  | BinaryLiterals

  -- | Allow the use of floating literal syntax for all instances of 'Num', including 'Int' and 'Integer'.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XNumDecimals>
  | NumDecimals

  -- | Enable support for type classes with no type parameter.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XNullaryTypeClasses>
  | NullaryTypeClasses

  -- | Enable explicit namespaces in module import/export lists.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XExplicitNamespaces>
  | ExplicitNamespaces

  -- | Allow the user to write ambiguous types, and the type inference engine to infer them.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XAllowAmbiguousTypes>
  | AllowAmbiguousTypes

  -- | Enable @foreign import javascript@.
  | JavaScriptFFI

  -- | Allow giving names to and abstracting over patterns.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XPatternSynonyms>
  | PatternSynonyms

  -- | Allow anonymous placeholders (underscore) inside type signatures.  The
  -- type inference engine will generate a message describing the type inferred
  -- at the hole's location.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XPartialTypeSignatures>
  | PartialTypeSignatures

  -- | Allow named placeholders written with a leading underscore inside type
  -- signatures.  Wildcards with the same name unify to the same type.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XNamedWildCards>
  | NamedWildCards

  -- | Enable @deriving@ for any class.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XDeriveAnyClass>
  | DeriveAnyClass

  -- | Enable @deriving@ for the 'Language.Haskell.TH.Syntax.Lift' class.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XDeriveLift>
  | DeriveLift

  -- | Enable support for 'static pointers' (and the @static@
  -- keyword) to refer to globally stable names, even across
  -- different programs.
  --
  -- * <https://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#ghc-flag--XStaticPointers>
  | StaticPointers

  -- | Switches data type declarations to be strict by default (as if
  -- they had a bang using @BangPatterns@), and allow opt-in field
  -- laziness using @~@.
  --
  -- * <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XStrictData>
  | StrictData

  -- | Switches all pattern bindings to be strict by default (as if
  -- they had a bang using @BangPatterns@), ordinary patterns are
  -- recovered using @~@. Implies @StrictData@.
  --
  -- * <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XStrict>
  | Strict

  -- | Allows @do@-notation for types that are @'Applicative'@ as well
  -- as @'Monad'@. When enabled, desugaring @do@ notation tries to use
  -- @(<*>)@ and @'fmap'@ and @'join'@ as far as possible.
  | ApplicativeDo

  -- | Allow records to use duplicated field labels for accessors.
  | DuplicateRecordFields

  -- | Enable explicit type applications with the syntax @id \@Int@.
  | TypeApplications

  -- | Dissolve the distinction between types and kinds, allowing the compiler
  -- to reason about kind equality and therefore enabling GADTs to be promoted
  -- to the type-level.
  | TypeInType

  -- | Allow recursive (and therefore undecideable) super-class relationships.
  | UndecidableSuperClasses

  -- | A temporary extension to help library authors check if their
  -- code will compile with the new planned desugaring of fail.
  | MonadFailDesugaring

  -- | A subset of @TemplateHaskell@ including only quoting.
  | TemplateHaskellQuotes

  -- | Allows use of the @#label@ syntax.
  | OverloadedLabels

  -- | Allow functional dependency annotations on type families to declare them
  -- as injective.
  | TypeFamilyDependencies

  -- | Allow multiple @deriving@ clauses, each optionally qualified with a
  -- /strategy/.
  | DerivingStrategies

  -- | Enable the use of unboxed sum syntax.
  | UnboxedSums

  -- | Allow use of hexadecimal literal notation for floating-point values.
  | HexFloatLiterals

  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded, Typeable, Data)

instance Binary KnownExtension

{-# DEPRECATED knownExtensions
   "KnownExtension is an instance of Enum and Bounded, use those instead." #-}
knownExtensions :: [KnownExtension]
knownExtensions = [minBound..maxBound]

-- | Extensions that have been deprecated, possibly paired with another
-- extension that replaces it.
--
deprecatedExtensions :: [(Extension, Maybe Extension)]
deprecatedExtensions =
  [ (EnableExtension RecordPuns, Just (EnableExtension NamedFieldPuns))
  , (EnableExtension PatternSignatures, Just (EnableExtension ScopedTypeVariables))
  ]
-- NOTE: when adding deprecated extensions that have new alternatives
-- we must be careful to make sure that the deprecation messages are
-- valid. We must not recommend aliases that cannot be used with older
-- compilers, perhaps by adding support in Cabal to translate the new
-- name to the old one for older compilers. Otherwise we are in danger
-- of the scenario in ticket #689.

instance Pretty Extension where
  pretty (UnknownExtension other) = Disp.text other
  pretty (EnableExtension ke)     = Disp.text (show ke)
  pretty (DisableExtension ke)    = Disp.text ("No" ++ show ke)

instance Parsec Extension where
  parsec = classifyExtension <$> P.munch1 isAlphaNum

instance Text Extension where
  parse = do
    extension <- Parse.munch1 isAlphaNum
    return (classifyExtension extension)

instance Pretty KnownExtension where
  pretty ke = Disp.text (show ke)

instance Text KnownExtension where
  parse = do
    extension <- Parse.munch1 isAlphaNum
    case classifyKnownExtension extension of
        Just ke ->
            return ke
        Nothing ->
            fail ("Can't parse " ++ show extension ++ " as KnownExtension")

classifyExtension :: String -> Extension
classifyExtension string
  = case classifyKnownExtension string of
    Just ext -> EnableExtension ext
    Nothing ->
        case string of
        'N':'o':string' ->
            case classifyKnownExtension string' of
            Just ext -> DisableExtension ext
            Nothing -> UnknownExtension string
        _ -> UnknownExtension string

-- | 'read' for 'KnownExtension's is really really slow so for the Text
-- instance
-- what we do is make a simple table indexed off the first letter in the
-- extension name. The extension names actually cover the range @'A'-'Z'@
-- pretty densely and the biggest bucket is 7 so it's not too bad. We just do
-- a linear search within each bucket.
--
-- This gives an order of magnitude improvement in parsing speed, and it'll
-- also allow us to do case insensitive matches in future if we prefer.
--
classifyKnownExtension :: String -> Maybe KnownExtension
classifyKnownExtension "" = Nothing
classifyKnownExtension string@(c : _)
  | inRange (bounds knownExtensionTable) c
  = lookup string (knownExtensionTable ! c)
  | otherwise = Nothing

knownExtensionTable :: Array Char [(String, KnownExtension)]
knownExtensionTable =
  accumArray (flip (:)) [] ('A', 'Z')
    [ (head str, (str, extension))
    | extension <- [toEnum 0 ..]
    , let str = show extension ]
