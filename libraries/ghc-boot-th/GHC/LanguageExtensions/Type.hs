-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.LanguageExtensions.Type
-- Copyright   :  (c) The GHC Team
--
-- Maintainer  :  ghc-devs@haskell.org
-- Portability :  portable
--
-- A data type defining the language extensions supported by GHC.
--
{-# LANGUAGE DeriveGeneric, Safe #-}
module GHC.LanguageExtensions.Type ( Extension(..) ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Generics

-- | The language extensions known to GHC.
--
-- Note that there is an orphan 'Binary' instance for this type supplied by
-- the "GHC.LanguageExtensions" module provided by @ghc-boot@. We can't provide
-- here as this would require adding transitive dependencies to the
-- @template-haskell@ package, which must have a minimal dependency set.
data Extension
-- See Note [Updating flag description in the User's Guide] in
-- GHC.Driver.Session
   = Cpp
   | OverlappingInstances
   | UndecidableInstances
   | IncoherentInstances
   | UndecidableSuperClasses
   | MonomorphismRestriction
   | MonoPatBinds
   | MonoLocalBinds
   | RelaxedPolyRec           -- Deprecated
   | ExtendedDefaultRules     -- Use GHC's extended rules for defaulting
   | ForeignFunctionInterface
   | UnliftedFFITypes
   | InterruptibleFFI
   | CApiFFI
   | GHCForeignImportPrim
   | JavaScriptFFI
   | ParallelArrays           -- Syntactic support for parallel arrays
   | Arrows                   -- Arrow-notation syntax
   | TemplateHaskell
   | TemplateHaskellQuotes    -- subset of TH supported by stage1, no splice
   | QualifiedDo
   | QuasiQuotes
   | ImplicitParams
   | ImplicitPrelude
   | ScopedTypeVariables
   | AllowAmbiguousTypes
   | UnboxedTuples
   | UnboxedSums
   | UnliftedNewtypes
   | BangPatterns
   | TypeFamilies
   | TypeFamilyDependencies
   | TypeInType
   | OverloadedStrings
   | OverloadedLists
   | NumDecimals
   | DisambiguateRecordFields
   | RecordWildCards
   | RecordPuns
   | ViewPatterns
   | GADTs
   | GADTSyntax
   | NPlusKPatterns
   | DoAndIfThenElse
   | BlockArguments
   | RebindableSyntax
   | ConstraintKinds
   | PolyKinds                -- Kind polymorphism
   | DataKinds                -- Datatype promotion
   | InstanceSigs
   | ApplicativeDo
   | LinearTypes

   | StandaloneDeriving
   | DeriveDataTypeable
   | AutoDeriveTypeable       -- Automatic derivation of Typeable
   | DeriveFunctor
   | DeriveTraversable
   | DeriveFoldable
   | DeriveGeneric            -- Allow deriving Generic/1
   | DefaultSignatures        -- Allow extra signatures for defmeths
   | DeriveAnyClass           -- Allow deriving any class
   | DeriveLift               -- Allow deriving Lift
   | DerivingStrategies
   | DerivingVia              -- Derive through equal representation

   | TypeSynonymInstances
   | FlexibleContexts
   | FlexibleInstances
   | ConstrainedClassMethods
   | MultiParamTypeClasses
   | NullaryTypeClasses
   | FunctionalDependencies
   | UnicodeSyntax
   | ExistentialQuantification
   | MagicHash
   | EmptyDataDecls
   | KindSignatures
   | RoleAnnotations
   | ParallelListComp
   | TransformListComp
   | MonadComprehensions
   | GeneralizedNewtypeDeriving
   | RecursiveDo
   | PostfixOperators
   | TupleSections
   | PatternGuards
   | LiberalTypeSynonyms
   | RankNTypes
   | ImpredicativeTypes
   | TypeOperators
   | ExplicitNamespaces
   | PackageImports
   | ExplicitForAll
   | AlternativeLayoutRule
   | AlternativeLayoutRuleTransitional
   | DatatypeContexts
   | NondecreasingIndentation
   | RelaxedLayout
   | TraditionalRecordSyntax
   | LambdaCase
   | MultiWayIf
   | BinaryLiterals
   | NegativeLiterals
   | HexFloatLiterals
   | DuplicateRecordFields
   | OverloadedLabels
   | EmptyCase
   | PatternSynonyms
   | PartialTypeSignatures
   | NamedWildCards
   | StaticPointers
   | TypeApplications
   | Strict
   | StrictData
   | MonadFailDesugaring
   | EmptyDataDeriving
   | NumericUnderscores
   | QuantifiedConstraints
   | StarIsType
   | ImportQualifiedPost
   | CUSKs
   | StandaloneKindSignatures
   | LexicalNegation
   deriving (Eq, Enum, Show, Generic, Bounded)
-- 'Ord' and 'Bounded' are provided for GHC API users (see discussions
-- in https://gitlab.haskell.org/ghc/ghc/merge_requests/2707 and
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/826).
instance Ord Extension where compare a b = compare (fromEnum a) (fromEnum b)
