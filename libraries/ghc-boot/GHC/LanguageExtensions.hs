-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.LanguageExtensions
-- Copyright   :  (c) The GHC Team
--
-- Maintainer  :  ghc-devs@haskell.org
-- Portability :  portable
--
-- A data type defining the language extensions supported by GHC.
--
{-# LANGUAGE DeriveGeneric #-}
module GHC.LanguageExtensions ( Extension(..) ) where

import GHC.Generics
import Data.Binary

-- | The language extensions known to GHC.
data Extension
-- See Note [Updating flag description in the User's Guide] in DynFlags
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
   | QuasiQuotes
   | ImplicitParams
   | ImplicitPrelude
   | ScopedTypeVariables
   | AllowAmbiguousTypes
   | UnboxedTuples
   | BangPatterns
   | TypeFamilies
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
   | RebindableSyntax
   | ConstraintKinds
   | PolyKinds                -- Kind polymorphism
   | DataKinds                -- Datatype promotion
   | InstanceSigs
   | ApplicativeDo

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
   | DuplicateRecordFields
   | OverloadedLabels
   | EmptyCase
   | PatternSynonyms
   | PartialTypeSignatures
   | NamedWildCards
   | StaticPointers
   | Strict
   | StrictData
   | MonadFailDesugaring
   deriving (Eq, Enum, Show, Generic)

instance Binary Extension
