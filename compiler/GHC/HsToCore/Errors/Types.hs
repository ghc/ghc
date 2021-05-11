{-# LANGUAGE GADTs #-}

module GHC.HsToCore.Errors.Types where

import Data.Typeable

import GHC.Prelude

import GHC.Core (CoreExpr)
import GHC.Core.Type
import GHC.Driver.Session
import GHC.Hs (HsBindLR, GhcRn, Pat, GhcTc)
import GHC.Hs.Expr
import GHC.Types.Error
import GHC.Types.Id
import GHC.Types.Name (Name)
import qualified GHC.LanguageExtensions as LangExt
import {-# SOURCE #-} GHC.HsToCore.Pmc.Solver.Types (Nabla) -- sad, but avoids dep cycles

newtype MinBound = MinBound Integer
newtype MaxBound = MaxBound Integer
type MaxUncoveredPatterns = Int
type MaxPmCheckModels = Int

-- | Diagnostics messages emitted during desugaring.
data DsMessage where
  -- | Simply wraps a generic 'DiagnosticMessage'. More
  -- constructors will be added in the future (#18516).
  DsUnknownMessage :: (Diagnostic a, Typeable a) => a -> DsMessage

  DsEmptyEnumeration :: DsMessage

  DsIdentitiesFound  :: !Id   -- The conversion function
                     -> !Type -- The type of conversion
                     -> DsMessage

  DsOverflowedLiterals :: !Integer
                       -> !Name
                       -> !(Maybe (MinBound, MaxBound))
                       -> !NegLiteralExtEnabled
                       -> DsMessage

  -- FIXME(adn) Use a proper type instead of 'SDoc'.
  DsRedundantBangPatterns :: !(HsMatchContext GhcRn) -> !SDoc -> DsMessage

  -- FIXME(adn) Use a proper type instead of 'SDoc'.
  DsOverlappingPatterns :: !(HsMatchContext GhcRn) -> !SDoc -> DsMessage

  DsInaccessibleRhs :: !(HsMatchContext GhcRn) -> !SDoc -> DsMessage

  DsMaxPmCheckModelsReached :: !MaxPmCheckModels -> DsMessage

  DsNonExhaustivePatterns :: !(HsMatchContext GhcRn)
                          -> !ExhaustivityCheckType
                          -> !MaxUncoveredPatterns
                          -> [Id]
                          -> [Nabla]
                          -> DsMessage

  DsTopLevelBindsNotAllowed :: BindsType -> HsBindLR GhcTc GhcTc -> DsMessage

  DsUselessSpecialiseForClassMethodSelector :: Id -> DsMessage

  DsUselessSpecialiseForNoInlineFunction :: Id -> DsMessage

  DsMultiplicityCoercionsNotSupported :: DsMessage

  DsLevityPolyInExpr :: !CoreExpr -> !LevityExprProvenance -> DsMessage

  DsLevityPolyInType :: !Type -> !LevityCheckProvenance -> DsMessage

-- | Where the levity checking for the input type originated
data LevityCheckProvenance
  = LevityCheckInVarType
  | LevityCheckInBinder !Var
  | LevityCheckInWildcardPattern
  | LevityCheckInUnboxedTuplePattern !(Pat GhcTc)
  | LevityCheckGenSig

-- | Where the levity checking for the expression originated
data LevityExprProvenance
  = LevityCheckHsExpr !(HsExpr GhcTc)
  | LevityCheckWpFun !SDoc -- FIXME(adn) Alas 'WpFun' gives us an SDoc here.

data NegLiteralExtEnabled
  = YesUsingNegLiterals
  | NotUsingNegLiterals

negLiteralExtEnabled :: DynFlags -> NegLiteralExtEnabled
negLiteralExtEnabled dflags =
 if (xopt LangExt.NegativeLiterals dflags) then YesUsingNegLiterals else NotUsingNegLiterals

newtype ExhaustivityCheckType = ExhaustivityCheckType (Maybe WarningFlag)

data BindsType
  = UnliftedTypeBinds
  | StrictBinds
