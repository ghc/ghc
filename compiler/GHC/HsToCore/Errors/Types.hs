{-# LANGUAGE GADTs #-}

module GHC.HsToCore.Errors.Types where

import Data.Typeable

import GHC.Prelude

import GHC.Core (CoreRule, CoreExpr)
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Driver.Session
import GHC.Hs (HsType, LForeignDecl, LFamilyDecl, HsRecUpdField, LocatedN, HsBindLR, GhcRn, Pat, GhcTc)
import GHC.Hs.Expr
import GHC.Types.Error
import GHC.Types.Id
import GHC.Types.Name (Name)
import qualified GHC.LanguageExtensions as LangExt
import {-# SOURCE #-} GHC.HsToCore.Pmc.Solver.Types (Nabla) -- sad, but avoids dep cycles
import GHC.Types.ForeignCall

newtype MinBound = MinBound Integer
newtype MaxBound = MaxBound Integer
type MaxUncoveredPatterns = Int
type MaxPmCheckModels = Int

-- | Diagnostics messages emitted during desugaring.
data DsMessage where
  -- | Simply wraps a generic 'Diagnostic' message.
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

  -- FIXME(adn) Use a proper type instead of 'SDoc', but unfortunately
  -- 'SrcInfo' gives us an 'SDoc' to begin with.
  DsRedundantBangPatterns :: !(HsMatchContext GhcRn) -> !SDoc -> DsMessage

  -- FIXME(adn) Use a proper type instead of 'SDoc', but unfortunately
  -- 'SrcInfo' gives us an 'SDoc' to begin with.
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

  DsOrphanRule :: !CoreRule -> DsMessage

  DsRuleLhsTooComplicated :: !CoreExpr -> !CoreExpr -> DsMessage

  DsRuleIgnoredDueToConstructor :: !DataCon -> DsMessage

  DsRuleBindersNotBound :: ![Var]
                        -- ^ The list of unbound binders
                        -> ![Var]
                        -- ^ The original binders
                        -> !CoreExpr
                        -- ^ The original LHS
                        -> !CoreExpr
                        -- ^ The optimised LHS
                        -> DsMessage

  DsMultipleConForNewtype :: [LocatedN Name] -> DsMessage

  DsLazyPatCantBindVarsOfUnliftedType :: [Var] -> DsMessage

  DsNotYetHandledByTH :: !ThRejectionReason -> DsMessage

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

-- | Why TemplateHaskell rejected the splice. Used in the 'DsNotYetHandledByTH'
-- constructor of a 'DsMessage'.
data ThRejectionReason
  = ThAmbiguousRecordUpdates !(HsRecUpdField GhcRn)
  | ThAbstractClosedTypeFamily !(LFamilyDecl GhcRn)
  | ThForeignLabel !CLabelString
  | ThForeignExport !(LForeignDecl GhcRn)
  | ThMinimalPragmas
  | ThSCCPragmas
  | ThNoUserInline
  | ThExoticFormOfType !(HsType GhcRn)
  | ThAmbiguousRecordSelectors !(HsExpr GhcRn)
  | ThMonadComprehensionSyntax !(HsExpr GhcRn)
  | ThCostCentres !(HsExpr GhcRn)
  | ThExpressionForm !(HsExpr GhcRn)
  | ThExoticStatement [Stmt GhcRn (LHsExpr GhcRn)]
  | ThGuardedLambdas !(Match GhcRn (LHsExpr GhcRn))

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
