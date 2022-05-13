{-# LANGUAGE ExistentialQuantification #-}

module GHC.HsToCore.Errors.Types where

import Data.Typeable

import GHC.Prelude

import GHC.Core (CoreRule, CoreExpr, RuleName)
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Driver.Session
import GHC.Hs
import GHC.HsToCore.Pmc.Solver.Types
import GHC.Types.Basic (Activation)
import GHC.Types.Error
import GHC.Types.ForeignCall
import GHC.Types.Id
import GHC.Types.Name (Name)
import qualified GHC.LanguageExtensions as LangExt

newtype MinBound = MinBound Integer
newtype MaxBound = MaxBound Integer
type MaxUncoveredPatterns = Int
type MaxPmCheckModels = Int

-- | Diagnostics messages emitted during desugaring.
data DsMessage
  -- | Simply wraps a generic 'Diagnostic' message.
  = forall a. (Diagnostic a, Typeable a) => DsUnknownMessage a

    {-| DsEmptyEnumeration is a warning (controlled by the -Wempty-enumerations flag) that is
        emitted if an enumeration is empty.

        Example(s):

          main :: IO ()
          main = do
            let enum = [5 .. 3]
            print enum

          Here 'enum' would yield an empty list, because 5 is greater than 3.

        Test case(s):
          warnings/should_compile/T10930
          warnings/should_compile/T18402
          warnings/should_compile/T10930b
          numeric/should_compile/T10929
          numeric/should_compile/T7881
          deSugar/should_run/T18172

    -}
  | DsEmptyEnumeration

    {-| DsIdentitiesFound is a warning (controlled by the -Widentities flag) that is
        emitted on uses of Prelude numeric conversions that are probably the identity
        (and hence could be omitted).

        Example(s):

          main :: IO ()
          main = do
            let x = 10
            print $ conv 10

            where
              conv :: Int -> Int
              conv x = fromIntegral x

        Here calling 'conv' is essentially the identity function, and therefore can be omitted.

        Test case(s):
          deSugar/should_compile/T4488
    -}
  | DsIdentitiesFound !Id   -- The conversion function
                      !Type -- The type of conversion

  | DsOverflowedLiterals !Integer
                         !Name
                         !(Maybe (MinBound, MaxBound))
                         !NegLiteralExtEnabled

  -- FIXME(adn) Use a proper type instead of 'SDoc', but unfortunately
  -- 'SrcInfo' gives us an 'SDoc' to begin with.
  | DsRedundantBangPatterns !(HsMatchContext GhcRn) !SDoc

  -- FIXME(adn) Use a proper type instead of 'SDoc', but unfortunately
  -- 'SrcInfo' gives us an 'SDoc' to begin with.
  | DsOverlappingPatterns !(HsMatchContext GhcRn) !SDoc

  -- FIXME(adn) Use a proper type instead of 'SDoc'
  | DsInaccessibleRhs !(HsMatchContext GhcRn) !SDoc

  | DsMaxPmCheckModelsReached !MaxPmCheckModels

  | DsNonExhaustivePatterns !(HsMatchContext GhcRn)
                            !ExhaustivityCheckType
                            !MaxUncoveredPatterns
                            [Id]
                            [Nabla]

  | DsTopLevelBindsNotAllowed !BindsType !(HsBindLR GhcTc GhcTc)

  | DsUselessSpecialiseForClassMethodSelector !Id

  | DsUselessSpecialiseForNoInlineFunction !Id

  | DsMultiplicityCoercionsNotSupported

  | DsOrphanRule !CoreRule

  | DsRuleLhsTooComplicated !CoreExpr !CoreExpr

  | DsRuleIgnoredDueToConstructor !DataCon

  | DsRuleBindersNotBound ![Var]
                          -- ^ The list of unbound binders
                          ![Var]
                          -- ^ The original binders
                          !CoreExpr
                          -- ^ The original LHS
                          !CoreExpr
                          -- ^ The optimised LHS

  | DsMultipleConForNewtype [LocatedN Name]

  | DsLazyPatCantBindVarsOfUnliftedType [Var]

  | DsNotYetHandledByTH !ThRejectionReason

  | DsAggregatedViewExpressions [[LHsExpr GhcTc]]

  | DsUnbangedStrictPatterns !(HsBindLR GhcTc GhcTc)

  | DsCannotMixPolyAndUnliftedBindings !(HsBindLR GhcTc GhcTc)

  | DsWrongDoBind !(LHsExpr GhcTc) !Type

  | DsUnusedDoBind !(LHsExpr GhcTc) !Type

  | DsRecBindsNotAllowedForUnliftedTys ![LHsBindLR GhcTc GhcTc]

  | DsRuleMightInlineFirst !RuleName !Var !Activation

  | DsAnotherRuleMightFireFirst !RuleName
                                !RuleName -- the \"bad\" rule
                                !Var

-- The positional number of the argument for an expression (first, second, third, etc)
newtype DsArgNum = DsArgNum Int

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
  | ThExoticLiteral !(HsLit GhcRn)
  | ThExoticPattern !(Pat GhcRn)
  | ThGuardedLambdas !(Match GhcRn (LHsExpr GhcRn))
  | ThNegativeOverloadedPatterns !(Pat GhcRn)
  | ThHaddockDocumentation
  | ThWarningAndDeprecationPragmas [LIdP GhcRn]
  | ThSplicesWithinDeclBrackets
  | ThNonLinearDataCon

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
