{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.HsToCore.Errors.Types where

import GHC.Prelude

import GHC.Core (CoreRule, CoreExpr, RuleName)
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Core.Type
import GHC.Driver.DynFlags (DynFlags, xopt)
import GHC.Driver.Flags (WarningFlag)
import GHC.Hs
import GHC.HsToCore.Pmc.Solver.Types
import GHC.Types.Basic (Activation)
import GHC.Types.Error
import GHC.Types.ForeignCall
import GHC.Types.Id
import GHC.Types.Name (Name)
import qualified GHC.LanguageExtensions as LangExt

import GHC.Generics (Generic)

newtype MinBound = MinBound Integer
newtype MaxBound = MaxBound Integer
type MaxUncoveredPatterns = Int
type MaxPmCheckModels = Int

-- | Diagnostics messages emitted during desugaring.
data DsMessage
  -- | Simply wraps a generic 'Diagnostic' message.
  = DsUnknownMessage (UnknownDiagnosticFor DsMessage)

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
  | DsRedundantBangPatterns !HsMatchContextRn !SDoc

  -- FIXME(adn) Use a proper type instead of 'SDoc', but unfortunately
  -- 'SrcInfo' gives us an 'SDoc' to begin with.
  | DsOverlappingPatterns !HsMatchContextRn !SDoc

  -- FIXME(adn) Use a proper type instead of 'SDoc'
  | DsInaccessibleRhs !HsMatchContextRn !SDoc

  | DsMaxPmCheckModelsReached !MaxPmCheckModels

  | DsNonExhaustivePatterns !HsMatchContextRn
                            !ExhaustivityCheckType
                            !MaxUncoveredPatterns
                            [Id]
                            [Nabla]

  | DsTopLevelBindsNotAllowed !BindsType !(HsBindLR GhcTc GhcTc)

    {-| DsUselessSpecialisePragma is a warning (controlled by the -Wuseless-specialisations flag)
        that is emitted for SPECIALISE pragmas that (most likely) don't do anything.

        Examples:

          foo :: forall a. a -> a
          {-# SPECIALISE foo :: Int -> Int #-}
    -}
  | DsUselessSpecialisePragma
      !Name
      !Bool -- ^ is this a @SPECIALISE instance@ pragma?
      !UselessSpecialisePragmaReason

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

  {-| DsIncompleteRecordSelector is a warning triggered when we are not certain whether
      a record selector application will be successful. Currently, this means that
      the warning is triggered when there is a record selector of a data type that
      does not have that field in all its constructors.

      Example(s):
      data T = T1 | T2 {x :: Bool}
      f :: T -> Bool
      f a = x a

     Test cases:
       DsIncompleteRecSel1
       DsIncompleteRecSel2
       DsIncompleteRecSel3
  -}
  | DsIncompleteRecordSelector !Name       -- ^ The selector
                               ![ConLike]  -- ^ The partial constructors
                               !Int        -- ^ The max number of constructors reported

  deriving Generic

-- The positional number of the argument for an expression (first, second, third, etc)
newtype DsArgNum = DsArgNum Int

-- | Why TemplateHaskell rejected the splice. Used in the 'DsNotYetHandledByTH'
-- constructor of a 'DsMessage'.
data ThRejectionReason
  = ThAmbiguousRecordUpdates !(HsRecUpdField GhcRn GhcRn)
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
  | ThDataConVisibleForall

-- | Why is a @SPECIALISE@ pragmas useless?
data UselessSpecialisePragmaReason
  -- | Useless @SPECIALISE@ pragma for a class method
  = UselessSpecialiseForClassMethodSelector
  -- | Useless @SPECIALISE@ pragma for a function with NOINLINE
  | UselessSpecialiseForNoInlineFunction
  -- | Useless @SPECIALISE@ pragma which generates a specialised function
  -- which is identical to the original function at runtime.
  | UselessSpecialiseNoSpecialisation
  deriving Generic

uselessSpecialisePragmaKeepAnyway :: UselessSpecialisePragmaReason -> Bool
uselessSpecialisePragmaKeepAnyway = \case
  UselessSpecialiseForClassMethodSelector -> False
  UselessSpecialiseForNoInlineFunction    -> False
  UselessSpecialiseNoSpecialisation       -> True
    -- See #25389/T25389 for why we might want to keep this specialisation
    -- around even if it seemingly does nothing.

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
