{-# LANGUAGE ExistentialQuantification #-}

module GHC.Types.Hint (
  GhcHint(..),
  InstantiationSuggestion(..)
  ) where

import GHC.Utils.Outputable
import qualified GHC.LanguageExtensions as LangExt
import Data.Typeable
import GHC.Unit.Module (ModuleName, Module)
import GHC.Hs.Extension (GhcTc)
import GHC.Types.Var (Var)
import GHC.Types.Basic (Activation, RuleName)
import {-# SOURCE #-} Language.Haskell.Syntax.Expr
  -- This {-# SOURCE #-} import should be removable once
  -- 'Language.Haskell.Syntax.Bind' no longer depends on 'GHC.Tc.Types.Evidence'.

-- | A type for hints emitted by GHC.
-- A /hint/ suggests a possible way to deal with a particular warning or error.
data GhcHint
  =
    {-| An \"unknown\" hint. This type constructor allows arbitrary
    -- hints to be embedded. The typical use case would be GHC plugins
    -- willing to emit hints alongside their custom diagnostics.
    -}
    forall a. (Outputable a, Typeable a) => UnknownHint a
    {-| Suggests adding a particular language extension. GHC will do its best trying
        to guess when the user is using the syntax of a particular language extension
        without having the relevant extension enabled.

        Example: If the user uses the keyword \"mdo\" (and we are in a monadic block), but
        the relevant extension is not enabled, GHC will emit a 'SuggestExtension RecursiveDo'.

        Test case(s): parser/should_fail/T12429, parser/should_fail/T8501c,
                      parser/should_fail/T18251e, ... (and many more)

    -}
  | SuggestExtension !LangExt.Extension
    {-| Suggests that a monadic code block is probably missing a \"do\" keyword.

        Example:
            main =
              putStrLn "hello"
              putStrLn "world"

        Test case(s): parser/should_fail/T8501a, parser/should_fail/readFail007,
                      parser/should_fail/InfixAppPatErr, parser/should_fail/T984
    -}
  | SuggestMissingDo
    {-| Suggests that a \"let\" expression is needed in a \"do\" block.

       Test cases: None (that explicitly test this particular hint is emitted).
    -}
  | SuggestLetInDo
    {-| Suggests to add an \".hsig\" signature file to the Cabal manifest.

      Triggered by: 'GHC.Driver.Errors.Types.DriverUnexpectedSignature', if Cabal
                    is being used.

      Example: See comment of 'DriverUnexpectedSignature'.

      Test case(s): driver/T12955

    -}
  | SuggestAddSignatureCabalFile !ModuleName
    {-| Suggests to explicitly list the instantiations for the signatures in
        the GHC invocation command.

      Triggered by: 'GHC.Driver.Errors.Types.DriverUnexpectedSignature', if Cabal
                    is /not/ being used.

      Example: See comment of 'DriverUnexpectedSignature'.

      Test case(s): driver/T12955
    -}
  | SuggestSignatureInstantiations !ModuleName [InstantiationSuggestion]
    {-| Suggests to use spaces instead of tabs.

        Triggered by: 'GHC.Parser.Errors.Types.PsWarnTab'.

        Examples: None
        Test Case(s): None
    -}
  | SuggestUseSpaces
    {-| Suggests wrapping an expression in parentheses

        Examples: None
        Test Case(s): None
    -}
  | SuggestParentheses
    {-| Suggests to increase the -fmax-pmcheck-models limit for the pattern match checker.

      Triggered by: 'GHC.HsToCore.Errors.Types.DsMaxPmCheckModelsReached'

      Test case(s): pmcheck/should_compile/TooManyDeltas
                    pmcheck/should_compile/TooManyDeltas
                    pmcheck/should_compile/T11822
    -}
  | SuggestIncreaseMaxPmCheckModels
    {-| Suggests adding a type signature, typically to resolve ambiguity or help GHC inferring types.

    -}
  | SuggestAddTypeSignature
    {-| Suggests to explicitly discard the result of a monadic action by binding the result to
        the '_' wilcard.

        Example:
           main = do
             _ <- getCurrentTime

    -}
  | SuggestBindToWildcard !(LHsExpr GhcTc)

  | SuggestAddInlineOrNoInlinePragma !Var !Activation

  | SuggestAddPhaseToCompetingRule !RuleName

    {-| Suggests increasing the limit for the number of iterations in the simplifier.

    -}
  | SuggestIncreaseSimplifierIterations

-- | An 'InstantiationSuggestion' for a '.hsig' file. This is generated
-- by GHC in case of a 'DriverUnexpectedSignature' and suggests a way
-- to instantiate a particular signature, where the first argument is
-- the signature name and the second is the module where the signature
-- was defined.
-- Example:
--
-- src/MyStr.hsig:2:11: error:
--     Unexpected signature: ‘MyStr’
--     (Try passing -instantiated-with="MyStr=<MyStr>"
--      replacing <MyStr> as necessary.)
data InstantiationSuggestion = InstantiationSuggestion !ModuleName !Module
