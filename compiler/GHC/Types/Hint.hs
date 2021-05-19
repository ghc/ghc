{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module GHC.Types.Hint where

import GHC.Prelude

import GHC.Utils.Outputable
import GHC.Types.Name.Reader
import GHC.LanguageExtensions
import Data.Typeable
import GHC.Unit.Module (ModuleName, Module)
import GHC.Hs.Extension (GhcTc)
import {-# SOURCE #-} Language.Haskell.Syntax.Expr

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
  | SuggestExtension !Extension
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
    -- FIXME(adn) This is not a hint but was migrated from the old \"PsHint\" type.
    -- It will be removed in a further refactoring as part of #18516.
  | SuggestInfixBindMaybeAtPat !RdrName
    -- FIXME(adn) This is not a hint but was migrated from the old \"PsHint\" type.
    -- It will be removed in a further refactoring as part of #18516.
  | TypeApplicationsInPatternsOnlyDataCons
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
  | Outputable (LHsExpr GhcTc) => SuggestBindToWildcard !(LHsExpr GhcTc)
  -- FIXME(adn) The constraint is not pretty, but it keeps circular imports at bay.

instance Outputable GhcHint where
  ppr = \case
    UnknownHint m
      -> ppr m
    SuggestExtension ext
      -> case ext of
          NegativeLiterals
            -> text "If you are trying to write a large negative literal, use NegativeLiterals"
          _ -> text "Perhaps you intended to use" <+> ppr ext
    SuggestMissingDo
      -> text "Possibly caused by a missing 'do'?"
    SuggestLetInDo
      -> text "Perhaps you need a 'let' in a 'do' block?"
           $$ text "e.g. 'let x = 5' instead of 'x = 5'"
    SuggestInfixBindMaybeAtPat fun
      -> text "In a function binding for the"
              <+> quotes (ppr fun)
              <+> text "operator."
           $$ if opIsAt fun
                 then perhapsAsPat
                 else empty
    TypeApplicationsInPatternsOnlyDataCons
      -> text "Type applications in patterns are only allowed on data constructors."
    SuggestAddSignatureCabalFile pi_mod_name
      -> text "Try adding" <+> quotes (ppr pi_mod_name)
           <+> text "to the"
           <+> quotes (text "signatures")
           <+> text "field in your Cabal file."
    SuggestSignatureInstantiations pi_mod_name suggestions
      -> let suggested_instantiated_with =
               hcat (punctuate comma $
                   [ ppr k <> text "=" <> ppr v
                   | InstantiationSuggestion k v <- suggestions
                   ])
         in text "Try passing -instantiated-with=\"" <>
              suggested_instantiated_with <> text "\"" $$
                text "replacing <" <> ppr pi_mod_name <> text "> as necessary."
    SuggestIncreaseMaxPmCheckModels
      -> text "Increase the limit or resolve the warnings to suppress this message."
    SuggestAddTypeSignature
      -> text "Add a type signature."
    SuggestBindToWildcard rhs
      -> hang (text "Suppress this warning by saying") 2 (quotes $ text "_ <-" <+> ppr rhs)

perhapsAsPat :: SDoc
perhapsAsPat = text "Perhaps you meant an as-pattern, which must not be surrounded by whitespace"


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
