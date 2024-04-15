{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module GHCi.UI.Exception
    ( printGhciException
    , GHCiMessage(..)
    , GhciCommandError(..)
    , GhciArgumentParseError(..)
    , GhciInput
    , reportError) where

import GHC.Prelude

import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Session

import GHC.Iface.Errors.Ppr
import GHC.Iface.Errors.Types

import qualified GHC.LanguageExtensions as LangExt

import GHC.Tc.Errors.Ppr
import GHC.Tc.Errors.Types

import GHC.Types.SourceError
import GHC.Types.Error
import GHC.Types
import GHC

import GHC.Unit.State

import GHC.Utils.Logger
import GHC.Utils.Outputable

import Control.Monad.IO.Class
import GHC.Generics

import Control.Monad.Trans.Except (ExceptT (..), throwE)
import System.Console.Haskeline (InputT)
import Control.Monad.Trans.Class

-- JADE_TODO
newtype GhciInput m a = GhciInput
  { getGhciInput :: ExceptT GhciCommandError (InputT m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans GhciInput where
  lift = lift -- JADE_TODO

reportError :: Monad m => GhciCommandError -> GhciInput m a
reportError = GhciInput . throwE


-- | Print the all diagnostics in a 'SourceError'.  Specialised for GHCi error reporting
-- for some error messages.
printGhciException :: (HasLogger m, MonadIO m, HasDynFlags m) => SourceError -> m ()
printGhciException err = do
  dflags <- getDynFlags
  logger <- getLogger
  let !diag_opts = initDiagOpts dflags
      !print_config = initPrintConfig dflags
  liftIO $ printMessages logger print_config diag_opts (GHCiMessage <$> (srcErrorMessages err))


newtype GHCiMessage = GHCiMessage { _getGhciMessage :: GhcMessage }

instance Diagnostic GHCiMessage where
  type DiagnosticOpts GHCiMessage = DiagnosticOpts GhcMessage

  diagnosticMessage opts (GHCiMessage msg) = ghciDiagnosticMessage opts msg

  diagnosticReason (GHCiMessage msg) = diagnosticReason msg

  diagnosticHints (GHCiMessage msg) = ghciDiagnosticHints msg

  diagnosticCode (GHCiMessage msg)  = diagnosticCode msg


-- | Modifications to hint messages which we want to display in GHCi.
ghciDiagnosticHints :: GhcMessage -> [GhcHint]
ghciDiagnosticHints msg = map modifyHintForGHCi (diagnosticHints msg)
  where
    modifyHintForGHCi :: GhcHint -> GhcHint
    modifyHintForGHCi = \case
      SuggestExtension extHint -> SuggestExtension $ modifyExtHintForGHCi extHint
      hint -> hint
    modifyExtHintForGHCi :: LanguageExtensionHint -> LanguageExtensionHint
    modifyExtHintForGHCi = \case
      SuggestSingleExtension    doc ext  -> SuggestSingleExtension    (suggestSetExt [ext] doc False) ext
      SuggestExtensionInOrderTo doc ext  -> SuggestExtensionInOrderTo (suggestSetExt [ext] doc False) ext
      SuggestAnyExtension       doc exts -> SuggestAnyExtension       (suggestSetExt exts  doc True ) exts
      SuggestExtensions         doc exts -> SuggestExtensions         (suggestSetExt exts  doc False) exts
    -- Suggest enabling extension with :set -X<ext>
    -- SuggestAnyExtension will be on multiple lines so the user can select which to enable without editing
    suggestSetExt :: [LangExt.Extension] -> SDoc -> Bool -> SDoc
    suggestSetExt exts doc enable_any = doc $$ hang header 2 exts_cmds
      where
        header = text "You may enable" <+> which <+> text "language extension" <> plural exts <+> text "in GHCi with:"
        which
          | [ _ext ] <- exts
          = text "this"
          | otherwise
          = if enable_any
            then text "these"
            else text "all of these"
        exts_cmds
          | enable_any
          = vcat $ map (\ext -> text ":set -X" <> ppr ext) exts
          | otherwise
          = text ":set" <> hcat (map (\ext -> text " -X" <> ppr ext) exts)

-- Modifications to error messages which we want to display in GHCi
ghciDiagnosticMessage :: GhcMessageOpts -> GhcMessage -> DecoratedSDoc
ghciDiagnosticMessage ghc_opts msg =
  case msg of
    GhcTcRnMessage tc_msg ->
      case tcRnMessage (tcMessageOpts ghc_opts) tc_msg of
        Nothing -> diagnosticMessage ghc_opts msg
        Just sdoc -> sdoc
    GhcDriverMessage  (DriverInterfaceError err) ->
      case ghciInterfaceError err of
        Just sdoc -> mkSimpleDecorated sdoc
        Nothing -> diagnosticMessage ghc_opts msg
    GhcDriverMessage {} -> diagnosticMessage ghc_opts msg
    GhcPsMessage  {} -> diagnosticMessage ghc_opts msg
    GhcDsMessage  {} -> diagnosticMessage ghc_opts msg
    GhcUnknownMessage  {} -> diagnosticMessage ghc_opts msg
  where
    tcRnMessage tc_opts tc_msg =
      case tc_msg of
        TcRnInterfaceError err -> mkSimpleDecorated <$> (ghciInterfaceError err)
        TcRnMessageWithInfo unit_state msg_with_info ->
          case msg_with_info of
           TcRnMessageDetailed err_info wrapped_msg
             -> messageWithInfoDiagnosticMessage unit_state err_info
                  (tcOptsShowContext tc_opts)
                  <$> tcRnMessage tc_opts wrapped_msg
        TcRnWithHsDocContext ctxt wrapped_msg ->
          messageWithHsDocContext tc_opts ctxt <$> tcRnMessage tc_opts wrapped_msg
        _ -> Nothing

    opts = tcOptsIfaceOpts (tcMessageOpts ghc_opts)

    ghciInterfaceError (Can'tFindInterface err looking_for) =
      hangNotEmpty (lookingForHerald looking_for) 2 <$> ghciMissingInterfaceErrorDiagnostic err
    ghciInterfaceError _ = Nothing

    ghciMissingInterfaceErrorDiagnostic reason =
      case reason of
        CantFindErr us module_or_interface cfi -> Just (pprWithUnitState us $ cantFindErrorX pkg_hidden_hint may_show_locations module_or_interface cfi)
        _ -> Nothing
      where

        may_show_locations = mayShowLocations ":set -v" (ifaceShowTriedFiles opts)

        pkg_hidden_hint = pkgHiddenHint hidden_msg (ifaceBuildingCabalPackage opts)
          where
            hidden_msg pkg =
              text "You can run" <+>
              quotes (text ":set -package " <> ppr (unitPackageName pkg)) <+>
              text "to expose it." $$
              text "(Note: this unloads all the modules in the current scope.)"

data GhciArgumentParseError
  = SpanPrematureEnd
  | SpanNoReadAs String String
  | SpanExpectedWS String

instance Outputable GhciArgumentParseError where
  ppr = \case
    SpanPrematureEnd
      -> "Premature end of string while expecting Int"
    SpanNoReadAs actual expected
      -> "Couldn't read" <+> text actual <+> "as" <+> text expected
    SpanExpectedWS str
      -> "Expected whitespace in" <+> text str

data GhciCommandError
  -- macro errors
  = GhciMacroAlreadyDefined String
  | GhciMacroInvalidStart String
  | GhciMacroNotDefined
  | GhciMacroOverwritesBuiltin String
  -- module name errors
  | GhciModuleNotFound String
  | GhciNoModuleNameGuess
  | GhciNoModuleInfoForCurrentFile
  | GhciNoLocationInfoForModule ModuleName
  | GhciNoResolvedModules
  | GhciNoModuleForName Name
  | GhciNoMatchingModuleExport
  -- argument parse error
  | GhciArgumentParseError GhciArgumentParseError
  -- Basic errors
  | GhciCommandNotSupportedInMultiMode
  | GhciInvalidArgumentString String
  | GhciFileNotFound String
  | GhciCommandSyntaxError String [String]
  | GhciInvalidPromptString
  | GhciPromptCallError String
  | GhciUnknownCommand String String
  | GhciNoLastCommandAvailable String
  | GhciUnknownFlag String [String]
  | GhciNoSetEditor
  deriving Generic

instance Outputable GhciCommandError where
  ppr = \case
    GhciMacroAlreadyDefined name
      -> "Macro" <+> quotes (text name) <+> "is already defined" <.> macro_name_hint
    GhciMacroOverwritesBuiltin name
      -> "Macro" <+> quotes (text name) <+> "overwrites builtin command" <.> macro_name_hint
    GhciMacroInvalidStart str
      -> "Macro name cannot start with" <+> text str
    GhciModuleNotFound modN
      -> "Module" <+> text modN <+> "not found"
    GhciNoModuleNameGuess
      -> "Couldn't guess that module name. Does it exist?"
    GhciNoModuleInfoForCurrentFile
      -> "No module info for current file! Try loading it?"
    GhciNoLocationInfoForModule name
      -> "Found a name, but no location information" <.> "The module is" <:> ppr name
    GhciNoResolvedModules
      -> "Couldn't resolve to any modules."
    GhciNoModuleForName name
      -> "No module for" <+> ppr name
    GhciNoMatchingModuleExport
      -> "No matching export in any local modules."
    GhciArgumentParseError ape -> ppr ape
    GhciCommandNotSupportedInMultiMode
      -> "Command is not supported (yet) in multi-mode"
    GhciInvalidArgumentString str
      -> text str
    GhciCommandSyntaxError cmd args
      -> "Syntax" <> colon $+$
           nest 2 (colon <> text cmd <+> hsep (map (angleBrackets . text) args))
    GhciUnknownCommand cmd help
      -> "Unknown command" <+> quotes (colon <> text cmd) $+$
           text help -- TODO
    GhciNoLastCommandAvailable help
      -> "There is no last command to perform" $+$
           text help -- TODO
    GhciFileNotFound f
      -> "File" <+> text f <+> "not found"
    GhciUnknownFlag flag suggestions
      -> "Unrecognised flag" <:> text flag $$
           case suggestions of
             [] -> empty
             suggs -> "did you mean one of" <> colon $$ nest 2 (vcat (map text suggs))
    GhciPromptCallError err
      -> text err
    GhciNoSetEditor
      -> "editor not set, use :set editor"
    where
      macro_name_hint = "Use" <+> quotes (colon <> "def!") <+> "to overwrite" <> dot


(<:>) :: SDoc -> SDoc -> SDoc
l <:> r = l <> colon <+> r

(<.>) :: SDoc -> SDoc -> SDoc
l <.> r = l <> dot <+> r
