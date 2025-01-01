{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module GHCi.UI.Exception
  ( GhciMessage(..)
  , GhciMessageOpts(..)
  , fromGhcOpts
  , toGhcHint
  , GhciCommandMessage(..)
  , GhciMacroError(..)
  , GhciModuleError(..)
  , InvalidMacroStart(..)
  , GHCi
  ) where

import GHC.Prelude

import GHC.Driver.Errors.Types

import GHC.Iface.Errors.Ppr
import GHC.Iface.Errors.Types

import qualified GHC.LanguageExtensions as LangExt

import GHC.Tc.Errors.Ppr
import GHC.Tc.Errors.Types

import GHC.Types.Error.Codes
import GHC.TypeLits

import GHC.Unit.State

import GHC.Utils.Outputable

import GHC.Generics
import GHC.Types.Error
import GHC.Types
import qualified GHC

import Data.List.NonEmpty (NonEmpty(..))

-- | The Options passed to 'diagnosticMessage'
-- in the 'Diagnostic' instance of 'GhciMessage'.
data GhciMessageOpts = GhciMessageOpts
  { ghcMessageOpts         :: DiagnosticOpts GhcMessage
  , ghciCommandMessageOpts :: DiagnosticOpts GhciCommandMessage
  }

-- | A 'GhciMessage' may either be a message that GHC emitted ('GhciGhcMessage'),
-- a message that is specific to GHCi ('GhciCommandMessage'),
-- or an external message ('GhciUnknownMessage') that can be used by external tools.
data GhciMessage where
  GhciCommandMessage :: GhciCommandMessage               -> GhciMessage
  GhciGhcMessage     :: GhcMessage                       -> GhciMessage
  GhciUnknownMessage :: UnknownDiagnosticFor GhciMessage -> GhciMessage

-- | A 'GhciHint' may either be a hint that GHC emitted ('GhciGhcHint')
-- or one that is specific to GHCi ('GhciCommandHint').
data GhciHint where
  GhciCommandHint :: GhciCommandHint -> GhciHint
  GhciGhcHint     :: GhcHint         -> GhciHint

-- | If this is a contained 'GhcHint' simply unwrap it,
-- otherwise use 'GhcHint's 'UnknownHint' constructor.
toGhcHint :: GhciHint -> GhcHint
toGhcHint (GhciGhcHint h)     = h
toGhcHint (GhciCommandHint h) = UnknownHint h

instance Outputable GhciHint where
  ppr = \case
    GhciCommandHint hint -> ppr hint
    GhciGhcHint     hint -> ppr hint

instance HasDefaultDiagnosticOpts GhciMessageOpts where
  defaultOpts = GhciMessageOpts
    (defaultDiagnosticOpts @GhcMessage)
    (defaultDiagnosticOpts @GhciCommandMessage)

-- | Create default 'GhciMessageOpts' but embed the given 'GhcMessageOpts'
fromGhcOpts :: DiagnosticOpts GhcMessage -> DiagnosticOpts GhciMessage
fromGhcOpts ghc_opts = defaultOpts { ghcMessageOpts = ghc_opts }

instance Diagnostic GhciMessage where
  type DiagnosticOpts GhciMessage = GhciMessageOpts
  type DiagnosticHint GhciMessage = GhciHint

  diagnosticMessage opts = \case
    GhciGhcMessage     m -> ghciDiagnosticMessage (ghcMessageOpts opts) m
    GhciCommandMessage m -> diagnosticMessage (ghciCommandMessageOpts opts) m
    GhciUnknownMessage (UnknownDiagnostic f _ m)
      -> diagnosticMessage (f opts) m

  diagnosticReason = \case
    GhciGhcMessage     m -> diagnosticReason m
    GhciCommandMessage m -> diagnosticReason m
    GhciUnknownMessage m -> diagnosticReason m

  diagnosticHints = \case
    GhciGhcMessage     m -> map GhciGhcHint     (ghciDiagnosticHints m)
    GhciCommandMessage m -> map GhciCommandHint (diagnosticHints m)
    GhciUnknownMessage m -> diagnosticHints m

  diagnosticCode = \case
    GhciGhcMessage     m -> diagnosticCode m
    GhciCommandMessage m -> diagnosticCode m
    GhciUnknownMessage m -> diagnosticCode m


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

-- | Modifications to error messages which we want to display in GHCi
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
        CantFindErr us module_or_interface cfi ->
          Just (pprWithUnitState us $ cantFindErrorX pkg_hidden_hint may_show_locations module_or_interface cfi)
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


data InvalidMacroStart = Colon | ExclamationMark

instance Outputable InvalidMacroStart where
  ppr = \case
    Colon
      -> "a colon"
    ExclamationMark
      -> "an exclamation mark"

data GhciMacroError
  = GhciMacroAlreadyDefined String
  | GhciMacroInvalidStart String InvalidMacroStart
  | GhciMacroNotDefined String
  | GhciMacroOverwritesBuiltin String
  deriving Generic

instance Diagnostic GhciMacroError where
  type DiagnosticOpts GhciMacroError = NoDiagnosticOpts
  type DiagnosticHint GhciMacroError = GhciCommandHint

  diagnosticMessage NoDiagnosticOpts = mkSimpleDecorated . \case
    GhciMacroAlreadyDefined name
      -> macro name "is already defined"
    GhciMacroInvalidStart name invStart
      -> macro name ("cannot start with" <+> ppr invStart)
    GhciMacroNotDefined name
      -> macro name "is not defined"
    GhciMacroOverwritesBuiltin name
      -> macro name "overwrites builtin command"
    where macro name what = "Macro" <+> quotes (text name) <+> what

  diagnosticReason = \case
    GhciMacroAlreadyDefined{}
      -> ErrorWithoutFlag
    GhciMacroInvalidStart{}
      -> ErrorWithoutFlag
    GhciMacroNotDefined{}
      -> ErrorWithoutFlag
    GhciMacroOverwritesBuiltin{}
      -> ErrorWithoutFlag

  diagnosticHints = \case
    GhciMacroAlreadyDefined{}
      -> [SuggestOverwrite]
    GhciMacroInvalidStart{}
      -> []
    GhciMacroNotDefined{}
      -> []
    GhciMacroOverwritesBuiltin{}
      -> [SuggestOverwrite]

  diagnosticCode = constructorCode @GHCi

data GhciModuleError
  = GhciModuleNotFound String
  | GhciNoModuleNameGuess
  | GhciNoModuleInfoForCurrentFile
  | GhciNoLocationInfoForModule GHC.ModuleName
  | GhciNoResolvedModules
  | GhciNoModuleForName GHC.Name
  | GhciNoMatchingModuleExport
  deriving Generic

instance Diagnostic GhciModuleError where
  type DiagnosticOpts GhciModuleError = NoDiagnosticOpts
  type DiagnosticHint GhciModuleError = GhciCommandHint

  diagnosticMessage NoDiagnosticOpts = mkSimpleDecorated . \case
    GhciModuleNotFound modN
      -> "Module" <+> text modN <+> "not found"
    GhciNoModuleNameGuess
      -> "Couldn't guess that module name. Does it exist?"
    GhciNoModuleInfoForCurrentFile
      -> "No module info for current file! Try loading it?"
    GhciNoLocationInfoForModule name
      -> "Found a name, but no location information" <> dot <+> "The module is" <> colon <+> ppr name
    GhciNoResolvedModules
      -> "Couldn't resolve to any modules."
    GhciNoModuleForName name
      -> "No module for" <+> ppr name
    GhciNoMatchingModuleExport
      -> "No matching export in any local modules."

  diagnosticReason = \case
    GhciModuleNotFound{} ->
      ErrorWithoutFlag
    GhciNoModuleNameGuess{} ->
      ErrorWithoutFlag
    GhciNoModuleInfoForCurrentFile{} ->
      ErrorWithoutFlag
    GhciNoLocationInfoForModule{} ->
      ErrorWithoutFlag
    GhciNoResolvedModules{} ->
      ErrorWithoutFlag
    GhciNoModuleForName{} ->
      ErrorWithoutFlag
    GhciNoMatchingModuleExport{} ->
      ErrorWithoutFlag

  diagnosticHints = \case
    GhciModuleNotFound{} ->
      []
    GhciNoModuleNameGuess{} ->
      []
    GhciNoModuleInfoForCurrentFile{} ->
      []
    GhciNoLocationInfoForModule{} ->
      []
    GhciNoResolvedModules{} ->
      []
    GhciNoModuleForName{} ->
      []
    GhciNoMatchingModuleExport{} ->
      []

  diagnosticCode = constructorCode @GHCi

-- | A Diagnostic emitted by GHCi while executing a command
-- these errors are assigned codes analogous to GHCs errors.
--
-- See 'GhciDiagnosticCode'.
data GhciCommandMessage
  -- module name errors
  = GhciMacroError GhciMacroError
  | GhciModuleError GhciModuleError
  -- others
  | GhciArgumentParseError SDoc
  | GhciCommandNotSupportedInMultiMode
  | GhciInvalidArgumentString String
  | GhciFileNotFound String
  | GhciCommandSyntaxError String
  | GhciInvalidPromptString
  | GhciPromptCallError String
  | GhciUnknownCommand String
  | GhciNoLastCommandAvailable
  | GhciUnknownFlag
     { ghciUnknownFlag :: !String
     , ghciUnknownFlagSuggestions :: [String]
     }
  | GhciNoSetEditor
  deriving Generic

-- | Hints that may be given by a 'GhciCommandMessage'
data GhciCommandHint
  = SuggestHelp
  | UseSetEditor
  | SuggestOverwrite
  | DidYouMean (NonEmpty String)

instance Outputable GhciCommandHint where
  ppr = \case
    SuggestHelp
      -> use "?" <+> "for help"
    DidYouMean (sugg :| rest)
      | [] <- rest -> "did you mean" <+> text sugg
      | otherwise  -> "did you mean one of" <> colon <+> nest 2 (hsep (map text (sugg : rest)))
    UseSetEditor
      -> use "set editor"
    SuggestOverwrite
      -> use "def!" <+> "to overwrite"
    where use cmd = "Use" <+> quotes (colon <> cmd)

instance Diagnostic GhciCommandMessage where
  type DiagnosticOpts GhciCommandMessage = NoDiagnosticOpts
  type DiagnosticHint GhciCommandMessage = GhciCommandHint

  diagnosticMessage opts (GhciMacroError err)  = diagnosticMessage opts err
  diagnosticMessage opts (GhciModuleError err) = diagnosticMessage opts err
  diagnosticMessage NoDiagnosticOpts err = mkSimpleDecorated $ case err of
    -- this can be improved with better/generic argument parsing
    GhciArgumentParseError ape
      -> ape
    GhciCommandNotSupportedInMultiMode
      -> "Command is not supported (yet) in multi-mode"
    -- In the future it would be better for toArgs to have a proper error type
    -- which could be used here and also help with reporting OPTIONS_GHC pragma errors better.
    GhciInvalidArgumentString str
      -> text str
    GhciCommandSyntaxError cmd
      -> "Syntax" <> colon $+$ nest 2 (colon <> text cmd)
    GhciInvalidPromptString
      -> "Can't parse prompt string. Use Haskell syntax"
    GhciUnknownCommand cmd
      -> "Unknown command" <+> quotes (colon <> text cmd)
    GhciNoLastCommandAvailable
      -> "There is no last command to perform"
    GhciFileNotFound f
      -> "File" <+> text f <+> "not found"
    GhciUnknownFlag { ghciUnknownFlag = flag }
      -> "Unrecognised flag" <> colon <+> text flag
    GhciPromptCallError err'
      -> "Error while calling prompt function" <> colon $$ nest 2 (quotes $ text err')
    GhciNoSetEditor
      -> "Editor not set"

  -- this might change in the future when we add more GHCi diagnostics
  diagnosticReason = \case
    GhciMacroError err
      -> diagnosticReason err
    GhciModuleError err
      -> diagnosticReason err
    GhciArgumentParseError{}
      -> ErrorWithoutFlag
    GhciCommandNotSupportedInMultiMode{}
      -> ErrorWithoutFlag
    GhciInvalidArgumentString{}
      -> ErrorWithoutFlag
    GhciFileNotFound {}
      -> ErrorWithoutFlag
    GhciCommandSyntaxError{}
      -> ErrorWithoutFlag
    GhciInvalidPromptString{}
      -> ErrorWithoutFlag
    GhciPromptCallError{}
      -> ErrorWithoutFlag
    GhciUnknownCommand{}
      -> ErrorWithoutFlag
    GhciNoLastCommandAvailable{}
      -> ErrorWithoutFlag
    GhciUnknownFlag{}
      -> ErrorWithoutFlag
    GhciNoSetEditor{}
      -> ErrorWithoutFlag

  diagnosticHints = \case
    GhciMacroError err
      -> diagnosticHints err
    GhciModuleError err
      -> diagnosticHints err
    GhciArgumentParseError{}
      -> []
    GhciCommandNotSupportedInMultiMode{}
      -> []
    GhciInvalidArgumentString{}
      -> []
    GhciFileNotFound{}
      -> []
    GhciCommandSyntaxError{}
      -> []
    GhciInvalidPromptString{}
      -> []
    GhciPromptCallError{}
      -> []
    GhciUnknownCommand{}
      -> [SuggestHelp]
    GhciNoLastCommandAvailable{}
      -> [SuggestHelp]
    GhciUnknownFlag { ghciUnknownFlagSuggestions = suggs }
      -> case suggs of
           []   -> []
           x:xs -> [DidYouMean (x :| xs)]
    GhciNoSetEditor{}
      -> [UseSetEditor]

  diagnosticCode = \case
    GhciMacroError  err -> diagnosticCode err
    GhciModuleError err -> diagnosticCode err
    err -> constructorCode @GHCi err

-- | type index for the ghci diagnostic code namespace
data GHCi

instance DiagnosticCodeNameSpace GHCi where
  type instance NameSpaceTag      GHCi     = "GHCi"
  type instance DiagnosticCodeFor GHCi con = GhciDiagnosticCode con
  type instance ConRecursIntoFor  GHCi con = GhciConRecursInto con

type GhciDiagnosticCode :: Symbol -> Nat
type family GhciDiagnosticCode c = n | n -> c where
  GhciDiagnosticCode "GhciCommandNotSupportedInMultiMode" = 83514
  GhciDiagnosticCode "GhciInvalidArgumentString"          = 68894
  GhciDiagnosticCode "GhciCommandSyntaxError"             = 72682
  GhciDiagnosticCode "GhciInvalidPromptString"            = 50882
  GhciDiagnosticCode "GhciPromptCallError"                = 22747
  GhciDiagnosticCode "GhciUnknownCommand"                 = 54713
  GhciDiagnosticCode "GhciNoLastCommandAvailable"         = 29130
  GhciDiagnosticCode "GhciUnknownFlag"                    = 15670
  GhciDiagnosticCode "GhciNoSetEditor"                    = 34086
  GhciDiagnosticCode "GhciMacroInvalidStart"              = 64996
  GhciDiagnosticCode "GhciMacroAlreadyDefined"            = 93909
  GhciDiagnosticCode "GhciMacroNotDefined"                = 40561
  GhciDiagnosticCode "GhciMacroOverwritesBuiltin"         = 86201
  GhciDiagnosticCode "GhciFileNotFound"                   = 31901
  GhciDiagnosticCode "GhciModuleNotFound"                 = 23305
  GhciDiagnosticCode "GhciNoModuleNameGuess"              = 21939
  GhciDiagnosticCode "GhciNoModuleInfoForCurrentFile"     = 96587
  GhciDiagnosticCode "GhciNoLocationInfoForModule"        = 12769
  GhciDiagnosticCode "GhciNoResolvedModules"              = 54909
  GhciDiagnosticCode "GhciNoModuleForName"                = 21847
  GhciDiagnosticCode "GhciNoMatchingModuleExport"         = 59723
  GhciDiagnosticCode "GhciArgumentParseError"             = 35671

type GhciConRecursInto :: Symbol -> Maybe Type
type family GhciConRecursInto con where
  GhciConRecursInto "GhciMacroError"  = 'Just GhciMacroError
  GhciConRecursInto "GhciModuleError" = 'Just GhciModuleError
  GhciConRecursInto _                 = 'Nothing
