{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module GHCi.UI.Exception(printGhciException, GHCiMessage(..)) where

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

import GHC.Types.Error
import GHC.Types.SourceError

import GHC.Unit.State

import GHC.Utils.Logger
import GHC.Utils.Outputable

import Control.Monad.IO.Class


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
