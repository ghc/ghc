{-# LANGUAGE ScopedTypeVariables #-}
module GHC.Driver.Errors (
    reportError
  , reportDiagnostic
  , printMessages
  , printOrThrowDiagnostics
  , mkDriverPsHeaderMessage
  ) where

import GHC.Driver.Errors.Types
import GHC.Prelude
import GHC.Types.SrcLoc
import GHC.Types.SourceError
import GHC.Types.Error
import GHC.Utils.Json
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Logger

reportError :: Logger -> NamePprCtx -> DiagOpts -> SrcSpan -> SDoc -> IO ()
reportError logger nameContext opts span doc = do
  let
    message :: MsgEnvelope DiagnosticMessage
    message = mkErrorMsgEnvelope span nameContext DiagnosticMessage {
        diagMessage = mkDecorated [doc]
      , diagReason = ErrorWithoutFlag
      , diagHints = []
      }
  printMessage logger NoDiagnosticOpts opts message

reportDiagnostic :: Logger -> NamePprCtx -> DiagOpts -> SrcSpan -> DiagnosticReason -> SDoc -> IO ()
reportDiagnostic logger nameContext opts span reason doc = do
  let
    message :: MsgEnvelope DiagnosticMessage
    message = mkMsgEnvelope opts span nameContext DiagnosticMessage {
        diagMessage = mkDecorated [doc]
      , diagReason = reason
      , diagHints = []
      }
  printMessage logger NoDiagnosticOpts opts message

printMessages :: forall a. (Diagnostic a) => Logger -> DiagnosticOpts a -> DiagOpts -> Messages a -> IO ()
printMessages logger msg_opts opts = mapM_ (printMessage logger msg_opts opts) . sortMessages
  where
    sortMessages :: Messages a -> [MsgEnvelope a]
    sortMessages = sortMsgBag (Just opts) . getMessages

printMessage :: forall a. (Diagnostic a) => Logger -> DiagnosticOpts a -> DiagOpts -> MsgEnvelope a -> IO ()
printMessage logger msg_opts opts message = do
  decorated <- decorateDiagnostic logflags messageClass location doc
  if log_diags_as_json then do
    let
      rendered :: String
      rendered = renderWithContext (log_default_user_context logflags) decorated

      jsonMessage :: JsonDoc
      jsonMessage = jsonDiagnostic rendered message

    logJsonMsg logger messageClass jsonMessage
  else do
    logMsg logger (Message messageClass decorated)
  where
    logflags :: LogFlags
    logflags = logFlags logger

    doc :: SDoc
    doc = updSDocContext (\_ -> ctx) (messageWithHints diagnostic)

    messageClass :: MessageClass
    messageClass = UnsafeMCDiagnostic location severity (errMsgReason message) (diagnosticCode diagnostic)

    style :: PprStyle
    style = mkErrStyle (errMsgContext message)

    location :: SrcSpan
    location = errMsgSpan message

    ctx :: SDocContext
    ctx = (diag_ppr_ctx opts) { sdocStyle = style }

    diagnostic :: a
    diagnostic = errMsgDiagnostic message

    severity :: Severity
    severity = errMsgSeverity message

    messageWithHints :: a -> SDoc
    messageWithHints e =
      let main_msg = formatBulleted $ diagnosticMessage msg_opts e
          in case diagnosticHints e of
               []  -> main_msg
               [h] -> main_msg $$ hang (text "Suggested fix:") 2 (ppr h)
               hs  -> main_msg $$ hang (text "Suggested fixes:") 2
                                       (formatBulleted  $ mkDecorated . map ppr $ hs)

    log_diags_as_json :: Bool
    log_diags_as_json = log_diagnostics_as_json (logFlags logger)

decorateDiagnostic :: LogFlags -> MessageClass -> SrcSpan -> SDoc -> IO SDoc
decorateDiagnostic logflags msg_class srcSpan msg = addCaret
  where
    -- Pretty print the warning flag, if any (#10752)
    message :: SDoc
    message = mkLocMessageWarningGroups (log_show_warn_groups logflags) msg_class srcSpan msg

    addCaret :: IO SDoc
    addCaret = do
      caretDiagnostic <-
          if log_show_caret logflags
          then getCaretDiagnostic msg_class srcSpan
          else pure empty
      return $ getPprStyle $ \style ->
        withPprStyle (setStyleColoured True style)
          (message $+$ caretDiagnostic $+$ blankLine)

-- | Given a bag of diagnostics, turn them into an exception if
-- any has 'SevError', or print them out otherwise.
printOrThrowDiagnostics :: Logger -> GhcMessageOpts -> DiagOpts -> Messages GhcMessage -> IO ()
printOrThrowDiagnostics logger print_config opts msgs
  | errorsOrFatalWarningsFound msgs
  = throwErrors msgs
  | otherwise
  = printMessages logger print_config opts msgs

-- | Convert a 'PsError' into a wrapped 'DriverMessage'; use it
-- for dealing with parse errors when the driver is doing dependency analysis.
-- Defined here to avoid module loops between GHC.Driver.Error.Types and
-- GHC.Driver.Error.Ppr
mkDriverPsHeaderMessage :: MsgEnvelope PsMessage -> MsgEnvelope DriverMessage
mkDriverPsHeaderMessage = fmap DriverPsHeaderMessage
