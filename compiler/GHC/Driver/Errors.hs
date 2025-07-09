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
printMessage logger msg_opts opts message
  | log_diags_as_json = logJsonMsg logger messageClass message
  | otherwise = logMsg logger messageClass location doc
  where
    doc :: SDoc
    doc = updSDocContext (\_ -> ctx) (messageWithHints diagnostic)

    messageClass :: MessageClass
    messageClass = MCDiagnostic severity (errMsgReason message) (diagnosticCode diagnostic)

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
