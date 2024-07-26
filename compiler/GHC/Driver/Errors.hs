{-# LANGUAGE ScopedTypeVariables #-}
module GHC.Driver.Errors (
    printOrThrowDiagnostics
  , printMessages
  , mkDriverPsHeaderMessage
  ) where

import GHC.Driver.Errors.Types
import GHC.Prelude
import GHC.Types.SourceError
import GHC.Types.Error
import GHC.Utils.Error
import GHC.Utils.Outputable (hang, ppr, ($$),  text, mkErrStyle, sdocStyle, updSDocContext )
import GHC.Utils.Logger

printMessages :: forall a. (Diagnostic a) => Logger -> DiagnosticOpts a -> DiagOpts -> Messages a -> IO ()
printMessages logger msg_opts opts msgs
  = sequence_ [ let style = mkErrStyle name_ppr_ctx
                    ctx   = (diag_ppr_ctx opts) { sdocStyle = style }
                in (if log_diags_as_json
                    then logJsonMsg logger (MCDiagnostic sev reason (diagnosticCode dia)) msg
                    else logMsg logger (MCDiagnostic sev reason (diagnosticCode dia)) s $
                  updSDocContext (\_ -> ctx) (messageWithHints dia))
              | msg@MsgEnvelope { errMsgSpan       = s,
                                  errMsgDiagnostic = dia,
                                  errMsgSeverity   = sev,
                                  errMsgReason     = reason,
                                  errMsgContext    = name_ppr_ctx }
                  <- sortMsgBag (Just opts) (getMessages msgs) ]
  where
    messageWithHints :: a -> SDoc
    messageWithHints e =
      let main_msg = formatBulleted $ diagnosticMessage msg_opts e
          in case diagnosticHints e of
               []  -> main_msg
               [h] -> main_msg $$ hang (text "Suggested fix:") 2 (ppr h)
               hs  -> main_msg $$ hang (text "Suggested fixes:") 2
                                       (formatBulleted  $ mkDecorated . map ppr $ hs)
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
