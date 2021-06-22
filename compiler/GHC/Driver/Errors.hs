module GHC.Driver.Errors (
    printOrThrowDiagnostics
  , printMessages
  , handleFlagWarnings
  , mkDriverPsHeaderMessage
  ) where

import GHC.Driver.Errors.Types
import GHC.Data.Bag
import GHC.Prelude
import GHC.Types.SrcLoc
import GHC.Types.SourceError
import GHC.Types.Error
import GHC.Utils.Error
import GHC.Utils.Outputable (hang, ppr, ($$), SDocContext,  text, withPprStyle, mkErrStyle, sdocStyle )
import GHC.Utils.Logger
import qualified GHC.Driver.CmdLine as CmdLine

printMessages :: Diagnostic a => Logger -> DiagOpts -> Messages a -> IO ()
printMessages logger opts msgs
  = sequence_ [ let style = mkErrStyle unqual
                    ctx   = (diag_ppr_ctx opts) { sdocStyle = style }
                in logMsg logger (MCDiagnostic sev . diagnosticReason $ dia) s $
                   withPprStyle style (messageWithHints ctx dia)
              | MsgEnvelope { errMsgSpan      = s,
                              errMsgDiagnostic = dia,
                              errMsgSeverity = sev,
                              errMsgContext   = unqual } <- sortMsgBag (Just opts)
                                                                       (getMessages msgs) ]
  where
    messageWithHints :: Diagnostic a => SDocContext -> a -> SDoc
    messageWithHints ctx e =
      let main_msg = formatBulleted ctx $ diagnosticMessage e
          in case diagnosticHints e of
               []  -> main_msg
               [h] -> main_msg $$ hang (text "Suggested fix:") 2 (ppr h)
               hs  -> main_msg $$ hang (text "Suggested fixes:") 2
                                       (formatBulleted ctx . mkDecorated . map ppr $ hs)

handleFlagWarnings :: Logger -> DiagOpts -> [CmdLine.Warn] -> IO ()
handleFlagWarnings logger opts warns = do
  let -- It would be nicer if warns :: [Located SDoc], but that
      -- has circular import problems.
      bag = listToBag [ mkPlainMsgEnvelope opts loc $
                        GhcDriverMessage $
                        DriverUnknownMessage $
                        mkPlainDiagnostic reason noHints $ text warn
                      | CmdLine.Warn reason (L loc warn) <- warns ]

  printOrThrowDiagnostics logger opts (mkMessages bag)

-- | Given a bag of diagnostics, turn them into an exception if
-- any has 'SevError', or print them out otherwise.
printOrThrowDiagnostics :: Logger -> DiagOpts -> Messages GhcMessage -> IO ()
printOrThrowDiagnostics logger opts msgs
  | errorsOrFatalWarningsFound msgs
  = throwErrors msgs
  | otherwise
  = printMessages logger opts msgs

-- | Convert a 'PsError' into a wrapped 'DriverMessage'; use it
-- for dealing with parse errors when the driver is doing dependency analysis.
-- Defined here to avoid module loops between GHC.Driver.Error.Types and
-- GHC.Driver.Error.Ppr
mkDriverPsHeaderMessage :: MsgEnvelope PsMessage -> MsgEnvelope DriverMessage
mkDriverPsHeaderMessage = fmap DriverPsHeaderMessage
