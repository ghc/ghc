module GHC.Driver.Errors (
    printOrThrowDiagnostics
  , printMessages
  , handleFlagWarnings
  , mkDriverPsHeaderMessage
  ) where

import GHC.Driver.Session
import GHC.Driver.Errors.Types
import GHC.Data.Bag
import GHC.Prelude
import GHC.Parser.Errors ( PsError(..) )
import GHC.Types.SrcLoc
import GHC.Types.SourceError
import GHC.Types.Error
import GHC.Utils.Error
import GHC.Utils.Outputable (hang, ppr, ($$), SDocContext,  text, withPprStyle, mkErrStyle )
import GHC.Utils.Logger
import qualified GHC.Driver.CmdLine as CmdLine

printMessages :: Diagnostic a => Logger -> DynFlags -> Messages a -> IO ()
printMessages logger dflags msgs
  = sequence_ [ let style = mkErrStyle unqual
                    ctx   = initSDocContext dflags style
                in putLogMsg logger dflags (MCDiagnostic sev . diagnosticReason $ dia) s $
                   withPprStyle style (messageWithHints ctx dia)
              | MsgEnvelope { errMsgSpan      = s,
                              errMsgDiagnostic = dia,
                              errMsgSeverity = sev,
                              errMsgContext   = unqual } <- sortMsgBag (Just dflags)
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

handleFlagWarnings :: Logger -> DynFlags -> [CmdLine.Warn] -> IO ()
handleFlagWarnings logger dflags warns = do
  let -- It would be nicer if warns :: [Located SDoc], but that
      -- has circular import problems.
      bag = listToBag [ mkPlainMsgEnvelope dflags loc $
                        GhcDriverMessage $
                        DriverUnknownMessage $
                        mkPlainDiagnostic reason noHints $ text warn
                      | CmdLine.Warn reason (L loc warn) <- warns ]

  printOrThrowDiagnostics logger dflags (mkMessages bag)

-- | Given a bag of diagnostics, turn them into an exception if
-- any has 'SevError', or print them out otherwise.
printOrThrowDiagnostics :: Logger -> DynFlags -> Messages GhcMessage -> IO ()
printOrThrowDiagnostics logger dflags msgs
  | errorsOrFatalWarningsFound msgs
  = throwErrors msgs
  | otherwise
  = printMessages logger dflags msgs

-- | Convert a 'PsError' into a wrapped 'DriverMessage'; use it
-- for dealing with parse errors when the driver is doing dependency analysis.
-- Defined here to avoid module loops between GHC.Driver.Error.Types and
-- GHC.Driver.Error.Ppr
mkDriverPsHeaderMessage :: PsError -> MsgEnvelope DriverMessage
mkDriverPsHeaderMessage ps_err
  = mkPlainErrorMsgEnvelope (errLoc ps_err) $
    DriverPsHeaderMessage (errDesc ps_err) (errHints ps_err)
