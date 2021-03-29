module GHC.Driver.Errors (
    printOrThrowDiagnostics
  , printBagOfErrors
  , handleFlagWarnings
  , partitionMessageBag
  ) where

import GHC.Driver.Session
import GHC.Data.Bag
import GHC.Utils.Exception
import GHC.Utils.Error ( formatBulleted, sortMsgBag, mkPlainMsgEnvelope )
import GHC.Types.SourceError ( mkSrcErr )
import GHC.Prelude
import GHC.Types.SrcLoc
import GHC.Types.Error
import GHC.Utils.Outputable ( text, withPprStyle, mkErrStyle )
import GHC.Utils.Logger
import qualified GHC.Driver.CmdLine as CmdLine

-- | Partitions the messages and returns a tuple which first element are the warnings, and the
-- second the errors.
partitionMessageBag :: Diagnostic e => Bag (MsgEnvelope e) -> (Bag (MsgEnvelope e), Bag (MsgEnvelope e))
partitionMessageBag = partitionBag isWarningMessage

printBagOfErrors :: Diagnostic a => Logger -> DynFlags -> Bag (MsgEnvelope a) -> IO ()
printBagOfErrors logger dflags bag_of_errors
  = sequence_ [ let style = mkErrStyle unqual
                    ctx   = initSDocContext dflags style
                in putLogMsg logger dflags (MCDiagnostic sev . diagnosticReason $ dia) s $
                   withPprStyle style (formatBulleted ctx (diagnosticMessage dia))
              | MsgEnvelope { errMsgSpan      = s,
                              errMsgDiagnostic = dia,
                              errMsgSeverity = sev,
                              errMsgContext   = unqual } <- sortMsgBag (Just dflags)
                                                                       bag_of_errors ]

handleFlagWarnings :: Logger -> DynFlags -> [CmdLine.Warn] -> IO ()
handleFlagWarnings logger dflags warns = do
  let warns' = filter (should_print_warning dflags . CmdLine.warnReason)  warns
      -- It would be nicer if warns :: [Located SDoc], but that
      -- has circular import problems.
      bag = listToBag [ mkPlainMsgEnvelope dflags WarningWithoutFlag loc (text warn)
                      | CmdLine.Warn _ (L loc warn) <- warns' ]

  printOrThrowDiagnostics logger dflags bag
  where
    -- Given a warn reason, check to see if it's associated -W opt is enabled
    should_print_warning :: DynFlags -> DiagnosticReason -> Bool
    should_print_warning dflags (WarningWithFlag Opt_WarnDeprecatedFlags)
      = wopt Opt_WarnDeprecatedFlags dflags
    should_print_warning dflags (WarningWithFlag Opt_WarnUnrecognisedWarningFlags)
      = wopt Opt_WarnUnrecognisedWarningFlags dflags
    should_print_warning _ _
      = True

-- | Given a bag of diagnostics, turn them into an exception if
-- any has 'SevError', or print them out otherwise.
printOrThrowDiagnostics :: Logger -> DynFlags -> Bag WarnMsg -> IO ()
printOrThrowDiagnostics logger dflags warns
  | any ((==) SevError . errMsgSeverity) warns
  = throwIO (mkSrcErr warns)
  | otherwise
  = printBagOfErrors logger dflags warns
