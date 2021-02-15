{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Driver.Errors (
    printOrThrowWarnings
  , printBagOfErrors
  , handleFlagWarnings
  , partitionMessageBag
  ) where

import GHC.Driver.Session
import GHC.Data.Bag
import GHC.Utils.Exception
import GHC.Utils.Error ( formatBulleted, sortMsgBag )
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
                in putLogMsg logger dflags (MCDiagnostic . diagnosticReason $ dia) s $
                   withPprStyle style (formatBulleted ctx (diagnosticMessage dia))
              | MsgEnvelope { errMsgSpan      = s,
                              errMsgDiagnostic = dia,
                              errMsgContext   = unqual } <- sortMsgBag (Just dflags)
                                                                       bag_of_errors ]

handleFlagWarnings :: Logger -> DynFlags -> [CmdLine.Warn] -> IO ()
handleFlagWarnings logger dflags warns = do
  let warns' = filter (shouldPrintWarning dflags . CmdLine.warnReason)  warns

      -- It would be nicer if warns :: [Located SDoc], but that
      -- has circular import problems.
      bag = listToBag [ mkPlainMsgEnvelope WarnReason loc (text warn)
                      | CmdLine.Warn _ (L loc warn) <- warns' ]

  printOrThrowWarnings logger dflags bag

-- Given a warn reason, check to see if it's associated -W opt is enabled
shouldPrintWarning :: DynFlags -> CmdLine.WarnReason -> Bool
shouldPrintWarning dflags CmdLine.ReasonDeprecatedFlag
  = wopt Opt_WarnDeprecatedFlags dflags
shouldPrintWarning dflags CmdLine.ReasonUnrecognisedFlag
  = wopt Opt_WarnUnrecognisedWarningFlags dflags
shouldPrintWarning _ _
  = True

-- | Given a bag of warnings, turn them into an exception if
-- -Werror is enabled, or print them out otherwise.
printOrThrowWarnings :: Logger -> DynFlags -> Bag WarnMsg -> IO ()
printOrThrowWarnings logger dflags warns = do
  let (make_error, warns') =
        mapAccumBagL
          (\make_err warn ->
            case is_warn_msg_fatal dflags warn of
              Nothing ->
                (make_err, warn)
              Just err_reason ->
                (True, promote_warning_to_error err_reason warn))
          False warns
  if make_error
    then throwIO (mkSrcErr warns')
    else printBagOfErrors logger dflags warns

  where

    -- | Checks if given 'WarnMsg' is a fatal warning.
    is_warn_msg_fatal :: DynFlags -> WarnMsg -> Maybe DiagnosticReason
    is_warn_msg_fatal dflags (diagnosticReason . errMsgDiagnostic -> reason) =
      case reason of
        ErrReason                           -> Nothing -- nothing to do, this is already an error.
        ErrReasonPromotedWithWError         -> Nothing -- same as above.
        ErrReasonPromotedFromWarning _wflag -> Nothing -- same as above.
        WarnReason ->
          if gopt Opt_WarnIsError dflags
            then Just ErrReasonPromotedWithWError
            else Nothing
        WarnReasonWithFlag wflag ->
          if wopt_fatal wflag dflags
            then Just $ ErrReasonPromotedFromWarning wflag
            else Nothing

    -- | Promotes a 'WarnMsg' into an error.
    promote_warning_to_error :: DiagnosticReason -> WarnMsg -> MsgEnvelope DecoratedMessage
    promote_warning_to_error errReason msg =
      let diag' = (errMsgDiagnostic msg) { diagReason = errReason }
      in msg { errMsgDiagnostic = diag' }

