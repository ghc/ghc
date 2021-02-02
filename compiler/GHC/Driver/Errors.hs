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

-- | Promotes a 'WarnMsg' into an error, given a proper 'ErrReason'.
promoteWarningToError :: ErrReason -> WarnMsg -> MsgEnvelope DecoratedSDoc
promoteWarningToError errReason msg = msg { errMsgSeverity = SevError errReason }

-- | Partitions the messages and returns a tuple which first element are the warnings, and the
-- second the errors.
partitionMessageBag :: Bag (MsgEnvelope e) -> (Bag (MsgEnvelope e), Bag (MsgEnvelope e))
partitionMessageBag = partitionBag isWarningMessage

printBagOfErrors :: RenderableDiagnostic a => Logger -> DynFlags -> Bag (MsgEnvelope a) -> IO ()
printBagOfErrors logger dflags bag_of_errors
  = sequence_ [ let style = mkErrStyle unqual
                    ctx   = initSDocContext dflags style
                in putLogMsg logger dflags (MCDiagnostic sev) s $
                   withPprStyle style (formatBulleted ctx (renderDiagnostic doc))
              | MsgEnvelope { errMsgSpan      = s,
                              errMsgDiagnostic = doc,
                              errMsgSeverity  = sev,
                              errMsgContext   = unqual } <- sortMsgBag (Just dflags)
                                                                       bag_of_errors ]

handleFlagWarnings :: Logger -> DynFlags -> [CmdLine.Warn] -> IO ()
handleFlagWarnings logger dflags warns = do
  let warns' = filter (shouldPrintWarning dflags . CmdLine.warnReason)  warns

      -- It would be nicer if warns :: [Located SDoc], but that
      -- has circular import problems.
      bag = listToBag [ mkPlainMsgEnvelope sevWarnNoReason loc (text warn)
                      | CmdLine.Warn _ (L loc warn) <- warns' ]

  printOrThrowWarnings logger dflags bag

-- | Checks if given 'WarnMsg' is a fatal warning.
isWarnMsgFatal :: DynFlags -> WarnMsg -> Maybe ErrReason
isWarnMsgFatal dflags (errMsgSeverity -> severity) =
  case severity of
    SevError _              -> Nothing -- nothing to do, this is already an error.
    SevWarning NoWarnReason ->
      if gopt Opt_WarnIsError dflags
        then Just ErrPromotedWithWError
        else Nothing
    SevWarning (WarnReason wflag) ->
      if wopt_fatal wflag dflags
        then Just $ ErrPromotedFromWarning wflag
        else Nothing

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
            case isWarnMsgFatal dflags warn of
              Nothing ->
                (make_err, warn)
              Just err_reason ->
                (True, promoteWarningToError err_reason warn))
          False warns
  if make_error
    then throwIO (mkSrcErr warns')
    else printBagOfErrors logger dflags warns
