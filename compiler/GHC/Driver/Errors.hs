module GHC.Driver.Errors (
    warningsToMessages
  , printOrThrowWarnings
  , printBagOfErrors
  , isWarnMsgFatal
  , handleFlagWarnings
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

-- | Converts a list of 'WarningMessages' into a tuple where the second element contains only
-- error, i.e. warnings that are considered fatal by GHC based on the input 'DynFlags'.
warningsToMessages :: DynFlags -> WarningMessages -> (WarningMessages, ErrorMessages)
warningsToMessages dflags =
  partitionBagWith $ \warn ->
    case isWarnMsgFatal dflags warn of
      Nothing -> Left warn
      Just err_reason ->
        Right warn{ errMsgSeverity = SevError
                  , errMsgReason = ErrReason err_reason }

printBagOfErrors :: RenderableDiagnostic a => Logger -> DynFlags -> Bag (MsgEnvelope a) -> IO ()
printBagOfErrors logger dflags bag_of_errors
  = sequence_ [ let style = mkErrStyle unqual
                    ctx   = initSDocContext dflags style
                in putLogMsg logger dflags reason sev s $
                   withPprStyle style (formatBulleted ctx (renderDiagnostic doc))
              | MsgEnvelope { errMsgSpan      = s,
                              errMsgDiagnostic = doc,
                              errMsgSeverity  = sev,
                              errMsgReason    = reason,
                              errMsgContext   = unqual } <- sortMsgBag (Just dflags)
                                                                       bag_of_errors ]

handleFlagWarnings :: Logger -> DynFlags -> [CmdLine.Warn] -> IO ()
handleFlagWarnings logger dflags warns = do
  let warns' = filter (shouldPrintWarning dflags . CmdLine.warnReason)  warns

      -- It would be nicer if warns :: [Located SDoc], but that
      -- has circular import problems.
      bag = listToBag [ mkPlainWarnMsg loc (text warn)
                      | CmdLine.Warn _ (L loc warn) <- warns' ]

  printOrThrowWarnings logger dflags bag

-- | Checks if given 'WarnMsg' is a fatal warning.
isWarnMsgFatal :: DynFlags -> WarnMsg -> Maybe (Maybe WarningFlag)
isWarnMsgFatal dflags MsgEnvelope{errMsgReason = Reason wflag}
  = if wopt_fatal wflag dflags
      then Just (Just wflag)
      else Nothing
isWarnMsgFatal dflags _
  = if gopt Opt_WarnIsError dflags
      then Just Nothing
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
                (True, warn{ errMsgSeverity = SevError
                           , errMsgReason = ErrReason err_reason
                           }))
          False warns
  if make_error
    then throwIO (mkSrcErr warns')
    else printBagOfErrors logger dflags warns
