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
import GHC.Utils.Error ( formatErrDoc, sortMsgBag )
import GHC.Types.SourceError ( mkSrcErr )
import GHC.Prelude
import GHC.Types.SrcLoc
import GHC.Types.Error
import GHC.Utils.Outputable ( text, withPprStyle, mkErrStyle )
import qualified GHC.Driver.CmdLine as CmdLine

-- | Converts a list of 'WarningMessages' into 'Messages', where the second element contains only
-- error, i.e. warnings that are considered fatal by GHC based on the input 'DynFlags'.
warningsToMessages :: DynFlags -> WarningMessages -> Messages
warningsToMessages dflags =
  partitionBagWith $ \warn ->
    case isWarnMsgFatal dflags warn of
      Nothing -> Left warn
      Just err_reason ->
        Right warn{ errMsgSeverity = SevError
                  , errMsgReason = ErrReason err_reason }

printBagOfErrors :: DynFlags -> Bag ErrMsg -> IO ()
printBagOfErrors dflags bag_of_errors
  = sequence_ [ let style = mkErrStyle unqual
                    ctx   = initSDocContext dflags style
                in putLogMsg dflags reason sev s $ withPprStyle style (formatErrDoc ctx doc)
              | ErrMsg { errMsgSpan      = s,
                         errMsgDoc       = doc,
                         errMsgSeverity  = sev,
                         errMsgReason    = reason,
                         errMsgContext   = unqual } <- sortMsgBag (Just dflags)
                                                                  bag_of_errors ]

handleFlagWarnings :: DynFlags -> [CmdLine.Warn] -> IO ()
handleFlagWarnings dflags warns = do
  let warns' = filter (shouldPrintWarning dflags . CmdLine.warnReason)  warns

      -- It would be nicer if warns :: [Located MsgDoc], but that
      -- has circular import problems.
      bag = listToBag [ mkPlainWarnMsg loc (text warn)
                      | CmdLine.Warn _ (L loc warn) <- warns' ]

  printOrThrowWarnings dflags bag

-- | Checks if given 'WarnMsg' is a fatal warning.
isWarnMsgFatal :: DynFlags -> WarnMsg -> Maybe (Maybe WarningFlag)
isWarnMsgFatal dflags ErrMsg{errMsgReason = Reason wflag}
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
printOrThrowWarnings :: DynFlags -> Bag WarnMsg -> IO ()
printOrThrowWarnings dflags warns = do
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
    else printBagOfErrors dflags warns
