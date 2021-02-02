{-# LANGUAGE ViewPatterns #-}

module GHC.Driver.Errors (
    warningsToMessages
  , printOrThrowWarnings
  , printBagOfErrors
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
import qualified GHC.Driver.CmdLine as CmdLine

-- | Converts a list of 'WarningMessages' into a tuple where the second element contains only
-- error, i.e. warnings that are considered fatal by GHC based on the input 'DynFlags'.
warningsToMessages :: DynFlags -> WarningMessages -> (WarningMessages, ErrorMessages)
warningsToMessages dflags =
  partitionBagWith $ \warn ->
    case isWarnMsgFatal dflags warn of
      Nothing     -> Left warn
      Just reason -> Right (promoteWarningToError (ErrPromotedFromWarning reason) warn)

-- | Promotes a 'WarnMsg' into an error, given a proper 'ErrReason'.
promoteWarningToError :: ErrReason -> WarnMsg -> MsgEnvelope DecoratedSDoc
promoteWarningToError errReason msg = msg { errMsgSeverity = SevError errReason }

printBagOfErrors :: RenderableDiagnostic a => DynFlags -> Bag (MsgEnvelope a) -> IO ()
printBagOfErrors dflags bag_of_errors
  = sequence_ [ let style = mkErrStyle unqual
                    ctx   = initSDocContext dflags style
                in putLogMsg dflags (MCDiagnostic sev) s $
                   withPprStyle style (formatBulleted ctx (renderDiagnostic doc))
              | MsgEnvelope { errMsgSpan      = s,
                              errMsgDiagnostic = doc,
                              errMsgSeverity  = sev,
                              errMsgContext   = unqual } <- sortMsgBag (Just dflags)
                                                                       bag_of_errors ]

handleFlagWarnings :: DynFlags -> [CmdLine.Warn] -> IO ()
handleFlagWarnings dflags warns = do
  let warns' = filter (shouldPrintWarning dflags . CmdLine.warnReason)  warns

      -- It would be nicer if warns :: [Located SDoc], but that
      -- has circular import problems.
      bag = listToBag [ mkPlainMsgEnvelope (SevWarning NoWarnReason) loc (text warn)
                      | CmdLine.Warn _ (L loc warn) <- warns' ]

  printOrThrowWarnings dflags bag

-- | Checks if given 'WarnMsg' is a fatal warning.
isWarnMsgFatal :: DynFlags -> WarnMsg -> Maybe (Either GeneralFlag WarningFlag)
isWarnMsgFatal dflags (errMsgSeverity -> severity) =
  case severity of
    SevError _              -> Nothing -- nothing to do, this is already an error.
    SevWarning NoWarnReason ->
      if gopt Opt_WarnIsError dflags
        then Just (Left Opt_WarnIsError)
        else Nothing
    SevWarning (WarnReason wflag) ->
      if wopt_fatal wflag dflags
        then Just (Right wflag)
        else Nothing
    SevWarning (WarnDemotedFromError _) -> Nothing

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
                (True, promoteWarningToError (ErrPromotedFromWarning err_reason) warn))
          False warns
  if make_error
    then throwIO (mkSrcErr warns')
    else printBagOfErrors dflags warns
