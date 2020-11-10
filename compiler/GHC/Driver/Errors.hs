{-# LANGUAGE ViewPatterns #-}

module GHC.Driver.Errors (
    module GHC.Driver.Errors.Types
  , warningsToMessages
  , printOrThrowWarnings
  , printBagOfErrors
  , isWarnMsgFatal
  , handleFlagWarnings
  ) where

import Data.Bifunctor ( bimap )
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
import GHC.Driver.Errors.Types
import GHC.Driver.Errors.Ppr ()

-- | Converts a list of 'WarningMessages' into 'Messages', where the second element contains only
-- error, i.e. warnings that are considered fatal by GHC based on the input 'DynFlags'.
warningsToMessages :: DynFlags -> WarningMessages GhcWarning -> Messages GhcWarning GhcError
warningsToMessages dflags (getWarningMessages -> w) =
  bimap mkWarningMessages (fmap GhcFatalWarning . mkErrorMessages) $
  flip partitionBagWith w $ \warn ->
    case isWarnMsgFatal dflags warn of
      Nothing -> Left warn
      Just err_reason ->
        Right warn{ errMsgSeverity = SevError
                  , errMsgReason = ErrReason err_reason }

printBagOfErrors :: RenderableDiagnostic a => DynFlags -> Bag (ErrMsg a) -> IO ()
printBagOfErrors dflags bag_of_errors
  = sequence_ [ let style = mkErrStyle unqual
                    ctx   = initSDocContext dflags style
                in putLogMsg dflags reason sev s
                $ withPprStyle style (formatErrDoc ctx (renderDiagnostic doc))
              | ErrMsg { errMsgSpan      = s,
                         errMsgDiagnostic = doc,
                         errMsgSeverity  = sev,
                         errMsgReason    = reason,
                         errMsgContext   = unqual } <- sortMsgBag (Just dflags)
                                                                  bag_of_errors ]

-- | Given some 'WarningMessages', turn (and throw them) into an exception if
-- -Werror is enabled, or print them out otherwise.
printOrThrowWarnings :: DynFlags -> WarningMessages GhcWarning -> IO ()
printOrThrowWarnings dflags (getWarningMessages -> warns) = do
  let (make_error, warns') =
        mapAccumBagL
          (\make_err warn ->
            case isWarnMsgFatal dflags warn of
              Nothing ->
                (make_err, warn)
              Just err_reason ->
                (True, makeIntoError (ErrReason err_reason) warn))
          False warns
  if make_error
    then throwIO (mkSrcErr $
                  promoteWarningsToErrors GhcFatalWarning (mkWarningMessages warns'))
    else printBagOfErrors dflags warns

-- | Checks if given 'WarnMsg' is a fatal warning.
isWarnMsgFatal :: DynFlags -> ErrMsg w -> Maybe (Maybe WarningFlag)
isWarnMsgFatal dflags ErrMsg{errMsgReason = Reason wflag}
  = if wopt_fatal wflag dflags
      then Just (Just wflag)
      else Nothing
isWarnMsgFatal dflags _
  = if gopt Opt_WarnIsError dflags
      then Just Nothing
      else Nothing

handleFlagWarnings :: DynFlags -> [CmdLine.Warn] -> IO ()
handleFlagWarnings dflags warns = do
  let warns' = filter (shouldPrintWarning dflags . CmdLine.warnReason)  warns
      mk (CmdLine.Warn _ (L loc warn)) = mkPlainWarnMsg loc (text warn)
      msgs = mkWarningMessages (listToBag . map mk $ warns')
  printOrThrowWarnings dflags (GhcWarningRaw <$> msgs)

-- Given a warn reason, check to see if it's associated -W opt is enabled
shouldPrintWarning :: DynFlags -> CmdLine.WarnReason -> Bool
shouldPrintWarning dflags CmdLine.ReasonDeprecatedFlag
  = wopt Opt_WarnDeprecatedFlags dflags
shouldPrintWarning dflags CmdLine.ReasonUnrecognisedFlag
  = wopt Opt_WarnUnrecognisedWarningFlags dflags
shouldPrintWarning _ _
  = True
