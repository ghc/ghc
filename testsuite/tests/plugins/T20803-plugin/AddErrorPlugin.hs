{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}

module AddErrorPlugin where

import GHC.Plugins
import GHC.Types.Error
import GHC.Hs
import GHC.Data.Bag
import GHC.Parser.Errors.Types

import System.IO

-- Tests whether it's possible to add a parse error
plugin :: Plugin
plugin = defaultPlugin {parsedResultAction = parsedAction}

parsedAction :: [CommandLineOption] -> ModSummary -> HsParsedModule
             -> (Messages PsWarning, Messages PsError)
             -> Hsc (HsParsedModule, (Messages PsWarning, Messages PsError))
parsedAction _ _ pm (warns, _) = do
  liftIO $ putStrLn "parsePlugin"
  -- TODO: Remove #20791
  liftIO $ hFlush stdout
  pure (pm, (warns, mkMessages $ unitBag err))
  where
    err = MsgEnvelope
      { errMsgSpan = UnhelpfulSpan UnhelpfulNoLocationInfo
      , errMsgContext = alwaysQualify
      , errMsgDiagnostic = PsErrEmptyLambda
      , errMsgSeverity = SevError
      }
