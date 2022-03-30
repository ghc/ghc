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

parsedAction :: [CommandLineOption] -> ModSummary
             -> ParsedResult -> Hsc ParsedResult
parsedAction _ _ (ParsedResult pm msgs) = do
  liftIO $ putStrLn "parsePlugin"
  -- TODO: Remove #20791
  liftIO $ hFlush stdout
  pure (ParsedResult pm msgs{psErrors = mkMessages $ unitBag err})
  where
    err = MsgEnvelope
      { errMsgSpan = UnhelpfulSpan UnhelpfulNoLocationInfo
      , errMsgContext = alwaysQualify
      , errMsgDiagnostic = PsErrEmptyLambda
      , errMsgSeverity = SevError
      }
