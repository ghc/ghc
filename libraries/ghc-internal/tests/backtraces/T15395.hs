{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

import GHC.Internal.Control.Exception
import GHC.Internal.Data.Foldable (traverse_)
import GHC.Internal.Exception.Backtrace
import GHC.Internal.Exception.Context
import GHC.Internal.Exception.Type
import GHC.Internal.STM (atomically, throwSTM)
import qualified GHC.Internal.Stack as HCS
import qualified GHC.Internal.Stack.Types as HCS
import Control.Monad (when)
import qualified Data.List as List
import System.Exit (exitFailure)

main :: IO ()
main = do
  -- Make sure there are HasCallStackBacktraces
  setBacktraceMechanismState HasCallStackBacktrace True
  mapM_ (uncurry runCase)
    [ ("throw", throwAction)
    , ("throwIO", throwIOAction)
    , ("error", errorAction)
    , ("throwSTM", throwSTMAction)
    , ("undefined", undefinedAction)
    ]

runCase :: String -> IO () -> IO ()
runCase name act = do
  putStrLn $ "=== Validate stack size of '" ++ name ++ "' has length 1"
  catchAndVerifyStackTraceLength name act
  putStrLn ""

throwAction :: IO ()
throwAction = evaluate $ throw $ ErrorCall "my throw error"

throwIOAction :: IO ()
throwIOAction = throwIO $ ErrorCall "my throwIO error"

errorAction :: IO ()
errorAction = error "plain error"

throwSTMAction :: IO ()
throwSTMAction = atomically $ throwSTM $ ErrorCall "my throwSTM error"

undefinedAction :: IO ()
undefinedAction = evaluate undefined

catchAndVerifyStackTraceLength :: String -> IO () -> IO ()
catchAndVerifyStackTraceLength name act = do
  try act >>= \ case
    Right _ -> do
      putStrLn $ "Exception expected but got a result for '" ++ name ++ "'"
      exitFailure
    Left exc ->
      verifyBacktraceSize name exc

verifyBacktraceSize :: String -> SomeException -> IO ()
verifyBacktraceSize label se = do
  message <- evaluate (displayException se)
  putStrLn "==== Caught exception:"
  putStrLn message
  putStrLn "==== Exception Backtraces:"
  let backtraces = getExceptionAnnotations @Backtraces $ someExceptionContext se
  traverse_ validateBacktrace backtraces

validateBacktrace :: Backtraces -> IO ()
validateBacktrace bt =
  case btrHasCallStack bt of
    Nothing -> pure ()
    Just cs -> do
      let stack = HCS.getCallStack cs

      traverse_ mustNotReferenceInternalPackages stack
      traverse_ (putStrLn . prettyCallSite) stack
  where
    prettyCallSite (f, loc) = "- " ++ f ++ ", called at " ++ HCS.prettySrcLoc loc

    mustNotReferenceInternalPackages (_, loc) =
      case List.find (HCS.srcLocPackage loc ==) internalPackages of
        Just val -> fail $ "Stack trace must not reference '" ++ val ++ "' package."
        Nothing -> pure ()

internalPackages :: [String]
internalPackages = ["base", "ghc", "ghc-internal", "ghc-experimental"]
