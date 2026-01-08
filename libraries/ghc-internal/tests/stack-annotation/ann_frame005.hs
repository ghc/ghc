import Control.Concurrent.STM
import Control.Exception
import Control.Exception.Backtrace (BacktraceMechanism(IPEBacktrace), setBacktraceMechanismState)
import Control.Exception.Context (displayExceptionContext)
import Control.Monad
import Data.List (isInfixOf)
import TestUtils

data SimpleBoom = SimpleBoom deriving (Show)

instance Exception SimpleBoom

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  mapM_ (uncurry runCase)
    [ ("throwIO SimpleBoom", throwIOAction)
    , ("undefined", undefinedAction)
    , ("error", errorAction)
    , ("throwSTM", throwSTMAction)
    ]

runCase :: String -> IO () -> IO ()
runCase label action = do
  putStrLn ("=== " ++ label ++ " ===")
  annotateCallStackIO $
    annotateStackStringIO ("catch site for " ++ label) $
      catch action (handler label)

throwIOAction :: IO ()
throwIOAction =
  annotateStackStringIO "raising action" $
    annotateStackStringIO "throwIO SimpleBoom" $
      throwIO SimpleBoom

undefinedAction :: IO ()
undefinedAction =
  annotateStackStringIO "raising undefined action" $
    void $
      evaluate $
        annotateStackString "undefined thunk" (undefined :: Int)

errorAction :: IO ()
errorAction =
  annotateStackStringIO "raising error action" $
    void $
      evaluate $
        annotateStackString "error thunk" (error "error from annotateStackString" :: Int)

throwSTMAction :: IO ()
throwSTMAction =
  annotateStackStringIO "raising throwSTM action" $
    atomically $
      annotateStackString "throwSTM SimpleBoom" $
        throwSTM SimpleBoom

handler :: String -> SomeException -> IO ()
handler label se =
  annotateStackStringIO ("handler for " ++ label) $
    annotateStackStringIO ("forcing SomeException for " ++ label) $ do
      message <- evaluate (displayException se)
      putStrLn ("Caught exception: " ++ message)
      let ctx = displayExceptionContext (someExceptionContext se)
          ctxLines = lines ctx
      putStrLn "Exception context:"
      case ctxLines of
        [] -> putStrLn "<empty>"
        ls -> mapM_ (putStrLn . ("- " ++)) ls
      let handlerTag = "handler for " ++ label
      -- Check that the callstack is from the callsite, not the handling site
      when (any (handlerTag `isInfixOf`) ctxLines) $
        error $ "handler annotation leaked into context for " ++ label
      putStrLn "Handler annotation not present in context"
