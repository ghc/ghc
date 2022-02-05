module ThrowTestLib
  ( runThrowTest,
    CustomException,
  )
where

import Control.Exception
import GHC.Exception
import GHC.Exception.Backtrace
import GHC.Exception.Type
import GHC.IO.Unsafe

data CustomException = CustomException1 | CustomException2 | CustomException3 | CustomException4 deriving (Show)

instance Exception CustomException

runThrowTest :: (CustomException -> IO Int) -> IO ()
runThrowTest throwFunction = do
  print "=== Test backtraces ==="
  printBacktrace throwFunction [] 0 CustomException1
  printBacktrace throwFunction [HasCallStackBacktraceMech] 0 CustomException1
  printBacktrace throwFunction [CostCenterBacktraceMech] 0 CustomException2
  -- ExecutionStackBacktraceMech unfortunately crashes unless GHC was
  -- configured with '--enable-dwarf-unwind'.
  -- printBacktrace [ExecutionStackBacktraceMech]
  printBacktrace throwFunction [IPEBacktraceMech] 1 CustomException3
  printBacktrace
    throwFunction
    [ IPEBacktraceMech,
      CostCenterBacktraceMech,
      HasCallStackBacktraceMech
    ]
    2
    CustomException4

printBacktrace :: (CustomException -> IO Int) -> [BacktraceMechanism] -> Int -> CustomException -> IO ()
printBacktrace throwFunction mechs deepness e = do
  print $ "Backtrace mechanisms " ++ show mechs ++ ":"
  setDefaultBacktraceMechanisms mechs
  _ <-
    catch
      (produceDeepStack throwFunction deepness e)
      (\e -> printBacktraces e >> pure 42)
  pure ()

printBacktraces :: SomeExceptionWithBacktrace -> IO ()
printBacktraces = putStrLn . pprBacktraces

-- Force creation of Stg stack return frames for IPE based backtraces.
produceDeepStack :: (CustomException -> IO Int) -> Int -> CustomException -> IO Int
produceDeepStack throwFunction deepness e = case unsafePerformIO $ getDeepStackCase deepness e of
  -- Due to the thrown exception, the cases below are never hit!
  -- But we need to include some "noise" here, such that GHC doesn't simplify
  -- the case expression away.
  0 -> pure 42
  i -> pure i
  where
    -- Get the exception to throw as parameter, so that GHC cannot use an
    -- already evaluated thunk from a prior execution.
    getDeepStackCase :: Int -> CustomException -> IO Int
    getDeepStackCase 0 e = throwFunction e
    getDeepStackCase n e = getDeepStackCase (n - 1) e
