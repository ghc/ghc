import Control.Exception
import Control.Exception.Annotation
import GHC.Internal.Exception.Backtrace

data MyBacktraces = MyBacktraces

instance ExceptionAnnotation MyBacktraces where
  displayExceptionAnnotation MyBacktraces = "MyBacktraces"

main :: IO ()
main = do
  setCollectExceptionAnnotation (pure MyBacktraces)
  catchAndPrint functionThatThrows
  catchAndPrint functionThatErrors

catchAndPrint :: IO () -> IO ()
catchAndPrint act = do
  act `catch` \(exc :: SomeException) -> do
    putStrLn $ displayExceptionWithInfo exc

functionThatThrows :: IO ()
functionThatThrows = do
  throwIO $ ErrorCall "functionThatThrows"

functionThatErrors :: a
functionThatErrors = do
  error "functionThatErrors"
