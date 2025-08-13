import Control.Exception
import Control.Exception.Annotation
import GHC.Internal.Exception.Backtrace


newtype MyBacktraces = MyBacktraces Backtraces

instance ExceptionAnnotation MyBacktraces where
  displayExceptionAnnotation (MyBacktraces bt) = unlines
    [ "My custom Backtraces:"
    , displayBacktraces bt
    ]

main :: IO ()
main = do
  setCollectExceptionAnnotation (MyBacktraces <$> collectBacktraces)
  catchAndPrint whileHandlingThrowIO
  catchAndPrint whileHandlingError

catchAndPrint :: IO () -> IO ()
catchAndPrint act = do
  act `catch` \(exc :: SomeException) -> do
    putStrLn $ displayExceptionWithInfo exc

whileHandlingThrowIO :: IO ()
whileHandlingThrowIO =
  handleJust
    (\e -> case e of
      ErrorCall{} -> Just e
      _ -> Nothing)
    (\_ -> throwIO $ ErrorCall "Error in Exception Handler")
    (throwIO $ ErrorCall "Main Error")

whileHandlingError :: IO ()
whileHandlingError = do
  handleJust
    (\e -> case e of
      ErrorCall{} -> Just e
      _ -> Nothing)
    (\_ -> error "Error in Exception Handler")
    (error "Main Error")
