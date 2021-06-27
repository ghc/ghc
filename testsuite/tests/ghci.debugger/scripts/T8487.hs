import Control.Exception

f = do
  ma <- try $ evaluate a
  x <- case ma of
    Right str -> return a
    Left  err -> return $ show (err :: SomeExceptionWithLocation)
  putStrLn x
  where
    a :: String
    a = error "hi"
