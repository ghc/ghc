module Main where

import Control.Concurrent.STM
import Control.Exception

main :: IO ()
main = do
  result <- try @SomeException $ atomically $
    -- LHS retries → CATCH_RETRY_FRAME gets running_alt_code=1, RHS executes.
    -- RHS throws → raiseExceptionHelper walks the stack, finds the
    -- CATCH_RETRY_FRAME (running_alt_code=1), and must NOT abort tso->trec.
    orElse retry (throwSTM (ErrorCall "test"))
  case result of
    Left _  -> putStrLn "OK"
    Right _ -> putStrLn "impossible"
