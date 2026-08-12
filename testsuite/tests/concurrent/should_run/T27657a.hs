{-# LANGUAGE ScopedTypeVariables #-}

-- A retry escaping a catchSTM handler must reach the enclosing orElse. An IO
-- CATCH_FRAME in the way trips an assertion in findRetryFrameHelper.

import Control.Exception
import GHC.Conc

main :: IO ()
main = do
  r <- atomically $
         catchSTM (throwSTM (ErrorCall "boom"))
                  (\(_ :: SomeException) -> retry)
           `orElse` pure "T27657a: completed"
  putStrLn r
