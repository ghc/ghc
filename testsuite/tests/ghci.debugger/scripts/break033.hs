module Break033 where

import Control.Exception

f :: Int -> IO Int
f x = do
  r <- try (evaluate (x `div` 0))
  case r of
    Left DivideByZero -> pure x
    Left e            -> throwIO e
    Right y           -> pure y
