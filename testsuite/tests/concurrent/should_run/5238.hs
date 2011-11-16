{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Concurrent.STM

main = do
  ms1 ← getMaskingState
  atomically $ (throwSTM Overflow) `catchSTM`
               (\(e ∷ SomeException) → return ())
  ms2 ← getMaskingState
  putStrLn $ show (ms1, ms2)
