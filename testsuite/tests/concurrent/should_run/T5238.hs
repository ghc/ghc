{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import GHC.Conc

main = do
  ms1 ← getMaskingState
  atomically $ (throwSTM Overflow) `catchSTM`
               (\(e ∷ SomeExceptionWithLocation) → return ())
  ms2 ← getMaskingState
  putStrLn $ show (ms1, ms2)
