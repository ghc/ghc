{-# LANGUAGE GHC2021, UnboxedTuples #-}
module Main (main) where

import Control.Exception

newtype Tricky = TrickyCon { unTrickyCon :: (# #) -> Tricky }

data StrictBox a = SBox !a !a

main :: IO ()
main = do
  let
    tricky :: Tricky
    {-# OPAQUE tricky #-}
    tricky = TrickyCon $ \(# #) -> TrickyCon $ \(# #) ->
      error "tricky called with at least two args"

    applyToN :: Int -> Tricky -> Tricky
    {-# OPAQUE applyToN #-}
    applyToN n a | n == 0    = a
                 | otherwise = applyToN (n - 1) a `unTrickyCon` (# #)

    val = applyToN 12345 tricky

  v <- try @ErrorCall $ evaluate (SBox val val)
  case v of
    Left _ -> pure ()
    Right _ -> putStrLn "unreachable"
