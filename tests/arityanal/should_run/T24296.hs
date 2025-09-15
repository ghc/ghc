{-# LANGUAGE GHC2021, UnboxedTuples #-}
module Main (main) where

newtype Tricky = TrickyCon { unTrickyCon :: (# #) -> Tricky }

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

  case applyToN 12345 tricky of
    !_ -> putStrLn "unreachable"
