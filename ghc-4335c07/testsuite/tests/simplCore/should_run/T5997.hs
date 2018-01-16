{-# LANGUAGE BangPatterns #-}

module Main where

-- Stack overflow if the tail recursion does not work out properly

incompleteBetaWorker :: Double -> Double
incompleteBetaWorker _ = loop 1 1 1 1
  where
    -- Constants
    eps = 1e-15
    -- Loop
    loop :: Double -> Int -> Double -> Double -> Double
    loop !psq !ns !term !betain
      | done           = betain'
      | ns > 10000000  = betain'
      | otherwise      = loop psq' (ns + 1) term' betain'
      where
        -- New values
        term'   = term
        betain' = betain
        psq'    = if ns < 0 then psq + 1 else psq
        -- This condition cause stack overflow
        done = db <= eps && db <= eps*betain' where db = abs term'
        -- With this it loops endlessly
        -- done = db <= eps * betain' where db = abs term'

main :: IO ()
main = print $ incompleteBetaWorker 0
