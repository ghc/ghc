module RecompCompletePragmaB where

import RecompCompletePragmaA

-- Use the pattern from A
usePattern :: Int -> Bool
usePattern P = True

usePattern2 :: Bool -> Int
usePattern2 Q = 0
