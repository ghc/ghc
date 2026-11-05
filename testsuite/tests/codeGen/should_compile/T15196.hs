module M where

-- The NCG must not emit a parity jump for this comparison (#15196).
-- CHECK-NOT: {{[[:<:]]jp[[:>:]]}}
f :: Double -> Double -> Bool
f x y = if x < y then True else False
