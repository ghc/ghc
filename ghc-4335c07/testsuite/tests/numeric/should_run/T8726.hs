import Control.Monad
import Data.Bits
import Data.List
import Data.Ord

-- | test-values to use as numerator/denominator
posvals :: [Integer]
posvals = [1,2,3,4,5,9,10,14,15,16,17] ++
          [ n | e <- ([5..70]++[96,128,160,192,224])
              , ofs <- [-1..1], let n = bit e + ofs ]

posvalsSum :: Integer
posvalsSum = 0x300000003000000030000000300000003000001800000000000000000

vals :: [Integer]
vals = sortBy (comparing abs) $ map negate posvals ++ [0] ++ posvals


main :: IO ()
main = do
    unless (sum posvals == posvalsSum) $
        fail $ "sum posvals == " ++ show (sum posvals)

    forM_ [ (n,d) | n <- vals, d <- vals, d /= 0 ] $ \(n,d) -> do
        let check sp p = unless (p n d) $ fail (sp ++ " " ++ show n ++ " " ++ show d)

        check "rem0"      prop_rem0
        check "mod0"      prop_mod0

        check "divMod0"   prop_divMod0
        check "divMod1"   prop_divMod1
        check "divMod2"   prop_divMod2

        check "quotRem0"  prop_quotRem0
        check "quotRem1"  prop_quotRem1
        check "quotRem2"  prop_quotRem2

    -- putStrLn "passed"

-- QuickCheck style properties

prop_rem0 :: Integer -> Integer -> Bool
prop_rem0 n d
  | n >= 0     = (n `rem` d) `inside` (-1,abs d)
  | otherwise  = (n `rem` d) `inside` (-(abs d),1)
  where
    inside v (l,u) = l < v && v < u

prop_mod0 :: Integer -> Integer -> Bool
prop_mod0 n d
  | d >= 0     = (n `mod` d) `inside` (-1,d)
  | otherwise  = (n `mod` d) `inside` (d,1)
  where
    inside v (l,u) = l < v && v < u

-- | Invariant from Haskell Report
prop_divMod0 :: Integer -> Integer -> Bool
prop_divMod0 n d = (n `div` d) * d + (n `mod` d) == n

prop_divMod1 :: Integer -> Integer -> Bool
prop_divMod1 n d = divMod n d == (n `div` d, n `mod` d)

-- | Compare IUT to implementation of 'divMod' in terms of 'quotRem'
prop_divMod2 :: Integer -> Integer -> Bool
prop_divMod2 n d = divMod n d == divMod' n d
  where
    divMod' x y = if signum r == negate (signum y) then (q-1, r+y) else qr
      where qr@(q,r) = quotRem x y

-- | Invariant from Haskell Report
prop_quotRem0 :: Integer -> Integer -> Bool
prop_quotRem0 n d = (n `quot` d) * d + (n `rem` d) == n

prop_quotRem1 :: Integer -> Integer -> Bool
prop_quotRem1 n d = quotRem n d == (n `quot` d, n `rem` d)

-- | Test symmetry properties of 'quotRem'
prop_quotRem2 :: Integer -> Integer -> Bool
prop_quotRem2 n d = (qr == negQ (quotRem n (-d))    &&
                     qr == negR (quotRem (-n) (-d)) &&
                     qr == (negQ . negR) (quotRem (-n) d))
  where
    qr = quotRem n d
    negQ (q,r) = (-q,r)
    negR (q,r) = (q,-r)
