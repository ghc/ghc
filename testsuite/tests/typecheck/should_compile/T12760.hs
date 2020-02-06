{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module A where

import Data.List (minimumBy)
import Data.Ord (comparing)

data A a = A Int

newtype B = B Double
  deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating,RealFloat)

class C a where
    _c :: [a] -> D a

instance C B where
    _c = f2 u

data D x = D [(x,Double)] [ x ]

u = undefined

f1 :: RealFloat a => A a -> a -> [a] -> D a
f1 (A a1) m ps0 = D (zip tickvs []) labelvs
  where
    range _ | m == m = if m==0 then (-1,1) else (m, m)
    labelvs   = map fromRational $ f3 (fromIntegral a1) (range ps0)
    tickvs    = map fromRational $ f3 (fromIntegral a1) (head labelvs, head labelvs)

f2 :: RealFloat a => A a -> [a] -> D a
f2 lap ps = f1 u (minimum ps) ps

f3 :: RealFloat a => a -> (a,a) -> [Rational]
f3 k rs@(m,_ ) = map ((s*) . fromIntegral) [floor m .. ]
  where
    s = minimumBy (comparing ((+ k) . realToFrac)) [0]
