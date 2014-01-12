{-# LANGUAGE Rank2Types #-}

module Main where
import Prelude hiding( succ, pred )

newtype Ch = Ch (forall a. (a -> a) -> a -> a)

apply :: Ch -> (a->a) -> a -> a
apply (Ch f) = f

instance Eq Ch where
  a == b = isZero (a - b)

instance Show Ch where
  show a = show (fromCh a)

instance Num Ch where
  fromInteger n = toCh n
  m + n = apply n succ m
  m - n = apply n pred m
  m * n = apply m (n +) zero

zero :: Ch
zero = Ch (\f z -> z)

succ :: Ch -> Ch
succ n = Ch (\f z -> f (apply n f z))

isZero :: Ch -> Bool
isZero n = apply n (const False) True

toCh :: Integer -> Ch
toCh 0 = zero
toCh n = succ (toCh (n-1))

fromCh :: Ch -> Int
fromCh n = apply n (+1) 0

pred :: Ch -> Ch
pred n = snd (apply n g (zero, zero))
 where g (m,_) = (succ m, m)


main = print ((3+4)*12 - 10::Ch)
