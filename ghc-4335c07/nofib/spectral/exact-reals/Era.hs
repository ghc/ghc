-- Taken from "The worlds shortest correct exact real arithmetic program?"
-- David Lester, Information and Computation 216, pp39-46, 2012, Elsevier

module Main where

import Data.Ratio
import Data.Char

-- Should print exactly zero
main = print (sum (map (\x -> sin (x * pi)) [1..500::CR]))


digits :: Int   -- number of printed decimal digits
digits = 40     -- (change as required)

data CR = CR_ (Int -> Integer)

instance Eq  CR where x == y = approx (x-y) == 0

instance Ord CR where
  compare x y = compare (approx (x-y)) 0
  max = binary 0 max
  min = binary 0 min

instance Num CR where
  x+y = binary 2 (+) x y
  (CR_ x')*(CR_ y')
    = CR_ (\p -> rnd ((x' (p+sy)*y' (p+sx))%2^(p+sx+sy)))
    where x0 = abs (x' 0)+2; y0 = abs (y' 0)+2
          sx = sizeinbase x0 2+3; sy = sizeinbase y0 2+3
  negate (CR_ x') = CR_ (\p -> negate (x' p))
  abs x = max x (negate x)
  signum x = fromInteger (signum (approx x))
  fromInteger n = CR_ (\p -> n*2^p)

instance Fractional CR where
  recip (CR_ x') = CR_ (\p -> let s = head [n | n <- [0..],3<=abs (x' n)]
                              in rnd (2^(2*p+2*s+2)%(x' (p+2*s+2))))
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

div2n,mul2n :: CR -> Int -> CR
div2n (CR_ x') n = CR_ (\p -> if p >= n then x' (p-n) else rnd (x' p%2^n))
mul2n (CR_ x') n = CR_ (\p -> x' (p+n))

instance Floating CR where
  pi = 16*atan (1/5) - 4*atan (1/239)
  sqrt x = CR_ (\p -> floorsqrt (x' (2*p))) where (CR_ x') = x
  log x | t < 0  = error "log of negative number\n"
        | t <= 2 = - log (recip x)
        | t <= 8 = log_dr x
        | otherwise = {- 8 < t -}
                      log_dr (div2n x n)
                      + fromInteger (toInteger n)*log2
        where t = bits 2 x; n = sizeinbase t 2 - 3

  exp x | n < 0     = div2n (exp_dr s) (fromInteger (-n))
        | n > 0     = mul2n (exp_dr s) (fromInteger n)
        | otherwise = exp_dr s
        where n = bits 0 (x/log2); s = x-fromInteger n*log2

  atan x | t <= -20  = atan_dr (negate (recip x)) - piBy2
         | t <= -4   = -piBy4 - atan_dr (xp1/xm1)
         | t <= 3    = atan_dr x
         | t <= 19   = piBy4 + atan_dr (xm1/xp1)
         | otherwise = {- t >= 20 -}
                       piBy2 - atan_dr (recip x)
        where t = bits 3 x; xp1 = x+1; xm1 = x-1

  asin x | t > 0     = pi / 2 - atan (s/x)
         | t == 0    = atan (x/s)
         | otherwise = {- t < 0 -}
                       atan (s/x) - pi / 2
         where t = bits 0 x; s = sqrt (1 - x*x)

  sin   x = sin_rc (trig_rr x)
  cos   x = cos_rc (trig_rr x)
  acos  x = pi / 2 - asin x
  sinh  x = (y - recip y) / 2 where y = exp x
  cosh  x = (y + recip y) / 2 where y = exp x
  tanh  x = (y - y') / (y + y') where y = exp x; y' = recip y
  asinh x = log (x + sqrt (x*x+1))
  acosh x = log (x + sqrt (x*x-1))
  atanh x = log ((1+x)/(1-x)) / 2

trig_rr :: CR -> (Integer,CR)
trig_rr x = (s `mod` 8, x-piBy4*fromInteger s)
  where s = rbits 2 (x/piBy4)

sin_rc,cos_rc :: (Integer,CR) -> CR
sin_rc (n,y) | n == 0    = s
             | n == 1    = sqrt1By2*(c+s)
             | n == 2    = c
             | n == 3    = sqrt1By2*(c-s)
             | otherwise = {- n >= 4 -} -sin_rc(n-4,y)
             where s = sin_dr y; c = cos_dr y
cos_rc (n,y) = sin_rc ((n+2) `mod` 8,y)

acc_seq :: (Rational -> Integer -> Rational) -> [Rational]
acc_seq f = scanl f (1%1) [1..]

exp_seq,log_seq,sin_seq,cos_seq,atan_seq :: [Rational]
exp_seq = acc_seq (\a n -> a*(1%n))
log_seq = [(if even n then -1 else 1)%n | n <- [1..]]
sin_seq = acc_seq (\a n -> -a*(1%(2*n*(2*n+1))))
cos_seq = acc_seq (\a n -> -a*(1%(2*n*(2*n-1))))
atan_seq = [(if even n then 1 else -1)%(2*n+1) | n <- [0..]]

exp_dr,log_dr,log_drx,sin_dr,cos_dr,atan_dr,atan_drx :: CR -> CR
exp_dr = power_series exp_seq (+3)
log_dr x | 3 <= t && t <= 5 = log_drx x
         | otherwise        = mul2n (log_drx (sqrt x)) 1
         where t = bits 2 x

log_drx x = y*power_series log_seq (+2) y where y = x - 1
sin_dr  x = x*power_series sin_seq (+2) (x*x)
cos_dr  x = power_series cos_seq (+2) (x*x)
atan_dr x = x*atan_drx (x*x)
atan_drx  = power_series atan_seq (+2)

power_series :: [Rational] -> (Int -> Int) -> CR -> CR
power_series ps terms (CR_ x')
  = CR_ (\p -> let t = terms p; p' = p + l2t
                   l2t = 2*sizeinbase (toInteger t+1) 2+6
                   xr = x' p'
                   xn = 2^p'
                   g xn = rnd ((xn*xr)%(2^p'))
               in rnd (f (iterate g xn) (take t ps) % (2^l2t)))
  where
    f _ [] = 0
    f (x:xs) (c:cs) | t==0 = 0
                    | otherwise = t + f xs cs
      where
        t = rnd (c*(x%1))

piBy2,piBy4,log2,sqrt1By2 :: CR
piBy2    = div2n pi 1
piBy4    = div2n pi 2
log2     = -log_drx (recip (fromInteger 2))
sqrt1By2 = sqrt (recip (fromInteger 2))

instance Enum CR where 
  toEnum = fromInteger . toInteger
  fromEnum = fromInteger . rnd . toRational

instance Real CR where
  toRational x@(CR_ x') = x' n % 2^n
     where n = digitsToBits digits

instance RealFrac CR where
  properFraction x@(CR_ x') = (fromInteger n, x - fromInteger n)
    where n = x' 0

digitsToBits :: Int -> Int
digitsToBits d = ceiling (fromIntegral d* (logBase 2.0 10.0)) + 4

bits,rbits :: Int -> CR -> Integer
bits n (CR_ x') = x' n
rbits n x = rnd (bits n x%2^n)

approx = bits (digitsToBits digits)

binary :: Int -> (Integer -> Integer -> Integer) -> CR -> CR -> CR
binary 0 f (CR_ x') (CR_ y') = CR_ (\p -> f (x' p) (y' p))
binary n f (CR_ x') (CR_ y') = CR_ (\p -> rnd((f (x' (p+n)) (y' (p+n)))%2^n))

get_str :: Int -> CR -> String
get_str d x
  = (if s then "-" else "") ++ zs ++ (if d>0 then '.':fs else "")
  where
    b       = digitsToBits d
    ds      = show (rnd ((bits b x*10^d)%2^b))
    (s,ds') = let s = head ds == '-'
              in (s, if s then tail ds else ds)
    ds''    = take (max (d+1-length ds') 0) (repeat '0') ++ ds'
    (zs,fs) = splitAt (length ds'' -d) ds''

instance Read CR where
  readsPrec p = readSigned
     where
       readSigned ('-':s) = [(-k,t) | (k,t) <- readFloat s]
       readSigned ('+':s) = readFloat s
       readSigned s = readFloat s

instance Show CR where
  showsPrec p x | head xs == '-' = showParen (p > 6) (showString xs)
                | otherwise      = showString xs
    where
     xs = get_str digits x

readFloat :: ReadS CR
readFloat r = [(fromRational (n*10^^(k-d)),t) | (n,d,s) <- readFix r,
                                                (k,t) <- readExp s]
  where
    readFix r = [ (read (ds++ds'), length ds', t)
                | (ds,'.':s) <- lexDigits r
                , (ds',t) <- lexDigits s ]

    readExp (e:s) | e `elem` "eE" = readExp' s
    readExp s = [(0,s)]

    readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
    readExp' ('+':s) = readDec s
    readExp' s = readDec s

    readDec s = [(read ds,t) | (ds,t) <- lexDigits s]

lexDigits :: ReadS String
lexDigits s = [(cs,t) | (cs@(_:_),t) <- [span isDigit s]]

sizeinbase :: Integer -> Int -> Int
sizeinbase n b
  = f (abs n) where f n = if n <= 1 then 1 else 1 + f (n `div` toInteger b)

floorsqrt :: Integer -> Integer
floorsqrt x = until satisfy improve x
  where
    improve y = floor ((y*y+x)%(2*y))
    satisfy y = y*y <= x && x <= (y+1)*(y+1)

rnd :: Rational -> Integer
rnd x = floor (x+1/2)
