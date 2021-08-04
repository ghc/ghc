-- | Elliptic Curve Arithmetic.
--
-- /WARNING:/ These functions are vulnerable to timing attacks.
module Crypto.PubKey.ECC.Prim
    ( scalarGenerate
    , pointAdd
    , pointNegate
    , pointDouble
    , pointBaseMul
    , pointMul
    , pointAddTwoMuls
    , isPointAtInfinity
    , isPointValid
    ) where

import Data.Maybe
import Crypto.Number.ModArithmetic
import Crypto.Number.F2m
import Crypto.Number.Generate (generateBetween)
import Crypto.PubKey.ECC.Types
import Crypto.Random

-- | Generate a valid scalar for a specific Curve
scalarGenerate :: MonadRandom randomly => Curve -> randomly PrivateNumber
scalarGenerate curve = generateBetween 1 (n - 1)
  where
        n = ecc_n $ common_curve curve

--TODO: Extract helper function for `fromMaybe PointO...`

-- | Elliptic Curve point negation:
-- @pointNegate c p@ returns point @q@ such that @pointAdd c p q == PointO@.
pointNegate :: Curve -> Point -> Point
pointNegate _            PointO     = PointO
pointNegate (CurveFP c) (Point x y) = Point x (ecc_p c - y)
pointNegate CurveF2m{}  (Point x y) = Point x (x `addF2m` y)

-- | Elliptic Curve point addition.
--
-- /WARNING:/ Vulnerable to timing attacks.
pointAdd :: Curve -> Point -> Point -> Point
pointAdd _ PointO PointO = PointO
pointAdd _ PointO q = q
pointAdd _ p PointO = p
pointAdd c p q
  | p == q = pointDouble c p
  | p == pointNegate c q = PointO
pointAdd (CurveFP (CurvePrime pr _)) (Point xp yp) (Point xq yq)
    = fromMaybe PointO $ do
        s <- divmod (yp - yq) (xp - xq) pr
        let xr = (s ^ (2::Int) - xp - xq) `mod` pr
            yr = (s * (xp - xr) - yp) `mod` pr
        return $ Point xr yr
pointAdd (CurveF2m (CurveBinary fx cc)) (Point xp yp) (Point xq yq)
    = fromMaybe PointO $ do
        s <- divF2m fx (yp `addF2m` yq) (xp `addF2m` xq)
        let xr = mulF2m fx s s `addF2m` s `addF2m` xp `addF2m` xq `addF2m` a
            yr = mulF2m fx s (xp `addF2m` xr) `addF2m` xr `addF2m` yp
        return $ Point xr yr
  where a = ecc_a cc

-- | Elliptic Curve point doubling.
--
-- /WARNING:/ Vulnerable to timing attacks.
--
-- This perform the following calculation:
-- > lambda = (3 * xp ^ 2 + a) / 2 yp
-- > xr = lambda ^ 2 - 2 xp
-- > yr = lambda (xp - xr) - yp
--
-- With binary curve:
-- > xp == 0   => P = O
-- > otherwise =>
-- >    s = xp + (yp / xp)
-- >    xr = s ^ 2 + s + a
-- >    yr = xp ^ 2 + (s+1) * xr
--
pointDouble :: Curve -> Point -> Point
pointDouble _ PointO = PointO
pointDouble (CurveFP (CurvePrime pr cc)) (Point xp yp) = fromMaybe PointO $ do
    lambda <- divmod (3 * xp ^ (2::Int) + a) (2 * yp) pr
    let xr = (lambda ^ (2::Int) - 2 * xp) `mod` pr
        yr = (lambda * (xp - xr) - yp) `mod` pr
    return $ Point xr yr
  where a = ecc_a cc
pointDouble (CurveF2m (CurveBinary fx cc)) (Point xp yp)
    | xp == 0   = PointO
    | otherwise = fromMaybe PointO $ do
        s <- return . addF2m xp =<< divF2m fx yp xp
        let xr = mulF2m fx s s `addF2m` s `addF2m` a
            yr = mulF2m fx xp xp `addF2m` mulF2m fx xr (s `addF2m` 1)
        return $ Point xr yr
  where a = ecc_a cc

-- | Elliptic curve point multiplication using the base
--
-- /WARNING:/ Vulnerable to timing attacks.
pointBaseMul :: Curve -> Integer -> Point
pointBaseMul c n = pointMul c n (ecc_g $ common_curve c)

-- | Elliptic curve point multiplication (double and add algorithm).
--
-- /WARNING:/ Vulnerable to timing attacks.
pointMul :: Curve -> Integer -> Point -> Point
pointMul _ _ PointO = PointO
pointMul c n p
    | n <  0 = pointMul c (-n) (pointNegate c p)
    | n == 0 = PointO
    | n == 1 = p
    | odd n = pointAdd c p (pointMul c (n - 1) p)
    | otherwise = pointMul c (n `div` 2) (pointDouble c p)

-- | Elliptic curve double-scalar multiplication (uses Shamir's trick).
--
-- > pointAddTwoMuls c n1 p1 n2 p2 == pointAdd c (pointMul c n1 p1)
-- >                                             (pointMul c n2 p2)
--
-- /WARNING:/ Vulnerable to timing attacks.
pointAddTwoMuls :: Curve -> Integer -> Point -> Integer -> Point -> Point
pointAddTwoMuls _ _  PointO _  PointO = PointO
pointAddTwoMuls c _  PointO n2 p2     = pointMul c n2 p2
pointAddTwoMuls c n1 p1     _  PointO = pointMul c n1 p1
pointAddTwoMuls c n1 p1     n2 p2
    | n1 < 0    = pointAddTwoMuls c (-n1) (pointNegate c p1) n2 p2
    | n2 < 0    = pointAddTwoMuls c n1 p1 (-n2) (pointNegate c p2)
    | otherwise = go (n1, n2)

  where
    p0 = pointAdd c p1 p2

    go (0,  0 ) = PointO
    go (k1, k2) =
        let q = pointDouble c $ go (k1 `div` 2, k2 `div` 2)
        in case (odd k1, odd k2) of
            (True  , True  ) -> pointAdd c p0 q
            (True  , False ) -> pointAdd c p1 q
            (False , True  ) -> pointAdd c p2 q
            (False , False ) -> q

-- | Check if a point is the point at infinity.
isPointAtInfinity :: Point -> Bool
isPointAtInfinity PointO = True
isPointAtInfinity _      = False

-- | check if a point is on specific curve
--
-- This perform three checks:
--
-- * x is not out of range
-- * y is not out of range
-- * the equation @y^2 = x^3 + a*x + b (mod p)@ holds
isPointValid :: Curve -> Point -> Bool
isPointValid _                           PointO      = True
isPointValid (CurveFP (CurvePrime p cc)) (Point x y) =
    isValid x && isValid y && (y ^ (2 :: Int)) `eqModP` (x ^ (3 :: Int) + a * x + b)
  where a  = ecc_a cc
        b  = ecc_b cc
        eqModP z1 z2 = (z1 `mod` p) == (z2 `mod` p)
        isValid e = e >= 0 && e < p
isPointValid (CurveF2m (CurveBinary fx cc)) (Point x y) =
    and [ isValid x
        , isValid y
        , ((((x `add` a) `mul` x `add` y) `mul` x) `add` b `add` (squareF2m fx y)) == 0
        ]
  where a  = ecc_a cc
        b  = ecc_b cc
        add = addF2m
        mul = mulF2m fx
        isValid e = modF2m fx e == e

-- | div and mod
divmod :: Integer -> Integer -> Integer -> Maybe Integer
divmod y x m = do
    i <- inverse (x `mod` m) m
    return $ y * i `mod` m
