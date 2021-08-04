-- | Elliptic Curve Arithmetic.
--
-- /WARNING:/ These functions are vulnerable to timing attacks.
{-# LANGUAGE ScopedTypeVariables #-}
module Crypto.ECC.Simple.Prim
    ( scalarGenerate
    , scalarFromInteger
    , pointAdd
    , pointNegate
    , pointDouble
    , pointBaseMul
    , pointMul
    , pointAddTwoMuls
    , pointFromIntegers
    , isPointAtInfinity
    , isPointValid
    ) where

import Data.Maybe
import Data.Proxy
import Crypto.Number.ModArithmetic
import Crypto.Number.F2m
import Crypto.Number.Generate (generateBetween)
import Crypto.ECC.Simple.Types
import Crypto.Error
import Crypto.Random

-- | Generate a valid scalar for a specific Curve
scalarGenerate :: forall randomly curve . (MonadRandom randomly, Curve curve) => randomly (Scalar curve)
scalarGenerate =
    Scalar <$> generateBetween 1 (n - 1)
  where
    n = curveEccN $ curveParameters (Proxy :: Proxy curve)

scalarFromInteger :: forall curve . Curve curve => Integer -> CryptoFailable (Scalar curve)
scalarFromInteger n
    | n < 0  || n >= mx = CryptoFailed $ CryptoError_EcScalarOutOfBounds
    | otherwise         = CryptoPassed $ Scalar n
  where
    mx = case curveType (Proxy :: Proxy curve) of
            CurveBinary (CurveBinaryParam b) -> b
            CurvePrime (CurvePrimeParam p)   -> p

--TODO: Extract helper function for `fromMaybe PointO...`

-- | Elliptic Curve point negation:
-- @pointNegate p@ returns point @q@ such that @pointAdd p q == PointO@.
pointNegate :: Curve curve => Point curve -> Point curve
pointNegate        PointO     = PointO
pointNegate point@(Point x y) =
    case curveType point of
        CurvePrime (CurvePrimeParam p) -> Point x (p - y)
        CurveBinary {} -> Point x (x `addF2m` y)

-- | Elliptic Curve point addition.
--
-- /WARNING:/ Vulnerable to timing attacks.
pointAdd :: Curve curve => Point curve -> Point curve -> Point curve
pointAdd PointO PointO = PointO
pointAdd PointO q = q
pointAdd p PointO = p
pointAdd p q
  | p == q             = pointDouble p
  | p == pointNegate q = PointO
pointAdd point@(Point xp yp) (Point xq yq) =
    case ty of
        CurvePrime (CurvePrimeParam pr) -> fromMaybe PointO $ do
            s <- divmod (yp - yq) (xp - xq) pr
            let xr = (s ^ (2::Int) - xp - xq) `mod` pr
                yr = (s * (xp - xr) - yp) `mod` pr
            return $ Point xr yr
        CurveBinary (CurveBinaryParam fx) -> fromMaybe PointO $ do
            s <- divF2m fx (yp `addF2m` yq) (xp `addF2m` xq)
            let xr = mulF2m fx s s `addF2m` s `addF2m` xp `addF2m` xq `addF2m` a
                yr = mulF2m fx s (xp `addF2m` xr) `addF2m` xr `addF2m` yp
            return $ Point xr yr
  where
    ty = curveType point
    cc = curveParameters point
    a  = curveEccA cc

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
pointDouble :: Curve curve => Point curve -> Point curve
pointDouble PointO = PointO
pointDouble point@(Point xp yp) =
    case ty of
        CurvePrime (CurvePrimeParam pr) -> fromMaybe PointO $ do
            lambda <- divmod (3 * xp ^ (2::Int) + a) (2 * yp) pr
            let xr = (lambda ^ (2::Int) - 2 * xp) `mod` pr
                yr = (lambda * (xp - xr) - yp) `mod` pr
            return $ Point xr yr
        CurveBinary (CurveBinaryParam fx)
            | xp == 0    -> PointO
            | otherwise  -> fromMaybe PointO $ do
                s <- return . addF2m xp =<< divF2m fx yp xp
                let xr = mulF2m fx s s `addF2m` s `addF2m` a
                    yr = mulF2m fx xp xp `addF2m` mulF2m fx xr (s `addF2m` 1)
                return $ Point xr yr
  where
    ty = curveType point
    cc = curveParameters point
    a  = curveEccA cc

-- | Elliptic curve point multiplication using the base
--
-- /WARNING:/ Vulnerable to timing attacks.
pointBaseMul :: Curve curve => Scalar curve -> Point curve
pointBaseMul n = pointMul n (curveEccG $ curveParameters (Proxy :: Proxy curve))

-- | Elliptic curve point multiplication (double and add algorithm).
--
-- /WARNING:/ Vulnerable to timing attacks.
pointMul :: Curve curve => Scalar curve -> Point curve -> Point curve
pointMul _ PointO = PointO
pointMul (Scalar n) p
    | n == 0    = PointO
    | n == 1    = p
    | odd n     = pointAdd p (pointMul (Scalar (n - 1)) p)
    | otherwise = pointMul (Scalar (n `div` 2)) (pointDouble p)

-- | Elliptic curve double-scalar multiplication (uses Shamir's trick).
--
-- > pointAddTwoMuls n1 p1 n2 p2 == pointAdd (pointMul n1 p1)
-- >                                         (pointMul n2 p2)
--
-- /WARNING:/ Vulnerable to timing attacks.
pointAddTwoMuls :: Curve curve => Scalar curve -> Point curve -> Scalar curve -> Point curve -> Point curve
pointAddTwoMuls _  PointO _  PointO = PointO
pointAddTwoMuls _  PointO n2 p2     = pointMul n2 p2
pointAddTwoMuls n1 p1     _  PointO = pointMul n1 p1
pointAddTwoMuls (Scalar n1) p1 (Scalar n2) p2 = go (n1, n2)
  where
    p0 = pointAdd p1 p2

    go (0,  0 ) = PointO
    go (k1, k2) =
        let q = pointDouble $ go (k1 `div` 2, k2 `div` 2)
        in case (odd k1, odd k2) of
            (True  , True  ) -> pointAdd p0 q
            (True  , False ) -> pointAdd p1 q
            (False , True  ) -> pointAdd p2 q
            (False , False ) -> q

-- | Check if a point is the point at infinity.
isPointAtInfinity :: Point curve -> Bool
isPointAtInfinity PointO = True
isPointAtInfinity _      = False

-- | Make a point on a curve from integer (x,y) coordinate
--
-- if the point is not valid related to the curve then an error is
-- returned instead of a point
pointFromIntegers :: forall curve . Curve curve => (Integer, Integer) -> CryptoFailable (Point curve)
pointFromIntegers (x,y)
    | isPointValid (Proxy :: Proxy curve) x y = CryptoPassed $ Point x y
    | otherwise                               = CryptoFailed $ CryptoError_PointCoordinatesInvalid

-- | check if a point is on specific curve
--
-- This perform three checks:
--
-- * x is not out of range
-- * y is not out of range
-- * the equation @y^2 = x^3 + a*x + b (mod p)@ holds
isPointValid :: Curve curve => proxy curve -> Integer -> Integer -> Bool
isPointValid proxy x y =
    case ty of
        CurvePrime (CurvePrimeParam p) ->
            let a  = curveEccA cc
                b  = curveEccB cc
                eqModP z1 z2 = (z1 `mod` p) == (z2 `mod` p)
                isValid e = e >= 0 && e < p
             in isValid x && isValid y && (y ^ (2 :: Int)) `eqModP` (x ^ (3 :: Int) + a * x + b)
        CurveBinary (CurveBinaryParam fx) ->
            let a  = curveEccA cc
                b  = curveEccB cc
                add = addF2m
                mul = mulF2m fx
                isValid e = modF2m fx e == e
             in and [ isValid x
                    , isValid y
                    , ((((x `add` a) `mul` x `add` y) `mul` x) `add` b `add` (squareF2m fx y)) == 0
                    ]
  where
    ty = curveType proxy
    cc = curveParameters proxy

-- | div and mod
divmod :: Integer -> Integer -> Integer -> Maybe Integer
divmod y x m = do
    i <- inverse (x `mod` m) m
    return $ y * i `mod` m
