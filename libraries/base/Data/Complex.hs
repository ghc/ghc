{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Complex
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Complex numbers.
--
-----------------------------------------------------------------------------

module Data.Complex
        (
        -- * Rectangular form
          Complex((:+))

        , realPart
        , imagPart
        -- * Polar form
        , mkPolar
        , cis
        , polar
        , magnitude
        , phase
        -- * Conjugate
        , conjugate

        )  where

import Prelude hiding (Applicative(..))
import GHC.Base (Applicative (..))
import GHC.Generics (Generic, Generic1)
import GHC.Float (Floating(..))
import Data.Data (Data)
import Foreign (Storable, castPtr, peek, poke, pokeElemOff, peekElemOff, sizeOf,
                alignment)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))

infix  6  :+

-- -----------------------------------------------------------------------------
-- The Complex type

-- | A data type representing complex numbers.
--
-- You can read about complex numbers [on wikipedia](https://en.wikipedia.org/wiki/Complex_number).
--
-- In haskell, complex numbers are represented as @a :+ b@ which can be thought of
-- as representing \(a + bi\). For a complex number @z@, @'abs' z@ is a number with the 'magnitude' of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@
-- has the 'phase' of @z@, but unit 'magnitude'.
-- Apart from the loss of precision due to IEEE754 floating point numbers,
-- it holds that @z == 'abs' z * 'signum' z@.
--
-- Note that `Complex`'s instances inherit the deficiencies from the type
-- parameter's. For example, @Complex Float@'s 'Ord' instance has similar
-- problems to `Float`'s.
--
-- As can be seen in the examples, the 'Foldable'
-- and 'Traversable' instances traverse the real part first.
--
-- ==== __Examples__
--
-- >>> (5.0 :+ 2.5) + 6.5
-- 11.5 :+ 2.5
--
-- >>> abs (1.0 :+ 1.0) - sqrt 2.0
-- 0.0 :+ 0.0
--
-- >>> abs (signum (4.0 :+ 3.0))
-- 1.0 :+ 0.0
--
-- >>> foldr (:) [] (1 :+ 2)
-- [1,2]
--
-- >>> mapM print (1 :+ 2)
-- 1
-- 2
data Complex a
  = !a :+ !a    -- ^ forms a complex number from its real and imaginary
                -- rectangular components.
        deriving ( Eq          -- ^ @since 2.01
                 , Show        -- ^ @since 2.01
                 , Read        -- ^ @since 2.01
                 , Data        -- ^ @since 2.01
                 , Generic     -- ^ @since 4.9.0.0
                 , Generic1    -- ^ @since 4.9.0.0
                 , Functor     -- ^ @since 4.9.0.0
                 , Foldable    -- ^ @since 4.9.0.0
                 , Traversable -- ^ @since 4.9.0.0
                 )

-- -----------------------------------------------------------------------------
-- Functions over Complex

-- | Extracts the real part of a complex number.
--
-- ==== __Examples__
--
-- >>> realPart (5.0 :+ 3.0)
-- 5.0
--
-- >>> realPart ((5.0 :+ 3.0) * (2.0 :+ 3.0))
-- 1.0
realPart :: Complex a -> a
realPart (x :+ _) =  x

-- | Extracts the imaginary part of a complex number.
--
-- ==== __Examples__
--
-- >>> imagPart (5.0 :+ 3.0)
-- 3.0
--
-- >>> imagPart ((5.0 :+ 3.0) * (2.0 :+ 3.0))
-- 21.0
imagPart :: Complex a -> a
imagPart (_ :+ y) =  y

-- | The 'conjugate' of a complex number.
--
-- prop> conjugate (conjugate x) = x
--
-- ==== __Examples__
--
-- >>> conjugate (3.0 :+ 3.0)
-- 3.0 :+ (-3.0)
--
-- >>> conjugate ((3.0 :+ 3.0) * (2.0 :+ 2.0))
-- 0.0 :+ (-12.0)
{-# SPECIALISE conjugate :: Complex Double -> Complex Double #-}
conjugate        :: Num a => Complex a -> Complex a
conjugate (x:+y) =  x :+ (-y)

-- | Form a complex number from 'polar' components of 'magnitude' and 'phase'.
--
-- ==== __Examples__
--
-- >>> mkPolar 1 (pi / 4)
-- 0.7071067811865476 :+ 0.7071067811865475
--
-- >>> mkPolar 1 0
-- 1.0 :+ 0.0
{-# SPECIALISE mkPolar :: Double -> Double -> Complex Double #-}
mkPolar          :: Floating a => a -> a -> Complex a
mkPolar r theta  =  r * cos theta :+ r * sin theta

-- | @'cis' t@ is a complex value with 'magnitude' @1@
-- and 'phase' @t@ (modulo @2*'pi'@).
--
-- @
-- 'cis' = 'mkPolar' 1
-- @
--
-- ==== __Examples__
--
-- >>> cis 0
-- 1.0 :+ 0.0
--
-- The following examples are not perfectly zero due to [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754)
--
-- >>> cis pi
-- (-1.0) :+ 1.2246467991473532e-16
--
-- >>> cis (4 * pi) - cis (2 * pi)
-- 0.0 :+ (-2.4492935982947064e-16)
{-# SPECIALISE cis :: Double -> Complex Double #-}
cis              :: Floating a => a -> Complex a
cis theta        =  cos theta :+ sin theta

-- | The function 'polar' takes a complex number and
-- returns a ('magnitude', 'phase') pair in canonical form:
-- the 'magnitude' is non-negative, and the 'phase' in the range @(-'pi', 'pi']@;
-- if the 'magnitude' is zero, then so is the 'phase'.
--
-- @'polar' z = ('magnitude' z, 'phase' z)@
--
-- ==== __Examples__
--
-- >>> polar (1.0 :+ 1.0)
-- (1.4142135623730951,0.7853981633974483)
--
-- >>> polar ((-1.0) :+ 0.0)
-- (1.0,3.141592653589793)
--
-- >>> polar (0.0 :+ 0.0)
-- (0.0,0.0)
{-# SPECIALISE polar :: Complex Double -> (Double,Double) #-}
polar            :: (RealFloat a) => Complex a -> (a,a)
polar z          =  (magnitude z, phase z)

-- | The non-negative 'magnitude' of a complex number.
--
-- ==== __Examples__
--
-- >>> magnitude (1.0 :+ 1.0)
-- 1.4142135623730951
--
-- >>> magnitude (1.0 + 0.0)
-- 1.0
--
-- >>> magnitude (0.0 :+ (-5.0))
-- 5.0
{-# SPECIALISE magnitude :: Complex Double -> Double #-}
magnitude :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
                     (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)))
                    where k  = max (exponent x) (exponent y)
                          mk = - k
                          sqr z = z * z

-- | The 'phase' of a complex number, in the range @(-'pi', 'pi']@.
-- If the 'magnitude' is zero, then so is the 'phase'.
--
-- ==== __Examples__
--
-- >>> phase (0.5 :+ 0.5) / pi
-- 0.25
--
-- >>> phase (0 :+ 4) / pi
-- 0.5
{-# SPECIALISE phase :: Complex Double -> Double #-}
phase :: (RealFloat a) => Complex a -> a
phase (0 :+ 0)   = 0            -- SLPJ July 97 from John Peterson
phase (x:+y)     = atan2 y x


-- -----------------------------------------------------------------------------
-- Instances of Complex

-- | @since 2.01
instance  (RealFloat a) => Num (Complex a)  where
    {-# SPECIALISE instance Num (Complex Float) #-}
    {-# SPECIALISE instance Num (Complex Double) #-}
    (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
    (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
    (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y)       =  negate x :+ negate y
    abs z               =  magnitude z :+ 0
    signum (0:+0)       =  0
    signum z@(x:+y)     =  x/r :+ y/r  where r = magnitude z
    fromInteger n       =  fromInteger n :+ 0

-- | @since 2.01
instance  (RealFloat a) => Fractional (Complex a)  where
    {-# SPECIALISE instance Fractional (Complex Float) #-}
    {-# SPECIALISE instance Fractional (Complex Double) #-}
    (x:+y) / (x':+y')   =  (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
                           where x'' = scaleFloat k x'
                                 y'' = scaleFloat k y'
                                 k   = - max (exponent x') (exponent y')
                                 d   = x'*x'' + y'*y''

    fromRational a      =  fromRational a :+ 0

-- | @since 2.01
instance  (RealFloat a) => Floating (Complex a) where
    {-# SPECIALISE instance Floating (Complex Float) #-}
    {-# SPECIALISE instance Floating (Complex Double) #-}
    pi             =  pi :+ 0
    exp (x:+y)     =  expx * cos y :+ expx * sin y
                      where expx = exp x
    log z          =  log (magnitude z) :+ phase z

    x ** y = case (x,y) of
      (_ , (0:+0))  -> 1 :+ 0
      ((0:+0), (exp_re:+_)) -> case compare exp_re 0 of
                 GT -> 0 :+ 0
                 LT -> inf :+ 0
                 EQ -> nan :+ nan
      ((re:+im), (exp_re:+_))
        | (isInfinite re || isInfinite im) -> case compare exp_re 0 of
                 GT -> inf :+ 0
                 LT -> 0 :+ 0
                 EQ -> nan :+ nan
        | otherwise -> exp (log x * y)
      where
        inf = 1/0
        nan = 0/0

    sqrt (0:+0)    =  0
    sqrt z@(x:+y)  =  u :+ (if y < 0 then -v else v)
                      where (u,v) = if x < 0 then (v',u') else (u',v')
                            v'    = abs y / (u'*2)
                            u'    = sqrt ((magnitude z + abs x) / 2)

    sin (x:+y)     =  sin x * cosh y :+ cos x * sinh y
    cos (x:+y)     =  cos x * cosh y :+ (- sin x * sinh y)
    tan (x:+y)     =  (sinx*coshy:+cosx*sinhy)/(cosx*coshy:+(-sinx*sinhy))
                      where sinx  = sin x
                            cosx  = cos x
                            sinhy = sinh y
                            coshy = cosh y

    sinh (x:+y)    =  cos y * sinh x :+ sin  y * cosh x
    cosh (x:+y)    =  cos y * cosh x :+ sin y * sinh x
    tanh (x:+y)    =  (cosy*sinhx:+siny*coshx)/(cosy*coshx:+siny*sinhx)
                      where siny  = sin y
                            cosy  = cos y
                            sinhx = sinh x
                            coshx = cosh x

    asin z@(x:+y)  =  y':+(-x')
                      where  (x':+y') = log (((-y):+x) + sqrt (1 - z*z))
    acos z         =  y'':+(-x'')
                      where (x'':+y'') = log (z + ((-y'):+x'))
                            (x':+y')   = sqrt (1 - z*z)
    atan z@(x:+y)  =  y':+(-x')
                      where (x':+y') = log (((1-y):+x) / sqrt (1+z*z))

    asinh z        =  log (z + sqrt (1+z*z))
    -- Take care to allow (-1)::Complex, fixing #8532
    acosh z        =  log (z + (sqrt $ z+1) * (sqrt $ z-1))
    atanh z        =  0.5 * log ((1.0+z) / (1.0-z))

    log1p x@(a :+ b)
      | abs a < 0.5 && abs b < 0.5
      , u <- 2*a + a*a + b*b = log1p (u/(1 + sqrt(u+1))) :+ atan2 (1 + a) b
      | otherwise = log (1 + x)
    {-# INLINE log1p #-}

    expm1 x@(a :+ b)
      | a*a + b*b < 1
      , u <- expm1 a
      , v <- sin (b/2)
      , w <- -2*v*v = (u*w + u + w) :+ (u+1)*sin b
      | otherwise = exp x - 1
    {-# INLINE expm1 #-}

-- | @since 4.8.0.0
instance Storable a => Storable (Complex a) where
    sizeOf a       = 2 * sizeOf (realPart a)
    alignment a    = alignment (realPart a)
    peek p           = do
                        q <- return $ castPtr p
                        r <- peek q
                        i <- peekElemOff q 1
                        return (r :+ i)
    poke p (r :+ i)  = do
                        q <-return $  (castPtr p)
                        poke q r
                        pokeElemOff q 1 i

-- | @since 4.9.0.0
instance Applicative Complex where
  pure a = a :+ a
  f :+ g <*> a :+ b = f a :+ g b
  liftA2 f (x :+ y) (a :+ b) = f x a :+ f y b

-- | @since 4.9.0.0
instance Monad Complex where
  a :+ b >>= f = realPart (f a) :+ imagPart (f b)

-- | @since 4.15.0.0
instance MonadZip Complex where
  mzipWith = liftA2

-- | @since 4.15.0.0
instance MonadFix Complex where
  mfix f = (let a :+ _ = f a in a) :+ (let _ :+ a = f a in a)

-- -----------------------------------------------------------------------------
-- Rules on Complex

{-# RULES

"realToFrac/a->Complex Double"
  realToFrac = \x -> realToFrac x :+ (0 :: Double)

"realToFrac/a->Complex Float"
  realToFrac = \x -> realToFrac x :+ (0 :: Float)

  #-}
