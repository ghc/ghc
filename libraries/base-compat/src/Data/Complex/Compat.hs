{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Complex.Compat (
  module Base
#if MIN_VERSION_base(4,4,0) && !(MIN_VERSION_base(4,8,0))
, realPart
, imagPart
, mkPolar
, cis
, conjugate
#endif
) where

#if !(MIN_VERSION_base(4,4,0)) || MIN_VERSION_base(4,8,0)
import Data.Complex as Base
#else
import Data.Complex as Base hiding (
    realPart
  , imagPart
  , mkPolar
  , cis
  , conjugate
  )
import Prelude
#endif

#if MIN_VERSION_base(4,4,0) && !(MIN_VERSION_base(4,8,0))
-- | Extracts the real part of a complex number.
realPart :: Complex a -> a
realPart (x :+ _) =  x

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex a -> a
imagPart (_ :+ y) =  y

-- | The conjugate of a complex number.
{-# SPECIALISE conjugate :: Complex Double -> Complex Double #-}
conjugate        :: Num a => Complex a -> Complex a
conjugate (x:+y) =  x :+ (-y)

-- | Form a complex number from polar components of magnitude and phase.
{-# SPECIALISE mkPolar :: Double -> Double -> Complex Double #-}
mkPolar          :: Floating a => a -> a -> Complex a
mkPolar r theta  =  r * cos theta :+ r * sin theta

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
{-# SPECIALISE cis :: Double -> Complex Double #-}
cis              :: Floating a => a -> Complex a
cis theta        =  cos theta :+ sin theta
#endif
