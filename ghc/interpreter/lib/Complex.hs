-----------------------------------------------------------------------------
-- Standard Library: Complex numbers
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module Complex(Complex((:+)), realPart, imagPart, conjugate, mkPolar,
               cis, polar, magnitude, phase)  where

infix  6  :+

data (RealFloat a) => Complex a = !a :+ !a 
                      deriving (Eq,Read,Show)

realPart, imagPart :: (RealFloat a) => Complex a -> a
realPart (x:+y)     = x
imagPart (x:+y)     = y

conjugate          :: (RealFloat a) => Complex a -> Complex a
conjugate (x:+y)    = x :+ (-y)

mkPolar            :: (RealFloat a) => a -> a -> Complex a
mkPolar r theta     = r * cos theta :+ r * sin theta

cis                :: (RealFloat a) => a -> Complex a
cis theta           = cos theta :+ sin theta

polar              :: (RealFloat a) => Complex a -> (a,a)
polar z             = (magnitude z, phase z)

magnitude, phase   :: (RealFloat a) => Complex a -> a
magnitude (x:+y)    = scaleFloat k
                       (sqrt ((scaleFloat mk x)^2 + (scaleFloat mk y)^2))
                      where k  = max (exponent x) (exponent y)
                            mk = - k
phase (0:+0)        = 0
phase (x:+y)        = atan2 y x

instance (RealFloat a) => Num (Complex a) where
    (x:+y) + (x':+y')  = (x+x') :+ (y+y')
    (x:+y) - (x':+y')  = (x-x') :+ (y-y')
    (x:+y) * (x':+y')  = (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y)      = negate x :+ negate y
    abs z              = magnitude z :+ 0
    signum 0           = 0
    signum z@(x:+y)    = x/r :+ y/r where r = magnitude z
    fromInteger n      = fromInteger n :+ 0
    fromInt n          = fromInt n :+ 0

instance (RealFloat a) => Fractional (Complex a) where
    (x:+y) / (x':+y')  = (x*x''+y*y'') / d :+ (y*x''-x*y'') / d
			 where x'' = scaleFloat k x'
			       y'' = scaleFloat k y'
			       k   = - max (exponent x') (exponent y')
			       d   = x'*x'' + y'*y''
    fromRational a     = fromRational a :+ 0
    fromDouble a       = fromDouble a :+ 0

instance (RealFloat a) => Floating (Complex a) where
    pi            = pi :+ 0
    exp (x:+y)    = expx * cos y :+ expx * sin y
		    where expx = exp x
    log z         = log (magnitude z) :+ phase z
    sqrt 0        = 0
    sqrt z@(x:+y) = u :+ (if y < 0 then -v else v)
		    where (u,v) = if x < 0 then (v',u') else (u',v')
			  v'    = abs y / (u'*2)
			  u'    = sqrt ((magnitude z + abs x) / 2)
    sin (x:+y)    = sin x * cosh y :+ cos x * sinh y
    cos (x:+y)    = cos x * cosh y :+ (- sin x * sinh y)
    tan (x:+y)    = (sinx*coshy:+cosx*sinhy)/(cosx*coshy:+(-sinx*sinhy))
		    where sinx  = sin x
			  cosx	= cos x
			  sinhy = sinh y
			  coshy = cosh y
    sinh (x:+y)   = cos y * sinh x :+ sin  y * cosh x
    cosh (x:+y)   = cos y * cosh x :+ sin y * sinh x
    tanh (x:+y)   = (cosy*sinhx:+siny*coshx)/(cosy*coshx:+siny*sinhx)
		    where siny  = sin y
			  cosy	= cos y
			  sinhx = sinh x
			  coshx = cosh x
    asin z@(x:+y) =  y':+(-x')
                     where  (x':+y') = log (((-y):+x) + sqrt (1 - z*z))
    acos z@(x:+y) =  y'':+(-x'')
                     where (x'':+y'') = log (z + ((-y'):+x'))
                           (x':+y')   = sqrt (1 - z*z)
    atan z@(x:+y) =  y':+(-x')
                     where (x':+y') = log (((1-y):+x) / sqrt (1+z*z))
    asinh z       = log (z + sqrt (1+z*z))
    acosh z       = log (z + (z+1) * sqrt ((z-1)/(z+1)))
    atanh z       = log ((1+z) / sqrt (1-z*z))

-----------------------------------------------------------------------------
