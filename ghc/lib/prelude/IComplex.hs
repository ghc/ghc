-- Complex Numbers

module PreludeComplex where

import Cls
import Core

import IDouble	-- instances
import IChar
import IFloat
import IInt
import IInteger
import IList
import List		( (++), foldr )
import Prel		( (.), (&&), (||), (^), atan2 )
import PS		( _PackedString, _unpackPS )
import Text
import TyComplex

-- infix  6  :+

-- data  (RealFloat a)     => Complex a = a :+ a  deriving (Eq,Binary,Text)

instance (Eq a) => Eq (Complex a) where
    (x :+ y) == (x2 :+ y2) = x == x2 && y == y2
    (x :+ y) /= (x2 :+ y2) = x /= x2 || y /= y2

instance (RealFloat a) => Num (Complex a) where
    (x:+y) + (x2:+y2)	=  (x+x2) :+ (y+y2)
    (x:+y) - (x2:+y2)	=  (x-x2) :+ (y-y2)
    (x:+y) * (x2:+y2)	=  (x*x2-y*y2) :+ (x*y2+y*x2)
    negate (x:+y)	=  negate x :+ negate y
    abs z		=  magnitude z :+ 0
    signum 0		=  0
    signum z@(x:+y)	=  x/r :+ y/r  where { r = magnitude z }
    fromInteger n	=  fromInteger n :+ 0
    fromInt n		=  fromInt n :+ 0

instance (RealFloat a) => Fractional (Complex a) where
    (x:+y) / (x2:+y2)	=  (x*x3+y*y3) / d :+ (y*x3-x*y3) / d
			  where  x3 = scaleFloat k x2
				 y3 = scaleFloat k y2
				 k  = - max (exponent x2) (exponent y2)
				 d  = x2*x3 + y2*y3
    fromRational a	=  fromRational a :+ 0
    recip a		=  (1 :+ 0) / a

instance (RealFloat a) => Floating (Complex a) where
    pi             =  pi :+ 0
    exp (x:+y)     =  expx * cos y :+ expx * sin y
                      where expx = exp x
    log z          =  log (magnitude z) :+ phase z

    (**) a b       =  exp (log a * b)
    logBase a b    =  log b / log a

    sqrt 0         =  0
    sqrt z@(x:+y)  =  u :+ (if y < 0 then -v else v)
                      where (u,v) = if x < 0 then (v2,u2) else (u2,v2)
                            v2    = abs y / (u2*2)
                            u2    = sqrt ((magnitude z + abs x) / 2)

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

    asin z@(x:+y)  =  y2:+(-x2)
                      where (x2:+y2) = log (((-y):+x) + sqrt (1 - z*z))
    acos z@(x:+y)  =  y3:+(-x3)
                      where (x3:+y3) = log (z + ((-y2):+x2))
                            (x2:+y2)   = sqrt (1 - z*z)
    atan z@(x:+y)  =  y2:+(-x2)
                      where (x2:+y2) = log (((1-y):+x) / sqrt (1+z*z))

    asinh z        =  log (z + sqrt (1+z*z))
    acosh z        =  log (z + (z+1) * sqrt ((z-1)/(z+1)))
    atanh z        =  log ((1+z) / sqrt (1-z*z))


instance (Text a) => Text (Complex a) where

    -- magic fixity wired in: infix 6 :+

    readsPrec p
      = readParen ( p > 6 )
	  (\ r -> [ (x :+ y, s2) | (x,    s0) <- readsPrec 7 r,
				   (":+", s1) <- lex s0,
				   (y,    s2) <- readsPrec 7 s1 ])
    showsPrec d (a :+ b)
      = showParen (d > 6)
	  (showsPrec 7 a . showString " :+ " . showsPrec 7 b)

{-# SPECIALIZE instance Eq	    (Complex Double) #-}
{-# SPECIALIZE instance Num	    (Complex Double) #-}
{-# SPECIALIZE instance Fractional  (Complex Double) #-}
{-# SPECIALIZE instance Floating    (Complex Double) #-}

--NO:{-# SPECIALIZE instance Eq	    (Complex Float) #-}
--NO:{-# SPECIALIZE instance Num	    (Complex Float) #-}
--NO:{-# SPECIALIZE instance Fractional  (Complex Float) #-}
--NO:{-# SPECIALIZE instance Floating    (Complex Float) #-}
  
#if defined(__UNBOXED_INSTANCES__)

{-# SPECIALIZE instance Eq	    (Complex Double#) #-}
{-# SPECIALIZE instance Num	    (Complex Double#) #-}
{-# SPECIALIZE instance Fractional  (Complex Double#) #-}
{-# SPECIALIZE instance Floating    (Complex Double#) #-}
{-# SPECIALIZE instance Text	    (Complex Double#) #-}

#endif

-- ToDo: something for Binary

-- ToDo: Complex Double#  s/a{/a{Double#,?/

--{-# GENERATE_SPECS realPart a{Double#} #-}
realPart 	 :: Complex a -> a
realPart (x:+y)	 =  x

--{-# GENERATE_SPECS imagPart a{Double#} #-}
imagPart 	 :: Complex a -> a
imagPart (x:+y)	 =  y

--{-# GENERATE_SPECS conjugate a{Double#,Double} #-}
{-# GENERATE_SPECS conjugate a{Double} #-}
conjugate	 :: (RealFloat a) => Complex a -> Complex a
conjugate (x:+y) =  x :+ (-y)

--{-# GENERATE_SPECS mkPolar a{Double#,Double} #-}
{-# GENERATE_SPECS mkPolar a{Double} #-}
mkPolar		 :: (RealFloat a) => a -> a -> Complex a
mkPolar r theta	 =  r * cos theta :+ r * sin theta

--{-# GENERATE_SPECS cis a{Double#,Double} #-}
{-# GENERATE_SPECS cis a{Double} #-}
cis		 :: (RealFloat a) => a -> Complex a
cis theta	 =  cos theta :+ sin theta

--{-# GENERATE_SPECS polar a{Double#,Double} #-}
{-# GENERATE_SPECS polar a{Double} #-}
polar		 :: (RealFloat a) => Complex a -> (a,a)
polar z		 =  (magnitude z, phase z)

--{-# GENERATE_SPECS magnitude a{Double#,Double} #-}
{-# GENERATE_SPECS magnitude a{Double} #-}
magnitude :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
		     (sqrt ((scaleFloat mk x)^2 + (scaleFloat mk y)^2))
		    where k  = max (exponent x) (exponent y)
		          mk = - k

--{-# GENERATE_SPECS phase a{Double#,Double} #-}
{-# GENERATE_SPECS phase a{Double} #-}
phase :: (RealFloat a) => Complex a -> a
phase (x:+y) =  atan2 y x
