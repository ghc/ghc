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
import Prel		( (.), (&&), (||), (^), atan2, otherwise )
import PS		( _PackedString, _unpackPS )
import Text
import TyArray
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
    abs z		=  magnitude z :+ __i0
    signum 0		=  __i0
    signum z@(x:+y)	=  x/r :+ y/r  where { r = magnitude z }
    fromInteger n	=  fromInteger n :+ __i0
    fromInt n		=  fromInt n :+ __i0

instance (RealFloat a) => Fractional (Complex a) where
    (x:+y) / (x2:+y2)	=  (x*x3+y*y3) / d :+ (y*x3-x*y3) / d
			  where  x3 = scaleFloat k x2
				 y3 = scaleFloat k y2
				 k  = - max (exponent x2) (exponent y2)
				 d  = x2*x3 + y2*y3
    fromRational a	=  fromRational a :+ __i0
    recip a		=  __i1 / a

instance (RealFloat a) => Floating (Complex a) where
    pi             =  pi :+ __i0
    exp (x:+y)     =  expx * cos y :+ expx * sin y
                      where expx = exp x
    log z          =  log (magnitude z) :+ phase z

    (**) a b       =  exp (log a * b)
    logBase a b    =  log b / log a

    sqrt z@(x:+y)  | z == __i0
	           =  __i0
		   | otherwise
		   =  u :+ (if y < __i0 then -v else v)
                      where (u,v) = if x < __i0 then (v2,u2) else (u2,v2)
                            v2    = abs y / (u2 * __i2)
                            u2    = sqrt ((magnitude z + abs x) / __i2)

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
                      where (x2:+y2) = log (((-y):+x) + sqrt (__i1 - z*z))
    acos z@(x:+y)  =  y3:+(-x3)
                      where (x3:+y3) = log (z + ((-y2):+x2))
                            (x2:+y2)   = sqrt (__i1 - z*z)
    atan z@(x:+y)  =  y2:+(-x2)
                      where (x2:+y2) = log (((__i1 - y):+x) / sqrt (__i1 + z*z))

    asinh z        =  log (z + sqrt (__i1 + z*z))
    acosh z        =  log (z + (z + __i1) * sqrt ((z - __i1)/(z + __i1)))
    atanh z        =  log ((__i1 + z) / sqrt (__i1 - z*z))


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

    readList	= _readList (readsPrec 0)
    showList	= _showList (showsPrec 0) 

{-# SPECIALIZE instance Eq	    (Complex Double) #-}
{-# SPECIALIZE instance Num	    (Complex Double) #-}
{-# SPECIALIZE instance Fractional  (Complex Double) #-}
{-# SPECIALIZE instance Floating    (Complex Double) #-}
{-# SPECIALIZE instance Text	    (Complex Double) #-}

#if defined(__UNBOXED_INSTANCES__)

{-# SPECIALIZE instance Eq	    (Complex Double#) #-}
{-# SPECIALIZE instance Num	    (Complex Double#) #-}
{-# SPECIALIZE instance Fractional  (Complex Double#) #-}
{-# SPECIALIZE instance Floating    (Complex Double#) #-}
{-# SPECIALIZE instance Text	    (Complex Double#) #-}

#endif

-- ToDo: something for Binary

-- ToDo: Complex Double#  s/a{/a{Double#,?/

{-# GENERATE_SPECS realPart a{Double#} #-}
realPart 	 :: Complex a -> a
realPart (x:+y)	 =  x

{-# GENERATE_SPECS imagPart a{Double#} #-}
imagPart 	 :: Complex a -> a
imagPart (x:+y)	 =  y

{-# GENERATE_SPECS conjugate a{Double#,Double} #-}
conjugate	 :: (RealFloat a) => Complex a -> Complex a
conjugate (x:+y) =  x :+ (-y)

{-# GENERATE_SPECS mkPolar a{Double#,Double} #-}
mkPolar		 :: (RealFloat a) => a -> a -> Complex a
mkPolar r theta	 =  r * cos theta :+ r * sin theta

{-# GENERATE_SPECS cis a{Double#,Double} #-}
cis		 :: (RealFloat a) => a -> Complex a
cis theta	 =  cos theta :+ sin theta

{-# GENERATE_SPECS polar a{Double#,Double} #-}
polar		 :: (RealFloat a) => Complex a -> (a,a)
polar z		 =  (magnitude z, phase z)

{-# GENERATE_SPECS magnitude a{Double#,Double} #-}
magnitude :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
		      (sqrt ((scaleFloat mk x)^2 + (scaleFloat mk y)^2))
		    where k  = max (exponent x) (exponent y)
		          mk = - k

{-# GENERATE_SPECS phase a{Double#,Double} #-}
phase :: (RealFloat a) => Complex a -> a
phase (x:+y) =  atan2 y x
