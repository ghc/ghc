-- !!! conversions: Double <=> Rational/Integer things
--
import Data.Ratio

main = putStr (show r42  ++ "\n" ++
	       show nu42 ++ ", " ++
	       show de42 ++ "\n" ++
	       show nu42d ++ ", " ++
	       show de42d ++ "\n" ++
	       show s2 ++ ", " ++
	       show e2 ++ "\n" ++
	       show s ++ ", " ++
	       show e ++ "\n" )
  where  
    d42 :: Double
    r42 :: Rational
    nu42, de42 :: Integer
    nu42d, de42d :: Double

    d42  = 42
    r42  = toRational d42
    nu42 = numerator   r42
    de42 = denominator r42
    nu42d= fromInteger nu42
    de42d= fromInteger de42

    (s,e)= decodeFloat (nu42d / de42d )   
    (s2,e2) = decodeFloat d42
