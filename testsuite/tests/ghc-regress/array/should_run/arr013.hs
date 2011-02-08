import Data.Ratio -- 1.3
import Data.Array -- 1.3
infix 1 =:
(=:) a b = (a,b)

main = putStr (shows sub_b "\n")
    where
	sub_b :: Array Int Double
	sub_b = ixmap (102, 113) id b

	b :: Array Int Double
    	b = fmap ( \ r -> fromRational r / pi )
		 (ixmap (101,200) (\ i -> toInteger i - 100) a)

	a :: Array Integer (Ratio Integer)
	a = array (1,100) ((1 =: 1) : [i =: fromInteger i * a!(i-1)
					| i <- [2..100]])
