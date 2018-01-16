module Memo(Triangle,mkmemo,lazyAbove) where
import Numbers
import Vectors
import EdgePlate
import Comparing(above)
import Data.Ix
import Data.Array

data Triangle a = a :^ a deriving (Eq,Ord, {-1.3-}Show)

instance (Enum a,Ord a,Ix a) => Ix (Triangle a) where
	range (t0 :^ b0 , t1 :^ b1) =
		[t :^ b | t <- [t0 .. t1]
			, b <- take (1+index(t0,t1) t) [b0 .. ]
			, t :^ b <= t1 :^ b1
		]
	index (t0 :^ b0 , t1 :^ b1) (t :^ b) =
		ti * (ti+1) `div` 2 + index (b0,b1) b
		where ti = index (t0,t1) t
	inRange (t0 :^ b0 , t1 :^ b1) (t :^ b) =
		inRange (t0,t1) t &&
		inRange (b0,b1) b && index (t0,t1) t >= index (b0,b1) b

mkmemo :: (Plate -> Plate -> a) -> Object -> Array (Triangle Int) a
mkmemo f obj =
	array (2:^1 , len:^(len-1)) [((top:^bottom) , f ls ks)
				    |ls@(Plt top _) <- obj
				    ,ks@(Plt bottom _) <- obj
				    ,top > bottom]
	where len = length obj


lazyAbove :: Array (Triangle Int) Bool -> Plate -> Plate -> Bool
lazyAbove memory top@(Plt n _) bottom@(Plt m _)
	| inRange (bounds memory) (n :^ m) = memory ! (n:^m)
	| n==m				   = False
	| otherwise			   = if memory ! (m:^n)
					     then False
					     else top`above`bottom
