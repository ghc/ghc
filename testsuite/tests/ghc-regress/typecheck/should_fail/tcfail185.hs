-- See Trac #1606

module ShouldFail where

f :: Int -> Int -> Bool -> Bool -> Int -> Int
f a b = \ x y -> let { y1 = y; y2 = y1; y3 = y2; y4 = y3; y5 = y4;
		       y6 = y5; y7 = y6 } in x






