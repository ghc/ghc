BpGen.hs
Written by Sam Waugh
Date started : 9th November 1992 

This module implements backprop using pattern presentation style, 
allowing for a general number of layers.  No sigmoid on last layer.
+ 0.1 to sigmoid derivative.  It does not implement momentum.

Need to use modules for matrix and vector operations.

> module BpGen {-partain:(Dimensions(..),
>		Layer(..),  Layers(..),
>		Eg(..),     Egs(..),
>		Weight(..), Weights(..),
>		maxplace, classeg, calcerror, selectegs,
>		trainweights, randweights)-} where

> import {-fool mkdependHS-}
>	 Random
> import List(transpose)
> infixl 7 $$

-------------------------------------------------------------------------------
|				Data Types				      |
-------------------------------------------------------------------------------

> type Dimensions = [Int]	  -- for network topology
> type Layer	  = [Double]	  -- vector for layers (incl. input and output)
> type Layers	  = [Layer]
> type Weight	  = [[Double]]	  -- connections between layers
> type Weights	  = [Weight]
> type Eg	  = (Layer,Layer) -- attributes and classes
> type Egs	  = [Eg]


-------------------------------------------------------------------------------
|				Utility functions			      |
-------------------------------------------------------------------------------

Maxplace finds the position of the maximum element in a list.
sublist subtracts two vectors, $$ performs across vector multiplication
weivecmult multiplies a matrix and a vector
classeg takes the weights of a network and an input vector, and produces
a list of the Layers of the network after classification
calcerror calculates the root mean squared error of the data set
Also implemented sqr and sig (Sigmoid function).

> maxplace :: (Ord a) => [a] -> Int
> maxplace xs = length (takeWhile (/=(maximum xs)) xs)

> sqr :: (Num a) => a -> a
> sqr x = x * x

> sig :: (Floating a) => a -> a
> sig x = 1.0 / (1.0 + exp (negate x))

> sublist, ($$) :: (Num a) => [a] -> [a] -> [a]
> sublist = zipWith (-)
> ($$)    = zipWith (*)

> weivecmult :: Weight -> Layer -> Layer
> weivecmult w v = [sum (wi $$ v) | wi <- w]


> classeg :: Weights -> Layer -> Layers
> classeg [] y = [y]
> classeg (w:ws) l
>  = let l' = if null ws then weivecmult w templ
>			 else map sig (weivecmult w templ)
>	 templ = if null ws then l
>			    else 1.0 : l
>    in templ : (classeg ws l')



> calcerror :: Weights -> Egs -> Double
> calcerror ws egs = sqrt (calcerror1 ws egs)

> calcerror1 :: Weights -> Egs -> Double
> calcerror1 _ []  = 0.0
> calcerror1 ws ((x,t):egs)
>    = (sum.(map sqr).(sublist t).last) (classeg ws x)
>    + calcerror1 ws egs


-------------------------------------------------------------------------------
|			Network Training Functions			      |
-------------------------------------------------------------------------------

selectegs produces a list of random numbers corresponding to the examples
to be selected during training.  (It takes the range of the examples)

> selectegs :: Int -> [Int]
> selectegs n = map (`rem` n) (randomInts n n)


trainweights calls trainepoch to iteratively train the network.  It
also checks the error at the end of each call to see if it has fallen to
a reasonable level.

> trainweights :: Egs -> Weights -> Int -> Double -> Double
>		-> [Int] -> (Weights, [Double])
> trainweights _   ws 0       _   _   _  = (ws, [])
> --should be:trainweights egs ws (eps+1) err eta rs
> trainweights egs ws eps err eta rs
>    | eps < 0 = error "BpGen.trainweights"
>    | otherwise
>    = let (ws',rs')	= trainepoch egs ws (length egs) eta rs
>	   newerr	= calcerror ws' egs
>	   (ws'', errs) = trainweights egs ws' (eps-1) err eta rs'
>      in if newerr < err then (ws',  [newerr])
>			  else (ws'', newerr:errs)


trainepoch iteratively calls classeg and backprop to train the network,
as well as selecting an example.

> trainepoch :: Egs -> Weights -> Int -> Double -> [Int] -> (Weights, [Int])
> trainepoch _   ws 0        _   rs     = (ws,rs)
> --should be: trainepoch egs ws (egno+1) eta (r:rs)
> trainepoch egs ws egno eta (r:rs)
>    | egno < 0 = error "BpGen.trainepoch"
>    | otherwise
>    = let (x,t) = egs !! r
>	   ws'	 = backprop eta (classeg ws x) ws t
>      in trainepoch egs ws' (egno-1) eta rs


backprop causes weight changes after calculating the change

> backprop :: Double -> Layers -> Weights -> Layer -> Weights
> backprop eta (o:os) (w:ws) t
>  = changeweights eta (o:os) (calcchange os ws t) (w:ws)


calcchange calculates the changes to the weights

> calcchange :: Layers -> Weights -> Layer -> Layers
> calcchange [o]    []     t = [sublist t o]
> calcchange (o:os) (w:ws) t
>    = (sigop o (weivecmult (transpose w) (head ds))) : ds
>	  where ds = calcchange os ws t


sigop performs the calculations involving the derivative of the sigmoid.
This uses a constant to eliminate flat spots [Fahlman, 1988]

> sigop :: Layer -> Layer -> Layer
> sigop out change
>    = let sig' x = x * (1.0 - x) + 0.1
>      in (map sig' out) $$ change


changeweights makes the actual changes to weights

> changeweights :: Double -> Layers -> Layers -> Weights -> Weights
> changeweights eta os ds ws
>    = [[[wji + eta * dj * oi | (oi,wji) <- zip o wj]
>			      | (dj,wj)  <- zip d w]
>			      | (w,d,o)  <- zip3 ws ds os]


-------------------------------------------------------------------------------
|				Weight Manipulation			      |
-------------------------------------------------------------------------------

randweights generates random weights in the range -1.0 to +1.0

> randweights :: Dimensions -> Weights
> randweights dimensions
>    = genweights dimensions (map (\x -> 2.0 * x - 1.0) (randomDoubles 1 1))


Generates weights, taking the values from the list of Doubles.
The weight sizes are taken from the list of dimensions.

> genweights :: Dimensions -> [Double] -> Weights
> genweights [x] _ = []
> genweights (x:y:dimensions) rs
>    = let (w, rs') = if null dimensions then multSplitAt x 	y rs
>					 else multSplitAt (x+1) y rs
>      in w : (genweights (y:dimensions) rs')


> multSplitAt :: Int -> Int -> [a] -> ([[a]],[a])
> multSplitAt inner 0 xs = ([], xs)
> --should be:multSplitAt inner (outer + 1) xs
> multSplitAt inner outer xs
>   | outer < 0 = error "BpGen.multSplitAt"
>   | otherwise
>     = let (l,  xs')  = splitAt inner xs
>	    (ls, xs'') = multSplitAt inner (outer-1) xs'
>       in (l:ls, xs'')
