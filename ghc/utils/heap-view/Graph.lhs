Started 29/11/93: 

> module Main where
> import PreludeGlaST
> import LibSystem

Program to draw a graph of last @n@ pieces of data from standard input
continuously.

> n :: Int
> n = 40

> max_sample :: Int
> max_sample = 100

> screen_size :: Int
> screen_size = 200

Version of grapher that can handle the output of ghc's @+RTS -Sstderr@
option.  

Nice variant would be to take a list of numbers from the commandline
and display several graphs at once.

> main :: IO ()
> main =
>	getArgs				>>= \ r ->
>	case r of 
>	  [select] -> 
>		let selection = read select
>		in
>		xInitialise [] screen_size screen_size	>>
> 		hGetContents stdin			>>= \ input ->
>  		graphloop2 (parseGCData selection input) [] 
>	  _ -> 
>		error "usage: graph <number in range 0..17>\n"

The format of glhc18's stderr stuff is:

-- start of example (view in 120 column window)
graph +RTS -Sstderr -H500 

Collector: APPEL  HeapSize: 500 (bytes)

  Alloc  Collect   Live   Resid   GC    GC     TOT     TOT  Page Flts   No of Roots  Caf  Mut-  Old  Collec  Resid
  bytes   bytes    bytes   ency  user  elap    user    elap   GC  MUT  Astk Bstk Reg  No  able  Gen   tion   %heap
     248     248      60  24.2%  0.00  0.04    0.05    0.23    1    1     1    0   0   1     0    0   Minor
-- end of example
     0       1      2       3      4    5      6       7       8    9    10   11  12  13    14   15      16     17

That is: 6 header lines followed by 17-18 columns of integers,
percentages, floats and text.

The scaling in the following is largely based on guesses about likely
values - needs tuned.  

@gcParsers@ is a list of functions which parse the corresponding
column and attempts to scale the numbers into the range $0.0 .. 1.0$.
(But may return a number avove $1.0$ which graphing part will scale to
fit screen...)

(Obvious optimisation - replace by list of scaling information!)

(Obvious improvement - return (x,y) pair based on elapsed (or user) time.)

> gcParsers :: [ String -> Float ]
> gcParsers = [ heap, heap, heap, percent, time, time, time, time, flts, flts, stk, stk, reg, caf, caf, heap, text, percent ]
>  where
>   heap = scale 100000.0 . fromInt . check 0 . readDec
>   stk  = scale  25000.0 . fromInt . check 0 . readDec
>   int  = scale   1000.0 . fromInt . check 0 . readDec
>   reg = scale   10.0 . fromInt . check 0 . readDec
>   caf = scale  100.0 . fromInt . check 0 . readDec
>   flts = scale  100.0 . fromInt . check 0 . readDec
>   percent = scale 100.0 . check 0.0 . readFloat
>   time   = scale  20.0 . check 0.0 . readFloat
>   text s = 0.0

> check :: a -> [(a,String)] -> a
> check error_value parses = 
>	case parses of
>	  [] 		-> error_value
>	  ((a,s):_) 	-> a

> scale :: Float -> Float -> Float
> scale max n = n / max

> parseGCData :: Int -> String -> [Float]
> parseGCData column input = 
>	map ((gcParsers !! column) . (!! column) . words) (drop 6 (lines input))

Hmmm, how to add logarithmic scaling neatly?  Do I still need to?

Note: unpleasant as it is, the code cannot be simplified to something
like the following.  The problem is that the graph won't start to be
drawn until the first @n@ values are available. (Is there also a
danger of clearing the screen while waiting for the next input value?)
A possible alternative solution is to keep count of how many values
have actually been received.

< graphloop2 :: [Float] -> [Float] -> IO ()
< graphloop2 [] =
<	return ()
< graphloop2 ys =
<	let ys' = take n ys
<	    m = maximum ys'
<	    y_scale = (floor m) + 1
<	    y_scale' = fromInt y_scale
<	in
<	xCls						>>
< 	drawScales y_scale				>>
<	draw x_coords [ x / y_scale' | x <- ys' ]	>>
<	xHandleEvent					>>
<	graphloop2 (tail ys)


> graphloop2 :: [Float] -> [Float] -> IO ()
> graphloop2 (y:ys) xs =
>	let xs' = take n (y:xs)
>	    m = maximum xs'
>	    y_scale = (floor m) + 1
>	    y_scale' = fromInt y_scale
>	in
>	xCls						>>
> 	drawScales y_scale				>>
>	draw x_coords [ x / y_scale' | x <- xs' ]	>>
>	xHandleEvent					>>
>	graphloop2 ys xs'
> graphloop2 [] xs =
>	return ()

> x_coords :: [Float]
> x_coords = [ 0.0, 1 / (fromInt n) .. ]

Draw lines specified by coordinates in range (0.0 .. 1.0) onto screen.

> draw :: [Float] -> [Float] -> IO ()
> draw xs ys = drawPoly (zip xs' (reverse ys'))
>  where
>   xs' = [ floor (x * sz) | x <- xs ]
>   ys' = [ floor ((1.0 - y) * sz) | y <- ys ]
>   sz = fromInt screen_size

> drawPoly :: [(Int, Int)] -> IO ()
> drawPoly ((x1,y1):(x2,y2):poly) =
>	xDrawLine x1 y1 x2 y2		>>
>	drawPoly ((x2,y2):poly)
> drawPoly _ = return ()

Draw horizontal line at major points on y-axis.

> drawScales :: Int -> IO ()
> drawScales y_scale =
>	sequence (map drawScale ys)	>>
>	return ()
>  where
>   ys = [ (fromInt i) / (fromInt y_scale) | i <- [1 .. y_scale - 1] ]

> drawScale :: Float -> IO ()
> drawScale y =
>	let y' = floor ((1.0 - y) * (fromInt screen_size))
>	in
>	xDrawLine 0 y' screen_size y'

>#include "common-bits"
