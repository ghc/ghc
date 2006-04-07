> module Main where
> import PreludeGlaST
> import LibSystem

> import Parse

Program to do continuous heap profile.

Bad News: 

    The ghc runtime system writes its heap profile information to a
    named file (<progname>.hp).  The program merrily reads its input
    from a named file but has no way of synchronising with the program
    generating the file.

Good News 0:

    You can save the heap profile to a file:

	    <progname> <parameters> +RTS -h -i0.1 -RTS

    and then run:

	    hpView2 <progname>.hp Main:<functionname>

    This is very like using hp2ps but much more exciting because you
    never know what's going to happen next :-)


Good News 1:

    The prophet Stallman has blessed us with the shell command @mkfifo@
    (is there a standard Unix version?) which creates a named pipe.  If we
    instead run:

	    mkfifo <progname>.hp
	    hpView2 <progname>.hp Main:<functionname> &
	    <progname> <parameters> +RTS -h -i0.1 -RTS
	    rm <progname>.hp

    Good Things happen.

    NB If you don't delete the pipe, Bad Things happen: the program
    writes profiling info to the pipe until the pipe fills up then it
    blocks...


Right, on with the program:

Here's an example heap profile

          JOB "a.out -p"
          DATE "Fri Apr 17 11:43:45 1992"
          SAMPLE_UNIT "seconds"
          VALUE_UNIT "bytes"
          BEGIN_SAMPLE 0.00
            SYSTEM 24
          END_SAMPLE 0.00
          BEGIN_SAMPLE 1.00
            elim 180
            insert 24
            intersect 12
            disin 60
            main 12
            reduce 20
            SYSTEM 12
          END_SAMPLE 1.00
          MARK 1.50
          MARK 1.75
          MARK 1.80
          BEGIN_SAMPLE 2.00
            elim 192
            insert 24
            intersect 12
            disin 84
            main 12
            SYSTEM 24
          END_SAMPLE 2.00
          BEGIN_SAMPLE 2.82
          END_SAMPLE 2.82

In HpView.lhs, I had a fancy parser to handle all this - but it was
immensely inefficient.  We can produce something a lot more efficient
and robust very easily by noting that the only lines we care about
have precisely two entries on them.

> type Line = String
> type Word = String
> type Sample = (Float, [(String, Int)])

> parseProfile :: [[Word]] -> [Sample]
> parseProfile [] = []
> parseProfile ([keyword, time]:lines) | keyword == "BEGIN_SAMPLE" =
>	let (sample,rest) = parseSample lines
>	in
>	(read time, sample) : parseProfile rest
> parseProfile (_:xs) = parseProfile xs

> parseSample :: [[Word]] -> ([(String,Int)],[[Word]])
> parseSample ([word, count]:lines) =
>	if word == "END_SAMPLE" 
>	then ([], lines)
>	else let (samples, rest) = parseSample lines
>	     in ( (word, read count):samples,  rest )
> parseSample duff_lines = ([],duff_lines)

> screen_size = 200

> main :: IO ()
> main =
>	getArgs					>>= \ r ->
>	case r of 
>	  [filename, ident] -> 
>		xInitialise [] screen_size screen_size	>>
>		readFile filename			>>= \ hp ->
>		let samples = parseProfile (map words (lines hp))
>		    totals = [ sum [ s | (_,s) <- ss ] | (t,ss) <- samples ]
>
>		    ts = map scale totals
>		    is = map scale (slice samples ident)
>  		in
>		graphloop2 (is, []) (ts, [])
>	  _ -> error "usage: hpView2 file identifier\n"

For the example I'm running this on, the following scale does nicely.

> scale :: Int -> Float
> scale n = (fromInt n) / 10000.0

Slice drawing stuff... shows profile for each identifier (Ignores time
info in this version...)

> slice :: [Sample] -> String -> [Int]
> slice samples ident =
>	[ c | (t,ss) <- samples, c <- [lookupPairs ss ident 0] ]

> lookupPairs :: Eq a => [(a, b)] -> a -> b -> b
> lookupPairs ((a', b') : hs) a b =
>	if a == a' then b' else lookupPairs hs a b
> lookupPairs [] a b = b

Number of samples to display on screen

> n :: Int
> n = 40

Graph-drawing loop.  Get's the data for the particular identifier and
the total usage, scales to get total to fit screen and draws them.

> graphloop2 :: ([Float], [Float]) -> ([Float], [Float]) -> IO ()
> graphloop2 (i:is,is') (t:ts, ts') =
>	let is'' = take n (i:is')
>	    ts'' = take n (t:ts')
>
>	    -- scaling information:
>	    m = maximum ts''
>	    y_scale = (floor m) + 1
>	    y_scale' = fromInt y_scale
>	in
>	xCls						>>
> 	drawScales y_scale				>>
>	draw x_coords [ x / y_scale' | x <- is'' ]	>>
>	draw x_coords [ x / y_scale' | x <- ts'' ]	>>
>	xHandleEvent					>>
>	graphloop2 (is,is'') (ts, ts'')
> graphloop2 _ _ =
>	return ()

> x_coords :: [Float]
> x_coords = [ 0.0, 1 / (fromInt n) .. ]

Note: unpleasant as it is, the code cannot be simplified to something
like the following (which has scope for changing draw to take a list
of pairs).  The problem is that the graph won't start to be drawn
until the first @n@ values are available. (Is there also a danger of
clearing the screen while waiting for the next input value?)  A
possible alternative solution is to keep count of how many values have
actually been received.

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
