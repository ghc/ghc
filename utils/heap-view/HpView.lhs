> module Main where
> import PreludeGlaST
> import LibSystem

> import Parse

Program to interpret a heap profile.

Started 28/11/93: parsing of profile
Tweaked 28/11/93: parsing fiddled till it worked and graphical backend added

To be done:

0) think about where I want to go with this
1) further processing... sorting, filtering, ...
2) get dynamic display
3) maybe use widgets

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

By inspection, the format seems to be:

profile :== header { sample }
header :== job date { unit }
job :== "JOB" command
date :== "DATE" dte
unit :== "SAMPLE_UNIT" string | "VALUE_UNIT" string

sample :== samp | mark
samp :== "BEGIN_SAMPLE" time {pairs} "END_SAMPLE" time
pairs :== identifer count
mark :== "MARK" time

command :== string
dte :== string
time :== float
count :== integer

But, this doesn't indicate the line structure.  The simplest way to do
this is to treat each line as a single token --- for which the
following parser is useful:

Special purpose parser that recognises a string if it matches a given
prefix and returns the remainder.

> prefixP :: String -> P String String
> prefixP p =
>	itemP 			`thenP` \ a -> 
>	let (p',a') = splitAt (length p) a
>	in 	if p == p'
>		then unitP a'
>		else zeroP


To begin with I want to parse a profile into a list of readings for
each identifier at each time.

> type Sample = (Float, [(String, Int)])

> type Line = String


> profile :: P Line [Sample]
> profile = 
>	header			`thenP_`
>	zeroOrMoreP sample	

> header :: P Line ()
> header =
>	job			`thenP_`
>	date			`thenP_`
>	zeroOrMoreP unit	`thenP_`
>	unitP ()

> job :: P Line String
> job =	prefixP "JOB "

> date :: P Line String
> date = prefixP "DATE "

> unit :: P Line String
> unit =
>	( prefixP "SAMPLE_UNIT " )
>	`plusP`
>	( prefixP "VALUE_UNIT " )

> sample :: P Line Sample
> sample =
>	samp `plusP` mark

> mark :: P Line Sample
> mark =
>	prefixP "MARK "		`thenP` \ time ->
>	unitP (read time, [])

ToDo: check that @time1 == time2@

> samp :: P Line Sample
> samp = 
>	prefixP "BEGIN_SAMPLE " 	`thenP` \ time1 ->
>	zeroOrMoreP pair		`thenP` \ pairs ->
>	prefixP "END_SAMPLE "		`thenP` \ time2 ->
>	unitP (read time1, pairs)

> pair :: P Line (String, Int)
> pair =
>	prefixP "  "			`thenP` \ sample_line ->
>	let [identifier,count] = words sample_line
>	in unitP (identifier, read count)

This test works fine

> {-
> test :: String -> String
> test str = ppSamples (theP profile (lines str))

> test1 = test example

> test2 :: String -> Dialogue
> test2 file =
>	readFile file				exit
>	(\ hp -> appendChan stdout (test hp)	exit
>	done)
> -}

Inefficient pretty-printer (uses ++ excessively)

> ppSamples :: [ Sample ] -> String
> ppSamples = unlines . map ppSample

> ppSample :: Sample -> String
> ppSample (time, samps) = 
>	(show time) ++ unwords (map ppSamp samps)

> ppSamp :: (String, Int) -> String
> ppSamp (identifier, count) = identifier ++ ":" ++ show count

To get the test1 to work in gofer, you need to fiddle with the input
a bit to get over Gofer's lack of string-parsing code.

> example =
>  "JOB \"a.out -p\"\n" ++
>  "DATE \"Fri Apr 17 11:43:45 1992\"\n" ++
>  "SAMPLE_UNIT \"seconds\"\n" ++
>  "VALUE_UNIT \"bytes\"\n" ++
>  "BEGIN_SAMPLE 0.00\n" ++
>  "  SYSTEM 24\n" ++
>  "END_SAMPLE 0.00\n" ++
>  "BEGIN_SAMPLE 1.00\n" ++
>  "  elim 180\n" ++
>  "  insert 24\n" ++
>  "  intersect 12\n" ++
>  "  disin 60\n" ++
>  "  main 12\n" ++
>  "  reduce 20\n" ++
>  "  SYSTEM 12\n" ++
>  "END_SAMPLE 1.00\n" ++
>  "MARK 1.50\n" ++
>  "MARK 1.75\n" ++
>  "MARK 1.80\n" ++
>  "BEGIN_SAMPLE 2.00\n" ++
>  "  elim 192\n" ++
>  "  insert 24\n" ++
>  "  intersect 12\n" ++
>  "  disin 84\n" ++
>  "  main 12\n" ++
>  "  SYSTEM 24\n" ++
>  "END_SAMPLE 2.00\n" ++
>  "BEGIN_SAMPLE 2.82\n" ++
>  "END_SAMPLE 2.82"

 


Hack to let me test this code... Gofer doesn't have integer parsing built in.

> {-
> read :: String -> Int
> read s = 0
> -}

> screen_size = 200

ToDo: 

1) the efficiency of finding slices can probably be dramatically
   improved... if it matters.

2) the scaling should probably depend on the slices used

3) labelling graphs, colour, ...

4) responding to resize events

> main :: IO ()
> main =
>	getArgs				>>= \ r ->
>	case r of 
>	  filename:idents -> 
>		readFile filename	>>= \ hp ->
>		let samples = theP profile (lines hp)
>
>		    times = [ t | (t,ss) <- samples ]
>		    names = [ n | (t,ss) <- samples, (n,c) <- ss ]
>		    counts = [ c | (t,ss) <- samples, (n,c) <- ss ]
>
>		    time = maximum times
>		    x_scale = (fromInt screen_size) / time
>
>		    max_count = maximum counts
>		    y_scale = (fromInt screen_size) / (fromInt max_count)
>
>		    slices = map (slice samples) idents
>		in
>		xInitialise [] screen_size screen_size		    >>
> --		drawHeap x_scale y_scale samples		    >>
> 		sequence (map (drawSlice x_scale y_scale) slices)   >>
>		freeze
>	  _ -> error "usage: hpView filename identifiers\n"

> freeze :: IO ()
> freeze =
>	xHandleEvent				>>
> 	usleep 100				>>
>	freeze


Slice drawing stuff... shows profile for each identifier

> slice :: [Sample] -> String -> [(Float,Int)]
> slice samples ident =
>	[ (t,c) | (t,ss) <- samples, c <- [lookupPairs ss ident 0] ]

> lookupPairs :: Eq a => [(a, b)] -> a -> b -> b
> lookupPairs ((a', b') : hs) a b =
>	if a == a' then b' else lookupPairs hs a b
> lookupPairs [] a b = b

> drawSlice :: Float -> Float -> [(Float,Int)] -> IO ()
> drawSlice x_scale y_scale slc = 
>	drawPoly 
>	[ (round (x*x_scale), screen_size - (round ((fromInt y)*y_scale))) | (x,y) <- slc ]

> drawPoly :: [(Int, Int)] -> IO ()
> drawPoly ((x1,y1):(x2,y2):poly) =
>	xDrawLine x1 y1 x2 y2		>>
>	drawPoly ((x2,y2):poly)
> drawPoly _ = return ()


Very simple heap profiler... doesn't do a proper job at all.  Good for
testing.

> drawHeap :: Float -> Float -> [Sample] -> IO ()
> drawHeap x_scale y_scale samples =
>	sequence (map xBar 
>		[ (t*x_scale, (fromInt c)*y_scale) 
>		| (t,ss) <- samples, (n,c) <- ss ])	>>	
>	return ()

> xBar :: (Float, Float) -> IO ()
> xBar (x, y) = 
>	let {x' = round x; y' = round y} 
>	in xDrawLine x' screen_size x' (screen_size - y')

>#include "common-bits"
