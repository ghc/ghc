-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.8 2002/09/18 12:36:39 simonmar Exp $

-- (c) Simon Marlow 1997-1999
-----------------------------------------------------------------------------

module Main where

import GenUtils
import Printf
import Slurp
import CmdLine

import Html hiding ((!))
import qualified Html ((!))
import GlaExts
import FiniteMap
import GetOpt

import Char
import IO
import Array
import System
import List

(<!) = (Html.!)

-----------------------------------------------------------------------------
-- Top level stuff

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

usageHeader = "usage: nofib-analyse [OPTION...] <logfile1> <logfile2> ..."

main = do

 if not (null cmdline_errors) || OptHelp `elem` flags
	then die (concat cmdline_errors ++ usageInfo usageHeader argInfo)
	else do

 let { html  = OptHTMLOutput  `elem` flags; 
       ascii = OptASCIIOutput `elem` flags
     }

 if ascii && html 
	then die "Can't produce both ASCII and HTML"
	else do

 if devs && nodevs
	then die "Can't both display and hide deviations"
	else do

 results <- parse_logs other_args

 let column_headings = map (reverse . takeWhile (/= '/') . reverse) other_args

 if html 
	then putStr (renderHtml (htmlPage results column_headings))
	else putStr (asciiPage results column_headings)


parse_logs :: [String] -> IO [ResultTable]
parse_logs [] = do
	f <- hGetContents stdin
	return [parse_log f]
parse_logs log_files =
	mapM (\f -> do h <- openFile f ReadMode
		       c <- hGetContents h
		       return (parse_log c)) log_files

-----------------------------------------------------------------------------
-- List of tables we're going to generate

data PerProgTableSpec =
	forall a . Result a =>
	   SpecP 
		String			-- Name of the table
		String			-- HTML tag for the table
		(Results -> Maybe a)	-- How to get the result
		(Results -> Status)	-- How to get the status of this result
		(a -> Bool)		-- Result within reasonable limits?

data PerModuleTableSpec =
	forall a . Result a =>
	   SpecM 
		String			-- Name of the table
		String			-- HTML tag for the table
		(Results -> FiniteMap String a)	-- get the module map
		(a -> Bool)		-- Result within reasonable limits?

per_prog_result_tab =
	[ SpecP	"Binary Sizes" "binary-sizes" binary_size compile_status always_ok
	, SpecP	"Allocations" "allocations" allocs run_status always_ok
	, SpecP	"Run Time" "run-times" run_time run_status time_ok
	, SpecP	"Mutator Time" "mutator-time" mut_time run_status time_ok
	, SpecP	"GC Time" "gc-time" gc_time run_status time_ok
	, SpecP	"GC Work" "gc-work" gc_work run_status always_ok
	, SpecP	"Instructions" "instrs" instrs run_status always_ok
	, SpecP	"Memory Reads" "mem-reads" mem_reads run_status always_ok
	, SpecP	"Memory Writes" "mem-writes" mem_writes run_status always_ok
	, SpecP	"Cache Misses" "cache-misses" cache_misses run_status always_ok
	]

per_module_result_tab =
	[ SpecM "Module Sizes"  "mod-sizes"     module_size  always_ok
	, SpecM "Compile Times" "compile-time"  compile_time time_ok
	]

always_ok :: a -> Bool
always_ok = const True

time_ok :: Float -> Bool
time_ok t = t > tooquick_threshold

-----------------------------------------------------------------------------
-- HTML page generation

--htmlPage :: Results -> [String] -> Html
htmlPage results args
   =  header << thetitle << reportTitle
	  +++ hr
          +++ h1 << reportTitle
	  +++ gen_menu
	  +++ hr
	  +++ body (gen_tables results args)

gen_menu = unordList (map (prog_menu_item) per_prog_result_tab
      	              ++ map (module_menu_item) per_module_result_tab)

prog_menu_item (SpecP name anc _ _ _) = anchor <! [href ('#':anc)] << name
module_menu_item (SpecM name anc _ _) = anchor <! [href ('#':anc)] << name

gen_tables results args =
  foldr1 (+++) (map (htmlGenProgTable results args) per_prog_result_tab)
  +++ foldr1 (+++) (map (htmlGenModTable results args) per_module_result_tab)

htmlGenProgTable results args (SpecP title anc get_result get_status result_ok)
  =   sectHeading title anc
  +++ font <! [size "1"]
	<< mkTable (htmlShowResults results args get_result get_status result_ok)
  +++ hr

htmlGenModTable results args (SpecM title anc get_result result_ok)
  =   sectHeading title anc 
  +++ font <![size "1"] 
        << mkTable (htmlShowMultiResults results args get_result result_ok)
  +++ hr

sectHeading :: String -> String -> Html
sectHeading s nm = h2 << anchor <! [name nm] << s

htmlShowResults 
    :: Result a
	=> [ResultTable]
	-> [String]
	-> (Results -> Maybe a)
	-> (Results -> Status)
	-> (a -> Bool)
	-> HtmlTable

htmlShowResults (r:rs) ss f stat result_ok
  =   tabHeader ss
  </> aboves (zipWith tableRow [1..] results_per_prog)
  </> aboves ((if nodevs then []
                         else [tableRow (-1) ("-1 s.d.", lows),
                               tableRow (-1) ("+1 s.d.", highs)])
                    ++ [tableRow (-1) ("Average", gms)])
 where
	-- results_per_prog :: [ (String,[BoxValue a]) ]
	results_per_prog = map (calc_result rs f stat result_ok) (fmToList r)
	
	results_per_run  = transpose (map snd results_per_prog)
	(lows,gms,highs) = unzip3 (map calc_gmsd results_per_run)

htmlShowMultiResults
    :: Result a
	=> [ResultTable]
	-> [String]
	-> (Results -> FiniteMap String a)
	-> (a -> Bool)
	-> HtmlTable

htmlShowMultiResults (r:rs) ss f result_ok =
	multiTabHeader ss 
	 </> aboves (map show_results_for_prog results_per_prog_mod_run)
         </> aboves ((if nodevs then []
                                      else [td << bold << "-1 s.d."
                                            <-> tableRow (-1) ("", lows),
                                            td << bold << "+1 s.d."
                                            <-> tableRow (-1) ("", highs)])
                           ++ [td << bold << "Average"
                               <-> tableRow (-1) ("", gms)])

  where
	base_results = fmToList r :: [(String,Results)]

        -- results_per_prog_mod_run :: [(String,[(String,[BoxValue a])])]
        results_per_prog_mod_run = map get_results_for_prog base_results

        -- get_results_for_prog :: (String,Results) -> (String,[BoxValue a])
        get_results_for_prog (prog,r) = (prog, map get_results_for_mod (fmToList (f r)))

           where fms = map get_run_results rs

                 get_run_results fm = case lookupFM fm prog of
                                        Nothing  -> emptyFM
                                        Just res -> f res

                 get_results_for_mod (id,attr) = calc_result fms Just (const Success)
                                                             result_ok (id,attr)

        show_results_for_prog (prog,mrs) =
	    td <! [valign "top"] << bold << prog
	    <-> (if null mrs then
		   td << "(no modules compiled)"
	         else
		   toHtml (aboves (map (tableRow 0) mrs)))

        results_per_run  = transpose [xs | (_,mods) <- results_per_prog_mod_run,
                                           (_,xs) <- mods]
        (lows,gms,highs) = unzip3 (map calc_gmsd results_per_run)

tableRow :: Result a => Int -> (String, [BoxValue a]) -> HtmlTable
tableRow row_no (prog, results)
	=   td <! [bgcolor left_column_color] << prog
	<-> besides (map (\s -> td <! [align "right", clr] << show_box s) 
				results)
  where clr | row_no < 0  = bgcolor average_row_color
	    | even row_no = bgcolor even_row_color
	    | otherwise   = bgcolor odd_row_color

left_column_color = "#d0d0ff"  -- light blue
odd_row_color     = "#d0d0ff"  -- light blue
even_row_color    = "#f0f0ff"  -- v. light blue
average_row_color = "#ffd0d0"  -- light red

{-
findBest :: Result a => [BoxValue a] -> [(Bool,BoxValue a)]
findBest stuff@(Result base : rest)
  = map (\a -> (a==base, a))
  where
	best = snd (minimumBy (\a b -> fst a < fst b) no_pcnt_stuff

	no_pcnt_stuff = map unPcnt stuff

	unPcnt (r@(Percentage f) : rest) = (base * f/100, r) : unPcnt rest
	unPcnt (r@(Result a) : rest)     = (a, r) : unPcnt rest
	unPcnt (_ : rest)                = unPcnt rest
-}

logHeaders ss
  = besides (map (\s -> (td <! [align "right", width "100"] << bold << s)) ss)

mkTable t = table <! [cellspacing 0, cellpadding 0, border 0] << t

tabHeader ss
  =   (td <! [align "left", width "100"] << bold << "Program") 
  <-> logHeaders ss

multiTabHeader ss
  =   (td <! [align "left", width "100"] << bold << "Program")
  <-> (td <! [align "left", width "100"] << bold << "Module")
  <-> logHeaders ss

-- Calculate a color ranging from bright blue for -100% to bright red for +100%.

calcColor :: Int -> String
calcColor p | p >= 0    = "#"     ++ (showHex red 2 "0000")
	      | otherwise = "#0000" ++ (showHex blue 2 "")
	where red  = p * 255 `div` 100
	      blue = (-p) * 255 `div` 100

showHex 0 f s = if f > 0 then take f (repeat '0') ++ s else s
showHex i f s = showHex (i `div` 16) (f-1) (hexDig (i `mod` 16) : s)

hexDig i | i > 10 = chr (i-10 + ord 'a')
	 | otherwise = chr (i + ord '0')

-----------------------------------------------------------------------------
-- ASCII page generation

asciiPage results args =
  ( str reportTitle
  . str "\n\n"
  . interleave "\n\n" (map (asciiGenProgTable results args) per_prog_result_tab)
  . str "\n"
  . interleave "\n\n" (map (asciiGenModTable results args)  per_module_result_tab)
  ) "\n"

asciiGenProgTable results args (SpecP title anc get_result get_status result_ok)
  = str title 
  . str "\n"
  . ascii_show_results results args get_result get_status result_ok

asciiGenModTable results args (SpecM title anc get_result result_ok)
  = str title 
  . str "\n"
  . ascii_show_multi_results results args get_result result_ok

ascii_header ss
	= str "\n-------------------------------------------------------------------------------\n"
	. str (rjustify 15 "Program")
	. str (space 5)
	. foldr (.) id (map (str . rjustify fIELD_WIDTH) ss)
	. str "\n-------------------------------------------------------------------------------\n"

ascii_show_results
   :: Result a
	=> [ResultTable]
	-> [String]
	-> (Results -> Maybe a)
	-> (Results -> Status)
	-> (a -> Bool)
	-> ShowS

ascii_show_results (r:rs) ss f stat result_ok
	= ascii_header ss
	. interleave "\n" (map show_per_prog_results results_per_prog)
        . if nodevs then id
                    else   str "\n"
	                 . show_per_prog_results ("-1 s.d.",lows)
	                 . str "\n"
	                 . show_per_prog_results ("+1 s.d.",highs)
	. str "\n"
	. show_per_prog_results ("Average",gms)
 where
	-- results_per_prog :: [ (String,[BoxValue a]) ]
	results_per_prog = map (calc_result rs f stat result_ok) (fmToList r)
	
	results_per_run  = transpose (map snd results_per_prog)
        (lows,gms,highs) = unzip3 (map calc_gmsd results_per_run)

ascii_show_multi_results
   :: Result a
	=> [ResultTable]
	-> [String]
	-> (Results -> FiniteMap String a)
	-> (a -> Bool)
	-> ShowS

ascii_show_multi_results (r:rs) ss f result_ok
	= ascii_header ss 
	. interleave "\n" (map show_results_for_prog results_per_prog_mod_run)
	. str "\n"
        . if nodevs then id
                    else   str "\n"
	                 . show_per_prog_results ("-1 s.d.",lows)
	                 . str "\n"
	                 . show_per_prog_results ("+1 s.d.",highs)
	. str "\n"
	. show_per_prog_results ("Average",gms)
  where
	base_results = fmToList r :: [(String,Results)]

        -- results_per_prog_mod_run :: [(String,[(String,[BoxValue a])])]
        results_per_prog_mod_run = map get_results_for_prog base_results

        -- get_results_for_prog :: (String,Results) -> (String,[BoxValue a])
        get_results_for_prog (prog,r) = (prog, map get_results_for_mod (fmToList (f r)))

           where fms = map get_run_results rs

                 get_run_results fm = case lookupFM fm prog of
                                        Nothing  -> emptyFM
                                        Just res -> f res

                 get_results_for_mod (id,attr) = calc_result fms Just (const Success)
                                                             result_ok (id,attr)

        show_results_for_prog (prog,mrs) =
	      str ("\n"++prog++"\n")
	    . (if null mrs then
		   str "(no modules compiled)\n"
	         else
		   interleave "\n" (map show_per_prog_results mrs))

        results_per_run  = transpose [xs | (_,mods) <- results_per_prog_mod_run,
                                           (_,xs) <- mods]
        (lows,gms,highs) = unzip3 (map calc_gmsd results_per_run)

show_per_prog_results :: Result a => (String, [BoxValue a]) -> ShowS
show_per_prog_results (prog,results)
	= str (rjustify 15 prog)
	. str (space 5)
	. foldr (.) id (map (str . rjustify fIELD_WIDTH . show_box) results)

-----------------------------------------------------------------------------
-- Show the Results

class Num a => Result a where
	result_to_string :: a -> String
	convert_to_percentage :: a -> a -> Float

-- We assume an Int is a size, and print it in kilobytes.

instance Result Int where
	convert_to_percentage 0 size = 100
	convert_to_percentage base size = (fromIntegral size / fromIntegral base) * 100

	result_to_string n = show (n `div` 1024) ++ "k"

instance Result Integer where
	convert_to_percentage 0 size = 100
	convert_to_percentage base size = (fromInteger size / fromInteger base) * 100

	result_to_string n = show (n `quot` 1024) ++ "k"

instance Result Float where
	convert_to_percentage 0.0 size = 100.0
	convert_to_percentage base size = size / base * 100

	result_to_string = showFloat' Nothing (Just 2)

data BoxValue a = RunFailed Status | Percentage Float | Result a

-- calc_result is a nice exercise in higher-order programming...
calc_result 
  :: Result a
	=> [FiniteMap String b]		-- accumulated results
	-> (b -> Maybe a)		-- get a result from the b
	-> (b -> Status)		-- get a status from the b
	-> (a -> Bool)			-- is this result ok?
	-> (String,b)			-- the baseline result
	-> (String,[BoxValue a])

calc_result rts get_maybe_a get_stat result_ok (prog,base_r) =
	(prog, (just_result baseline base_stat :

	  let
		rts' = map (\rt -> get_stuff (lookupFM rt prog)) rts

		get_stuff Nothing  = (Nothing, NotDone)
		get_stuff (Just r) = (get_maybe_a r, get_stat r)
	  in
	  (
	  case baseline of
	  	Just base | result_ok base
		   -> map (\(r,s) -> percentage  r s base) rts'
	  	_other
		   -> map (\(r,s) -> just_result r s) rts'
	   )))
 where
	baseline  = get_maybe_a base_r
	base_stat = get_stat base_r

	just_result Nothing  s = RunFailed s
	just_result (Just a) s = Result a

	percentage Nothing   s base = RunFailed s
	percentage (Just a)  s base = Percentage 
					 (convert_to_percentage base a)
show_box (RunFailed s)  = show_stat s
show_box (Percentage p) = show_pcntage p
show_box (Result a)     = result_to_string a

-----------------------------------------------------------------------------
-- Calculating geometric means and standard deviations

{-
This is done using the log method, to avoid needing really large
intermediate results.  The formula for a geometric mean is 

	(a1 * .... * an) ^ 1/n

which is equivalent to

	e ^ ( (log a1 + ... + log an) / n )

where log is the natural logarithm function.

Similarly, to compute the geometric standard deviation we compute the
deviation of each log, take the root-mean-square, and take the
exponential again:

        e ^ sqrt( ( sqr(log a1 - lbar) + ... + sqr(log an - lbar) ) / n )

where lbar is the mean log,

        (log a1 + ... + log an) / n

This is a *factor*: i.e., the 1 s.d. points are (gm/sdf,gm*sdf); do
not subtract 100 from gm before performing this calculation.

We therefore return a (low, mean, high) triple.

-}

calc_gmsd :: [BoxValue a] -> (BoxValue Float, BoxValue Float, BoxValue Float)
calc_gmsd xs 
  | null percentages = (RunFailed NotDone, RunFailed NotDone, RunFailed NotDone)
  | otherwise        = let sqr x = x * x
                           len   = fromIntegral (length percentages)
                           logs  = map log percentages
                           lbar  = sum logs / len
                           devs  = map (sqr . (lbar-)) logs
                           dbar  = sum devs / len
                           gm    = exp lbar
                           sdf   = exp (sqrt dbar)
                       in
                       (Percentage (gm/sdf),
                        Percentage gm,
                        Percentage (gm*sdf))
 where
  percentages = [ if f < 5 then 5 else f | Percentage f <- xs ]
	-- can't do log(0.0), so exclude zeros
        -- small values have inordinate effects so cap at -95%.

-----------------------------------------------------------------------------
-- Generic stuff for results generation

show_pcntage n = show_float_signed (n-100) ++ "%"

show_float_signed = showFloat False False True False False Nothing (Just 2)

show_stat Success     = "(no result)"
show_stat WrongStdout = "(stdout)"
show_stat WrongStderr = "(stderr)"
show_stat (Exit x)    = "exit(" ++ show x ++")"
show_stat OutOfHeap   = "(heap)"
show_stat OutOfStack  = "(stack)"
show_stat NotDone     = "-----"

str = showString

interleave s = foldr1 (\a b -> a . str s . b) 

fIELD_WIDTH = 16 :: Int

-----------------------------------------------------------------------------
