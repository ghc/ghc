-----------------------------------------------------------------------------
-- $Id: Slurp.hs,v 1.4 2002/09/18 12:36:40 simonmar Exp $

-- (c) Simon Marlow 1997-1999
-----------------------------------------------------------------------------

module Slurp (Status(..), Results(..), ResultTable(..), parse_log) where

import CmdLine
import FiniteMap
import RegexString
import Maybe

-----------------------------------------------------------------------------
-- This is the structure into which we collect our results:

type ResultTable = FiniteMap String Results

data Status
	= NotDone
	| Success
	| OutOfHeap
	| OutOfStack
	| Exit Int
	| WrongStdout
	| WrongStderr 

data Results = Results { 
	compile_time   	:: FiniteMap String Float,
	module_size	:: FiniteMap String Int,
	binary_size    	:: Maybe Int,
	link_time      	:: Maybe Float,
	run_time       	:: Maybe Float,
	mut_time	:: Maybe Float,
	instrs          :: Maybe Integer,
	mem_reads       :: Maybe Integer,
	mem_writes      :: Maybe Integer,
	cache_misses    :: Maybe Integer,
	gc_work        	:: Maybe Integer,
	gc_time        	:: Maybe Float,
	allocs         	:: Maybe Integer,
	run_status     	:: Status,
	compile_status 	:: Status
	}

emptyResults = Results { 
	compile_time   	= emptyFM,
	module_size    	= emptyFM,
	binary_size    	= Nothing,
	link_time      	= Nothing,
	run_time       	= Nothing,
	mut_time       	= Nothing,
	instrs          = Nothing,
	mem_reads       = Nothing,
	mem_writes      = Nothing,
	cache_misses    = Nothing,
	gc_time        	= Nothing,
	gc_work        	= Nothing,
	allocs	       	= Nothing,
	compile_status 	= NotDone,
	run_status     	= NotDone
	}

-----------------------------------------------------------------------------
-- Parse the log file

{-
Various banner lines:

==nofib== awards: size of QSort.o follows...
==nofib== banner: size of banner follows...
==nofib== awards: time to link awards follows...
==nofib== awards: time to run awards follows...
==nofib== boyer2: time to compile Checker follows...
-}

banner_re = mkRegex "^==nofib==[ \t]+([A-Za-z0-9_]+):[ \t]+(size of|time to link|time to run|time to compile)[ \t]+([A-Za-z0-9_]+)(\\.o)?[ \t]+follows"

{-
This regexp for the output of "time" works on FreeBSD, other versions
of "time" will need different regexps.
-}

time_re = mkRegex "^[ \t]*([0-9.]+)[ \t]+real[ \t]+([0-9.]+)[ \t]+user[ \t]+([0-9.]+)[ \t]+sys[ \t]*$"

time_gnu17_re = mkRegex "^[ \t]*([0-9.]+)user[ \t]+([0-9.]+)system[ \t]+([0-9.:]+)elapsed"
                -- /usr/bin/time --version reports: GNU time 1.7
                -- notice the order is different, and the elapsed time is [hh:]mm:ss.s

size_re = mkRegex "^[ \t]*([0-9]+)[ \t]+([0-9]+)[ \t]+([0-9]+)"

{-
<<ghc: 5820820 bytes, 0 GCs, 0/0 avg/max bytes residency (0 samples), 41087234 bytes GC work, 0.00 INIT (0.05 elapsed), 0.08 MUT (0.18 elapsed), 0.00 GC (0.00 elapsed) :ghc>>

	= (bytes, gcs, avg_resid, max_resid, samples, gc_work,
	   init, init_elapsed, mut, mut_elapsed, gc, gc_elapsed)

ghc1_re = pre GHC 4.02
ghc2_re = GHC 4.02 (includes "xxM in use")
ghc3_re = GHC 4.03 (includes "xxxx bytes GC work")
-}

ghc1_re = mkRegex "^<<ghc:[ \t]+([0-9]+)[ \t]+bytes,[ \t]*([0-9]+)[ \t]+GCs,[ \t]*([0-9]+)/([0-9]+)[ \t]+avg/max bytes residency \\(([0-9]+) samples\\), ([0-9]+) bytes GC work, ([0-9.]+) INIT \\(([0-9.]+) elapsed\\), ([0-9.]+) MUT \\(([0-9.]+) elapsed\\), ([0-9.]+) GC \\(([0-9.]+) elapsed\\) :ghc>>"

ghc2_re = mkRegex "^<<ghc:[ \t]+([0-9]+)[ \t]+bytes,[ \t]*([0-9]+)[ \t]+GCs,[ \t]*([0-9]+)/([0-9]+)[ \t]+avg/max bytes residency \\(([0-9]+) samples\\), ([0-9]+)M in use, ([0-9.]+) INIT \\(([0-9.]+) elapsed\\), ([0-9.]+) MUT \\(([0-9.]+) elapsed\\), ([0-9.]+) GC \\(([0-9.]+) elapsed\\) :ghc>>"

ghc3_re = mkRegex "^<<ghc:[ \t]+([0-9]+)[ \t]+bytes,[ \t]*([0-9]+)[ \t]+GCs,[ \t]*([0-9]+)/([0-9]+)[ \t]+avg/max bytes residency \\(([0-9]+) samples\\), ([0-9]+) bytes GC work, ([0-9]+)M in use, ([0-9.]+) INIT \\(([0-9.]+) elapsed\\), ([0-9.]+) MUT \\(([0-9.]+) elapsed\\), ([0-9.]+) GC \\(([0-9.]+) elapsed\\) :ghc>>"

ghc4_re = mkRegex "^<<ghc-instrs:[ \t]+([0-9]+)[ \t]+bytes,[ \t]*([0-9]+)[ \t]+GCs,[ \t]*([0-9]+)/([0-9]+)[ \t]+avg/max bytes residency \\(([0-9]+) samples\\), ([0-9]+) bytes GC work, ([0-9]+)M in use, ([0-9.]+) INIT \\(([0-9.]+) elapsed\\), ([0-9.]+) MUT \\(([0-9.]+) elapsed\\), ([0-9.]+) GC \\(([0-9.]+) elapsed\\), ([0-9]+) instructions, ([0-9]+) memory reads, ([0-9]+) memory writes, ([0-9]+) L2 cache misses :ghc-instrs>>"

wrong_exit_status = mkRegex "^\\**[ \t]*expected exit status ([0-9]+) not seen ; got ([0-9]+)"

wrong_output = mkRegex "^expected (stdout|stderr) not matched by reality$"

out_of_heap = mkRegex "^\\+ Heap exhausted;$"

out_of_stack = mkRegex "^\\+ Stack space overflow:"

parse_log :: String -> ResultTable
parse_log
	= combine_results		-- collate information
	. concat
	. map process_chunk		-- get information from each chunk
	. tail				-- first chunk is junk
	. chunk_log [] []		-- break at banner lines
	. lines

combine_results :: [(String,Results)] -> FiniteMap String Results
combine_results = foldr f emptyFM
 where
	f (prog,results) fm = addToFM_C comb fm prog results
	comb Results{ compile_time = ct1, link_time = lt1, 
		      module_size = ms1,
		      run_time = rt1, mut_time = mt1, 
		      instrs = is1, mem_reads = mr1, mem_writes = mw1,
		      cache_misses = cm1,
		      gc_time = gt1, gc_work = gw1,
		      binary_size = bs1, allocs = al1, 
		      run_status = rs1, compile_status = cs1 }
	     Results{ compile_time = ct2, link_time = lt2, 
		      module_size = ms2,
		      run_time = rt2, mut_time = mt2,
		      instrs = is2, mem_reads = mr2, mem_writes = mw2,
		      cache_misses = cm2,
		      gc_time = gt2, gc_work = gw2,
		      binary_size = bs2, allocs = al2, 
		      run_status = rs2, compile_status = cs2 }
	  =  Results{ compile_time   = plusFM_C const ct1 ct2,
		      module_size    = plusFM_C const ms1 ms2,
		      link_time      = combMaybes lt1 lt2,
		      run_time       = combMaybes rt1 rt2,
		      mut_time       = combMaybes mt1 mt2,
		      instrs         = combMaybes is1 is2,
		      mem_reads      = combMaybes mr1 mr2,
		      mem_writes     = combMaybes mw1 mw2,
		      cache_misses   = combMaybes cm1 cm2,
		      gc_time        = combMaybes gt1 gt2,
		      gc_work        = combMaybes gw1 gw2,
		      binary_size    = combMaybes bs1 bs2,
		      allocs         = combMaybes al1 al2,
		      run_status     = combStatus rs1 rs2,
		      compile_status = combStatus cs1 cs2 }

combMaybes m1 m2 = case maybeToList m1 ++ maybeToList m2 of
			[] -> Nothing
			(x:_) -> Just x

combStatus NotDone x = x
combStatus x NotDone = x
combStatus x y = x

chunk_log :: [String] -> [String] -> [String] -> [([String],[String])]
chunk_log header chunk [] = [(header,chunk)]
chunk_log header chunk (l:ls) =
	case matchRegex banner_re l of
		Nothing -> chunk_log header (l:chunk) ls
		Just stuff -> (header,chunk) : chunk_log stuff [] ls

process_chunk :: ([String],[String]) -> [(String,Results)]
process_chunk (prog : what : mod : _, chk) =
 case what of
	"time to compile" -> parse_compile_time prog mod chk
	"time to run"     -> parse_run_time prog (reverse chk) NotDone
	"time to link"    -> parse_link_time prog chk
	"size of"	  -> parse_size prog mod chk
	_		  -> error ("process_chunk: "++what)

parse_compile_time prog mod [] = []
parse_compile_time prog mod (l:ls) =
	case matchRegex time_re l of {
	     Just (real:user:system:_) ->
		let ct  = addToFM emptyFM mod (read user)
		in 
		[(prog,emptyResults{compile_time = ct})];
	     Nothing -> 

	case matchRegex time_gnu17_re l of {
	     Just (user:system:elapsed:_) ->
		let ct  = addToFM emptyFM mod (read user)
		in 
		[(prog,emptyResults{compile_time = ct})];
	     Nothing -> 

	case matchRegex ghc1_re l of {
	    Just (allocs:_:_:_:_:init:_:mut:_:gc:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  time = (read init + read_mut + read_gc) :: Float 
		  ct  = addToFM emptyFM mod time
	      in
		[(prog,emptyResults{compile_time = ct})];
	    Nothing ->

	case matchRegex ghc2_re l of {
	   Just (allocs:_:_:_:_:_:init:_:mut:_:gc:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  time = (read init + read_mut + read_gc) :: Float 
		  ct  = addToFM emptyFM mod time
	      in
		[(prog,emptyResults{compile_time = ct})];
	    Nothing ->

	case matchRegex ghc3_re l of {
	   Just (allocs:_:_:_:_:_:_:init:_:mut:_:gc:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  time = (read init + read_mut + read_gc) :: Float 
		  ct  = addToFM emptyFM mod time
	      in
		[(prog,emptyResults{compile_time = ct})];
	    Nothing ->

	case matchRegex ghc4_re l of {
	   Just (allocs:_:_:_:_:_:_:init:_:mut:_:gc:_:_:_:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  time = (read init + read_mut + read_gc) :: Float 
		  ct  = addToFM emptyFM mod time
	      in
		[(prog,emptyResults{compile_time = ct})];
	    Nothing ->

		parse_compile_time prog mod ls
	}}}}}}

parse_link_time prog [] = []
parse_link_time prog (l:ls) =
	  case matchRegex time_re l of {
	     Just (real:user:system:_) ->
		[(prog,emptyResults{link_time = Just (read user)})];
	     Nothing ->

	  case matchRegex time_gnu17_re l of {
	     Just (user:system:elapsed:_) ->
		[(prog,emptyResults{link_time = Just (read user)})];
	     Nothing ->

          parse_link_time prog ls
          }}

parse_run_time prog [] NotDone = []
parse_run_time prog [] ex =[(prog,emptyResults{run_status=ex})]
parse_run_time prog (l:ls) ex =
	case matchRegex ghc1_re l of {
	   Just (allocs:_:_:_:_:init:_:mut:_:gc:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  time = (read init + read_mut + read_gc) :: Float 
	      in
	      [(prog,emptyResults{run_time   = Just time,
				  mut_time   = Just read_mut,
				  gc_time    = Just read_gc,
				  allocs     = Just (read allocs),
				  run_status = Success })];
	   Nothing -> 

	case matchRegex ghc2_re l of {
	   Just (allocs:_:_:_:_:_:init:_:mut:_:gc:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  time = (read init + read_mut + read_gc) :: Float 
	      in
	      [(prog,emptyResults{run_time   = Just time,
				  mut_time   = Just read_mut,
				  gc_time    = Just read_gc,
				  allocs     = Just (read allocs),
				  run_status = Success })];
	    Nothing ->
	
	case matchRegex ghc3_re l of {
	   Just (allocs:_:_:_:_:gc_work:_:init:_:mut:_:gc:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  read_gc_work = read gc_work
		  time = (read init + read_mut + read_gc) :: Float 
	      in
	      [(prog,emptyResults{run_time   = Just time,
				  mut_time   = Just read_mut,
				  gc_work    = Just read_gc_work,
				  gc_time    = Just read_gc,
				  allocs     = Just (read allocs),
				  run_status = Success })];
	    Nothing ->
	
	case matchRegex ghc4_re l of {
	   Just (allocs:_:_:_:_:gc_work:_:init:_:mut:_:gc:_:is:mem_rs:mem_ws:cache_misses:_) ->
	      let 
		  read_mut = read mut
		  read_gc  = read gc
		  read_gc_work = read gc_work
		  time = (read init + read_mut + read_gc) :: Float 
	      in
	      [(prog,emptyResults{run_time   = Just time,
				  mut_time   = Just read_mut,
				  gc_work    = Just read_gc_work,
				  gc_time    = Just read_gc,
				  instrs     = Just (read is),
				  mem_reads  = Just (read mem_rs),
				  mem_writes = Just (read mem_ws),
				  cache_misses = Just (read cache_misses),
				  allocs     = Just (read allocs),
				  run_status = Success })];
	    Nothing ->
	
	case matchRegex wrong_output l of {
	    Just ("stdout":_) -> 
		parse_run_time prog ls (combineRunResult WrongStdout ex);
	    Just ("stderr":_) -> 
		parse_run_time prog ls (combineRunResult WrongStderr ex);
	    Nothing ->
			
	case matchRegex wrong_exit_status l of {
	    Just (wanted:got:_) -> 
		parse_run_time prog ls (combineRunResult (Exit (read got)) ex);
	    Nothing -> 

	case matchRegex out_of_heap l of {
	    Just _ -> 
		parse_run_time prog ls (combineRunResult OutOfHeap ex);
	    Nothing -> 

	case matchRegex out_of_stack l of {
	    Just _ -> 
		parse_run_time prog ls (combineRunResult OutOfStack ex);
	    Nothing -> 
		parse_run_time prog ls ex;

	}}}}}}}}

combineRunResult OutOfHeap  _           = OutOfHeap
combineRunResult _          OutOfHeap   = OutOfHeap
combineRunResult OutOfStack _           = OutOfStack
combineRunResult _          OutOfStack  = OutOfStack
combineRunResult (Exit e)   _           = Exit e
combineRunResult _          (Exit e)    = Exit e
combineRunResult exit       _		 = exit

parse_size prog mod [] = []
parse_size prog mod (l:ls) =
	case matchRegex size_re l of
	    Nothing -> parse_size prog mod ls
	    Just (text:datas:bss:_) 
		 | prog == mod ->
			[(prog,emptyResults{binary_size = 
					      Just (read text + read datas),
				    compile_status = Success})]
		 | otherwise ->
			let ms  = addToFM emptyFM mod (read text + read datas)
			in
			[(prog,emptyResults{module_size = ms})]

