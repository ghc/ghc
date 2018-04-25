{-# OPTIONS -fno-warn-type-defaults #-}

module Benchmarks where
import Config
import BuildBox	
import Control.Monad
import qualified BuildBox.Data.Log	as Log

-- | DPH benchmark configuation.
benchmarksDPH :: Config -> [Benchmark]
benchmarksDPH config

 	-- dot product --------------------------------------------------------
 =	  (let	run n	= bench config
				("dph.dotp.vectorised.par.N" ++ show n)
				("dph-examples/dist/build/dph-dotp/dph-dotp vectorised 10000000 +RTS -N" ++ show n)
	   in	map run [1, 2, 4, 8])
	
 ++	[ bench config
		"dph.dotp.vectorised.seq.N4"
		"dph-examples/dist/build/dph-dotp-seq/dph-dotp-seq vectorised 10000000 +RTS -N4"		

	, bench config
		"dph.dotp.vector.seq.N4"
		"dph-examples/dist/build/dph-dotp/dph-dotp vector 10000000 +RTS -N4" ]


	  -- sum of squares ---------------------------------------------------
 ++	(let 	run n	= bench config
				("dph.sumsq.vectorised.par.N" ++ show n)
				("dph-examples/dist/build/dph-sumsq/dph-sumsq vectorised 100000000 +RTS -N" ++ show n)
	 in 	map run [1, 2, 4, 8])

 ++	[ bench config
		"dph.sumsq.vectorised.seq.N4"
		"dph-examples/dist/build/dph-sumsq-seq/dph-sumsq-seq vectorised 100000000 +RTS -N4"

	, bench config
		"dph.sumsq.vector.seq.N4"
		"dph-examples/dist/build/dph-sumsq/dph-sumsq vector 100000000 +RTS -N4" ]
		

	  -- evens ------------------------------------------------------------
 ++	(let 	run n	= bench config
				("dph.evens.vectorised.par.N" ++ show n)
				("dph-examples/dist/build/dph-evens/dph-evens vectorised 10000000 +RTS -N" ++ show n)
	 in	map run [1, 2, 4, 8])

 ++	[ bench config
		"dph.evens.vectorised.seq.N4"
		"dph-examples/dist/build/dph-evens-seq/dph-evens-seq vectorised 10000000 +RTS -N4"
	
	, bench config
		"dph.evens.vector.seq.N4"
		"dph-examples/dist/build/dph-evens-seq/dph-evens-seq vector 10000000 +RTS -N4" ]


{-	  -- primes ------------------------------------------------------------
 ++	(let 	run n	= bench config
				("dph.primes.vectorised.par.N" ++ show n)
				("dph-examples/dist/build/dph-primes/dph-primes vectorised 20000000 +RTS -N" ++ show n)
	 in	map run [1, 2, 4, 8])

 ++	[ bench config
		"dph.primes.vectorised.seq.N4"
		"dph-examples/dist/build/dph-primes-seq/dph-primes-seq vectorised 20000000 +RTS -N4"
	
	, bench config
		"dph.primes.vector.seq.N4"
		"dph-examples/dist/build/dph-primes-seq/dph-primes-seq vector 20000000 +RTS -N4" ]
-}	

	  -- quicksort --------------------------------------------------------
 ++	(let	run n	= bench config 
				("dph.quicksort.vectorised.par.N" ++ show n)
				("dph-examples/dist/build/dph-quicksort/dph-quicksort 100000 +RTS -N" ++ show n)
	 in	map run [1, 2, 4, 8])


	  -- quickhull --------------------------------------------------------
 ++	(let	run n	= bench config 
				("dph.quickhull.vectorised.par.N" ++ show n)
				("dph-examples/dist/build/dph-quickhull/dph-quickhull 1000000 +RTS -K20M -N" ++ show n)
	 in	map run [1, 2, 4, 8])


 ++	[ bench config 
		"dph.quickhull.vectorised.seq.N4"
		"dph-examples/dist/build/dph-quickhull-seq/dph-quickhull-seq 1000000 +RTS -N4 -K40M"

	, bench config
		"dph.quickhull.vector-immutable.seq.N4"
		"dph-examples/dist/build/dph-quickhull-vector/dph-quickhull-vector split  1000000 +RTS -N4"

	, bench config
		"dph.quickhull.vector-mutable.seq.N4"
		"dph-examples/dist/build/dph-quickhull-vector/dph-quickhull-vector vector 1000000 +RTS -N4"

	, bench config
		"dph.quickhull.vector-forkIO.par.N4"
		"dph-examples/dist/build/dph-quickhull-vector/dph-quickhull-vector io 1000000 +RTS -N4"

	, bench config
		"dph.quickhull.vector-forkIO.par.N8"
		"dph-examples/dist/build/dph-quickhull-vector/dph-quickhull-vector io 1000000 +RTS -N8"

	, benchUp config
	 	"dph.quickhull.c.seq"
		(inDir "dph-examples/spectral/QuickHull/c" $ qssystem "make")
		"dph-examples/spectral/QuickHull/c/quickhull 1000000" ]
		
{-	  -- nbody ------------------------------------------------------------
 ++	(let	run n	= bench config
				("dph.nbody.vectorised.par.N" ++ show n)
				("dph-examples/dist/build/dph-nbody/dph-nbody --max-steps 10 -b 100 -s nested-bh +RTS -N" ++ show n)
				
	 in	map run [1, 2, 4])
	
 ++ 	[ bench config
		"dph.nbody.vectorised.seq.N4"
		"dph-examples/dist/build/dph-nbody/dph-nbody --max-steps 10 -b 100 -s nested-bh +RTS -N4"

	, bench config
		"dph.nbody.vector.seq.N4"
		"dph-examples/dist/build/dph-nbody/dph-nbody --max-steps 10 -b 100 -s vector-bh +RTS -N4"
	]
-}

-- | Repa benchmark configuration.
benchmarksRepa :: Config -> [Benchmark]
benchmarksRepa config
 =	-- mmult --------------------------------------------------------------
	(let 	mmult	= "repa-examples/dist/build/repa-mmult/repa-mmult"
		run n	= bench config
				("repa.mmult.par.N" ++ show n)
				(mmult ++ " -random 1024 1024 -random 1024 1024 +RTS -N" ++ show n)

	in	[run 1, run 2, run 4, run 8])

 ++	[ benchUp config
		"repa.mmult.c.seq"
		(inDir "repa-examples" $ qssystem "make dist/build/repa-mmult-c/repa-mmult-c")
		"repa-examples/dist/build/repa-mmult-c/repa-mmult-c -random 1024 1024 -random 1024 1024" ]

		
 	-- laplace ------------------------------------------------------------
 ++	(let	laplace = "repa-examples/dist/build/repa-laplace/repa-laplace"
		input	= "repa-examples/examples/Laplace/data/pls-400x400.bmp"
		inputgz	= input ++ ".gz"

		run n	= benchUp config
		 		("repa.laplace.par.N" ++ show n)
				(do	ensureDir "output"
					check $ HasExecutable laplace
					whenM (test $ HasFile inputgz)
				  	 $ qssystem $ "gzip -d " ++ inputgz)
				(laplace ++ " get 1000 " ++ input ++ " output/laplace.bmp +RTS -qg -N" ++ show n)

	  in	[run 1, run 2, run 4, run 6, run 8])

 ++	[ benchUp config
		"repa.laplace.c.seq"
		(inDir "repa-examples" $ qssystem "make dist/build/repa-laplace-c/repa-laplace-c")
		"repa-examples/dist/build/repa-laplace-c/repa-laplace-c 400 400 1000 output/laplace_c-seq.ppm" ]

	
	-- blur ---------------------------------------------------------------
 ++	(let	blur	= "repa-examples/dist/build/repa-blur/repa-blur"
		input	= "repa-examples/data/lena.bmp"
		inputgz	= input ++ ".gz"
		
		run n	= benchUp config
				("repa.blur.par.N" ++ show n)
				(do	ensureDir "output"
					check $ HasExecutable blur
					whenM (test $ HasFile inputgz)
				 	 $ qssystem $ "gzip -d " ++ inputgz)
				(blur ++ " 5 " ++ input ++ " output/lena-blur.bmp +RTS -qg -N" ++ show n)

	  in	map run [1, 2, 4, 6, 8])
	

	-- canny -------------------------------------------------------------
 ++	(let	canny   = "repa-examples/dist/build/repa-canny/repa-canny"
		input	= "repa-examples/data/lena.bmp"
		inputgz	= input ++ ".gz"
		
	 	run n	= benchUp config
				("repa.canny.par.N" ++ show n)
				(do	ensureDir "output"
					check $ HasExecutable canny
					whenM (test $ HasFile inputgz)
				 	 $ qssystem $ "gzip -d " ++ inputgz)
				(canny ++ " " ++ input ++ " output/lena-canny.bmp +RTS -qg -N" ++ show n)
				
	  in	map run [1, 2, 4, 6, 8])


	-- fft2d-highpass -----------------------------------------------------
 ++	(let	fft2d	= "repa-examples/dist/build/repa-fft2d-highpass/repa-fft2d-highpass"
		input	= "repa-examples/data/lena.bmp"
		inputgz	= input ++ ".gz"
		
		run n	= benchUp config
				("repa.fft2d.par.N" ++ show n)
				(do	ensureDir "output"
					check $ HasExecutable fft2d
					whenM (test $ HasFile inputgz)
				 	 $ qssystem $ "gzip -d " ++ inputgz)
				(fft2d ++ " 1 " ++ input ++ " output/fft2d.bmp +RTS -qg -N" ++ show n)
	 in	map run [1, 2, 4, 6, 8])


	-- fft3d-highpass -----------------------------------------------------
 ++	(let	fft3d	= "repa-examples/dist/build/repa-fft3d-highpass/repa-fft3d-highpass"
	
		run n	= benchUp config
				("repa.fft3d.par.N" ++ show n)
				(ensureDir "output/fft3d")
				(fft3d ++ " 128 output/fft3d/slice +RTS -qg -N" ++ show n)

	 in	map run [1, 2, 4, 6, 8])
	


-- | Define a plain benchmark with no setup or teardown command
bench :: Config -> String -> String -> Benchmark
bench config name cmd
 = Benchmark
	name
	(return ())
	(systemWithTimings (configVerbose config) cmd)
	(return [])


-- | Define a benchmark with a setup command
benchUp :: Config -> String -> Build () -> String -> Benchmark
benchUp config name cmdUp cmdBench
 = Benchmark
	name
	cmdUp
	(systemWithTimings (configVerbose config) cmdBench)
	(return [])

	
-- | Run a system command, expecing it to print the kernel timings to stdout.
--   We ignore whatever is printed to stderr.
systemWithTimings :: Bool -> String -> Build [WithUnits (Aspect Single)]
systemWithTimings verbose cmd
 = do	when verbose
	 $ outLn $ "\n    " ++ cmd

	(code, logOut, logErr)
		<- systemTeeLog False cmd Log.empty 

	if code == ExitSuccess
	 then	return	$ parseTimings (Log.toString logOut)
	 else	throw   $ ErrorSystemCmdFailed cmd code logOut logErr


-- | Parse kernel timings from a repa example program.
--   Format is  elapsedTime/systemTime  in milliseconds.
parseTimings :: String -> [WithUnits (Aspect Single)]
parseTimings str
 = let	(lElapsed : _)	= lines str
	thing		= dropWhile (/= '=') lElapsed

	elapsedTime
	 = case thing of
		[]	-> error $ "parseTimings: no time in " ++ show thing
		_	-> tail thing
				
   in	[ Time KernelWall `secs` (read elapsedTime / 1000) ]


