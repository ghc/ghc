{-# LANGUAGE PatternGuards #-}
module BuildTest
	( BuildResults(..)
	, buildTest)
where
import Benchmarks
import Config
import BuildBox
import Data.Maybe
import Control.Monad



-- | Run regression tests.	
buildTest :: Config -> Environment -> Build ()
buildTest config env
 = do	outLn "* Running regression tests"
	
	-- Get the current time.
	utcTime	<- io $ getCurrentTime

	-- Load the baseline file if it was given.
	mBaseline <- case configAgainstResults config of
			Nothing		-> return Nothing
			Just fileName
			 -> do	file	<- io $ readFile fileName
				return	$ Just file
				
	let resultsPrior :: [BenchResult Stats]
	    resultsPrior
		= maybe []
			(\contents -> map statBenchResult $ buildResultBench $ read contents)
			mBaseline
	let scratchDir 
		= fromMaybe ("buildTest: must specify --scratch") 
		$ configScratchDir config

	-- Run the DPH benchmarks
	benchResultsDPH
	 <- (flip $ maybe $ return []) (configDoTestDPH config)
	 $  \dir -> inDir dir 
	 $  mapM (outRunBenchmarkWith (configIterations config)  resultsPrior)
		 (benchmarksDPH config)

	-- Run the Repa benchmarks
	benchResultsRepa
	 <- (flip $ maybe $ return []) (configDoTestRepa config)
	 $  \dir -> inDir dir
	 $  mapM (outRunBenchmarkWith (configIterations config)  resultsPrior)
	 	 (benchmarksRepa config)

	-- Run NoSlow benchmarks
	benchResultsNoSlow
	 <- (flip $ maybe $ return []) (configDoTestNoSlow config)
	 $ \dir -> inDir dir
	 $ withTempFile $ \filePath ->
	   do	ssystem  $ "dist/build/noslow/noslow -o " ++ filePath
		liftM parseNoSlowLog $ io $ readFile filePath


	let benchResults
		= benchResultsDPH ++ benchResultsRepa ++ benchResultsNoSlow

	-- Make the build results.
	let buildResults
		= BuildResults
		{ buildResultTime		= utcTime
		, buildResultEnvironment	= env
		, buildResultBench		= benchResults }

	-- Compute comparisons against the baseline file.
	let resultComparisons
	 	= compareManyBenchResults 
			(resultsPrior)
			(map statBenchResult benchResults)

	-- Write results to a file if requested.
	resultFiles
	 <- maybe (return (error "results file has not been written"))
		  (\(fileName, shouldStamp) -> do
			stamp	<- if shouldStamp
				 	then io $ getStampyTime
					else return ""
						
			let fileName'	= fileName ++ stamp
			
			outLn $ "* Writing results to " ++ fileName'
			io $ writeFile fileName' 		$ show buildResults
			io $ writeFile (fileName' ++ ".txt") 	$ render $ reportBenchResults Nothing resultComparisons

			return [fileName', fileName' ++ ".txt"])
			
		(configWriteResults config)
	

	-- Upload results if requesed,
	--	this requires that they be written to file as above.
	maybe 	(return ())
		(\uploadPath -> do
			outLn $ "* Uploading results to " ++ uploadPath
			mapM_ (\file -> ssystem $ "scp " ++ file ++ " " ++ uploadPath) resultFiles)
			
		(configUploadResults config)

	
	-- Mail results to recipient if requested.
	let spaceHack = text . unlines . map (\l -> " " ++ l) . lines
	maybe 	(return ())
		(\(from, to) -> do
			outLn $ "* Mailing results to " ++ to 
			
			banner	<- maybe	
					(return blank)
					(\file -> (io $ readFile file) >>= return . text)
					(configMailBanner config)

			let branchName 
			        = fromMaybe "" 
			        $ (liftM (++ ": ") $ configMailBranchName config)
			
			mail	<- createMailWithCurrentTime from to 
					("[nightly] " ++ branchName ++ "DPH Performance Test Succeeded")
					$ render $ vcat
					[ banner
					, ppr env
					, blank
					, spaceHack $ render $ reportBenchResults (configSwingFraction config) resultComparisons
					, blank ]
			
			outLn $ "  - Writing mail file"
			io $ writeFile "dph-buildbot.mail" $ render $ renderMail mail
				
			outLn $ "  - Sending mail"
			sendMailWithMailer mail defaultMailer
			return ())
		(configMailFromTo config)



-- | Parse a noslow benchmark results files.
parseNoSlowLog :: String -> [BenchResult Single]
parseNoSlowLog str
 = case lines str of
	[]	-> error $ "parseNoSlowLog: no lines"
	ls	-> map parseNoSlowLine $ tail ls
	
parseNoSlowLine :: String -> BenchResult Single
parseNoSlowLine str
 = let	[name, mean, meanLB, meanUB, _, _, _]
		= words $ map (\c -> if c == ',' then ' ' else c) str
	
   in	BenchResult
		{ benchResultName	= normaliseNoSlowName $ read name
		, benchResultRuns	
			= [ BenchRunResult 0 []
				[ Time KernelWall `secs` (read meanLB) 
				, Time KernelWall `secs` (read mean)
				, Time KernelWall `secs` (read meanUB) ] ]
		}


-- | Normalise the name of a noslow benchmark to match our format.
normaliseNoSlowName :: String -> String
normaliseNoSlowName name
	= ("noslow." ++)
	$ map (\c -> if c == '/' then '.' else c)
	$ name
