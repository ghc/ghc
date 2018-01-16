{-# LANGUAGE ScopedTypeVariables #-}

import DPH.War.Options
import DPH.War.Way
import DPH.War.Config
import DPH.War.Job
import DPH.War.JobCreate
import DPH.War.Controller
import DPH.War.Pretty

import BuildBox
import System.Console.ParseArgs
import System.Environment
import System.Directory
import System.IO
import System.Random
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Control.Exception
import Data.List
import Data.Maybe
import qualified Data.Sequence		as Seq
import qualified Data.Foldable		as Seq
import qualified Data.Set		as Set
import qualified Data.Traversable	as Seq

main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
	args	<- parseArgsIO ArgsComplete mainArgs
        when (gotArg args ArgHelp)
         $ usageError args ""
        
	-- Load war config from the cmd line options
	let config = loadConfig args
		
	-- All the starting test directories from the command line.
	testDirs
		<- mapM (makeRelativeToCurrentDirectory <=< canonicalizePath)
		$  [fromMaybe "test" (getArgString args ArgTestDir)]

	-- Trace all the files reachable from these directories.
	testFilesRaw
		<- liftM (join . Seq.fromList)
		$  mapM traceFilesFrom testDirs
		
	-- Canonicalize all the paths and put them in a set (which sorts them)
	testFilesSet
		<- liftM (Set.fromList . Seq.toList)
		$  Seq.mapM canonicalizePath
		$  testFilesRaw

	let testFilesSorted
		= filter (not . isInfixOf "skip-")	-- skip over skippable files.
		$ filter (not . isInfixOf "-skip")
		$ filter (not . isInfixOf "war-")	-- don't look at srcs in copied build dirs.
		$ Set.toList testFilesSet

	-- Create test chains based on the files we have.
	let ways'
		= case configWays config of
		   []	-> [Way "std" [] []]
		   ways	-> ways

	let jobChains :: [[Job]]
	    jobChains
		= concat
		$ map (filter (not . null))
		$ [ map (\way -> createJobs config way testFilesSet file) ways'
			| file <- testFilesSorted]

	-- Channel for threads to write their results to.
	(chanResult :: ChanResult)
		<- atomically $ newTChan

	-- Run all the chains.
	results <- runJobChains config chanResult jobChains

	-- Write out a log of failed tests if we were asked to
	when (isJust $ configLogFailed config)
	 $ do   let Just fileLog = configLogFailed config
	        workingDir       <- getCurrentDirectory

	        let diag jr      = diagnoseJobResults
	                                (configFormatPathWidth config)
	                                False -- no color
	                                workingDir
	                                (jobResultJob jr)
	                                (jobResultResults jr)
	                                
	        let ssResults    = [doc | (success, doc) <- map diag results
	                                , not success ]
	        
	        writeFile fileLog ((render $ vcat ssResults) ++ "\n")
	
	return ()


-- | Run some job chains.
runJobChains 
	:: Config 	-- ^ war configuration
	-> ChanResult	-- ^ channel to write job results to
	-> [[Job]]	-- ^ chains of jobs
	-> IO [JobResult]

runJobChains config chanResult jcs
 = do	
	-- Count the total number of chains for the status display.
	let chainsTotal	= length jcs
	
	-- Fork a gang to run all the job chains.
	gang	<- forkGangActions (configThreads config)
	 	$ zipWith (runJobChain config chanResult chainsTotal)
			[1..]
			jcs

	-- Fork the gang controller that manages the console and handles
	-- user input.
	varResults	<- newEmptyMVar
	jobResults      
	 <- forkIO 
	 $ do   results <- controller config gang chainsTotal chanResult
	        putMVar varResults results
	 `finally` (putMVar varResults [])

	-- Wait until the controller to finished
	results <- takeMVar varResults

	-- Wait until the gang is finished running chains, 
	-- or has been killed by the controller.
	joinGang gang
	
	return results
	


-- | Run a job chain, printing the results to the console.
--   If any job in the chain fails, then skip the rest.
runJobChain 
	:: Config		-- ^ war configuration
	-> ChanResult		-- ^ channel to write job results to
	-> Int			-- ^ total number of chains
	-> Int			-- ^ index of this chain
	-> [Job]		-- ^ chain of jobs to run
	-> IO ()

runJobChain config chanResult chainsTotal chainNum chain
 = do	uid		<- getUniqueId
	let state	= (buildStateDefault uid "/tmp")
			{ buildStateLogSystem
				= if configDebug config
					then Just stderr
					else Nothing }
	
	runBuildWithState state
 	 $ zipWithM_ (runJob config chanResult chainNum) [1..] chain

	return ()


-- | Dispatch a single job of a chain.
runJob
	:: Config 		-- ^ war configuration
	-> ChanResult		-- ^ channel to write results to
	-> Int			-- ^ index of this chain
	-> Int			-- ^ index of this job of the chain
	-> Job			-- ^ the job to run
	-> Build ()

runJob config chanResult chainNum jobNum job
 = do	
	-- Run the job
	results		<- dispatchJob job
	
	-- Push the results into the channel for display
	io $ atomically $ writeTChan chanResult 
		(JobResult chainNum jobNum job results)
		
	return ()


-- | Get a unique(ish) id for this process.
--   The random seeds the global generator with the cpu time in psecs,
--   which should be good enough.
getUniqueId :: IO Integer
getUniqueId
 	= randomRIO (0, 1000000000)	

