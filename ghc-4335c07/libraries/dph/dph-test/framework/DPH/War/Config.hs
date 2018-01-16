
module DPH.War.Config
	( Config(..)
	, loadConfig)
where
import DPH.War.Options
import DPH.War.Way
import System.Console.ParseArgs
import Data.List
import Data.Maybe

-- | Configuration information read from command line arguments.
data Config
	= Config {
 	-- | Raw options list passed to war.
 	  configArgs		:: Args MainArg

	-- | Whether to emit debugging info for war.
	, configDebug		:: Bool

	-- | Number of threads to use when running tests.
	, configThreads		:: Int 

	-- | Whether to run in batch mode with no color and no interactive
	--	test failure resolution.
	, configBatch		:: Bool 

	-- | Where to write the list of failed tests to.
	, configLogFailed	:: Maybe FilePath 

	-- | What ways to compile the tests with.
	, configWays		:: [Way] 

	-- | Clean up ddc generated files after each test
	, configClean		:: Bool 
	
	-- | Width of reports.
	, configFormatPathWidth	:: Int }


-- | Load command line arguments into a `Config`.
loadConfig :: Args MainArg -> Config
loadConfig args
 = let
	-- Calculate all the ways we should run the tests
	--	If no options are given for comp or run, then just use
	--	a "normal" way with no options.
	makeWayPair (name:opts)	= (name, opts)
	makeWayPair way		= error $ "bad way specification " ++ intercalate " " way

--	compWayPairs_		= [makeWayPair opts | OptCompWay opts	<- options ]
        compWayPairs_           = []
	compWayPairs		= if null compWayPairs_ 
					then [("std", [])] 
					else compWayPairs_

--	runWayPairs_		= [makeWayPair opts | OptRunWay  opts	<- options ]
        runWayPairs_            = []
	runWayPairs		= if null runWayPairs_ 
					then [("std", [])]
					else runWayPairs_

	ways			= [ Way (compName ++ "-" ++ runName) compOpts runOpts
					| (compName, compOpts)	<- compWayPairs
					, (runName,  runOpts)	<- runWayPairs ]

    in	Config
	{ configArgs		= args
	, configDebug		= gotArg    args ArgVerbose
	, configThreads		= fromMaybe 1 $ getArgInt args ArgJobs
	, configBatch		= False
	, configLogFailed	= Nothing
	, configWays		= ways
	, configClean		= gotArg    args ArgClean
	, configFormatPathWidth	= 80
	}
