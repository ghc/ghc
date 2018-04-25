{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

-- DPH buildbot
-- 	Used to automate building and performance testing of GHC, DPH and Repa
--
--	TODO: Capture output of system commands for logging on website.
--	      Make a log file for each of the stages, and post to web site along with results file.
--	      We might need to write a "tee" function in Haskell
--
--	TODO: Set number of threads to test with for Repa on cmd line.
--
import BuildBox
import Args
import Config
import BuildGhc
import BuildLibs
import BuildTest
import Control.Monad
import Control.Monad.Error.Class
import System.Console.ParseArgs	hiding (args)
import System.IO
import System.Random
import Data.Maybe

main :: IO ()
main 
 = do	args	<- parseArgsIO (ArgsTrailing "files") buildArgs
	mainWithArgs args


-- | Decide what to do
mainWithArgs :: Args BuildArg -> IO ()
mainWithArgs args

	-- Print usage help
	| gotArg args ArgHelp
	= usageError args ""

	-- Send a test mail
	| gotArg args ArgSendTestMail
	= successfully . runBuildPrint "/tmp"
	$ do	outLn "Sending test mail."
		let from	= fromMaybe (error "you must specify --mailfrom with --send-test-mail")
					    (getArg args ArgMailFrom) 
	
		let to		= fromMaybe (error "you must specify --mailto with --send-test-mail")
					    (getArg args ArgMailTo)
				
		mail	<- createMailWithCurrentTime from to "DDC BuildBot test mail" "Looks like it worked..."
		sendMailWithMailer mail defaultMailer
		
		io $ writeFile "dph-buildbot.mail" (render $ renderMail mail)
		return ()

	
	-- Run some build process.
	| (or $ map (gotArg args)
		[ ArgGhcUnpack,  ArgGhcBuild,  ArgGhcUnpackBuild
		, ArgLibs
		, ArgDoTestRepa, ArgDoTestDPH, ArgDoTestNoSlow])

	= do	-- Load up cmd line args into our config structure.
		config		<- slurpConfig args
		uid :: Integer	<- liftM fromIntegral $ getStdRandom (randomR (0 :: Integer, 1000000))
		let buildState
			= (buildStateDefault uid "/tmp")
			{ buildStateLogSystem	= if gotArg args ArgVerbose
			 				then Just stdout
							else Nothing }
							
		-- Decide if we're doing a daily, or one-shot build.
		if gotArg args ArgDaily
		 then	mainDaily args config buildState
		 else	mainBuild args config buildState

	| otherwise
	= usageError args "Nothing to do...\n"


-- | Run the build every day.
mainDaily :: Args BuildArg -> Config -> BuildState -> IO ()
mainDaily args config buildState
 = do	putStrLn "* DPH Build bot starting up."

	curTime	<- getZonedTime
	putStrLn $ "  - current time is " ++ show curTime
	putStrLn $ "  - waiting for build time..."
	
	-- We're assuming the time on the cmd line is specified
	-- in the machines local time.
	let Just strTimeOfDay	= getArg args ArgDaily
	buildUTC	<- readLocalTimeOfDayAsUTC strTimeOfDay

	mtmUTC 		<- getMidnightTomorrowUTC

	-- Build the schedule.
	let whenModifier
		| gotArg args ArgDailyNow 	= Just Immediate
		| gotArg args ArgDailyTomorrow	= Just (WaitUntil mtmUTC)
		| otherwise			= Nothing

	let schedule
		= makeSchedule 
			[ ("build"
			  , Daily buildUTC
			  , whenModifier
			  , runTotalCatch config) ]
	
	-- Run the loop.
	-- NOTE: We need to seq on buildUTC incase the string isn't going to parse.
	_ 	<-    buildUTC 
		`seq` runBuildPrintWithState buildState
			$ cronLoop schedule
	return ()
	

-- | Run a single-shot build.
mainBuild :: Args BuildArg -> Config -> BuildState -> IO ()
mainBuild _args config buildState
 = do	_	<- runBuildPrintWithState buildState
			$ runTotalCatch config
	return ()


-- | A wrapper for the build that sends mail if it fails.
runTotalCatch :: Config -> Build ()
runTotalCatch config
 = do	catchError (runTotal config)
 	 $ \err -> do
		outBlank
		outLine
		outLn "* Build Failed"
		outBlank
		outLn $ render $ ppr err
		maybe 	(return ())
			(\(from, to) -> do
				-- target for fail messages can be overridden
				let failto	= fromMaybe to (configMailFailTo config)

				outBlank
				outLn $ "* Mailing result to " ++ to 

				mail	<- createMailWithCurrentTime from failto "[nightly] DPH Performance Test FAILED :-("
					$ render $ vcat
					[ text "DPH Performance Build Failed :-("
					, blank
					, ppr err
					, blank ]

				sendMailWithMailer mail defaultMailer				
				return ())
			(configMailFromTo config)

	outLn "* Build finished"
	outLINE

			
-- | The total build. 
--   This only runs the stages set in the config.
runTotal :: Config -> Build ()
runTotal config
 = do	outLINE
	outLn "* Starting Build\n"
	
	-- Check the current environment.
	env	<- getEnvironmentWith 
			[ ("GHC", getVersionGHC $ configWithGhc config)
			, ("GCC", getVersionGCC "gcc") ]
			
	outLn $ render $ ppr $ env
	
	outBlank
	
	-- Unpack GHC
	let ghcUnpack'
		| Just snapShot		<- configGhcUnpack config
		, Just scratchDir	<- configScratchDir config
		= ghcUnpack snapShot scratchDir
		
		| otherwise
		= return ()
		
	ghcUnpack'
			
	-- If we've been told to build GHC, then use
	-- 	the completed build as the default compiler.
	let ghcBuild'
		| Just buildPath	<- configGhcBuild config
		= do	ghcBuild buildPath
			return config
				{ configWithGhc	   = buildPath ++ "/inplace/bin/ghc-stage2"
				, configWithGhcPkg = buildPath ++ "/inplace/bin/ghc-pkg" }
			
		| otherwise
		= return config
		
	configNew	<- ghcBuild'
	
			
	-- Install libraries into the GHC build.
	when (isJust $ configLibs configNew)
	 $ libsBuild configNew
						
	-- Run benchmarks and write results to file, or mail them to the list.
	when (or $ map isJust 
	         [ configDoTestRepa configNew
	         , configDoTestDPH configNew
	         , configDoTestNoSlow configNew])
	 $ buildTest configNew env


	