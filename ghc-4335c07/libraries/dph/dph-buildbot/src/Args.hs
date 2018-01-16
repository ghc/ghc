{-# LANGUAGE ScopedTypeVariables #-}

module Args
	( BuildArg(..)
	, buildArgs)
where
import System.Console.ParseArgs

-- Command line args for the buildbot.
data BuildArg
	= ArgHelp
	| ArgVerbose

	-- Automated builds
	| ArgDaily
	| ArgDailyNow
	| ArgDailyTomorrow

	-- Building GHC and libs.
	| ArgScratchDir
	| ArgGhcUnpack
	| ArgGhcBuild
	| ArgGhcUnpackBuild
	| ArgGhcUse
	| ArgLibs

	-- Testing DPH and Repa
	| ArgDoTestDPH
	| ArgDoTestRepa
	| ArgDoTestNoSlow

        | ArgUseDPH
        | ArgUseRepa
        | ArgUseNoSlow

	| ArgTestIterations
	| ArgMailFrom 
	| ArgMailTo
	| ArgMailFailTo
	| ArgMailBanner
	| ArgMailBranchName
	| ArgSendTestMail
	| ArgWriteResults
	| ArgWriteResultsStamped
	| ArgUploadResults
	| ArgAgainstResults
	| ArgSwingFraction
	deriving (Eq, Ord, Show)


buildArgs :: [Arg BuildArg]
buildArgs
 = 	[ Arg	{ argIndex	= ArgHelp
		, argAbbr	= Just 'h'
		, argName	= Just "help"
		, argData	= Nothing
		, argDesc	= "Print this usage help." }

	, Arg	{ argIndex	= ArgVerbose
		, argAbbr	= Just 'v'
		, argName	= Just "verbose"
		, argData	= Nothing
		, argDesc	= "Verbose logging of build commands." }

	-- Automated builds
	, Arg	{ argIndex	= ArgDaily
		, argAbbr	= Nothing
		, argName	= Just "daily"
		, argData	= argDataOptional "time" ArgtypeString
		, argDesc	= "Run the build commands every day at this time. fmt: HH:MM:SS" }

	, Arg	{ argIndex	= ArgDailyNow
		, argAbbr	= Nothing
		, argName	= Just "now"
		, argData	= Nothing
		, argDesc	= "(opt. for --daily) Also run the build right now." }

	, Arg	{ argIndex	= ArgDailyTomorrow
		, argAbbr	= Nothing
		, argName	= Just "tomorrow"
		, argData	= Nothing
		, argDesc	= "(opt. for --daily) Run the first build tomorrow." }


	-- Building GHC and libs.
	, Arg	{ argIndex	= ArgScratchDir
		, argAbbr	= Nothing
		, argName	= Just "scratch"
		, argData	= argDataOptional "dir" ArgtypeString
		, argDesc	= "For --ghc-unpack and --ghc-unpack-build, where to put the unpacked tree." }

	, Arg	{ argIndex	= ArgGhcUnpack
		, argAbbr	= Nothing
		, argName	= Just "ghc-unpack"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Unpack this GHC snapshot and update it from darcs.haskell.org." }

	, Arg	{ argIndex	= ArgGhcBuild
		, argAbbr	= Nothing
		, argName	= Just "ghc-build"
		, argData	= argDataOptional "dir" ArgtypeString
		, argDesc	= "Build an already unpacked and updated GHC snapshot." }

	, Arg	{ argIndex	= ArgGhcUnpackBuild
		, argAbbr	= Nothing
		, argName	= Just "ghc-unpack-build"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "Unpack this GHC snapshot, update, and build it." }

	, Arg	{ argIndex	= ArgGhcUse
		, argAbbr	= Nothing
		, argName	= Just "ghc-use"
		, argData	= argDataOptional "dir" ArgtypeString
		, argDesc	= "Use the previously built GHC in this dir" }

	, Arg	{ argIndex	= ArgLibs
		, argAbbr	= Nothing
		, argName	= Just "ghc-libs"
		, argData	= argDataOptional "spec" ArgtypeString
		, argDesc	= "Install some libraries into the GHC build" }


	-- Testing DPH and Repa
	, Arg	{ argIndex	= ArgDoTestDPH
		, argAbbr	= Nothing
		, argName	= Just "test-dph"
		, argData	= Nothing
		, argDesc	= "Run DPH regression tests." }
		
	, Arg	{ argIndex	= ArgDoTestRepa
		, argAbbr	= Nothing
		, argName	= Just "test-repa"
		, argData	= Nothing
		, argDesc	= "Run Repa regression tests." }

	, Arg	{ argIndex	= ArgDoTestNoSlow
		, argAbbr	= Nothing
		, argName	= Just "test-noslow"
		, argData	= Nothing
		, argDesc	= "Run NoSlow regression tests." }

        , Arg   { argIndex      = ArgUseDPH 
                , argAbbr       = Nothing
                , argName       = Just "use-dph"
                , argData       = argDataOptional "dir" ArgtypeString
                , argDesc       = "Use this DPH repo for testing." }

        , Arg   { argIndex      = ArgUseRepa
                , argAbbr       = Nothing
                , argName       = Just "use-repa"
                , argData       = argDataOptional "dir" ArgtypeString
                , argDesc       = "Use this Repa repo for testing." }

        , Arg   { argIndex      = ArgUseNoSlow
                , argAbbr       = Nothing
                , argName       = Just "use-noslow"
                , argData       = argDataOptional "dir" ArgtypeString
                , argDesc       = "Use this NoSlow repo for testing." }

	, Arg	{ argIndex	= ArgTestIterations
		, argAbbr	= Just 'i'
		, argName	= Just "iterations"
		, argData	= argDataDefaulted "int" ArgtypeInt 1
		, argDesc	= "(opt. for test modes) Number of times to run each benchmark." }

	, Arg	{ argIndex	= ArgAgainstResults
		, argAbbr	= Just 'a'
		, argName	= Just "against"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for test modes) Print running comparison against results in this file." }

	, Arg	{ argIndex	= ArgSwingFraction
		, argAbbr	= Just 's'
		, argName	= Just "swing"
		, argData	= argDataOptional "fraction" ArgtypeDouble
		, argDesc	= "(opt. for test modes) Treat a fractional swing vs the baseline as interesting (eg 0.1)" }

	, Arg	{ argIndex	= ArgWriteResults
		, argAbbr	= Just 'w'
		, argName	= Just "write"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for test modes) Write results to this file." }

	, Arg	{ argIndex	= ArgWriteResultsStamped
		, argAbbr	= Just 'p'
		, argName	= Just "write-stamped"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for test modes)  ... appending a time stamp to the name." }		

	, Arg	{ argIndex	= ArgUploadResults
		, argAbbr	= Just 'u'
		, argName	= Just "upload"
		, argData	= argDataOptional "scp-path" ArgtypeString
		, argDesc	= "(opt. for test modes)  ... and scp the results to this path." }

	, Arg	{ argIndex	= ArgMailFrom
		, argAbbr	= Nothing
		, argName	= Just "mail-from"
		, argData	= argDataOptional "address" ArgtypeString
		, argDesc	= "(opt. for test modes) Send test results from this address." }

	, Arg	{ argIndex	= ArgMailTo
		, argAbbr	= Nothing
		, argName	= Just "mail-to"
		, argData	= argDataOptional "address" ArgtypeString
		, argDesc	= "(opt. for test modes)  ... to this address." }			

	, Arg	{ argIndex	= ArgMailFailTo
		, argAbbr	= Nothing
		, argName	= Just "mail-fail-to"
		, argData	= argDataOptional "address" ArgtypeString
		, argDesc	= "(opt. for test modes)  ... but send failure messages to this other address." }			

	, Arg	{ argIndex	= ArgMailBanner
		, argAbbr	= Nothing
		, argName	= Just "mail-banner"
		, argData	= argDataOptional "file" ArgtypeString
		, argDesc	= "(opt. for test modes)  ... appending the banner to the front of the message." }

        , Arg   { argIndex      = ArgMailBranchName
                , argAbbr       = Nothing
                , argName       = Just "mail-branch-name"
                , argData       = argDataOptional "name" ArgtypeString
                , argDesc       = "(opt. for test modes)  ... putting this branch name in the subject." }

	-- Setup debugging
	, Arg	{ argIndex	= ArgSendTestMail
		, argAbbr	= Nothing
		, argName	= Just "send-test-mail"
		, argData	= Nothing
		, argDesc	= "Send a test mail to check mailer configuration." }
	]
