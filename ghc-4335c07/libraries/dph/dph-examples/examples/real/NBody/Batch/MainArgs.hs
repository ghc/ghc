
module Batch.MainArgs
	( MainArg(..)
	, mainArgs)
where
import System.Console.ParseArgs

data MainArg
	= ArgHelp
	| ArgSolver
	| ArgMaxSteps
	| ArgTimeStep
	| ArgBodyCount
	| ArgBodyMass
	| ArgEpsilon
	| ArgDiscSize
	| ArgStartSpeed
	| ArgDumpFinal
	| ArgPrintFinal
	| ArgPrintTimings
	deriving (Eq, Ord, Show)
	
mainArgs :: [Arg MainArg]
mainArgs
 = 	[ Arg	{ argIndex	= ArgHelp
		, argAbbr	= Just 'h'
		, argName	= Just "help"
		, argData	= Nothing
		, argDesc	= "Print this usage help." }

	-- Solver selection.
	, Arg	{ argIndex	= ArgSolver
		, argAbbr	= Just 's'
		, argName	= Just "solver"
		, argData	= argDataDefaulted "name" ArgtypeString "vector-bh"
		, argDesc	= "One of: list-bh, vector-naive, vector-bh, nested-bh. (default vector-bh)" }

	-- Simulation setup.
	, Arg	{ argIndex	= ArgTimeStep
		, argAbbr	= Just 't'
		, argName	= Just "timestep"
		, argData	= argDataDefaulted "Double" ArgtypeDouble 1
		, argDesc	= "Time step between states (default 1)" }

	, Arg	{ argIndex	= ArgBodyCount
		, argAbbr	= Just 'b'
		, argName	= Just "bodies"
		, argData	= argDataDefaulted "Int" ArgtypeInt 200 
		, argDesc	= "Number of bodies in simulation (default 200)" }

	, Arg	{ argIndex	= ArgBodyMass
		, argAbbr	= Just 'm'
		, argName	= Just "mass"
		, argData	= argDataDefaulted "Double" ArgtypeDouble 10
		, argDesc	= "Mass of each body (default 10)" }

	, Arg	{ argIndex	= ArgEpsilon
		, argAbbr	= Just 'e'
		, argName	= Just "epsilon"
		, argData	= argDataDefaulted "Double" ArgtypeDouble 100
		, argDesc	= "Smoothing parameter (default 100)" }
		
	, Arg	{ argIndex	= ArgDiscSize
		, argAbbr	= Just 'd'
		, argName	= Just "disc"
		, argData	= argDataDefaulted "Double" ArgtypeDouble 50
		, argDesc	= "Starting size of disc containing bodies (default 50)" }

	, Arg	{ argIndex	= ArgStartSpeed
		, argAbbr	= Just 'p'
		, argName	= Just "speed"
		, argData	= argDataDefaulted "Double" ArgtypeDouble 0.5
		, argDesc	= "Starting rotation speed of bodies (default 0.5)" }

	, Arg	{ argIndex	= ArgMaxSteps
		, argAbbr	= Nothing
		, argName	= Just "max-steps"
		, argData	= argDataDefaulted "steps" ArgtypeInt 1000
		, argDesc	= "Exit simulation after this many steps (default 1000)" }

	, Arg	{ argIndex	= ArgDumpFinal
		, argAbbr	= Nothing
		, argName	= Just "dump-final"
		, argData	= argDataOptional "FilePath" ArgtypeString
		, argDesc	= "Dump final body positions and masses to file" }

	, Arg	{ argIndex	= ArgPrintFinal
		, argAbbr	= Nothing
		, argName	= Just "print-final"
		, argData	= Nothing
		, argDesc	= "Print final body positions and masses to stdout" }

	, Arg	{ argIndex	= ArgPrintTimings
		, argAbbr	= Nothing
		, argName	= Just "print-timings"
		, argData	= Nothing
		, argDesc	= "Print timings of the solver" }

	]
