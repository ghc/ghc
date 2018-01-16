import System.Exit (exitWith, ExitCode(ExitFailure))

main = exitWith $ ExitFailure 42

