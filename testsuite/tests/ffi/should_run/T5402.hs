module Test where
import System.Exit
foreign export ccall hsmain :: IO ()
hsmain = exitWith (ExitFailure 42)
