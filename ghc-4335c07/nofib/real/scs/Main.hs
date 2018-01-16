module Main(main) where

import Data.Array
import System.Environment

import Parse
import Simulate
import Types

main :: IO ()
main = do
	args <- getArgs
	if (length args < 4 || length args > 6)
	  then putStr (unlines ["Set Circuit Simulator", "scs <file> <seed> <dt> <end time> [<temperature>] [<random background charge>]"])
	  else do
	  	let [file, seed', dt', end_time', temperature', rbc'] = take 6 (args ++ ["0", "0"])
	        
  		input <- readFile (file ++ ".in")
		let
			(circuit, names)		= get (parse_circuit input       )  "Syntactic error in circuit description"
			seed				= get (parse_integer seed'       ) ("Can't read seed: "                     ++ seed'       )
			dt				= get (parse_exact   dt'         ) ("Can't read dt: "                       ++ dt'         )
			end_time			= get (parse_exact   end_time'   ) ("Can't read end time: "                 ++ end_time'   )
			temperature			= get (parse_approx  temperature') ("Can't read temperature: "              ++ temperature')
			rbc				= get (parse_approx  rbc'        ) ("Can't read random background charge: " ++ rbc'        )
			trace				= simulate circuit seed dt end_time temperature rbc
			output	| dt <= 0		= error "dt must be positive"
				| end_time <= 0		= error "end time must be positive"
				| temperature < 0	= error "temperature must be nonnegative"
				| otherwise		= list names trace
		putStr output
  where
  	get [e] _ = e
	get  _  s = error s
	
scs :: Name -> Seed -> Time -> Time -> Temperature -> RBC -> IO ()
scs file seed dt end_time temperature rbc = do
		input <- readFile (file ++ ".in")
		let
			(circuit, names)	= head (parse_circuit input)
			trace			= simulate circuit seed dt end_time temperature rbc
			output			= list names trace
		putStr output

list :: [Name] -> [(Time, Vector Approx)] -> String
list names trace = unlines (header : results)
  where
	header	= "t:Time\t" ++ show (tail names)
	results	= map (\(t, v) -> (show . circa) t ++ "\t" ++ (show . elems) v) trace
