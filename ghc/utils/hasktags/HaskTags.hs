module Main where
import System
import Char
import List


-- search for definitions of things 
-- we do this by looking for the following patterns:
-- data XXX = ...      giving a datatype location
-- newtype XXX = ...   giving a newtype location
-- bla :: ...          giving a function location
--
-- by doing it this way, we avoid picking up local definitions


main :: IO ()
main = do
	filenames <- getArgs
	foundthings <- mapM findthings filenames
	mapM_ (\x -> putStrLn $ dumpthing x) (concat foundthings)
	
type FileName = String

type ThingName = String

data Pos = Pos FileName Int
	deriving Show

data FoundThing = FoundThing ThingName Pos
	deriving Show

dumpthing :: FoundThing -> String
dumpthing (FoundThing name (Pos filename line)) = 
	name ++ "\t" ++ filename ++ "\t" ++ (show line)

data Token = Token String Pos
	deriving Show

findthings :: FileName -> IO [FoundThing]
findthings filename = 
	do
		text <- readFile filename
		let aslines = lines text
		let wordlines = map words aslines 
		let nocoms = map stripslcomments wordlines
		let tokens = concat $ zipWith (withline filename) nocoms $ ints 0	
		return $ findstuff tokens
	
withline :: FileName -> [String] -> Int -> [Token]
withline fname words i = map (\w -> Token w (Pos fname i)) words 

stripslcomments :: [String] -> [String]
stripslcomments ("--":xs) = []
stripslcomments (x:xs) = x : stripslcomments xs 
stripslcomments [] = []

ints :: Int -> [Int]
ints i = i:(ints $ i+1)

findstuff :: [Token] -> [FoundThing]
findstuff ((Token "data" _):(Token name pos):xs) = 
	FoundThing name pos : (getcons xs) ++ (findstuff xs)
findstuff ((Token "type" _):(Token name pos):xs) = 
	FoundThing name pos : findstuff xs
findstuff ((Token name pos):(Token "::" _):xs) = 
	FoundThing name pos : findstuff xs
findstuff (x:xs) = findstuff xs
findstuff [] = []


-- get the constructor definitions, knowing that a datatype has just started

getcons :: [Token] -> [FoundThing]
getcons ((Token "=" _):(Token name pos):xs) = 
	FoundThing name pos : getcons2 xs
getcons (x:xs) = getcons xs
getcons [] = []


getcons2 ((Token "=" _):xs) = []
getcons2 ((Token "|" _):(Token name pos):xs) = 
	FoundThing name pos : getcons2 xs
getcons2 (x:xs) = getcons2 xs
getcons2 [] = []


