module Main where
import Char
import List
import IO
import System.Environment
import System.Console.GetOpt
import System.Exit


-- search for definitions of things 
-- we do this by looking for the following patterns:
-- data XXX = ...      giving a datatype location
-- newtype XXX = ...   giving a newtype location
-- bla :: ...          giving a function location
--
-- by doing it this way, we avoid picking up local definitions
-- 		(whether this is good or not is a matter for debate)
--

-- We generate both CTAGS and ETAGS format tags files
-- The former is for use in most sensible editors, while EMACS uses ETAGS


main :: IO ()
main = do
        progName <- getProgName
	args <- getArgs
        let usageString = "Usage: " ++ progName ++ " [OPTION...] [files...]"
	let (modes, filenames, errs) = getOpt Permute options args
	if errs /= [] || elem Help modes || filenames == []
         then do
           putStr $ unlines errs 
	   putStr $ usageInfo usageString options
	   exitWith (ExitFailure 1)
         else return ()
        let mode = getMode (Append `delete` modes)
        let openFileMode = if elem Append modes
			   then AppendMode
			   else WriteMode
	filedata <- mapM findthings filenames
        if mode == BothTags || mode == CTags
         then do 
           ctagsfile <- openFile "tags" openFileMode
	   writectagsfile ctagsfile filedata
           hClose ctagsfile
         else return ()
	if mode == BothTags || mode == ETags 
         then do
           etagsfile <- openFile "TAGS" openFileMode
	   writeetagsfile etagsfile filedata
           hClose etagsfile
         else return ()

-- | getMode takes a list of modes and extract the mode with the
--   highest precedence.  These are as follows: Both, CTags, ETags
--   The default case is Both.
getMode :: [Mode] -> Mode
getMode [] = BothTags
getMode [x] = x
getMode (x:xs) = max x (getMode xs)


data Mode = ETags | CTags | BothTags | Append | Help deriving (Ord, Eq, Show)

options :: [OptDescr Mode]
options = [ Option "c" ["ctags"]
	    (NoArg CTags) "generate CTAGS file (ctags)"
	  , Option "e" ["etags"]
	    (NoArg ETags) "generate ETAGS file (etags)"
	  , Option "b" ["both"]
	    (NoArg BothTags) ("generate both CTAGS and ETAGS")
	  , Option "a" ["append"]
	    (NoArg Append) ("append to existing CTAGS and/or ETAGS file(s)")
	  , Option "h" ["help"] (NoArg Help) "This help"
	  ]

type FileName = String

type ThingName = String

-- The position of a token or definition
data Pos = Pos 
		FileName 	-- file name
		Int			-- line number 
		Int     	-- token number
		String 		-- string that makes up that line
	deriving Show

-- A definition we have found
data FoundThing = FoundThing ThingName Pos
	deriving Show

-- Data we have obtained from a file
data FileData = FileData FileName [FoundThing]

data Token = Token String Pos
	deriving Show


-- stuff for dealing with ctags output format

writectagsfile :: Handle -> [FileData] -> IO ()
writectagsfile ctagsfile filedata = do
	let things = concat $ map getfoundthings filedata
	mapM_ (\x -> hPutStrLn ctagsfile $ dumpthing x) things

getfoundthings :: FileData -> [FoundThing]
getfoundthings (FileData filename things) = things

dumpthing :: FoundThing -> String
dumpthing (FoundThing name (Pos filename line _ _)) = 
	name ++ "\t" ++ filename ++ "\t" ++ (show $ line + 1)


-- stuff for dealing with etags output format

writeetagsfile :: Handle -> [FileData] -> IO ()
writeetagsfile etagsfile filedata = do
	mapM_ (\x -> hPutStr etagsfile $ e_dumpfiledata x) filedata

e_dumpfiledata :: FileData -> String
e_dumpfiledata (FileData filename things) = 
	"\x0c\n" ++ filename ++ "," ++ (show thingslength) ++ "\n" ++ thingsdump
	where 
		thingsdump = concat $ map e_dumpthing things 
		thingslength = length thingsdump

e_dumpthing :: FoundThing -> String
e_dumpthing (FoundThing name (Pos filename line token fullline)) =
	(concat $ take (token + 1) $ spacedwords fullline) 
	++ "\x7f" ++ (show line) ++ "," ++ (show $ line+1) ++ "\n"
	
	
-- like "words", but keeping the whitespace, and so letting us build
-- accurate prefixes	
	
spacedwords :: String -> [String]
spacedwords [] = []
spacedwords xs = (blanks ++ wordchars):(spacedwords rest2)
	where 
		(blanks,rest) = span Char.isSpace xs
		(wordchars,rest2) = span (\x -> not $ Char.isSpace x) rest
	
	
-- Find the definitions in a file	
	
findthings :: FileName -> IO FileData
findthings filename = do
	text <- readFile filename
        evaluate text -- forces evaluation of text
                      -- too many files were being opened otherwise since
                      -- readFile is lazy
	let aslines = lines text
	let wordlines = map words aslines
	let noslcoms = map stripslcomments wordlines
	let tokens = concat $ zipWith3 (withline filename) noslcoms 
					aslines [0 ..]
	let nocoms = stripblockcomments tokens
	return $ FileData filename $ findstuff nocoms
  where evaluate [] = return ()
        evaluate (c:cs) = c `seq` evaluate cs
	
-- Create tokens from words, by recording their line number
-- and which token they are through that line

withline :: FileName -> [String] -> String -> Int -> [Token]		
withline filename words fullline i = 
	zipWith (\w t -> Token w (Pos filename i t fullline)) words $ [0 ..]

-- comments stripping

stripslcomments :: [String] -> [String]
stripslcomments ("--":xs) = []
stripslcomments (x:xs) = x : stripslcomments xs 
stripslcomments [] = []

stripblockcomments :: [Token] -> [Token]
stripblockcomments ((Token "\\end{code}" _):xs) = afterlitend xs
stripblockcomments ((Token "{-" _):xs) = afterblockcomend xs
stripblockcomments (x:xs) = x:stripblockcomments xs
stripblockcomments [] = []

afterlitend2 :: [Token] -> [Token]
afterlitend2 (x:xs) = afterlitend xs
afterlitend2 [] = []

afterlitend :: [Token] -> [Token]
afterlitend ((Token "\\begin{code}" _):xs) = xs
afterlitend (x:xs) = afterlitend xs
afterlitend [] = []

afterblockcomend :: [Token] -> [Token]
afterblockcomend ((Token token _):xs) | contains "-}" token = xs
						| otherwise = afterblockcomend xs
afterblockcomend [] = []


-- does one string contain another string

contains :: Eq a => [a] -> [a] -> Bool
contains sub full = any (isPrefixOf sub) $ tails full 

ints :: Int -> [Int]
ints i = i:(ints $ i+1)


-- actually pick up definitions

findstuff :: [Token] -> [FoundThing]
findstuff ((Token "data" _):(Token name pos):xs) = 
	FoundThing name pos : (getcons xs) ++ (findstuff xs)
findstuff ((Token "newtype" _):(Token name pos):xs) = 
	FoundThing name pos : findstuff xs
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

