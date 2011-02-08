
 module Main (main) where

 import GenUtils
 import DataTypes
 import Parser
 import Interp
 import PrintTEX

 import System.Environment -- 1.3 (partain)
 import Data.Char -- 1.3

 --fakeArgs = "game001.txt"
 --fakeArgs = "pca2.pgn"
 --fakeArgs = "silly.pgn"
 --fakeArgs = "small.pgn"
 --fakeArgs = "sicil.pgn"
 --fakeArgs = "badgame.pgn"
 --fakeArgs = "mycgames.pgn"
 fakeArgs = "rab.pgn"

 version = "0.3"


 main = do
       [test_dir] <- getArgs
       let (style,fn,filename) = interpArgs (words "-d tex mygames.pgn")
       file <- readFile (test_dir ++ "/" ++filename)
       std_in <- getContents
       let games = pgnParser fn file   -- parse relavent pgn games
       putStr (prog style std_in games)

{- OLD 1.2:
 main = 
       getArgs         abort                           $ \ args ->
       --let args = (words "-d tex analgames.pgn") in
       let (style,fn,filename) = interpArgs args in
       readFile filename abort                         $ \ file ->
       readChan stdin abort                            $ \ std_in ->
       let games = pgnParser fn file   -- parse relavent pgn games
       in
       appendChan stdout (prog style std_in games) abort done
-}

 interpArgs :: [String] -> (OutputStyle,Int -> Bool,String)
 --interpArgs [] = (ViewGame,const True,fakeArgs)
 interpArgs [] = interpArgs (words "-d pgn analgames.pgn")
 interpArgs files = interpArgs' OutputPGN (const True) files

 interpArgs' style fn ("-d":"pgn":xs)    = interpArgs' OutputPGN    fn xs
 interpArgs' style fn ("-d":"rawpgn":xs) = interpArgs' OutputRawPGN fn xs
 interpArgs' style fn ("-d":"play":xs)   = interpArgs' ViewGame     fn xs
 interpArgs' style fn ("-d":"parser":xs) = interpArgs' OutputParser fn xs
 interpArgs' style fn ("-d":"tex":xs)    = interpArgs' OutputTEX    fn xs
 interpArgs' style fn ("-d":"head":xs)   = interpArgs' OutputHeader fn xs
 interpArgs' style fn ("-g":range:xs) 
       = interpArgs' style (changeFn (parse range)) xs
    where
       changeFn (Digit n:Line:Digit m:r) x = moreChangeFn r x || x >= n && x <= m 
       changeFn (Line:Digit m:r) x = moreChangeFn r x || x <= m 
       changeFn (Digit n:Line:r) x = moreChangeFn r x || x >= n 
       changeFn (Digit n:r) x = moreChangeFn r x || x == n
       changeFn _ _ = rangeError
       moreChangeFn [] = const False
       moreChangeFn (Comma:r) = changeFn r
       moreChangeFn _ = rangeError
       parse xs@(n:_) 
               | isDigit n = case span isDigit xs of
                               (dig,rest) -> Digit (read dig) : parse rest
       parse ('-':r) = Line : parse r
       parse (',':r) = Comma : parse r
       parse [] = []
       parse _ = rangeError
       rangeError = error ("incorrect -g option (" ++ range ++ ")\n")

 interpArgs' style fn [file] = (style,fn,file)
 interpArgs' style fn args = error ("bad args: " ++ unwords args)

 data Tok 
       = Digit Int             -- n
       | Line                  -- -
       | Comma                 -- ,

 data OutputStyle

       = OutputPGN             -- pgn
       | OutputRawPGN          -- rawpgn
       | OutputHeader          -- header
       | ViewGame              -- play
       | ViewGameEmacs         -- emacs
       | TwoColumn             -- 2col
       | TestGames             -- test
       | OutputTEX



       | OutputParser  -- simply dump out the string read in.
       | CmpGen        -- cmp 2nd and 3rd generations of output 



 prog  :: OutputStyle          -- style of action
       -> String               -- stdin (for interactive bits)
       -> [AbsGame]            -- input games
       -> String               -- result
 prog OutputPGN _
               = pgnPrinter True       -- print out game(s)
               . map runInterp         -- interprete all games
 prog OutputRawPGN _
               = pgnPrinter False      -- print out game(s)
               . map runInterp         -- interprete all games
 prog OutputHeader _
               = pgnHeadPrinter        -- print out game(s) headers
               . map runInterp         -- interprete all games
 prog OutputTEX _
               = texPrinter            -- print out game(s)
               . map runInterp         -- interprete all games
 prog ViewGame std_in
               = interactViewer std_in -- print out game(s)
               . runInterp             -- interprete the game
               . head                  -- should check for only *one* object
 prog OutputParser _ 
               = userFormat









 type PrintState = (Bool,MoveNumber) 

 pgnPrinter :: Bool -> [RealGame] -> String
 pgnPrinter detail = unlines . concat . map printGame
   where
       printMoveNumber :: Bool -> MoveNumber -> String
       printMoveNumber False (MoveNumber _ Black) = ""
       printMoveNumber _     mvnum = userFormat mvnum ++ " "

       printQuantums :: PrintState -> [Quantum] -> [String]
       printQuantums ps = concat . fst . mapAccumL printQuantum ps

       printQuantum :: PrintState -> Quantum -> ([String],PrintState)
       printQuantum (pnt,mv) (QuantumMove move ch an brd) =
               ([printMoveNumber pnt mv ++ move ++ ch],(False,incMove mv))
       printQuantum (pnt,mv) (QuantumNAG i) = 
               if detail
               then (["$" ++ show i],(False,mv))
               else ([],(False,mv))
       printQuantum (pnt,mv) (QuantumComment comms) = 
               if detail
               then ("{" : comms ++ ["}"],(True,mv))
               else ([],(False,mv))
       printQuantum (pnt,mv) (QuantumAnalysis anal) = 
               if detail
               then ("(" : printQuantums (True,decMove mv) anal ++ [")"],
                       (True,mv))
               else ([],(False,mv))
       printQuantum (pnt,mv) (QuantumResult str) = ([str],(True,mv))
       printQuantum _ _ = error "PANIC: strange Quantum"

       printGame :: RealGame -> [String]
       printGame (Game tags qu) = 
               [ userFormat tag | tag <- tags] ++
               formatText 75 (printQuantums (False,initMoveNumber) qu)



 printHeadGame :: RealGame -> [String]
 printHeadGame (Game tags qu) = [
       rjustify 4 gameno ++ " " ++
       take 20 (rjustify 20 white) ++ " - " ++ 
       take 20 (ljustify 20 black) ++ " " ++ 
       take 26 (ljustify 28 site) ++ " " ++ result ]
   where
       (date,site,game_no,res,white,black,opening) = getHeaderInfo tags
       gameno = case game_no of
                 Nothing -> ""
                 Just n -> show n
       result = userFormat res

 pgnHeadPrinter :: [RealGame] -> String
 pgnHeadPrinter = unlines . concat . map printHeadGame





 interactViewer :: String -> RealGame -> String
 interactViewer stdin (Game tags qu) = replayQ qu (lines stdin)

 replayQ (QuantumMove _ _ _ brd:rest) std_in 
       = "\027[H" ++ userFormat brd ++ waitQ rest std_in
 replayQ (_:rest) std_in = replayQ rest std_in
 replayQ [] _ = []

 waitQ game std_in = ">>" ++ 
    (case std_in of
       [] -> ""
       (q:qs) -> replayQ game qs)


