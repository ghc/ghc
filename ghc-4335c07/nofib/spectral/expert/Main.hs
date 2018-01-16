{------------------------------------------------------------------------------
                                 EXPERT SYSTEM

This prototype expert system program uses the modules `result.g', `table.g',
`knowledge.g', `match.g' and `search.g'. The main program reads in the file
`animals', treats the first line as the main goal to be solved, and converts
the remaining lines into the table of definitions representing the permanent
knowledge about the problem area. The program then solves the main goal and
displays the questions and solutions to the user, using the answers to the
questions to continue the search for solutions. Each answer should be `yes' or
`no'. After each solution, the user is asked whether the solution is adequate
or whether the search should be continued for alternative solutions.
------------------------------------------------------------------------------}

module Main where
import Result
import Table
import Knowledge
import Match
import Search
import System.IO
import System.Environment

-- The `main' function reads in the data file before interacting with user.
-- The `process' function takes the contents of the file and the input from the
-- user and produces the output. It builds an initial goal and a definition
-- table from the file contents, and an information table from the user's
-- input, and calls the `solve' function. The list of questions and solutions
-- from this call is stripped to remove duplicate questions, and displayed as
-- output.  The questions are also extracted and used to help build the
-- information table which contains question-and-answer pairs.

main = do
    prog <- getProgName
    args <- getArgs
    case args of
      [filename] -> getData filename
      []	 -> getData "runtime_files/animals"
      _		 -> hPutStr stderr ("Usage: " ++ prog ++ " datafile\n")

getData filename = do
    contents <- readFile filename
    interact (process contents)

{- OLD 1.2:
main rs =
   GetProgName : GetArgs :
   let (r0:r1:rrs) = rs in
   case r1 of
      StrList [filename] -> getData filename rrs
      StrList [] -> getData "animals" rrs
      StrList args -> case r0 of
         Str prog -> [AppendChan stderr ("Usage: " ++ prog ++ " datafile\n")]
         Failure _ -> []

getData filename rs =
   ReadFile filename :
   let (r:rrs) = rs in
   case r of
      Failure ioerr -> [AppendChan stderr
         ("Unable to read file " ++ filename ++ "\n")]
      Str contents -> interact (process contents) rrs
-}

process contents input =
   "Solving: " ++ showPhrase problem ++ "\n" ++
   display results (vars problem) replies
   where
   problem = goal (words (head (lines contents)))
   defs = definitions (tail (lines contents))
   info = enterList newTable [(q,a) | (Question q, a) <- zip results replies]
   replies = [words l /= ["no"] | l <- lines input]
   db = (defs,info)
   newsoln = Soln newTable ['X' : show n | n<-[0..]]
   results = strip [] (solve db newsoln problem)

-- The `strip' function takes the list of questions and solutions from the main
-- call to `solve' and removes all but the first occurrence of each question,
-- to make sure that the user is not asked the same question twice. The first
-- argument is a list of the questions seen so far.

strip qs [] = []
strip qs (Question q : rs) =
   if elem q qs then strip qs rs else
   Question q : strip (q:qs) rs
strip qs (soln:rs) = soln : strip qs rs

-- The display function displays a list of questions and solutions as a
-- character stream. It also takes the list of variable names in the original
-- goal to interpret solution environments using `showVars', and the list of
-- answers from the user to determine whether to continue displaying more
-- solutions.

display [] xs as = "No (more) solutions\n"
display (Question q : rs) xs as =
   "Is it true that " ++ q ++ "?\n" ++ display rs xs (tail as)
display (Soln env vs : rs) xs as =
   "Solution: " ++ sol ++ ". More?\n" ++ etc  where
   sol = showVars env xs
   etc = if as == [] || head as == False then "" else display rs xs (tail as)

showVars env vs =
   foldr1 join (map showVar vs) where
   join x y = x ++ "; " ++ y
   showVar v = v ++ " = " ++ showPhrase (subst env (Var v))
