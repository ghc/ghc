
 module PrintTEX (texPrinter) where

 import GenUtils
 import DataTypes
 import Data.Array -- 1.3
 import Data.Char -- 1.3







 splitUpQuantum :: [Quantum] -> [[Quantum]]
 splitUpQuantum q = splitUpQuantums q []
   where
       splitUpQuantums [] [] = []
       splitUpQuantums [] mvs = [reverse mvs]
       splitUpQuantums (mv@(QuantumMove _ _ _ _):rest) mvs
               = splitUpQuantums rest (mv:mvs)
       splitUpQuantums (mv@(QuantumNAG _):rest) mvs
               = splitUpQuantums rest mvs
       splitUpQuantums (x:xs) [] = [x] : splitUpQuantums xs []
       splitUpQuantums (x:xs) mvs 
               = [reverse mvs,[x]] ++ splitUpQuantums xs []

 type TeXState = 
       (Bool,          -- if Top level !
       Board,          -- current board
       MoveNumber)     -- the Current Move Number

 printTeXQuantums :: TeXState -> [Quantum] -> [String]
 printTeXQuantums ps
       = concat . fst . mapAccumL printTeXQuantum ps . splitUpQuantum

 printTeXQuantum :: TeXState -> [Quantum] -> ([String],TeXState)
 printTeXQuantum state@(_,board,_) [QuantumComment ["\004"]] = 
       (mkTeXBoard board,state)
 printTeXQuantum state@(_,board,_) [QuantumComment (('\004':comm):comms)] = 
       (mkTeXBoard board ++ formatText 70 (parseSquiggles (comm:comms)),state)
 printTeXQuantum state [QuantumComment comms] = 
       (formatText 70 (parseSquiggles comms),state)
 printTeXQuantum (pnt,brd,mv) [QuantumAnalysis anal] =
       (printTeXQuantums (False,err,decMove mv) anal,(pnt,brd,mv))
   where err = error "Syntax error using ^D"
 printTeXQuantum state@(_,board,_) [QuantumResult str] = 
       (mkTeXBoard board ++ [printTeXResult (mkResult str)],state)
 printTeXQuantum state mvs@(QuantumMove _ _ _ _:_) =
       printTeXMoves state mvs
 printTeXQuantum _ _ = error "PANIC: strange Quantum"


 parseSquiggles = map parseSquiggle
 parseSquiggle ('<':'s':'a':'w':'>':r) = "\\wbetter{}" ++ r
 parseSquiggle ('<':'a':'w':'>':r)     = "\\wupperhand{}" ++ r
 parseSquiggle ('<':'w':'a':'w':'>':r) = "\\wdecisive{}" ++ r
 parseSquiggle ('<':'s':'a':'b':'>':r) = "\\bbetter{}" ++ r
 parseSquiggle ('<':'a':'b':'>':r)     = "\\bupperhand{}" ++ r
 parseSquiggle ('<':'w':'a':'b':'>':r) = "\\bdecisive{}" ++ r
 parseSquiggle wd = wd


 printTeXResult :: Result -> String
 printTeXResult Win     = "$1\\!-\\!0$"
 printTeXResult Loss    = "$0\\!-\\!1$"
 printTeXResult Draw    = "${1 \\over 2}\\!-\\!{1 \\over 2}$"
 printTeXResult Unknown = "$*$"

 printTeXMoves (tl,_,mv) mvs 
       = ([text],(True,brd,incMove last_mv_num))
    where
       aux_mvs = zip3 mvs (iterate incMove mv) (False:repeat True)

       (QuantumMove _ _ _ brd,last_mv_num,_) = last aux_mvs
       text = initText tl
           ++ concat (fst (mapAccumL (pntMove tl) (mv,False) mvs))
           ++ endText tl 

       initText False = 
            case mv of
               MoveNumber i Black -> "|" ++ show i ++ "\\ldots~"
               _ -> "|"
       initText True = 
               "\\begin{center}|\n" ++
               "{\\bf" ++
               "\\begin{tabular}{rp{50pt}p{50pt}}\n" ++
            case mv of
               MoveNumber i Black -> show i ++ " & \\ldots"
               _ -> ""

       endText True = case getMoveColour last_mv_num of
               White -> "&\\\\\n\\end{tabular}}|\n\\end{center}"
               Black -> "\\end{tabular}}|\n\\end{center}"
       endText False =  "|"



       pntMove True (mv@(MoveNumber i White),bl) move
               = (show i ++ " & " 
               ++ printableMove move,
                 (incMove mv,True))
       pntMove True (mv@(MoveNumber i Black),bl) move
               = (" & " ++ printableMove move ++ "\\\\\n",
                 (incMove mv,True))
       pntMove False (mv@(MoveNumber i White),bl) move
               = ((if bl then "; " else "") ++ show i ++ ".~"
                       ++ printableMove move,
                 (incMove mv,True))
       pntMove False (mv@(MoveNumber i Black),bl) move
               = ((if bl then ", " else "") ++ printableMove move,
                 (incMove mv,True))

 printableMove :: Quantum -> String
 printableMove (QuantumMove move ch an _) = map fn move ++ rest
    where
       fn 'x' = '*'
       fn 'O' = '0'
       fn c   = c
       rest = case ch of
               "#" -> an ++ " mate"
               _   -> ch ++ an

 mkTeXBoard :: Board -> [String]
 mkTeXBoard (Board arr _ _) = 
       ["\n\\board"] ++
       ["{" ++ [ fn ((x-y) `rem` 2 == 0) (arr ! (x,y)) | x <- [1..8]] ++ "}" 
                       | y <- reverse [1..8]] ++
       ["$$\\showboard$$"]
  where
       fn _ (WhitesSq p) = head (userFormat p)
       fn _ (BlacksSq p) = toLower (head (userFormat p))
       fn True VacantSq = '*'
       fn False VacantSq = ' '

 printTeXGame :: RealGame -> [String]
 printTeXGame (Game tags qu) = [
       "\\clearpage",
       "\\begin{center}",
       "\\fbox{\\fbox{\\large\\begin{tabular}{l}",
       ("Game " ++ gameno ++ " \\hspace{.3 in} " 
               ++ date 
               ++ " \\hspace{.3 in} " 
               ++ result 
               ++ "\\\\"),
       "\\hline" ++ (if null opening then "" else "\n" ++ opening ++ "\\\\"),
       "\\raisebox{2.5pt}[11pt]{\\framebox[11pt]{\\rule{0pt}{4.25pt}}} "
               ++ white ++ "\\\\",
       "\\rule[-1pt]{11pt}{11pt} "++ black ++ "\\\\",
       site,
       "\\end{tabular}}}",
       "\\end{center}"] ++
       (printTeXQuantums (True,startBoard,initMoveNumber) qu)
   where
       (date,site,game_no,res,white,black,opening) = getHeaderInfo tags
       gameno = case game_no of
                 Nothing -> ""
                 Just n -> show n
       result = printTeXResult res

 texPrinter :: [RealGame] -> String
 texPrinter games = 
          texHeader 
       ++ (unlines(concat(map printTeXGame games)))
       ++ texFooter

 texHeader =
       "\\documentstyle[twocolumn,a4wide,chess]{article}\n" ++
       "\\textwidth 7.0 in\n" ++
       "\\textheight 63\\baselineskip\n" ++
       "\\columnsep .4 in\n" ++
       "\\columnseprule .5 pt\n" ++
       "\\topmargin -0.5 in\n" ++
       "\\headheight 0 pt\n" ++
       "\\headsep 0 pt\n" ++
       "\\oddsidemargin -0.3 in\n" ++
       "\\font\\sc=cmcsc10\n\\pagestyle{empty}\n" ++
       "\\begin{document}\n\\thispagestyle{empty}\n\n"

 texFooter = "\n\\end{document}\n"


