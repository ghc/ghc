module Problem(readProblem) where

import Data.Char(isUpper,toLower)
import Board

readProblem :: String -> (Board, (Colour, Int))
readProblem = parseProblem . lines

comment s = (s == [] || take 2 s == "--")

parseProblem :: [String] -> (Board, (Colour, Int))
parseProblem s = (bd, gl)
	where
	bd = parseBoard bdtxt
	gl = parseGoal gltxt 
        (bdtxt, gltxt) = splitAt 8 (filter (not . comment) s)

parseBoard :: [String] -> Board
parseBoard = convert . concat . zipWith parseRank (reverse [1..8])
	where
	convert = foldr addPiece emptyBoard
	addPiece (p,sq) = putPieceAt sq p

parseRank r = concat . zipWith (parseSquare r) [1..8] . filter (/= ' ')

parseSquare r f '-' = []
parseSquare r f  c  = 
	[((clr,kin), (f,r))]
	where
	clr = if isUpper c then Black else White 
	kin = case toLower c of
              'k' -> King 
	      'q' -> Queen
	      'r' -> Rook
	      'b' -> Bishop
	      'n' -> Knight
	      'p' -> Pawn

parseGoal :: [String] -> (Colour, Int)
parseGoal [gltxt] = (c, n)
        where
        ws = words gltxt
        c  = read (head ws)
        n  = read (last ws)
