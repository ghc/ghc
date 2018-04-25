--	The Last Piece Puzzle
--
-- Simon Peyton Jones, May 2002

module Main where
import Prelude hiding( flip )
import Data.Maybe
import Text.PrettyPrint
import qualified Data.Map as Map

----------------------------
--	Driver

main = printDoc (display solutions)
solutions = search (1,2) Female initialBoard initialPieces

display :: Solution -> Doc
display (Soln bd) = vcat [text "Success!",
		          nest 2 (displayBoard bd)]

display (Choose ss) = vcat (map display ss)

display (Fail bd (row,col)) 
--  | row >= maxRow-1 = (text "Dead end:" <+> displayBoard bd)
  | otherwise	    = empty

displayBoard :: Board -> Doc
displayBoard bd
  = vcat (map row [maxRow, maxRow-1 .. 1])
    $$ text ""
  where
    row n = hcat (map (sq n) [1..maxCol])
    sq n col = case check bd (n,col) of
		  Just id -> char id
		  Nothing -> char '.'


-------------------------------------
--	Pieces
data Piece = P  PieceId 
		[[Offset]] 	-- Male in bottom LH
		[[Offset]]	-- Female in bottom LH
	-- In both cases, the list of offset is all the
	-- squares except the bottom LH one

type PieceId = Char

data Sex = Male | Female
flip Male   = Female
flip Female = Male


-------------------------------------
--	The main search

data Solution = Soln Board
	      | Choose [Solution]	-- Non-empty
	      | Fail Board Square

search :: Square -> Sex 	-- Square we are up to
       -> Board			-- Current board
       -> [Piece]		-- Remaining pieces
       -> Solution

search sq sex bd [] 
  = Soln bd	-- Finished

search (row,col) sex bd ps	-- Next row
  | col == maxCol+1 = search (row+1, 1) (flip sex) bd ps

search sq sex bd ps	-- Occupied square
  | isJust (check bd sq) = search (next sq) (flip sex) bd ps

search sq sex bd ps
  = case mapMaybe (try sq sex bd) choices of
	[] -> Fail bd sq
	ss -> Choose ss
  where
    choices = [(id, os, ps) | (P id ms fs, ps) <- pickOne ps,
			      let oss = case sex of
					  Male   -> ms
					  Female -> fs,
			      os <- oss]

try :: Square -> Sex -> Board -> (PieceId,[Offset],[Piece]) -> Maybe Solution
try sq sex bd (id,os,ps)
  = case (fit bd sq id os) of
	Just bd' -> Just (search (next sq) (flip sex) bd' ps)
	Nothing  -> Nothing

fit :: Board -> Square -> PieceId -> [Offset] -> Maybe Board
fit bd sq id []     = Just (extend bd sq id)
fit bd sq id (o:os) = case extend_maybe bd (sq `add` o) id of
			Just bd' -> fit bd' sq id os
			Nothing  -> Nothing

--------------------------
--	Offsets and squares

type Offset  = (Int,Int)
type Square  = (Int,Int)
	-- (1,1) is bottom LH corner

add :: Square -> Offset -> Square
add (row,col) (orow, ocol) = (row + orow, col + ocol)

next :: Square -> Square
next (row,col) = (row,col+1)

maxRow,maxCol :: Int
maxRow = 8
maxCol = 8



------------------------ 
--	Boards
check  :: Board -> Square -> Maybe PieceId
extend 	     :: Board -> Square -> PieceId -> Board
extend_maybe :: Board -> Square -> PieceId -> Maybe Board

type Board = Map.Map Square PieceId

emptyBoard = Map.empty

check bd sq = Map.lookup sq bd

extend bd sq id = Map.insert sq id bd

extend_maybe bd sq@(row,col) id 
  | row > maxRow || col < 1 || col > maxCol
  = Nothing
  | otherwise
  = case check bd sq of
	Just _  -> Nothing
	Nothing -> Just (extend bd sq id)


--------------------------
--	Utility

pickOne :: [a] -> [(a,[a])]
pickOne xs = go id xs
  where
    go f [] = []
    go f (x:xs) = (x, f xs) : go ((x :) . f) xs


printDoc :: Doc -> IO ()
printDoc d = fullRender ZigZagMode 200 1.5 put done d
	 where
	    put (Chr c)  next = putChar c >> next 
	    put (Str s)  next = putStr  s >> next 
	    put (PStr s) next = putStr  s >> next 

	    done = putStr "\n"


-----------------------------------
--	The initial setup
initialBoard = fromJust (fit emptyBoard (1,1) 'a' [(1,0),(1,1)])

initialPieces = [bPiece, cPiece, dPiece, ePiece, fPiece,
		 gPiece, hPiece, iPiece, jPiece, kPiece, lPiece,
		 mPiece, nPiece]

nPiece = P 'n' [ [(0,1),(1,1),(2,1),(2,2)],
		 [(1,0),(1,-1),(1,-2),(2,-2)] ]
	       []

mPiece = P 'm' [ [(0,1),(1,0),(2,0),(3,0)] ]
	       [ [(0,1),(0,2),(0,3),(1,3)],
		 [(1,0),(2,0),(3,0),(3,-1)] ]

lPiece = P 'l' [ [(0,1),(0,2),(0,3),(1,2)],
		 [(1,0),(2,0),(3,0),(2,-1)] ]
	       [ [(1,-1),(1,0),(1,1),(1,2)],
		 [(1,0),(2,0),(3,0),(1,1)] ]

kPiece = P 'k' [ [(0,1),(1,0),(2,0),(2,-1)] ]
	       [ [(1,0),(1,1),(1,2),(2,2)] ]


jPiece = P 'j' [ [(0,1),(0,2),(0,3),(1,1)],
		 [(1,0),(2,0),(3,0),(1,-1)],
		 [(1,-2),(1,-1),(1,0),(1,1)] ]
	       [ [(1,0),(2,0),(3,0),(2,2)] ]

iPiece = P 'i' [ [(1,0),(2,0),(2,1),(3,1)],
		 [(0,1),(0,2),(1,0),(1,-1)],
		 [(1,0),(1,1),(2,1),(3,1)] ]
	       [ [(0,1),(1,0),(1,-1),(1,-2)] ]

hPiece = P 'h' [ [(0,1),(1,1),(1,2),(2,2)],
		 [(1,0),(1,-1),(2,-1),(2,-2)],
		 [(1,0),(1,1),(2,1),(2,2)] ]
	       [ [(0,1),(1,0),(1,-1),(2,-1)] ]


gPiece = P 'g' [ ]
	       [ [(0,1),(1,1),(1,2),(1,3)],
		 [(1,0),(1,-1),(2,-1),(3,-1)],
		 [(0,1),(0,2),(1,2),(1,3)],
		 [(1,0),(2,0),(2,-1),(3,-1)] ]

fPiece = P 'f' [ [(0,1),(1,1),(2,1),(3,1)],
		 [(1,0),(1,-1),(1,-2),(1,-3)],
		 [(1,0),(2,0),(3,0),(3,1)] ]
	       [ [(0,1),(0,2),(0,3),(1,0)] ]


ePiece = P 'e' [ [(0,1),(1,1),(1,2)],
		 [(1,0),(1,-1),(2,-1)] ]
	       [ [(0,1),(1,1),(1,2)],
		 [(1,0),(1,-1),(2,-1)] ]

dPiece = P 'd' [ [(0,1),(1,1),(2,1)],
		 [(1,0),(1,-1),(1,-2)] ]
	       [ [(1,0),(2,0),(2,1)] ]


cPiece = P 'c' [ ]
	       [ [(0,1),(0,2),(1,1)],
		 [(1,0),(1,-1),(2,0)],
		 [(1,-1),(1,0),(1,1)],
		 [(1,0),(1,1),(2,0)] ]

bPiece = P 'b'	[ [(0,1),(0,2),(1,2)],
		  [(1,0),(2,0),(2,-1)],
		  [(0,1),(1,0),(2,0)] ]
		[ [(1,0),(1,1),(1,2)] ]




