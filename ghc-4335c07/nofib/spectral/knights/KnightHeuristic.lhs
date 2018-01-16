%               Filename:  KnightHeuristic.lhs
%               Version :  1.4
%               Date    :  3/4/92
\section{Knights Tour Heuristic.}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%M O D U L E%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
module KnightHeuristic(
	ChessSet,
	startTour,
	descendents,
	tourFinished 
) where
\end{code}


%%%%%%%%%%%%%%%%%% I M P O R T S  /  T Y P E   D E F S %%%%%%%%%%%%%%
This module imports the @ChessSet@ algebraic data type, and a sort
function. For reasons unknown to me the crappy @quickSort@ gives the best
results when used here??? 
The enumerated type @Direction@ represents the possible 
directions in which a knight can move on a chess board. For example the
constructor @UL@ represents a knight moving two spaces {\em Up}, then 
one space {\em Left} on a chess board.

\begin{code}
import Sort(quickSort) 
import ChessSetList

data Direction = UL | UR | DL |DR | LU | LD | RU | RD
\end{code}


%%%%%%%%%%%%%%%%%%%%% B O D Y  O F  M O D U L E %%%%%%%%%%%%%%%%%%%%%
\begin{code}
move::Direction -> Tile -> Tile
move UL (x,y) = (x-1,y-2)
move UR (x,y) = (x+1,y-2)
move DL (x,y) = (x-1,y+2)
move DR (x,y) = (x+1,y+2)
move LU (x,y) = (x-2,y-1)
move LD (x,y) = (x-2,y+1)
move RU (x,y) = (x+2,y-1)
move RD (x,y) = (x+2,y+1)
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
We can short-circuit a knights tour for an odd sized board, because a tour
will never exist. This can be proved because if we place a knight anywhere
on the board, their will always be a column or row to which the knight
cannot move.

\begin{code}
startTour::Tile -> Int -> ChessSet
startTour st size 
   | (size `mod` 2) == 0 = createBoard size st 
   | otherwise           = error "Tour doesnt exist for odd size board"
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@MoveKnight@ has a pre-condition that it must be possible to move the knight in
direction @dir@. This condition can be checked with a prior call to the
@canMove@ function.

\begin{code}
moveKnight::ChessSet -> Direction -> ChessSet
moveKnight board dir
   = addPiece (move dir (lastPiece board)) board

canMove::ChessSet -> Direction -> Bool
canMove board dir
   = canMoveTo (move dir (lastPiece board)) board

canMoveTo::Tile -> ChessSet -> Bool
canMoveTo t@(x,y) board
   = (x >= 1) && (x <=sze) && 
     (y >= 1) && (y <=sze) &&
     isSquareFree t board
     where
        sze = sizeBoard board
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
The function @Descendents@ forms the heart of the knights tour. It
has various heuristics built into it that try to reject dead end positions.
These set of heuristics are take from Richard's book \cite{bornat:prog1} under
the section ``Positively the last flourish''. For reasons not clear to me,
the combination of these heuristics work so well that the system finds the 
first twenty tours\footnote{I could'nt be botherd asking for more} 
without backtracking !

The Heuristics used in the following function are summerised below :
\begin{enumerate}
\item	At any point in the tour, if it is possible to move to the first
	tile of the tour, and if by moving to that square produces a 
	dead end, then their is no point in carrying on with the current
	tour because you will never be able to get back to the first square
	- dead end.
\item	At any point in the tour, if for each of the possible moves you can
	take their is only one position that has a single descendent
	\footnote{The descendents of a tile is the number of moves you
	can make from that tile without falling off the board, or jumping
	onto a tile that has already been visited.}, then you {\em must} 
	take that move otherwise you will be cutting off that tile for a 
	subsequent move in the tour -  dead end.
\item   At any point in the tour, if for each of the possible moves you can
	take their is more than one position with a single descendent, then
	you have to abandon the current tour because taking any
	of the single descendents tiles will cause the other single descendent
	tiles to be unreachable - dead end.
\item	At a given point in the tour, if for each of the possible moves you
	can take, their are no single descendent tiles, then visit the tiles
	in order of smallest number of descendents first - (don't know why
	this works, it just does !!!).
\end{enumerate}

\begin{code}
descendents::ChessSet -> [ChessSet]
descendents board 
   | (canJumpFirst board) &&
     (deadEnd (addPiece (firstPiece board) board)) = []
   | otherwise          = case (length singles) of
			     0  -> map snd (quickSort (descAndNo board))
			     1  -> singles
			     _  -> []		-- Going to be dead end
		          where
		             singles = singleDescend board

singleDescend::ChessSet -> [ChessSet]		
singleDescend board =[x | (y,x) <- descAndNo board, y==1] 
  
descAndNo::ChessSet -> [(Int,ChessSet)]
descAndNo board
   = [(length (possibleMoves (deleteFirst x)),x) | x<- allDescend board]
    
allDescend::ChessSet -> [ChessSet]
allDescend board
   =  map (moveKnight board) (possibleMoves board)

possibleMoves::ChessSet -> [Direction]
possibleMoves board
   =[x | x <- [UL,UR,DL,DR,LU,LD,RU,RD], (canMove board x)]

deadEnd::ChessSet -> Bool
deadEnd board = (length (possibleMoves board)) == 0

canJumpFirst::ChessSet -> Bool
canJumpFirst board
  = canMoveTo (firstPiece board) (deleteFirst board)
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
A tour is finished if their are 64 pieces on the board (in the case of an 8x8
board), and move number 64 of the tour can jump back to the first square
(move number 1).

\begin{code}
tourFinished::ChessSet -> Bool
tourFinished board
   = (noPieces board == sze*sze) && (canJumpFirst board)  
     where
        sze = sizeBoard board
\end{code}
