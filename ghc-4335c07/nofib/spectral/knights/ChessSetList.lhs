%               Filename:  ChessSetList.lhs
%               Version :  1.4
%               Date    :  3/4/92
\section{Building Chess Boards Out Of Lists.}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%M O D U L E%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Lots of data abstraction is used in this version of the knights tour. The
searching mechanism can be either a sequential depth first search, or a
data parallel search (for instance wedge first??). This module
abstracts data type specific operations used in the Heuristic part of the tour.

\begin{code}
module ChessSetList(Tile,
		    ChessSet,
		    createBoard,
		    sizeBoard,
		    addPiece,
		    deleteFirst,
		    noPieces,
		    positionPiece,
		    lastPiece,
		    firstPiece,
		    pieceAtTile,
		    isSquareFree
) where
\end{code}


%%%%%%%%%%%%%%%%%% I M P O R T S  /  T Y P E   D E F S %%%%%%%%%%%%%%
@Tile@ is a type synonym that represents the $(x,y)$ coordinates of a
tile on chess board. The chess board is represented as an algebraic 
data type\footnote{And hence we can include it in class @Text@, making it
@show@able} of an :
\begin{itemize}
\item   {\tt Int} representing the size of the chess board.
\item   {\tt Int} representing the current move number.
\item	{\tt Tile} representing the first move of the knight.
\item 	{\tt [Tile]} representing the trail of the knights moves, where the
        $n^{th}$ tile from the back of the list represents the $n^{th}$ move.
\end{itemize}
We include information in this type that could of been deduced from the
trail alone, but adding the information prevents
unnecessary traversal of the trail.

\begin{code}
import Sort(quickSort)

type Tile     = (Int,Int)
data ChessSet = Board Int Int Tile [Tile]
\end{code}


%%%%%%%%%%%%%%%%%%%% C L A S S  I N S T A N C E S %%%%%%%%%%%%%%%%%%%
Various instance declarations for @show@ , @==@ and @<=@. Note the little
hack with ordinals, we do not want to compare chess sets, but if we have 
for instance a tuple of @(Int,ChessSet)@, then we want to compare on the
@Int@ part of the tuple. Therefore {\em any} @ChessSet@ is smaller than any
other.

\begin{code}
instance Eq ChessSet where
    _ == _ = True

instance Ord ChessSet where
    _ <= _ = True			

instance Show ChessSet where
   showsPrec p board@(Board sze n f ts) 
      = showString (printBoard sze sortedTrail 1)
        where
	   sortedTrail = quickSort 
			    (assignMoveNo ts sze n)
\end{code}


%%%%%%%%%%%%%%%%%%%%% B O D Y  O F  M O D U L E %%%%%%%%%%%%%%%%%%%%%
\begin{code}
createBoard::Int -> Tile -> ChessSet
createBoard x t= Board x 1 t [t]

sizeBoard::ChessSet -> Int
sizeBoard (Board s _ _ _) = s

noPieces::ChessSet -> Int 
noPieces (Board _ n _ _) = n

addPiece::Tile -> ChessSet -> ChessSet
addPiece t (Board s n f ts) = Board s (n+1) f (t:ts)
\end{code}


@deletePiece@ deletes the $x^{th}$ piece placed on the board, and 
depending on the representation ensures the remaining trail is valid
(i.e info reguarding position in valid).

\begin{code}
deleteFirst::ChessSet -> ChessSet
deleteFirst (Board s n f ts) = Board s (n-1) (last ts') ts'
			       where
			           ts' = init ts


positionPiece::Int -> ChessSet -> Tile
positionPiece x (Board _ n _ ts) = ts !! (n - x)

lastPiece::ChessSet -> Tile
lastPiece (Board _ _ _ (t:ts)) = t

firstPiece::ChessSet -> Tile
firstPiece (Board _ _ f _) = f

pieceAtTile::Tile -> ChessSet -> Int
pieceAtTile x (Board _ _ _ ts)
   = find x ts
     where
	find x [] = error "Tile not used"
	find x (y:xs) 
	   | x == y    = 1 + length xs
	   | otherwise = find x xs

isSquareFree::Tile -> ChessSet -> Bool
isSquareFree x (Board _ _ _ ts) = x `notElem` ts
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% M I S C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Various auxiliary functions used above which I would of liked to 
include in @where@ clauses if they were not so large.

\begin{code}
assignMoveNo [] size x
   = []
assignMoveNo ((x,y):t) size z
   =(((y-1)*size)+x,z):assignMoveNo t size (z-1)

printBoard s [] n
   | (n  > (s*s))   = ""
   | (n `mod` s /=0)= "*"++(spaces (s*s) 1) ++(printBoard s [] (n+1))
   | (n `mod` s ==0)= "*\n"                 ++(printBoard s [] (n+1))
printBoard s trail@((i,j):xs) n
   | (i==n) && 
     (n `mod` s ==0)= (show j)++"\n"++(printBoard s xs (n+1))
   | (i==n) && 
     (n `mod` s /=0)= (show j)++(spaces (s*s) j)++(printBoard s xs    (n+1))
   | (n `mod` s /=0)= "*"     ++(spaces (s*s) 1)++(printBoard s trail (n+1))
   | (n `mod` s ==0)= "*\n"                     ++(printBoard s trail (n+1))

spaces s y = take ((logTen s) - (logTen y) + 1) [' ',' '..]
	     where
	        logTen 0 = 0
		logTen x = 1+ logTen (x `div` 10)

\end{code}
