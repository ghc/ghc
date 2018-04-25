%               Filename:  ChessSetArray.lhs
%               Version :  1.4
%               Date    :  3/4/92
\section{Building chess boards out of arrays}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%M O D U L E%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Lots of data abstraction is used in this version of the knights tour. The
searching mechanism can be either a sequential depth first search, or a
data parallel search (for instance wedge first??). This module
abstracts data type specific operations used in the Heuristic part of the tour.

\begin{code}
module ChessSetArray(Tile,
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
\item	{\tt Tile} representing the first move of the knight.
\item 	A 1D array (A) of {\tt Int} where $A_{i}=n$ represents the $n^{th}$
	move of the knight; where $n\ge 1$ or the empty tile if $n=0$. 
	A tile at position $(x,y)$ would be represnted by the array element
	$A_{(x-1)*size + y}$.
	
\end{itemize}
A One dimensional array was used in this implementation due to problems with
the current release of Glasgow Haskell. We include information in this 
type that could of been deduced from the trail alone, but adding the 
information prevents unnecessary traversal of the trail.


\begin{code}
import Data.Array
import Sort(quickSort)

type Tile     = (Int,Int)
data ChessSet = Board Int Int Tile Tile (Array Int Int)
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
   showsPrec p board@(Board s n l f ts) 
      = showString "Move number " . (showsPrec p n).
	showString "\n" . showString (printBoard s (elems ts) 1)
\end{code}


%%%%%%%%%%%%%%%%%%%%% B O D Y  O F  M O D U L E %%%%%%%%%%%%%%%%%%%%%
\begin{code}
createBoard::Int -> Tile -> ChessSet
createBoard x t = Board x 1 t t onlyFirst
		  where
		     onlyFirst = empty // [(tileIndex x t, 1)]
		     empty     = array (1,x*x) [ (i,0) | i<-[1..x*x]]

sizeBoard::ChessSet -> Int
sizeBoard (Board s _ _ _ _) = s

noPieces::ChessSet -> Int 
noPieces (Board _ n _ _ _) = n

addPiece::Tile -> ChessSet -> ChessSet
addPiece t (Board s n l f ts) =Board s (n+1) t f 
				    (ts // [(tileIndex s t, n+1)])
\end{code}


@deletePiece@ deletes the $x^{th}$ piece placed on the board, and 
depending on the representation ensures the remaining trail is valid.


\begin{code}
deleteFirst::ChessSet -> ChessSet
deleteFirst (Board s n l f ts) = Board s n l l 
				       (ts // [(tileIndex s f, 0)])
\end{code}

{\bf Note:} the below function does not change the trail.

\begin{code}
positionPiece::Int -> ChessSet -> Tile
positionPiece x (Board s _ _ _ ts) 
   = findPiece x ts [ i | i<-[1..s*s] ]
     where
        findPiece x ts []     = error "Piece not found"
	findPiece x ts (y:ys) = if ((ts ! y)==x) then (indexTile s y)
			        else
				   findPiece x ts ys
        
lastPiece::ChessSet -> Tile
lastPiece (Board _ _ l _ _) = l

firstPiece::ChessSet -> Tile
firstPiece (Board _ _ _ f _) = f

pieceAtTile::Tile -> ChessSet -> Int
pieceAtTile x (Board s _ _ _ ts)
   = ts ! (tileIndex s x)

isSquareFree::Tile -> ChessSet -> Bool
isSquareFree x (Board s _ _ _ ts) = (ts ! (tileIndex s x)) == 0

\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% M I S C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Various auxiliary functions used above which I would of liked to 
include in @where@ clauses if they were not so large.

\begin{code}

tileIndex:: Int -> Tile -> Int
tileIndex size (x,y) = ((x-1)*size) + y

indexTile::Int -> Int -> Tile
indexTile size x     = ((x `div` size)+1 , x `mod` size)

printBoard s [] i    = []
printBoard s (x:xs) i 
   | (i/=s) && (x==0) ="*"     ++(spaces (s*s) 1)++(printBoard s xs (i+1))
   | (i==s) && (x==0) ="*\n"           	         ++(printBoard s xs 1)
   | (i/=s) 	      =(show x)++(spaces (s*s) x)++(printBoard s xs (i+1))
   | (i==s)           =(show x)++ "\n" 	         ++(printBoard s xs 1)

spaces s y = take ((logTen s) - (logTen y) + 1) [' ',' '..]
	     where
	        logTen 1 = 0
		logTen x = 1+ logTen (x `div` 10)

\end{code}
