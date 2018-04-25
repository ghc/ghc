module Board(
  Kind(King,Queen,Rook,Bishop,Knight,Pawn),
  Colour(Black,White), Piece, Square, Board, 
  showBoard, showPiece, showSquare,
  emptyBoard, pieceAt, rmPieceAt, putPieceAt,
  emptyAtAll, kingSquare, forcesColoured,
  colourOf, kindOf, opponent, onboard)  where

import Data.Char(toLower)

data Kind = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq,Ord)

data Colour = Black | White deriving (Eq,Ord,Read,Show)

type Piece = (Colour,Kind)
type Square = (Int,Int)

data Board = Board
		[(Kind,Square)] -- white
		[(Kind,Square)] -- black

showBoard bd =
	unlines (map showRank (reverse [1..8]))
	where
	showRank r = foldr consFile [] [1..8]
		where
		consFile f s =
			case pieceAt bd (f,r) of
			Nothing -> " -" ++ s
			Just p  -> ' ': pieceToChar p : s

pieceToChar :: Piece -> Char
pieceToChar (Black,k) = kindToChar k
pieceToChar (White,k) = toLower (kindToChar k)

kindToChar :: Kind -> Char
kindToChar k =
	case k of
	King	-> 'K'
	Queen	-> 'Q'
	Rook	-> 'R'
	Bishop	-> 'B'
	Knight	-> 'N'
	Pawn	-> 'P'

showPiece :: Piece -> String
showPiece (c,k) = [kindToChar k]

showSquare :: Colour -> Square -> String
showSquare c (x,y) =
	["QR","QN","QB","Q","K","KB","KN","KR"] !! (x-1) ++
	show (  case c of
		Black -> 9-y
		White -> y )

pieceAt :: Board -> Square -> Maybe Piece
pieceAt (Board wkss bkss) sq =
        pieceAtWith White (pieceAtWith Black Nothing bkss) wkss
	where
	pieceAtWith c n [] = n
	pieceAtWith c n ((k,s):xs) = if s==sq then Just (c,k) else pieceAtWith c n xs

emptyAtAll :: Board -> (Square->Bool) -> Bool
emptyAtAll (Board wkss bkss) e =
	emptyAtAllAnd (emptyAtAllAnd True bkss) wkss
	where
	emptyAtAllAnd b []         = b
	emptyAtAllAnd b ((_,s):xs) = not (e s) && emptyAtAllAnd b xs

rmPieceAt White sq (Board wkss bkss) = Board (rPa sq wkss) bkss
rmPieceAt Black sq (Board wkss bkss) = Board wkss (rPa sq bkss)

rPa sq (ks@(k,s):kss) = if s==sq then kss else ks : rPa sq kss

putPieceAt sq (White,k) (Board wkss bkss) = Board ((k,sq):wkss) bkss
putPieceAt sq (Black,k) (Board wkss bkss) = Board wkss ((k,sq):bkss)

kingSquare :: Colour -> Board -> Square
kingSquare White (Board kss _) = kSq kss
kingSquare Black (Board _ kss) = kSq kss

kSq ((King,s):_)   = s
kSq (       _:kss) = kSq kss 

opponent Black = White
opponent White = Black

colourOf :: Piece -> Colour
colourOf (c,_) = c

kindOf :: Piece -> Kind
kindOf (_,k) = k

onboard :: Square -> Bool
onboard (p,q) = 1<=p && p<=8 && 1<=q && q<=8

forcesColoured White (Board kss _) = kss
forcesColoured Black (Board _ kss) = kss

emptyBoard = Board [] []
