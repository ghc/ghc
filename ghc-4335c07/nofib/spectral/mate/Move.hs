module Move (
  Move(Move), MoveInFull(MoveInFull),
  showMoveInFull, showMoves, moveDetailsFor, kingincheck) where

import Board

data Move = Move
		Square		-- to here
		(Maybe Piece)	-- capturing this
		(Maybe Piece)	-- gaining promotion to this
		deriving (Eq,Ord)

data MoveInFull = MoveInFull Piece Square Move
                  deriving (Eq, Ord)

showMoveInFull :: MoveInFull -> String
showMoveInFull = showMove True

showMove withPiece (MoveInFull p@(c,k) sq (Move sq' mcp mpp)) =
	let capt = mcp /= Nothing
	    prom = mpp /= Nothing in
	( if withPiece then 
		showPiece p ++
		(if k==King || k==Pawn && not (capt||prom) then ""
		else "/" ++ showSquare c sq)
	  else "" ) ++
	(maybe "-" (\cp -> "x" ++ showPiece cp ++ "/") mcp) ++
	showSquare c sq' ++
	(maybe "" (\pp -> "(" ++ showPiece pp ++ ")") mpp)

showMoves (mif:mifs) = showMoveInFull mif ++ showMovesAfter mif mifs

showMovesAfter _ [] = ""
showMovesAfter (MoveInFull p' sq' _) (mif@(MoveInFull p sq _):mifs) =
	", " ++ showMove (p/=p' || sq/=sq') mif ++ showMovesAfter mif mifs

moveDetailsFor :: Colour -> Board -> [(MoveInFull,Board)]
moveDetailsFor c bd =
	foldr ( \ksq ms ->
		foldr (\rm ms' -> maybe id (:) (tryMove c ksq rm bd) ms')
                   ms
                   (rawmoves c ksq bd) )
	      [] 
              (forcesColoured c bd)

tryMove :: Colour -> (Kind,Square) -> Move -> Board -> Maybe (MoveInFull,Board)
tryMove c ksq@(k,sq) m@(Move sq' mcp mpp) bd =
	if not (kingincheck c bd2) then Just (MoveInFull p sq m, bd2)
	else Nothing
	where
	p   =   (c,k)
	bd1 =	rmPieceAt c sq bd
	p'  =	maybe p id mpp
	bd2 =	maybe (putPieceAt sq' p' bd1)
		      (const (putPieceAt sq' p' (rmPieceAt (opponent c) sq' bd1)))
		      mcp
 
-- NB raw move = might illegally leave the king in check.
rawmoves :: Colour -> (Kind,Square) -> Board -> [Move]
rawmoves c (k,sq) bd = m c sq bd
	where
        m = case k of
	    King   -> kingmoves
	    Queen  -> queenmoves
	    Rook   -> rookmoves
	    Bishop -> bishopmoves
	    Knight -> knightmoves
	    Pawn   -> pawnmoves

bishopmoves :: Colour -> Square -> Board -> [Move]
bishopmoves c sq bd =
	( moveLine bd c sq (\(x,y) -> (x-1,y+1)) $
	  moveLine bd c sq (\(x,y) -> (x+1,y+1)) $
	  moveLine bd c sq (\(x,y) -> (x-1,y-1)) $
	  moveLine bd c sq (\(x,y) -> (x+1,y-1)) id
        ) []

rookmoves :: Colour -> Square -> Board -> [Move]
rookmoves c sq bd =
	( moveLine bd c sq (\(x,y) -> (x-1,y)) $
	  moveLine bd c sq (\(x,y) -> (x+1,y)) $
	  moveLine bd c sq (\(x,y) -> (x,y-1)) $
	  moveLine bd c sq (\(x,y) -> (x,y+1)) id
        ) []

moveLine :: Board -> Colour -> Square -> (Square->Square) -> ([Move]->a) -> [Move] -> a
moveLine bd c sq inc cont = ml sq
	where
	ml sq ms =
		let sq' = inc sq in
		if onboard sq' then
			case pieceAt bd sq' of
			Nothing -> ml sq' (Move sq' Nothing Nothing : ms)
			Just p' -> if colourOf p' /= c then
					cont (Move sq' (Just p') Nothing : ms)
                                   else cont ms
		else cont ms

kingmoves :: Colour -> Square -> Board -> [Move]
kingmoves c (p,q) bd =
	sift c bd []     [(p-1,q+1), (p,q+1), (p+1,q+1),
	  	 	  (p-1,q),            (p+1,q),
		 	  (p-1,q-1), (p,q-1), (p+1,q-1)]

knightmoves :: Colour -> Square -> Board -> [Move]
knightmoves c (p,q) bd =
	sift c bd [] [	  	 (p-1,q+2),(p+1,q+2),
			  (p-2,q+1),		  (p+2,q+1),
                          (p-2,q-1),		  (p+2,q-1),
		  		 (p-1,q-2),(p+1,q-2) ]

sift :: Colour -> Board -> [Move] -> [Square] -> [Move]
sift _ _  ms [] = ms
sift c bd ms (sq:sqs) =
	if onboard sq then
		case pieceAt bd sq of
                Nothing -> sift c bd (Move sq Nothing Nothing : ms) sqs
		Just p' -> if colourOf p' == c then sift c bd ms sqs
                           else sift c bd (Move sq (Just p') Nothing : ms) sqs
	else sift c bd ms sqs

pawnmoves :: Colour -> Square -> Board -> [Move]
pawnmoves c (p,q) bd = movs ++ caps
	where
	movs =	let on1 = (p,q+fwd)
		    on2 = (p,q+2*fwd) in
		if pieceAt bd on1 == Nothing then
			promote on1 Nothing ++
			if (q==2 && c==White || q==7 && c==Black) &&
			 	pieceAt bd on2 == Nothing then [Move on2 Nothing Nothing] 
			else []
		else []
	caps =	concat [ promote sq mcp
                       | sq <- [(p+1,q+fwd), (p-1,q+fwd)],
                         mcp@(Just p') <- [pieceAt bd sq], colourOf p'/=c ]
	fwd  =	case c of
       		White -> 1
		Black -> -1
	promote sq@(x,y) mcp =  
		if (c==Black && y==1 || c==White && y==8) then
			map (Move sq mcp . Just)
			    [(c,Queen), (c,Rook), (c,Bishop), (c,Knight)]
		else [Move sq mcp Nothing]

queenmoves :: Colour -> Square -> Board -> [Move]
queenmoves c sq bd = bishopmoves c sq bd ++ rookmoves c sq bd

kingincheck :: Colour -> Board -> Bool
kingincheck c bd =
	any givesCheck (forcesColoured (opponent c) bd)
	where
	givesCheck (k,(x,y)) = kthreat k
		where
		kthreat King =
			abs (x-xk) <= 1 && abs (y-yk) <= 1
		kthreat Queen =
			kthreat Rook || kthreat Bishop
		kthreat Rook =
			x==xk &&
                        emptyAtAll bd (\(xe,ye) -> xe==xk && min y yk < ye && ye < max y yk) ||
			y==yk &&
                        emptyAtAll bd (\(xe,ye) -> ye==yk && min x xk < xe && xe < max x xk)
		kthreat	Bishop =
			x+y==xk+yk &&
			emptyAtAll bd (\(xe,ye) -> xe+ye==xk+yk && min x xk < xe && xe < max x xk) ||
			x-y==xk-yk &&
			emptyAtAll bd (\(xe,ye) -> xe-ye==xk-yk && min x xk < xe && xe < max x xk)
		kthreat	Knight =
			abs (x-xk) == 2 && abs (y-yk) == 1 ||
			abs (x-xk) == 1 && abs (y-yk) == 2
		kthreat Pawn =
			abs (x-xk) == 1 &&
			case c of
			Black -> yk == y+1
			White -> yk == y-1
	(xk,yk) = kingSquare c bd

