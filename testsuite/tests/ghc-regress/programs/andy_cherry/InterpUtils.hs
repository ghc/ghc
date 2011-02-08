
 module InterpUtils where

 import GenUtils
 import DataTypes
 import Data.Array -- 1.3





 findCastleKMove brd = (castleK,makeACastleK brd)
 findCastleQMove brd = (castleQ,makeACastleQ brd)

 findAPawnMove
       :: ExBoardPos 
       -> ExBoardPos 
       -> Maybe Piece 
       -> Board
       -> (String,Board)





 findAPawnMove move_src move_dest queen brd@(Board arr mv _) 
       = debug (move_txt,new_brd)
   where

       move_colour = getMoveColour mv

       debug   = {- trace (
               {- userFormat brd ++ -}
               userFormat (getMoveColour mv) ++ 
               -- " (" ++ userFormat absmove ++ ")" ++
               "\nALL   :" ++ unwords (map userFormat all_moves) ++
               "\n") -} id 




       correct_src = concat (map (getAllMovesFor brd) currPieces)

       currPieces  =
               [ (Pawn,x,y) |
                       (x,y) <- start_range,
                       r <- [arr ! (x,y)],
                       lookupSquare move_colour r == Friendly,
                       (Just Pawn) <- [getSquarePiece r]]



       start_range
          = case (move_src,move_dest) of
               ((Just f,Just r),_) -> [(f,r)]
               ((Just f,_),_) -> [(f,r) | r <- [2..7]]
               -- no capture !
               (_,(Just f,_)) -> [(f,r) | r <- [2..7]]
               _ -> error "strange pawn move:"

       the_correct_move = if (length correct_move /= 1)
                          then error ("\nAmbiguous move:"
               ++ show (unwords (map userFormat correct_move))
               ++ ":" ++ {- userFormat absmove ++ -} "\n"
               ++ userFormat brd)
               else head correct_move

       correct_move = 
               filter (sameQueening queen.extractSpecialFromPlayMove)
               (filter (compExBPandBP move_dest.extractDestFromPlayMove)
                       correct_src)
       sameQueening (Just p) (Queening p') = p == p'
       sameQueening Nothing  (Queening p') = Queen == p'
       sameQueening _ _ = True

       move_txt = createShortMove the_correct_move "" brd
       corr_txt = 
           userFormatBoardPos 
               (extractSrcFromPlayMove the_correct_move) ++
           userFormatBoardPos
               (extractDestFromPlayMove the_correct_move) 
               {- queening ?? -}
       new_brd = makeAMove brd the_correct_move





 findAMove
       :: Piece
       -> ExBoardPos 
       -> ExBoardPos 
       -> Board
       -> (String,Board)

 findAMove move_piece move_src move_dest brd@(Board arr mv _) 
       = debug (move_txt,new_brd)
   where



       move_colour = getMoveColour mv

       debug   = {- trace (
               {- userFormat brd ++ -}
               userFormat (getMoveColour mv) ++ 
               " (" ++ {- userFormat absmove ++ -} ")" ++
               "\nALL   :" ++ unwords (map userFormat all_moves) ++
               "\nDEST  :" ++ unwords (map userFormat correct_dest) ++
               "\nSRC   :" ++ unwords (map userFormat correct_move) ++
               "\n") -} id 
               




       all_moves = allValidMoves brd move_piece (const True)



       correct_dest = filter
               (compExBPandBP move_dest.extractDestFromPlayMove)
                       all_moves
       correct_move = filter
               (compExBPandBP move_src.extractSrcFromPlayMove)
                       correct_dest
       the_correct_move = if (length correct_move /= 1)
                          then error ("\nAmbiguous move:"
               ++ show (unwords (map userFormat correct_move))
               ++ ":" {- ++ userFormat absmove -} ++ "\n"
               ++ userFormat brd)
               else head correct_move
       disamb = case move_dest of
                 (Just _,Nothing) -> ""        -- fg => fxg4, no disambig.
                 _ -> disAmb
                    (extractSrcFromPlayMove the_correct_move)
                    (map (extractSrcFromPlayMove) correct_dest)

       move_txt = createShortMove the_correct_move disamb brd
       corr_txt = 
           userFormatBoardPos 
               (extractSrcFromPlayMove the_correct_move) ++
           userFormatBoardPos
               (extractDestFromPlayMove the_correct_move) 
               {- queening -}
       new_brd = makeAMove brd the_correct_move
 --partain: findAMove _ _ _ brd = error ("strange move: ")

 allValidMoves :: Board -> Piece -> (ChessFile -> Bool) -> [PlayMove]
 allValidMoves brd piece corr_file
   = concat (map (getAllMovesFor brd) (getCurrPieces brd piece corr_file)) 

 getCurrPieces 
       :: Board 
       -> Piece 
       -> (ChessFile -> Bool)
       -> [(Piece,ChessFile,ChessRank)]
 getCurrPieces (Board arr (MoveNumber _ col) _) pc corr_file =
       [ (p,x,y) |
               ((x,y), r) <- assocs arr,
               lookupSquare col r == Friendly,
               (Just p) <- [getSquarePiece r],
               p == pc,
               corr_file x
                ]






 getAllMovesFor :: Board -> (Piece,Int,Int) -> [PlayMove]



 getAllMovesFor brd (Rook,x,y) = 
       [ mkPlayMove Rook (x,y) (x',y')
         | (x',y') <- (
               movePiece 0    1 brd x y ++
               movePiece 0 (-1) brd x y ++
               movePiece 1    0 brd x y ++
               movePiece (-1) 0 brd x y) ]
 getAllMovesFor brd (Bishop,x,y) = 
       [ mkPlayMove Bishop (x,y) (x',y')
         | (x',y') <- (
               movePiece 1      1  brd x y ++
               movePiece 1    (-1) brd x y ++
               movePiece (-1)   1  brd x y ++
               movePiece (-1) (-1) brd x y) ]
 getAllMovesFor brd (Queen,x,y) = 
       [ mkPlayMove Queen (x,y) (x',y')
         | (x',y') <- (
               movePiece 0    1    brd x y ++
               movePiece 0 (-1)    brd x y ++
               movePiece 1    0    brd x y ++
               movePiece (-1) 0    brd x y ++
               movePiece 1      1  brd x y ++
               movePiece 1    (-1) brd x y ++
               movePiece (-1)   1  brd x y ++
               movePiece (-1) (-1) brd x y) ]



 getAllMovesFor brd (Knight,x,y) =
       [ mkPlayMove Knight (x,y) (x',y')
           | (xd,yd) <- concat 
                       [ [(d1,d2 * 2),(d1 * 2,d2)]
                               | d1 <- [1,-1], d2 <- [1,-1]],
               x' <- [xd + x],
               y' <- [yd + y],
               case lookupBoard brd (x',y') of
                 Vacant -> True
                 Friendly -> False
                 Baddy -> True
                 OffBoard -> False]

 getAllMovesFor brd (King,x,y) =
       [ mkPlayMove King (x,y) (x',y')
           | (xd,yd) <- [(1,1),(1,0),(1,-1),(0,1),
                         (0,-1),(-1,1),(-1,0),(-1,-1)],
               x' <- [xd + x],
               y' <- [yd + y],
               case lookupBoard brd (x',y') of
                 Vacant -> True
                 Friendly -> False
                 Baddy -> True
                 OffBoard -> False]




 getAllMovesFor brd@(Board _ (MoveNumber _ col) may_ep) (Pawn,x,y) 
       = real_pawn_moves
   where
       pawn_moves = 
               case lookupBoard brd (x,y+del) of
                 Friendly -> []
                 Baddy -> []
                 Vacant -> (mkPlayMove Pawn (x,y) (x,y+del) :
                    if y /= sta then [] else
                    case lookupBoard brd (x,y+del*2) of
                       Friendly -> []
                       Baddy -> []
                       Vacant -> 
                         [ PlayMove Pawn (x,y) (x,y+del*2) BigPawnMove])
       left_pc = case lookupBoard brd (x-1,y+del) of
                        Baddy -> [mkPlayMove Pawn (x,y) (x-1,y+del) ]
                        _ -> []
       right_pc = case lookupBoard brd (x+1,y+del) of
                        Baddy -> [mkPlayMove Pawn (x,y) (x+1,y+del) ]
                        _ -> []
       all_pawn_moves = pawn_moves ++ left_pc ++ right_pc 
       real_pawn_moves = en_passant ++
               (if y + del == qn       -- if can queens
               then concat [ let fn = PlayMove Pawn f t . Queening
                             in
                               [ fn Queen,
                                 fn Rook,
                                 fn Bishop,
                                 fn Knight ]
                               | PlayMove _ f t _ <- all_pawn_moves ]
                 else all_pawn_moves)
       en_passant = 
           case (y == ep,may_ep) of
               (True,Just f) | f == x+1 || f == x-1 
                 -> [PlayMove Pawn (x,y) (f,y+del) EnPassant]
               _ -> []
       del,sta,qn,ep :: Int
       (del,sta,qn,ep) -- delta (direction), start, Queening and E.P. Rank
           = case col of
               White -> (1,2,8,5)
               Black -> (-1,7,1,4)

 movePiece xd yd brd x y = 
       case lookupBoard brd (x',y') of
         OffBoard -> []
         Friendly -> []
         Baddy    -> [(x',y')]
         Vacant   ->  (x',y') : movePiece xd yd brd x' y'
       where
           x' = x + xd
           y' = y + yd 




 makeAMove :: Board -> PlayMove -> Board
 makeAMove board@(Board brd mv@(MoveNumber _ col) _)
       move@(PlayMove piece pos pos' NothingSpecial)  =
       Board (brd //  [ pos =: VacantSq,
                       pos' =: mkColBoardSq col piece ])
                       (incMove mv) Nothing
 makeAMove board@(Board brd mv@(MoveNumber _ col) _)
       move@(PlayMove piece pos@(f,_) pos' BigPawnMove)  =
       Board (brd //  [ pos =: VacantSq,
                       pos' =: mkColBoardSq col piece ])
                       (incMove mv) (Just f)
 makeAMove board@(Board brd mv@(MoveNumber _ col) _)
       move@(PlayMove piece pos@(f,_) pos' (Queening q))  =
       Board (brd //  [ pos =: VacantSq,
                       pos' =: mkColBoardSq col q])
                       (incMove mv) (Just f)
 makeAMove board@(Board brd mv@(MoveNumber _ col) _)   -- ASSERT ?
       move@(PlayMove piece (f,_) (f',_) EnPassant) =
       Board (brd // [ (f,st) =: VacantSq,
                       (f',fn) =: mkColBoardSq col Pawn,
                       (f',st) =: VacantSq ])
                       (incMove mv) Nothing
   where (st,fn) = case col of
                     White -> (5,6)
                     Black -> (4,3)

 makeACastleK (Board brd mv@(MoveNumber _ White) _) =
       Board (brd //
             [ (5,1) =: VacantSq,
               (6,1) =: mkColBoardSq White Rook,
               (7,1) =: mkColBoardSq White King,
               (8,1) =: VacantSq ]) (incMove mv) Nothing
 makeACastleK (Board brd mv@(MoveNumber _ Black) _) =

       Board (brd //
             [ (5,8) =: VacantSq,
               (6,8) =: mkColBoardSq Black Rook,
               (7,8) =: mkColBoardSq Black King,
               (8,8) =: VacantSq ]) (incMove mv) Nothing
 makeACastleQ (Board brd mv@(MoveNumber _ White) _) =
       Board (brd //
             [ (5,1) =: VacantSq,
               (4,1) =: mkColBoardSq White Rook,
               (3,1) =: mkColBoardSq White King,
               (1,1) =: VacantSq ]) (incMove mv) Nothing
 makeACastleQ (Board brd mv@(MoveNumber _ Black) _) =
       Board (brd //
             [ (5,8) =: VacantSq,
               (4,8) =: mkColBoardSq Black Rook,
               (3,8) =: mkColBoardSq Black King,
               (1,8) =: VacantSq ]) (incMove mv) Nothing

 disAmb _ [_] = ""
 disAmb (a,b) t@[(n,m),(x,y)] 
       | n == x    = userFormatRank b
       | otherwise = userFormatFile a
 disAmb src lst = error ("PANIC: cant disambiguate: " ++ show src ++ show lst)

 createShortMove :: PlayMove -> String -> Board -> String
 createShortMove (PlayMove Pawn (f,r) dest q) "" brd = 
       (if lookupBoard brd dest == Baddy || EnPassant == q
        then userFormatFile f ++ "x" ++ userFormatBoardPos dest
        else userFormatBoardPos dest) ++
       case q of
         Queening p -> "=" ++ userFormat p
         _ -> ""
 createShortMove (PlayMove p _ dest _) extra brd =
       userFormat p ++ extra ++ capt ++ userFormatBoardPos dest
   where
       capt = if lookupBoard brd dest == Baddy
              then "x"
              else ""

 getEPStart :: Colour -> ChessFile
 getEPStart White = 5
 getEPStart Black = 4

 getEPEnd :: Colour -> ChessFile
 getEPEnd White = 6
 getEPEnd Black = 3

 getHomeRank :: Colour -> ChessRank
 getHomeRank White = 1
 getHomeRank Black = 8

