

 module Interp (runInterp) where

 import GenUtils
 import DataTypes
 import InterpUtils
 import Parser (pgnLexer)



 runInterp :: AbsGame -> RealGame
 runInterp (Game tags toks) = Game tags (pgnInterp toks initParState)



 initParState = (FirstBoard startBoard)

 type Par a = StoreBoard -> a
 thenP :: Par a -> (a -> Par b) -> Par b
 returnP :: a -> Par a

 returnP a = \s -> a
 thenP m k s = case m s of
                 r -> k r s
                 
 failP a = \s -> error a
 consP q rest = \s -> q : pgnInterp rest s
 thenP' :: Par StoreBoard -> Par a -> Par a
 thenP' m k s = case m s of
                 r -> k r 
 newGameP :: Par a -> Par a
 newGameP m = \ _ -> m initParState

 getCurrColour :: Par Colour
 getCurrColour = 
       getBoard                `thenP` \ (Board _ (MoveNumber _ col) _) ->
       returnP col

 checkColour :: MoveNumber -> Par ()
 checkColour (MoveNumber i col) =
       getBoard                `thenP` \ (Board _ (MoveNumber i' col') _) ->
       if i == i' && col == col' 
       then returnP ()
       else failP ("number mis-match: " 
               ++ userFormat (MoveNumber i col) 
               ++ " (looking for " 
               ++ userFormat (MoveNumber i' col') 
               ++ ")\n")



 data StoreBoard 
       = FirstBoard Board
       | UndoableBoard Board {- new -} Board {- back one -}

 updateBoard :: Board -> Par StoreBoard
 updateBoard brd (FirstBoard old_brd) 
       = UndoableBoard brd old_brd
 updateBoard brd (UndoableBoard old_brd _) 
       = UndoableBoard brd old_brd

 getBoard :: Par Board
 getBoard s@(FirstBoard brd) 
       = brd
 getBoard s@(UndoableBoard brd _) 
       = brd

 undoBoard :: Par StoreBoard
 undoBoard (FirstBoard _) 
       = error "Incorrect start to some analysis"
 undoBoard (UndoableBoard _ old_brd)
       = FirstBoard old_brd



 pgnInterp :: [Token] -> Par [Quantum]
 pgnInterp (IntToken n:PeriodToken:PeriodToken:PeriodToken:rest) =
       checkColour (MoveNumber n Black)                `thenP` \ () ->
       pgnInterp rest
 pgnInterp (IntToken n:PeriodToken:rest) =
       checkColour (MoveNumber n White)                `thenP` \ () ->
       pgnInterp rest

 pgnInterp (SymbolToken str:CommentToken (ann:rs):r)
       | all (flip elem "!?") ann =
       pgnInterp (SymbolToken str:pgnLexer ann ++ (CommentToken rs:r))




 pgnInterp (CommentToken (n:tag:rest):r)
       | head tag == '(' && take 2 (reverse tag) == ":)" && length rest > 1 =
       getCurrColour                           `thenP` \ col ->
       let 
           invert Black r   = r -- because the move has *already* happend
           invert _ "0.00"  = "0.00"   -- dont negate 0
           invert _ ('-':r) = r
           invert _ r       = '-':r
       in 
       pgnInterp (LeftRBToken:map SymbolToken (take (length rest-1) rest)
               ++ [CommentToken ["Score:",invert col n],RightRBToken] ++ r)


 pgnInterp (CommentToken []:rest) = pgnInterp rest
 pgnInterp (CommentToken comm:rest) =
       consP (QuantumComment comm) rest
 pgnInterp (NAGToken nag:rest) =
       consP (QuantumNAG nag) rest
 pgnInterp (NAGAnnToken nag _:rest) =
       consP (QuantumNAG nag) rest
 pgnInterp (SymbolToken "0-1":rest) =
       consP (QuantumResult "0-1") rest
 pgnInterp (SymbolToken "1-0":rest) =
       consP (QuantumResult "1-0") rest
 pgnInterp (SymbolToken "1/2-1/2":rest) =
       consP (QuantumResult "1/2-1/2") rest
 pgnInterp (AsterixToken:rest) =
       consP (QuantumResult "*") rest
 pgnInterp (SymbolToken move:rest@(NAGAnnToken _ str:_)) =
       getBoard                `thenP` \ brd ->
       parseMove move brd      `thenP` \ (mv,ch,corrMv,new_brd) ->
       updateBoard new_brd     `thenP'`
       consP (QuantumMove mv ch str new_brd) rest
 pgnInterp (SymbolToken move:rest) =
       getBoard                `thenP` \ brd ->
       parseMove move brd      `thenP` \ (mv,ch,corrMv,new_brd) ->
       updateBoard new_brd     `thenP'`
       consP (QuantumMove mv ch "" new_brd) rest
 pgnInterp (LeftRBToken:rest) =
       getAnalysis rest 0 []   `thenP` \ (anal,rest) -> 
       (undoBoard              `thenP'`
       pgnInterp anal)         `thenP` \ anal' ->
       consP (QuantumAnalysis anal') rest
 pgnInterp [] = returnP []
 pgnInterp toks = failP ("when reading: " 
               ++ unwords (map userFormat (take 10 toks)))



 getAnalysis (t@LeftRBToken:r) n anal = getAnalysis r (n+1) (t:anal)
 getAnalysis (t@RightRBToken:r) n anal 
       | n == (0 :: Int) = returnP (reverse anal,r)
       | otherwise = getAnalysis r (n-1) (t:anal)
 getAnalysis (t:r) n anal = getAnalysis r n (t:anal)
 getAnalysis [] n anal = failP "no closing ')'"




 parseMove :: String -> Board -> Par (String,String,String,Board)
 parseMove move brd@(Board _ (MoveNumber _ col) _) = 
   case mapMaybeFail charToMoveTok move of
    Nothing -> failP ("strange move:" ++ move)
    Just mv_toks ->
       let 
          (chs,mv_toks') = getChecks (reverse mv_toks)
          (queen,mv_toks'') = getQueen mv_toks'
       in 
       case parseAlgMove mv_toks'' queen brd of 
         (the_mv,new_brd) -> returnP (the_mv,chs,"$$",new_brd)



 parseAlgMove 
       :: [MoveTok]
       -> Maybe Piece 
       -> Board 
       -> (String,Board)
 parseAlgMove [PartCastleTok,MoveToTok,PartCastleTok] Nothing
               = findCastleKMove
 parseAlgMove [PartCastleTok,MoveToTok,PartCastleTok,
                   MoveToTok,PartCastleTok] Nothing
               = findCastleQMove

 parseAlgMove (PieceTok King:r) Nothing   = parsePieceMove r King 
 parseAlgMove (PieceTok Queen:r) Nothing  = parsePieceMove r Queen 
 parseAlgMove (PieceTok Rook:r) Nothing   = parsePieceMove r Rook 
 parseAlgMove (PieceTok Knight:r) Nothing  = parsePieceMove r Knight 
 parseAlgMove (PieceTok Bishop:r) Nothing  = parsePieceMove r Bishop 



 parseAlgMove [FileTok sf,RankTok sr,MoveToTok,FileTok df,RankTok dr] q  =
       findAPawnMove (extendBP (sf,sr)) (extendBP (df,dr)) q 
 parseAlgMove [FileTok sf,RankTok sr,CaptureTok,FileTok df,RankTok dr] q  =
       findAPawnMove (extendBP (sf,sr)) (extendBP (df,dr)) q 



 parseAlgMove [FileTok sf,RankTok sr,FileTok df,RankTok dr] q = \ brd -> 
   case lookupBoardPiece brd (sf,sr) of
       Nothing -> error ("cant find piece at: " ++ userFormatBoardPos (sf,sr))
       Just Pawn -> findAPawnMove (extendBP (sf,sr)) (extendBP (df,dr)) q brd
       Just King | sf == 5 && df == 7 -> findCastleKMove brd
       Just King | sf == 5 && df == 3 -> findCastleQMove brd
       Just p -> findAMove p (extendBP (sf,sr)) (extendBP (df,dr)) brd

 -- later !



 parseAlgMove [FileTok df,RankTok dr] q =
       findAPawnMove (Nothing,Nothing) (extendBP (df,dr)) q 



 parseAlgMove [FileTok sf,CaptureTok,FileTok df,RankTok dr] q  =
       findAPawnMove (Just sf,Nothing) (extendBP (df,dr)) q 



 parseAlgMove [FileTok sf,FileTok df] q  =
       findAPawnMove (Just sf,Nothing) (Just df,Nothing) q 



 parseAlgMove [FileTok sf,CaptureTok,FileTok df] q  =
       findAPawnMove (Just sf,Nothing) (Just df,Nothing) q 
 parseAlgMove _ _ = error "!>!"



 parsePieceMove [FileTok df,RankTok dr] p
       = findAMove p (Nothing,Nothing) (extendBP (df,dr)) 



 parsePieceMove [CaptureTok,FileTok df,RankTok dr] p
       = findAMove p (Nothing,Nothing) (extendBP (df,dr))



 parsePieceMove [RankTok sr,FileTok df,RankTok dr] p 
       = findAMove p (Nothing,Just sr) (extendBP (df,dr))
 parsePieceMove [RankTok sr,CaptureTok,FileTok df,RankTok dr] p
       = findAMove p (Nothing,Just sr) (extendBP (df,dr))



 parsePieceMove [FileTok sf,FileTok df,RankTok dr] p 
       = findAMove p (Just sf,Nothing) (extendBP (df,dr))
 parsePieceMove [FileTok sf,CaptureTok,FileTok df,RankTok dr] p
       = findAMove p (Just sf,Nothing) (extendBP (df,dr))



 parsePieceMove [FileTok sf,RankTok sr,MoveToTok,FileTok df,RankTok dr] p
       = findAMove p (extendBP (sf,sr)) (extendBP (df,dr))
 parsePieceMove [FileTok sf,RankTok sr,CaptureTok,FileTok df,RankTok dr] p
       = findAMove p (extendBP (sf,sr)) (extendBP (df,dr))
 parsePieceMove _ p = failP ("syntax error in move:")

 getChecks (CheckTok:CheckTok:r) = ("#",r)
 getChecks (CheckTok:r) = ("+",r)
 getChecks (MateTok:r)  = ("#",r)
 getChecks r            = ("",r)

 getQueen (PieceTok p:QueensWith:r) = (Just p,reverse r)
 getQueen r = (Nothing,reverse r)


