module Game where

import Board
import Wins
import Tree

type Player = Evaluation -> Evaluation -> Evaluation
type Move = (Board,Evaluation)



alternate :: Piece -> Player -> Player -> Board -> [Move]
alternate _ _ _ b | fullBoard b = []
alternate _ _ _ b | static b == XWin = []
alternate _ _ _ b | static b == OWin = []
alternate player f g board = move:alternate opposition g f board'
	where
	move@(board',eval) = best f possibles scores
	scores = map (bestMove opposition g f) possibles
	possibles = newPositions player board
	opposition = opposite player

opposite :: Piece -> Piece
opposite X = O
opposite O = X


best :: Player -> [Board] -> [Evaluation] -> Move
best f (b:bs) (s:ss) = best' b s bs ss
	where
	best' b s [] [] = (b,s)
	best' b s (b':bs) (s':ss) | s==(f s s') = best' b s bs ss
				  | otherwise 	    = best' b' s' bs ss

showMove :: Move -> String
showMove (b,e) = show e ++ "\n" ++ showBoard b

bestMove :: Piece -> Player -> Player -> Board -> Evaluation
bestMove p f g = (mise f g).cropTree.mapTree static.searchTree p

cropTree :: (Tree Evaluation) -> (Tree Evaluation)
cropTree (Branch a []) = (Branch a [])
cropTree (Branch (Score x) l) = Branch (Score x) (map cropTree l)
cropTree (Branch x l) = Branch x []

searchTree :: Piece -> Board -> (Tree Board)
searchTree p board = prune 5 (repTree (newPositions p) (newPositions (opposite p)) board)

mise :: Player -> Player -> (Tree Evaluation) -> Evaluation
mise f g (Branch a []) = a
mise f g (Branch _ l) = foldr f (g OWin XWin) (map (mise g f) l)


max' :: Evaluation -> Evaluation -> Evaluation
max' XWin _ = XWin
max' _ XWin = XWin
max' b OWin = b
max' OWin b = b
max' a@(Score x) b@(Score y) 	| x>y = a
			 	| otherwise = b

min' :: Evaluation -> Evaluation -> Evaluation
min' OWin _ = OWin
min' _ OWin = OWin
min' b XWin = b
min' XWin b = b
min' a@(Score x) b@(Score y) 	| x<y = a
				| otherwise = b

