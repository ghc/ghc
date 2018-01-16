-- Time-stamp: <2011-10-07 11:36:17 simonmar>
-----------------------------------------------------------------------------

module Game where

import Board
import Tree

import Control.Parallel
import Control.Parallel.Strategies
import Debug.Trace

type Player = Evaluation -> Evaluation -> Evaluation
type Move = (Board,Evaluation)

alternate :: Int -> Piece -> Player -> Player -> Board -> [Move]
alternate _ _ _ _ b | fullBoard b = []
alternate _ _ _ _ b | static b == XWin = []
alternate _ _ _ _ b | static b == OWin = []
alternate depth player f g board = move : alternate depth opponent g f board'
	where
	move@(board',eval) = best f possibles scores
        scores = map (bestMove depth opponent g f) possibles `using` parList rseq
	possibles = newPositions player board
        opponent = opposite player

opposite :: Piece -> Piece
opposite X = O
opposite O = X


best :: Player -> [Board] -> [Evaluation] -> Move
best f (b:bs) (s:ss) = best' b s bs ss
	where
	best' b s [] [] = (b,s)
	best' b s (b':bs) (s':ss) | s==(f s s') = best' b s bs ss
				  | otherwise 	= best' b' s' bs ss

showMove :: Move -> String
showMove (b,e) = show e ++ "\n" ++ showBoard b

bestMove :: Int -> Piece -> Player -> Player -> Board -> Evaluation
bestMove depth p f g 
  = parMise 2 f g 
  . cropTree
  . mapTree static
  . prune depth
  . searchTree p

cropTree :: (Tree Evaluation) -> (Tree Evaluation)
cropTree (Branch a []) = (Branch a [])
cropTree (Branch (Score x) l) = Branch (Score x) (map cropTree l)
cropTree (Branch x l) = Branch x []

searchTree :: Piece -> Board -> (Tree Board)
searchTree p board = repTree (newPositions p) (newPositions (opposite p)) board

mise :: Player -> Player -> (Tree Evaluation) -> Evaluation
mise f g (Branch a []) = a
mise f g (Branch _ l) = foldr f (g OWin XWin) (map (mise g f) l)

parMise :: Int -> Player -> Player -> (Tree Evaluation) -> Evaluation
parMise 0 f g t = mise f g t
parMise n f g (Branch a []) = a
parMise n f g (Branch _ l) = foldr f (g OWin XWin) (map (parMise (n-1) g f) l `using` parList rseq)
