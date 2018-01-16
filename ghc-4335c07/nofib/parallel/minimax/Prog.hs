-- Time-stamp: <2009-05-06 13:55:20 simonmar>
-----------------------------------------------------------------------------

module Prog(prog,randomBoard,solve) where

import Board
import Wins
import Game
import Tree
import System.Random
import Data.List

-- First arg decaffinates game
prog :: Int -> String
prog decaf = showMove (head game)
	       --"OXO\n" ++
	       --concat (map showMove game)
	       where
	       game = if decaf == 0 
	                then error "Decaffination error\n"
			else alternate decaf X maxE minE testBoard

-- X to play: find the best move
solve :: Int -> Board -> String
solve depth board
  = unlines
  . map showMove
  . take 1
  . alternate depth X maxE minE $ board

testBoard = [[Empty,O,Empty,Empty],[Empty,X,Empty,Empty],[Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty]]

randomBoard :: Int -> IO Board
randomBoard moves =  do
  g <- newStdGen
  let (g1,g2) = split g
      xs = randomRs (1,boardDim) g1
      ys = randomRs (1,boardDim) g2

  let
    play 0 _ _ board = board
    play n (pos:poss) (p:ps) board
     | not (empty pos board) = play n poss (p:ps) board
     | otherwise             = play (n-1) poss ps (placePiece p board pos)

  return $ play moves (zip xs ys) (cycle [X,O]) initialBoard

