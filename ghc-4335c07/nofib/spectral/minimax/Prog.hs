module Prog(prog) where

import Board
import Wins
import Game
import Tree


prog :: String -> String
prog _ = 
	"OXO\n" ++
	concat (map showMove game)
	where
	game = alternate X max' min' testBoard


testBoard = [[Empty,O,Empty],[Empty,X,Empty],[Empty,Empty,Empty]]

