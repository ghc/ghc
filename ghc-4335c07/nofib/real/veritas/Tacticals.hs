module Tacticals where

import Core_datatype
import Kernel
import Lookup
import X_interface

import Tactics

import Goals
import Globals		-- partain
import Tags		-- partain

import Tree

import Edlib

import Type_defs

import Vtslib



infixr 5 `orelse`

infixr 4 `andthen`



orelse f1 f2 trst 
	= f1 trst
	  `handle` 
	  ( \ _ -> f2 trst )



andthen f1 f2 trst 
	= f1 trst     /./
	  subtrst f2 


repeat_tac f trst 
	= f trst /./
	  ( \ trst' -> subtrst (repeat_tac f) trst' 
	               `handle` 
	               ( \ _ -> reTurn trst ))



for' 0 f trst = reTurn trst

for' i f trst 
	= f trst                  /./
	  subtrst (for' (i-1) f) 



subtrst f trst@(TreeSt (Tree _ trL _ _ _) _ _) 
	= subtrst' 0 (length trL) f trst


subtrst' i j f trst 
	| i >= j    = reTurn trst
	| otherwise = tree_down i trst   /./
                      f                  /./
                      tree_up            /./
		      subtrst' (i+1) j f
