module Main where

import Auto

import Tactics

import ThmTactics

import Display

import Tree

import Vtslib

import Editor

import Globals

import Getops

import Edlib

import Lookup

import X_interface

import Goals

import Type_defs

import Core_datatype

import Kernel

import Tags

import Parse
 
import System.IO


--proof_edit : string list * string list -> unit 

main = do
    hSetBinaryMode stdin  True
    hSetBinaryMode stdout True
    ins <- getContents
    putStr (main' ins)

main' instr 
	= rqts 
	  where
	  ( _ , _ , rqts ) = edits
	  edits = proof_edit default_ds ( split '\n' args ) (instr , rsps , [1..]) 
          default_ds = "" --home ++ "/VTS"
	  args = "" -- temp test
	  rsps = [0..] -- dummy response list
	  










{-
--	= parse_spec_trm (strings_to_input [s]) 
--		(set_lookup_table initial_state lt) sg

-- other parsers later

parse_sgn lt sg s 
	= sgparse_spec_sgn (strings_to_input [s]) 
		(set_lookup_table initial_state lt)

parse_dec lt sg s 
	= parse_spec_dec (strings_to_input [s]) 
		(set_lookup_table initial_state lt) sg
-}



goto_next tr@(TreeSt t _ _) 
	= tree_top tr /./
	  (\ tr' -> case tree_search incomplete_tree False t of
	       	        ((iL,_):_) -> reTurn ( tree_goto iL tr )
	                _          -> case search_tree of
				          ((iL,_):_) -> reTurn(tree_goto iL tr')
				          _          -> reTurn tr 
			  	      where
			              (TreeSt t' _ _) = tr' 
	  			      search_tree = tree_search 
						      incomplete_tree False t' )




incomplete_tree (Tree _ tl (SOME _) _ _ ) = False

incomplete_tree (Tree _ tl NONE _ _ ) = forall is_complete tl





goto_named tr@(TreeSt t _ _ ) 
	= x_form True form /.>/
	  tree_top tr /./ 
	  exp 
	  where
	  exp ( SOME [OutText uid] , tr' ) 
		  = case tree_search is_node True t' of
			 ((iL,_):_) -> reTurn ( tree_goto iL tr' )
			 _          -> x_set_status "No such node"  ./.
				       reTurn tr 
		    where
		    (TreeSt t' _ _ ) = tr'
		    is_node (Tree (Goal _ _ _ uid' _ _ _ _) _ _ _ _ ) 
				= uid == uid'
	  exp _ = reTurn tr 

	  form = [InComment "Goto Node",
		  InComment "",
		  InComment "Uid of Node",
		  InSingleText "" ""]

tree_cmdL = 
	[
	    ( "Top", tree_top ),
	    ( "P",	tree_up ),
	    ( "PExpand", show_goal_cmd ),
	    ( "1Expand", show_subgoal_cmd 0),
	    ( "2Expand", show_subgoal_cmd 1),
	    ( "3Expand", show_subgoal_cmd 2),
	    ( "4Expand", show_subgoal_cmd 3),
	    ( "Theory",  show_theory ),
	    ( "Object",  show_object ),
	    ( "Comment", show_comment),
	    ( "Arguments", show_arguments),
	    ( "1", 	tree_down 0 ),
	    ( "2",	tree_down 1),
	    ( "3",	tree_down 2),
	    ( "4",	tree_down 3),
	    ( "Next", goto_next),
	    ( "Named", goto_named),
	    ( "Undo",tree_undo),
	    ( "Tactic",show_tactics),
	    ( "Strip",strip_tac),
	    ( "Triv",triv_tac),
	    lift_tactic conj_tac,
	    lift_ordtactic gen_tac,
	    lift_tactic auto_tac,
	    lift_tactic reflex_tac,
	    lift_ordtactic disch_tac,
	    lift_tactic disj_tac,
	    lift_tactic eq_tac,
	    lift_tactic or_tac,
	    lift_tactic lemma_tac,
	    lift_tactic not_tac,
	    lift_tactic exists_elim_tac,
	    lift_tactic taut_tac,
	    lift_tactic axiom_tac,
	    lift_tactic hyp_tac,
--	    lift_ordtactic split_tac,
	    lift_tactic induction_tac,
	    lift_tactic complete_tac
{-,
	    lift_tactic rewrite_tac,
-}
	]
strip_tac = strip (error "") auto_tac


triv_tac = triv auto_tac


edit = editor tree_cmdL initialize update_state my_quit finish 


proof_edit default_ds argL 
--	= getops "d:s:t:" argL 
	= exp ( [] , [] ) --('t',SOME "Trm")], ["hello"]) --temp arg parse 
	  where
	  exp _ --(options, [arg])  
		= edit 
	  	  `handle`
	  	  err_handler
	  err_handler mesg = x_error mesg /./ 
		             ( \ _ -> proof_edit default_ds argL )
