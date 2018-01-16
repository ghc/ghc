module Tactics where

import Core_datatype
import X_interface

import Vtslib

import Globals

import Lookup

import Kernel

import Edlib

import Goals

import Tags	-- partain

import Tree

import Type_defs

data TACTIC 
	= Tactic String		
		 ( Global_state -> Lookup_table -> Obj -> Xin ->
				Xst ( MayBe ( Option [String] ) String ) )
		 ( Global_state -> Sgn -> Lookup_table -> Option [String] -> Obj
		    -> MayBe ([Obj] , ( [Option Done] -> Option Done )) String )

data Ordered_tactic 
	= OrdTactic String
		    ( Global_state -> Lookup_table -> Obj -> Xin ->
				Xst ( MayBe ( Option [String] ) String ) )
		    ( Global_state -> Sgn -> Lookup_table -> 
			Option [String] -> Obj -> 
			 MayBe ( [Obj] , [Lookup_table] , [Bool] ,
			    ([Option Done] -> [Bool] -> 
				( [Bool] , [Sgn] , Option Done ))) String )




lift_tactic (Tactic name arg_fn subgoal_fn) 
    = ( name , lift )
      where
      lift (TreeSt t@(Tree g [] dn vf u) spine gst ) 
	  = arg_fn gst lt obj /./
	    exp
	    where
	    (Goal NONE NONE com uid obj rw sg lt) = g
	    exp args 
		= return_val ( subgoal_fn gst sg lt args obj ) /./
		  exp'
	          where
		  exp' ( subgoals , valid_fn ) 
			= make_goal1 sg lt subgoals [] /./
			  exp'' 
			  where
	                  exp'' subtrees
			       = reTurn ( TreeSt (vf' gst t1) spine gst )
			  	 where
	  	  	         vf' = lift_tactic_valid valid_fn
	  	                 g1 =Goal (SOME name) args com uid obj rw sg lt
	  	                 t1 = Tree g1 subtrees NONE vf' (SOME t)
      lift _ = return_err "cannot apply tactic" 



lift_ordtactic (OrdTactic name arg_fn subgoal_fn) 
    = ( name , lift )
      where
      lift (TreeSt t@(Tree g [] dn vf u) spine gst ) 
	  = arg_fn gst lt obj /./
	    exp
	    where
	    (Goal NONE NONE com uid obj rw sg lt) = g
	    exp args 
	        = return_val ( subgoal_fn gst sg lt args obj ) /./
		  exp'
	          where
		  exp' ( subgoals , ltL , rwL , valid_fn )
		       = make_goal2 sg (zip ltL (zip rwL subgoals)) [] /./
			 exp''
			 where
		         exp'' subtrees
				= reTurn ( TreeSt (vf' gst t1) spine gst )
			  	  where
	  	  	  	  vf' = lift_ordtactic_valid valid_fn
	  	          	  g1 =Goal (SOME name) args com uid obj rw sg lt
	  	          	  t1 = Tree g1 subtrees NONE vf' (SOME t)
      lift _ = return_err "cannot apply tactic" 



lift_tactic_valid vf gst t@(Tree g tl NONE vf' u) 
	= Tree g tl dn vf' u
--	  `handle`
--	   _ -> t
	  where
	  dnL = map get_done tl
	  dn  = vf dnL 

lift_tactic_valid _ _ t = t




lift_ordtactic_valid vf gst t @(Tree g tl NONE vf' u) 
	= Tree g tl' dn vf' u
--	    handle _ => t
	  where
	  dnL = map get_done tl
	  bL  = map get_rw tl
	  (bL',sgL,dn) = vf dnL bL
	  tl' = map set_info (zip tl (zip bL' sgL))

lift_ordtactic_valid _ _ t = t




make_goal1 sg lt ( obj : objL ) glL
	= make_goal1' sg lt obj /./
	  ( \ gl -> make_goal1  sg lt objL ( glL <: gl ))

make_goal1 _ _ [] glL 
	= reTurn glL 




make_goal1' sg lt obj
	= genuid /./
	  ( \ uid -> reTurn ( Tree (Goal NONE NONE NONE uid obj True sg lt)
							 [] NONE id' NONE ))



make_goal2 sg ((lt, (rw, obj)) : pL ) glL 
	= make_goal2' sg lt rw obj /./
	  ( \ gl -> make_goal2  sg pL ( glL <: gl ))

make_goal2 _ [] glL 
	= reTurn glL 




make_goal2' sg lt rw obj
	= genuid /./
	  ( \ uid -> reTurn ( Tree (Goal NONE NONE NONE uid 
					obj rw sg lt) [] NONE id' NONE ))


get_done (Tree _ _ dn _ _) = dn

get_rw (Tree (Goal _ _ _ _ _ rw _ _) _ _ _ _) = rw

set_info (Tree (Goal cmd arg com uid obj _ _ lt) tl dn vf u , (rw,sg)) 
	= Tree (Goal cmd arg com uid obj rw sg lt) tl dn vf u

id' x y = y

null_arg_fn gst lt spc = reTurn NONE 
