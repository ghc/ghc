module Display where

import Core_datatype

import Edlib

import X_interface

import Kernel

import Parse

import Unparse

import Vtslib

import Type_defs

import Lookup

import Globals

import Tags

import Goals

import Tree


typeL = ["Thm","Trm","Sgn","Dec"]


wins  = ["Own","Scratch"]



select :: (Eq b) => b -> [b] -> Int

select s [] = 0

select s (s' : l) = if s==s' then 0 else 1+select s l



make_form ty sp win err
	= [
	        InComment "         Parse Object",
		InRadio "Type of Object" 
			(case ty of 
				SOME x -> select x typeL 
				_      -> 0 )
			typeL ,
		InComment "",
		InMultiText "Specification"
			    ( case sp of
				SOME x -> x 
				_      -> "" ),
		InRadio "Display Window" 
			( case win of 
				SOME x -> select x wins 
				_      -> 0 )
			wins
	    ] ++
	    (case err of 
		SOME err' -> [InComment err'] 
		_         -> [])

{-
unerr (Unbound s) = "Unbound symbol " ^ s

unerr (UnboundRec s) = "Unbound rec symbol " ^ s

unerr NonAtomic = "Not atomic"

unerr (NonInfixBinder s) = "Expected infixbinder, found " ^ s

unerr (NonNumber s) = "Not a number " ^ s

unerr (Reserved s) = "Unexpected reserved word " ^ s

unerr OtherError = "Syntax error"
-}

--eexperr t1 t2 = "Expected "^t1^" but found "^t2


theory_form = [
                InComment "     Show Theory",
                InRadio  "Display Theory" 1 ["Strcutured","Unstructered",
                                              "Just Names"]
              ]


	

show_goal_cmd tr@(TreeSt t _ gst) 
	= x_display title display ./.
	  reTurn tr 
	  where
	  title   = (case obj of 
			ThmSpec _ -> "Thm Goal No: "
                        TrmSpec _ -> "Trm Goal No: " 
                        SgnSpec _ -> "Sgn Goal No: "
                        DecSpec _ -> "Dec Goal No: " )
		     ++ uid
	  display = unparse_obj sg gst obj
	  (Tree (Goal _ _ _ uid obj _ sg _) _ _ _ _ ) = t



show_subgoal_cmd :: Int -> (Tree_state GOAL b (c, d, e)) -> Xin 
			-> Xst ( MayBe (Tree_state GOAL b (c, d, e)) f )

show_subgoal_cmd i tr@( TreeSt (Tree _ tl _ _ _) _ gst) 
	= x_display title display ./.
	  reTurn tr 
	  where
	  title = ( case obj of 
 			ThmSpec _ -> "Thm Goal No: "
                        TrmSpec _ -> "Trm Goal No: "
                        SgnSpec _ -> "Sgn Goal No: "
                        DecSpec _ -> "Dec Goal No: " )
	           ++ uid
	  display = unparse_obj sg gst obj
	  ( Tree (Goal _ _ _ uid obj _ sg _) _ _ _ _ ) = tl !! i



show_theory tr@( TreeSt (Tree g _ _ _ _) _ gst) 
	= x_form True theory_form /./
	  disp   
	  where
	  disp ( SOME [(OutRadio "Strcutured")] )
		= show_theory' tr (unparse_Sgn (error "") (get_attributes gst)) g 
          disp ( SOME [(OutRadio "Unstructered")] )
		= show_theory' tr display_Sgn g
          disp ( SOME [(OutRadio "Just Names")] )
                = show_theory' tr summary_Sgn g 
          disp _ = reTurn tr 

	  display_Sgn = error "unimplemented in display"
	  summary_Sgn = error "unimplemented in display"






show_theory' tr unparse_fn (Goal _ _ _ uid _ True sg lt) 
	= x_display title display ./.
	  reTurn tr 
          where
          title   = "Signature of Node No. " ++ uid
          display = unparse_fn sg

show_theory' tr unparse_fn (Goal _ _ _ _ _ False _ _) 
	= x_set_status "Theory not yet defined" ./.
	  reTurn tr 




		
show_object tr@( TreeSt (Tree g _ _ _ _) _ gst) 
	= case g of
              	Goal _ _ _ uid _ True sg lt 
	            -> show_obj sg gst' sg att NONE NONE NONE NONE /./
		       ( \ _ -> reTurn tr )
		       where
--	               gst' = get_default_ps gst
	               gst' = gst -- find get_default_ps
		       att  = get_attributes gst
                Goal _ _ _ _ _ False _ _ 
		    -> x_set_status "Theory not yet defined" ./.
		       reTurn tr 




show_com t@(Tree (Goal _ _ (SOME com) uid _ _ _ _) _ _ _ _) 
	= x_display ("Comment for Node No: " ++ uid) com ./.	
	  reTurn t 

show_com t@(Tree (Goal _ _ NONE _ _ _ _ _) _ _ _ _) 
	= x_set_status "No Comment defined"  ./.
	  reTurn t 


get_comment (Tree (Goal _ _ NONE _ _ _ _ _) _ _ _ _) = ""

get_comment (Tree (Goal _ _ (SOME s) _ _ _ _ _) _ _ _ _) = s




set_comment "" (Tree (Goal a b _ d e f g h) i j k l) 
	= Tree (Goal a b NONE d e f g h) i j k l

set_comment c (Tree (Goal a b _ d e f g h) i j k l) 
	= Tree (Goal a b (SOME c) d e f g h) i j k l




unparse_obj sg gst (TrmSpec tm) 
	= unparse_trm sg (get_attributes gst) tm

unparse_obj sg gst (ThmSpec tm) 
	= unparse_trm sg (get_attributes gst) tm

unparse_obj sg gst (DecSpec dc) 
	= unparse_dec sg (get_attributes gst) dc

unparse_obj sg gst (SgnSpec isg) 
	= unparse_sgn sg (get_attributes gst) isg



{- no - valid result (Ok "" ) returned - only possibility of error - deal with this ? -}

show_obj sg ps lt attL ty sp win err 
	= x_form True (make_form ty sp win err) /./
	  exp 
	  where
	  exp NONE = reTurn "" 

	  exp ( SOME [OutRadio obj_type, OutText obj_spec, OutRadio win] ) 
		= display_fn (show_fn obj_spec) ./.
	  	  reTurn "" 
	          where
		  display_fn = case win of
				       "Own"     -> own_win obj_type
				       "Scratch" -> scratch_win obj_type
				       _         -> own_win obj_type
		  show_fn = case obj_type of
				      "Trm" -> show_Trm sg ps lt attL
{-
				      "Thm" -> show_Thm sg ps lt attL
				      "Sgn" -> show_Sgn sg ps lt attL
				      "Dec" -> show_Dec sg ps lt attL
-}
				      _     -> show_error

--	  exp _ = reTurn ""  

{-
			handle 
			    Syntax (e,inp) => 
				let val err_mesg = unerr e 
				    val where = under_line_input inp
				    val new_form = redo err_mesg where info
			        in show_obj xio sg ps lt attL new_form end
			  | Expect (t1,t2,inp) => 
				let val err_mesg = eexperr t1 t2
				    val where = under_line_input inp
				    val new_form = redo err_mesg where info
			        in show_obj xio sg ps lt attL new_form end
			  | Fail s => 
				let val err_mesg = "Error: "^ s
				    val where = ""
				    val new_form = redo err_mesg where info
			        in show_obj xio sg ps lt attL new_form end)
-}




{-
redo err whre (SOME [OutRadio obj_type, OutText obj_spec, OutRadio win]) 
	= [
	        InComment "Parse Object",
		InRadio "Type of Object" ["Trm","Thm","Sgn","Dec"],
		InComment "Specification of Object",
		InMultiText obj_spec,
		InComment err,
		InComment whre,
		InRadio "Display Window" ["Own", "Scratch"]
	  ]
-}





show_Trm sg ps lt attL tm_rep 
	= unparse_Trm sg attL 
		(parse_tm (tgL,sg) tm_rep )
--		(parse_typed_Trm tm_rep ps sg) 


{-
show_Thm sg ps lt attL th_rep 
	= unparse_Thm lt attL 
		(parse_Thm th_rep ps sg)



show_Sgn sg ps lt attL sg_rep 
	= unparse_Sgn lt attL 
		(parse_typed_Sgn sg_rep ps) 



show_Dec sg ps lt attL dc_rep 
	= unparse_Dec lt attL 
		(parse_typed_Dec dc_rep ps sg) 
-}


show_error s = "Bad Object"





own_win = x_display 



scratch_win obj_type s = x_scratch ("\n\n" ++ obj_type ++ "\n\n" ++ s)



split_line = split '\n'




show_comment tr@(TreeSt t _ _) 
	= show_com t                /./
	  (\ _ -> reTurn tr )



edit_comment tr @ (TreeSt t tl gst) 
	= x_form True [InComment "Edit Comment", InMultiText "Comment:" com] /./
	  exp
	  where
	  exp (SOME [OutText com']) 
		= reTurn ( TreeSt (set_comment com' t) tl gst )
	  exp _ = reTurn tr 

          com = get_comment t



show_arguments tr@(TreeSt t _ _) 
	= show_args t      ./.
	  reTurn tr 



show_args t@(Tree (Goal (SOME tac) op_args _ uid _ _ _ _) _ _ _ _) 
	= x_display title concat_args
	  where
	  concat_args = case op_args of
		             NONE      -> "NONE"
		 	     SOME args -> foldr ((++).(\s->s++ "\n\n")) "" args
	  title = "Node: " ++ uid ++ "\nTactic: " ++ tac ++ "\nArguments:"

show_args t@(Tree (Goal _ _ _ _ _ _ _ _) _ _ _ _) 
	= x_set_status "No tactic applied!"


show_tactics t = x_show_tactics ./. 
		 reTurn t 
