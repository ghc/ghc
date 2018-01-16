module Goals where

import Unparse

import X_interface

import Edlib

import Core_datatype

import Vtslib

import Tree

import Globals

import Lookup -- stub

import Parse

import Kernel

import Sub_Core1

import Sub_Core2

import Sub_Core3

import Sub_Core4

import Type_defs

import Tags

{-
(******************************************************************************)
(*   A goal consists of: a tactic name, a list of arguments, a comment, a     *)
(*   unique identifier and a specification.                                   *)
(******************************************************************************)
-}

data GOAL = Goal ( Option String )   
	         ( Option [String] ) -- arguments if any  
	         ( Option String )   -- comment if any
	         String	             -- unique identifier 
	         Obj       	     -- spec (goal)	    
	         Bool                -- read only?      
	         Sgn		     -- its signature  
	         Lookup_table	     -- its lookup table




data Obj  = TrmSpec ITrm |
            ThmSpec ITrm |	
            SgnSpec ISgn |
            DecSpec IDec 




data Done = TrmDone Trm |
            ThmDone Thm |
            SgnDone Sgn |
            DecDone Dec





show_option :: (String -> a) -> (b -> String) -> (Option b) -> a

show_option xfn gfn NONE = xfn "None"

show_option xfn gfn (SOME x) = xfn (gfn x)



summary_depth :: Int

summary_depth = 4



is_done :: (Option a) -> String

is_done NONE = ""

is_done (SOME _) = "Done"




short :: Int -> String -> String

short = short' -- Check that they are the same



short' :: Int -> String -> String

short' _ [] = []

short' 0 _ = "..."

short' i ('\n' : l) = ' ' : remove_spaces (i-1) l

short' i (c : l) = c : short' (i-1) l





remove_spaces :: Int -> String -> String

remove_spaces i ( ' ' : l) = remove_spaces i l

remove_spaces i ( '\n' : l) = remove_spaces i l

remove_spaces i l = short' i l





depth_print_tm i l sg tm = short 50 (unparse_trm sg l tm) 

depth_print_sg i l sg isg = short 50 (unparse_sgn sg l isg)

depth_print_dc i l sg dc = short 50 (unparse_dec sg l dc)




{-
show_parent gst [] 
	= x_unset_parent 

show_parent gst ((_,t):_) 
	= x_set_parent done summary 
	  where
	  (done,summary) = show_summary (get_attributes gst) t
-}



show_summary attL (Tree (Goal _ _ _ _ (TrmSpec tm) _ sg _) _ dn _ _) 
	= (is_done dn, depth_print_tm summary_depth attL sg tm)

show_summary attL (Tree (Goal _ _ _ _ (ThmSpec tm) _ sg _) _ dn _ _) 
	= (is_done dn, depth_print_tm summary_depth attL sg tm)

show_summary attL (Tree (Goal _ _ _ _ (SgnSpec isg) _ sg _) _ dn _ _) 
	= (is_done dn, depth_print_sg summary_depth attL sg isg)

show_summary attL (Tree (Goal _ _ _ _ (DecSpec dc) _ sg _) _ dn _ _) 
	= (is_done dn, depth_print_dc summary_depth attL sg dc)






show_goal :: Global_state -> (TREE GOAL b c) -> Xio_fn

show_goal  gst t@(Tree (Goal tac args com uid obj rw _ lt) _ _ _ _) 
	= show_node uid obj				     ...
	  show_option x_set_tactic id tac                    ...
	  show_option x_set_argument head args		     ...
	  show_option x_set_comment (short 50) com           ...
	  x_set_rw (if rw then "Editable" else "Read Only")  ...
	  x_set_goal done summary 
	  where
	  (done,summary) = show_summary (get_attributes gst) t
--	  (done,summary) = show_summary []  t




show_node uid (TrmSpec tm)
        = x_set_node ("Trm Node: " ++ uid)

show_node uid (ThmSpec tm)
        = x_set_node ("Thm Node: " ++ uid)

show_node uid (DecSpec dc)
        = x_set_node ("Dec Node: " ++ uid)

show_node uid (SgnSpec sg)
        = x_set_node ("Sgn Node: " ++ uid)





show_subgoals gst goalL 
	= x_set_subgoals (map (show_summary (get_attributes gst)) goalL)



show_done NONE     = x_set_done "Incomplete"

show_done (SOME _) = x_set_done "Complete"





{- old initialize
initialize (TreeSt t@(Tree goal goalL dn _ _ ) spine gst ) 
	= show_parent gst spine   ...
	  show_done dn            ...
	  show_goal gst t         ...
	  show_subgoals gst goalL
-}

initialize dset thy styp spec NONE 
        = get_tree dset thy styp spec NONE /./
	  exp
	  where
          exp ( SOME (tr@(TreeSt t@(Tree goal goalL dn _ _) spine gst)))
		= show_goal gst t          ...
                  show_subgoals gst goalL  ./.
                  reTurn tr 
          exp NONE 
		= end_x 	./.
		  return_err "no state" 


get_tree :: (Option String) -> (Option String) -> (Option String) ->
		(Option String) -> (Option String) -> Xin -> 
	 	 Xst ( MayBe (Option (Tree_state GOAL b Global_state )) String )

get_tree dset thy styp spec errmesg
	= x_form True (input_form dset thy styp spec errmesg) /./
	  exp
	  where
	  exp NONE = reTurn NONE 

          exp ( SOME [OutText ds, OutText ty, OutText sp, OutRadio st] )
		= parse_fn /.>/
		  genuid   /./
		  exp2 
		  where
                  parse_fn = case st of
                                   "Thm" -> mkspec ThmSpec parse_trm sp
                                   "Trm" -> mkspec TrmSpec parse_trm sp
--                                   "Dec" -> mkspec DecSpec parse_dec sp
--                                   "Sgn" -> mkspec SgnSpec parse_sgn sp
--                                   _     -> return_err "Bad Type" 
		  mkspec ctr p_fn sp
			= case p_fn sg ps sp of
				Ok res   -> reTurn ( ctr res )
				Bad mesg -> return_err mesg 
			  where
			  ps = default_tag_tbl

		  exp2 ( obj , uid )
			= reTurn ( SOME tst )
			  where
                          tst = TreeSt tr [] gst
                  	  tr  = Tree gl [] NONE ( \ x y -> y ) NONE
                  	  gl  = Goal NONE NONE NONE uid obj True sg lt
                  	  lt = [] --create_lookup_table isg
                  	  at = [] --defaults
                  	  gst = ( 0 , [] , default_tag_tbl ) 

	  exp _ = error "unimplemented in get_tree"
          sg  = empty --erestore_Sgn (ds,ty)
	  isg = internal_Sgn sg

{-
                    handle
                        Syntax (e,inp) =>
                            let val err = unerr e
                                val where = under_line_input inp
                            in get_tree (SOME ds) (SOME ty) (SOME st) (SOME sp)
                                        (SOME (err  ^ "\n" ^ where)) xio
                            end
                      | Expect (t1,t2,inp) =>
                            let val err = eexperr t1 t2
                                val where = under_line_input inp
                            in get_tree (SOME ds) (SOME ty) (SOME st) (SOME sp)
                                        (SOME (err  ^ "\n" ^ where)) xio
                            end
                      | Fail s =>
                            let val err = "Error: "^ s
                            in get_tree (SOME ds) (SOME ty) (SOME st) (SOME sp)
                                        (SOME err) xio
                            end
                      | _ =>

                           let val err = "Error in specification"
                            in get_tree (SOME ds) (SOME ty) (SOME st) (SOME sp)
                                        (SOME err) xio
                            end
                    )
-}


input_form dset thy styp spec errmesg 
	= [ InComment "XVTSEDIT VERSION 1.1",
            InSingleText "Dataset" ( case dset of 
				  	SOME ds -> ds 
				  	_       -> default_ds ),
            InSingleText "Theory " ( case thy  of 
				  	SOME ty -> ty 
				  	_       -> default_ty ),
            InMultiText  "Spec   " ( case spec of 
				  	SOME sp -> sp 
				  	_       -> "" ),
            InRadio "Type   " 
		    ( case styp of 
		  	SOME st -> my_index st types 
		  	_       -> 0 )
                    types ] ++
            (case errmesg of
                  SOME err -> [ InComment err ]
                  NONE     -> [] )


types = ["Thm","Trm","Sgn","Dec"]



my_index :: Eq b => b -> [b] -> Int

my_index s (s' : l) = if s == s' then 0 else 1 + my_index s l

my_index s []        = 0


default_ty = "empty"

default_ds = "default_ds not passed in yet"






{-
(******************************************************************************)
(*   When sorted out equalities, should check differences and only update     *)
(*   those fields which differ                                                *)
(******************************************************************************)
-}

update_state (TreeSt   ( Tree goal1 goalL1 dn1 _ _ ) spine1 gst1 )
       (TreeSt t@( Tree goal2 goalL2 dn2 _ _ ) spine2 gst2 ) 
	= ((show_goal gst2 t)          ...
	  (show_subgoals gst2 goalL2))



my_quit current 
	= x_form True form /./ exp
	  where
	  exp NONE       = reTurn False 
	  exp ( SOME _ ) = reTurn True 
	  form = [InComment "\n\nSelect ok to Quit\n\n"]



finish = end_x ./. reTurn (error "Finish state evaluated") 

