module X_interface where

import Edlib

import Type_defs

import Vtslib

import Core_datatype

{-
(******************************************************************************)
(*   This is the definition of the X-interface.                               *)
(******************************************************************************)
-}


data SubTermData = STData [Int] Int Int [SubTermData] 

data Form_input  = InSingleText String String              |
		   InMultiText	String String		   |
		   InToggle 	String [( String , Bool )] |
		   InRadio 	String Int [String]        |
		   InComment	String		           |
		   InSubterm	String SubTermData    

data Form_output = OutText	String   |
		   OutToggle	[String] |
		   OutRadio	String   |
		   OutSubterm   String


--x_program = "/usr/nfs/hilbert/data/elr/nd1/X/src/newed/fe"

{-
(******************************************************************************)
(*   Define the escape sequences that control the interaction between SML     *)
(*   and X                                                                    *)
(******************************************************************************)
-}

esc_seq			= '\27'
x_quit_esc_seq		= "\27\&0"
x_enable_esc_seq	= "\27\&1"
x_cmd_esc_seq		= "\27\&2"
x_display_esc_seq	= "\27\&3"
x_inter_arg_esc_seq	= "\27\&4"
x_end_arg_esc_seq	= "\27\&5"
x_error_esc_seq		= "\27\&6"
x_scratch_esc_seq	= "\27\&7"
x_form_esc_seq		= "\27\&8"
x_cancel_esc_seq	= "\27\&9"
x_single_text_esc_seq   = "\27\&a"
x_multi_text_esc_seq    = "\27\&b"
x_toggle_esc_seq	= "\27\&c"
x_radio_esc_seq		= "\27\&d"
x_comment_esc_seq	= "\27\&e"
x_multi_arg_esc_seq	= "\27\&A"
x_set_par_esc_seq	= "\27\&B"
x_set_nod_esc_seq	= "\27\&C"
x_set_rw_esc_seq	= "\27\&D"
x_set_don_esc_seq	= "\27\&E"
x_set_dis_esc_seq	= "\27\&F"
x_set_tac_esc_seq	= "\27\&G"
x_set_arg_esc_seq	= "\27\&H"
x_set_com_esc_seq	= "\27\&I"
x_set_sub_esc_seq	= "\27\&J"
x_set_sta_esc_seq	= "\27\&K"
x_unset_par_esc_seq	= "\27\&L"
x_bell_esc_seq		= "\27\&M"
x_tacs_up_esc_seq	= "\27\&N"
x_subtrm_esc_seq	= "\27\&O"
x_set_full_esc_seq	= "\27\&P"
x_unset_full_esc_seq	= "\27\&Q"
x_hide_main_esc_seq     = "\27\&R"
x_show_main_esc_seq     = "\27\&T"


-- GRIP x_send
        
x_send s ins
	= (ins, s )
	  
        
	  





x_send_argL argL 
	= x_send x_multi_arg_esc_seq      ...
	  x_send [toEnum (length argL)]      ...
	  app ( map x_send_arg argL )     ...
	  x_send x_end_arg_esc_seq



x_send_arg arg 
	= x_send arg		        ...
	  x_send x_inter_arg_esc_seq


x_multi_send x_send_fn argL 
	= x_send x_multi_arg_esc_seq	               ...
	  x_send [toEnum (length argL)]                   ...
	  app (map (x_multi_send_arg x_send_fn) argL)  ...
	  x_send x_end_arg_esc_seq



x_multi_send_arg x_send_fn arg 
	= x_send_fn arg			...
	  x_send x_inter_arg_esc_seq



x_get_argL 
	= x_get_arg_no     /./ 
	  x_get_i_args     /.>/ 
	  x_get_esc_seq    /./
	  (\ ( argL, esc ) 
		-> if esc /= x_end_arg_esc_seq
		          then return_err mesg 
		          else reTurn argL  )
	  where
	  mesg = "Bad argument terminator " 





x_get_i_args :: Int -> Xin -> Xst ( MayBe [String] String )

x_get_i_args 0 = reTurn [] 

x_get_i_args i = x_get_arg          /.:>/
		 x_get_i_args (i-1) 




x_get_arg :: Xin -> Xst ( MayBe String String )

x_get_arg ( c : ist , rsps , glno )
	| c == esc_seq = if [ c , c' ] == x_inter_arg_esc_seq 
			      then reTurn "" ( ist2, rsps , glno )
	                      else return_err mesg ( ist2 , rsps , glno )
	| otherwise    = x_get_arg ( ist, rsps , glno )             /// 
	                 (\ str -> reTurn ( c : str ))
			 where
	                 mesg = "Bad argument escape" 
			 ( c' : ist2 ) = ist

x_get_arg ( [] , _ , _ ) = error "stdin empty"
		


x_get_arg_no 
	= x_get_esc_seq  /./ exp
	  where
	  exp esc_seq xin
		| esc_seq == x_multi_arg_esc_seq
			 = reTurn ( fromEnum c ) ( ist , rsps , glno )
		| otherwise
		         = return_err "Expected multiple arguments" xin 
	  		   where
	  		   ( c : ist , rsps , glno ) 
				   = case xin of
					( [] , _ , _ ) -> error "xin empty"
					otherwise      -> xin



x_multi_get_arg x_get_fn 
	= x_get_arg_no                 /./
	  x_multi_get_i_args x_get_fn  /.>/ 
	  x_get_esc_seq                /./ 
	  (\ ( argL, esc ) 
		-> if esc /= x_end_arg_esc_seq
			    then return_err mesg 
		            else reTurn argL )
	  where
	  mesg = "Bad argument terminator " 




x_multi_get_i_args :: (Xin -> Xst ( MayBe b c )) -> Int -> Xin 
						-> Xst ( MayBe [b] c )

x_multi_get_i_args x_get_fn 0 = reTurn [] 

x_multi_get_i_args x_get_fn i 
	= x_get_fn 			     /.:>/
	  x_multi_get_i_args x_get_fn (i-1) 




x_get_esc_seq ( a : b : ist , rsps , glno )
	= reTurn [ a , b ] ( ist , rsps , glno )



end_x = x_send x_quit_esc_seq



x_enable_buttons 
	= x_send x_enable_esc_seq



x_get_cmd 
	= x_enable_buttons      ./.
	  x_get_esc_seq 	/.>/
	  x_get_argL        	/./ 
	  exp 
	  where
	  exp ( esc_seq, argL ) 
		| esc_seq /= x_cmd_esc_seq
		            = return_err "Expected command"
		| otherwise = reTurn ( head argL )




x_display title display xin
	= ( \ ( xin , _ , xout ) -> ( xin , xout ))
	  ( x_form False [InComment title, InMultiText "" display] xin )
{-
	= x_send x_display_esc_seq          ...
	  x_send_argL [title,display]
-}



x_error error_mesg 
	= x_send x_error_esc_seq        ...
	  x_send_argL [error_mesg]      ./.
	  x_get_esc_seq                 /./
	  exp 
	  where	
	  exp esc_seq 
	     | esc_seq /= x_cancel_esc_seq
		         = return_err "Expected cancel" 
	     | otherwise = reTurn ( error "" )



x_scratch str 
	= x_send x_scratch_esc_seq  ...
	  x_send_argL [str]



x_form answer infoL 
	= x_send x_form_esc_seq          ...
	  x_send_bool answer             ...
	  x_multi_send x_send_info infoL ./.
	  if answer then x_get_form 
	 	    else reTurn NONE 


	    
x_set_parent done summary 
	= x_send x_set_par_esc_seq   ...
	  x_send_argL [done,summary]



x_unset_parent = x_send x_unset_par_esc_seq



x_set_node node 
	= x_send x_set_nod_esc_seq ...
	  x_send_argL [node]



x_set_rw rw 
	= x_send x_set_rw_esc_seq ...
	  x_send_argL [rw]



x_set_done done 
	= x_send x_set_don_esc_seq ...
	  x_send_argL [done]



x_set_goal :: String -> String -> Xio_fn

x_set_goal done display  -- used to be x_set_display
	= x_send x_set_dis_esc_seq   ...
	  x_send_argL [done,display]



x_set_tactic tactic 
	= x_send x_set_tac_esc_seq ...
          x_send_argL [tactic]



x_set_argument arg 
	= x_send x_set_arg_esc_seq ...
	  x_send_argL [arg]



x_set_comment comment 
	= x_send x_set_com_esc_seq  ...
	  x_send_argL [comment]



x_set_subgoals done_summaryL 
	= x_send x_set_sub_esc_seq ...
	  x_send_argL ( foldr flatten [] done_summaryL )
	  where
	  flatten :: ( String , String ) -> [String]  -> [String]
	  flatten (done,summary) args = done : summary : args



x_set_status status 
	= x_send x_set_sta_esc_seq  ...
          x_send_argL [status]



x_bell = x_send x_bell_esc_seq



x_show_tactics = x_send x_tacs_up_esc_seq


x_show_window 
	= x_send x_show_main_esc_seq


x_hide_window 
	= x_send x_hide_main_esc_seq


bst True = "1"

bst False = "0"



x_send_bool True = x_send "1"

x_send_bool False = x_send "0"



x_send_info (InSingleText l s) 
	= x_send x_single_text_esc_seq ...
	  x_send_argL [l,s]

x_send_info (InMultiText l s) 
	= x_send x_multi_text_esc_seq   ...
          x_send_argL [l,s]

x_send_info (InToggle s sL) 
	= x_send x_toggle_esc_seq  ...
	  x_send_argL [s]          ...
	  x_send_argL ( foldr (\ (x,y) z -> x : bst y : z ) [] sL )

x_send_info (InRadio s i sL) 
	= x_send x_radio_esc_seq   ...
	  x_send_argL [s]          ...
	  x_send_num i		   ...
          x_send_argL sL

x_send_info (InComment s) 
	= x_send x_comment_esc_seq  ...
	  x_send_argL [s]

x_send_info (InSubterm s sdata) 
	= x_send x_subtrm_esc_seq  ...
	  x_send_argL [s]          ...
	  x_send_subtrm sdata


x_send_toggle (s,b) 
	= x_send_bool b  ...
          x_send_arg s



x_send_form [] 
	= x_send x_end_arg_esc_seq

x_send_form [info] 
	= x_send_info info         ...
	  x_send x_end_arg_esc_seq

x_send_form (info : infoL) 
	= x_send_info info            ...
	  x_send x_inter_arg_esc_seq  ...
          x_send_form infoL



x_get_form 
	= x_get_esc_seq /./
	  exp 
	  where
	  exp esc_seq 
	    | esc_seq == x_cancel_esc_seq 
		    = reTurn NONE 
	    | esc_seq == x_form_esc_seq
		    = x_multi_get_arg x_get_info           /./
		      (\ arg -> reTurn ( SOME arg ))
	    | otherwise 
		    = return_err " XError Expected form escape" 
		     


x_get_info 
	= x_get_esc_seq  /.>/
	  x_get_argL     /./ 
	  exp 
	  where
	  exp ( esc_seq, argL ) 
	    | esc_seq == x_single_text_esc_seq ||
	      esc_seq == x_multi_text_esc_seq 
		    = reTurn ( OutText ( head argL ))
	    | esc_seq == x_toggle_esc_seq 
		    = reTurn ( OutToggle argL )
	    | esc_seq == x_radio_esc_seq 
		    = reTurn ( OutRadio ( head argL ))
	    | esc_seq == x_subtrm_esc_seq
		    = reTurn ( OutSubterm ( head argL ))
	    | otherwise
		    = return_err " XError Bad form escape sequence" 



x_send_subtrm (STData iL i j stinfoL) 
	= x_multi_send x_send_num iL    	...
	  x_send_num i                  	...
	  x_send_num j				...
	  x_multi_send x_send_subtrm stinfoL



x_send_num i 
	= x_send [ toEnum ((i `div` 256) `mod` 256) , toEnum (i `mod` 256) ]


{-
x_get_num ( msc : lsc : ist , rsps )
	= x_send str ( ist , rsps ) ./.
	  reTurn "" 
	  where
	  str = show ( fromEnum msc * 256 + fromEnum lsc ) ++ "\n" 
-}



x_set_full :: [Int] -> String -> String -> Xio_fn

x_set_full (i:int) title display 
	= x_send x_set_full_esc_seq                    ...
          x_send_argL [show i, title, display]



x_unset_full = x_send x_unset_full_esc_seq
