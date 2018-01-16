module Editor where

import Core_datatype
import Kernel
import Lookup

import Vtslib

import X_interface

import Goals
import Globals		-- partain
import Tags		-- partain

import Edlib

import Type_defs

import Tree




editor cmdL initial update quit_check finish 
	= initial NONE NONE NONE NONE NONE                      /./
	  ( \ st -> process_cmds cmdL update quit_check st st ) /.>/
	  finish 				                /./
	  (\ ( state' , _ ) -> reTurn state' )

process_cmds cmdL update quit_check prev state 
	= update prev state ./.
	  x_get_cmd         /./ 
	  exp	
	  `handle`
	  err_handler
	  where
	  exp "Quit" 
		= reTurn state
{-
		= quit_check state     /./
		  ( \ st -> if st then reTurn state  
		      		  else process_cmds cmdL update 
						        quit_check state state )
-}

	  exp cmd
	      = do_cmd cmdL state cmd 			   /./
	        process_cmds cmdL update quit_check state 
{-
	      = do_cmd cmdL state cmd 			   /.>/
		(x_show_tactics ./. reTurn (error ""))     /./
	        ( \ ( st , _ ) -> process_cmds cmdL update quit_check state st )
-}

          err_handler s -- XError
		= x_error s  /./       
		  ( \ _ -> process_cmds cmdL update quit_check prev state ) 
--		  clean_x


{-
	  err_handler _ 
		= process_cmds cmdL update quit_check prev state /./
		  x_set_status "Can't execute command"           --/./
--		  clean_x 
-}

{- Interrupt unimplemented

	    Interrupt => 
	        (
		 x_error xio "Command Interrupt" ;
		 process_cmds cmdL update quit_check xio prev state
		)

and catchTopCont () = (
          System.Unsafe.toplevelcont :=
            callcc (fn k => (
              callcc (fn k' => (throw k k'));
              raise Interrupt)))
-}



do_cmd cmdL state cmd 
	= case my_assocs cmd cmdL of
	       SOME cmd_fn -> cmd_fn state 
	       NONE        -> x_error ( "Bad command: " ++ cmd ) /./ 
	       		      ( \ _ -> reTurn state  )

my_assocs :: String -> [(String,b)] -> Option b

my_assocs key [] = NONE

my_assocs key ((key',entry):l')
        | key == key' = SOME entry
        | otherwise   = my_assocs key l'

