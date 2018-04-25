module Getops where

import Core_datatype

import Vtslib

import Type_defs

import Edlib



{-
(******************************************************************************)
(*   Syntax of flags:                                                         *)
(*                                                                            *)
(*   flags  ::= '--'							      *)
(*		'-' option [ flags]                                           *)
(*   option ::= <flag_char>+                                                  *)
(*              <arg_char> <arg>                                              *)
(******************************************************************************)
-}


data ArgType = Flag | Arg 




getops :: [Char] -> [[Char]] -> MayBe ([(Char, Option [Char])], [[Char]]) [Char]

getops template argL 
	= Ok ( [ ('t',SOME "Trm")], ["hello"])
--	= process_flags (gen_pattern template) argL



process_flags :: [(Char, ArgType)] -> [[Char]] 
			    -> MayBe ([(Char, Option [Char])], [[Char]]) [Char]

process_flags pattern [] = Ok ([], [])

process_flags pattern ("--" : argL) 
	= Ok ([], argL)

{-
process_flags pattern (arg : argL) 
	| is_option arg
	    = process_options pattern True (tail arg) argL |>|
	      process_flags pattern                        |@|
	      ( \ ( opts' , opts ) argL' -> Ok (opts' ++ opts, argL'))  
	| otherwise 
	    = Ok ([], arg : argL)
-}





process_options :: [(Char, ArgType)] -> Bool -> [Char] -> [a] 
				      -> MayBe ([(Char, Option a)], [a]) [Char]

process_options pattern allow_arg (opt : optL) argL 
	= case (assoc opt pattern, (allow_arg, optL, argL)) of
	      (NONE,_)  -> Bad ( "Bad option: " <: opt )
	      (SOME Flag,_) 
	                -> process_options pattern False optL argL        |@|
			   ( \ opts argL' -> Ok ((opt,NONE):opts, argL') )
			   
	      (SOME Arg, (True,[],arg:argL)) 
			-> Ok ([(opt,SOME arg)], argL)
	      (SOME Arg, (True,[],[])) 
			-> Bad (opt : " requires an argument")
	      (SOME Arg, _) 
			-> Bad ("Option syntax error")

process_options pattern allow_arg "" argL = Ok ([],argL)





is_option :: [Char] -> Bool

is_option ( '-' : _ ) = True 

is_option _ = False
	    




gen_pattern :: [Char] -> [(Char, ArgType)]

gen_pattern [] = []

gen_pattern (opt : ':' : optL) 
	= (opt,Arg) : gen_pattern optL

gen_pattern (opt : optL) 
	= (opt,Flag) : gen_pattern optL





{-
assoc _ [] = NONE 

assoc x ((y,z):l) 
	| x==y = SOME z 
	| x/=y = assoc x l
-}


