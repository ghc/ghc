
module Tags 
where

import Parse

import Build_Tm

import Unparse

import Build_itrm

import Token

import Type_defs

import Kernel

import Core_datatype

import Vtslib

type Tag_table = [ Tag ]

tgL = ( [ ("reflex" , [ Term_Arg ]            , [ Thm_Fn reflex_fn ] ) ,
	  ("\226"   , [ Deriv_Arg , Int_Arg ] , [ Thm_Fn beta_fn ] ) ,
	  ("\232"   , [ Deriv_Arg , Int_Arg ] , [ Thm_Fn eta_fn ] ) ,
	  ("symmetry", [ Deriv_Arg ]          , [ Thm_Fn symmetry_fn ] ) ,
	  ("\186"   , [ Term_Arg ]            , [ Thm_Fn taut_fn ] ) ,
	  ("if_true" , [ Deriv_Arg , Deriv_Arg , Int_Arg ] , [ Thm_Fn if_true_fn] )
	    ])

reflex_fn [ Tg_Trm tm ] = reflex tm

beta_fn [ Tg_Thm th , Tg_Int iL ] = beta_rw th iL

eta_fn [ Tg_Thm th , Tg_Int iL ] = eta_rw th iL

symmetry_fn [ Tg_Thm th ] = symmetry th

taut_fn [ Tg_Trm tm ] = taut tm

if_true_fn [ Tg_Thm th1 , Tg_Thm th2 , Tg_Int iL ] = cond_true_rw th1 th2 iL



default_tag_tbl = tgL

{-

(******************************************************************************)
(*   Define an (partially) uncurried version of the the function T.add_tg     *)
(******************************************************************************)

	fun build_enrty ( tg , args , trm_fn , Trm_fn , Thm_fn ) tbl =
		add_tag tg args trm_fn  Trm_fn Thm_fn tbl

	fun unimplemented tg _ = fail (tg ^ " is not implemented")

	fun const x y = x

	fun zero_trm_fn tm [] [] [] = tm
	  | zero_trm_fn _ _ _ _ = fail "Bad tag arguments"

	fun one_trm_fn f [tm] [] [] = f tm
	  | one_trm_fn _ _ _ _ = fail "Bad tag arguments"

	fun into_fn [tm1,tm2] [th] _ = into tm1 tm2 th
	and from_fn [tm] _ _ = from tm
	and taut_fn [tm] _ _ = taut tm
	and beta_fn _ [th] [iL] = beta_rw th iL
	and eta_fn _ [th] [iL] = eta_rw th iL
	and if_true_fn [] [th1,th2] [iL] = cond_true_rw th1 th2 iL
	and if_false_fn [] [th1,th2] [iL] = cond_false_rw th1 th2 iL
	and rw_fn [] [th1,th2] [iL] = subterm_rw th1 th2 iL
	(* and recurse_rw_fn  _ [th1,th2] [iL] = recurse_rw th1 th2 iL *)
	and inj_fn _ [th] _ = injection th
	and widen_fn [tm] [th] _ = widen tm th 
	and choose_fn _ [th] _ = choose th
	and exists_intro_fn [tm1,tm2] [th] _ = exists_intro th tm1 tm2
	and reflex_fn [tm] _ _ = reflex tm

	val default_entries =
		[
			( "into" ,
				[ Term_Arg , Term_Arg , Deriv_Arg ] ,
				unimplemented "into on trms" ,
				into_fn ,
				unimplemented "into on thms" ) ,
			( "from" , 
				[ Term_Arg ] ,
				unimplemented "from to trms" ,
				unimplemented "from to Trms" ,
				from_fn ) ,
			( "º" , 
				[ Term_Arg ] , 
				unimplemented "º to trms" ,
				unimplemented "º to Trms" ,
				taut_fn ) ,
			( "beta_rw" , 
				[ Deriv_Arg , Int_Arg ] ,
				unimplemented "beta_rw on trms" ,
				unimplemented "beta_rw on Trms" ,
				beta_fn ) ,
			( "eta_rw" , 
				[ Deriv_Arg , Int_Arg ] ,
				unimplemented "eta_rw on trms" ,
				unimplemented "eta_rw on Trms" ,
				eta_fn ) ,
			( "if_true_rw" , 
				[ Deriv_Arg , Int_Arg ] ,
				unimplemented "if_true_rw on trms" ,
				unimplemented "if_true_rw on Trms" ,
				if_true_fn ) ,
			( "if_false_rw" , 
				[ Deriv_Arg , Int_Arg ] ,
				unimplemented "if_false_rw on trms" ,
				unimplemented "if_false_rw on Trms" ,
				if_false_fn ) ,
		    (*
			( "recurse_rw" , 
				[ Deriv_Arg , Int_Arg ] ,
				unimplemented "recurse_rw on trms" ,
				unimplemented "recurse_rw on Trms" ,
				recurse_rw_fn ) ,
		    *)
			( "ò" , 
				[ Deriv_Arg , Int_Arg ] ,
				unimplemented "ò on trms" ,
				unimplemented "ò on Trms" ,
				rw_fn ) ,
			( "widen" , 
				[ Term_Arg , Deriv_Arg ] ,
				unimplemented "widen on trms" ,
				widen_fn ,
				unimplemented "widen on Thms" ) ,
			( "inj" , 
				[ Deriv_Arg ] ,
				unimplemented "inj on trms" ,
				unimplemented "inj on Trms" ,
				inj_fn ) ,
			( "choose" , 
				[ Deriv_Arg ] ,
				unimplemented "chose on trms" ,
				choose_fn ,
				unimplemented "choose on Thms" ) ,
			( "exists_intro" , 
				[ Deriv_Arg , Term_Arg , Term_Arg ] ,
				unimplemented "exists_intro on trms" ,
				unimplemented "exists_intro on Trms" ,
				exists_intro_fn ) ,
			( "reflex" , 
				[ Term_Arg ] ,
				unimplemented "reflex on trms" ,
				unimplemented "reflex on Trms" ,
				reflex_fn ) 
		]

	val default_tag_tbl = foldr build_enrty empty_tbl default_entries
	
-}
