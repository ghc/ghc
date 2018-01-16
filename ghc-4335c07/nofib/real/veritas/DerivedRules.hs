module DerivedRules where

import Type_defs

import Kernel

import Core_datatype

import Build_Tm

import Edlib

import Parse

import Sub_Core1

import Sub_Core2

import Sub_Core3

import Sub_Core4

import Vtslib

conj_thm_trm 
	= Binder Forall (Symbol_dec (Constant Bool' [] []) [])
	  ( Binder Forall (Symbol_dec (Constant Bool' [] []) [])
	  ( Binder Imp (Symbol_dec (Sym 1 0 [] []) [])
	  ( Binder Imp (Symbol_dec (Sym 1 0 [] []) [])
	  ( Binary' And (Sym 3 0 [] []) (Sym 2 0 [] []) [] []) [] [])
	    [] []) [] []) [] []

disj_thm_trm 
	= Binder Forall (Symbol_dec (Constant Bool' [] []) [])
	  ( Binder Forall (Symbol_dec (Constant Bool' [] []) [])
	  ( Binder Forall (Symbol_dec (Constant Bool' [] []) [])
	  ( Binder Imp (Symbol_dec 
	    (Binary' Or (Sym 2 0 [] []) (Sym 1 0 [] []) [] []) [])
	    ( Binder Imp (Symbol_dec (Binder Imp (Symbol_dec (Sym 3 0 [] []) [])
	    ( Sym 2 0 [] []) [] []) [])
	    ( Binder Imp (Symbol_dec (Binder Imp (Symbol_dec (Sym 3 0 [] []) [])
	    (Sym 3 0 [] []) [] []) []) (Sym 3 0 [] []) [] [])
	    [] []) [] []) [] []) [] []) [] []


conj_thm = taut (trm_to_Trm empty conj_thm_trm)



disj_thm = taut (trm_to_Trm empty disj_thm_trm)


find_trms :: (Int -> ITrm -> Bool) -> (Int -> ITrm -> ITrm) -> ITrm 
							-> (ITrm, [[Int]])

find_trms p f tm 
	= (tm', map (\ (_,iL) -> iL) iLL) 
	  where
	  find n iL tm | p n tm    = [(n,iL)] 
		       | otherwise = find' n iL tm
	
	  find' n iL (App tm1 tm2 _ _) 
		= find n (iL <: 0) tm1 ++ 
		  find n (iL <: 1) tm2

	  find' n iL (Pair tm1 tm2 tm3 _ _) 
		= find n (iL <: 0) tm1 ++ 
		  find n (iL <: 1) tm2 ++
		  find n (iL <: 2) tm3

	  find' n iL (Binder q dc tm _ _) 
		= find_dec n (iL <: 0) dc ++
		  find (n+1) (iL <: 1) tm

	  find' n iL (Unary _ tm _ _) 
		= find n (iL <: 0) tm

	  find' n iL (Binary' _ tm1 tm2 _ _) 
		= find n (iL <: 0) tm1 ++
		  find n (iL <: 1) tm2

	  find' n iL (Cond dc tm1 tm2 _ _) 
		= find_dec n (iL <: 0) dc ++
		  find (n+1) (iL <: 1) tm1 ++
		  find (n+1) (iL <: 2) tm2 

	  find' n iL (Recurse tmL tm _ _) 
		= concat (map' (\ i tm -> find n (iL <: i) tm) tmL)
	
	  find' _ _ _ = []


          find_dec n iL (Symbol_dec tm _) 
		= find n (iL <: 0) tm

	  find_dec n iL (Axiom_dec tm _) 
		= find n (iL <: 0) tm

	  find_dec n _ _ = []



	  lift_fn tm (n,iL) 
		= replace_trm tm (f n subtm) iL 
		  where
		  (subtm,_) = select_trm tm iL

	  iLL = find 0 [] tm

	  tm' = foldl lift_fn  tm iLL


{-
(******************************************************************************)
(*                 » ù   » æ                                                  *)
(*                 --------- conj                                             *)
(*                  » ù ³ æ                                                   *)
(******************************************************************************)
-}

conj th1 th2 
	= th6 
	  where
	  sg = sgn_of_Thm th1
	  th3 = specialise (weaken sg conj_thm) (typ_of_Thm th1)
	  th4 = specialise th3 (typ_of_Thm th2)
	  th5 = modus_ponens th4 th1
	  th6 = modus_ponens th5 th2



disj th1 th2 th3 
	= th8 
	  where
	  sg = sgn_of_Thm th1
	  th3 = specialise (weaken sg disj_thm) (typ_of_Thm th1)
	  th4 = specialise th3 (typ_of_Thm th2)
	  th5 = specialise th4 (typ_of_Thm th3)
	  th6 = modus_ponens th5 th1
	  th7 = modus_ponens th6 th2
	  th8 = modus_ponens th7 th3



find_betas :: ITrm -> (ITrm, [[Int]])

find_betas 
	= find_trms is_beta_red do_beta_red 
	  where
	  is_beta_red _ (App (Binder Lambda _ _ _ _) _ _ _) = True
	  is_beta_red _ _ = False
	  do_beta_red _ (App (Binder Lambda dc tm1 _ _) tm2 _ _) 
		= subst_trm dc tm1 tm2

rep_beta :: Thm -> [[Int]] -> Thm
rep_beta = foldl beta_rw

