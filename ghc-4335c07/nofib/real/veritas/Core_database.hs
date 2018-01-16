{-
 * Module for converting vts90 objects into strings and vica-versa
 -}

module Core_database where

import Dcore

import Sub_Core1

import Sub_Core2

import Sub_Core3

import Sub_Core4

import Vtslib

import Core_datatype

data Constant_tag 
	= BOOL_TAG | TRUE_TAG | FALSE_TAG | UNIV_TAG deriving (Eq)

data Declaration_tag 
	= SYMBOL_DEC_TAG | AXIOM_DEC_TAG | DEF_TAG | DATA_TAG | 
	  DECPAIR_TAG deriving (Eq)

data Term_tag 
	= SYM_TAG | APP_TAG | PAIR_TAG | BINDER_TAG | 
	  CONSTANT_TAG | UNARY_TAG | BINARY_TAG | COND_TAG | 
	  CONST_TAG | RECURSE_TAG deriving (Eq)

data Signature_tag 
	= EMPTY_TAG | EXTEND_TAG | COMBINE_TAG | SHARE_TAG deriving (Eq)

trm_tag_list 
	= [SYM_TAG, APP_TAG, PAIR_TAG, BINDER_TAG,
	   CONSTANT_TAG, UNARY_TAG, BINARY_TAG, COND_TAG,
	   CONST_TAG, RECURSE_TAG]
 
dec_tag_list 
	= [SYMBOL_DEC_TAG, AXIOM_DEC_TAG, DEF_TAG, DATA_TAG, DECPAIR_TAG]

sgn_tag_list 
	= [EMPTY_TAG,EXTEND_TAG,COMBINE_TAG,SHARE_TAG]

con_tag_list 
	= [BOOL_TAG, TRUE_TAG, FALSE_TAG, UNIV_TAG]

unary_tag_list = [Not]

binary_tag_list 
	= [Eq',And,Or,Issubtype]
     
binder_tag_list 
	= [Forall,Exists,Imp,Lambda,Pi,Sigma,Subtype,Choose]



{-
    (* Functions for encoding tags *)

    (* Each function is of type    *)
    (* 		???_tag -> string  *)
-}


encode_trm_tag tag = toEnum (encode tag trm_tag_list)
    
encode_dec_tag tag = toEnum (encode tag dec_tag_list)

encode_sgn_tag tag = toEnum (encode tag sgn_tag_list)

encode_unary_tag tag = toEnum (encode tag unary_tag_list)

encode_binary_tag tag = toEnum (encode tag binary_tag_list)

encode_binder_tag tag = toEnum (encode tag binder_tag_list)

encode_con_tag tag = toEnum (encode tag con_tag_list)






{-
    (* Functions for decoding strings *)

    (* Each function is of type    *)
    (* 		string -> ???_tag  *)
-}

decode_trm_tag i = trm_tag_list !! i

decode_dec_tag i = dec_tag_list !! i

decode_sgn_tag i = sgn_tag_list !! i

decode_unary_tag i = unary_tag_list !! i

decode_binary_tag i = binary_tag_list !! i

decode_binder_tag i = binder_tag_list !! i

decode_constant_tag i = con_tag_list !! i
  




{- turn a term into a string -}

trm_to_str tm 
	= s1 ++ s2 ++ s3 
	  where
	  s1 = trm_to_str' tm				-- the term        
	  s2 = trmL_to_str (other_typ tm)		-- its extra types
	  s3 = attL_to_str (get_trm_att tm [])	        -- its attributes






{- turn a declaration into a string -}

dec_to_str dc 
	= s1 ++ s2 
	  where
	  s1 = dec_to_str' dc				-- the declaration 
	  s2 = attL_to_str (get_dec_att dc)		-- its attributes 





{- turn a signature into a string -}

sgn_to_str sg 
	= s1 ++ s2 
	  where
	  s1 = sgn_to_str' sg				-- the signature  
	  s2 = attL_to_str (get_sgn_att sg)		-- its attributes  



	 

{- turn a term into a string ignoring its extra types and its attributes -}

trm_to_str' (Sym i j _ _) 
	= encode_trm_tag SYM_TAG : int_to_str i ++ int_to_str j

trm_to_str' (App tm1 tm2 _ _) 
	= encode_trm_tag APP_TAG : trm_to_str tm1 ++ trm_to_str tm2

trm_to_str' (Pair tm1 tm2 tm3 _ _) 
	= encode_trm_tag PAIR_TAG : trm_to_str tm1 ++ trm_to_str tm2

trm_to_str' (Binder b dc tm _ _) 
	= encode_trm_tag BINDER_TAG : [ encode_binder_tag b ] ++
	     dec_to_str dc ++ trm_to_str tm

trm_to_str' (Constant c _ _) 
	= encode_trm_tag CONSTANT_TAG : constant_to_str c

trm_to_str' (Unary c tm _ _) 
	= encode_trm_tag UNARY_TAG : [encode_unary_tag c] ++ trm_to_str tm

trm_to_str' (Binary' c tm1 tm2 _ _) 
	= encode_trm_tag BINARY_TAG : [encode_binary_tag c] ++ 
		trm_to_str tm1 ++ trm_to_str tm2

trm_to_str' (Cond dc tm1 tm2 _ _) 
	= encode_trm_tag COND_TAG : dec_to_str dc ++ 
		trm_to_str tm1 ++ trm_to_str tm2

trm_to_str' (Const i j k _ _) 
	= encode_trm_tag CONST_TAG : int_to_str i ++ 
		int_to_str j ++ int_to_str k

trm_to_str' (Recurse tmL tm _ _) 
	= encode_trm_tag RECURSE_TAG : trmL_to_str tmL ++ trm_to_str tm





{- turn a declaration into a string ignoring its attributes -}

dec_to_str' (Symbol_dec tm _) 
	= encode_dec_tag SYMBOL_DEC_TAG : trm_to_str tm

dec_to_str' (Axiom_dec tm _) 
	= encode_dec_tag AXIOM_DEC_TAG : trm_to_str tm

dec_to_str' (Def tm1 tm2 _) 
	= encode_dec_tag DEF_TAG : trm_to_str tm1 ++ trm_to_str tm2

dec_to_str' (Data dcL tmLL _) 
	= encode_dec_tag DATA_TAG : decL_to_str dcL ++ trmLL_to_str tmLL

dec_to_str' (Decpair dc1 dc2 _) 
	= encode_dec_tag DECPAIR_TAG : dec_to_str dc1 ++ dec_to_str dc2





{- turn a signature into a string ignoring its attributes -}

sgn_to_str' (Empty _) 
	= [ encode_sgn_tag EMPTY_TAG ]

sgn_to_str' (Extend dc sg _) 
	= encode_sgn_tag EXTEND_TAG : dec_to_str dc ++ sgn_to_str sg

sgn_to_str' (Combine sg1 sg2 i _ _) 
	= encode_sgn_tag COMBINE_TAG : sgn_to_str sg1 ++ 
		sgn_to_str sg2 ++ int_to_str i

sgn_to_str' (Share sg i j k _ _) 
	= encode_sgn_tag SHARE_TAG : sgn_to_str sg ++ 
		int_to_str i ++ int_to_str j ++ int_to_str k





trmL_to_str tmL 
	= int_to_str (length tmL) ++ concat (map trm_to_str tmL)

trmLL_to_str tmLL 
	= int_to_str (length tmLL) ++ concat (map trmL_to_str tmLL)

trmLLL_to_str tmLLL 
	= int_to_str (length tmLLL) ++ concat (map trmLL_to_str tmLLL)





decL_to_str dcL 
	= int_to_str (length dcL) ++ concat (map dec_to_str dcL)



attL_to_str attL 
	= int_to_str (length attL) ++ concat (map att_to_str attL)

{- TEMP FUNCTION -}

att_to_str _ = "*** att_to_str NOT IMPLEMENTED -- core_database ***"



constant_to_str T 	 = [ encode_con_tag TRUE_TAG ]

constant_to_str F 	 = [ encode_con_tag FALSE_TAG ]

constant_to_str Bool' 	 = [ encode_con_tag BOOL_TAG ]

constant_to_str (Univ i) = encode_con_tag UNIV_TAG : int_to_str i






str_to_trm s 
	= (foldr add_type (set_trm_att tm [] attL) tmL , s3) 
	  where
    	  (tm,s1)   = s_to_trm' s
	  (tmL,s2)  = s_to_trmL s1
	  (attL,s3) = str_to_attL s2


str_to_dec s 
	= (set_dec_att dc attL,s2) 
	  where
    	  (dc,s1)   = s_to_dec' s
	  (attL,s2) = str_to_attL s1


str_to_sgn s 
	= (set_sgn_att sg attL,s2)
	  where
    	  (sg,s1)   = s_to_sgn' s
	  (attL,s2) = str_to_attL s1
		

s_to_trm' (ch:s1)
	= case decode_trm_tag ch of
	      SYM_TAG 
		  -> (Sym i j [] [] ,s3) 
		     where
		     (i,s2) = str_to_int s1
		     (j,s3) = str_to_int s2
    	      APP_TAG 
		  -> (App tm1 tm2 [] [] ,s3) 
		     where
		     (tm1,s2) = str_to_trm s1
		     (tm2,s3) = str_to_trm s2
    	      PAIR_TAG 
		  -> (Pair tm1 tm2 tm3 [] [] ,s4) 
		     where
		     (tm1,s2) = str_to_trm s1
		     (tm2,s3) = str_to_trm s2
		     (tm3,s4) = str_to_trm s3
    	      BINDER_TAG 
		  -> (Binder b dc tm [] [] ,s4)
		     where
		     (b,s2)  = str_to_binder s1
		     (dc,s3) = str_to_dec s2
		     (tm,s4) = str_to_trm s3
    	      CONSTANT_TAG 
		  -> (Constant c [] [] ,s2)
		     where
		     (c,s2) = str_to_constant s1
    	      UNARY_TAG 
		  -> (Unary c tm [] [] ,s3) 
		     where
		     (c,s2)  = str_to_unary s1
		     (tm,s3) = str_to_trm s2
    	      BINARY_TAG 
		  -> (Binary' c tm1 tm2 [] [] ,s4) 
		     where	
		     (c,s2)   = str_to_binary s1
		     (tm1,s3) = str_to_trm s2
		     (tm2,s4) = str_to_trm s3
    	      COND_TAG 
		  -> (Cond dc tm1 tm2 [] [] ,s4) 
		     where
		     (dc,s2)  = str_to_dec s1
		     (tm1,s3) = str_to_trm s2
		     (tm2,s4) = str_to_trm s3
    	      CONST_TAG 
		  -> (Const i j k [] [] ,s4) 
		     where
		     (i,s2) = str_to_int s1
		     (j,s3) = str_to_int s2
		     (k,s4) = str_to_int s3
	      RECURSE_TAG 
		  -> (Recurse tmL tm [] [] ,s3) 
		     where
		     (tmL,s2) = s_to_trmL s1
		     (tm,s3)  = str_to_trm s2





s_to_dec' (ch:s1) 
	= case decode_dec_tag ch of
	      SYMBOL_DEC_TAG 
		  -> (Symbol_dec tm [] ,s2)
		     where
		     (tm,s2) = str_to_trm s1
    	      AXIOM_DEC_TAG 
		  -> (Axiom_dec tm [] ,s2)
		     where
		     (tm,s2) = str_to_trm s1
	      DEF_TAG 
		  -> (Def tm1 tm2 [] ,s3) 
		     where
		     (tm1,s2) = str_to_trm s1
		     (tm2,s3) = str_to_trm s2
	      DATA_TAG 
		  -> (Data dcL tmLL [] ,s3) 
		     where
		     (dcL,s2) = s_to_decL s1
		     (tmLL,s3) = s_to_trmLL s2
	      DECPAIR_TAG 
		  -> (Decpair dc1 dc2 [] ,s3)
		     where
		     (dc1,s2) = str_to_dec s1
		     (dc2,s3) = str_to_dec s2





s_to_sgn' (ch:s1) 
	= case decode_sgn_tag ch of
	      EMPTY_TAG 
		  -> (Empty [], s1)
	      EXTEND_TAG 
		  -> (Extend dc sg [] ,s3) 
		     where
		     (dc,s2) = str_to_dec s1 
		     (sg,s3) = str_to_sgn s2
	      COMBINE_TAG 
		  -> (Combine sg1 sg2 i (sm2 ++ map (\ j -> j + i) sm1) [] ,s4)
		     where
		     (sg1,s2) = str_to_sgn s1
		     (sg2,s3) = str_to_sgn s2
		     (i,s4)   = str_to_int s3
		     sm1 = get_share_map sg1
		     sm2 = get_share_map sg2
	      SHARE_TAG 
		  -> (Share sg i j k (addequivL i j k sm) [] ,s5)
		     where
		     (sg,s2) = str_to_sgn s1 
		     (i,s3) = str_to_int s2
		     (j,s4) = str_to_int s3
		     (k,s5) = str_to_int s4
		     sm = get_share_map sg






s_to_trmL s = str_to_list str_to_trm s


s_to_trmLL s = str_to_list s_to_trmL s


s_to_trmLLL s =	str_to_list s_to_trmLL s


s_to_decL s = str_to_list str_to_dec s


str_to_attL s = str_to_list str_to_att s


str_to_binder (ch:s1) 
	= (decode_binder_tag ch,s1)


str_to_unary (ch:s1) 
	= (decode_unary_tag ch,s1)


str_to_binary :: [Int] -> (Binary_conn, [Int])

str_to_binary (ch:s1) 
	= (decode_binary_tag ch,s1) 


str_to_constant (ch:s1) 
	= case decode_constant_tag ch of
	      BOOL_TAG  -> (Bool',s1)
	      TRUE_TAG  -> (T,s1)
	      FALSE_TAG -> (F,s1)
	      UNIV_TAG  -> (Univ i,s2) 
			   where
			   (i,s2) = str_to_int s1 

{-
temp function
-}

str_to_att ( a : x)  = ( ( Name_Style , Symbol_Name ( Name [ toEnum a ]) ) , x )  
