module Lookup where

import Vtslib

import Core_datatype

{-
(******************************************************************************)
(*  memoised lookup table of signatures.                                      *)
(******************************************************************************)
-}

{-
(******************************************************************************)
(*   A lookup table is an list of association lists.                          *)
(*   Each element of an association list consists of                          *)
(*         string                  - The name of a symbol  (or constructor)   *)
(*         int * bool              - The j index of a symbol, or              *)
(*         int * int * int list    - The j,k indexes for of a constructor     *)
(*         (int * opr) option      - The precedence and type of operator      *)
(*   The boolean in the symbol part indicates wheather the symbol is          *)
(*   accessabe by a rec expression.                                           *)
(*   in                                                                       *)
(******************************************************************************)
-}

type Index = Sum (Int , Bool) ( Int , Int , [(Int , Int)] )

type Entry = [(String , Option (Index , (Int , String) ))] 

type Lookup_table = [Entry]

{-

    local

(******************************************************************************)
(*   Turn a name into a lookup table                                          *)
(******************************************************************************)

    fun  get_nms jf (Name s) =
	    [(s, (jf, NONE))]
      | get_nms jf (Operator (s,p,opr)) =
      	    [(s,(jf,SOME (p,opr)))]

(******************************************************************************)
(*   Turn a list of names and a list of list of terms (taken from a           *)
(*   datatype declaration) and turn them into a lookup table                  *)
(******************************************************************************)

    and get_nmsL jf k [] [] =
    	    []
      | get_nmsL jf k (nm :: nmL) (tmL :: tmLL) =
      	    get_nms (inr (jf,k,(length tmL, rec_pars 0 tmL))) nm @ 
	    get_nmsL jf (k+1) nmL tmLL
      | get_nmsL jf k _ _ =
	    []

    and rec_pars _ [] = []
      | rec_pars i (tm :: tmL) = 
	    (if is_00_sm tm then [i] else []) @ rec_pars (i+1) tmL

    and is_00_sm (Sym (0,0,_,_)) = true 
      | is_00_sm _ = false

    
(******************************************************************************)
(*   Return the lookup table for a declaration                                *)
(******************************************************************************)

    fun get_dec_nms rec_flag j (Symbol_dec (_,attL)) =
	    (case get_att Name_Style attL
	       of SOME (Symbol_Name nm) => (get_nms (inl (j,rec_flag)) nm, j)
	        | _ => ([], j))
      | get_dec_nms rec_flag j (Axiom_dec (_,attL)) =
            (case get_att Name_Style attL
	       of SOME (Symbol_Name nm) => (get_nms (inl (j,rec_flag)) nm, j)
	        | _ => ([], j))
      | get_dec_nms rec_flag j (Def (_,_,attL)) =
      	    (case get_att Name_Style attL
	       of SOME (Symbol_Name nm) => (get_nms (inl (j,rec_flag)) nm, j)
	        | _ => ([], j))
      | get_dec_nms rec_flag j (Data (_,tmLL,attL)) =
            (case get_att Name_Style attL
	       of SOME (Datatype_Name (nm::nmL)) => 
		      (get_nms (inr (j,0,(0,[]))) nm @
			       get_nmsL j 1 nmL tmLL, j)
	        | _ => ([], j))
      | get_dec_nms rec_flag j (Decpair (dc1,dc2,_)) =
      	    let val (nms1,j1) = get_dec_nms rec_flag (j+1) dc1
	    	val (nms2,j2) = get_dec_nms rec_flag (j1+1) dc2
	    in (nms1@nms2, j2) end
		
(******************************************************************************)
(*   Return the lookup table for a pass1 decaration                           *)
(******************************************************************************)

    fun get_pdec_nms rec_flag j (P1_Dec (nmL, _)) = get_p1_nmsL rec_flag j nmL
      | get_pdec_nms rec_flag j (P1_Def (P1_Lhs (_,nm,_),_)) =
	    get_p1_nmsL rec_flag j [nm]
      | get_pdec_nms rec_flag j (P1_Define (nm,_,_)) =
	    get_p1_nmsL rec_flag j [nm]
      | get_pdec_nms rec_flag j (P1_Data (P1_Lhs (_,nm,_), conL,_)) =
      	    (get_nms (inr (j,0,(0,[]))) nm @ get_con_nms j 1 conL, j)
      | get_pdec_nms rec_flag j (P1_Decp (dc1,dc2)) =
      	    let val (nms1,j1) = get_pdec_nms rec_flag (j+1) dc1
	    	val (nms2,j2) = get_pdec_nms rec_flag (j1+1) dc2
	    in (nms1@nms2, j2) end
      | get_pdec_nms rec_flag j (P1_BDec dc) =
	    get_p1bdc rec_flag j dc
      | get_pdec_nms rec_flag j (P1_Database_dec dc) =
            let val (dc1, _) = internal_Dec dc
            in  get_dec_nms rec_flag j dc1 end

(******************************************************************************)
(*   Return the lookup table for a pass1 binding declaration                  *)
(******************************************************************************)

    and get_p1bdc rec_flag j (P1_Bdec (nmL, _)) =
            get_p1_nmsL rec_flag j nmL
      | get_p1bdc rec_flag j (P1_Bdecp (dc1,dc2)) =
            let val (nms1, j1) = get_p1bdc rec_flag (j+1) dc1
                val (nms2, j2) = get_p1bdc rec_flag (j1+1) dc2
            in (nms1@nms2, j2) end

(******************************************************************************)
(*   Return the llokup table for a list of names, that representing           *)
(*   sequence of paired declarations                                          *)
(******************************************************************************)

    and get_p1_nmsL rec_flag j [nm] = (get_nms (inl (j,rec_flag)) nm, j)
      | get_p1_nmsL rec_flag j (nm :: nmL) =
            let val nms1 = get_nms (inl (j+1,rec_flag)) nm
                val (nms2,j1) = get_p1_nmsL rec_flag (j+2) nmL
            in (nms1@nms2, j1) end
      | get_p1_nmsL rec_flag j [] = ([], j)

(******************************************************************************)
(*   Return the lookup table for the constructors of a pass1 datatype         *)
(*   declaration                                                              *)
(******************************************************************************)

    and get_con_nms j k [] = []
      | get_con_nms j k (P1_Rhs (nm,tmL) :: nmL) =
            get_nms (inr (j,k,(length tmL,get_pvars 0 tmL))) nm @
	    get_con_nms j (k+1) nmL

    and get_pvars _ [] = []
      | get_pvars i (tm :: tmL) = 
	    (if is_00_psm tm then [i] else []) @ get_pvars (i+1) tmL

    and is_00_psm (P1_Sym (0,0,_)) = true
      | is_00_psm _ = false

    in (* local *)

(******************************************************************************)
(*   Take a signature and return its lookup table                             *)
(******************************************************************************)

    fun create_lookup_table (sg : sgn) : lookup_table =
	    case sg
	      of Empty _ =>
	             []
	       | Extend (dc,sg,_) =>
		     extend_lookup_table true dc (create_lookup_table sg)
	       | Combine (sg1,sg2,_,_,_) => 
		     let val tbl1 = create_lookup_table sg2
			 and tbl2 = create_lookup_table sg1
		     in [] :: tbl1 @ tbl2 end
	       | Share (sg,_,_,_,_,_) =>
		     [] :: create_lookup_table sg

(******************************************************************************)
(*   Extend a lookup table with a new declaration                             *)
(******************************************************************************)

    and extend_lookup_table rec_flag dc tbl : lookup_table =
	    let val (nms, _) = get_dec_nms rec_flag 0 dc
	    in nms :: tbl end

    fun combine_lookup_table tbl1 tbl2 : lookup_table =
	    tbl2 @ [[]] @ tbl1

    fun share_lookup_table tbl (i : int) (j : int)  : lookup_table =
	    [] :: tbl

    fun create_tbl (sg : pass1_sgn) : lookup_table =
	    case sg
	      of P1_Empty =>
	             []
	       | P1_Signature (dc) =>
		     extend_tbl true dc []
	       | P1_Extend (dc,sg) =>
		     extend_tbl true dc (create_tbl sg)
	       | P1_Combine (sg1,sg2) => 
		     let val tbl1 = create_tbl sg2
			 and tbl2 = create_tbl sg1
		     in [] :: tbl1 @ tbl2 end
	       | P1_Sharing (sg,_) =>
		     [] :: create_tbl sg
	       | P1_Database_sgn sg =>
		     create_lookup_table (internal_Sgn sg)
	       | P1_Named_sgn (_,sg) =>
		    create_tbl sg
	       | P1_plus_plus (sg1,sg2) =>
		     let val tbl1 = create_tbl sg2
			 and tbl2 = create_tbl sg1
		     in [] :: tbl1 @ tbl2 end

    and extend_tbl rec_flag pdc tbl  : lookup_table =
	    let val (nms,_) = get_pdec_nms rec_flag 0 pdc
	    in nms :: tbl end

(******************************************************************************)
(*   Lookup a string in the lookup table and return the pass1 symbol (or      *)
(*   constructor) and its optional operator status                            *)
(******************************************************************************)

    fun lookup_name (tbl : lookup_table) (str : string) =
	    let fun lookup i [] = 
			NONE
		  | lookup i (entry :: tbl) =
			case assoc str entry
			  of SOME (inl (j,true),opr_op) => 
				SOME (P1_Sym (i,j,true), opr_op)
			   | SOME (inr (j,k,_),opr_op) => 
				SOME (P1_Const (i,j,k,true), opr_op)
			   | _ => lookup (i+1) tbl
	    in lookup 0 tbl end

    fun lookup_rec_name (tbl : lookup_table) (str : string) =
	    let fun lookup i [] = 
			NONE
		  | lookup i (entry :: tbl) =
			case assoc str entry
			  of SOME (inl (j, false),opr_op) => 
				SOME (P1_Sym (i,j,true), opr_op)
			   | _ => lookup (i+1) tbl
	    in lookup 0 tbl end

    exception BadIndex

    fun lookup_sm_index (tbl : lookup_table) (i,j) =
	    let val entry = tbl !! i
		fun is_sm (_,(inl (j',_),_)) = j = j'
		  | is_sm _ = false
	    in case filter is_sm entry
		 of ((s,(inl (_,f),NONE)) :: _) => SOME (s, f, NONE) 
		  | ( (s,(inl (_,f),SOME (i, opr))) :: _) => 
			SOME (s, f, SOME (i,opr))
		  | _ => NONE
	    end

    fun lookup_cn_index (tbl : lookup_table) (i,j,k) =
	    let val entry = tbl !! i
		fun is_sm (_,(inr (j',k',_),_)) = (j',k') = (j,k)
		  | is_sm _ = false
	    in case filter is_sm entry
		 of ( (s,(inr (_,_,_),SOME (i, opr))) :: _) => 
			SOME (s, SOME (i,opr))
		  | ((s,(inr (_,_,_),_)) :: _) => SOME (s, NONE)
		  | _ => NONE
	    end
    end (* of local *)

    fun rec_parameters (tbl : lookup_table) (i,j,k) =
	    let val entry = tbl !! i
		fun is_sm (_,(inr (j',k',_),_)) = (j',k') = (j,k)
		  | is_sm _ = false
	    in case filter is_sm entry
		 of ((_,(inr (_,_,iL),_)) :: _) => iL
		  | _ => raise BadIndex
	    end

    fun operators ([] : lookup_table) = []
      | operators (ent :: tbl) =
	    foldr (curry op@) (operators tbl) (map get_ops ent)

    and get_ops (id, (inr _, SOME (i, opr))) =
	    [(id, Operator (id,i,opr))]
      | get_ops (id, (inr _, NONE)) =
	    [(id, Name id)]
      | get_ops _ =
	    []
end
-}
