
{-
 * Mon Nov  5 09:54:24 GMT 1990
 *
 * Implementation of untyped terms, signatures and declarations
 *
 * Each constructors last argument (of the tuple) is a list of
 * information attributes that the parser, unparsers, tactics etc use.
 *	
 * Each terms' next to last argument is a list of alternative types the the
 * term can have to its natutal type.
 *
-}

module Sub_Core2 where

import Vtslib

import Core_datatype
 
import Sub_Core1

mk_smsl :: [IDec] -> Int -> [ITrm] -> [ITrm]
mk_sms :: IDec -> Int -> Int -> (ITrm, Int)
mk_fnspace :: ITrm -> ITrm -> ITrm
mk_pi :: IDec -> ITrm -> ITrm
typ_of_dec :: IDec -> ITrm
is_def_dec :: IDec -> Bool
is_axm_dec :: IDec -> Bool
is_sym_dec :: IDec -> Bool
remake_ty :: ITrm -> [a] -> ITrm -> ITrm
extract_dc :: Int -> IDec -> IDec
uncurry_trm :: IDec -> Int -> ITrm -> ITrm
uncurry_cn :: [Int] -> Int -> Int -> Int -> Int -> Int -> [ITrm] -> [Attribute] -> ITrm
uncurry_sm :: [Int] -> Int -> Int -> Int -> Int -> [ITrm] -> [Attribute] -> ITrm
subst_dec :: IDec -> IDec -> ITrm -> IDec
subst_trm :: IDec -> ITrm -> ITrm -> ITrm
subst_cn :: a -> Int -> Int -> Int -> [ITrm] -> [Attribute] -> ITrm
--subst_sm :: [(Int, ITrm)] -> Int -> Int -> Int -> [ITrm] -> [Attribute] -> ITrm
create_table :: Int -> IDec -> ITrm -> (Int, [(Int , ITrm)])
shift_dec :: [Int] -> Int -> IDec -> IDec
shift_trm :: [Int] -> Int -> ITrm -> ITrm
share_trm :: [Int] -> ITrm -> ITrm
shift_cn :: [Int] -> Int -> Int -> Int -> Int -> Int -> [ITrm] -> [Attribute] -> ITrm
shift_sm :: [Int] -> Int -> Int -> Int -> Int -> [ITrm] -> [Attribute] -> ITrm
share_cn :: [Int] -> Int -> Int -> Int -> Int -> [ITrm] -> [Attribute] -> ITrm
share_sm :: [Int] -> Int -> Int -> Int -> [ITrm] -> [Attribute] -> ITrm
--canonical_i :: (Integral a) => [a] -> a -> a
get_share_map :: ISgn -> [Int]



{-
 return the share_map of a signature 				
 Note: share maps are memoised at every Share and Combine node	
-}

-- get_share_map :: ISgn -> Share_map

get_share_map sg 
	= shared sg 0 
          where
	  shared :: ISgn -> Int -> Share_map

          shared (Share _ _ _ _ sm _) j 
		= (list 0 j) ++ (map negate sm) -- ++ (map (\ i -> i + j) sm)

	  shared (Combine _ _ _ sm _) j 
		= (list 0 j) ++ (map negate sm) -- ++ (map (\ i -> i + j) sm)

          shared (Empty _) j 
		= list 0 j

	  shared (Extend _ sg _) j 
		= shared sg (j+1)


	  {- return the list [i,i+1,...,j-1] -}

	  list :: Int -> Int -> [Int]

    	  list i j = init [ i..j ] 

{-
	  list i j = if i >= j then [] 
			       else i : list (i+1) j
-}





{- take a share_map and and i-index and return the canonical i-index -}

canonical_i :: Share_map -> Int -> Int
 
canonical_i [] i = i

canonical_i shareL i = shareL !! i





share_sm shareL depth i j tyL att 
	= Sym (depth+canonical_i shareL (i-depth)) j tyL att


share_cn shareL depth i j k tyL att 
	= Const (depth+canonical_i shareL (i-depth)) j k tyL att


shift_sm shareL offset depth i j tmL att 
	= share_sm shareL depth (i+offset) j tmL att


shift_cn shareL offset depth i j k tyL att 
	= share_cn shareL depth (i+offset) j k tyL att





{-
  apply sharing to a term. ie replace all global symbols	
  that may be shared with the root symbol		
-}

-- share_trm :: Share_map -> ITrm -> ITrm

share_trm shareL tm 
	= f3 tm 
	  where
	  f1 = share_sm shareL 
	  f2 = share_cn shareL
	  (f3,_) = map_fn f1 f2





{- shift a term by an offset and then do any sharing that may be required -}	

shift_trm shareL offset tm 
	= f3 tm 
	  where
	  f1 = shift_sm shareL offset
	  f2 = shift_cn shareL offset
	  (f3,_) = map_fn f1 f2





{- shift a dec by an offset and then do any sharing that may be required -}

shift_dec shareL offset dc 
	= f3 dc 
	  where	
	  f1 = shift_sm shareL offset
	  f2 = shift_cn shareL offset
	  (_,f3) = map_fn f1 f2




{-
     create a assoc list of j indexes and terms from a declatation	
     and a term. The term should match the declaration pair for 
     decpair.								
-}

create_table j (Decpair dc1 dc2 _) (Pair tm1 tm2 _ _ _) 
	= (j2,al1 ++ al2)
	  where
	  (j1,al1) = create_table (j+1) dc1 tm1
	  (j2,al2) = create_table (j1+1) dc2 tm2

create_table j (Symbol_dec _ _) tm 
	= (j,[(j,tm)])

--create_table _ _ _ 
--	= error "SubstError" -- ** exn






subst_sm table n i j tyL att 
	| i==n = case assoc j table of
		  	SOME tm -> shift_trm [] n tm
--		  	NONE    -> error "System_Error" -- ** exn
	| i/=n = Sym (i-1) j tyL att




subst_cn n i j k tyL att 
	= Const (i-1) j k tyL att



{-
    * substitute a term for the symbols <0,j> in a term 
    * 		An empty share_map may given to the 
    *		shift_trm function since the shift is
    *		upwards so no new sharing can be done
-}

subst_trm dc tm1 tm2 
	= f3 tm1 
	  where
	  (_,table) = create_table 0 dc tm2
	  f1 = subst_sm table
	  f2 = subst_cn 
	  (f3,_) = map_fn f1 f2




subst_dec dc1 dc2 tm 
	= f3 dc2 
	  where
	  (_,table) = create_table 0 dc1 tm
	  f1 = subst_sm table
	  f2 = subst_cn 
	  (_,f3) = map_fn f1 f2




{-
    * build a list of all the indices down the spine of symbol dec 
    * with then in a declaration 				    
-}

ext_indices :: IDec -> Int -> [Int]

ext_indices dc j 
	= ext j dc 0 [] [] 
	  where
	  ext :: Int -> IDec -> Int -> [Int] -> [( IDec , [Int])] -> [Int]

	  ext 0 d j l _  = l

	  ext i (Decpair dc1 dc2 _) j l cl 
                = ext (i-1) dc1 (j+1) l ((dc2,j+1:l):cl)

	  ext i _ j _ ((dc,l):cl) = ext (i-1) dc (j+1) l cl



uncurry_sm iL dec_depth local_depth i j tmL attL 
	= if i - local_depth < dec_depth 
		then Sym local_depth (j + (iL !! i)) tmL attL
	        else Sym (i - dec_depth + 1) j tmL attL





uncurry_cn iL dec_depth local_depth i j k tmL attL 
	= if i - local_depth < dec_depth 
		then Const local_depth (j + (iL !! i)) k tmL attL
	        else Const (i - dec_depth + 1) j k tmL attL



{-
    (* move a term defined within a declaration to a term defined	*)
    (* on a signature extended by that declaration               	*)
-}

uncurry_trm dc j tm 
	= f3 tm 
	  where
	  iL = ext_indices dc j
	  f1 = uncurry_sm iL (length iL) 
	  f2 = uncurry_cn iL (length iL) 
	  (f3,_) = map_fn f1 f2





extract_dc i dc 
	= extract i dc [] 
	  where
	  extract :: Int -> IDec -> [IDec] -> IDec

	  extract 0 (dc @ (Symbol_dec _ _)) _ = dc
	
      	  extract 0 (dc @ (Axiom_dec  _ _)) _ = dc

      	  extract 0 (dc @ (Def _ _ _)) _ = dc
	
      	  extract 0 (dc @ (Data _ _ _)) _ = dc

--    	  extract 0 _ _ = error "BadIndex" -- ** exn

      	  extract i (Symbol_dec _ _) (dc:dcL) 
		= extract (i-1) dc dcL

      	  extract i (Axiom_dec  _ _) (dc:dcL) 
		= extract (i-1) dc dcL

      	  extract i (Def _ _ _) (dc:dcL) 
		= extract (i-1) dc dcL

      	  extract i (Data _ _ _) (dc:dcL) 
		= extract (i-1) dc dcL

      	  extract i (Decpair dc1 dc2 _) dcL 
		= extract (i-1) dc1 (dc2:dcL)
	
--      	  extract _ _ _ = error "BadIndex" -- ** exn





remake_ty (Binder q dc tm _ _) [] tm1 
	= subst_trm dc tm tm1

remake_ty (Binder q dc tm tmL att) (_:l) tm1 
	= Binder q dc (remake_ty tm l tm1) tmL att

--remake_ty _ _ _ = 
--	    error "VTS_ERROR" -- ** exn





{-
    (* return if a declaration defines just symbols *)
-}

is_sym_dec (Symbol_dec _ _) = True

is_sym_dec (Decpair dc1 dc2 _) 
	= is_sym_dec dc1 && is_sym_dec dc2

is_sym_dec _ = False





{- return if a declaration defines just axioms -}

is_axm_dec (Axiom_dec _ _) = True

is_axm_dec (Decpair dc1 dc2 _) 
	= is_axm_dec dc1 && is_axm_dec dc2

is_axm_dec _ = False





{- return if a declaration defines definitions -}

is_def_dec (Def _ _ _) = True

is_def_dec (Decpair dc1 dc2 _) 
	= is_def_dec dc1 && is_def_dec dc2

is_def_dec _ = False




{- return the type introduced by a declaration -}

typ_of_dec (Symbol_dec tm _) = tm

typ_of_dec (Axiom_dec tm _) = tm

typ_of_dec (dc @ (Decpair dc1 dc2 _)) 
	= if is_sym_dec dc 
		then Binder Sigma dc1 (typ_of_dec dc2) [] []
	        else if is_axm_dec dc 
			then Binary' And (typ_of_dec dc1) 
				(shift_trm [] (-1) (typ_of_dec dc2)) [] []
	    		else Sym 0 0 [] []-- TEMPORARY DEBUG SYM 0 NOT CORRECTerror "System_Error" -- ** exn

--typ_of_dec _ = error "System_Error" -- ** exn





mk_pi dc tm
	= Binder Pi dc tm [] []



mk_fnspace tm1 tm2 
	= Binder Pi (Symbol_dec tm1 []) (shift_trm [] 1 tm2) [] []







mk_sms (Symbol_dec _ _) i j 
	= (Sym i j [] [] , j+1)

mk_sms (dc @ (Decpair dc1 dc2 _)) i j 
	= (Pair sms1 sms2 (typ_of_dec dc) [] [] , j2) 
	  where
	  (sms1, j1) = mk_sms dc1 i (j+1)
	  (sms2, j2) = mk_sms dc2 i j1

--mk_sms _ _ _ =
--	    error "VTS_ERROR" -- ** exn






mk_smsl [] i tm1 = tm1 

mk_smsl (dc:dcL) i tml 
	= mk_smsl dcL (i+1) (sms : tml)
	  where
	  (sms, _) = mk_sms dc i 0

