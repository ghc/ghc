
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

module Sub_Core1 where

import Vtslib

import Core_datatype


{-
 * Equality on trms, decs and sgns is just on the important parts, ie ignore
 * extra types and information
 *
-}


{-
 * Pairwise compare values of two lists 
 * ie, eq_pair_wise cmp [a1,...,an] [b1,...,bn] = cmp a1 b1 andalso ... cmp an bn 
-}

eq_pair_wise cmp [] [] = True

eq_pair_wise cmp ( x1 : l1 ) ( x2 : l2 ) 
	= cmp x1 x2 && eq_pair_wise cmp l1 l2

eq_pair_wise cmp _ _ = False





eq_trm (Sym i1 j1 _ _) (Sym i2 j2 _ _) 
	= i1 == i2 && j1 == j2

eq_trm (App tm1 tm2 _ _) (App tm3 tm4 _ _) 
	= eq_trm tm1 tm3 && eq_trm tm2 tm4

eq_trm (Pair tm1 tm2 tm3 _ _) (Pair tm4 tm5 tm6 _ _) 
	= eq_trm tm1 tm4 && eq_trm tm2 tm5 && eq_trm tm3 tm6

eq_trm (Binder b1 dc1 tm1 _ _) (Binder b2 dc2 tm2 _ _) 
	= b1 == b2 && eq_dec dc1 dc2 && eq_trm tm1 tm2 

eq_trm (Constant c1 _ _) (Constant c2 _ _) 
	= c1 == c2 

eq_trm (Unary c1 tm1 _ _) (Unary c2 tm2 _ _) 
	= c1 == c2 && eq_trm tm1 tm2 

eq_trm (Binary' c1 tm1 tm2 _ _) (Binary' c2 tm3 tm4 _ _) 
	= c1 == c2 && eq_trm tm1 tm3 && eq_trm tm2 tm4 

eq_trm (Cond dc1 tm1 tm2 _ _) (Cond dc2 tm3 tm4 _ _) 
	= eq_dec dc1 dc2 && eq_trm tm1 tm3 && eq_trm tm2 tm4

eq_trm (Const i1 j1 k1 _ _) (Const i2 j2 k2 _ _) 
	= i1 == i2 && j1 == j2 && k1 == k2 

eq_trm (Recurse tmL1 tm1 _ _) (Recurse tmL2 tm2 _ _) 
	= eq_pair_wise eq_trm tmL1 tmL2 && eq_trm tm1 tm2

eq_trm _ _ = False






eq_dec (Symbol_dec tm1 _) (Symbol_dec tm2 _) 
	= eq_trm tm1 tm2

eq_dec (Axiom_dec tm1 _) (Axiom_dec tm2 _) 
	= eq_trm tm1 tm2

eq_dec (Def tm1 tm2 _) (Def tm3 tm4 _) 
	= eq_trm tm1 tm3 && eq_trm tm2 tm4

eq_dec (Data dcL1 tmLL1 _) (Data dcL2 tmLL2 _) 
	= eq_pair_wise eq_dec dcL1 dcL2 && 
	  eq_pair_wise (eq_pair_wise eq_trm) tmLL1 tmLL2 

eq_dec (Decpair dc1 dc2 _) (Decpair dc3 dc4 _) 
	= eq_dec dc1 dc3 && eq_dec dc2 dc4

eq_dec _ _ = False



      
eq_sgn (Empty _) (Empty _) = True 

eq_sgn (Extend dc1 sg1 _) (Extend dc2 sg2 _) 
	= eq_dec dc1 dc2 && eq_sgn sg1 sg2
	
eq_sgn (Combine sg1 sg2 i1 _ _) (Combine sg3 sg4 i2 _ _) 
	= i1 == i2 && eq_sgn sg1 sg3 && eq_sgn sg2 sg4

eq_sgn (Share sg1 i1 j1 k1 _ _) (Share sg2 i2 j2 k2 _ _) 
	= i1 == i2 && j1 == j2 && j1 == j2 && eq_sgn sg1 sg2

eq_sgn _ _ = False





{-
 * Extract a subterm from a term along with a list of all the 
 * declarations encounted on the spine down to the subterm.
 * generate an exception if the index does not point at a valid subterm
 * 
-}

-- select_trm :: ITrm -> [ Int ] -> ( ITrm , [ IDec ] )


select_trm tm iL  
	= sel_trm iL tm [] 
	  where
	  sel_trm :: [ Int] -> ITrm -> [ IDec ] -> ( ITrm , [ IDec ] )

	  sel_trm (i:iL) (App tm1 tm2 _ _) dcL 
		= sel_trm iL ([tm1,tm2]!!i) dcL

	  sel_trm (i:iL) (Pair tm1 tm2 _ _ _) dcL 
		= sel_trm iL ([tm1,tm2]!!i) dcL

	  sel_trm (i:iL) (Binder _ dc tm _ _) dcL 
		| i==0 = sel_dec iL dc dcL 
		| i==1 = sel_trm iL tm (dc:dcL)

	  sel_trm (0:iL) (Unary _ tm _ _) dcL 
		= sel_trm iL tm dcL

	  sel_trm (i:iL) (Binary' _ tm1 tm2 _ _) dcL 
		= sel_trm iL ([tm1,tm2]!!i) dcL

	  sel_trm (i:iL) (Cond (dc @ (Axiom_dec tm inf)) tm1 tm2 _ _) dcL 
		| i==0 = sel_dec iL dc dcL
		| i/=0 = sel_trm iL ([tm1,tm2]!!(i-1)) (dc1:dcL) 
	          	 where
		 	 dc1 = if i==1 
				then dc
				else if i==2
				       then Axiom_dec (Unary Not tm [] inf) []
				       else error "sel_trm"

	  sel_trm (i:iL) (Recurse tmL _ _ _) dcL 
		= sel_trm iL (tmL!!i) dcL

	  sel_trm [] tm dcL = (tm,dcL)

--          sel_trm _ _ _ = error "Subterm"  -- ** exn


	  sel_dec :: [ Int ] -> IDec -> [ IDec ] -> ( ITrm , [ IDec ] )

	  sel_dec (0:iL) (Symbol_dec tm _) dcL 
		= sel_trm iL tm dcL

	  sel_dec (0:iL) (Axiom_dec tm _) dcL 
		= sel_trm iL tm dcL

	  sel_dec (i:iL) (Decpair dc1 dc2 _) dcL 
		| i==0 = sel_dec iL dc1 dcL 
	        | i==1 = sel_dec iL dc2 (dc1:dcL)

--	  sel_dec _ _ _ = error "Subterm" -- ** exn







{-
 * replace a subterm of a term. this function assumes that the replacement
 * subterm is of an appropriate type and defined on the right signature.
 *
-} 

-- replace_trm :: ITrm -> ITrm -> [ Int ] -> ITrm

replace_trm tm1 tm2 iL 
	= rep_trm iL tm1 
	  where
	  swap 0 (_:xL) y 
		= y : xL

	  swap i (x:xL) y 
		= x : swap (i-1) xL y

--	  swap _ _ _ = error "Subterm" -- ** exn

	  rep_trm :: [Int] -> ITrm -> ITrm

	  rep_trm (i:iL) (App tm1 tm2 tyL inf) 
		| i==0 = App (rep_trm iL tm1) tm2 tyL inf 
		| i==1 = App tm1 (rep_trm iL tm2) tyL inf

	  rep_trm (i:iL) (Pair tm1 tm2 tm3 tyL inf) 
		| i==0 = Pair (rep_trm iL tm1) tm2 tm3 tyL inf
		| i==1 = Pair tm1 (rep_trm iL tm2) tm3 tyL inf

	  rep_trm (i:iL) (Binder q dc tm tyL inf) 
		| i==0 = Binder q (rep_dec iL dc) tm tyL inf
		| i==1 = Binder q dc (rep_trm iL tm) tyL inf

	  rep_trm (i:iL) (Unary c tm tyL inf) 
		| i==0 = Unary c (rep_trm iL tm) tyL inf

	  rep_trm (i:iL) (Binary' c tm1 tm2 tyL inf) 
		| i==0 = Binary' c (rep_trm iL tm1) tm2 tyL inf
		| i==1 = Binary' c tm1 (rep_trm iL tm2) tyL inf

	  rep_trm (i:iL) (Cond dc tm1 tm2 tyL inf) 
		| i==0 = Cond (rep_dec iL dc) tm1 tm2 tyL inf
		| i==1 = Cond dc (rep_trm iL tm1) tm2 tyL inf
		| i==2 = Cond dc tm1 (rep_trm iL tm2) tyL inf

	  rep_trm (i:iL) (Recurse tmL tm tyL inf) 
		= Recurse tmL1 tm tyL inf 
		  where
		  tmL1 = swap i tmL (rep_trm iL (tmL!!i))

	  rep_trm [] _ = tm2

	  rep_trm iL _ = error ( "iL(len):" ++ show ( length iL) ++ "\niL:" ++  concat ( map show iL ) ++ "|" ) --"Subterm" -- ** exn



	  rep_dec (0:iL) (Symbol_dec tm inf) 
		= Symbol_dec (rep_trm iL tm) inf
		 
 	  rep_dec (0:iL) (Axiom_dec tm inf) 
		= Axiom_dec (rep_trm iL tm) inf

	  rep_dec (i:iL) (Decpair dc1 dc2 inf) 
		| i==0 = Decpair (rep_dec iL dc1) dc2 inf
		| i==1 = Decpair dc1 (rep_dec iL dc2) inf

--	  rep_dec _ _ = error "Subterm" -- ** exn






{-
 * map two functions (one for symbol and one for constructors) over
 * a term, applying then to all the non localally bound symbols and
 * constructors in the term. Return two functions: the first applies to
 * terms; the second to declarations
 *
-}

type Sym_map = Int -> Int -> Int -> [ ITrm ] -> [ Attribute ] -> ITrm
	
type Const_map = Int -> Int -> Int -> Int -> [ ITrm ] -> [ Attribute ] -> ITrm


-- map_fn :: Sym_map -> Const_map -> (( ITrm -> ITrm ) , ( IDec -> IDec ))

map_fn map_symbol map_const
	= (map_trm 0, map_dec 0) 
	  where
	  map_trm n (Sym i j tyL inf) 
		= if i < n then Sym i j tyL1 inf
			   else map_symbol n i j tyL1 inf
		  where
		  tyL1 = map (map_trm n) tyL 

	  map_trm n (Const i j k tyL inf) 
		= if i < n then Const i j k tyL1 inf
			   else map_const n i j k tyL1 inf
		  where
		  tyL1 = map (map_trm n) tyL 

	  map_trm n (Constant c tyL inf) 
		= Constant c (map (map_trm n) tyL) inf

	  map_trm n (App tm1 tm2 tyL inf) 
		= App (map_trm n tm1) (map_trm n tm2) (map (map_trm n) tyL) inf

	  map_trm n (Pair tm1 tm2 tm3 tyL inf) 
		= Pair (map_trm n tm1) (map_trm n tm2) (map_trm n tm3)
			(map (map_trm n) tyL) inf

	  map_trm n (Binder q dc tm tyL inf) 
		= Binder q (map_dec n dc) (map_trm (n+1) tm)
			  (map (map_trm n) tyL) inf

	  map_trm n (Unary c tm tyL inf) 
		= Unary c (map_trm n tm) (map (map_trm n) tyL) inf

	  map_trm n (Binary' c tm1 tm2 tyL inf) 
		= Binary' c (map_trm n tm1) (map_trm n tm2) 
			(map (map_trm n) tyL) inf

	  map_trm n (Cond dc tm1 tm2 tyL inf) 
		= Cond dc1 tm3 tm4 tyL1 inf 
		  where
		  dc1 = map_dec n dc
		  tm3 = map_trm (n+1) tm1
		  tm4 = map_trm (n+1) tm2
		  tyL1 = map (map_trm n) tyL

	  map_trm n (Recurse tmL tm tyL inf) 
		= Recurse tmL1 tm1 tyL1 inf 
		  where
		  tmL1 = map (map_trm n) tmL
		  tm1  = map_trm n tm
		  tyL1 = map (map_trm n) tyL

	  map_trm n (Tagid tg tg_argL)
		= Tagid tg tg_argL  -- need to shift tg_argL

	  map_trm n (ITrm_Err mesg )
		= ITrm_Err mesg



	  map_dec n (Symbol_dec tm inf) 
		= Symbol_dec (map_trm n tm) inf

	  map_dec n (Axiom_dec tm inf) 
		= Axiom_dec (map_trm n tm) inf
	
	  map_dec n (Decpair dc1 dc2 inf) 
		= Decpair (map_dec n dc1) (map_dec (n+1) dc2) inf

	  map_dec n (Def tm1 tm2 inf) 
		= Def (map_trm n tm1) (map_trm n tm2) inf

	  map_dec n (Data dcL tmLL inf) 
		= Data dcl' tmLL' inf'
		  where
		  (dcl', tmLL', inf') = (map_data n dcL tmLL inf)


	  map_data n [] tmLL inf 
		= ([], map (map (map_trm (n+1))) tmLL,inf)

	  map_data n (dc:dcL) tmLL inf 
		= (map_dec n dc:dcL,tmLL1,inf1) 
		  where
		  (dcL1,tmLL1,inf1) = map_data (n+1) dcL tmLL inf





{- return the nth sub-signature of a signature. -}

nth_sgn 0 sg = sg

nth_sgn i (Extend _ sg _) 
	= nth_sgn (i-1) sg

nth_sgn i (Combine sg1 sg2 k _ _) 
	= if i<k then nth_sgn (i-1) sg2 
		 else nth_sgn (i-k-1) sg1

nth_sgn i (Share sg _ _ _ _ _) 
	= nth_sgn (i-1) sg

--nth_sgn i (Empty _) 
--	= error	"Nth_Sgn" -- ** exn







{- return the ith declaration of a signature -}

nth_dec i sg 
	= case nth_sgn i sg of
	      Extend dc _ _ -> dc
--	      _ 	    -> error "NthDec" -- ** exn





{- Compute the length of a signature -}

-- len_sgn :: ISgn -> Int

len_sgn (Empty _) = 0

len_sgn (Extend _ sg _) = 1 + len_sgn sg

len_sgn (Combine sg _ k _ _) = 1+ k + len_sgn sg

len_sgn (Share sg _ _ _ _ _) = 1 + len_sgn sg

