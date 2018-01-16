module Tree where

import Core_datatype

import Vtslib

import Edlib

import Type_defs

import X_interface

data TREE a b c 
	= Tree a 
	       [TREE a b c]
	       ( Option b )
	       ( c -> TREE a b c -> TREE a b c )
	       ( Option ( TREE a b c ))
		      
		      
data Tree_state a b c 
	= TreeSt ( TREE a b c )
		 [( Int , TREE a b c )] 
		 c

{-
(******************************************************************************)
(*   All tree editor functions are of type                                    *)
(*         X.xinterface -> ('a,'b,'c) tree_state -> ('a,'b'c) tree_state      *)
(******************************************************************************)
-}

{-
(******************************************************************************)
(*   Take a function of type ('a tree -> 'a) tree and turn it into a tree     *)
(*   editor function                                                          *)
(******************************************************************************)
-}



lift_non_io_tree_fn tree_fn t@(TreeSt tr tr_st gst) 
	= tree_fn tr |.| 
	  ( \ res -> reTurn ( TreeSt res tr_st gst )) 
	  `handle` 
          failtest t 





{-
(******************************************************************************)
(*   Take a function of type (xinterface -> 'a tree -> 'a) tree and turn      *)
(*   it into a tree editor function                                           *)
(******************************************************************************)
-}

lift_io_tree_fn tree_fn t@(TreeSt tr tr_st gst) 
	= tree_fn tr /./ 
	  ( \ fn_res -> reTurn ( TreeSt fn_res tr_st gst )) 
	  `handle` 
	  failtest t 



lift_non_io_tree_st_fn tree_fn tr_tr_st 
	= tree_fn tr_tr_st |.|
	  reTurn  
	  `handle`
	  failtest tr_tr_st 



lift_io_tree_st_fn tree_fn tr_tr_st 
	= tree_fn tr_tr_st 
	  `handle` 
	  (\ _  -> reTurn tr_tr_st )


failtest t s 
	= x_error s        /./
	  ( \ _ -> reTurn t )




replace :: b -> Int -> [b] -> MayBe [b] String

replace = replace' []



replace' :: [b] -> b -> Int -> [b] -> MayBe [b] String

replace' rl x 0 (_ : l) = Ok ( rl ++ (x : l))

replace' rl x i (y : l) = replace' (rl <: y) x (i-1) l

replace' _ x i l = Bad " Match"




    
undo (Tree _ _ _ _ (SOME tr)) = tr

undo tr@(Tree _ _ _ _ NONE) = tr





down i (TreeSt tr@(Tree _ trL _ _ _) tr_st gst) 
	= TreeSt (trL!!i) ((i,tr) : tr_st) gst




up (TreeSt tr ( (i, Tree x trL dn vf tropt) : tr_st) gst) 
	= replace tr i trL |||
	  exp
	  where
	  exp rl = Ok ( TreeSt tr2 tr_st gst )
		   where
	  	   tr2 = if done && not (is_complete tr)
		  	    then if is_complete tr 
			       then tr1 
			       else mk_incomplete tr1
		      	    else (vf gst tr1 ) --`handle` \ _ -> tr1)
	  	   tr1 = Tree x rl dn vf tropt
	  	   done = is_complete tr1

up tr_st = Ok tr_st




is_complete (Tree _ _ NONE _ _) = False

is_complete (Tree _ _ (SOME _) _ _) = True




mk_incomplete (Tree x trL _ vf tropt) 
	= Tree x trL NONE vf tropt




top (tr_st@(TreeSt _ (_:_) gst)) 
	= up tr_st ||| top

top tr_st = Ok tr_st




search p f t = search_tree p f t []

search_tree p f t@(Tree _ l _ _ _) il 
	= if p t 
		then (if f then search_sub_tree p f l 0 il else []) ++ [(il,t)]
		else search_sub_tree p f l 0 il




search_sub_tree p f [] _ _ = []

search_sub_tree p f (t:l) i il 
	= search_tree p f t (i:il) ++ search_sub_tree p f l (i+1) il




goto [] tr_st     = tr_st

goto (i:il) tr_st = goto il (down i tr_st) 



tree_undo   = lift_non_io_tree_fn      ( mk_ok undo ) 
tree_top    = lift_non_io_tree_st_fn   top 
tree_up     = lift_non_io_tree_st_fn   up 
tree_down   = lift_non_io_tree_st_fn . mk_ok . down 
tree_search p f t = search p f t
tree_goto  a b = goto a b
