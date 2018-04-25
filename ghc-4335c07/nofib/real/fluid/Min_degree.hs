{-
	New implementation of minimum degree ordering (more
	efficient).
	Algorithm from Duff86.

	XZ, 19/2/92
-}

module Min_degree (min_degree) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto
import Data.List(nub,partition)--1.3

-- minimum degree ordering
-- the entry lists in old_rows must be in assending order
min_degree :: (My_Array Int [Int]) -> [Int]	
min_degree old_rows = find_min init_counts [] [] []
	where
	-- initial row degree counts
	init_counts =
		s_accumArray (++) ([]::[Int]) (s_bounds old_rows)
		(map (\(x,y)->(length y,[x])) (s_assocs old_rows))
	-- find rows with minimum degrees (recursive)
	find_min counts cliques pro res =
		if remaining == []
		then res
		else find_min new_counts new_cliques processed new_pivots
		where
		-- updated result
		new_pivots = res ++ [pivot_i]
		-- processed rows
		processed = mg_line pro [pivot_i]
		-- updated row counts
		new_counts =
			s_accumArray mg_line ([]::[Int]) (s_bounds counts)
			((map (\(i,js)->(i,rm_list chgd js)) (sparse_assocs counts)) ++ updt)
			where
			chgd = mg_lines ([pivot_i]:[ js | (_, js) <- updt ])
		updt = count_update new_cols []
		-- counts of remaining rows
		remaining = sparse_assocs counts
		(_, (pivot_i:_)) = head remaining
		-- (List of) cliques with the processed column removed.
		-- Also, whole clique is removed if there is less
		-- 2 entries in it.
		rmed = do_rm cliques []
		-- the function does the removal
		do_rm (cli:clis) rmd =
			do_rm clis
			( 
				if (l2 == []) || (head l2) /= pivot_i
				then cli:rmd
				else
					case r of
					(r1:r2:_) -> r:rmd
					_         -> rmd
			)
			where
			r = l1 ++ (tail l2)
			(l1,l2) = partition ((<) pivot_i) cli
		do_rm _ res = res
		-- new cliques
		new_cliques = nub (new_cols:rmed)
		-- new clique
		new_cols = remove pivot_i (get_cols pivot_i cliques)
		    where
		      remove x = filter ((/=) x)	-- old haskell 1.0 function
		-- the function which updates the row counts
		count_update (r:rs) res =
			count_update rs
			(((length (get_cols r (new_cols:cliques)))-1,[r]):res)
		count_update _ res = res
		-- find nonzero entries
		get_cols = \i cli ->
			rm_list pro (mg_lines ((old_rows!^i):(filter (elem i) cli)))

-- the following functions assum lists are in assending order

-- check if two lists have something in common
inter_sec x@(x1:xs) y@(y1:ys)
	| x1 == y1  = True
	| x1 < y1   = inter_sec xs y
	| otherwise = inter_sec x ys
inter_sec _ _ = False

-- remove entries in the 1st list from the 2nd list
rm_list x@(x1:xs) y@(y1:ys)
	| x1 == y1  = rm_list xs ys
	| x1 < y1   = rm_list xs y
	| otherwise = y1:rm_list x ys
rm_list _ y = y

-- morge two lists
mg_line x@(x1:xs) y@(y1:ys)
	| x1 == y1  = x1:mg_line xs ys
	| x1 < y1   = x1:mg_line xs y
	| otherwise = y1:mg_line x ys
mg_line x y = x ++ y

-- merge many lists
mg_lines :: Ord a => [[a]] -> [a]

mg_lines = foldl1 mg_line
