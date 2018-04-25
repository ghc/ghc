module S_Array (
		My_Array, S_array(..),     -- counterpart of Array
		s_listArray,
		(!^),                      -- counterpart of (!)
		s_bounds,                  -- counterpart of bounds
		s_elems,                   -- counterpart of elems
		s_accumArray,              -- counterpart of accumArray
		s_accum,                   -- counterpart of accum
		s_amap,                    -- counterpart of amap
		arr_merg, arr_zip,
		fork, leaf,
		Maybe(..),
		Bin_Trie(..),
		Normal(..)
	)
	where

import Data.List(partition)
import Data.Ix

import Norm 
import Parallel

infixl 9 !^

{-
		definitions of data types
-}

type Assoc a b = (a,b) -- 1.3

-- data type of index
type Ix_type = Int
type My_Array a b = S_array b

-- data type of default value
--1.3:data Maybe a =
--	Nothing | Just a
--	deriving ()

-- data type of radix trie
data Bin_Trie a =
	Null | Leaf a | Fork Int (Bin_Trie a) (Bin_Trie a)
	deriving ()
-- data type of sparse array
data S_array a =
	Mk_t_Array (Ix_type,Ix_type) (Maybe a) (Bin_Trie a)
	deriving ()

{-
    declarations of exported functions
-}

s_listArray :: Normal a => (Int, Int) -> [a] -> S_array a
s_listArray b vs =
-- default value is set to Nothing
	Mk_t_Array b Nothing
	-- radix trie is generated from [a] by gen_trie_from_list
	(gen_trie_from_list (height b)
	(map leaf (take (b_size b) vs)))

(!^) :: My_Array Int a -> Int -> a
(!^) a@(Mk_t_Array b@(b1,_) default_v b_trie) i =
	if inRange b i               -- check index
	then get_v_from_trie (find_leaf b_trie (i-b1)) default_v
	else err_out

s_bounds :: My_Array Int a -> (Int, Int)
s_bounds (Mk_t_Array b _ _) = b

s_elems :: My_Array Int a -> [a]
s_elems (Mk_t_Array b default_v b_trie) =
	-- flatten a radix trie
	flatten (b_size b) b_trie
	where
	def_v = get_just_v default_v
	flatten n (Fork s t1 t2) =
		(flatten (min n s) t1) ++ (flatten (max 0 (n-s)) t2)
	flatten n Null = take n (repeat def_v)
	flatten _ (Leaf v) = [v]

s_accum :: (Normal b, Eq b) =>
	(b -> a -> b) -> S_array b -> [Assoc Int a] -> S_array b
s_accum f (Mk_t_Array b@(b1,_) default_v b_trie) asocs =
	if check_as b asocs
	then
		Mk_t_Array b default_v
		(do_accum b_trie (size b) (map_as b1 asocs))
	else err_out
	where
	defed = not (undefinedd default_v)
	def_v = get_just_v default_v
	-- generate a radix trie, slightly different from gen_trie
	gen_a_trie _ [] = Null
	gen_a_trie 1 as = gen_leaf def_v as
	gen_a_trie s as =
		fork s' (gen_a_trie s' as1) (gen_a_trie s' (map_as s' as2))
		where
		s' = s `div` 2
		(as1,as2) = partition (\(i, _)->(i<s')) as
	-- generate a leaf with accumulated value
	gen_leaf v as =
		if defed && (x == def_v)
		then Null
		else leaf x
		where x = foldl f v (map (\(_, v')->v') as)
	-- update radix trie with accumulated values
	do_accum br _ [] = br
	do_accum br s as =
		case br of
			(Fork s' br1 br2) ->
				fork s' (do_accum br1 s' as1) (do_accum br2 s' (map_as s' as2))
				where
				(as1,as2) = partition (\(i, _)->(i<s')) as
			Null -> gen_a_trie s as
			(Leaf v) -> gen_leaf v as

s_accumArray :: (Normal b, Eq b) => (b -> a -> b) -> b -> (Int, Int) -> [Assoc Int a] -> S_array b
s_accumArray f z b =
	s_accum f (Mk_t_Array b (Just z) Null)

s_amap :: Normal b => (a -> b) -> My_Array Int a -> My_Array Int b
s_amap f (Mk_t_Array b default_v b_trie) =
	Mk_t_Array b new_def_v (do_replace b_trie)
	where
	-- modify default value if necessary
	new_def_v =
		if undefinedd default_v
		then Nothing
		else Just ((f.get_just_v) default_v)
	-- function for replacing leaves with new values
	do_replace br =
		case br of
		(Fork s br1 br2) ->
			fork s (do_replace br1) (do_replace br2)
		Null -> Null
		(Leaf v) -> leaf (f v)

gen_trie_from_list :: Int -> [Bin_Trie a] -> Bin_Trie a
gen_trie_from_list n_tot = sub_gen n_tot
	where
	sub_gen _ [] = Null
	sub_gen 0 [t] = t
	sub_gen n sub_ts =
		sub_gen (n-1) (group ((2::Ix_type)^(n_tot-n)) sub_ts)

-- group subtries
group :: Int -> [Bin_Trie a] -> [Bin_Trie a]
group n (t1:t2:rest) = (fork n t1 t2) : (group n rest)
group n [t] = [fork n t Null]
group _ r@[] = r

arr_zip :: (Normal a, Normal b) =>
	My_Array Int a -> My_Array Int b -> My_Array Int (a, b)
arr_zip = arr_merg (\x y->(x,y))

arr_merg :: Normal c =>
	(a->b->c) -> (My_Array Int a) -> (My_Array Int b) -> (My_Array Int c)
arr_merg f (Mk_t_Array b def_v1 t1) (Mk_t_Array _ def_v2 t2) =
	Mk_t_Array b dv (t_op t1 t2)
	where
	just_v1 = get_just_v def_v1
	just_v2 = get_just_v def_v2
	dv =
		if not (undefinedd def_v1 || undefinedd def_v2)
		then Just (f just_v1 just_v2)
		else Nothing
	t_op (Fork s br11 br12) (Fork _ br21 br22) =
		fork s (t_op br11 br21) (t_op br12 br22)
	t_op Null t = do_rep (f just_v1) t
	t_op t Null = do_rep (\x->f x just_v2) t
	t_op (Leaf v1) (Leaf v2) = leaf (f v1 v2)
	do_rep f' (Fork s br1 br2) =
		fork s (do_rep f' br1) (do_rep f' br2)
	do_rep f' Null = Null
	do_rep f' (Leaf v) = leaf (f' v)

{-
		declarations of internal functions
-}

-- error functions

err_out = error "1" -- "s_array: index out of range!"
err_undefed = error "2" -- "s_array: value not defined!"
err_multi = error "3" -- "s_array: multiple value definitions!"

map_as :: Int -> [Assoc Int a] -> [Assoc Int a]
map_as = \s -> map (\(i,v)->((i-s),v))

check_as :: (Int, Int) -> [Assoc Int a] -> Bool
check_as = \b asocs -> and (map (\(i , _) -> inRange b i) asocs)

-- generate a fork
fork :: Int -> Bin_Trie a -> Bin_Trie a -> Bin_Trie a
fork s br1 br2 =
	 br1 `par` (br2  `par`			 -- FOR PARALLEL VERSION
	(if (empty_trie br1)&&(empty_trie br2) then br1 else Fork s br1 br2)
	) 					 -- FOR PARALLEL VERSION

leaf :: Normal a => a -> Bin_Trie a
leaf v | normal v = Leaf v

-- testing functions

-- test if a trie is empty
empty_trie :: Bin_Trie a-> Bool
empty_trie Null = True
empty_trie _ = False

-- value retrieve functions

undefinedd :: Maybe a -> Bool
undefinedd Nothing = True
undefinedd _ = False

-- locate a leaf ( or a Null node )
find_leaf :: Bin_Trie a-> Int -> Bin_Trie a
find_leaf = \t k ->
	case t of
		(Fork s t1 t2) ->
			if k<s then find_leaf t1 k else find_leaf t2 (k-s)
		_ -> t

get_just_v :: Maybe a -> a
get_just_v (Just v) = v
get_just_v _ = err_undefed

get_v_from_trie :: Bin_Trie a -> Maybe a -> a
get_v_from_trie (Leaf v) _ = v
get_v_from_trie _ (Just v) = v
get_v_from_trie _ _ = err_undefed

height :: (Int, Int) -> Int
height = \b@(b1,b2) ->
	if b1<=b2
	then ceiling (logBase 2 (fromIntegral (b_size b)))
	else 0

b_size :: (Int, Int) -> Int
b_size = \(b1,b2) ->
	if b1<=b2
	then b2-b1+1
	else 0

size = \b@(b1,b2) ->
	if b1<=b2 then (2::Ix_type)^(height b) else 0
