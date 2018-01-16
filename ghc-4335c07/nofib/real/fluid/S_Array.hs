{-
	This is a substitute for module PreludeArray oriented
	to sparse arrays.  Arrays are constructed using binary
	radix tries.  Signatures of functions are kept as close as
	possible to their standard counterparts (index type is
	restricted to Int).

	This module also includes some extra functions
	specifically for sparse array operations.

	XZ, 19/2/92
-}

module S_Array (
		S_array,                   -- counterpart of Array
		s_array,                   -- counterparts of array
		s_def_array,               -- new
		s_listArray,               -- counterparts of listArray
		s_def_listArray,           -- new
		(!^),                      -- counterpart of (!)
		s_bounds,                  -- counterpart of bounds
		s_indices,                 -- counterpart of indices
		s_elems,                   -- counterpart of elems
		s_assocs,                  -- counterpart of assocs
		s_accumArray,              -- counterpart of accumArray
		(//^),                     -- counterpart of (//)
		s_accum,                   -- counterpart of accum
		s_amap,                    -- counterpart of amap
		s_ixmap,                   -- counterpart of ixmap
		sparse_assocs {-,             -- new
		Norm.. partain: NOT YET ************** -}
	)
	where

import Norm

import Data.Array
import Data.Ix
import Data.List

infixl 9 !^
infixl 9 //^
infix  4 :^:

type Assoc a b = (a,b)

{-
		definitions of data types
-}

-- data type of index
type Ix_type = Int

-- data type of default value
--1.3:data Maybe a =
--	Nothing | Just a
--	deriving (Eq)

-- data type of radix trie
data Bin_Trie a =
	Null | Leaf a | (Bin_Trie a) :^: (Bin_Trie a)
	deriving ()
-- data type of sparse array
data S_array a =
	Mk_t_Array (Ix_type,Ix_type) Ix_type (Maybe a) (Bin_Trie a)
	deriving ()

{-
    function signatures
-}

s_array :: (Ix_type,Ix_type) -> [Assoc Ix_type a] -> S_array a
s_def_array :: (Eq a) =>
	(Ix_type,Ix_type) -> a -> [Assoc Ix_type a] -> S_array a
s_listArray :: (Ix_type,Ix_type) -> [a] -> S_array a
s_def_listArray :: (Eq a) =>
	(Ix_type,Ix_type) -> a -> [a] -> S_array a
(!^) :: S_array a -> Ix_type -> a
s_bounds :: S_array a -> (Ix_type,Ix_type)
s_indices :: S_array a -> [Ix_type]
s_elems :: S_array a -> [a]
s_assocs :: S_array a -> [Assoc Ix_type a]
sparse_assocs :: S_array a -> [Assoc Ix_type a]
s_accumArray :: (Eq a) =>
	(a->b->a) -> a -> (Ix_type,Ix_type) -> [Assoc Ix_type b] -> S_array a
(//^) :: (Eq a) =>
	S_array a -> [Assoc Ix_type a] -> S_array a
s_accum :: (Eq a) =>
	(a->b->a) -> S_array a -> [Assoc Ix_type b] -> S_array a
--hbc doesn't like: s_amap :: (b->a) -> S_array b -> S_array a
s_ixmap ::
	(Ix_type,Ix_type) -> (Ix_type->Ix_type) -> S_array a -> S_array a

{-
    declarations of exported functions
-}

s_array b@(b1,_) asocs =
	if check_as b asocs  -- check if indices are within bounds
	then                 -- ok
		-- do construction
		-- radix trie is generated from [Assoc Ix_type a] by gen_trie
		-- default value is set to Nothing
		Mk_t_Array b sz Nothing
		(gen_trie sz (convt_as b1 asocs))
	else err_out
	where
	sz = size b

-- new function, refer s_array for comments
s_def_array b@(b1,_) def_v asocs =
	if check_as b asocs
	then
		-- default value is set to (Just dev_v)
		Mk_t_Array b sz (Just def_v)
		(gen_trie sz (convt_as b1 new_as))
	else err_out
	where
	sz = size b
	-- remove trivial associations
	new_as =
		filter (\(i,v)->v/=def_v) asocs

s_listArray b vs =
	-- default value is set to Nothing
	Mk_t_Array b (size b) Nothing
	-- radix trie is generated from [a] by gen_trie_from_list
	(gen_trie_from_list (height b)
	(map Leaf (take (b_size b) vs)))

-- new function
s_def_listArray b def_v vs =
	-- default value is set to (Just dev_v)
	Mk_t_Array b (size b) (Just def_v)
	-- radix trie is generated from [a] by gen_trie_from_list
	(gen_trie_from_list (height b) (map convt (take (b_size b) vs)))
	where
	-- remove trivial values
	convt = \x ->
		if (x == def_v)
		then Null
		else Leaf x

(!^) a@(Mk_t_Array b@(b1,_) sz default_v b_trie) i =
	if inRange b i               -- check index
	then get_v_from_trie (find_leaf b_trie sz (i-b1)) default_v
	else err_out

s_bounds (Mk_t_Array b _ _ _) = b

s_indices = range.s_bounds

s_elems (Mk_t_Array b sz default_v b_trie) =
	-- flatten a radix trie
	flatten (b_size b) sz b_trie
	where
	def_v = get_just_v default_v
	flatten n s (t1 :^: t2) = l1 ++ l2
		where
		l1 = flatten (min s' n) s' t1
		l2 = flatten (max 0 (n-s')) s' t2
		s' = s `div` 2
	flatten n _ Null = [ def_v | i <- range (1,n) ]
	flatten _ _ (Leaf v) = [v]

s_assocs a =
	zipWith (\x y->(x,y)) (s_indices a) (s_elems a)

-- new function for obtaining non-trivial associations
sparse_assocs (Mk_t_Array (b1,_) sz _ b_trie) =
	flatten_sparse b1 sz b_trie
	where
	flatten_sparse n s (br1:^:br2) =
		(flatten_sparse n s' br1) ++ (flatten_sparse (n+s') s' br2)
		where s' = s `div` 2
	flatten_sparse _ _ Null = []
	flatten_sparse n _ (Leaf v) = [(n,v)]

(//^) (Mk_t_Array b@(b1,_) sz default_v b_trie) asocs =
	if check_as b asocs
	then
		if undefinedd default_v  -- check if default is defined
		then
			-- not defined, directly update
			Mk_t_Array b sz default_v
			(do_update b_trie sz (convt_as b1 asocs))
		else
			-- defined, convert trivials (to Null) than update
			Mk_t_Array b sz default_v
			(do_update b_trie sz (map_as b1 (map convt asocs)))
	else err_out
	where
	def_v = get_just_v default_v
	-- conversion function
	convt = \(i,v) ->
		if ( v==def_v )
		then (i,Null)
		else (i,Leaf v)
	-- trie update function
	do_update br _ [] = br
	do_update br s as =
		case br of
			(br1 :^: br2) ->
				fork (do_update br1 s' as1) (do_update br2 s' (map_as s' as2))
				where
				s' = s `div` 2
				(as1,as2) = partition (\(i,_)->(i<s')) as
			Null -> gen_trie s as
			(Leaf _) ->
				case as of
					[(_,v)] -> v
					_ -> err_multi

s_accum f (Mk_t_Array b@(b1,_) sz default_v b_trie) asocs =
	if check_as b asocs
	then
		Mk_t_Array b sz default_v
		(do_accum b_trie sz (map_as b1 asocs))
	else err_out
	where
	defed = not (undefinedd default_v)
	def_v = get_just_v default_v
	-- generate a radix trie, slightly different from gen_trie
	gen_a_trie _ [] = Null
	gen_a_trie 1 as = gen_leaf def_v as
	gen_a_trie s as =
		fork (gen_a_trie s' as1) (gen_a_trie s' (map_as s' as2))
		where
		s' = s `div` 2
		(as1,as2) = partition (\(i,_)->(i<s')) as
	-- generate a leaf with accumulated value
	gen_leaf v as =
		if defed && (x == def_v)
		then Null
		else Leaf x
		where x = foldl f v (map (\(_,v')->v') as)
	-- update radix trie with accumulated values
	do_accum br _ [] = br
	do_accum br s as =
		case br of
			(br1 :^: br2) ->
				fork (do_accum br1 s' as1) (do_accum br2 s' (map_as s' as2))
				where
				s' = s `div` 2
				(as1,as2) = partition (\(i,_)->(i<s')) as
			Null -> gen_a_trie s as
			(Leaf v) -> gen_leaf v as

s_accumArray f z b =
	s_accum f (Mk_t_Array b (size b) (Just z) Null)

s_amap f (Mk_t_Array b sz default_v b_trie) =
	Mk_t_Array b sz new_def_v (do_replace b_trie)
	where
	-- modify default value if necessary
	new_def_v =
		if undefinedd default_v
		then default_v
		else Just ((f.get_just_v) default_v)
	-- function for replacing leaves with new values
	do_replace br =
		case br of
			(br1 :^: br2) ->
				fork (do_replace br1) (do_replace br2)
			Null -> br
			(Leaf v) -> Leaf (f v)

s_ixmap b f (Mk_t_Array (b1,_) sz default_v b_trie) =
	Mk_t_Array b (size b) default_v
	(gen_trie_from_list (height b)
	(map (find_leaf b_trie sz)
	(map (\i->(f i)-b1) (range b))))

{-
		declarations of internal functions
-}

-- error functions

err_out = error "s_array: index out of range!"
err_undefed = error "s_array: value not defined!"
err_multi = error "s_array: multiple value definitions!"

-- functions operating on [Assoc Ix_type a]

convt_as ::
	Ix_type -> [Assoc Ix_type a] -> [Assoc Ix_type (Bin_Trie a)]
convt_as = \s -> map (\ (i,v) -> ((i-s),Leaf v))

map_as :: Ix_type -> [Assoc Ix_type a] -> [Assoc Ix_type a]
map_as = \s -> map (\ (i,v)->((i-s),v))

check_as :: (Ix_type,Ix_type) -> [Assoc Ix_type a] -> Bool
check_as = \b asocs -> and (map (\(i , _) -> inRange b i) asocs)

-- functions for generating radix tries

-- generate a trie from [Assoc Ix_type (Bin_Trie a)]
gen_trie :: Ix_type -> [Assoc Ix_type (Bin_Trie a)] -> Bin_Trie a
gen_trie _ [] = Null
gen_trie 1 [(_,v)] = v
gen_trie 1 _ = err_multi
gen_trie s as =
	fork (gen_trie s' as1) (gen_trie s' (map_as s' as2))
	where
	s' = s `div` 2
	(as1,as2) = partition (\(i,_)->(i<s')) as

-- generate a trie from [(Bin_Trie a)]
gen_trie_from_list :: Ix_type -> [Bin_Trie a] -> Bin_Trie a
gen_trie_from_list _ [] = Null
gen_trie_from_list 0 [t] = t
gen_trie_from_list n sub_ts =
	gen_trie_from_list (n-1) (groop sub_ts )

-- group subtries
groop :: [Bin_Trie a] -> [Bin_Trie a]
groop (t1:t2:rest) = (fork t1 t2) : (groop rest)
groop [t] = [fork t Null]
groop _ = []

-- generate a fork
fork :: Bin_Trie a -> Bin_Trie a -> Bin_Trie a
fork Null Null = Null
fork br1 br2 = br1 :^: br2

-- testing functions

-- test if a trie is empty
empty_trie :: Bin_Trie a -> Bool
empty_trie Null = True
empty_trie _ = False

-- test if a default value is defined
undefinedd :: Maybe a -> Bool
undefinedd Nothing = True
undefinedd _ = False

-- value retrieve functions

-- locate a leaf ( or a Null node )
find_leaf :: Bin_Trie a -> Ix_type -> Ix_type -> Bin_Trie a
find_leaf = \t s k ->
	case t of
		(t1 :^: t2) ->
			find_leaf t' s' (if k<s' then k else (k-s'))
			where
			s' = s `div` 2
			t' = if k<s' then t1 else t2
		_ -> t

get_just_v :: (Maybe a) -> a
get_just_v (Just v) = v
get_just_v _ = err_undefed

get_leaf_v :: (Bin_Trie a) -> a
get_leaf_v (Leaf v) = v
get_leaf_v _ = err_undefed

get_v_from_trie :: (Bin_Trie a) -> (Maybe a) -> a
get_v_from_trie (Leaf v) _ = v
get_v_from_trie _ (Just v) = v
get_v_from_trie _ _ = err_undefed

-- functions for calculating sizes

height :: (Ix_type,Ix_type) -> Ix_type
height = \b@(b1,b2) ->
	if b1<=b2
	then ceiling (logBase 2 (fromIntegral (b_size b)))
	--partain: could use: then ceiling ((logBase (2::Float) ((fromIntegral (b_size b))::Float)) ::Float)
	else 0

size :: (Ix_type,Ix_type) -> Ix_type
size = \b@(b1,b2) ->
	if b1<=b2
	then (2::Ix_type)^(height b)
	else 0

b_size :: (Ix_type,Ix_type) -> Ix_type
b_size = \(b1,b2) ->
	if b1<=b2
	then b2-b1+1
	else 0

{-
		definitions of (==) on S_array
-}

instance (Eq a) => Eq (S_array a) where
	(Mk_t_Array b1 sz d1 b_trie1) == (Mk_t_Array b2 _ d2 b_trie2) =
		b1 == b2 && eq_trie leaf_no sz b_trie1 b_trie2
		where
		leaf_no = b_size b1
		eq_def = d1 == d2

		-- partain: added sig
		eq_trie :: (Eq b) => Ix_type -> Ix_type -> Bin_Trie b -> Bin_Trie b -> Bool
		eq_trie 0 _ _ _ = True
		eq_trie l s (lb1:^:rb1) (lb2:^:rb2) =
			eq_trie (min s' l) s' lb1 lb2 &&
			eq_trie (max 0 (l-s')) s' rb1 rb2
			where
			s' = s `div` 2
		eq_trie _ _ Null Null = eq_def
		eq_trie _ _ (Leaf x) (Leaf y) = x == y
		eq_trie _ _ _ _ = False

{-
		definitions for class Text
-}

instance (Show a) =>
	Show (S_array a) where
	showsPrec p a =
		showParen (p>9) (
		showString "array " .
		shows (s_bounds a) . showChar ' ' .
		shows (s_assocs a))
instance (Read a) =>
	Read (S_array a) where
	readsPrec p =
		readParen (p>9)
		(\r ->
			[ (s_array b as, u) |
				("array", s) <- lex r,
				(b,t)<- reads s,
				(as,u) <- reads t
			]
			++
			[ (s_listArray b xs, u) |
				("listArray",s) <- lex r,
				(b,t) <- reads s,
				(xs,u) <- reads t
			]
		)

instance (Show a) =>
	Show (Bin_Trie a) where
	showsPrec p t =
		case t of
		Null -> showString "Null"
		(Leaf m) ->
			showParen (p>9)
			(showString "Leaf " . showsPrec 10 m)
		(u:^:v) ->
			showParen (p>4)
			(showsPrec 5 u . showString " :^: " . showsPrec 5 v)

instance (Normal a) => Normal (Bin_Trie a) where
	normal (b1 :^: b2) = normal b1 `andAnd` normal b2
	normal (Leaf v) = normal v
	normal Null = True

instance (Normal a) => Normal (Maybe a) where
	normal (Just v) = normal v
	normal Nothing = True

instance (Normal a) => Normal (S_array a) where
	normal a@(Mk_t_Array b s def_v b_trie)
		| normal b `andAnd` normal s `andAnd` normal def_v `andAnd` normal b_trie = True
