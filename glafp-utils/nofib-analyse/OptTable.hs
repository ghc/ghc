-----------------------------------------------------------------------------
-- $Id: OptTable.hs,v 1.1 1999/11/12 11:54:17 simonmar Exp $
--
-- 	OGI_Table : Class for combinators used in building 2D tables.
--
-- 	Copyright (c) 1999 Andy Gill
--
-- This module is distributed as Open Source software under the
-- Artistic License; see the file "Artistic" that is included
-- in the distribution for details.
-----------------------------------------------------------------------------

module OptTable (
	OptTable,		-- abstract
	single,
	beside,
	above,
	getMatrix,
	) where

import qualified ClassTable as TC

instance TC.Table OptTable where
	single    = OptTable.single
	beside    = OptTable.beside
	above     = OptTable.above
	getMatrix = OptTable.getMatrix

instance (Show a) => Show (OptTable a) where
	showsPrec p = TC.showsTable

type TableI a = [[(a,(Int,Int))]] -> [[(a,(Int,Int))]]

data OptTable a	= Table (Int -> Int -> TableI a) Int Int

{-
 - Perhaps one day I'll fell adventureous, and write the Show instance
 - to show boxes aka the above ascii renditions.
 -}

-- You can create a (1x1) table entry
single :: a -> OptTable a
single a = Table (\ x y z -> [(a,(x+1,y+1))] : z) 1 1

-- You can compose tables, horizonally and vertically
above :: OptTable a -> OptTable a -> OptTable a
beside :: OptTable a -> OptTable a -> OptTable a

t1 `above` t2 = trans (combine (trans t1) (trans t2) (.))

t1 `beside` t2 = combine t1 t2 (\ lst1 lst2 r ->
    let
	-- Note this depends on the fact that
	-- that the result has the same number
	-- of lines as the y dimention; one list
	-- per line. This is not true in general
	-- but is always true for these combinators.
	-- I should assert this!
	beside (x:xs) (y:ys) = (x ++ y) : beside xs ys
	beside (x:xs) []     = x        : xs ++ r
	beside []     (y:ys) = y        : ys ++ r
	beside []     []     =                  r
    in
	beside (lst1 []) (lst2 []))

-- trans flips (transposes) over the x and y axis of
-- the table. It is only used internally, and typically
-- in pairs, ie. (flip ... munge ... (un)flip).

trans :: OptTable a -> OptTable a
trans (Table f1 x1 y1) = Table (flip f1) y1 x1

combine :: OptTable a 
	-> OptTable b 
	-> (TableI a -> TableI b -> TableI c) 
	-> OptTable c
combine (Table f1 x1 y1) (Table f2 x2 y2) comb = Table new_fn (x1+x2) max_y
    where
	max_y = max y1 y2
	new_fn x y =
	   case compare y1 y2 of
	    EQ -> comb (f1 0 y) 	    (f2 x y)
	    GT -> comb (f1 0 y)             (f2 x (y + y1 - y2))
	    LT -> comb (f1 0 (y + y2 - y1)) (f2 x y)

-- This is the other thing you can do with a Table;
-- turn it into a 2D list, tagged with the (x,y)
-- sizes of each cell in the table.

getMatrix :: OptTable a -> [[(a,(Int,Int))]]
getMatrix (Table r _ _) = r 0 0 []

