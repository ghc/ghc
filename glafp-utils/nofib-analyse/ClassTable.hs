-----------------------------------------------------------------------------
-- 	TableClass : Class for combinators used in building 2D tables.
--
-- 	Copyright (c) 1999 Andy Gill
--
-- This module is distributed as Open Source software under the
-- Artistic License; see the file "Artistic" that is included
-- in the distribution for details.
-----------------------------------------------------------------------------

module ClassTable (
		Table(..),
		showsTable,
		showTable,
	) where

infixr 4 `beside`
infixr 3 `above`

{----------------------------------------------------------------------------
   These combinators can be used to build formated 2D tables.
   The specific target useage is for HTML table generation.
 ----------------------------------------------------------------------------

   Examples of use:

  	> table1 :: (Table t) => t String
  	> table1 = single "Hello"	+-----+
					|Hello|
	  This is a 1x1 cell		+-----+
	  Note: single has type
	 
		single :: (Table t) => a -> t a
	
	  So the cells can contain anything.
	
	> table2 :: (Table t) => t String
	> table2 = single "World"	+-----+
					|World|
					+-----+


	> table3 :: (Table t) => t String
	> table3 = table1 %-% table2	+-----%-----+
					|Hello%World|
	 % is used to indicate		+-----%-----+
	 the join edge between
	 the two Tables.  

	> table4 :: (Table t) => t String
	> table4 = table3 %/% table2	+-----+-----+
					|Hello|World|
	  Notice the padding on the	%%%%%%%%%%%%%
	  smaller (bottom) cell to	|World      |
	  force the table to be a	+-----------+
	  rectangle.

	> table5 :: (Table t) => t String
	> table5 = table1 %-% table4	+-----%-----+-----+
					|Hello%Hello|World|
	  Notice the padding on the	|     %-----+-----+
	  leftmost cell, again to	|     %World      |
	  force the table to be a	+-----%-----------+
	  rectangle.
 
   Now the table can be rendered with processTable, for example:
	Main> processTable table5
	[[("Hello",(1,2)),
	  ("Hello",(1,1)),
	  ("World",(1,1))],
	 [("World",(2,1))]] :: [[([Char],(Int,Int))]]
	Main> 

----------------------------------------------------------------------------}

class Table t where
	-- There are no empty tables

	--Single element table
  single       :: a          -> t a
	-- horizontal composition
  beside       :: t a -> t a -> t a
	-- vertical composition
  above        :: t a -> t a -> t a
	-- generation of raw table matrix
  getMatrix    :: t a -> [[(a,(Int,Int))]]

showsTable :: (Show a,Table t) => t a -> ShowS
showsTable table = shows (getMatrix table)

showTable :: (Show a,Table t) => t a -> String
showTable table = showsTable table ""


