%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Demand]{@Demand@: the amount of demand on a value}

\begin{code}
module Demand(
	Demand(..),

	wwLazy, wwStrict, wwUnpackData, wwUnpackNew, wwPrim, wwEnum, 
	isStrict,

	showDemands
     ) where

#include "HsVersions.h"

import BasicTypes	( NewOrData(..) )
import Outputable
import Util		( panic )
\end{code}


%************************************************************************
%*									*
\subsection{The @Demand@ data type}
%*									*
%************************************************************************

\begin{code}
data Demand
  = WwLazy		-- Argument is lazy as far as we know
	MaybeAbsent	-- (does not imply worker's existence [etc]).
			-- If MaybeAbsent == True, then it is
			-- *definitely* lazy.  (NB: Absence implies
			-- a worker...)

  | WwStrict		-- Argument is strict but that's all we know
			-- (does not imply worker's existence or any
			-- calling-convention magic)

  | WwUnpack		-- Argument is strict & a single-constructor type
	NewOrData
	Bool		-- True <=> wrapper unpacks it; False <=> doesn't
	[Demand]	-- Its constituent parts (whose StrictInfos
			-- are in the list) should be passed
			-- as arguments to the worker.

  | WwPrim		-- Argument is of primitive type, therefore
			-- strict; doesn't imply existence of a worker;
			-- argument should be passed as is to worker.

  | WwEnum		-- Argument is strict & an enumeration type;
			-- an Int# representing the tag (start counting
			-- at zero) should be passed to the worker.
  deriving( Eq )

type MaybeAbsent = Bool -- True <=> not even used

-- versions that don't worry about Absence:
wwLazy	    = WwLazy 	  False
wwStrict    = WwStrict
wwUnpackData xs = WwUnpack DataType False xs
wwUnpackNew  x  = WwUnpack NewType  False [x]
wwPrim	    = WwPrim
wwEnum	    = WwEnum
\end{code}


%************************************************************************
%*									*
\subsection{Functions over @Demand@}
%*									*
%************************************************************************

\begin{code}
isStrict :: Demand -> Bool

isStrict WwStrict	= True
isStrict (WwUnpack DataType _ _) = True
isStrict (WwUnpack NewType _ ds) = isStrict (head ds)
isStrict WwPrim		= True
isStrict WwEnum		= True
isStrict _		= False
\end{code}


%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************

\begin{code}
showDemands :: [Demand] -> String
showDemands wrap_args = show_demands wrap_args ""


#ifdef REALLY_HASKELL_1_3

instance Read Demand where
    readList str = read_em [] str
instance Show Demand where
    showsPrec prec wrap rest = show_demand wrap rest
    showList wrap_args rest  = show_demands wrap_args rest

#else

instance Text Demand where
    readList str = read_em [] str
    showList wrap_args rest = show_demands wrap_args rest

#endif

read_em acc ('L' : xs)	= read_em (WwLazy   False : acc) xs
read_em acc ('A' : xs)	= read_em (WwLazy   True  : acc) xs
read_em acc ('S' : xs)	= read_em (WwStrict : acc) xs
read_em acc ('P' : xs)	= read_em (WwPrim : acc) xs
read_em acc ('E' : xs)	= read_em (WwEnum : acc) xs
read_em acc (')' : xs)	= [(reverse acc, xs)]
read_em acc ( 'U'  : '(' : xs) = do_unpack DataType True  acc xs
read_em acc ( 'u'  : '(' : xs) = do_unpack DataType False acc xs
read_em acc ( 'N'  : '(' : xs) = do_unpack NewType  True  acc xs
read_em acc ( 'n'  : '(' : xs) = do_unpack NewType  False acc xs
read_em acc rest	= [(reverse acc, rest)]

do_unpack new_or_data wrapper_unpacks acc xs
	  = case (read_em [] xs) of
	      [(stuff, rest)] -> read_em (WwUnpack new_or_data wrapper_unpacks stuff : acc) rest
	      _ -> panic ("Demand.do_unpack:"++show acc++"::"++xs)

show_demands wrap_args rest
  = foldr show_demand rest wrap_args

show_demand (WwLazy False)  	  rest = 'L' : rest
show_demand (WwLazy True)   	  rest = 'A' : rest
show_demand WwStrict	      	  rest = 'S' : rest
show_demand WwPrim	      	  rest = 'P' : rest
show_demand WwEnum	      	  rest = 'E' : rest
show_demand (WwUnpack nd wu args) rest = ch:'(':showList args (')' : rest)
				      where
					ch = case nd of
						DataType | wu        -> 'U'
							 | otherwise -> 'u'
						NewType  | wu        -> 'N'
							 | otherwise -> 'n'

instance Outputable Demand where
    ppr si = text (showList [si] "")
\end{code}
