%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Demand]{@Demand@: the amount of demand on a value}

\begin{code}
#include "HsVersions.h"

module Demand where

import PprStyle		( PprStyle )
import Outputable
import Pretty		( SYN_IE(Pretty), PrettyRep, ppStr )
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

  | WwUnpack		-- Argument is strict & a single-constructor
	[Demand]	-- type; its constituent parts (whose StrictInfos
			-- are in the list) should be passed
			-- as arguments to the worker.

  | WwPrim		-- Argument is of primitive type, therefore
			-- strict; doesn't imply existence of a worker;
			-- argument should be passed as is to worker.

  | WwEnum		-- Argument is strict & an enumeration type;
			-- an Int# representing the tag (start counting
			-- at zero) should be passed to the worker.
  deriving (Eq, Ord)
      -- we need Eq/Ord to cross-chk update infos in interfaces

type MaybeAbsent = Bool -- True <=> not even used

-- versions that don't worry about Absence:
wwLazy	    = WwLazy 	  False
wwStrict    = WwStrict
wwUnpack xs = WwUnpack xs
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
isStrict (WwUnpack _)	= True
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
#ifdef REALLY_HASKELL_1_3
instance Read Demand where
#else
instance Text Demand where
#endif
    readList str = read_em [{-acc-}] str
      where
	read_em acc ('L' : xs)	= read_em (WwLazy   False : acc) xs
	read_em acc ('A' : xs)	= read_em (WwLazy   True  : acc) xs
	read_em acc ('S' : xs)	= read_em (WwStrict : acc) xs
	read_em acc ('P' : xs)	= read_em (WwPrim : acc) xs
	read_em acc ('E' : xs)	= read_em (WwEnum : acc) xs

	read_em acc (')' : xs)	= [(reverse acc, xs)]
	read_em acc ( 'U'  : '(' : xs)
	  = case (read_em [] xs) of
	      [(stuff, rest)] -> read_em (WwUnpack stuff : acc) rest
	      _ -> panic ("Text.Demand:"++str++"::"++xs)

	read_em acc rest	= [(reverse acc, rest)]

#ifdef REALLY_HASKELL_1_3
instance Show Demand where
#endif
    showList wrap_args rest = foldr show1 rest wrap_args
      where
	show1 (WwLazy False)  rest = 'L' : rest
	show1 (WwLazy True)   rest = 'A' : rest
	show1 WwStrict	      rest = 'S' : rest
	show1 WwPrim	      rest = 'P' : rest
	show1 WwEnum	      rest = 'E' : rest
	show1 (WwUnpack args) rest = "U(" ++ showList args (')' : rest)

instance Outputable Demand where
    ppr sty si = ppStr (showList [si] "")
\end{code}



