%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Demand]{@Demand@: the amount of demand on a value}

\begin{code}
module Demand(
	Demand(..),

	wwLazy, wwStrict, wwUnpackData, wwUnpackNew, wwPrim, wwEnum, 
	isStrict, isLazy, isPrim,

	pprDemands, seqDemand, seqDemands
     ) where

#include "HsVersions.h"

import BasicTypes	( NewOrData(..) )
import Outputable
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

seqDemand :: Demand -> ()
seqDemand (WwLazy a)         = a `seq` ()
seqDemand (WwUnpack nd b ds) = nd `seq` b `seq` seqDemands ds
seqDemand other		     = ()

seqDemands [] = ()
seqDemands (d:ds) = seqDemand d `seq` seqDemands ds
\end{code}


%************************************************************************
%*									*
\subsection{Functions over @Demand@}
%*									*
%************************************************************************

\begin{code}
isStrict :: Demand -> Bool
isStrict (WwUnpack NewType _ ds) = isStrict (head ds)
isStrict (WwUnpack other _ _)    = True
isStrict WwStrict = True
isStrict WwEnum	  = True
isStrict WwPrim	  = True
isStrict _	  = False

isPrim :: Demand -> Bool
isPrim WwPrim = True
isPrim other  = False
\end{code}

\begin{code}
isLazy :: Demand -> Bool
isLazy (WwLazy False) = True	-- NB "Absent" args do *not* count!
isLazy _	      = False	-- (as they imply a worker)
\end{code}


%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************


\begin{code}
pprDemands demands bot = hcat (map pprDemand demands) <> pp_bot
		       where
			 pp_bot | bot       = ptext SLIT("B")
				| otherwise = empty


pprDemand (WwLazy False)  	 = char 'L'
pprDemand (WwLazy True)   	 = char 'A'
pprDemand WwStrict	      	 = char 'S'
pprDemand WwPrim	      	 = char 'P'
pprDemand WwEnum	      	 = char 'E'
pprDemand (WwUnpack nd wu args)  = char ch <> parens (hcat (map pprDemand args))
				      where
					ch = case nd of
						DataType | wu        -> 'U'
							 | otherwise -> 'u'
						NewType  | wu        -> 'N'
							 | otherwise -> 'n'

instance Outputable Demand where
    ppr (WwLazy False) = empty
    ppr other_demand   = ptext SLIT("__D") <+> pprDemand other_demand

instance Show Demand where
    showsPrec p d = showsPrecSDoc p (ppr d)
\end{code}


\begin{code}
{-	------------------- OMITTED NOW -------------------------------
	-- Reading demands is done in Lex.lhs
	-- Also note that the (old) code here doesn't take proper
	-- account of the 'B' suffix for bottoming functions

#ifdef REALLY_HASKELL_1_3

instance Read Demand where
    readList str = read_em [] str

instance Show Demand where
    showsPrec p d = showsPrecSDoc p (ppr d)

#else

instance Text Demand where
    readList str  = read_em [] str
    showsPrec p d = showsPrecSDoc p (ppr d)
#endif

readDemands :: String -> 

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
	      _ -> pprPanic "Demand.do_unpack:" (ppr acc <+> dcolon <> text xs)

-------------------- END OF OMISSION ------------------------------  -}
\end{code}

