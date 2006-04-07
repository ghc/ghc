%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Demand]{@Demand@: the amount of demand on a value}

\begin{code}
#ifndef OLD_STRICTNESS
module Demand () where
#else

module Demand(
	Demand(..),

	wwLazy, wwStrict, wwUnpack, wwPrim, wwEnum, 
	isStrict, isLazy, isPrim,

	pprDemands, seqDemand, seqDemands,

	StrictnessInfo(..),	
	mkStrictnessInfo,
	noStrictnessInfo,
	ppStrictnessInfo, seqStrictnessInfo,
	isBottomingStrictness, appIsBottom,

     ) where

#include "HsVersions.h"

import Outputable
import Util ( listLengthCmp )
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
			--  *definitely* lazy.  (NB: Absence implies
			-- a worker...)

  | WwStrict		-- Argument is strict but that's all we know
			-- (does not imply worker's existence or any
			-- calling-convention magic)

  | WwUnpack		-- Argument is strict & a single-constructor type
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
wwUnpack xs = WwUnpack False xs
wwPrim	    = WwPrim
wwEnum	    = WwEnum

seqDemand :: Demand -> ()
seqDemand (WwLazy a)      = a `seq` ()
seqDemand (WwUnpack b ds) = b `seq` seqDemands ds
seqDemand other		  = ()

seqDemands [] = ()
seqDemands (d:ds) = seqDemand d `seq` seqDemands ds
\end{code}


%************************************************************************
%*									*
\subsection{Functions over @Demand@}
%*									*
%************************************************************************

\begin{code}
isLazy :: Demand -> Bool
isLazy (WwLazy _) = True
isLazy _	  = False

isStrict :: Demand -> Bool
isStrict d = not (isLazy d)

isPrim :: Demand -> Bool
isPrim WwPrim = True
isPrim other  = False
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
pprDemand (WwUnpack wu args)     = char ch <> parens (hcat (map pprDemand args))
				      where
					ch = if wu then 'U' else 'u'

instance Outputable Demand where
    ppr (WwLazy False) = empty
    ppr other_demand   = ptext SLIT("__D") <+> pprDemand other_demand

instance Show Demand where
    showsPrec p d = showsPrecSDoc p (ppr d)

-- Reading demands is done in Lex.lhs
\end{code}


%************************************************************************
%*									*
\subsection[strictness-IdInfo]{Strictness info about an @Id@}
%*									*
%************************************************************************

We specify the strictness of a function by giving information about
each of the ``wrapper's'' arguments (see the description about
worker/wrapper-style transformations in the PJ/Launchbury paper on
unboxed types).

The list of @Demands@ specifies: (a)~the strictness properties of a
function's arguments; and (b)~the type signature of that worker (if it
exists); i.e. its calling convention.

Note that the existence of a worker function is now denoted by the Id's
workerInfo field.

\begin{code}
data StrictnessInfo
  = NoStrictnessInfo

  | StrictnessInfo [Demand] 	-- Demands on the arguments.

		   Bool		-- True <=> the function diverges regardless of its arguments
				-- Useful for "error" and other disguised variants thereof.  
				-- BUT NB: f = \x y. error "urk"
				-- 	   will have info  SI [SS] True
				-- but still (f) and (f 2) are not bot; only (f 3 2) is bot
  deriving( Eq )

	-- NOTA BENE: if the arg demands are, say, [S,L], this means that
	-- 	(f bot) is not necy bot, only (f bot x) is bot
	-- We simply cannot express accurately the strictness of a function
	-- like		f = \x -> case x of (a,b) -> \y -> ...
	-- The up-side is that we don't need to restrict the strictness info
	-- to the visible arity of the function.

seqStrictnessInfo :: StrictnessInfo -> ()
seqStrictnessInfo (StrictnessInfo ds b) = b `seq` seqDemands ds
seqStrictnessInfo other		        = ()
\end{code}

\begin{code}
mkStrictnessInfo :: ([Demand], Bool) -> StrictnessInfo

mkStrictnessInfo (xs, is_bot)
  | all totally_boring xs && not is_bot	= NoStrictnessInfo		-- Uninteresting
  | otherwise		    	        = StrictnessInfo xs is_bot
  where
    totally_boring (WwLazy False) = True
    totally_boring other	  = False

noStrictnessInfo = NoStrictnessInfo

isBottomingStrictness (StrictnessInfo _ bot) = bot
isBottomingStrictness NoStrictnessInfo       = False

-- appIsBottom returns true if an application to n args would diverge
appIsBottom (StrictnessInfo ds bot)   n = bot && (listLengthCmp ds n /=GT) -- not more than 'n' elts in 'ds'.
appIsBottom  NoStrictnessInfo	      n	= False

ppStrictnessInfo NoStrictnessInfo		   = empty
ppStrictnessInfo (StrictnessInfo wrapper_args bot) = hsep [pprDemands wrapper_args bot]
\end{code}

\begin{code}
#endif /* OLD_STRICTNESS */
\end{code}
