%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Demand]{@Demand@: the amount of demand on a value}

\begin{code}
module NewDemand(
	Demand(..), Keepity(..), topDmd,
	StrictSig(..), topSig, botSig, mkStrictSig,
	DmdType(..), topDmdType, mkDmdFun,
	Result(..)
     ) where

#include "HsVersions.h"

import BasicTypes	( Arity )
import qualified Demand
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{Strictness signatures
%*									*
%************************************************************************

\begin{code}
data StrictSig = StrictSig Arity DmdType
	       deriving( Eq )
	-- Equality needed when comparing strictness 
	-- signatures for fixpoint finding

topSig = StrictSig 0 topDmdType
botSig = StrictSig 0 botDmdType

mkStrictSig :: Arity -> DmdType -> StrictSig
mkStrictSig arity ty 
  = WARN( arity /= dmdTypeDepth ty, ppr arity $$ ppr ty )
    StrictSig arity ty

instance Outputable StrictSig where
  ppr (StrictSig arity ty) = ppr ty
\end{code}


%************************************************************************
%*									*
\subsection{Demand types}
%*									*
%************************************************************************

\begin{code}
data DmdType = DmdRes Result | DmdFun Demand DmdType
	     deriving( Eq )
	-- Equality needed for fixpoints in DmdAnal

data Result = TopRes	-- Nothing known	
	    | RetCPR	-- Returns a constructed product
	    | BotRes	-- Diverges or errors
	    deriving( Eq )
	-- Equality needed for fixpoints in DmdAnal

instance Outputable DmdType where
  ppr (DmdRes TopRes) = char 'T'
  ppr (DmdRes RetCPR) = char 'M'
  ppr (DmdRes BotRes) = char 'X'
  ppr (DmdFun d r)    = ppr d <> ppr r

topDmdType = DmdRes TopRes
botDmdType = DmdRes BotRes

mkDmdFun :: [Demand] -> Result -> DmdType
mkDmdFun ds res = foldr DmdFun (DmdRes res) ds

dmdTypeDepth :: DmdType -> Arity
dmdTypeDepth (DmdFun _ ty) = 1 + dmdTypeDepth ty
dmdTypeDepth (DmdRes _)    = 0
\end{code}


%************************************************************************
%*									*
\subsection{Demands}
%*									*
%************************************************************************

\begin{code}
data Demand
  = Lazy		-- L; used for unlifted types too, so that
			--	A `lub` L = L
  | Abs			-- A
  | Call Demand		-- C(d)
  | Eval 		-- V
  | Seq Keepity		-- S/U(ds)
	[Demand]
  | Err			-- X
  | Bot			-- B
  deriving( Eq )
	-- Equality needed for fixpoints in DmdAnal

data Keepity = Keep | Drop
	     deriving( Eq )

topDmd :: Demand	-- The most uninformative demand
topDmd = Lazy

instance Outputable Demand where
    ppr Lazy 	   = char 'L'
    ppr Abs	   = char 'A'
    ppr Eval       = char 'V'
    ppr Err        = char 'X'
    ppr Bot        = char 'B'
    ppr (Call d)   = char 'C' <> parens (ppr d)
    ppr (Seq k ds) = ppr k <> parens (hcat (map ppr ds))

instance Outputable Keepity where
  ppr Keep = char 'S'
  ppr Drop = char 'U'
\end{code}

