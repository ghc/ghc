%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Demand]{@Demand@: the amount of demand on a value}

\begin{code}
module NewDemand(
	Demand(..), Keepity(..), Deferredness(..), topDmd,
	StrictSig(..), topSig, botSig, mkStrictSig,
	DmdType(..), topDmdType, mkDmdType, mkTopDmdType,
	DmdEnv, emptyDmdEnv,
	DmdResult(..), isBotRes
     ) where

#include "HsVersions.h"

import BasicTypes	( Arity )
import Var		( Id )
import VarEnv		( VarEnv, emptyVarEnv )
import UniqFM		( ufmToList )
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

mkStrictSig :: Id -> Arity -> DmdType -> StrictSig
mkStrictSig id arity ty 
  = WARN( arity /= dmdTypeDepth ty, ppr id <+> (ppr arity $$ ppr ty) )
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
data DmdType = DmdType 
		    DmdEnv	-- Demand on explicitly-mentioned 
				--	free variables
		    [Demand]	-- Demand on arguments
		    DmdResult	-- Nature of result

	-- 		IMPORTANT INVARIANT
	-- The default demand on free variables not in the DmdEnv is:
	-- DmdResult = BotRes        <=>  Bot
	-- DmdResult = TopRes/ResCPR <=>  Abs

type DmdEnv = VarEnv Demand

data DmdResult = TopRes	-- Nothing known	
	       | RetCPR	-- Returns a constructed product
	       | BotRes	-- Diverges or errors
	       deriving( Eq )

-- Equality needed for fixpoints in DmdAnal
instance Eq DmdType where
  (==) (DmdType fv1 ds1 res1)
       (DmdType fv2 ds2 res2) =  ufmToList fv1 == ufmToList fv2
			      && ds1 == ds2 && res1 == res2

instance Outputable DmdType where
  ppr (DmdType fv ds res) 
    = hsep [text "DmdType",
	    hcat (map ppr ds) <> ppr res,
	    braces (fsep (map pp_elt (ufmToList fv)))]
    where
      pp_elt (uniq, dmd) = ppr uniq <> text "->" <> ppr dmd

instance Outputable DmdResult where
  ppr TopRes = char 'T'
  ppr RetCPR = char 'M'
  ppr BotRes = char 'X'

emptyDmdEnv = emptyVarEnv
topDmdType = DmdType emptyDmdEnv [] TopRes
botDmdType = DmdType emptyDmdEnv [] BotRes

isBotRes :: DmdResult -> Bool
isBotRes BotRes = True
isBotRes other  = False

mkDmdType :: DmdEnv -> [Demand] -> DmdResult -> DmdType
mkDmdType fv ds res = DmdType fv ds res

mkTopDmdType :: [Demand] -> DmdResult -> DmdType
mkTopDmdType ds res = DmdType emptyDmdEnv ds res

dmdTypeDepth :: DmdType -> Arity
dmdTypeDepth (DmdType _ ds _) = length ds
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
	Deferredness
	[Demand]
  | Err			-- X
  | Bot			-- B
  deriving( Eq )
	-- Equality needed for fixpoints in DmdAnal

data Deferredness = Now | Defer
		  deriving( Eq )

data Keepity = Keep | Drop
	     deriving( Eq )

topDmd :: Demand	-- The most uninformative demand
topDmd = Lazy

instance Outputable Demand where
    ppr Lazy 	     = char 'L'
    ppr Abs	     = char 'A'
    ppr Eval         = char 'V'
    ppr Err          = char 'X'
    ppr Bot          = char 'B'
    ppr (Call d)     = char 'C' <> parens (ppr d)
    ppr (Seq k l ds) = ppr k <> ppr l <> parens (hcat (map ppr ds))

instance Outputable Deferredness where
  ppr Now   = empty
  ppr Defer = char '*'

instance Outputable Keepity where
  ppr Keep = char 'S'
  ppr Drop = char 'U'
\end{code}

