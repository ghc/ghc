%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Demand]{@Demand@: the amount of demand on a value}

\begin{code}
module NewDemand(
	Demand(..), Keepity(..), Deferredness(..), 
	topDmd, lazyDmd, seqDmd, evalDmd, isStrictDmd, isAbsentDmd,

	DmdType(..), topDmdType, mkDmdType, mkTopDmdType, 
		dmdTypeDepth, dmdTypeRes,
	DmdEnv, emptyDmdEnv,
	DmdResult(..), isBotRes, returnsCPR,

	StrictSig(..), mkStrictSig, topSig, botSig, 
	splitStrictSig, strictSigResInfo,
	pprIfaceStrictSig, appIsBottom, isBottomingSig
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
	       deriving( Eq, Show )
	-- Equality for fixpoints
	-- Show needed for Show in Lex.Token (sigh)

-- Equality needed for fixpoints in DmdAnal
instance Eq DmdType where
  (==) (DmdType fv1 ds1 res1)
       (DmdType fv2 ds2 res2) =  ufmToList fv1 == ufmToList fv2
			      && ds1 == ds2 && res1 == res2

instance Outputable DmdType where
  ppr (DmdType fv ds res) 
    = hsep [text "DmdType",
	    hcat (map ppr ds) <> ppr res,
	    if null fv_elts then empty
	    else braces (fsep (map pp_elt fv_elts))]
    where
      pp_elt (uniq, dmd) = ppr uniq <> text "->" <> ppr dmd
      fv_elts = ufmToList fv

instance Outputable DmdResult where
  ppr TopRes = empty	  -- Keep these distinct from Demand letters
  ppr RetCPR = char 'm'	  -- so that we can print strictness sigs as
  ppr BotRes = char 'b'   --    dddr
			  -- without ambiguity

emptyDmdEnv = emptyVarEnv
topDmdType = DmdType emptyDmdEnv [] TopRes
botDmdType = DmdType emptyDmdEnv [] BotRes

isBotRes :: DmdResult -> Bool
isBotRes BotRes = True
isBotRes other  = False

returnsCPR :: DmdResult -> Bool
returnsCPR RetCPR = True
returnsCPR other  = False

mkDmdType :: DmdEnv -> [Demand] -> DmdResult -> DmdType
mkDmdType fv ds res = DmdType fv ds res

mkTopDmdType :: [Demand] -> DmdResult -> DmdType
mkTopDmdType ds res = DmdType emptyDmdEnv ds res

dmdTypeDepth :: DmdType -> Arity
dmdTypeDepth (DmdType _ ds _) = length ds

dmdTypeRes :: DmdType -> DmdResult
dmdTypeRes (DmdType _ _ res_ty) = res_ty
\end{code}


%************************************************************************
%*									*
\subsection{Strictness signature
%*									*
%************************************************************************

In a let-bound Id we record its strictness info.  
In principle, this strictness info is a demand transformer, mapping
a demand on the Id into a DmdType, which gives
	a) the free vars of the Id's value
	b) the Id's arguments
	c) an indication of the result of applying 
	   the Id to its arguments

However, in fact we store in the Id an extremely emascuated demand transfomer,
namely 
		a single DmdType
(Nevertheless we dignify StrictSig as a distinct type.)

This DmdType gives the demands unleashed by the Id when it is applied
to as many arguments as are given in by the arg demands in the DmdType.

For example, the demand transformer described by the DmdType
		DmdType {x -> U(LL)} [V,A] Top
says that when the function is applied to two arguments, it
unleashes demand U(LL) on the free var x, V on the first arg,
and A on the second.  

If this same function is applied to one arg, all we can say is
that it uses x with U*(LL), and its arg with demand L.

\begin{code}
newtype StrictSig = StrictSig DmdType
		  deriving( Eq )

instance Outputable StrictSig where
   ppr (StrictSig ty) = ppr ty

instance Show StrictSig where
   show (StrictSig ty) = showSDoc (ppr ty)

mkStrictSig :: Id -> Arity -> DmdType -> StrictSig
mkStrictSig id arity dmd_ty
  = WARN( arity /= dmdTypeDepth dmd_ty, ppr id <+> (ppr arity $$ ppr dmd_ty) )
    StrictSig dmd_ty

splitStrictSig :: StrictSig -> ([Demand], DmdResult)
splitStrictSig (StrictSig (DmdType _ dmds res)) = (dmds, res)

strictSigResInfo :: StrictSig -> DmdResult
strictSigResInfo (StrictSig (DmdType _ _ res)) = res

topSig = StrictSig topDmdType
botSig = StrictSig botDmdType

-- appIsBottom returns true if an application to n args would diverge
appIsBottom (StrictSig (DmdType _ ds BotRes)) n = n >= length ds
appIsBottom _				      _ = False

isBottomingSig (StrictSig (DmdType _ _ BotRes)) = True
isBottomingSig _				= False

pprIfaceStrictSig :: StrictSig -> SDoc
-- Used for printing top-level strictness pragmas in interface files
pprIfaceStrictSig (StrictSig (DmdType _ dmds res))
  = hcat (map ppr dmds) <> ppr res
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

topDmd, lazyDmd, seqDmd :: Demand
topDmd  = Lazy			-- The most uninformative demand
lazyDmd = Lazy
seqDmd  = Seq Keep Now []	-- Polymorphic seq demand
evalDmd = Eval

isStrictDmd :: Demand -> Bool
isStrictDmd Bot 	  = True
isStrictDmd Err 	  = True    	   
isStrictDmd (Seq _ Now _) = True
isStrictDmd Eval	  = True
isStrictDmd (Call _)	  = True
isStrictDmd other	  = False

isAbsentDmd :: Demand -> Bool
isAbsentDmd Bot	  = True
isAbsentDmd Err	  = True
isAbsentDmd Abs	  = True
isAbsentDmd other = False

instance Outputable Demand where
    ppr Lazy 	     = char 'L'
    ppr Abs	     = char 'A'
    ppr Eval         = char 'V'
    ppr Err          = char 'X'
    ppr Bot          = char 'B'
    ppr (Call d)     = char 'C' <> parens (ppr d)
    ppr (Seq k l []) = ppr k <> ppr l
    ppr (Seq k l ds) = ppr k <> ppr l <> parens (hcat (map ppr ds))

instance Outputable Deferredness where
  ppr Now   = empty
  ppr Defer = char '*'

instance Outputable Keepity where
  ppr Keep = char 'S'
  ppr Drop = char 'U'
\end{code}

