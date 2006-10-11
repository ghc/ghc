%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Demand]{@Demand@: the amount of demand on a value}

\begin{code}
module NewDemand(
	Demand(..), 
	topDmd, lazyDmd, seqDmd, evalDmd, errDmd, isStrictDmd, 
	isTop, isAbsent, seqDemand,

	DmdType(..), topDmdType, botDmdType, mkDmdType, mkTopDmdType, 
		dmdTypeDepth, seqDmdType,
	DmdEnv, emptyDmdEnv,
	DmdResult(..), retCPR, isBotRes, returnsCPR, resTypeArgDmd,
	
	Demands(..), mapDmds, zipWithDmds, allTop, seqDemands,

	StrictSig(..), mkStrictSig, topSig, botSig, cprSig,
        isTopSig,
	splitStrictSig,
	pprIfaceStrictSig, appIsBottom, isBottomingSig, seqStrictSig,
     ) where

#include "HsVersions.h"

import StaticFlags
import BasicTypes
import VarEnv
import UniqFM
import Util
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{Demands}
%*									*
%************************************************************************

\begin{code}
data Demand
  = Top			-- T; used for unlifted types too, so that
			--	A `lub` T = T
  | Abs			-- A

  | Call Demand		-- C(d)

  | Eval Demands	-- U(ds)

  | Defer Demands	-- D(ds)

  | Box Demand		-- B(d)

  | Bot			-- B
  deriving( Eq )
	-- Equality needed for fixpoints in DmdAnal

data Demands = Poly Demand	-- Polymorphic case
	     | Prod [Demand]	-- Product case
	     deriving( Eq )

allTop (Poly d)  = isTop d
allTop (Prod ds) = all isTop ds

isTop Top = True
isTop d   = False 

isAbsent Abs = True
isAbsent d   = False 

mapDmds :: (Demand -> Demand) -> Demands -> Demands
mapDmds f (Poly d)  = Poly (f d)
mapDmds f (Prod ds) = Prod (map f ds)

zipWithDmds :: (Demand -> Demand -> Demand)
	    -> Demands -> Demands -> Demands
zipWithDmds f (Poly d1)  (Poly d2)  = Poly (d1 `f` d2)
zipWithDmds f (Prod ds1) (Poly d2)  = Prod [d1 `f` d2 | d1 <- ds1]
zipWithDmds f (Poly d1)  (Prod ds2) = Prod [d1 `f` d2 | d2 <- ds2]
zipWithDmds f (Prod ds1) (Prod ds2) = Prod (zipWithEqual "zipWithDmds" f ds1 ds2)

topDmd, lazyDmd, seqDmd :: Demand
topDmd  = Top			-- The most uninformative demand
lazyDmd = Box Abs
seqDmd  = Eval (Poly Abs)	-- Polymorphic seq demand
evalDmd = Box seqDmd		-- Evaluate and return
errDmd  = Box Bot		-- This used to be called X

isStrictDmd :: Demand -> Bool
isStrictDmd Bot      = True
isStrictDmd (Eval _) = True
isStrictDmd (Call _) = True
isStrictDmd (Box d)  = isStrictDmd d
isStrictDmd other    = False

seqDemand :: Demand -> ()
seqDemand (Call d)   = seqDemand d
seqDemand (Eval ds)  = seqDemands ds
seqDemand (Defer ds) = seqDemands ds
seqDemand (Box d)    = seqDemand d
seqDemand _          = ()

seqDemands :: Demands -> ()
seqDemands (Poly d)  = seqDemand d
seqDemands (Prod ds) = seqDemandList ds

seqDemandList :: [Demand] -> ()
seqDemandList [] = ()
seqDemandList (d:ds) = seqDemand d `seq` seqDemandList ds

instance Outputable Demand where
    ppr Top  = char 'T'
    ppr Abs  = char 'A'
    ppr Bot  = char 'B'

    ppr (Defer ds)      = char 'D' <> ppr ds
    ppr (Eval ds)       = char 'U' <> ppr ds
				      
    ppr (Box (Eval ds)) = char 'S' <> ppr ds
    ppr (Box Abs)	= char 'L'
    ppr (Box Bot)	= char 'X'

    ppr (Call d)	= char 'C' <> parens (ppr d)


instance Outputable Demands where
    ppr (Poly Abs) = empty
    ppr (Poly d)   = parens (ppr d <> char '*')
    ppr (Prod ds)  = parens (hcat (map ppr ds))
	-- At one time I printed U(AAA) as U, but that
	-- confuses (Poly Abs) with (Prod AAA), and the
	-- worker/wrapper generation differs slightly for these two
	-- [Reason: in the latter case we can avoid passing the arg;
	--  see notes with WwLib.mkWWstr_one.]
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

	-- 		ANOTHER IMPORTANT INVARIANT
	-- The Demands in the argument list are never
	--	Bot, Defer d
	-- Handwavey reason: these don't correspond to calling conventions
	-- See DmdAnal.funArgDemand for details


-- This guy lets us switch off CPR analysis
-- by making sure that everything uses TopRes instead of RetCPR
-- Assuming, of course, that they don't mention RetCPR by name.
-- They should onlyu use retCPR
retCPR | opt_CprOff = TopRes
       | otherwise  = RetCPR

seqDmdType (DmdType env ds res) = 
  {- ??? env `seq` -} seqDemandList ds `seq` res `seq` ()

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
cprDmdType = DmdType emptyVarEnv [] retCPR

isTopDmdType :: DmdType -> Bool
-- Only used on top-level types, hence the assert
isTopDmdType (DmdType env [] TopRes) = ASSERT( isEmptyVarEnv env) True	
isTopDmdType other		     = False

isBotRes :: DmdResult -> Bool
isBotRes BotRes = True
isBotRes other  = False

resTypeArgDmd :: DmdResult -> Demand
-- TopRes and BotRes are polymorphic, so that
--	BotRes = Bot -> BotRes
--	TopRes = Top -> TopRes
-- This function makes that concrete
-- We can get a RetCPR, because of the way in which we are (now)
-- giving CPR info to strict arguments.  On the first pass, when
-- nothing has demand info, we optimistically give CPR info or RetCPR to all args
resTypeArgDmd TopRes = Top
resTypeArgDmd RetCPR = Top
resTypeArgDmd BotRes = Bot

returnsCPR :: DmdResult -> Bool
returnsCPR RetCPR = True
returnsCPR other  = False

mkDmdType :: DmdEnv -> [Demand] -> DmdResult -> DmdType
mkDmdType fv ds res = DmdType fv ds res

mkTopDmdType :: [Demand] -> DmdResult -> DmdType
mkTopDmdType ds res = DmdType emptyDmdEnv ds res

dmdTypeDepth :: DmdType -> Arity
dmdTypeDepth (DmdType _ ds _) = length ds
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

mkStrictSig :: DmdType -> StrictSig
mkStrictSig dmd_ty = StrictSig dmd_ty

splitStrictSig :: StrictSig -> ([Demand], DmdResult)
splitStrictSig (StrictSig (DmdType _ dmds res)) = (dmds, res)

isTopSig (StrictSig ty) = isTopDmdType ty

topSig, botSig, cprSig :: StrictSig
topSig = StrictSig topDmdType
botSig = StrictSig botDmdType
cprSig = StrictSig cprDmdType
	

-- appIsBottom returns true if an application to n args would diverge
appIsBottom (StrictSig (DmdType _ ds BotRes)) n = listLengthCmp ds n /= GT
appIsBottom _				      _ = False

isBottomingSig (StrictSig (DmdType _ _ BotRes)) = True
isBottomingSig _				= False

seqStrictSig (StrictSig ty) = seqDmdType ty

pprIfaceStrictSig :: StrictSig -> SDoc
-- Used for printing top-level strictness pragmas in interface files
pprIfaceStrictSig (StrictSig (DmdType _ dmds res))
  = hcat (map ppr dmds) <> ppr res
\end{code}
    

