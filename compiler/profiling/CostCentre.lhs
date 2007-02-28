%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CostCentre]{The @CostCentre@ data type}

\begin{code}
module CostCentre (
	CostCentre(..), CcName, IsDupdCC(..), IsCafCC(..),
		-- All abstract except to friend: ParseIface.y

	CostCentreStack,
	CollectedCCs,
	noCCS, subsumedCCS, currentCCS, overheadCCS, dontCareCCS,
	noCostCentre, noCCAttached,
	noCCSAttached, isCurrentCCS,  isSubsumedCCS, currentOrSubsumedCCS,
	isDerivedFromCurrentCCS, maybeSingletonCCS,
	decomposeCCS,

	mkUserCC, mkAutoCC, mkAllCafsCC, 
	mkSingletonCCS, dupifyCC, pushCCOnCCS,
	isCafCCS, isCafCC,
	isSccCountCostCentre,
	sccAbleCostCentre,
	ccFromThisModule,

	pprCostCentreCore,
	costCentreUserName,

	cmpCostCentre	-- used for removing dups in a list
    ) where

#include "HsVersions.h"

import Var		( Id )
import Name
import Module		( Module )
import Outputable	
import FastTypes
import FastString
import Util	        ( thenCmp )
\end{code}

A Cost Centre Stack is something that can be attached to a closure.
This is either:
	
	- the current cost centre stack (CCCS)
	- a pre-defined cost centre stack (there are several
	  pre-defined CCSs, see below).

\begin{code}
data CostCentreStack
  = NoCCS

  | CurrentCCS		-- Pinned on a let(rec)-bound 
			-- thunk/function/constructor, this says that the 
			-- cost centre to be attached to the object, when it 
			-- is allocated, is whatever is in the 
			-- current-cost-centre-stack register.

  | SubsumedCCS		-- Cost centre stack for top-level subsumed functions
			-- (CAFs get an AllCafsCC).
			-- Its execution costs get subsumed into the caller.
			-- This guy is *only* ever pinned on static closures,
			-- and is *never* the cost centre for an SCC construct.

  | OverheadCCS		-- We charge costs due to the profiling-system
  			-- doing its work to "overhead".
			--
			-- Objects whose CCS is "Overhead"
			-- have their *allocation* charged to "overhead",
			-- but have the current CCS put into the object
			-- itself.

			-- For example, if we transform "f g" to "let
			-- g' = g in f g'" (so that something about
			-- profiling works better...), then we charge
			-- the *allocation* of g' to OverheadCCS, but
			-- we put the cost-centre of the call to f
			-- (i.e., current CCS) into the g' object.  When
			-- g' is entered, the CCS of the call
			-- to f will be set.

  | DontCareCCS		-- We need a CCS to stick in static closures
			-- (for data), but we *don't* expect them to
			-- accumulate any costs.  But we still need
			-- the placeholder.  This CCS is it.

  | PushCC CostCentre CostCentreStack
		-- These are used during code generation as the CCSs
		-- attached to closures.  A PushCC never appears as
		-- the argument to an _scc_.
		--
		-- The tail (2nd argument) is either NoCCS, indicating
		-- a staticly allocated CCS, or CurrentCCS indicating
		-- a dynamically created CCS.  We only support
		-- statically allocated *singleton* CCSs at the
		-- moment, for the purposes of initialising the CCS
		-- field of a CAF.

  deriving (Eq, Ord)	-- needed for Ord on CLabel
\end{code}

A Cost Centre is the argument of an _scc_ expression.
 
\begin{code}
data CostCentre
  = NoCostCentre	-- Having this constructor avoids having
			-- to use "Maybe CostCentre" all the time.

  | NormalCC {  
		cc_name :: CcName,	-- Name of the cost centre itself
		cc_mod  :: Module,	-- Name of module defining this CC.
		cc_is_dupd :: IsDupdCC,	-- see below
		cc_is_caf  :: IsCafCC	-- see below
    }

  | AllCafsCC {	
		cc_mod  :: Module	-- Name of module defining this CC.
    }

type CcName = FastString

data IsDupdCC
  = OriginalCC	-- This says how the CC is *used*.  Saying that
  | DupdCC		-- it is DupdCC doesn't make it a different
			-- CC, just that it a sub-expression which has
			-- been moved ("dupd") into a different scope.
			--
			-- The point about a dupd SCC is that we don't
			-- count entries to it, because it's not the
			-- "original" one.
			--
			-- In the papers, it's called "SCCsub",
			--  i.e. SCCsub CC == SCC DupdCC,
			-- but we are trying to avoid confusion between
			-- "subd" and "subsumed".  So we call the former
			-- "dupd".

data IsCafCC = CafCC | NotCafCC

-- synonym for triple which describes the cost centre info in the generated
-- code for a module.
type CollectedCCs
  = ( [CostCentre]       -- local cost-centres that need to be decl'd
    , [CostCentre]       -- "extern" cost-centres
    , [CostCentreStack]  -- pre-defined "singleton" cost centre stacks
    )
\end{code}

WILL: Would there be any merit to recording ``I am now using a
cost-centre from another module''?  I don't know if this would help a
user; it might be interesting to us to know how much computation is
being moved across module boundaries.

SIMON: Maybe later...

\begin{code}

noCCS 			= NoCCS
subsumedCCS 		= SubsumedCCS
currentCCS	 	= CurrentCCS
overheadCCS	 	= OverheadCCS
dontCareCCS	 	= DontCareCCS

noCostCentre  		= NoCostCentre
\end{code}

Predicates on Cost-Centre Stacks

\begin{code}
noCCSAttached NoCCS			= True
noCCSAttached _				= False

noCCAttached NoCostCentre		= True
noCCAttached _				= False

isCurrentCCS CurrentCCS			= True
isCurrentCCS _	      			= False

isSubsumedCCS SubsumedCCS 		= True
isSubsumedCCS _		     		= False

isCafCCS (PushCC cc NoCCS)		= isCafCC cc
isCafCCS _				= False

isDerivedFromCurrentCCS CurrentCCS	= True
isDerivedFromCurrentCCS (PushCC _ ccs)	= isDerivedFromCurrentCCS ccs
isDerivedFromCurrentCCS _		= False

currentOrSubsumedCCS SubsumedCCS	= True
currentOrSubsumedCCS CurrentCCS		= True
currentOrSubsumedCCS _			= False

maybeSingletonCCS (PushCC cc NoCCS)	= Just cc
maybeSingletonCCS _			= Nothing
\end{code}

Building cost centres

\begin{code}
mkUserCC :: FastString -> Module -> CostCentre
mkUserCC cc_name mod
  = NormalCC { cc_name = cc_name, cc_mod =  mod,
	       cc_is_dupd = OriginalCC, cc_is_caf = NotCafCC {-might be changed-}
    }

mkAutoCC :: Id -> Module -> IsCafCC -> CostCentre
mkAutoCC id mod is_caf
  = NormalCC { cc_name = str, cc_mod =  mod,
	       cc_is_dupd = OriginalCC, cc_is_caf = is_caf
    }
  where 
        name = getName id
        -- beware: we might be making an auto CC for a compiler-generated
        -- thing (like a CAF when -caf-all is on), so include the uniq.
        -- See bug #249, tests prof001, prof002
        str | isSystemName name = mkFastString (showSDoc (ppr name))
            | otherwise         = occNameFS (getOccName id)

mkAllCafsCC m = AllCafsCC  { cc_mod = m }



mkSingletonCCS :: CostCentre -> CostCentreStack
mkSingletonCCS cc = pushCCOnCCS cc NoCCS

pushCCOnCCS :: CostCentre -> CostCentreStack -> CostCentreStack
pushCCOnCCS = PushCC

dupifyCC cc = cc {cc_is_dupd = DupdCC}

isCafCC, isDupdCC :: CostCentre -> Bool

isCafCC (AllCafsCC {})		         = True
isCafCC (NormalCC {cc_is_caf = CafCC}) = True
isCafCC _		                 = False

isDupdCC (NormalCC   {cc_is_dupd = DupdCC}) = True
isDupdCC _		                     = False

isSccCountCostCentre :: CostCentre -> Bool
  -- Is this a cost-centre which records scc counts

#if DEBUG
isSccCountCostCentre NoCostCentre  = panic "isSccCount:NoCostCentre"
#endif
isSccCountCostCentre cc | isCafCC cc  = False
                        | isDupdCC cc = False
			| otherwise   = True

sccAbleCostCentre :: CostCentre -> Bool
  -- Is this a cost-centre which can be sccd ?

#if DEBUG
sccAbleCostCentre NoCostCentre  = panic "sccAbleCC:NoCostCentre"
#endif
sccAbleCostCentre cc | isCafCC cc = False
		     | otherwise  = True

ccFromThisModule :: CostCentre -> Module -> Bool
ccFromThisModule cc m = cc_mod cc == m
\end{code}

\begin{code}
instance Eq CostCentre where
	c1 == c2 = case c1 `cmpCostCentre` c2 of { EQ -> True; _ -> False }

instance Ord CostCentre where
	compare = cmpCostCentre

cmpCostCentre :: CostCentre -> CostCentre -> Ordering

cmpCostCentre (AllCafsCC  {cc_mod = m1}) (AllCafsCC  {cc_mod = m2}) = m1 `compare` m2

cmpCostCentre (NormalCC {cc_name = n1, cc_mod =  m1, cc_is_caf = c1}) 
	      (NormalCC {cc_name = n2, cc_mod =  m2, cc_is_caf = c2}) 
    -- first key is module name, then we use "kinds" (which include
    -- names) and finally the caf flag
  = (m1 `compare` m2) `thenCmp` (n1 `compare` n2) `thenCmp` (c1 `cmp_caf` c2)

cmpCostCentre other_1 other_2
  = let
	tag1 = tag_CC other_1
	tag2 = tag_CC other_2
    in
    if tag1 <# tag2 then LT else GT
  where
    tag_CC (NormalCC   {}) = (_ILIT 1 :: FastInt)
    tag_CC (AllCafsCC  {}) = _ILIT 2

cmp_caf NotCafCC CafCC     = LT
cmp_caf NotCafCC NotCafCC  = EQ
cmp_caf CafCC    CafCC     = EQ
cmp_caf CafCC    NotCafCC  = GT

decomposeCCS :: CostCentreStack -> ([CostCentre],CostCentreStack)
decomposeCCS (PushCC cc ccs) = (cc:more, ccs') 
  where (more,ccs') = decomposeCCS ccs
decomposeCCS ccs = ([],ccs)
\end{code}

-----------------------------------------------------------------------------
Printing Cost Centre Stacks.

The outputable instance for CostCentreStack prints the CCS as a C
expression.

NOTE: Not all cost centres are suitable for using in a static
initializer.  In particular, the PushCC forms where the tail is CCCS
may only be used in inline C code because they expand to a
non-constant C expression.

\begin{code}
instance Outputable CostCentreStack where
  ppr NoCCS		= ptext SLIT("NO_CCS")
  ppr CurrentCCS	= ptext SLIT("CCCS")
  ppr OverheadCCS	= ptext SLIT("CCS_OVERHEAD")
  ppr DontCareCCS	= ptext SLIT("CCS_DONT_CARE")
  ppr SubsumedCCS	= ptext SLIT("CCS_SUBSUMED")
  ppr (PushCC cc NoCCS) = ppr cc <> ptext SLIT("_ccs")
  ppr (PushCC cc ccs)   = ptext SLIT("PushCostCentre") <> 
			   parens (ppr ccs <> comma <> 
			   parens(ptext SLIT("void *")) <> ppr cc)
\end{code}

-----------------------------------------------------------------------------
Printing Cost Centres.

There are several different ways in which we might want to print a
cost centre:

	- the name of the cost centre, for profiling output (a C string)
	- the label, i.e. C label for cost centre in .hc file.
	- the debugging name, for output in -ddump things
	- the interface name, for printing in _scc_ exprs in iface files.

The last 3 are derived from costCentreStr below.  The first is given
by costCentreName.

\begin{code}
instance Outputable CostCentre where
  ppr cc = getPprStyle $ \ sty ->
	   if codeStyle sty
  	   then ppCostCentreLbl cc
	   else text (costCentreUserName cc)

-- Printing in an interface file or in Core generally
pprCostCentreCore (AllCafsCC {cc_mod = m})
  = text "__sccC" <+> braces (ppr m)
pprCostCentreCore (NormalCC {cc_name = n, cc_mod = m,
			     cc_is_caf = caf, cc_is_dupd = dup})
  = text "__scc" <+> braces (hsep [
	ftext (zEncodeFS n),
	ppr m,
	pp_dup dup,
	pp_caf caf
    ])

pp_dup DupdCC = char '!'
pp_dup other   = empty

pp_caf CafCC = text "__C"
pp_caf other   = empty

-- Printing as a C label
ppCostCentreLbl (NoCostCentre)		  = text "NONE_cc"
ppCostCentreLbl (AllCafsCC  {cc_mod = m}) = ppr m <> text "_CAFs_cc"
ppCostCentreLbl (NormalCC {cc_name = n, cc_mod = m, cc_is_caf = is_caf}) 
  = ppr m <> char '_' <> ftext (zEncodeFS n) <> 
	text (case is_caf of { CafCC -> "_CAF"; _ -> "" }) <> text "_cc"

-- This is the name to go in the user-displayed string, 
-- recorded in the cost centre declaration
costCentreUserName (NoCostCentre)  = "NO_CC"
costCentreUserName (AllCafsCC {})  = "CAF"
costCentreUserName cc@(NormalCC {cc_name = name, cc_is_caf = is_caf})
  =  case is_caf of { CafCC -> "CAF:";   _ -> "" } ++ unpackFS name
\end{code}
