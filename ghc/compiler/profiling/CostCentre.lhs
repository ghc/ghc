%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CostCentre]{The @CostCentre@ data type}

\begin{code}
module CostCentre (
	CostCentre, CcKind, IsDupdCC{-ToDo:rm-}, IsCafCC(..),
	CostCentreStack,
	noCCS, subsumedCCS, currentCCS, overheadCCS, dontCareCCS,
	noCostCentre, noCCAttached,
	noCCSAttached, isCurrentCCS,  isSubsumedCCS, currentOrSubsumedCCS,

	mkUserCC, mkAutoCC, mkDictCC, mkAllCafsCC, mkAllDictsCC,
	mkSingletonCCS, cafifyCC, dupifyCC,
	isCafCC, isDictCC, isDupdCC, isEmptyCC, isCafCCS,
	isSccCountCostCentre,
	sccAbleCostCentre,
	ccFromThisModule,
	ccMentionsId,

	pprCostCentreDecl, pprCostCentreStackDecl,

	cmpCostCentre	-- used for removing dups in a list
    ) where

#include "HsVersions.h"

import Var		( externallyVisibleId, Id )
import CStrings		( stringToC )
import Name		( Module, getOccString, moduleString, identToC, pprModule )
import Outputable	
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

  | SingletonCCS CostCentre
			-- This is primarily for CAF cost centres, which
			-- are attached to top-level thunks right at the
			-- end of STG processing, before code generation.
			-- Hence, a CAF cost centre never appears as the
			-- argument of an _scc_.
			-- Also, we generate these singleton CCSs statically
			-- as part of code generation.

  deriving (Eq, Ord)	-- needed for Ord on CLabel
\end{code}

A Cost Centre is the argument of an _scc_ expression.
 
\begin{code}
type Group = FAST_STRING	-- "Group" that this CC is in; eg directory

data CostCentre
  = NoCostCentre	-- Having this constructor avoids having
			-- to use "Maybe CostCentre" all the time.

  | NormalCC	CcKind		-- CcKind will include a cost-centre name
		Module		-- Name of module defining this CC.
		Group	  	-- "Group" that this CC is in.
		IsDupdCC	-- see below
		IsCafCC		-- see below

  | AllCafsCC	Module		-- Ditto for CAFs.
		Group	 	-- We record module and group names.
			-- Again, one "big" CAF cc per module, where all
			-- CAF costs are attributed unless the user asked for
			-- per-individual-CAF cost attribution.

  | AllDictsCC	Module		-- Ditto for dictionaries.
		Group		-- We record module and group names.
			-- Again, one "big" DICT cc per module, where all
			-- DICT costs are attributed unless the user asked for
			-- per-individual-DICT cost attribution.
		IsDupdCC -- see below

data CcKind
  = UserCC  FAST_STRING	-- Supplied by user: String is the cc name
  | AutoCC  Id		-- CC -auto-magically inserted for that Id
  | DictCC  Id

data IsDupdCC
  = AnOriginalCC	-- This says how the CC is *used*.  Saying that
  | ADupdCC		-- it is ADupdCC doesn't make it a different
			-- CC, just that it a sub-expression which has
			-- been moved ("dupd") into a different scope.
			--
			-- The point about a dupd SCC is that we don't
			-- count entries to it, because it's not the
			-- "original" one.
			--
			-- In the papers, it's called "SCCsub",
			--  i.e. SCCsub CC == SCC ADupdCC,
			-- but we are trying to avoid confusion between
			-- "subd" and "subsumed".  So we call the former
			-- "dupd".

data IsCafCC
  = IsCafCC
  | IsNotCafCC
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

isCafCCS (SingletonCCS cc)		= isCafCC cc
isCafCCS _				= False

isDictCCS (SingletonCCS cc)		= isDictCC cc
isDictCCS _				= False

currentOrSubsumedCCS SubsumedCCS	= True
currentOrSubsumedCCS CurrentCCS		= True
currentOrSubsumedCCS _			= False
\end{code}

Building cost centres

\begin{code}
mkUserCC :: FAST_STRING -> Module -> Group -> CostCentre

mkUserCC cc_name module_name group_name
  = NormalCC (UserCC cc_name) module_name group_name
	     AnOriginalCC IsNotCafCC{-might be changed-}

mkDictCC, mkAutoCC :: Id -> Module -> Group -> IsCafCC -> CostCentre

mkDictCC id module_name group_name is_caf
  = NormalCC (DictCC id) module_name group_name
	     AnOriginalCC is_caf

mkAutoCC id module_name group_name is_caf
  = NormalCC (AutoCC id) module_name group_name
	     AnOriginalCC is_caf

mkAllCafsCC  m g   = AllCafsCC  m g
mkAllDictsCC m g is_dupd
  = AllDictsCC m g (if is_dupd then ADupdCC else AnOriginalCC)

mkSingletonCCS :: CostCentre -> CostCentreStack
mkSingletonCCS cc = SingletonCCS cc

cafifyCC, dupifyCC  :: CostCentre -> CostCentre

cafifyCC cc@(AllDictsCC _ _ _) = cc -- ToDo ???
cafifyCC (NormalCC kind m g is_dupd is_caf)
  = ASSERT(not_a_calf_already is_caf)
    NormalCC kind m g is_dupd IsCafCC
  where
    not_a_calf_already IsCafCC = False
    not_a_calf_already _       = True
cafifyCC cc = pprPanic "cafifyCC" (ppr cc)

dupifyCC (AllDictsCC m g _) = AllDictsCC m g ADupdCC
dupifyCC (NormalCC kind m g is_dupd is_caf)
  = NormalCC kind m g ADupdCC is_caf
dupifyCC cc = pprPanic "dupifyCC" (ppr cc)

isEmptyCC, isCafCC, isDictCC, isDupdCC :: CostCentre -> Bool

isEmptyCC (NoCostCentre)		= True
isEmptyCC _				= False

isCafCC (AllCafsCC _ _)		   = True
isCafCC (NormalCC _ _ _ _ IsCafCC) = True
isCafCC _		           = False

isDictCC (AllDictsCC _ _ _)		= True
isDictCC (NormalCC (DictCC _) _ _ _ _)  = True
isDictCC _				= False

isDupdCC (AllDictsCC _ _ ADupdCC)   = True
isDupdCC (NormalCC _ _ _ ADupdCC _) = True
isDupdCC _		            = False

isSccCountCostCentre :: CostCentre -> Bool
  -- Is this a cost-centre which records scc counts

#if DEBUG
isSccCountCostCentre NoCostCentre  = panic "isSccCount:NoCostCentre"
#endif
isSccCountCostCentre cc | isCafCC cc  = False
                        | isDupdCC cc = False
			| isDictCC cc = True
			| otherwise   = True

sccAbleCostCentre :: CostCentre -> Bool
  -- Is this a cost-centre which can be sccd ?

#if DEBUG
sccAbleCostCentre NoCostCentre  = panic "sccAbleCC:NoCostCentre"
#endif
sccAbleCostCentre cc | isCafCC cc = False
		     | otherwise  = True

ccFromThisModule :: CostCentre -> Module -> Bool

ccFromThisModule (NormalCC _ m _ _ _) mod_name = m == mod_name
ccFromThisModule (AllCafsCC  m _)     mod_name = m == mod_name
ccFromThisModule (AllDictsCC m _ _)   mod_name = m == mod_name
\end{code}

\begin{code}
ccMentionsId :: CostCentre -> Maybe Id

ccMentionsId (NormalCC (AutoCC id) _ _ _ _) = Just id
ccMentionsId (NormalCC (DictCC id) _ _ _ _) = Just id
ccMentionsId other			    = Nothing
\end{code}

\begin{code}
instance Eq CostCentre where
	c1 == c2 = case c1 `cmpCostCentre` c2 of { EQ -> True; _ -> False }

instance Ord CostCentre where
	compare = cmpCostCentre

cmpCostCentre :: CostCentre -> CostCentre -> Ordering

cmpCostCentre (AllCafsCC  m1 _)   (AllCafsCC  m2 _)   = m1 `compare` m2
cmpCostCentre (AllDictsCC m1 _ _) (AllDictsCC m2 _ _) = m1 `compare` m2

cmpCostCentre (NormalCC k1 m1 _ _ c1) (NormalCC k2 m2 _ _ c2)
    -- first key is module name, then we use "kinds" (which include
    -- names) and finally the caf flag
  = (m1 `compare` m2) `thenCmp` (k1 `cmp_kind` k2) `thenCmp` (c1 `cmp_caf` c2)

cmpCostCentre other_1 other_2
  = let
	tag1 = tag_CC other_1
	tag2 = tag_CC other_2
    in
    if tag1 _LT_ tag2 then LT else GT
  where
    tag_CC (NormalCC _ _ _ _ _) = (ILIT(1) :: FAST_INT)
    tag_CC (AllCafsCC  _ _)   	= ILIT(2)
    tag_CC (AllDictsCC _ _ _) 	= ILIT(3)

cmp_kind (UserCC n1) (UserCC n2) = n1 `compare` n2
cmp_kind (AutoCC i1) (AutoCC i2) = i1 `compare` i2
cmp_kind (DictCC i1) (DictCC i2) = i1 `compare` i2
cmp_kind other_1     other_2
  = let
	tag1 = tag_CcKind other_1
	tag2 = tag_CcKind other_2
    in
    if tag1 _LT_ tag2 then LT else GT
  where
    tag_CcKind (UserCC _) = (ILIT(1) :: FAST_INT)
    tag_CcKind (AutoCC _) = ILIT(2)
    tag_CcKind (DictCC _) = ILIT(3)

cmp_caf IsNotCafCC IsCafCC     = LT
cmp_caf IsNotCafCC IsNotCafCC  = EQ
cmp_caf IsCafCC    IsCafCC     = EQ
cmp_caf IsCafCC    IsNotCafCC  = GT
\end{code}

-----------------------------------------------------------------------------
Printing Cost Centre Stacks.

There are two ways to print a CCS:

	- for debugging output (i.e. -ddump-whatever),
	- as a C label

\begin{code}
instance Outputable CostCentreStack where
  ppr ccs = case ccs of
		NoCCS		-> ptext SLIT("NO_CCS")
		CurrentCCS	-> ptext SLIT("CCCS")
		OverheadCCS	-> ptext SLIT("CCS_OVERHEAD")
		DontCareCCS	-> ptext SLIT("CCS_DONTZuCARE")
		SubsumedCCS	-> ptext SLIT("CCS_SUBSUMED")
		SingletonCCS cc -> 
			getPprStyle $ \sty ->
			if (codeStyle sty) 
			    then ptext SLIT("CCS_") <> 
				 ptext (identToC (costCentreStr cc))
			    else ptext SLIT("CCS.") <> text (costCentreStr cc)

pprCostCentreStackDecl :: CostCentreStack -> SDoc

pprCostCentreStackDecl ccs@(SingletonCCS cc)
  = let
       (mod_name, grp_name, is_subsumed, externally_visible) = get_cc_info cc
    in
    hcat [ ptext SLIT("CCS_DECLARE"), char '(',
    	   ppr ccs,	 	comma,	-- better be codeStyle
    	   ppCostCentreLbl cc, 	comma,
    	   ptext is_subsumed, 	comma,
    	   if externally_visible
       		then empty 
       		else ptext SLIT("static"),
    	   text ");"
	 ]

pprCostCentreStackDecl ccs 
  = pprPanic "pprCostCentreStackDecl: " (ppr ccs)
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
		else
	   if ifaceStyle sty
		then ppCostCentreIface cc
		else text (costCentreStr cc)

ppCostCentreLbl cc   = ptext SLIT("CC_") <> ptext (identToC (costCentreStr cc))
ppCostCentreIface cc = doubleQuotes (text (costCentreStr cc))
ppCostCentreName cc  = doubleQuotes (text (stringToC (costCentreName cc)))

costCentreStr (NoCostCentre)		= "NO_CC"
costCentreStr (AllCafsCC m _) 		= "CAFs."  ++ moduleString m
costCentreStr (AllDictsCC m _ d) 	= "DICTs." ++ moduleString m
costCentreStr (NormalCC kind mod_name grp_name is_dupd is_caf)
  =  case is_caf of { IsCafCC -> "CAF:";   _ -> "" }
  ++ moduleString mod_name
  ++ case kind of { UserCC name -> _UNPK_ name;
		    AutoCC id   -> getOccString id ++ "/AUTO";
		    DictCC id   -> getOccString id ++ "/DICT"
		  }
  -- ToDo: group name
  ++ case is_dupd of { ADupdCC -> "/DUPD";   _ -> "" }

-- This is the name to go in the cost centre declaration
costCentreName (NoCostCentre)		= "NO_CC"
costCentreName (AllCafsCC _ _)		= "CAFs_in_..."
costCentreName (AllDictsCC _ _ _)	= "DICTs_in_..."
costCentreName (NormalCC kind mod_name grp_name is_dupd is_caf)
  =  case is_caf of { IsCafCC -> "CAF:";   _ -> "" }
  ++ case kind of { UserCC name -> _UNPK_ name;
		    AutoCC id   -> getOccString id;
		    DictCC id   -> getOccString id
		  }
\end{code}

Cost Centre Declarations

\begin{code}
#ifdef DEBUG
pprCostCentreDecl is_local (NoCostCentre)
  = panic "pprCostCentreDecl: no cost centre!"
#endif
pprCostCentreDecl is_local cc
  = if is_local then
	hcat [
	    ptext SLIT("CC_DECLARE"),char '(',
	    cc_ident, 		  comma,
	    ppCostCentreName cc,  comma,
	    doubleQuotes (pprModule mod_name), comma,
	    doubleQuotes (ptext grp_name),     comma,
	    ptext is_subsumed,    comma,
	    if externally_visible
	       then empty 
	       else ptext SLIT("static"),
	    text ");"]
    else
	hcat [ ptext SLIT("CC_EXTERN"),char '(', cc_ident, text ");" ]
  where
    cc_ident = ppCostCentreLbl cc

    (mod_name, grp_name, is_subsumed, externally_visible)
      = get_cc_info cc


get_cc_info :: CostCentre -> 
	(Module,			-- module 
	 Group,				-- group name
	 FAST_STRING,			-- subsumed value
	 Bool)				-- externally visible
	  
get_cc_info cc
  = case cc of
	  AllCafsCC m g -> (m, g, cc_IS_CAF, True)

	  AllDictsCC m g _ -> (m, g, cc_IS_DICT, True)

	  NormalCC (DictCC i) m g is_dupd is_caf
	    -> (m, g, cc_IS_DICT, externallyVisibleId i)

	  NormalCC x m g is_dupd is_caf
	    -> (m, g, do_caf is_caf,
		case x of { UserCC _ -> True; AutoCC i -> externallyVisibleId i})
      where
	cc_IS_CAF      = SLIT("CC_IS_CAF")
	cc_IS_DICT     = SLIT("CC_IS_DICT")
	cc_IS_BORING   = SLIT("CC_IS_BORING")

	do_caf IsCafCC	     = cc_IS_CAF
	do_caf IsNotCafCC    = cc_IS_BORING
\end{code}
