%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CostCentre]{The @CostCentre@ data type}

\begin{code}
module CostCentre (
	CostCentre, CcKind, IsDupdCC{-ToDo:rm-}, IsCafCC(..),
	noCostCentre, subsumedCosts,
	useCurrentCostCentre,
	noCostCentreAttached, costsAreSubsumed, isCurrentCostCentre,
	currentOrSubsumedCosts,
	preludeCafsCostCentre, preludeDictsCostCentre,
	overheadCostCentre, dontCareCostCentre,

	mkUserCC, mkAutoCC, mkDictCC, mkAllCafsCC, mkAllDictsCC,
	cafifyCC, dupifyCC,
	isCafCC, isDictCC, isDupdCC,
	isSccCountCostCentre,
	sccAbleCostCentre,
	ccFromThisModule,
	ccMentionsId,

	uppCostCentre, uppCostCentreDecl, showCostCentre, -- printing

	cmpCostCentre	-- used for removing dups in a list
    ) where

#include "HsVersions.h"

import Id		( externallyVisibleId, GenId, showId, Id )
import CStrings		( identToC, stringToC )
import Name		( OccName, getOccString, moduleString, nameString )
import Outputable	
import Util	        ( panic, panic#, assertPanic, thenCmp )

pprIdInUnfolding = panic "Whoops"
\end{code}

\begin{code}
data CostCentre
  = NoCostCentre	-- Having this constructor avoids having
			-- to use "Maybe CostCentre" all the time.

  | NormalCC	CcKind	 -- CcKind will include a cost-centre name
		FAST_STRING	 -- Name of module defining this CC.
		FAST_STRING   -- "Group" that this CC is in.
		IsDupdCC -- see below
		IsCafCC	 -- see below

  | CurrentCC		-- Pinned on a let(rec)-bound thunk/function/constructor,
			-- this says that the cost centre to be attached to
			-- the object, when it is allocated, is whatever is in the
			-- current-cost-centre register.
			-- This guy is *never* the cost centre for an SCC construct,
			-- and is only used for *local* (non-top-level) definitions.

  | SubsumedCosts	-- Cost centre for top-level subsumed functions
			-- (CAFs get an AllCafsCC).
			-- Its execution costs get subsumed into the caller.
			-- This guy is *only* ever pinned on static closures,
			-- and is *never* the cost centre for an SCC construct.

  | AllCafsCC	FAST_STRING	-- Ditto for CAFs.
		FAST_STRING  -- We record module and group names.
			-- Again, one "big" CAF cc per module, where all
			-- CAF costs are attributed unless the user asked for
			-- per-individual-CAF cost attribution.

  | AllDictsCC	FAST_STRING	-- Ditto for dictionaries.
		FAST_STRING  -- We record module and group names.
			-- Again, one "big" DICT cc per module, where all
			-- DICT costs are attributed unless the user asked for
			-- per-individual-DICT cost attribution.
		IsDupdCC -- see below

  | OverheadCC		-- We charge costs due to the profiling-system
  			-- doing its work to "overhead".
			--
			-- Objects whose cost-centre is "Overhead"
			-- have their *allocation* charged to "overhead",
			-- but have the current CC put into the object
			-- itself.
			--
			-- For example, if we transform "f g" to "let
			-- g' = g in f g'" (so that something about
			-- profiling works better...), then we charge
			-- the *allocation* of g' to OverheadCC, but
			-- we put the cost-centre of the call to f
			-- (i.e., current CC) into the g' object.  When
			-- g' is entered, the cost-centre of the call
			-- to f will be set.

  | PreludeCafsCC	-- In compiling the prelude, we do sometimes
  | PreludeDictsCC	-- need a CC to blame; i.e., when there's a CAF,
			-- or other costs that really shouldn't be
			-- subsumed/blamed-on-the-caller.  These costs
			-- should be *small*.  We treat PreludeCafsCC
			-- as if it were shorthand for
			-- (AllCafsCC <PreludeSomething> _).  Analogously
			-- for PreludeDictsCC...
	IsDupdCC	-- see below/above

  | DontCareCC		-- We need a cost-centre to stick in static closures
			-- (for data), but we *don't* expect them to
			-- accumulate any costs.  But we still need
			-- the placeholder.  This CC is it.

data CcKind
  = UserCC  FAST_STRING	-- Supplied by user: String is the cc name
  | AutoCC  Id		-- CC -auto-magically inserted for that Id
  | DictCC  Id

data IsDupdCC
  = AnOriginalCC	-- This says how the CC is *used*.  Saying that
  | ADupdCC		-- it is ADupdCC doesn't make it a different
			-- CC, just that it a sub-expression which has
			-- been moved ("dupd") into a different scope.
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
noCostCentre  = NoCostCentre
subsumedCosts = SubsumedCosts
useCurrentCostCentre = CurrentCC
overheadCostCentre = OverheadCC
preludeCafsCostCentre = PreludeCafsCC
dontCareCostCentre = DontCareCC
preludeDictsCostCentre is_dupd
  = PreludeDictsCC (if is_dupd then ADupdCC else AnOriginalCC)

noCostCentreAttached NoCostCentre  = True
noCostCentreAttached _		   = False

isCurrentCostCentre CurrentCC = True
isCurrentCostCentre _	      = False

costsAreSubsumed SubsumedCosts	= True
costsAreSubsumed _		= False

currentOrSubsumedCosts SubsumedCosts	= True
currentOrSubsumedCosts CurrentCC	= True
currentOrSubsumedCosts _		= False

mkUserCC :: FAST_STRING -> FAST_STRING -> FAST_STRING -> CostCentre

mkUserCC cc_name module_name group_name
  = NormalCC (UserCC cc_name) module_name group_name
	     AnOriginalCC IsNotCafCC{-might be changed-}

mkDictCC, mkAutoCC :: Id -> FAST_STRING -> FAST_STRING -> IsCafCC -> CostCentre

mkDictCC id module_name group_name is_caf
  = NormalCC (DictCC id) module_name group_name
	     AnOriginalCC is_caf

mkAutoCC id module_name group_name is_caf
  = NormalCC (AutoCC id) module_name group_name
	     AnOriginalCC is_caf

mkAllCafsCC  m g   = AllCafsCC  m g
mkAllDictsCC m g is_dupd
  = AllDictsCC m g (if is_dupd then ADupdCC else AnOriginalCC)

cafifyCC, dupifyCC  :: CostCentre -> CostCentre

cafifyCC cc@(AllDictsCC _ _ _) = cc -- ToDo ???
cafifyCC cc@(PreludeDictsCC _) = cc --    ditto
cafifyCC (NormalCC kind m g is_dupd is_caf)
  = ASSERT(not_a_calf_already is_caf)
    NormalCC kind m g is_dupd IsCafCC
  where
    not_a_calf_already IsCafCC = False
    not_a_calf_already _       = True
cafifyCC cc = panic ("cafifyCC"++(showCostCentre False cc))

dupifyCC (AllDictsCC m g _) = AllDictsCC m g ADupdCC
dupifyCC (PreludeDictsCC _) = PreludeDictsCC ADupdCC
dupifyCC (NormalCC kind m g is_dupd is_caf)
  = NormalCC kind m g ADupdCC is_caf
dupifyCC cc = panic ("dupifyCC"++(showCostCentre False cc))

isCafCC, isDictCC, isDupdCC :: CostCentre -> Bool

isCafCC (AllCafsCC _ _)		   = True
isCafCC PreludeCafsCC		   = True
isCafCC (NormalCC _ _ _ _ IsCafCC) = True
isCafCC _		           = False

isDictCC (AllDictsCC _ _ _)		= True
isDictCC (PreludeDictsCC _)		= True
isDictCC (NormalCC (DictCC _) _ _ _ _)  = True
isDictCC _				= False

isDupdCC (AllDictsCC _ _ ADupdCC)   = True
isDupdCC (PreludeDictsCC ADupdCC)   = True
isDupdCC (NormalCC _ _ _ ADupdCC _) = True
isDupdCC _		            = False

isSccCountCostCentre :: CostCentre -> Bool
  -- Is this a cost-centre which records scc counts

#if DEBUG
isSccCountCostCentre NoCostCentre  = panic "isSccCount:NoCostCentre"
isSccCountCostCentre SubsumedCosts = panic "isSccCount:SubsumedCosts"
isSccCountCostCentre CurrentCC	   = panic "isSccCount:CurrentCC"
isSccCountCostCentre DontCareCC    = panic "isSccCount:DontCareCC"
#endif
isSccCountCostCentre OverheadCC       = False
isSccCountCostCentre cc | isCafCC cc  = False
                        | isDupdCC cc = False
			| isDictCC cc = True
			| otherwise   = True

sccAbleCostCentre :: CostCentre -> Bool
  -- Is this a cost-centre which can be sccd ?

#if DEBUG
sccAbleCostCentre NoCostCentre  = panic "sccAbleCC:NoCostCentre"
sccAbleCostCentre SubsumedCosts = panic "sccAbleCC:SubsumedCosts"
sccAbleCostCentre CurrentCC	= panic "sccAbleCC:CurrentCC"
sccAbleCostCentre DontCareCC	= panic "sccAbleCC:DontCareCC"
#endif
sccAbleCostCentre OverheadCC	  = False
sccAbleCostCentre cc | isCafCC cc = False
		     | otherwise  = True

ccFromThisModule :: CostCentre -> FAST_STRING{-module name-} -> Bool

ccFromThisModule (NormalCC _ m _ _ _) mod_name = m == mod_name
ccFromThisModule (AllCafsCC  m _)     mod_name = m == mod_name
ccFromThisModule (AllDictsCC m _ _)   mod_name = m == mod_name
ccFromThisModule PreludeCafsCC	      _	       = False
ccFromThisModule (PreludeDictsCC _)   _	       = False
ccFromThisModule OverheadCC	      _	       = False
ccFromThisModule DontCareCC	      _	       = False
  -- shouldn't ask about any others!
\end{code}

\begin{code}
ccMentionsId :: CostCentre -> Maybe Id

ccMentionsId (NormalCC (AutoCC id) _ _ _ _) = Just id
ccMentionsId (NormalCC (DictCC id) _ _ _ _) = Just id
ccMentionsId other			    = Nothing
\end{code}

\begin{code}
cmpCostCentre :: CostCentre -> CostCentre -> Ordering

cmpCostCentre (AllCafsCC  m1 _)   (AllCafsCC  m2 _)   = m1 `compare` m2
cmpCostCentre (AllDictsCC m1 _ _) (AllDictsCC m2 _ _) = m1 `compare` m2
cmpCostCentre PreludeCafsCC    	  PreludeCafsCC	      = EQ
cmpCostCentre (PreludeDictsCC _)  (PreludeDictsCC _)  = EQ
cmpCostCentre OverheadCC       	  OverheadCC	      = EQ
cmpCostCentre DontCareCC       	  DontCareCC	      = EQ

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
    tag_CC PreludeCafsCC      	= ILIT(4)
    tag_CC (PreludeDictsCC _) 	= ILIT(5)
    tag_CC OverheadCC	      	= ILIT(6)
    tag_CC DontCareCC	      	= ILIT(7)

    -- some BUG avoidance here...
    tag_CC NoCostCentre  = panic# "tag_CC:NoCostCentre"
    tag_CC SubsumedCosts = panic# "tag_CC:SubsumedCosts"
    tag_CC CurrentCC	 = panic# "tag_CC:SubsumedCosts"


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

\begin{code}
showCostCentre    :: Bool -> CostCentre -> String
uppCostCentre	  :: Bool -> CostCentre -> SDoc
uppCostCentreDecl :: Bool -> CostCentre -> SDoc

{- 	PprUnfolding is gone now
showCostCentre PprUnfolding print_as_string cc
  = ASSERT(not print_as_string) -- we never "print as string w/ Unfolding"
    ASSERT(not (noCostCentreAttached cc))
    ASSERT(not (currentOrSubsumedCosts cc))
    uppShow 80 (upp_cc_uf cc)
-}

showCostCentre print_as_string cc
  = showSDoc (uppCostCentre print_as_string cc)

uppCostCentre print_as_string NoCostCentre
  | print_as_string	= text "\"NO_CC\""
  | otherwise		= ptext SLIT("NO_CC")

uppCostCentre print_as_string SubsumedCosts
  | print_as_string 	= text "\"SUBSUMED\""
  | otherwise		= ptext SLIT("CC_SUBSUMED")

uppCostCentre print_as_string CurrentCC
  | print_as_string 	= text "\"CURRENT_CC\""
  | otherwise		= ptext SLIT("CCC")

uppCostCentre print_as_string OverheadCC
  | print_as_string	= text "\"OVERHEAD\""
  | otherwise		= ptext SLIT("CC_OVERHEAD")

uppCostCentre print_as_string cc
  = getPprStyle $ \ sty ->
    let
        friendly_sty = userStyle sty || debugStyle sty    -- i.e. probably for human consumption
	prefix_CC	   = ptext SLIT("CC_")
	basic_thing        = do_cc friendly_sty cc
	basic_thing_string = stringToC basic_thing
    in
    if print_as_string then
    	hcat [char '"', text basic_thing_string, char '"']

    else if friendly_sty then
	text basic_thing
    else
	hcat [prefix_CC, identToC (_PK_ basic_thing)]
  where
    do_cc friendly_sty DontCareCC	  = "DONT_CARE"
    do_cc friendly_sty (AllCafsCC  m _)   = if print_as_string
					    then "CAFs_in_..."
					    else "CAFs." ++ _UNPK_ m
    do_cc friendly_sty (AllDictsCC m _ d) = do_dupd friendly_sty d (
				            if print_as_string
					    then "DICTs_in_..."
			       		    else "DICTs." ++ _UNPK_ m)
    do_cc friendly_sty PreludeCafsCC	  = if print_as_string
					    then "CAFs_in_..."
					    else "CAFs"
    do_cc friendly_sty (PreludeDictsCC d) = do_dupd friendly_sty d (
				            if print_as_string
					    then "DICTs_in_..."
					    else "DICTs")

    do_cc friendly_sty (NormalCC kind mod_name grp_name is_dupd is_caf)
      = let
            basic_kind  = do_kind kind
	    module_kind = do_caf is_caf (moduleString mod_name ++ '/':
					       basic_kind)
            grp_str   = if (_NULL_ grp_name) then mod_name else grp_name
            full_kind = do_caf is_caf
	                 (moduleString mod_name  ++ 
			  ('/' : _UNPK_ grp_str) ++ 
			  ('/' : basic_kind))
	in
        if friendly_sty then
	   do_dupd friendly_sty is_dupd full_kind
	else
	    module_kind
      where
	do_caf IsCafCC ls = "CAF:" ++ ls
	do_caf _       ls = ls

    	do_kind (UserCC name) = _UNPK_ name
	do_kind (AutoCC id)   = do_id id ++ (if friendly_sty then "/AUTO" else "")
	do_kind (DictCC id)   = do_id id ++ (if friendly_sty then "/DICT" else "")

        {-
	 do_id is only applied in a (not print_as_string) context for local ids,
	 hence using the occurrence name is enough.
	-}
	do_id :: Id -> String
	do_id id = getOccString id

    ---------------
    do_dupd friendly_sty ADupdCC str = if friendly_sty then str ++ "/DUPD" else str
    do_dupd _	         _       str = str
\end{code}

Printing unfoldings is sufficiently weird that we do it separately.
This should only apply to CostCentres that can be ``set to'' (cf
@sccAbleCostCentre@).  That excludes CAFs and 
`overhead'---which are added at the very end---but includes dictionaries.
Dict \tr{_scc_}s may cross module boundaries to show ``scope'' info;
even if we won't ultimately do a \tr{SET_CCC} from it.
\begin{code}
upp_cc_uf (PreludeDictsCC d)
  = hsep [ptext SLIT("_PRELUDE_DICTS_CC_"), upp_dupd d]
upp_cc_uf (AllDictsCC m g d)
  = hsep [ptext SLIT("_ALL_DICTS_CC_"), 
            char '"',ptext m,char '"',
            char '"',ptext g,char '"',
            upp_dupd d]

upp_cc_uf cc@(NormalCC cc_kind m g is_dupd is_caf)
  = ASSERT(sccAbleCostCentre cc)
    hsep [pp_kind cc_kind, 
            char '"', ptext m, char '"', 
            char '"', ptext g, char '"',
	    upp_dupd is_dupd, pp_caf is_caf]
  where
    pp_kind (UserCC name) = hcat [ptext SLIT("_USER_CC_ "), char '"', ptext name, char '"']
    pp_kind (AutoCC id)   = (<>) (ptext SLIT("_AUTO_CC_ ")) (show_id id)
    pp_kind (DictCC id)	  = (<>) (ptext SLIT("_DICT_CC_ ")) (show_id id)

    show_id id = pprIdInUnfolding {-no_in_scopes-} id

    pp_caf IsCafCC    = ptext SLIT("_CAF_CC_")
    pp_caf IsNotCafCC = ptext SLIT("_N_")

#ifdef DEBUG
upp_cc_uf other = panic ("upp_cc_uf:"++(showCostCentre True other))
#endif

upp_dupd AnOriginalCC = ptext SLIT("_N_")
upp_dupd ADupdCC      = ptext SLIT("_D_")
\end{code}

\begin{code}
uppCostCentreDecl is_local cc
#ifdef DEBUG
  | noCostCentreAttached cc || currentOrSubsumedCosts cc
  = panic "uppCostCentreDecl: no cost centre!"
  | otherwise
#endif
  = if is_local then
	hcat [
	    ptext SLIT("CC_DECLARE"),char '(',
	    upp_ident, comma,
	    uppCostCentre True {-as String!-} cc, comma,
	    pp_str mod_name, comma,
	    pp_str grp_name, comma,
	    text is_subsumed, comma,
	    if externally_visible {- || all_toplev_ids_visible -}
			-- all_toplev stuff removed SLPJ Sept 97;
			-- not sure this is right.
	       then empty 
	       else ptext SLIT("static"),
	    text ");"]
    else
	hcat [ ptext SLIT("CC_EXTERN"),char '(', upp_ident, text ");" ]
  where
    upp_ident = uppCostCentre False{-as identifier!-} cc

    pp_str s  = doubleQuotes (ptext s)

    (mod_name, grp_name, is_subsumed, externally_visible)
      = case cc of
	  AllCafsCC m g -> (m, g, cc_IS_CAF, True)

	  AllDictsCC m g _ -> (m, g, cc_IS_DICT, True)

	  NormalCC (DictCC i) m g is_dupd is_caf
	    -> (m, g, cc_IS_DICT, externallyVisibleId i)

	  NormalCC x m g is_dupd is_caf
	    -> (m, g, do_caf is_caf,
		case x of { UserCC _ -> True; AutoCC i -> externallyVisibleId i})
      where
	cc_IS_CAF      = "CC_IS_CAF"
	cc_IS_DICT     = "CC_IS_DICT"
	cc_IS_SUBSUMED = "CC_IS_SUBSUMED"
	cc_IS_BORING   = "CC_IS_BORING"

	do_caf IsCafCC	     = cc_IS_CAF
	do_caf IsNotCafCC    = cc_IS_BORING
\end{code}
