%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[CostCentre]{The @CostCentre@ data type}

\begin{code}
#include "HsVersions.h"

module CostCentre (
	CostCentre, CcKind, IsDupdCC{-ToDo:rm-}, IsCafCC(..),
	noCostCentre, subsumedCosts,
	useCurrentCostCentre,
	noCostCentreAttached, costsAreSubsumed,
	currentOrSubsumedCosts,
	preludeCafsCostCentre, preludeDictsCostCentre,
	overheadCostCentre, dontCareCostCentre,

	mkUserCC, mkAutoCC, mkDictCC, mkAllCafsCC, mkAllDictsCC,
	cafifyCC, unCafifyCC, dupifyCC,
	isCafCC, isDictCC, isDupdCC,
	setToAbleCostCentre,
	ccFromThisModule,
	ccMentionsId,

	uppCostCentre, uppCostCentreDecl, showCostCentre, -- printing

	cmpCostCentre,	-- used for removing dups in a list

	Id, Maybe, Unpretty(..), CSeq
    ) where

import CmdLineOpts	( GlobalSwitch(..) )
import CLabelInfo	( identToC, stringToC )
import Id		( cmpId, showId, pprIdInUnfolding,
			  externallyVisibleId, Id
			)
import Maybes		( Maybe(..) )
import Outputable
import Pretty		( ppShow, prettyToUn )
import UniqSet
import Unpretty
import Util
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

cafifyCC, unCafifyCC, dupifyCC  :: CostCentre -> CostCentre

cafifyCC cc@(AllDictsCC _ _ _) = cc -- ???????? ToDo
cafifyCC cc@(PreludeDictsCC _) = cc -- ditto
cafifyCC (NormalCC kind m g is_dupd is_caf)
  = ASSERT(not_a_calf_already is_caf)
    NormalCC kind m g is_dupd IsCafCC
  where
    not_a_calf_already IsCafCC = False
    not_a_calf_already _       = True
cafifyCC cc = panic ("cafifyCC"++(showCostCentre PprDebug False cc))

-- WDP 95/07: pretty dodgy
unCafifyCC (NormalCC kind m g is_dupd IsCafCC) = NormalCC kind m g is_dupd IsNotCafCC
unCafifyCC (AllCafsCC _ _)	= CurrentCC
unCafifyCC PreludeCafsCC	= CurrentCC
unCafifyCC (AllDictsCC _ _ _)	= CurrentCC
unCafifyCC (PreludeDictsCC _)	= CurrentCC
unCafifyCC other_cc		= other_cc

dupifyCC (AllDictsCC m g _) = AllDictsCC m g ADupdCC
dupifyCC (PreludeDictsCC _) = PreludeDictsCC ADupdCC
dupifyCC (NormalCC kind m g is_dupd is_caf)
  = NormalCC kind m g ADupdCC is_caf
dupifyCC cc = panic ("dupifyCC"++(showCostCentre PprDebug False cc))

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

setToAbleCostCentre :: CostCentre -> Bool
  -- Is this a cost-centre to which CCC might reasonably
  -- be set?  setToAbleCostCentre is allowed to panic on
  -- "nonsense" cases, too...

#if DEBUG
setToAbleCostCentre NoCostCentre    = panic "setToAbleCC:NoCostCentre"
setToAbleCostCentre SubsumedCosts   = panic "setToAbleCC:SubsumedCosts"
setToAbleCostCentre CurrentCC	    = panic "setToAbleCC:CurrentCC"
setToAbleCostCentre DontCareCC	    = panic "setToAbleCC:DontCareCC"
#endif

setToAbleCostCentre OverheadCC	    = False -- see comments in type defn
setToAbleCostCentre other	    = not (isCafCC other || isDictCC other)

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
cmpCostCentre :: CostCentre -> CostCentre -> TAG_

cmpCostCentre (AllCafsCC  m1 _)   (AllCafsCC  m2 _)   = _CMP_STRING_ m1 m2
cmpCostCentre (AllDictsCC m1 _ _) (AllDictsCC m2 _ _) = _CMP_STRING_ m1 m2
cmpCostCentre PreludeCafsCC    	  PreludeCafsCC	      = EQ_
cmpCostCentre (PreludeDictsCC _)  (PreludeDictsCC _)  = EQ_
cmpCostCentre OverheadCC       	  OverheadCC	      = EQ_
cmpCostCentre DontCareCC       	  DontCareCC	      = EQ_

cmpCostCentre (NormalCC k1 m1 _ _ c1) (NormalCC k2 m2 _ _ c2)
    -- first key is module name, then we use "kinds" (which include
    -- names)
  = case (_CMP_STRING_ m1 m2) of
      LT_  -> LT_
      EQ_  -> cmp_kind k1 k2
      GT__ -> GT_

cmpCostCentre other_1 other_2
  = let
	tag1 = tag_CC other_1
	tag2 = tag_CC other_2
    in
    if tag1 _LT_ tag2 then LT_ else GT_
  where
    tag_CC (NormalCC _ _ _ _ _) = (ILIT(1) :: FAST_INT)
    tag_CC (AllCafsCC  _ _)   	= ILIT(2)
    tag_CC (AllDictsCC _ _ _) 	= ILIT(3)
    tag_CC PreludeCafsCC      	= ILIT(4)
    tag_CC (PreludeDictsCC _) 	= ILIT(5)
    tag_CC OverheadCC	      	= ILIT(6)
    tag_CC DontCareCC	      	= ILIT(7)

    -- some BUG avoidance here...
    tag_CC NoCostCentre  = case (panic "tag_CC:NoCostCentre")  of { c -> tag_CC c }
    tag_CC SubsumedCosts = case (panic "tag_CC:SubsumedCosts") of { c -> tag_CC c }
    tag_CC CurrentCC	 = case (panic "tag_CC:SubsumedCosts") of { c -> tag_CC c }


cmp_kind (UserCC n1) (UserCC n2) = _CMP_STRING_ n1 n2
cmp_kind (AutoCC i1) (AutoCC i2) = cmpId i1 i2
cmp_kind (DictCC i1) (DictCC i2) = cmpId i1 i2
cmp_kind other_1     other_2
  = let
	tag1 = tag_CcKind other_1
	tag2 = tag_CcKind other_2
    in
    if tag1 _LT_ tag2 then LT_ else GT_
  where
    tag_CcKind (UserCC _) = (ILIT(1) :: FAST_INT)
    tag_CcKind (AutoCC _) = ILIT(2)
    tag_CcKind (DictCC _) = ILIT(3)
\end{code}

\begin{code}
showCostCentre    :: PprStyle -> Bool -> CostCentre -> String
uppCostCentre	  :: PprStyle -> Bool -> CostCentre -> Unpretty
uppCostCentreDecl :: PprStyle -> Bool -> CostCentre -> Unpretty

showCostCentre (PprUnfolding _) print_as_string cc
  = ASSERT(not print_as_string) -- we never "print as string w/ Unfolding"
    ASSERT(not (noCostCentreAttached cc))
    ASSERT(not (currentOrSubsumedCosts cc))
    uppShow 80 (upp_cc_uf cc)

showCostCentre sty print_as_string cc
  = uppShow 80 (uppCostCentre sty print_as_string cc)

uppCostCentre sty print_as_string NoCostCentre
  | friendly_style sty	= uppNil
  | print_as_string	= uppStr "\"NO_CC\""
  | otherwise		= uppPStr SLIT("NO_CC")

uppCostCentre sty print_as_string SubsumedCosts
  | print_as_string 	= uppStr "\"SUBSUMED\""
  | otherwise		= uppPStr SLIT("CC_SUBSUMED")

uppCostCentre sty print_as_string CurrentCC
  | print_as_string 	= uppStr "\"CURRENT_CC\""
  | otherwise		= uppPStr SLIT("CCC")

uppCostCentre sty print_as_string OverheadCC
  | print_as_string	= uppStr "\"OVERHEAD\""
  | otherwise		= uppPStr SLIT("CC_OVERHEAD")

uppCostCentre sty print_as_string cc
  = let
	prefix_CC = uppPStr SLIT("CC_")

	basic_thing -- (basic_thing, suffix_CAF)
	  = do_cc cc

	basic_thing_string
	  = if friendly_sty then basic_thing else stringToC basic_thing
    in
    if print_as_string then
    	uppBesides [uppChar '"', uppStr basic_thing_string, uppChar '"']

    else if friendly_sty then
	uppStr basic_thing
    else
	uppBesides [prefix_CC,
		    prettyToUn (identToC (_PK_ basic_thing))]
  where
    friendly_sty = friendly_style sty

    add_module_name_maybe m str
      = if print_as_string then str else (str ++ ('.' : m))

    ----------------
    do_cc OverheadCC	     = "OVERHEAD"
    do_cc DontCareCC	     = "DONT_CARE"
    do_cc (AllCafsCC  m _)   = if print_as_string
			       then "CAFs_in_..."
			       else "CAFs." ++ _UNPK_ m
    do_cc (AllDictsCC m _ d) = do_dupd d (
			       if print_as_string
			       then "DICTs_in_..."
			       else "DICTs." ++ _UNPK_ m)
    do_cc PreludeCafsCC	     = if print_as_string
			       then "CAFs_in_..."
			       else "CAFs"
    do_cc (PreludeDictsCC d) = do_dupd d (
			       if print_as_string
			       then "DICTs_in_..."
			       else "DICTs")

    do_cc (NormalCC kind mod_name grp_name is_dupd is_caf)
      = let
	    basic_kind = do_kind kind
	    is_a_calf  = do_calved is_caf
	in
	if friendly_sty then
	    do_dupd is_dupd (basic_kind ++ ('/': _UNPK_ mod_name) ++ ('/': _UNPK_ grp_name) ++ is_a_calf)
	else
	    basic_kind
      where
    	do_kind (UserCC name) = _UNPK_ name
	do_kind (AutoCC id)   = do_id id ++ (if friendly_sty then "/AUTO" else "")
	do_kind (DictCC id)   = do_id id ++ (if friendly_sty then "/DICT" else "")

	do_id :: Id -> String
	do_id id
	  = if print_as_string
	    then _UNPK_ (getOccurrenceName id) -- don't want module in the name
	    else showId sty id	      -- we really do

	do_calved IsCafCC = "/CAF"
	do_calved _   	  = ""

    ---------------
    do_dupd ADupdCC str = if friendly_sty then str ++ "/DUPD" else str
    do_dupd _	    str = str

friendly_style sty -- i.e., probably for human consumption
  = case sty of
      PprForUser -> True
      PprDebug   -> True
      PprShowAll -> True
      _ 	 -> False
\end{code}

Printing unfoldings is sufficiently weird that we do it separately.
This should only apply to CostCentres that can be ``set to'' (cf
@setToAbleCostCentre@).  That excludes CAFs and 
`overhead'---which are added at the very end---but includes dictionaries.
Dict \tr{_scc_}s may cross module boundaries to show ``scope'' info;
even if we won't ultimately do a \tr{SET_CCC} from it.
\begin{code}
upp_cc_uf (PreludeDictsCC d)
  = uppCat [uppPStr SLIT("_PRELUDE_DICTS_CC_"), upp_dupd d]
upp_cc_uf (AllDictsCC m g d)
  = uppCat [uppPStr SLIT("_ALL_DICTS_CC_"), uppStr (show (_UNPK_ m)), uppStr (show (_UNPK_ g)), upp_dupd d]

upp_cc_uf cc@(NormalCC cc_kind m g is_dupd is_caf)
  = ASSERT(isDictCC cc || setToAbleCostCentre cc)
    uppCat [pp_kind cc_kind, uppStr (show (_UNPK_ m)), uppStr (show (_UNPK_ g)),
	    upp_dupd is_dupd, pp_caf is_caf]
  where
    pp_kind (UserCC name) = uppBeside (uppPStr SLIT("_USER_CC_ ")) (uppStr (show (_UNPK_ name)))
    pp_kind (AutoCC id)   = uppBeside (uppPStr SLIT("_AUTO_CC_ ")) (show_id id)
    pp_kind (DictCC id)	  = uppBeside (uppPStr SLIT("_DICT_CC_ ")) (show_id id)

    show_id id = prettyToUn (pprIdInUnfolding no_in_scopes id)
	where
	  no_in_scopes = emptyUniqSet

    pp_caf IsCafCC    = uppPStr SLIT("_CAF_CC_")
    pp_caf IsNotCafCC = uppPStr SLIT("_N_")

#ifdef DEBUG
upp_cc_uf other = panic ("upp_cc_uf:"++(showCostCentre PprDebug True other))
#endif

upp_dupd AnOriginalCC = uppPStr SLIT("_N_")
upp_dupd ADupdCC      = uppPStr SLIT("_DUPD_CC_")
\end{code}

\begin{code}
uppCostCentreDecl sty is_local cc
#ifdef DEBUG
  | noCostCentreAttached cc || currentOrSubsumedCosts cc
  = panic "uppCostCentreDecl: no cost centre!"
  | otherwise
#endif
  = if is_local then
	uppBesides [
	    uppStr "CC_DECLARE(",
	    upp_ident, uppComma,
	    uppCostCentre sty True {-as String!-} cc, uppComma,
	    pp_str mod_name, uppComma,
	    pp_str grp_name, uppComma,
	    uppStr is_subsumed, uppComma,
	    if externally_visible then uppNil else uppPStr SLIT("static"),
	    uppStr ");"]
    else
	uppBesides [ uppStr "CC_EXTERN(", upp_ident, uppStr ");" ]
  where
    upp_ident = uppCostCentre sty False{-as identifier!-} cc

    pp_str s  = uppBeside (uppPStr (_CONS_ '"'  s))  (uppChar '"')
    pp_char c = uppBeside (uppPStr (_CONS_ '\'' c)) (uppChar '\'')

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
