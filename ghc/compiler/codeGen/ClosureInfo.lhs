%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: ClosureInfo.lhs,v 1.58 2003/06/09 13:17:38 matthewc Exp $
%
\section[ClosureInfo]{Data structures which describe closures}

Much of the rationale for these things is in the ``details'' part of
the STG paper.

\begin{code}
module ClosureInfo (
	ClosureInfo, LambdaFormInfo, SMRep, 	-- all abstract
	StandardFormInfo, ArgDescr(..),

	CallingConvention(..),

	mkClosureLFInfo, mkConLFInfo, mkSelectorLFInfo,
	mkApLFInfo, mkLFImported, mkLFArgument, mkLFLetNoEscape,
	UpdateFlag,

	closureSize, closureNonHdrSize,
	closureGoodStuffSize, closurePtrsSize,
	slopSize,

	layOutDynClosure, layOutStaticClosure, layOutStaticNoFVClosure,
	layOutDynConstr, layOutStaticConstr,
	mkVirtHeapOffsets, mkStaticClosure,

	nodeMustPointToIt, getEntryConvention, 
	FCode, CgInfoDownwards, CgState, 

	blackHoleOnEntry,

	staticClosureRequired,

	closureName, infoTableLabelFromCI,
	closureLabelFromCI, closureSRT,
	entryLabelFromCI, 
	closureLFInfo, closureSMRep, closureUpdReqd,
	closureSingleEntry, closureReEntrant, closureSemiTag,
	closureFunInfo,	isStandardFormThunk,
	GenStgArg,

	isToplevClosure,
	closureTypeDescr,		-- profiling

	isStaticClosure,
	allocProfilingMsg,
	cafBlackHoleClosureInfo, seCafBlackHoleClosureInfo,

	staticClosureNeedsLink,

	mkInfoTable, mkRetInfoTable, mkVecInfoTable,
    ) where

#include "../includes/config.h"
#include "../includes/MachDeps.h"
#include "HsVersions.h"

import AbsCSyn		
import StgSyn
import CgMonad

import Constants	( mIN_UPD_SIZE, mIN_SIZE_NonUpdHeapObject )
import CgRetConv	( assignRegs )
import CLabel
import CmdLineOpts	( opt_SccProfilingOn, opt_OmitBlackHoling,
			  opt_Parallel, opt_DoTickyProfiling,
			  opt_SMP, opt_Unregisterised )
import Id		( Id, idType, idArity, idName, idPrimRep )
import DataCon		( DataCon, dataConTag, fIRST_TAG, dataConTyCon,
			  isNullaryDataCon, dataConName
			)
import Name		( Name, nameUnique, getOccName, getName )
import OccName		( occNameUserString )
import PprType		( getTyDescription )
import PrimRep
import SMRep		-- all of it
import Type		( isUnLiftedType, Type, repType, splitTyConApp_maybe )
import TyCon		( isFunTyCon )
import BasicTypes	( TopLevelFlag(..), isNotTopLevel, isTopLevel )
import Util		( mapAccumL, listLengthCmp, lengthIs )
import FastString
import Outputable
import Literal
import Constants
import Bitmap

import Maybe		( isJust )
import DATA_BITS
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-datatypes]{Data types for closure information}
%*									*
%************************************************************************

Information about a closure, from the code generator's point of view.

A ClosureInfo decribes the info pointer of a closure.  It has
enough information 
  a) to construct the info table itself
  b) to allocate a closure containing that info pointer (i.e.
	it knows the info table label)

We make a ClosureInfo for
	- each let binding (both top level and not)
	- each data constructor (for its shared static and
		dynamic info tables)

\begin{code}
data ClosureInfo
  = ClosureInfo {
	closureName   :: !Name,		  -- The thing bound to this closure
	closureLFInfo :: !LambdaFormInfo, -- NOTE: not an LFCon (see below)
	closureSMRep  :: !SMRep,	  -- representation used by storage mgr
	closureSRT    :: !C_SRT,	  -- What SRT applies to this closure
	closureType   :: !Type,		  -- Type of closure (ToDo: remove)
	closureDescr  :: !String	  -- closure description (for profiling)
    }

  -- constructor closures don't have a unique info table label (they use
  -- the constructor's info table), and they don't have an SRT.
  | ConInfo {
	closureCon       :: !DataCon,
	closureSMRep     :: !SMRep
    }
\end{code}

%************************************************************************
%*									*
\subsubsection[LambdaFormInfo-datatype]{@LambdaFormInfo@: source-derivable info}
%*									*
%************************************************************************

Information about an identifier, from the code generator's point of
view.  Every identifier is bound to a LambdaFormInfo in the
environment, which gives the code generator enough info to be able to
tail call or return that identifier.

Note that a closure is usually bound to an identifier, so a
ClosureInfo contains a LambdaFormInfo.

\begin{code}
data LambdaFormInfo
  = LFReEntrant		-- Reentrant closure (a function)
	TopLevelFlag	-- True if top level
	!Int		-- Arity
	!Bool		-- True <=> no fvs
	ArgDescr	-- Argument descriptor (should reall be in ClosureInfo)

  | LFCon		-- Constructor
	DataCon		-- The constructor

  | LFThunk		-- Thunk (zero arity)
	TopLevelFlag
	!Bool		-- True <=> no free vars
	!Bool		-- True <=> updatable (i.e., *not* single-entry)
	StandardFormInfo
	!Bool		-- True <=> *might* be a function type

  | LFUnknown		-- Used for function arguments and imported things.
			--  We know nothing about  this closure.  Treat like
			-- updatable "LFThunk"...
			-- Imported things which we do know something about use
			-- one of the other LF constructors (eg LFReEntrant for
			-- known functions)
	!Bool		-- True <=> *might* be a function type

  | LFLetNoEscape	-- See LetNoEscape module for precise description of
			-- these "lets".
	!Int		-- arity;

  | LFBlackHole		-- Used for the closures allocated to hold the result
			-- of a CAF.  We want the target of the update frame to
			-- be in the heap, so we make a black hole to hold it.
        CLabel          -- Flavour (info label, eg CAF_BLACKHOLE_info).


data StandardFormInfo	-- Tells whether this thunk has one of a small number
			-- of standard forms

  = NonStandardThunk	-- No, it isn't

  | SelectorThunk
       Int             	-- 0-origin offset of ak within the "goods" of 
			-- constructor (Recall that the a1,...,an may be laid
			-- out in the heap in a non-obvious order.)

{- A SelectorThunk is of form

     case x of
       con a1,..,an -> ak

   and the constructor is from a single-constr type.
-}

  | ApThunk 
	Int		-- arity

{- An ApThunk is of form

	x1 ... xn

   The code for the thunk just pushes x2..xn on the stack and enters x1.
   There are a few of these (for 1 <= n <= MAX_SPEC_AP_SIZE) pre-compiled
   in the RTS to save space.
-}

\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-construction]{Functions which build LFInfos}
%*									*
%************************************************************************

@mkClosureLFInfo@ figures out the appropriate LFInfo for the closure.

\begin{code}
mkClosureLFInfo :: Id		-- The binder
		-> TopLevelFlag	-- True of top level
		-> [Id]		-- Free vars
		-> UpdateFlag 	-- Update flag
		-> [Id] 	-- Args
		-> LambdaFormInfo

mkClosureLFInfo bndr top fvs upd_flag args@(_:_) -- Non-empty args
  = LFReEntrant top (length args) (null fvs) (mkArgDescr (getName bndr) args)

mkClosureLFInfo bndr top fvs upd_flag []
  = ASSERT( not updatable || not (isUnLiftedType id_ty) )
    LFThunk top (null fvs) updatable NonStandardThunk 
	(might_be_a_function id_ty)
  where
	updatable = isUpdatable upd_flag
	id_ty = idType bndr

might_be_a_function :: Type -> Bool
might_be_a_function ty
  | Just (tc,_) <- splitTyConApp_maybe (repType ty), 
    not (isFunTyCon tc) = False
  | otherwise = True
\end{code}

@mkConLFInfo@ is similar, for constructors.

\begin{code}
mkConLFInfo :: DataCon -> LambdaFormInfo
mkConLFInfo con = LFCon con

mkSelectorLFInfo id offset updatable
  = LFThunk NotTopLevel False updatable (SelectorThunk offset) 
	(might_be_a_function (idType id))

mkApLFInfo id upd_flag arity
  = LFThunk NotTopLevel (arity == 0) (isUpdatable upd_flag) (ApThunk arity)
	(might_be_a_function (idType id))
\end{code}

Miscellaneous LF-infos.

\begin{code}
mkLFArgument id = LFUnknown (might_be_a_function (idType id))

mkLFLetNoEscape = LFLetNoEscape

mkLFImported :: Id -> LambdaFormInfo
mkLFImported id
  = case idArity id of
      n | n > 0 -> LFReEntrant TopLevel n True (panic "arg_descr")  -- n > 0
      other -> mkLFArgument id -- Not sure of exact arity
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-sizes]{Functions about closure {\em sizes}}
%*									*
%************************************************************************

\begin{code}
closureSize :: ClosureInfo -> HeapOffset
closureSize cl_info = fixedHdrSize + closureNonHdrSize cl_info

closureNonHdrSize :: ClosureInfo -> Int
closureNonHdrSize cl_info
  = tot_wds + computeSlopSize tot_wds 
			      (closureSMRep cl_info)
			      (closureNeedsUpdSpace cl_info) 
  where
    tot_wds = closureGoodStuffSize cl_info

-- we leave space for an update if either (a) the closure is updatable
-- or (b) it is a static thunk.  This is because a static thunk needs
-- a static link field in a predictable place (after the slop), regardless
-- of whether it is updatable or not.
closureNeedsUpdSpace (ClosureInfo { closureLFInfo = 
					LFThunk TopLevel _ _ _ _ }) = True
closureNeedsUpdSpace cl_info = closureUpdReqd cl_info

slopSize :: ClosureInfo -> Int
slopSize cl_info
  = computeSlopSize (closureGoodStuffSize cl_info)
		    (closureSMRep cl_info)
		    (closureNeedsUpdSpace cl_info)

closureGoodStuffSize :: ClosureInfo -> Int
closureGoodStuffSize cl_info
  = let (ptrs, nonptrs) = sizes_from_SMRep (closureSMRep cl_info)
    in	ptrs + nonptrs

closurePtrsSize :: ClosureInfo -> Int
closurePtrsSize cl_info
  = let (ptrs, _) = sizes_from_SMRep (closureSMRep cl_info)
    in	ptrs

-- not exported:
sizes_from_SMRep :: SMRep -> (Int,Int)
sizes_from_SMRep (GenericRep _ ptrs nonptrs _)   = (ptrs, nonptrs)
sizes_from_SMRep BlackHoleRep			 = (0, 0)
\end{code}

Computing slop size.  WARNING: this looks dodgy --- it has deep
knowledge of what the storage manager does with the various
representations...

Slop Requirements:
\begin{itemize}
\item
Updateable closures must be @mIN_UPD_SIZE@.
	\begin{itemize}
	\item
	Indirections require 1 word
	\item
	Appels collector indirections 2 words
	\end{itemize}
THEREFORE: @mIN_UPD_SIZE = 2@.

\item
Collectable closures which are allocated in the heap
must be	@mIN_SIZE_NonUpdHeapObject@.

Copying collector forward pointer requires 1 word

THEREFORE: @mIN_SIZE_NonUpdHeapObject = 1@
\end{itemize}

Static closures have an extra ``static link field'' at the end, but we
don't bother taking that into account here.

\begin{code}
computeSlopSize :: Int -> SMRep -> Bool -> Int

computeSlopSize tot_wds (GenericRep _ _ _ _) True		-- Updatable
  = max 0 (mIN_UPD_SIZE - tot_wds)

computeSlopSize tot_wds (GenericRep True _ _ _) False	-- Non updatable
  = 0							-- Static

computeSlopSize tot_wds (GenericRep False _ _ _) False	-- Non updatable
  = max 0 (mIN_SIZE_NonUpdHeapObject - tot_wds)		-- Dynamic

computeSlopSize tot_wds BlackHoleRep _			-- Updatable
  = max 0 (mIN_UPD_SIZE - tot_wds)
\end{code}

%************************************************************************
%*									*
\subsection[layOutDynClosure]{Lay out a closure}
%*									*
%************************************************************************

\begin{code}
layOutDynClosure, layOutStaticClosure
	:: Id			    -- STG identifier of this closure
	-> (a -> PrimRep)    	    -- how to get a PrimRep for the fields
	-> [a]			    -- the "things" being layed out
	-> LambdaFormInfo	    -- what sort of closure it is
	-> C_SRT		    -- its SRT
	-> String		    -- closure description
	-> (ClosureInfo,	    -- info about the closure
	    [(a, VirtualHeapOffset)])	-- things w/ offsets pinned on them

layOutDynClosure    = layOutClosure False
layOutStaticClosure = layOutClosure True

layOutStaticNoFVClosure id lf_info srt_info descr
  = fst (layOutClosure True id (panic "kind_fn") [] lf_info srt_info descr)

layOutClosure
 	:: Bool			    -- True <=> static closure
	-> Id			    -- STG identifier of this closure
	-> (a -> PrimRep)    	    -- how to get a PrimRep for the fields
	-> [a]			    -- the "things" being layed out
	-> LambdaFormInfo	    -- what sort of closure it is
	-> C_SRT		    -- its SRT
	-> String		    -- closure description
	-> (ClosureInfo,	    -- info about the closure
	    [(a, VirtualHeapOffset)])	-- things w/ offsets pinned on them

layOutClosure is_static id kind_fn things lf_info srt_info descr
  = (ClosureInfo { closureName = name, 
		   closureLFInfo = lf_info,
		   closureSMRep = sm_rep, 
		   closureSRT = srt_info,
		   closureType = idType id,
		   closureDescr = descr },
     things_w_offsets)
  where
    name = idName id
    (tot_wds,		 -- #ptr_wds + #nonptr_wds
     ptr_wds,		 -- #ptr_wds
     things_w_offsets) = mkVirtHeapOffsets kind_fn things
    sm_rep = chooseSMRep is_static lf_info tot_wds ptr_wds


layOutDynConstr, layOutStaticConstr
	:: DataCon 	
	-> (a -> PrimRep)
	-> [a]
	-> (ClosureInfo,
	    [(a,VirtualHeapOffset)])

layOutDynConstr    = layOutConstr False
layOutStaticConstr = layOutConstr True

layOutConstr is_static data_con kind_fn args
   = (ConInfo { closureSMRep = sm_rep,
		closureCon = data_con },
      things_w_offsets)
  where
    (tot_wds,		 -- #ptr_wds + #nonptr_wds
     ptr_wds,		 -- #ptr_wds
     things_w_offsets) = mkVirtHeapOffsets kind_fn args
    sm_rep = chooseSMRep is_static (mkConLFInfo data_con) tot_wds ptr_wds
\end{code}

%************************************************************************
%*									*
\subsection[mkStaticClosure]{Make a static closure}
%*									*
%************************************************************************

Make a static closure, adding on any extra padding needed for CAFs,
and adding a static link field if necessary.

\begin{code}
mkStaticClosure lbl cl_info ccs fields cafrefs
  | opt_SccProfilingOn =
	     CStaticClosure
		lbl
		cl_info
	    	(mkCCostCentreStack ccs)
		all_fields
  | otherwise =
	     CStaticClosure
		lbl
		cl_info
	    	(panic "absent cc")
		all_fields

   where
    all_fields = fields ++ padding_wds ++ static_link_field

    upd_reqd = closureUpdReqd cl_info

    -- for the purposes of laying out the static closure, we consider all
    -- thunks to be "updatable", so that the static link field is always
    -- in the same place.
    padding_wds
	| not upd_reqd = []
	| otherwise    = replicate n (mkIntCLit 0) -- a bunch of 0s
	where n = max 0 (mIN_UPD_SIZE - length fields)

	-- We always have a static link field for a thunk, it's used to
	-- save the closure's info pointer when we're reverting CAFs
	-- (see comment in Storage.c)
    static_link_field
	| upd_reqd || staticClosureNeedsLink cl_info = [static_link_value]
	| otherwise 	     		             = []

	-- for a static constructor which has NoCafRefs, we set the
	-- static link field to a non-zero value so the garbage
	-- collector will ignore it.
    static_link_value
	| cafrefs	= mkIntCLit 0
	| otherwise	= mkIntCLit 1
\end{code}

%************************************************************************
%*									*
\subsection[SMreps]{Choosing SM reps}
%*									*
%************************************************************************

\begin{code}
chooseSMRep
	:: Bool			-- True <=> static closure
	-> LambdaFormInfo
	-> Int -> Int		-- Tot wds, ptr wds
	-> SMRep

chooseSMRep is_static lf_info tot_wds ptr_wds
  = let
	 nonptr_wds   = tot_wds - ptr_wds
	 closure_type = getClosureType is_static tot_wds ptr_wds lf_info
    in
    GenericRep is_static ptr_wds nonptr_wds closure_type	

-- we *do* get non-updatable top-level thunks sometimes.  eg. f = g
-- gets compiled to a jump to g (if g has non-zero arity), instead of
-- messing around with update frames and PAPs.  We set the closure type
-- to FUN_STATIC in this case.

getClosureType :: Bool -> Int -> Int -> LambdaFormInfo -> ClosureType
getClosureType is_static tot_wds ptr_wds lf_info
  = case lf_info of
	LFCon con | is_static && ptr_wds == 0	-> ConstrNoCaf
		  | otherwise			-> Constr
  	LFReEntrant _ _ _ _ 			-> Fun
	LFThunk _ _ _ (SelectorThunk _) _ 	-> ThunkSelector
	LFThunk _ _ _ _ _ 			-> Thunk
	_ -> panic "getClosureType"
\end{code}

%************************************************************************
%*									*
\subsection[mkVirtHeapOffsets]{Assigning heap offsets in a closure}
%*									*
%************************************************************************

@mkVirtHeapOffsets@ (the heap version) always returns boxed things with
smaller offsets than the unboxed things, and furthermore, the offsets in
the result list

\begin{code}
mkVirtHeapOffsets :: 
	  (a -> PrimRep)	-- To be able to grab kinds;
				--  	w/ a kind, we can find boxedness
	  -> [a]		-- Things to make offsets for
	  -> (Int,		-- *Total* number of words allocated
	      Int,		-- Number of words allocated for *pointers*
	      [(a, VirtualHeapOffset)])
				-- Things with their offsets from start of 
				--  object in order of increasing offset

-- First in list gets lowest offset, which is initial offset + 1.

mkVirtHeapOffsets kind_fun things
  = let (ptrs, non_ptrs)    	      = separateByPtrFollowness kind_fun things
    	(wds_of_ptrs, ptrs_w_offsets) = mapAccumL computeOffset 0 ptrs
	(tot_wds, non_ptrs_w_offsets) = mapAccumL computeOffset wds_of_ptrs non_ptrs
    in
	(tot_wds, wds_of_ptrs, ptrs_w_offsets ++ non_ptrs_w_offsets)
  where
    computeOffset wds_so_far thing
      = (wds_so_far + (getPrimRepSize . kind_fun) thing,
	 (thing, fixedHdrSize + wds_so_far)
	)
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-4-questions]{Four major questions about @ClosureInfo@}
%*									*
%************************************************************************

Be sure to see the stg-details notes about these...

\begin{code}
nodeMustPointToIt :: LambdaFormInfo -> FCode Bool
nodeMustPointToIt lf_info

  = case lf_info of
	LFReEntrant top _ no_fvs _ -> returnFC (
	    not no_fvs ||   -- Certainly if it has fvs we need to point to it
	    isNotTopLevel top
		    -- If it is not top level we will point to it
		    --   We can have a \r closure with no_fvs which
		    --   is not top level as special case cgRhsClosure
		    --   has been dissabled in favour of let floating

		-- For lex_profiling we also access the cost centre for a
		-- non-inherited function i.e. not top level
		-- the  not top  case above ensures this is ok.
	    )

	LFCon _ -> returnFC True

	-- Strictly speaking, the above two don't need Node to point
	-- to it if the arity = 0.  But this is a *really* unlikely
	-- situation.  If we know it's nil (say) and we are entering
	-- it. Eg: let x = [] in x then we will certainly have inlined
	-- x, since nil is a simple atom.  So we gain little by not
	-- having Node point to known zero-arity things.  On the other
	-- hand, we do lose something; Patrick's code for figuring out
	-- when something has been updated but not entered relies on
	-- having Node point to the result of an update.  SLPJ
	-- 27/11/92.

	LFThunk _ no_fvs updatable NonStandardThunk _
	  -> returnFC (updatable || not no_fvs || opt_SccProfilingOn)

	  -- For the non-updatable (single-entry case):
	  --
	  -- True if has fvs (in which case we need access to them, and we
	  --		    should black-hole it)
	  -- or profiling (in which case we need to recover the cost centre
	  --		 from inside it)

	LFThunk _ no_fvs updatable some_standard_form_thunk _
	  -> returnFC True
	  -- Node must point to any standard-form thunk.

	LFUnknown _   -> returnFC True
	LFBlackHole _ -> returnFC True
		    -- BH entry may require Node to point

	LFLetNoEscape _ -> returnFC False
\end{code}

The entry conventions depend on the type of closure being entered,
whether or not it has free variables, and whether we're running
sequentially or in parallel.

\begin{tabular}{lllll}
Closure Characteristics & Parallel & Node Req'd & Argument Passing & Enter Via \\
Unknown 			& no & yes & stack	& node \\
Known fun ($\ge$ 1 arg), no fvs 	& no & no  & registers 	& fast entry (enough args) \\
\ & \ & \ & \ 						& slow entry (otherwise) \\
Known fun ($\ge$ 1 arg), fvs	& no & yes & registers 	& fast entry (enough args) \\
0 arg, no fvs @\r,\s@ 		& no & no  & n/a 	& direct entry \\
0 arg, no fvs @\u@ 		& no & yes & n/a 	& node \\
0 arg, fvs @\r,\s@ 		& no & yes & n/a 	& direct entry \\
0 arg, fvs @\u@ 		& no & yes & n/a 	& node \\

Unknown 			& yes & yes & stack	& node \\
Known fun ($\ge$ 1 arg), no fvs 	& yes & no  & registers & fast entry (enough args) \\
\ & \ & \ & \ 						& slow entry (otherwise) \\
Known fun ($\ge$ 1 arg), fvs	& yes & yes & registers & node \\
0 arg, no fvs @\r,\s@ 		& yes & no  & n/a 	& direct entry \\
0 arg, no fvs @\u@ 		& yes & yes & n/a 	& node \\
0 arg, fvs @\r,\s@ 		& yes & yes & n/a 	& node \\
0 arg, fvs @\u@ 		& yes & yes & n/a 	& node\\
\end{tabular}

When black-holing, single-entry closures could also be entered via node
(rather than directly) to catch double-entry.

\begin{code}
data CallingConvention
  = EnterIt				-- no args, not a function

  | JumpToIt CLabel			-- no args, not a function, but we
					-- know what its entry code is

  | ReturnIt				-- it's a function, but we have
					-- zero args to apply to it, so just
					-- return it.

  | SlowCall				-- Unknown fun, or known fun with
					-- too few args.

  | DirectEntry 			-- Jump directly, with args in regs
	CLabel 				--   The code label
	Int 				--   Its arity
	[MagicId]			--   Its register assignments 
					--	(possibly empty)

getEntryConvention :: Name		-- Function being applied
		   -> LambdaFormInfo	-- Its info
		   -> [PrimRep]		-- Available arguments
		   -> FCode CallingConvention

getEntryConvention name lf_info arg_kinds
 =  nodeMustPointToIt lf_info	`thenFC` \ node_points ->
    returnFC (

    -- if we're parallel, then we must always enter via node.  The reason
    -- is that the closure may have been fetched since we allocated it.

    if (node_points && opt_Parallel) then EnterIt else

    -- Commented out by SDM after futher thoughts:
    --   - the only closure type that can be blackholed is a thunk
    --   - we already enter thunks via node (unless the closure is
    --     non-updatable, in which case why is it being re-entered...)

    case lf_info of

	LFReEntrant _ arity _ _ ->
	    if null arg_kinds then
		if arity == 0 then
		   EnterIt		-- a non-updatable thunk
	    	else 
		   ReturnIt		-- no args at all
	    else if listLengthCmp arg_kinds arity == LT then
		SlowCall		-- not enough args
	    else
		DirectEntry (mkEntryLabel name) arity arg_regs
	  where
	    (arg_regs, _) = assignRegs [node] (take arity arg_kinds)
		-- we don't use node to pass args now (SDM)

	LFCon con
	    | isNullaryDataCon con
	      -- a real constructor.  Don't bother entering it, just jump
	      -- to the constructor entry code directly.
			  -> --false:ASSERT (null arg_kinds)	
			     -- Should have no args (meaning what?)
			     JumpToIt (mkStaticConEntryLabel (dataConName con))

	     | otherwise {- not nullary -}
			  -> --false:ASSERT (null arg_kinds)	
			     -- Should have no args (meaning what?)
			     JumpToIt (mkConEntryLabel (dataConName con))

	LFThunk _ _ updatable std_form_info is_fun
	  -- must always "call" a function-typed thing, cannot just enter it
	  | is_fun -> SlowCall
	  | updatable || opt_DoTickyProfiling  -- to catch double entry
		|| opt_SMP  -- always enter via node on SMP, since the
			    -- thunk might have been blackholed in the 
			    -- meantime.
	     -> ASSERT(null arg_kinds) EnterIt
	  | otherwise
	     -> ASSERT(null arg_kinds) 
		JumpToIt (thunkEntryLabel name std_form_info updatable)

	LFUnknown True  -> SlowCall -- might be a function
	LFUnknown False -> ASSERT2 (null arg_kinds, ppr name <+> ppr arg_kinds) EnterIt -- not a function

	LFBlackHole _ -> SlowCall -- Presumably the black hole has by now
				  -- been updated, but we don't know with
				  -- what, so we slow call it

	LFLetNoEscape 0
	  -> JumpToIt (mkReturnPtLabel (nameUnique name))

	LFLetNoEscape arity
	  -> if (not (arg_kinds `lengthIs` arity)) then pprPanic "let-no-escape: " (ppr name <+> ppr arity) else
	     DirectEntry (mkReturnPtLabel (nameUnique name)) arity arg_regs
	 where
	    (arg_regs, _) = assignRegs [] arg_kinds
	    -- node never points to a LetNoEscape, see above --SDM
    	    --live_regs     = if node_points then [node] else []
    )

blackHoleOnEntry :: ClosureInfo -> Bool

-- Static closures are never themselves black-holed.
-- Updatable ones will be overwritten with a CAFList cell, which points to a 
-- black hole;
-- Single-entry ones have no fvs to plug, and we trust they don't form part 
-- of a loop.

blackHoleOnEntry ConInfo{} = False
blackHoleOnEntry (ClosureInfo { closureLFInfo = lf_info, closureSMRep = rep })
  | isStaticRep rep
  = False	-- Never black-hole a static closure

  | otherwise
  = case lf_info of
	LFReEntrant _ _ _ _	  -> False
	LFLetNoEscape _		  -> False
	LFThunk _ no_fvs updatable _ _
	  -> if updatable
	     then not opt_OmitBlackHoling
	     else opt_DoTickyProfiling || not no_fvs
                  -- the former to catch double entry,
                  -- and the latter to plug space-leaks.  KSW/SDM 1999-04.

	other -> panic "blackHoleOnEntry"	-- Should never happen

isStandardFormThunk :: LambdaFormInfo -> Bool

isStandardFormThunk (LFThunk _ _ _ (SelectorThunk _) _) = True
isStandardFormThunk (LFThunk _ _ _ (ApThunk _) _)	= True
isStandardFormThunk other_lf_info 			= False

\end{code}

-----------------------------------------------------------------------------
SRT-related stuff

\begin{code}
staticClosureNeedsLink :: ClosureInfo -> Bool
-- A static closure needs a link field to aid the GC when traversing
-- the static closure graph.  But it only needs such a field if either
-- 	a) it has an SRT
--	b) it's a constructor with one or more pointer fields
-- In case (b), the constructor's fields themselves play the role
-- of the SRT.
staticClosureNeedsLink (ClosureInfo { closureSRT = srt })
  = needsSRT srt
staticClosureNeedsLink (ConInfo { closureSMRep = sm_rep, closureCon = con })
  = not (isNullaryDataCon con) && not_nocaf_constr
  where
    not_nocaf_constr = 
	case sm_rep of 
	   GenericRep _ _ _ ConstrNoCaf -> False
	   _other			-> True
\end{code}

Avoiding generating entries and info tables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At present, for every function we generate all of the following,
just in case.  But they aren't always all needed, as noted below:

[NB1: all of this applies only to *functions*.  Thunks always
have closure, info table, and entry code.]

[NB2: All are needed if the function is *exported*, just to play safe.]


* Fast-entry code  ALWAYS NEEDED

* Slow-entry code
	Needed iff (a) we have any un-saturated calls to the function
	OR	   (b) the function is passed as an arg
	OR	   (c) we're in the parallel world and the function has free vars
			[Reason: in parallel world, we always enter functions
			with free vars via the closure.]

* The function closure
	Needed iff (a) we have any un-saturated calls to the function
	OR	   (b) the function is passed as an arg
	OR	   (c) if the function has free vars (ie not top level)

  Why case (a) here?  Because if the arg-satis check fails,
  UpdatePAP stuffs a pointer to the function closure in the PAP.
  [Could be changed; UpdatePAP could stuff in a code ptr instead,
   but doesn't seem worth it.]

  [NB: these conditions imply that we might need the closure
  without the slow-entry code.  Here's how.

	f x y = let g w = ...x..y..w...
		in
		...(g t)...

  Here we need a closure for g which contains x and y,
  but since the calls are all saturated we just jump to the
  fast entry point for g, with R1 pointing to the closure for g.]


* Standard info table
	Needed iff (a) we have any un-saturated calls to the function
	OR	   (b) the function is passed as an arg
	OR 	   (c) the function has free vars (ie not top level)

	NB.  In the sequential world, (c) is only required so that the function closure has
	an info table to point to, to keep the storage manager happy.
	If (c) alone is true we could fake up an info table by choosing
	one of a standard family of info tables, whose entry code just
	bombs out.

	[NB In the parallel world (c) is needed regardless because
	we enter functions with free vars via the closure.]

	If (c) is retained, then we'll sometimes generate an info table
	(for storage mgr purposes) without slow-entry code.  Then we need
	to use an error label in the info table to substitute for the absent
	slow entry code.

\begin{code}
staticClosureRequired
	:: Name
	-> StgBinderInfo
	-> LambdaFormInfo
	-> Bool
staticClosureRequired binder bndr_info
		      (LFReEntrant top_level _ _ _)	-- It's a function
  = ASSERT( isTopLevel top_level )
	-- Assumption: it's a top-level, no-free-var binding
	not (satCallsOnly bndr_info)

staticClosureRequired binder other_binder_info other_lf_info = True
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-misc-funs]{Misc functions about @ClosureInfo@, etc.}
%*									*
%************************************************************************

\begin{code}

isStaticClosure :: ClosureInfo -> Bool
isStaticClosure cl_info = isStaticRep (closureSMRep cl_info)

closureUpdReqd :: ClosureInfo -> Bool
closureUpdReqd (ClosureInfo { closureLFInfo = LFThunk _ _ upd _ _ }) = upd
closureUpdReqd (ClosureInfo { closureLFInfo = LFBlackHole _ })     = True
	-- Black-hole closures are allocated to receive the results of an
	-- alg case with a named default... so they need to be updated.
closureUpdReqd other_closure = False

closureSingleEntry :: ClosureInfo -> Bool
closureSingleEntry (ClosureInfo { closureLFInfo = LFThunk _ _ upd _ _}) = not upd
closureSingleEntry other_closure = False

closureReEntrant :: ClosureInfo -> Bool
closureReEntrant (ClosureInfo { closureLFInfo = LFReEntrant _ _ _ _ }) = True
closureReEntrant other_closure = False

closureSemiTag :: ClosureInfo -> Maybe Int
closureSemiTag (ConInfo { closureCon = data_con })
      = Just (dataConTag data_con - fIRST_TAG)
closureSemiTag _ = Nothing

closureFunInfo :: ClosureInfo -> Maybe (Int, ArgDescr)
closureFunInfo (ClosureInfo { closureLFInfo = LFReEntrant _ arity _ arg_desc})
  = Just (arity, arg_desc)
closureFunInfo _
  = Nothing
\end{code}

\begin{code}
isToplevClosure :: ClosureInfo -> Bool
isToplevClosure (ClosureInfo { closureLFInfo = lf_info })
  = case lf_info of
      LFReEntrant TopLevel _ _ _ -> True
      LFThunk TopLevel _ _ _ _   -> True
      other -> False
isToplevClosure _ = False
\end{code}

Label generation.

\begin{code}
infoTableLabelFromCI :: ClosureInfo -> CLabel
infoTableLabelFromCI (ClosureInfo { closureName = name,
				    closureLFInfo = lf_info, 
				    closureSMRep = rep })
  = case lf_info of
	LFBlackHole info -> info

	LFThunk _ _ upd_flag (SelectorThunk offset) _ -> 
		mkSelectorInfoLabel upd_flag offset

	LFThunk _ _ upd_flag (ApThunk arity) _ -> 
		mkApInfoTableLabel upd_flag arity

	LFThunk{}      -> mkInfoTableLabel name

	LFReEntrant _ _ _ (ArgGen _ _) -> mkInfoTableLabel name
	LFReEntrant _ _ _ _             -> mkInfoTableLabel name

	other -> panic "infoTableLabelFromCI"

infoTableLabelFromCI (ConInfo { closureCon = con, closureSMRep = rep })
  =  mkConInfoPtr con rep


mkConInfoPtr :: DataCon -> SMRep -> CLabel
mkConInfoPtr con rep
  | isStaticRep rep = mkStaticInfoTableLabel  name
  | otherwise	    = mkConInfoTableLabel     name
  where
    name = dataConName con

mkConEntryPtr :: DataCon -> SMRep -> CLabel
mkConEntryPtr con rep
  | isStaticRep rep = mkStaticConEntryLabel (dataConName con)
  | otherwise       = mkConEntryLabel       (dataConName con)

closureLabelFromCI (ClosureInfo { closureName = nm }) = mkClosureLabel nm
closureLabelFromCI _ = panic "closureLabelFromCI"

entryLabelFromCI :: ClosureInfo -> CLabel
entryLabelFromCI (ClosureInfo { closureName = id, 
			        closureLFInfo = lf_info, 
			        closureSMRep = rep })
  = case lf_info of
	LFThunk _ _ upd_flag std_form_info _ -> 
		thunkEntryLabel id std_form_info upd_flag
	other -> mkEntryLabel id

entryLabelFromCI (ConInfo { closureCon = con, closureSMRep = rep })
  = mkConEntryPtr con rep


-- thunkEntryLabel is a local help function, not exported.  It's used from both
-- entryLabelFromCI and getEntryConvention.

thunkEntryLabel thunk_id (ApThunk arity) is_updatable
  = mkApEntryLabel is_updatable arity
thunkEntryLabel thunk_id (SelectorThunk offset) upd_flag
  = mkSelectorEntryLabel upd_flag offset
thunkEntryLabel thunk_id _ is_updatable
  = mkEntryLabel thunk_id
\end{code}

\begin{code}
allocProfilingMsg :: ClosureInfo -> FastString
allocProfilingMsg ConInfo{} = FSLIT("TICK_ALLOC_CON")
allocProfilingMsg ClosureInfo{ closureLFInfo = lf_info }
  = case lf_info of
      LFReEntrant _ _ _ _   -> FSLIT("TICK_ALLOC_FUN")
      LFThunk _ _ True _ _  -> FSLIT("TICK_ALLOC_UP_THK")  -- updatable
      LFThunk _ _ False _ _ -> FSLIT("TICK_ALLOC_SE_THK")  -- nonupdatable
      LFBlackHole _	    -> FSLIT("TICK_ALLOC_BH")
      _			    -> panic "allocProfilingMsg"
\end{code}

We need a black-hole closure info to pass to @allocDynClosure@ when we
want to allocate the black hole on entry to a CAF.  These are the only
ways to build an LFBlackHole, maintaining the invariant that it really
is a black hole and not something else.

\begin{code}
cafBlackHoleClosureInfo (ClosureInfo { closureName = nm,
				       closureType = ty })
  = ClosureInfo { closureName   = nm,
		  closureLFInfo = LFBlackHole mkCAFBlackHoleInfoTableLabel,
		  closureSMRep  = BlackHoleRep,
		  closureSRT    = NoC_SRT,
		  closureType   = ty,
		  closureDescr  = "" }
cafBlackHoleClosureInfo _ = panic "cafBlackHoleClosureInfo"

seCafBlackHoleClosureInfo (ClosureInfo { closureName = nm,
				         closureType = ty })
  = ClosureInfo { closureName   = nm,
		  closureLFInfo = LFBlackHole mkSECAFBlackHoleInfoTableLabel,
		  closureSMRep  = BlackHoleRep,
		  closureSRT    = NoC_SRT,
		  closureType   = ty,
		  closureDescr  = ""  }
seCafBlackHoleClosureInfo _ = panic "seCafBlackHoleClosureInfo"
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-Profiling-funs]{Misc functions about for profiling info.}
%*									*
%************************************************************************

Profiling requires two pieces of information to be determined for
each closure's info table --- description and type.

The description is stored directly in the @CClosureInfoTable@ when the
info table is built.

The type is determined from the type information stored with the @Id@
in the closure info using @closureTypeDescr@.

\begin{code}
closureTypeDescr :: ClosureInfo -> String
closureTypeDescr (ClosureInfo { closureType = ty })
  = getTyDescription ty
closureTypeDescr (ConInfo { closureCon = data_con })
  = occNameUserString (getOccName (dataConTyCon data_con))
\end{code}

%************************************************************************
%*									*
\subsection{Making argument bitmaps}
%*									*
%************************************************************************

\begin{code}
-- bring in ARG_P, ARG_N, etc.
#include "../includes/StgFun.h"

data ArgDescr
  = ArgSpec
	!Int		-- ARG_P, ARG_N, ...
  | ArgGen 
	CLabel		-- label for a slow-entry point
	Liveness	-- the arg bitmap: describes pointedness of arguments

mkArgDescr :: Name -> [Id] -> ArgDescr
mkArgDescr nm args = argDescr nm (filter nonVoidRep (map idPrimRep args))
  where nonVoidRep VoidRep = False
	nonVoidRep _ = True

argDescr nm [PtrRep]    = ArgSpec ARG_P
argDescr nm [FloatRep]  = ArgSpec ARG_F
argDescr nm [DoubleRep] = ArgSpec ARG_D
argDescr nm [r] | is64BitRep r  = ArgSpec ARG_L
argDescr nm [r] | isNonPtrRep r = ArgSpec ARG_N

argDescr nm [r1,r2] | isNonPtrRep r1 && isNonPtrRep r2 = ArgSpec ARG_NN
argDescr nm [r1,PtrRep] | isNonPtrRep r1 = ArgSpec ARG_NP
argDescr nm [PtrRep,r1] | isNonPtrRep r1 = ArgSpec ARG_PN
argDescr nm [PtrRep,PtrRep] = ArgSpec ARG_PP

argDescr nm [r1,r2,r3] | isNonPtrRep r1 && isNonPtrRep r2 && isNonPtrRep r3 = ArgSpec ARG_NNN
argDescr nm [r1,r2,PtrRep] | isNonPtrRep r1 && isNonPtrRep r2 = ArgSpec ARG_NNP
argDescr nm [r1,PtrRep,r2] | isNonPtrRep r1 && isNonPtrRep r2 = ArgSpec ARG_NPN
argDescr nm [r1,PtrRep,PtrRep] | isNonPtrRep r1 = ArgSpec ARG_NPP
argDescr nm [PtrRep,r1,r2] | isNonPtrRep r1 && isNonPtrRep r2 = ArgSpec ARG_PNN
argDescr nm [PtrRep,r1,PtrRep] | isNonPtrRep r1 = ArgSpec ARG_PNP
argDescr nm [PtrRep,PtrRep,r1] | isNonPtrRep r1 = ArgSpec ARG_PPN
argDescr nm [PtrRep,PtrRep,PtrRep] = ArgSpec ARG_PPP

argDescr nm [PtrRep,PtrRep,PtrRep,PtrRep] = ArgSpec ARG_PPPP
argDescr nm [PtrRep,PtrRep,PtrRep,PtrRep,PtrRep] = ArgSpec ARG_PPPPP
argDescr nm [PtrRep,PtrRep,PtrRep,PtrRep,PtrRep,PtrRep] = ArgSpec ARG_PPPPPP

argDescr name reps = ArgGen (mkSlowEntryLabel name) liveness
 where bitmap = argBits reps
       lbl = mkBitmapLabel name
       liveness = Liveness lbl (length bitmap) (mkBitmap bitmap) 

argBits [] = []
argBits (rep : args)
  | isFollowableRep rep = False : argBits args
  | otherwise = take (getPrimRepSize rep) (repeat True) ++ argBits args
\end{code}


%************************************************************************
%*									*
\subsection{Generating info tables}
%*									*
%************************************************************************

Here we make a concrete info table, represented as a list of CAddrMode
(it can't be simply a list of Word, because the SRT field is
represented by a label+offset expression).

\begin{code}
mkInfoTable :: ClosureInfo -> [CAddrMode]
mkInfoTable cl_info
 | tablesNextToCode = extra_bits ++ std_info
 | otherwise        = std_info ++ extra_bits
 where
    std_info = mkStdInfoTable entry_amode
		  ty_descr_amode cl_descr_amode cl_type srt_len layout_amode

    entry_amode = CLbl (entryLabelFromCI cl_info) CodePtrRep 

    closure_descr = 
	case cl_info of
	  ClosureInfo { closureDescr = descr } -> descr
	  ConInfo { closureCon = con } -> occNameUserString (getOccName con)

    ty_descr_amode = CLit (MachStr (mkFastString (closureTypeDescr cl_info)))
    cl_descr_amode = CLit (MachStr (mkFastString closure_descr))

    cl_type = getSMRepClosureTypeInt (closureSMRep cl_info)

    srt = closureSRT cl_info	     
    needs_srt = needsSRT srt

    semi_tag = closureSemiTag cl_info
    is_con = isJust semi_tag

    (srt_label,srt_len)
	| Just tag <- semi_tag = (mkIntCLit 0, fromIntegral tag) -- constructor
	| otherwise = 
	  case srt of
	    NoC_SRT -> (mkIntCLit 0, 0)
	    C_SRT lbl off bitmap -> 
	      (CAddr (CIndex (CLbl lbl DataPtrRep) (mkIntCLit off) WordRep),
	       bitmap)

    ptrs  = closurePtrsSize cl_info
    nptrs = size - ptrs
    size  = closureNonHdrSize cl_info

    layout_info :: StgWord
#ifdef WORDS_BIGENDIAN
    layout_info = (fromIntegral ptrs `shiftL` hALF_WORD) .|. fromIntegral nptrs
#else 
    layout_info = (fromIntegral ptrs) .|. (fromIntegral nptrs `shiftL` hALF_WORD)
#endif	     

    layout_amode = mkWordCLit layout_info

    extra_bits
	| is_fun    = fun_extra_bits
	| is_con    = []
	| needs_srt = [srt_label]
 	| otherwise = []

    maybe_fun_stuff = closureFunInfo cl_info
    is_fun = isJust maybe_fun_stuff
    (Just (arity, arg_descr)) = maybe_fun_stuff

    fun_extra_bits
	| tablesNextToCode = reg_fun_extra_bits
	| otherwise        = reverse reg_fun_extra_bits

    reg_fun_extra_bits
	| ArgGen slow_lbl liveness <- arg_descr
		= [
		   CLbl slow_lbl CodePtrRep, 
		   livenessToAddrMode liveness,
		   srt_label,
		   fun_amode
		  ]
	| needs_srt = [srt_label, fun_amode]
	| otherwise = [fun_amode]

#ifdef WORDS_BIGENDIAN
    fun_desc = (fromIntegral fun_type `shiftL` hALF_WORD) .|. fromIntegral arity
#else 
    fun_desc = (fromIntegral fun_type) .|. (fromIntegral arity `shiftL` hALF_WORD)
#endif

    fun_amode = mkWordCLit fun_desc

    fun_type = case arg_descr of
		ArgSpec n -> n
		ArgGen _ (Liveness _ size _)
			| size <= mAX_SMALL_BITMAP_SIZE -> ARG_GEN
			| otherwise 			-> ARG_GEN_BIG

-- Return info tables come in two flavours: direct returns and
-- vectored returns.

mkRetInfoTable :: CLabel -> C_SRT -> Liveness -> [CAddrMode]
mkRetInfoTable entry_lbl srt liveness
 = mkBitmapInfoTable (CLbl entry_lbl CodePtrRep) srt liveness []

mkVecInfoTable :: [CAddrMode] -> C_SRT -> Liveness -> [CAddrMode]
mkVecInfoTable vector srt liveness
 = mkBitmapInfoTable zero_amode srt liveness vector

mkBitmapInfoTable
   :: CAddrMode
   -> C_SRT -> Liveness
   -> [CAddrMode]
   -> [CAddrMode]
mkBitmapInfoTable entry_amode srt liveness vector
 | tablesNextToCode = extra_bits ++ std_info
 | otherwise        = std_info ++ extra_bits
 where
   std_info = mkStdInfoTable entry_amode zero_amode zero_amode 
		cl_type srt_len liveness_amode

   liveness_amode = livenessToAddrMode liveness

   (srt_label,srt_len) =
	  case srt of
	    NoC_SRT -> (mkIntCLit 0, 0)
	    C_SRT lbl off bitmap -> 
		    (CAddr (CIndex (CLbl lbl DataPtrRep) (mkIntCLit off) WordRep),
	      	     bitmap)

   cl_type = case (null vector, isBigLiveness liveness) of
		(True, True)   -> rET_BIG
		(True, False)  -> rET_SMALL
		(False, True)  -> rET_VEC_BIG
		(False, False) -> rET_VEC_SMALL

   srt_bit | needsSRT srt || not (null vector) = [srt_label]
	   | otherwise = []

   extra_bits | tablesNextToCode = reverse vector ++ srt_bit
              | otherwise        = srt_bit ++ vector

-- The standard bits of an info table.  This part of the info table
-- corresponds to the StgInfoTable type defined in InfoTables.h.

mkStdInfoTable
   :: CAddrMode				-- entry label
   -> CAddrMode				-- closure type descr (profiling)
   -> CAddrMode				-- closure descr (profiling)
   -> Int				-- closure type
   -> StgHalfWord			-- SRT length
   -> CAddrMode				-- layout field
   -> [CAddrMode]
mkStdInfoTable entry_lbl type_descr closure_descr cl_type srt_len layout_amode
 = std_info
 where  
    std_info
	| tablesNextToCode = std_info'
	| otherwise        = entry_lbl : std_info'

    std_info' =
	  -- par info
	  prof_info ++
 	  -- ticky info
	  -- debug info
	  [layout_amode] ++
	  CLit (MachWord (fromIntegral type_info)) :
	  []

    prof_info 
	| opt_SccProfilingOn = [ type_descr, closure_descr ]
	| otherwise = []

    -- sigh: building up the info table is endian-dependent.
    -- ToDo: do this using .byte and .word directives.
    type_info :: StgWord
#ifdef WORDS_BIGENDIAN
    type_info = (fromIntegral cl_type `shiftL` hALF_WORD) .|.
		(fromIntegral srt_len)
#else 
    type_info = (fromIntegral cl_type) .|.
		(fromIntegral srt_len `shiftL` hALF_WORD)
#endif

isBigLiveness (Liveness _ size _) = size > mAX_SMALL_BITMAP_SIZE

livenessToAddrMode :: Liveness -> CAddrMode
livenessToAddrMode (Liveness lbl size bits)
	| size <= mAX_SMALL_BITMAP_SIZE = small
	| otherwise = CLbl lbl DataPtrRep
	where
	  small = mkWordCLit (fromIntegral size .|. (small_bits `shiftL` bITMAP_BITS_SHIFT))
	  small_bits = case bits of 
			[]  -> 0
			[b] -> fromIntegral b
			_   -> panic "livenessToAddrMode"

zero_amode = mkIntCLit 0

-- IA64 mangler doesn't place tables next to code
tablesNextToCode :: Bool
#ifdef ia64_TARGET_ARCH
tablesNextToCode = False
#else
tablesNextToCode = not opt_Unregisterised
#endif
\end{code}
