
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[ClosureInfo]{Data structures which describe closures}

Much of the rationale for these things is in the ``details'' part of
the STG paper.

\begin{code}
#include "HsVersions.h"

module ClosureInfo (
	ClosureInfo, LambdaFormInfo, SMRep, 	-- all abstract
	StandardFormInfo,

	EntryConvention(..),

	mkClosureLFInfo, mkConLFInfo, mkSelectorLFInfo, mkVapLFInfo,
	mkLFImported, mkLFArgument, mkLFLetNoEscape,

	closureSize, closureHdrSize,
	closureNonHdrSize, closureSizeWithoutFixedHdr,
	closureGoodStuffSize, closurePtrsSize,
	slopSize, fitsMinUpdSize,

	layOutDynClosure, layOutDynCon, layOutStaticClosure,
	layOutStaticNoFVClosure, layOutPhantomClosure,
	mkVirtHeapOffsets,

	nodeMustPointToIt, getEntryConvention,
	blackHoleOnEntry,

	staticClosureRequired,
	slowFunEntryCodeRequired, funInfoTableRequired,
	stdVapRequired, noUpdVapRequired,

	closureId, infoTableLabelFromCI, fastLabelFromCI,
	closureLabelFromCI,
	entryLabelFromCI, 
	closureLFInfo, closureSMRep, closureUpdReqd,
	closureSingleEntry, closureSemiTag, closureType,
	closureReturnsUnboxedType, getStandardFormThunkInfo,

	isToplevClosure,
	closureKind, closureTypeDescr,		-- profiling

	isStaticClosure, allocProfilingMsg,
	blackHoleClosureInfo,
	maybeSelectorInfo,

    	dataConLiveness				-- concurrency
    ) where

IMP_Ubiq(){-uitous-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(AbsCLoop)		-- here for paranoia-checking
#endif

import AbsCSyn		( MagicId, node, mkLiveRegsMask,
			  {- GHC 0.29 only -} AbstractC, CAddrMode
			)
import StgSyn
import CgMonad

import Constants	( mIN_UPD_SIZE, mIN_SIZE_NonUpdHeapObject,
			  mAX_SPEC_ALL_PTRS, mAX_SPEC_MIXED_FIELDS,
			  mAX_SPEC_ALL_NONPTRS,
			  oTHER_TAG
			)
import CgRetConv	( assignRegs, dataReturnConvAlg,
			  DataReturnConvention(..)
			)
import CLabel		( CLabel, mkStdEntryLabel, mkFastEntryLabel,
			  mkPhantomInfoTableLabel, mkInfoTableLabel,
			  mkConInfoTableLabel, mkStaticClosureLabel, 
			  mkBlackHoleInfoTableLabel, mkVapInfoTableLabel,
			  mkStaticInfoTableLabel, mkStaticConEntryLabel,
			  mkConEntryLabel, mkClosureLabel, mkVapEntryLabel
			)
import CmdLineOpts	( opt_SccProfilingOn, opt_ForConcurrent )
import HeapOffs		( intOff, addOff, totHdrSize, varHdrSize,
			  SYN_IE(VirtualHeapOffset), HeapOffset
			)
import Id		( idType, getIdArity,
			  externallyVisibleId,
			  dataConTag, fIRST_TAG,
			  isDataCon, isNullaryDataCon, dataConTyCon,
			  isTupleCon, SYN_IE(DataCon),
			  GenId{-instance Eq-}, SYN_IE(Id)
			)
import IdInfo		( ArityInfo(..) )
import Maybes		( maybeToBool )
import Name		( getOccString )
import Outputable	( PprStyle(..), Outputable(..) )
import PprType		( getTyDescription, GenType{-instance Outputable-} )
import Pretty		--ToDo:rm
import PrelInfo		( maybeCharLikeTyCon, maybeIntLikeTyCon )
import PrimRep		( getPrimRepSize, separateByPtrFollowness, PrimRep )
import SMRep		-- all of it
import TyCon		( TyCon{-instance NamedThing-} )
import Type		( isPrimType, splitFunTyExpandingDictsAndPeeking,
			  mkFunTys, maybeAppSpecDataTyConExpandingDicts,
			  SYN_IE(Type)
			)
import Util		( isIn, mapAccumL, panic, pprPanic, assertPanic )
\end{code}

The ``wrapper'' data type for closure information:

\begin{code}
data ClosureInfo
  = MkClosureInfo
	Id			-- The thing bound to this closure
	LambdaFormInfo		-- info derivable from the *source*
	SMRep			-- representation used by storage manager
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-OLD-DOC]{OLD DOCUMENTATION PROBABLY SUPERCEDED BY stg-details}
%*									*
%************************************************************************

We can optimise the function-entry code as follows.
\begin{itemize}

\item	If the ``function'' is not updatable, we can jump directly to its
	entry code, rather than indirecting via the info pointer in the
	closure.  (For updatable thunks we must go via the closure, in
	case it has been updated.)

\item	If the former bullet applies, and the application we are
	compiling gives the function as many arguments as it needs, we
	can jump to its fast-entry code.  (This only applies if the
	function has one or more args, because zero-arg closures have
	no fast-entry code.)

\item	If the function is a top-level non-constructor or imported, there
	is no need to make Node point to its closure.  In order for
	this to be right, we need to ensure that:
	\begin{itemize}
	\item	If such closures are updatable then they push their
		static address in the update frame, not Node. Actually
		we create a black hole and push its address.

	\item	The arg satisfaction check should load Node before jumping to
		UpdatePAP.

	\item 	Top-level constructor closures need careful handling.  If we are to
		jump direct to the constructor code, we must load Node first, even
		though they are top-level.  But if we go to their ``own''
		standard-entry code (which loads Node and then jumps to the
		constructor code) we don't need to load Node.
	\end{itemize}
\end{itemize}


{\em Top level constructors (@mkStaticConEntryInfo@)}

\begin{verbatim}
	x = {y,ys} \ {} Cons {y,ys}	-- Std form constructor
\end{verbatim}

x-closure: Cons-info-table, y-closure, ys-closure

x-entry: Node = x-closure; jump( Cons-entry )

x's EntryInfo in its own module:
\begin{verbatim}
		Base-label = Cons		-- Not x!!
		NodeMustPoint = True
		ClosureClass = Constructor
\end{verbatim}

	So if x is entered, Node will be set up and
	we'll jump direct to the Cons code.

x's EntryInfo in another module: (which may not know that x is a constructor)
\begin{verbatim}
		Base-label = x			-- Is x!!
		NodeMustPoint = False		-- All imported things have False
		ClosureClass = non-committal
\end{verbatim}

	If x is entered, we'll jump to x-entry, which will set up Node
	before jumping to the standard Cons code

{\em Top level non-constructors (@mkStaticEntryInfo@)}
\begin{verbatim}
	x = ...
\end{verbatim}

For updatable thunks, x-entry must push an allocated BH in update frame, not Node.

For non-zero arity, arg satis check must load Node before jumping to
	UpdatePAP.

x's EntryInfo in its own module:
\begin{verbatim}
		Base-label = x
		NodeMustPoint = False
		ClosureClass = whatever
\end{verbatim}

{\em Inner constructors (@mkConEntryInfo@)}

\begin{verbatim}
		Base-label = Cons		-- Not x!!
		NodeMustPoint = True		-- If its arity were zero, it would
						-- have been lifted to top level
		ClosureClass = Constructor
\end{verbatim}

{\em Inner non-constructors (@mkEntryInfo@)}

\begin{verbatim}
		Base-label = x
		NodeMustPoint = True		-- If no free vars, would have been
						-- lifted to top level
		ClosureClass = whatever
\end{verbatim}

{\em Imported}

\begin{verbatim}
		Nothing,
	or
		Base-label = x
		NodeMustPoint = False
		ClosureClass = whatever
\end{verbatim}

==============
THINK: we could omit making Node point to top-level constructors
of arity zero; but that might interact nastily with updates.
==============


==========
The info we need to import for imported things is:

\begin{verbatim}
	data ImportInfo = UnknownImportInfo
			| HnfImport Int		-- Not updatable, arity given
						-- Arity can be zero, for (eg) constrs
			| UpdatableImport	-- Must enter via the closure
\end{verbatim}

ToDo: move this stuff???

\begin{pseudocode}
mkStaticEntryInfo lbl cl_class
  = MkEntryInfo lbl False cl_class

mkStaticConEntryInfo lbl
  = MkEntryInfo lbl True ConstructorClosure

mkEntryInfo lbl cl_class
  = MkEntryInfo lbl True cl_class

mkConEntryInfo lbl
  = MkEntryInfo lbl True ConstructorClosure
\end{pseudocode}

%************************************************************************
%*									*
\subsection[ClosureInfo-datatypes]{Data types for closure information}
%*									*
%************************************************************************

%************************************************************************
%*									*
\subsubsection[LambdaFormInfo-datatype]{@LambdaFormInfo@: source-derivable info}
%*									*
%************************************************************************

\begin{code}
data LambdaFormInfo
  = LFReEntrant		-- Reentrant closure; used for PAPs too
	Bool		-- True if top level
	Int		-- Arity
	Bool		-- True <=> no fvs

  | LFCon		-- Constructor
	DataCon		-- The constructor (may be specialised)
	Bool		-- True <=> zero arity

  | LFTuple		-- Tuples
	DataCon		-- The tuple constructor (may be specialised)
	Bool		-- True <=> zero arity

  | LFThunk		-- Thunk (zero arity)
	Bool		-- True <=> top level
	Bool		-- True <=> no free vars
	Bool		-- True <=> updatable (i.e., *not* single-entry)
	StandardFormInfo

  | LFArgument		-- Used for function arguments.  We know nothing about
			-- this closure.  Treat like updatable "LFThunk"...

  | LFImported		-- Used for imported things.  We know nothing about this
			-- closure.  Treat like updatable "LFThunk"...
			-- Imported things which we do know something about use
			-- one of the other LF constructors (eg LFReEntrant for
			-- known functions)

  | LFLetNoEscape	-- See LetNoEscape module for precise description of
			-- these "lets".
	Int		-- arity;
	StgLiveVars-- list of variables live in the RHS of the let.
			-- (ToDo: maybe not used)

  | LFBlackHole		-- Used for the closures allocated to hold the result
			-- of a CAF.  We want the target of the update frame to
			-- be in the heap, so we make a black hole to hold it.

  -- This last one is really only for completeness;
  -- it isn't actually used for anything interesting
  {- | LFIndirection -}

data StandardFormInfo	-- Tells whether this thunk has one of a small number
			-- of standard forms

  = NonStandardThunk	-- No, it isn't

 | SelectorThunk
       Id              	-- Scrutinee
       DataCon         	-- Constructor
       Int             	-- 0-origin offset of ak within the "goods" of constructor
			-- (Recall that the a1,...,an may be laid out in the heap
			--  in a non-obvious order.)

{- A SelectorThunk is of form

     case x of
       con a1,..,an -> ak

   and the constructor is from a single-constr type.
   If we can't convert the heap-offset of the selectee into an Int, e.g.,
   it's "GEN_VHS+i", we just give up.
-}

  | VapThunk
	Id			-- Function
	[StgArg]		-- Args
	Bool			-- True <=> the function is not top-level, so
				-- must be stored in the thunk too

{- A VapThunk is of form

	f a1 ... an

   where f is a known function, with arity n
   So for this thunk we can use the label for f's heap-entry
   info table (generated when f's defn was dealt with),
   rather than generating a one-off info table and entry code
   for this one thunk.
-}


mkLFArgument	= LFArgument
mkLFBlackHole	= LFBlackHole
mkLFLetNoEscape = LFLetNoEscape

mkLFImported :: Id -> LambdaFormInfo
mkLFImported id
  = case getIdArity id of
      ArityExactly 0	-> LFThunk True{-top-lev-} True{-no fvs-}
				   True{-updatable-} NonStandardThunk
      ArityExactly n	-> LFReEntrant True n True  -- n > 0
      other	   	-> LFImported	-- Not sure of exact arity
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-construction]{Functions which build LFInfos}
%*									*
%************************************************************************

@mkClosureLFInfo@ figures out the appropriate LFInfo for the closure.

\begin{code}
mkClosureLFInfo :: Bool 	-- True of top level
		-> [Id]		-- Free vars
		-> UpdateFlag 	-- Update flag
		-> [Id] 	-- Args
		-> LambdaFormInfo

mkClosureLFInfo top fvs upd_flag args@(_:_)  -- Non-empty args
  = LFReEntrant top (length args) (null fvs)

mkClosureLFInfo top fvs ReEntrant []
  = LFReEntrant top 0 (null fvs)

mkClosureLFInfo top fvs upd_flag []
  = LFThunk top (null fvs) (isUpdatable upd_flag) NonStandardThunk

isUpdatable ReEntrant   = False
isUpdatable SingleEntry = False
isUpdatable Updatable   = True
\end{code}

@mkConLFInfo@ is similar, for constructors.

\begin{code}
mkConLFInfo :: DataCon -> LambdaFormInfo

mkConLFInfo con
  = -- the isNullaryDataCon will do this: ASSERT(isDataCon con)
    (if isTupleCon con then LFTuple else LFCon) con (isNullaryDataCon con)

mkSelectorLFInfo scrutinee con offset
  = LFThunk False False True (SelectorThunk scrutinee con offset)

mkVapLFInfo fvs upd_flag fun_id args fun_in_vap
  = LFThunk False (null fvs) (isUpdatable upd_flag) (VapThunk fun_id args fun_in_vap)
\end{code}


%************************************************************************
%*									*
\subsection[ClosureInfo-sizes]{Functions about closure {\em sizes}}
%*									*
%************************************************************************

\begin{code}
closureSize :: ClosureInfo -> HeapOffset
closureSize cl_info@(MkClosureInfo _ _ sm_rep)
  = totHdrSize sm_rep `addOff` (intOff (closureNonHdrSize cl_info))

closureSizeWithoutFixedHdr :: ClosureInfo -> HeapOffset
closureSizeWithoutFixedHdr cl_info@(MkClosureInfo _ _ sm_rep)
  = varHdrSize sm_rep `addOff` (intOff (closureNonHdrSize cl_info))

closureHdrSize :: ClosureInfo -> HeapOffset
closureHdrSize (MkClosureInfo _ _ sm_rep)
  = totHdrSize sm_rep

closureNonHdrSize :: ClosureInfo -> Int
closureNonHdrSize cl_info@(MkClosureInfo _ lf_info sm_rep)
  = tot_wds + computeSlopSize tot_wds sm_rep (closureUpdReqd cl_info) --ToDo: pass lf_info?
  where
    tot_wds = closureGoodStuffSize cl_info

closureGoodStuffSize :: ClosureInfo -> Int
closureGoodStuffSize (MkClosureInfo _ _ sm_rep)
  = let (ptrs, nonptrs) = sizes_from_SMRep sm_rep
    in	ptrs + nonptrs

closurePtrsSize :: ClosureInfo -> Int
closurePtrsSize (MkClosureInfo _ _ sm_rep)
  = let (ptrs, _) = sizes_from_SMRep sm_rep
    in	ptrs

-- not exported:
sizes_from_SMRep (SpecialisedRep k ptrs nonptrs _)   = (ptrs, nonptrs)
sizes_from_SMRep (GenericRep       ptrs nonptrs _)   = (ptrs, nonptrs)
sizes_from_SMRep (BigTupleRep      ptrs)	     = (ptrs, 0)
sizes_from_SMRep (MuTupleRep       ptrs)	     = (ptrs, 0)
sizes_from_SMRep (DataRep               nonptrs)     = (0, nonptrs)
sizes_from_SMRep BlackHoleRep			     = (0, 0)
sizes_from_SMRep (StaticRep        ptrs nonptrs)     = (ptrs, nonptrs)
#ifdef DEBUG
sizes_from_SMRep PhantomRep	  = panic "sizes_from_SMRep: PhantomRep"
sizes_from_SMRep DynamicRep	  = panic "sizes_from_SMRep: DynamicRep"
#endif
\end{code}

\begin{code}
fitsMinUpdSize :: ClosureInfo -> Bool
fitsMinUpdSize (MkClosureInfo _ _ BlackHoleRep) = True
fitsMinUpdSize cl_info = isSpecRep (closureSMRep cl_info) && closureNonHdrSize cl_info <= mIN_UPD_SIZE
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
	Cons cell requires 2 words
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

\item
@SpecialisedRep@ closures closures may require slop:
	\begin{itemize}
	\item
	@ConstantRep@ and @CharLikeRep@ closures always use the address of
	a static closure. They are never allocated or
	collected (eg hold forwarding pointer) hence never any slop.

	\item
	@IntLikeRep@ are never updatable.
	May need slop to be collected (as they will be size 1 or more
	this probably has no affect)

	\item
	@SpecRep@ may be updateable and will be collectable

	\item
	@StaticRep@ may require slop if updatable. Non-updatable ones are OK.

	\item
	@GenericRep@ closures will always be larger so never require slop.
	\end{itemize}

	***** ToDo: keep an eye on this!
\end{itemize}

\begin{code}
slopSize cl_info@(MkClosureInfo _ lf_info sm_rep)
  = computeSlopSize (closureGoodStuffSize cl_info) sm_rep (closureUpdReqd cl_info)

computeSlopSize :: Int -> SMRep -> Bool -> Int

computeSlopSize tot_wds (SpecialisedRep ConstantRep _ _ _) _
  = 0
computeSlopSize tot_wds (SpecialisedRep CharLikeRep _ _ _) _
  = 0

computeSlopSize tot_wds (SpecialisedRep _ _ _ _) True	-- Updatable
  = max 0 (mIN_UPD_SIZE - tot_wds)
computeSlopSize tot_wds (StaticRep _ _) True		-- Updatable
  = max 0 (mIN_UPD_SIZE - tot_wds)
computeSlopSize tot_wds BlackHoleRep _			-- Updatable
  = max 0 (mIN_UPD_SIZE - tot_wds)

computeSlopSize tot_wds (SpecialisedRep _ _ _ _) False	-- Not updatable
  = max 0 (mIN_SIZE_NonUpdHeapObject - tot_wds)

computeSlopSize tot_wds other_rep _			-- Any other rep
  = 0
\end{code}

%************************************************************************
%*									*
\subsection[layOutDynClosure]{Lay out a dynamic closure}
%*									*
%************************************************************************

\begin{code}
layOutDynClosure, layOutStaticClosure
	:: Id			    -- STG identifier w/ which this closure assoc'd
	-> (a -> PrimRep)    	    -- function w/ which to be able to get a PrimRep
	-> [a]			    -- the "things" being layed out
	-> LambdaFormInfo	    -- what sort of closure it is
	-> (ClosureInfo,	    -- info about the closure
	    [(a, VirtualHeapOffset)])	-- things w/ offsets pinned on them

layOutDynClosure name kind_fn things lf_info
  = (MkClosureInfo name lf_info sm_rep,
     things_w_offsets)
  where
    (tot_wds,		 -- #ptr_wds + #nonptr_wds
     ptr_wds,		 -- #ptr_wds
     things_w_offsets) = mkVirtHeapOffsets sm_rep kind_fn things
    sm_rep = chooseDynSMRep lf_info tot_wds ptr_wds

layOutStaticClosure name kind_fn things lf_info
  = (MkClosureInfo name lf_info (StaticRep ptr_wds (tot_wds - ptr_wds)),
     things_w_offsets)
  where
    (tot_wds,		 -- #ptr_wds + #nonptr_wds
     ptr_wds,		 -- #ptr_wds
     things_w_offsets) = mkVirtHeapOffsets (StaticRep bot bot) kind_fn things
    bot = panic "layoutStaticClosure"

layOutStaticNoFVClosure :: Id -> LambdaFormInfo -> ClosureInfo
layOutStaticNoFVClosure name lf_info
  = MkClosureInfo name lf_info (StaticRep ptr_wds nonptr_wds)
 where
  -- I am very uncertain that this is right - it will show up when testing
  -- my dynamic loading code.  ADR
  -- (If it's not right, we'll have to grab the kinds of the arguments from
  --  somewhere.)
  ptr_wds = 0
  nonptr_wds = 0

layOutPhantomClosure :: Id -> LambdaFormInfo -> ClosureInfo
layOutPhantomClosure name lf_info = MkClosureInfo name lf_info PhantomRep
\end{code}

A wrapper for when used with data constructors:
\begin{code}
layOutDynCon :: DataCon
	     -> (a -> PrimRep)
	     -> [a]
	     -> (ClosureInfo, [(a,VirtualHeapOffset)])

layOutDynCon con kind_fn args
  = ASSERT(isDataCon con)
    layOutDynClosure con kind_fn args (mkConLFInfo con)
\end{code}


%************************************************************************
%*									*
\subsection[SMreps]{Choosing SM reps}
%*									*
%************************************************************************

\begin{code}
chooseDynSMRep
	:: LambdaFormInfo
	-> Int -> Int		-- Tot wds, ptr wds
	-> SMRep

chooseDynSMRep lf_info tot_wds ptr_wds
  = let
	 nonptr_wds = tot_wds - ptr_wds

	 updatekind = case lf_info of
	     LFThunk _ _ upd _  -> if upd then SMUpdatable else SMSingleEntry
	     LFBlackHole	-> SMUpdatable
	     _  	    	-> SMNormalForm
    in
    if (nonptr_wds == 0 && ptr_wds <= mAX_SPEC_ALL_PTRS)
	    || (tot_wds <= mAX_SPEC_MIXED_FIELDS)
	    || (ptr_wds == 0 && nonptr_wds <= mAX_SPEC_ALL_NONPTRS) then
	let
	  spec_kind  = case lf_info of

	   (LFTuple _ True) -> ConstantRep

	   (LFTuple _ _)  -> SpecRep

	   (LFCon _ True) -> ConstantRep

	   (LFCon con _ ) -> if maybeToBool (maybeCharLikeTyCon tycon) then CharLikeRep
			     else if maybeToBool (maybeIntLikeTyCon tycon) then IntLikeRep
			     else SpecRep
			     where
			     tycon = dataConTyCon con

	   _ 		  -> SpecRep
	in
	SpecialisedRep spec_kind ptr_wds nonptr_wds updatekind
    else
	GenericRep ptr_wds nonptr_wds updatekind
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
mkVirtHeapOffsets :: SMRep	-- Representation to be used by storage manager
	  -> (a -> PrimRep)	-- To be able to grab kinds;
				--  	w/ a kind, we can find boxedness
	  -> [a]		-- Things to make offsets for
	  -> (Int,		-- *Total* number of words allocated
	      Int,		-- Number of words allocated for *pointers*
	      [(a, VirtualHeapOffset)])
				-- Things with their offsets from start of object
				-- 	in order of increasing offset

-- First in list gets lowest offset, which is initial offset + 1.

mkVirtHeapOffsets sm_rep kind_fun things
  = let (ptrs, non_ptrs)    	      = separateByPtrFollowness kind_fun things
    	(wds_of_ptrs, ptrs_w_offsets) = mapAccumL computeOffset 0 ptrs
	(tot_wds, non_ptrs_w_offsets) = mapAccumL computeOffset wds_of_ptrs non_ptrs
    in
	(tot_wds, wds_of_ptrs, ptrs_w_offsets ++ non_ptrs_w_offsets)
  where
    offset_of_first_word = totHdrSize sm_rep
    computeOffset wds_so_far thing
      = (wds_so_far + (getPrimRepSize . kind_fun) thing,
	 (thing, (offset_of_first_word `addOff` (intOff wds_so_far)))
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
  = let
	do_profiling = opt_SccProfilingOn
    in
    case lf_info of
	LFReEntrant top arity no_fvs -> returnFC (
	    not no_fvs ||   -- Certainly if it has fvs we need to point to it

	    not top -- If it is not top level we will point to it
		    --   We can have a \r closure with no_fvs which
		    --   is not top level as special case cgRhsClosure
		    --   has been dissabled in favour of let floating

		-- For lex_profiling we also access the cost centre for a
		-- non-inherited function i.e. not top level
		-- the  not top  case above ensures this is ok.
	    )

	LFCon	_ zero_arity -> returnFC True
	LFTuple _ zero_arity -> returnFC True

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

	LFThunk _ no_fvs updatable NonStandardThunk
	  -> returnFC (updatable || not no_fvs || do_profiling)

	  -- For the non-updatable (single-entry case):
	  --
	  -- True if has fvs (in which case we need access to them, and we
	  --		    should black-hole it)
	  -- or profiling (in which case we need to recover the cost centre
	  --		 from inside it)

	LFThunk _ no_fvs updatable some_standard_form_thunk
	  -> returnFC True
	  -- Node must point to any standard-form thunk.
	  -- For example,
	  --		x = f y
	  -- generates a Vap thunk for (f y), and even if y is a global
	  -- variable we must still make Node point to the thunk before entering it
	  -- because that's what the standard-form code expects.

	LFArgument  -> returnFC True
	LFImported  -> returnFC True
	LFBlackHole -> returnFC True
		    -- BH entry may require Node to point

	LFLetNoEscape _ _ -> returnFC False
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
data EntryConvention
  = ViaNode				-- The "normal" convention

  | StdEntry CLabel			-- Jump to this code, with args on stack
	     (Maybe CLabel) 	    	-- possibly setting infoptr to this

  | DirectEntry 			-- Jump directly to code, with args in regs
	CLabel 				--   The code label
	Int 				--   Its arity
	[MagicId]			--   Its register assignments (possibly empty)

getEntryConvention :: Id		-- Function being applied
		   -> LambdaFormInfo	-- Its info
		   -> [PrimRep]		-- Available arguments
		   -> FCode EntryConvention

getEntryConvention id lf_info arg_kinds
 =  nodeMustPointToIt lf_info	`thenFC` \ node_points ->
    let
	is_concurrent = opt_ForConcurrent
    in
    returnFC (

    if (node_points && is_concurrent) then ViaNode else

    case lf_info of

	LFReEntrant _ arity _ ->
	    if arity == 0 || (length arg_kinds) < arity then
		StdEntry (mkStdEntryLabel id) Nothing
	    else
		DirectEntry (mkFastEntryLabel id arity) arity arg_regs
	  where
	    (arg_regs, _) = assignRegs live_regs (take arity arg_kinds)
    	    live_regs = if node_points then [node] else []

	LFCon con zero_arity
			  -> let itbl = if zero_arity then
    	    	    	    	        mkPhantomInfoTableLabel con
    	    	    	    	    	else
    	    	    	    	    	mkConInfoTableLabel con
    	    	    	     in
			     --false:ASSERT (null arg_kinds)	-- Should have no args (meaning what?)
			     StdEntry (mkConEntryLabel con) (Just itbl)

	LFTuple tup zero_arity
			  -> --false:ASSERT (null arg_kinds)	-- Should have no args (meaning what?)
			     StdEntry (mkConEntryLabel tup) (Just (mkConInfoTableLabel tup))

	LFThunk _ _ updatable std_form_info
	  -> if updatable
	     then ViaNode
	     else StdEntry (thunkEntryLabel id std_form_info updatable) Nothing

	LFArgument  -> ViaNode
	LFImported  -> ViaNode
	LFBlackHole -> ViaNode	-- Presumably the black hole has by now
				-- been updated, but we don't know with
				-- what, so we enter via Node

	LFLetNoEscape arity _
	  -> ASSERT(arity == length arg_kinds)
	     DirectEntry (mkStdEntryLabel id) arity arg_regs
	 where
	    (arg_regs, _) = assignRegs live_regs arg_kinds
    	    live_regs     = if node_points then [node] else []
    )

blackHoleOnEntry :: Bool	-- No-black-holing flag
		 -> ClosureInfo
		 -> Bool

-- Static closures are never themselves black-holed.
-- Updatable ones will be overwritten with a CAFList cell, which points to a black hole;
-- Single-entry ones have no fvs to plug, and we trust they don't form part of a loop.

blackHoleOnEntry no_black_holing (MkClosureInfo _ _ (StaticRep _ _)) = False

blackHoleOnEntry no_black_holing (MkClosureInfo _ lf_info _)
  = case lf_info of
	LFReEntrant _ _ _	  -> False
	LFThunk _ no_fvs updatable _
	  -> if updatable
	     then not no_black_holing
	     else not no_fvs
	other -> panic "blackHoleOnEntry"	-- Should never happen

getStandardFormThunkInfo
	:: LambdaFormInfo
	-> Maybe [StgArg]		-- Nothing    => not a standard-form thunk
					-- Just atoms => a standard-form thunk with payload atoms

getStandardFormThunkInfo (LFThunk _ _ _ (SelectorThunk scrutinee _ _))
  = --trace "Selector thunk: missed opportunity to save info table + code"
    Nothing
	-- Just [StgVarArg scrutinee]
	-- We can't save the info tbl + code until we have a way to generate
	-- a fixed family thereof.

getStandardFormThunkInfo (LFThunk _ _ _ (VapThunk fun_id args fun_in_payload))
  | fun_in_payload = Just (StgVarArg fun_id : args)
  | otherwise	   = Just args

getStandardFormThunkInfo other_lf_info = Nothing

maybeSelectorInfo (MkClosureInfo _ (LFThunk _ _ _ (SelectorThunk _ con offset)) _) = Just (con,offset)
maybeSelectorInfo _ = Nothing
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

* Standard vap-entry code
  Standard vap-entry info table
	Needed iff we have any updatable thunks of the standard vap-entry shape.

* Single-update vap-entry code
  Single-update vap-entry info table
	Needed iff we have any non-updatable thunks of the
	standard vap-entry shape.


\begin{code}
staticClosureRequired
	:: Id
	-> StgBinderInfo
	-> LambdaFormInfo
	-> Bool
staticClosureRequired binder (StgBinderInfo arg_occ unsat_occ _ _ _)
		      (LFReEntrant top_level _ _)	-- It's a function
  = ASSERT( top_level )		 	-- Assumption: it's a top-level, no-free-var binding
    arg_occ 		-- There's an argument occurrence
    || unsat_occ	-- There's an unsaturated call
    || externallyVisibleId binder

staticClosureRequired binder other_binder_info other_lf_info = True

slowFunEntryCodeRequired	-- Assumption: it's a function, not a thunk.
	:: Id
	-> StgBinderInfo
	-> EntryConvention
	-> Bool
slowFunEntryCodeRequired binder (StgBinderInfo arg_occ unsat_occ _ _ _) entry_conv
  = arg_occ 		-- There's an argument occurrence
    || unsat_occ	-- There's an unsaturated call
    || externallyVisibleId binder
    || (case entry_conv of { DirectEntry _ _ _ -> False; other -> True })
	    {- The last case deals with the parallel world; a function usually
	       as a DirectEntry convention, but if it doesn't we must generate slow-entry code -}

slowFunEntryCodeRequired binder NoStgBinderInfo _ = True

funInfoTableRequired
	:: Id
	-> StgBinderInfo
	-> LambdaFormInfo
	-> Bool
funInfoTableRequired  binder (StgBinderInfo arg_occ unsat_occ _ _ _)
		     (LFReEntrant top_level _ _)
  = not top_level
    || arg_occ 		-- There's an argument occurrence
    || unsat_occ	-- There's an unsaturated call
    || externallyVisibleId binder

funInfoTableRequired other_binder_info binder other_lf_info = True

-- We need the vector-apply entry points for a function if
-- there's a vector-apply occurrence in this module

stdVapRequired, noUpdVapRequired :: StgBinderInfo -> Bool

stdVapRequired binder_info
  = case binder_info of
      StgBinderInfo _ _ std_vap_occ _ _ -> std_vap_occ
      _				        -> False

noUpdVapRequired binder_info
  = case binder_info of
      StgBinderInfo _ _ _ no_upd_vap_occ _ -> no_upd_vap_occ
      _					   -> False
\end{code}

@lfArity@ extracts the arity of a function from its LFInfo

\begin{code}
{- Not needed any more

lfArity_maybe (LFReEntrant _ arity _) = Just arity

-- Removed SLPJ March 97. I don't believe these two; 
-- LFCon is used for construcor *applications*, not constructors!
--
-- lfArity_maybe (LFCon con _)	      = Just (dataConArity con)
-- lfArity_maybe (LFTuple con _)	      = Just (dataConArity con)

lfArity_maybe other		      = Nothing
-}
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-misc-funs]{Misc functions about @ClosureInfo@, etc.}
%*									*
%************************************************************************

\begin{code}

isStaticClosure :: ClosureInfo -> Bool
isStaticClosure  (MkClosureInfo _ _ rep) = isStaticRep  rep

closureId :: ClosureInfo -> Id
closureId (MkClosureInfo id _ _) = id

closureSMRep :: ClosureInfo -> SMRep
closureSMRep (MkClosureInfo _ _ sm_rep) = sm_rep

closureLFInfo :: ClosureInfo -> LambdaFormInfo
closureLFInfo (MkClosureInfo _ lf_info _) = lf_info

closureUpdReqd :: ClosureInfo -> Bool

closureUpdReqd (MkClosureInfo _ (LFThunk _ _ upd _) _) = upd
closureUpdReqd (MkClosureInfo _ LFBlackHole _)         = True
	-- Black-hole closures are allocated to receive the results of an
	-- alg case with a named default... so they need to be updated.
closureUpdReqd other_closure			       = False

closureSingleEntry :: ClosureInfo -> Bool

closureSingleEntry (MkClosureInfo _ (LFThunk _ _ upd _) _) = not upd
closureSingleEntry other_closure			   = False
\end{code}

Note: @closureType@ returns appropriately specialised tycon and
datacons.
\begin{code}
closureType :: ClosureInfo -> Maybe (TyCon, [Type], [Id])

-- First, a turgid special case.  When we are generating the
-- standard code and info-table for Vaps (which is done when the function
-- defn is encountered), we don't have a convenient Id to hand whose
-- type is that of (f x y z).  So we need to figure out the type
-- rather than take it from the Id. The Id is probably just "f"!

closureType (MkClosureInfo id (LFThunk _ _ _ (VapThunk fun_id args _)) _)
  = maybeAppSpecDataTyConExpandingDicts (fun_result_ty (length args) fun_id)

closureType (MkClosureInfo id lf _) = maybeAppSpecDataTyConExpandingDicts (idType id)
\end{code}

@closureReturnsUnboxedType@ is used to check whether a closure, {\em
once it has eaten its arguments}, returns an unboxed type.  For
example, the closure for a function:
\begin{verbatim}
	f :: Int -> Int#
\end{verbatim}
returns an unboxed type.  This is important when dealing with stack
overflow checks.
\begin{code}
closureReturnsUnboxedType :: ClosureInfo -> Bool

closureReturnsUnboxedType (MkClosureInfo fun_id (LFReEntrant _ arity _) _)
  = isPrimType (fun_result_ty arity fun_id)

closureReturnsUnboxedType other_closure = False
	-- All non-function closures aren't functions,
	-- and hence are boxed, since they are heap alloc'd

-- ToDo: need anything like this in Type.lhs?
fun_result_ty arity id
  = let
	(arg_tys, res_ty)  = splitFunTyExpandingDictsAndPeeking (idType id)
    in
--    ASSERT(arity >= 0 && length arg_tys >= arity)
    (if (arity >= 0 && length arg_tys >= arity) then (\x->x) else pprPanic "fun_result_ty:" (hsep [int arity, ppr PprShowAll id, ppr PprDebug (idType id)])) $
    mkFunTys (drop arity arg_tys) res_ty
\end{code}

\begin{code}
closureSemiTag :: ClosureInfo -> Int

closureSemiTag (MkClosureInfo _ lf_info _)
  = case lf_info of
      LFCon data_con _ -> dataConTag data_con - fIRST_TAG
      LFTuple _ _      -> 0
      _	    	       -> fromInteger oTHER_TAG
\end{code}

\begin{code}
isToplevClosure :: ClosureInfo -> Bool

isToplevClosure (MkClosureInfo _ lf_info _)
  = case lf_info of
      LFReEntrant top _ _ -> top
      LFThunk top _ _ _   -> top
      _ -> panic "ClosureInfo:isToplevClosure"
\end{code}

Label generation.

\begin{code}
fastLabelFromCI :: ClosureInfo -> CLabel
fastLabelFromCI (MkClosureInfo id lf_info _)
{- 	[SLPJ Changed March 97]
	 (was ok, but is the only call to lfArity, 
	  and the id should guarantee to have the correct arity in it.

  = case lfArity_maybe lf_info of
	Just arity -> 
-}
  = case getIdArity id of
	ArityExactly arity -> mkFastEntryLabel id arity
	other	    	   -> pprPanic "fastLabelFromCI" (ppr PprDebug id)

infoTableLabelFromCI :: ClosureInfo -> CLabel
infoTableLabelFromCI (MkClosureInfo id lf_info rep)
  = case lf_info of
	LFCon con _ 	-> mkConInfoPtr con rep
	LFTuple tup _	-> mkConInfoPtr tup rep

	LFBlackHole     -> mkBlackHoleInfoTableLabel

	LFThunk _ _ upd_flag (VapThunk fun_id args _) -> mkVapInfoTableLabel fun_id upd_flag
					-- Use the standard vap info table
					-- for the function, rather than a one-off one
					-- for this particular closure

{-	For now, we generate individual info table and entry code for selector thunks,
	so their info table should be labelled in the standard way.
	The only special thing about them is that the info table has a field which
	tells the GC that it really is a selector.

	Later, perhaps, we'll have some standard RTS code for selector-thunk info tables,
	in which case this line will spring back to life.

	LFThunk _ _ upd_flag (SelectorThunk _ _ offset) -> mkSelectorInfoTableLabel upd_flag offset
					-- Ditto for selectors
-}

	other -> {-NO: if isStaticRep rep
		 then mkStaticInfoTableLabel id
		 else -} mkInfoTableLabel id

mkConInfoPtr :: Id -> SMRep -> CLabel
mkConInfoPtr con rep
  = ASSERT(isDataCon con)
    case rep of
      PhantomRep    -> mkPhantomInfoTableLabel con
      StaticRep _ _ -> mkStaticInfoTableLabel  con
      _		    -> mkConInfoTableLabel     con

mkConEntryPtr :: Id -> SMRep -> CLabel
mkConEntryPtr con rep
  = ASSERT(isDataCon con)
    case rep of
      StaticRep _ _ -> mkStaticConEntryLabel con
      _		    -> mkConEntryLabel con


closureLabelFromCI (MkClosureInfo id _ rep) 
	| isConstantRep rep
	= mkStaticClosureLabel id
	-- This case catches those pesky static closures for nullary constructors

closureLabelFromCI (MkClosureInfo id _ other_rep)   = mkClosureLabel id

entryLabelFromCI :: ClosureInfo -> CLabel
entryLabelFromCI (MkClosureInfo id lf_info rep)
  = case lf_info of
	LFThunk _ _ upd_flag std_form_info -> thunkEntryLabel id std_form_info upd_flag
	LFCon con _			   -> mkConEntryPtr con rep
	LFTuple tup _			   -> mkConEntryPtr tup rep
	other				   -> mkStdEntryLabel id

-- thunkEntryLabel is a local help function, not exported.  It's used from both
-- entryLabelFromCI and getEntryConvention.
-- I don't think it needs to deal with the SelectorThunk case
-- Well, it's falling over now, so I've made it deal with it.  (JSM)

thunkEntryLabel thunk_id (VapThunk fun_id args _) is_updatable
  = mkVapEntryLabel fun_id is_updatable
thunkEntryLabel thunk_id _ is_updatable
  = mkStdEntryLabel thunk_id
\end{code}

\begin{code}
allocProfilingMsg :: ClosureInfo -> FAST_STRING

allocProfilingMsg (MkClosureInfo _ lf_info _)
  = case lf_info of
      LFReEntrant _ _ _		-> SLIT("ALLOC_FUN")
      LFCon _ _			-> SLIT("ALLOC_CON")
      LFTuple _ _		-> SLIT("ALLOC_CON")
      LFThunk _ _ _ _ 		-> SLIT("ALLOC_THK")
      LFBlackHole		-> SLIT("ALLOC_BH")
      LFImported		-> panic "ALLOC_IMP"
\end{code}

We need a black-hole closure info to pass to @allocDynClosure@ when we
want to allocate the black hole on entry to a CAF.

\begin{code}
blackHoleClosureInfo (MkClosureInfo id _ _)
  = MkClosureInfo id LFBlackHole BlackHoleRep
\end{code}

The register liveness when returning from a constructor.  For
simplicity, we claim just [node] is live for all but PhantomRep's.  In
truth, this means that non-constructor info tables also claim node,
but since their liveness information is never used, we don't care.

\begin{code}
dataConLiveness (MkClosureInfo con _ PhantomRep)
  = case (dataReturnConvAlg con) of
      ReturnInRegs regs -> mkLiveRegsMask regs
      ReturnInHeap -> panic "dataConLiveness:PhantomRep in heap???"

dataConLiveness _ = mkLiveRegsMask [node]
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-Profiling-funs]{Misc functions about for profiling info.}
%*									*
%************************************************************************

Profiling requires three pices of information to be determined for
each closure's info table --- kind, description and type.

The description is stored directly in the @CClosureInfoTable@ when the
info table is built.

The kind is determined from the @LambdaForm@ stored in the closure
info using @closureKind@.

The type is determined from the type information stored with the @Id@
in the closure info using @closureTypeDescr@.

\begin{code}
closureKind :: ClosureInfo -> String

closureKind (MkClosureInfo _ lf _)
  = case lf of
      LFReEntrant _ n _		-> if n > 0 then "FN_K" else "THK_K"
      LFCon _ _			-> "CON_K"
      LFTuple _ _		-> "CON_K"
      LFThunk _ _ _ _ 		-> "THK_K"
      LFBlackHole		-> "THK_K" -- consider BHs as thunks for the moment... (ToDo?)
      LFImported		-> panic "IMP_KIND"

closureTypeDescr :: ClosureInfo -> String
closureTypeDescr (MkClosureInfo id lf _)
  = if (isDataCon id) then			 -- DataCon has function types
	getOccString (dataConTyCon id)		 -- We want the TyCon not the ->
    else
	getTyDescription (idType id)
\end{code}
