-----------------------------------------------------------------------------
--
-- Stg to C-- code generation:
-- 
-- The types   LambdaFormInfo
--             ClosureInfo
--
-- Nothing monadic in here!
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmClosure (
        SMRep, 
	DynTag,  tagForCon, isSmallFamily,
	ConTagZ, dataConTagZ,

	ArgDescr(..), Liveness, 
	C_SRT(..), needsSRT,

	isVoidRep, isGcPtrRep, addIdReps, addArgReps,
	argPrimRep, 

	-----------------------------------
	LambdaFormInfo,		-- Abstract
	StandardFormInfo,	-- ...ditto...
	mkLFThunk, mkLFReEntrant, mkConLFInfo, mkSelectorLFInfo,
	mkApLFInfo, mkLFImported, mkLFArgument, mkLFLetNoEscape,
	lfDynTag,
        maybeIsLFCon, isLFThunk, isLFReEntrant,

	-----------------------------------
	ClosureInfo,
	mkClosureInfo, mkConInfo, 

        closureSize,
        closureName, infoTableLabelFromCI, entryLabelFromCI,
	closureLabelFromCI, closureProf, closureSRT,
	closureLFInfo, closureSMRep, closureUpdReqd, 
	closureNeedsUpdSpace, closureIsThunk,
	closureSingleEntry, closureReEntrant, isConstrClosure_maybe,
	closureFunInfo,	isStandardFormThunk, isKnownFun,
        funTag, tagForArity, 

	enterIdLabel, enterLocalIdLabel, 

	nodeMustPointToIt, 
	CallMethod(..), getCallMethod,

	blackHoleOnEntry,

	isToplevClosure,
	isStaticClosure,
	cafBlackHoleClosureInfo, 

	staticClosureNeedsLink, clHasCafRefs 
    ) where

#include "../includes/MachDeps.h"

#define FAST_STRING_NOT_NEEDED
#include "HsVersions.h"

import StgSyn
import SMRep
import Cmm

import CLabel
import StaticFlags
import Id
import IdInfo
import DataCon
import Name
import Type
import TypeRep
import TcType
import TyCon
import BasicTypes
import Outputable
import Constants
import DynFlags

-----------------------------------------------------------------------------
--		Representations
-----------------------------------------------------------------------------

addIdReps :: [Id] -> [(PrimRep, Id)]
addIdReps ids = [(idPrimRep id, id) | id <- ids]

addArgReps :: [StgArg] -> [(PrimRep, StgArg)]
addArgReps args = [(argPrimRep arg, arg) | arg <- args]

argPrimRep :: StgArg -> PrimRep
argPrimRep arg = typePrimRep (stgArgType arg)

isVoidRep :: PrimRep -> Bool
isVoidRep VoidRep = True
isVoidRep _other  = False

isGcPtrRep :: PrimRep -> Bool
isGcPtrRep PtrRep = True
isGcPtrRep _      = False


-----------------------------------------------------------------------------
--		LambdaFormInfo
-----------------------------------------------------------------------------

-- Information about an identifier, from the code generator's point of
-- view.  Every identifier is bound to a LambdaFormInfo in the
-- environment, which gives the code generator enough info to be able to
-- tail call or return that identifier.

data LambdaFormInfo
  = LFReEntrant		-- Reentrant closure (a function)
	TopLevelFlag	-- True if top level
	!Int		-- Arity. Invariant: always > 0
	!Bool		-- True <=> no fvs
	ArgDescr	-- Argument descriptor (should really be in ClosureInfo)

  | LFThunk		-- Thunk (zero arity)
	TopLevelFlag
	!Bool		-- True <=> no free vars
	!Bool		-- True <=> updatable (i.e., *not* single-entry)
	StandardFormInfo
	!Bool		-- True <=> *might* be a function type

  | LFCon		-- A saturated constructor application
	DataCon		-- The constructor

  | LFUnknown		-- Used for function arguments and imported things.
			-- We know nothing about this closure.  
			-- Treat like updatable "LFThunk"...
			-- Imported things which we *do* know something about use
			-- one of the other LF constructors (eg LFReEntrant for
			-- known functions)
	!Bool		-- True <=> *might* be a function type
			--      The False case is good when we want to enter it,
			--	because then we know the entry code will do
			--	For a function, the entry code is the fast entry point

  | LFUnLifted		-- A value of unboxed type; 
			-- always a value, neeeds evaluation

  | LFLetNoEscape	-- See LetNoEscape module for precise description 

  | LFBlackHole		-- Used for the closures allocated to hold the result
			-- of a CAF.  We want the target of the update frame to
			-- be in the heap, so we make a black hole to hold it.


-------------------------
-- An ArgDsecr describes the argument pattern of a function

{-	XXX  -- imported from old ClosureInfo for now
data ArgDescr
  = ArgSpec		-- Fits one of the standard patterns
	!StgHalfWord	-- RTS type identifier ARG_P, ARG_N, ...

  | ArgGen	 	-- General case
	Liveness	-- Details about the arguments
-}

{-	XXX  -- imported from old ClosureInfo for now
-------------------------
-- We represent liveness bitmaps as a Bitmap (whose internal
-- representation really is a bitmap).  These are pinned onto case return
-- vectors to indicate the state of the stack for the garbage collector.
-- 
-- In the compiled program, liveness bitmaps that fit inside a single
-- word (StgWord) are stored as a single word, while larger bitmaps are
-- stored as a pointer to an array of words. 

data Liveness
  = SmallLiveness	-- Liveness info that fits in one word
	StgWord		-- Here's the bitmap

  | BigLiveness		-- Liveness info witha a multi-word bitmap
	CLabel		-- Label for the bitmap
-}

-------------------------
-- StandardFormInfo tells whether this thunk has one of 
-- a small number of standard forms

data StandardFormInfo
  = NonStandardThunk
	-- Not of of the standard forms

  | SelectorThunk
	-- A SelectorThunk is of form
	--      case x of
	--	       con a1,..,an -> ak
	-- and the constructor is from a single-constr type.
       WordOff         	-- 0-origin offset of ak within the "goods" of 
			-- constructor (Recall that the a1,...,an may be laid
			-- out in the heap in a non-obvious order.)

  | ApThunk 
	-- An ApThunk is of form
	--	x1 ... xn
	-- The code for the thunk just pushes x2..xn on the stack and enters x1.
	-- There are a few of these (for 1 <= n <= MAX_SPEC_AP_SIZE) pre-compiled
	-- in the RTS to save space.
	Int		-- Arity, n


------------------------------------------------------
--		Building LambdaFormInfo
------------------------------------------------------

mkLFArgument :: Id -> LambdaFormInfo
mkLFArgument id 
  | isUnLiftedType ty  	   = LFUnLifted
  | might_be_a_function ty = LFUnknown True
  | otherwise 		   = LFUnknown False
  where
    ty = idType id

-------------
mkLFLetNoEscape :: LambdaFormInfo
mkLFLetNoEscape = LFLetNoEscape

-------------
mkLFReEntrant :: TopLevelFlag	-- True of top level
	      -> [Id]	        -- Free vars
	      -> [Id] 		-- Args
	      -> ArgDescr	-- Argument descriptor
	      -> LambdaFormInfo

mkLFReEntrant top fvs args arg_descr 
  = LFReEntrant top (length args) (null fvs) arg_descr

-------------
mkLFThunk :: Type -> TopLevelFlag -> [Id] -> UpdateFlag -> LambdaFormInfo
mkLFThunk thunk_ty top fvs upd_flag
  = ASSERT( not (isUpdatable upd_flag) || not (isUnLiftedType thunk_ty) )
    LFThunk top (null fvs) 
	    (isUpdatable upd_flag)
	    NonStandardThunk 
	    (might_be_a_function thunk_ty)

--------------
might_be_a_function :: Type -> Bool
-- Return False only if we are *sure* it's a data type
-- Look through newtypes etc as much as poss
might_be_a_function ty
  = case tyConAppTyCon_maybe (repType ty) of
	Just tc -> not (isDataTyCon tc)
	Nothing -> True

-------------
mkConLFInfo :: DataCon -> LambdaFormInfo
mkConLFInfo con = LFCon con

-------------
mkSelectorLFInfo :: Id -> Int -> Bool -> LambdaFormInfo
mkSelectorLFInfo id offset updatable
  = LFThunk NotTopLevel False updatable (SelectorThunk offset) 
	(might_be_a_function (idType id))

-------------
mkApLFInfo :: Id -> UpdateFlag -> Arity -> LambdaFormInfo
mkApLFInfo id upd_flag arity
  = LFThunk NotTopLevel (arity == 0) (isUpdatable upd_flag) (ApThunk arity)
	(might_be_a_function (idType id))

-------------
mkLFImported :: Id -> LambdaFormInfo
mkLFImported id
  | Just con <- isDataConWorkId_maybe id
  , isNullaryRepDataCon con
  = LFCon con	-- An imported nullary constructor
		-- We assume that the constructor is evaluated so that
		-- the id really does point directly to the constructor

  | arity > 0
  = LFReEntrant TopLevel arity True (panic "arg_descr")

  | otherwise
  = mkLFArgument id -- Not sure of exact arity
  where
    arity = idArity id

-----------------------------------------------------
--		Dynamic pointer tagging
-----------------------------------------------------

type ConTagZ = Int	-- A *zero-indexed* contructor tag

type DynTag = Int	-- The tag on a *pointer*
			-- (from the dynamic-tagging paper)

{- 	Note [Data constructor dynamic tags]
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The family size of a data type (the number of constructors
or the arity of a function) can be either:
    * small, if the family size < 2**tag_bits
    * big, otherwise.

Small families can have the constructor tag in the tag bits.
Big families only use the tag value 1 to represent evaluatedness.
We don't have very many tag bits: for example, we have 2 bits on
x86-32 and 3 bits on x86-64. -}

isSmallFamily :: Int -> Bool
isSmallFamily fam_size = fam_size <= mAX_PTR_TAG

-- We keep the *zero-indexed* tag in the srt_len field of the info
-- table of a data constructor.
dataConTagZ :: DataCon -> ConTagZ
dataConTagZ con = dataConTag con - fIRST_TAG

tagForCon :: DataCon -> DynTag
tagForCon con 
  | isSmallFamily fam_size = con_tag + 1
  | otherwise		   = 1
  where
    con_tag  = dataConTagZ con
    fam_size = tyConFamilySize (dataConTyCon con)

tagForArity :: Int -> DynTag
tagForArity arity | isSmallFamily arity = arity
                  | otherwise           = 0

lfDynTag :: LambdaFormInfo -> DynTag
-- Return the tag in the low order bits of a variable bound
-- to this LambdaForm
lfDynTag (LFCon con)               = tagForCon con
lfDynTag (LFReEntrant _ arity _ _) = tagForArity arity
lfDynTag _other                    = 0


-----------------------------------------------------------------------------
--		Observing LambdaFormInfo
-----------------------------------------------------------------------------

-------------
maybeIsLFCon :: LambdaFormInfo -> Maybe DataCon
maybeIsLFCon (LFCon con) = Just con
maybeIsLFCon _ = Nothing

------------
isLFThunk :: LambdaFormInfo -> Bool
isLFThunk (LFThunk {})  = True
isLFThunk LFBlackHole   = True
	-- return True for a blackhole: this function is used to determine
	-- whether to use the thunk header in SMP mode, and a blackhole
	-- must have one.
isLFThunk _ = False

isLFReEntrant :: LambdaFormInfo -> Bool
isLFReEntrant (LFReEntrant {}) = True
isLFReEntrant _                = False

-----------------------------------------------------------------------------
--		Choosing SM reps
-----------------------------------------------------------------------------

lfClosureType :: LambdaFormInfo -> ClosureTypeInfo
lfClosureType (LFReEntrant _ arity _ argd) = Fun (fromIntegral arity) argd
lfClosureType (LFCon con)                  =  Constr (fromIntegral (dataConTagZ con))
                			              (dataConIdentity con)
lfClosureType (LFThunk _ _ _ is_sel _) 	   = thunkClosureType is_sel
lfClosureType _                 	   = panic "lfClosureType"

thunkClosureType :: StandardFormInfo -> ClosureTypeInfo
thunkClosureType (SelectorThunk off) = ThunkSelector (fromIntegral off)
thunkClosureType _                   = Thunk

-- We *do* get non-updatable top-level thunks sometimes.  eg. f = g
-- gets compiled to a jump to g (if g has non-zero arity), instead of
-- messing around with update frames and PAPs.  We set the closure type
-- to FUN_STATIC in this case.

-----------------------------------------------------------------------------
--		nodeMustPointToIt
-----------------------------------------------------------------------------

-- Be sure to see the stg-details notes about these...

nodeMustPointToIt :: LambdaFormInfo -> Bool
nodeMustPointToIt (LFReEntrant top _ no_fvs _)
  = not no_fvs ||   -- Certainly if it has fvs we need to point to it
    isNotTopLevel top
		    -- If it is not top level we will point to it
		    --   We can have a \r closure with no_fvs which
		    --   is not top level as special case cgRhsClosure
		    --   has been dissabled in favour of let floating

		-- For lex_profiling we also access the cost centre for a
		-- non-inherited function i.e. not top level
		-- the  not top  case above ensures this is ok.

nodeMustPointToIt (LFCon _) = True

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

nodeMustPointToIt (LFThunk _ no_fvs updatable NonStandardThunk _)
  = updatable || not no_fvs || opt_SccProfilingOn
	  -- For the non-updatable (single-entry case):
	  --
	  -- True if has fvs (in which case we need access to them, and we
	  --		    should black-hole it)
	  -- or profiling (in which case we need to recover the cost centre
	  --		 from inside it)

nodeMustPointToIt (LFThunk {})	-- Node must point to a standard-form thunk
  = True 

nodeMustPointToIt (LFUnknown _)   = True
nodeMustPointToIt LFUnLifted      = False
nodeMustPointToIt LFBlackHole     = True    -- BH entry may require Node to point
nodeMustPointToIt LFLetNoEscape   = False 

-----------------------------------------------------------------------------
--		getCallMethod
-----------------------------------------------------------------------------

{- The entry conventions depend on the type of closure being entered,
whether or not it has free variables, and whether we're running
sequentially or in parallel.

Closure 			      Node   Argument   Enter
Characteristics  		Par   Req'd  Passing    Via
-------------------------------------------------------------------------------
Unknown 			& no & yes & stack	& node
Known fun (>1 arg), no fvs 	& no & no  & registers 	& fast entry (enough args)
							& slow entry (otherwise)
Known fun (>1 arg), fvs		& no & yes & registers 	& fast entry (enough args)
0 arg, no fvs \r,\s 		& no & no  & n/a 	& direct entry
0 arg, no fvs \u 		& no & yes & n/a 	& node
0 arg, fvs \r,\s 		& no & yes & n/a 	& direct entry
0 arg, fvs \u	 		& no & yes & n/a 	& node

Unknown 			& yes & yes & stack	& node
Known fun (>1 arg), no fvs 	& yes & no  & registers & fast entry (enough args)
	 						& slow entry (otherwise)
Known fun (>1 arg), fvs		& yes & yes & registers & node
0 arg, no fvs \r,\s 		& yes & no  & n/a 	& direct entry 
0 arg, no fvs \u 		& yes & yes & n/a 	& node
0 arg, fvs \r,\s 		& yes & yes & n/a 	& node
0 arg, fvs \u 			& yes & yes & n/a 	& node
\end{tabular}

When black-holing, single-entry closures could also be entered via node
(rather than directly) to catch double-entry. -}

data CallMethod
  = EnterIt		-- No args, not a function

  | JumpToIt		-- A join point 

  | ReturnIt		-- It's a value (function, unboxed value,
			-- or constructor), so just return it.

  | SlowCall		-- Unknown fun, or known fun with
			-- too few args.

  | DirectEntry 	-- Jump directly, with args in regs
	CLabel 		--   The code label
	Int 		--   Its arity

getCallMethod :: DynFlags
              -> Name           -- Function being applied
              -> CafInfo        -- Can it refer to CAF's?
	      -> LambdaFormInfo	-- Its info
	      -> Int		-- Number of available arguments
	      -> CallMethod

getCallMethod _ _name _ lf_info _n_args
  | nodeMustPointToIt lf_info && opt_Parallel
  =	-- If we're parallel, then we must always enter via node.  
	-- The reason is that the closure may have been 	
	-- fetched since we allocated it.
    EnterIt

getCallMethod _ name caf (LFReEntrant _ arity _ _) n_args
  | n_args == 0    = ASSERT( arity /= 0 )
		     ReturnIt	-- No args at all
  | n_args < arity = SlowCall	-- Not enough args
  | otherwise      = DirectEntry (enterIdLabel name caf) arity

getCallMethod _ _name _ LFUnLifted n_args
  = ASSERT( n_args == 0 ) ReturnIt

getCallMethod _ _name _ (LFCon _) n_args
  = ASSERT( n_args == 0 ) ReturnIt

getCallMethod dflags name caf (LFThunk _ _ updatable std_form_info is_fun) n_args
  | is_fun 	-- it *might* be a function, so we must "call" it (which is always safe)
  = SlowCall	-- We cannot just enter it [in eval/apply, the entry code
		-- is the fast-entry code]

  -- Since is_fun is False, we are *definitely* looking at a data value
  | updatable || doingTickyProfiling dflags -- to catch double entry
      {- OLD: || opt_SMP
	 I decided to remove this, because in SMP mode it doesn't matter
	 if we enter the same thunk multiple times, so the optimisation
	 of jumping directly to the entry code is still valid.  --SDM
	-}
  = EnterIt
    -- We used to have ASSERT( n_args == 0 ), but actually it is
    -- possible for the optimiser to generate
    --   let bot :: Int = error Int "urk"
    --   in (bot `cast` unsafeCoerce Int (Int -> Int)) 3
    -- This happens as a result of the case-of-error transformation
    -- So the right thing to do is just to enter the thing

  | otherwise	-- Jump direct to code for single-entry thunks
  = ASSERT( n_args == 0 )
    DirectEntry (thunkEntryLabel name caf std_form_info updatable) 0

getCallMethod _ _name _ (LFUnknown True) _n_args
  = SlowCall -- might be a function

getCallMethod _ name _ (LFUnknown False) n_args
  = ASSERT2 ( n_args == 0, ppr name <+> ppr n_args ) 
    EnterIt -- Not a function

getCallMethod _ _name _ LFBlackHole _n_args
  = SlowCall	-- Presumably the black hole has by now
		-- been updated, but we don't know with
		-- what, so we slow call it

getCallMethod _ _name _ LFLetNoEscape _n_args
  = JumpToIt

isStandardFormThunk :: LambdaFormInfo -> Bool
isStandardFormThunk (LFThunk _ _ _ (SelectorThunk _) _) = True
isStandardFormThunk (LFThunk _ _ _ (ApThunk _) _)	= True
isStandardFormThunk _other_lf_info 			= False

isKnownFun :: LambdaFormInfo -> Bool
isKnownFun (LFReEntrant _ _ _ _) = True
isKnownFun LFLetNoEscape	 = True
isKnownFun _ = False

-----------------------------------------------------------------------------
--		staticClosureRequired
-----------------------------------------------------------------------------

{-  staticClosureRequired is never called (hence commented out)

    SimonMar writes (Sept 07) It's an optimisation we used to apply at
    one time, I believe, but it got lost probably in the rewrite of
    the RTS/code generator.  I left that code there to remind me to
    look into whether it was worth doing sometime

{- Avoiding generating entries and info tables
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
-}

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
-}

-----------------------------------------------------------------------------
--		Data types for closure information}
-----------------------------------------------------------------------------


{- Information about a closure, from the code generator's point of view.

A ClosureInfo decribes the info pointer of a closure.  It has
enough information 
  a) to construct the info table itself
  b) to allocate a closure containing that info pointer (i.e.
	it knows the info table label)

We make a ClosureInfo for
  - each let binding (both top level and not)
  - each data constructor (for its shared static and
	dynamic info tables)

Note [Closure CAF info]
~~~~~~~~~~~~~~~~~~~~~~~
The closureCafs field is relevant for *static closures only*.  It records
  * For an ordinary closure, whether a CAF is reachable from
    the code for the closure
  * For a constructor closure, whether a CAF is reachable
    from the fields of the constructor
It is initialised simply from the idCafInfo of the Id. 
-}

data ClosureInfo
  = ClosureInfo {
	closureName   :: !Name,		  -- The thing bound to this closure
	closureLFInfo :: !LambdaFormInfo, -- NOTE: not an LFCon (see below)
	closureSMRep  :: !SMRep,	  -- representation used by storage mgr
	closureSRT    :: !C_SRT,	  -- What SRT applies to this closure
	closureProf   :: !ProfilingInfo,
        closureCafs   :: !CafInfo,        -- See Note [Closure CAF info]
	closureInfLcl :: Bool             -- Can the info pointer be a local symbol?
    }

  -- Constructor closures don't have a unique info table label (they use
  -- the constructor's info table), and they don't have an SRT.
  | ConInfo {
	closureCon   :: !DataCon,
	closureSMRep :: !SMRep,
        closureCafs  :: !CafInfo        -- See Note [Closure CAF info]
    }

clHasCafRefs :: ClosureInfo -> CafInfo
-- Backward compatibility; remove
clHasCafRefs = closureCafs

--------------------------------------
--	Building ClosureInfos
--------------------------------------

mkClosureInfo :: Bool		-- Is static
	      -> Id
	      -> LambdaFormInfo 
	      -> Int -> Int	-- Total and pointer words
	      -> C_SRT
	      -> String		-- String descriptor
	      -> ClosureInfo
mkClosureInfo is_static id lf_info tot_wds ptr_wds srt_info val_descr
  = ClosureInfo { closureName = name, 
		  closureLFInfo = lf_info,
		  closureSMRep = sm_rep, 
		  closureSRT = srt_info,
		  closureProf = prof,
                  closureCafs = idCafInfo id,
		  closureInfLcl = isDataConWorkId id }
		    -- Make the _info pointer for the implicit datacon worker binding
		    -- local. The reason we can do this is that importing code always
		    -- either uses the _closure or _con_info. By the invariants in CorePrep
		    -- anything else gets eta expanded.
  where
    name   = idName id
    sm_rep = mkHeapRep is_static ptr_wds nonptr_wds (lfClosureType lf_info)
    prof   = mkProfilingInfo id val_descr
    nonptr_wds = tot_wds - ptr_wds

mkConInfo :: Bool	-- Is static
	  -> CafInfo 
	  -> DataCon	
	  -> Int -> Int	-- Total and pointer words
	  -> ClosureInfo
mkConInfo is_static cafs data_con tot_wds ptr_wds
   = ConInfo { closureSMRep = sm_rep
             , closureCafs = cafs
	     , closureCon = data_con }
  where
    sm_rep  = mkHeapRep is_static ptr_wds nonptr_wds (lfClosureType lf_info)
    lf_info = mkConLFInfo data_con
    nonptr_wds = tot_wds - ptr_wds

-- We need a black-hole closure info to pass to @allocDynClosure@ when we
-- want to allocate the black hole on entry to a CAF.  These are the only
-- ways to build an LFBlackHole, maintaining the invariant that it really
-- is a black hole and not something else.

cafBlackHoleClosureInfo :: ClosureInfo -> ClosureInfo
cafBlackHoleClosureInfo cl_info@(ClosureInfo {})
  = cl_info { closureLFInfo = LFBlackHole
	    , closureSMRep  = blackHoleRep
	    , closureSRT    = NoC_SRT
	    , closureInfLcl = False }
cafBlackHoleClosureInfo (ConInfo {}) = panic "cafBlackHoleClosureInfo"


--------------------------------------
--   Functions about closure *sizes*
--------------------------------------

closureSize :: ClosureInfo -> WordOff
closureSize cl_info = heapClosureSize (closureSMRep cl_info)

closureNeedsUpdSpace :: ClosureInfo -> Bool
-- We leave space for an update if either (a) the closure is updatable
-- or (b) it is a static thunk.  This is because a static thunk needs
-- a static link field in a predictable place (after the slop), regardless
-- of whether it is updatable or not.
closureNeedsUpdSpace (ClosureInfo { closureLFInfo = 
					LFThunk TopLevel _ _ _ _ }) = True
closureNeedsUpdSpace cl_info = closureUpdReqd cl_info

--------------------------------------
--   Other functions over ClosureInfo
--------------------------------------

blackHoleOnEntry :: DynFlags -> ClosureInfo -> Bool
-- Static closures are never themselves black-holed.
-- Updatable ones will be overwritten with a CAFList cell, which points to a 
-- black hole;
-- Single-entry ones have no fvs to plug, and we trust they don't form part 
-- of a loop.

blackHoleOnEntry _ ConInfo{} = False
blackHoleOnEntry dflags (ClosureInfo { closureLFInfo = lf_info, closureSMRep = rep })
  | isStaticRep rep
  = False	-- Never black-hole a static closure

  | otherwise
  = case lf_info of
	LFReEntrant _ _ _ _	  -> False
	LFLetNoEscape 		  -> False
	LFThunk _ no_fvs updatable _ _
	  -> if updatable
	     then not opt_OmitBlackHoling
	     else doingTickyProfiling dflags || not no_fvs
                  -- the former to catch double entry,
                  -- and the latter to plug space-leaks.  KSW/SDM 1999-04.

	_other -> panic "blackHoleOnEntry"	-- Should never happen


staticClosureNeedsLink :: ClosureInfo -> Bool
-- A static closure needs a link field to aid the GC when traversing
-- the static closure graph.  But it only needs such a field if either
-- 	a) it has an SRT
--	b) it's a constructor with one or more pointer fields
-- In case (b), the constructor's fields themselves play the role
-- of the SRT.
staticClosureNeedsLink (ClosureInfo { closureSRT = srt })
  = needsSRT srt
staticClosureNeedsLink (ConInfo { closureSMRep = rep })
  = not (isStaticNoCafCon rep)

isStaticClosure :: ClosureInfo -> Bool
isStaticClosure cl_info = isStaticRep (closureSMRep cl_info)

closureUpdReqd :: ClosureInfo -> Bool
closureUpdReqd ClosureInfo{ closureLFInfo = lf_info } = lfUpdatable lf_info
closureUpdReqd ConInfo{} = False

lfUpdatable :: LambdaFormInfo -> Bool
lfUpdatable (LFThunk _ _ upd _ _)  = upd
lfUpdatable LFBlackHole 	   = True
	-- Black-hole closures are allocated to receive the results of an
	-- alg case with a named default... so they need to be updated.
lfUpdatable _ = False

closureIsThunk :: ClosureInfo -> Bool
closureIsThunk ClosureInfo{ closureLFInfo = lf_info } = isLFThunk lf_info
closureIsThunk ConInfo{} = False

closureSingleEntry :: ClosureInfo -> Bool
closureSingleEntry (ClosureInfo { closureLFInfo = LFThunk _ _ upd _ _}) = not upd
closureSingleEntry _ = False

closureReEntrant :: ClosureInfo -> Bool
closureReEntrant (ClosureInfo { closureLFInfo = LFReEntrant _ _ _ _ }) = True
closureReEntrant _ = False

isConstrClosure_maybe :: ClosureInfo -> Maybe DataCon
isConstrClosure_maybe (ConInfo { closureCon = data_con }) = Just data_con
isConstrClosure_maybe _ 				  = Nothing

closureFunInfo :: ClosureInfo -> Maybe (Int, ArgDescr)
closureFunInfo (ClosureInfo { closureLFInfo = lf_info }) = lfFunInfo lf_info
closureFunInfo _ = Nothing

lfFunInfo :: LambdaFormInfo ->  Maybe (Int, ArgDescr)
lfFunInfo (LFReEntrant _ arity _ arg_desc)  = Just (arity, arg_desc)
lfFunInfo _                                 = Nothing

funTag :: ClosureInfo -> DynTag
funTag (ClosureInfo { closureLFInfo = lf_info }) = lfDynTag lf_info
funTag (ConInfo {})				 = panic "funTag"

isToplevClosure :: ClosureInfo -> Bool
isToplevClosure (ClosureInfo { closureLFInfo = lf_info })
  = case lf_info of
      LFReEntrant TopLevel _ _ _ -> True
      LFThunk TopLevel _ _ _ _   -> True
      _other			 -> False
isToplevClosure _ = False

--------------------------------------
--   Label generation
--------------------------------------

infoTableLabelFromCI :: ClosureInfo -> CLabel
infoTableLabelFromCI = fst . labelsFromCI

entryLabelFromCI :: ClosureInfo -> CLabel
entryLabelFromCI = snd . labelsFromCI

labelsFromCI :: ClosureInfo -> (CLabel, CLabel) -- (Info, Entry)
labelsFromCI (ClosureInfo { closureName = name,
			    closureLFInfo = lf_info,
			    closureCafs = cafs,
			    closureInfLcl = is_lcl })
  = case lf_info of
	LFBlackHole -> (mkCAFBlackHoleInfoTableLabel, mkCAFBlackHoleEntryLabel)

	LFThunk _ _ upd_flag (SelectorThunk offset) _ 
		      -> bothL (mkSelectorInfoLabel, mkSelectorEntryLabel) upd_flag offset

	LFThunk _ _ upd_flag (ApThunk arity) _ 
		      -> bothL (mkApInfoTableLabel, mkApEntryLabel) upd_flag arity

	LFThunk{}     -> bothL std_mk_lbls name cafs
	LFReEntrant{} -> bothL std_mk_lbls name cafs
	_other        -> panic "labelsFromCI"

  where 
    std_mk_lbls | is_lcl    = (mkLocalInfoTableLabel, mkLocalEntryLabel)
                | otherwise = (mkInfoTableLabel, mkEntryLabel)

labelsFromCI (ConInfo { closureCon = con, closureSMRep = rep, closureCafs = cafs })
  | isStaticRep rep 
  = bothL (mkStaticInfoTableLabel, mkStaticConEntryLabel) name cafs
  | otherwise	    
  = bothL (mkConInfoTableLabel,    mkConEntryLabel)       name cafs
  where
    name = dataConName con

bothL :: (a -> b -> c, a -> b -> c) -> a -> b -> (c, c)
bothL (f, g) x y = (f x y, g x y)

-- ClosureInfo for a closure (as opposed to a constructor) is always local
closureLabelFromCI :: ClosureInfo -> CLabel
closureLabelFromCI cl@(ClosureInfo { closureName = nm }) =
  mkLocalClosureLabel nm $ clHasCafRefs cl
closureLabelFromCI _ = panic "closureLabelFromCI"

thunkEntryLabel :: Name -> CafInfo -> StandardFormInfo -> Bool -> CLabel
-- thunkEntryLabel is a local help function, not exported.  It's used from both
-- entryLabelFromCI and getCallMethod.
thunkEntryLabel _thunk_id _ (ApThunk arity) upd_flag
  = enterApLabel upd_flag arity
thunkEntryLabel _thunk_id _ (SelectorThunk offset) upd_flag
  = enterSelectorLabel upd_flag offset
thunkEntryLabel thunk_id c _ _
  = enterIdLabel thunk_id c

enterApLabel :: Bool -> Arity -> CLabel
enterApLabel is_updatable arity
  | tablesNextToCode = mkApInfoTableLabel is_updatable arity
  | otherwise        = mkApEntryLabel is_updatable arity

enterSelectorLabel :: Bool -> WordOff -> CLabel
enterSelectorLabel upd_flag offset
  | tablesNextToCode = mkSelectorInfoLabel upd_flag offset
  | otherwise        = mkSelectorEntryLabel upd_flag offset

enterIdLabel :: Name -> CafInfo -> CLabel
enterIdLabel id c
  | tablesNextToCode = mkInfoTableLabel id c
  | otherwise        = mkEntryLabel id c

enterLocalIdLabel :: Name -> CafInfo -> CLabel
enterLocalIdLabel id c
  | tablesNextToCode = mkLocalInfoTableLabel id c
  | otherwise        = mkLocalEntryLabel id c


--------------------------------------
--   Profiling
--------------------------------------

-- Profiling requires two pieces of information to be determined for
-- each closure's info table --- description and type.

-- The description is stored directly in the @CClosureInfoTable@ when the
-- info table is built.

-- The type is determined from the type information stored with the @Id@
-- in the closure info using @closureTypeDescr@.

mkProfilingInfo :: Id -> String -> ProfilingInfo
mkProfilingInfo id val_descr
  | not opt_SccProfilingOn = NoProfilingInfo
  | otherwise = ProfilingInfo ty_descr_w8 val_descr_w8
  where
    ty_descr_w8  = stringToWord8s (getTyDescription (idType id))
    val_descr_w8 = stringToWord8s val_descr

getTyDescription :: Type -> String
getTyDescription ty
  = case (tcSplitSigmaTy ty) of { (_, _, tau_ty) ->
    case tau_ty of
      TyVarTy _	       	     -> "*"
      AppTy fun _      	     -> getTyDescription fun
      FunTy _ res      	     -> '-' : '>' : fun_result res
      TyConApp tycon _ 	     -> getOccString tycon
      PredTy sty	     -> getPredTyDescription sty
      ForAllTy _ ty          -> getTyDescription ty
    }
  where
    fun_result (FunTy _ res) = '>' : fun_result res
    fun_result other	     = getTyDescription other

getPredTyDescription :: PredType -> String
getPredTyDescription (ClassP cl _) = getOccString cl
getPredTyDescription (IParam ip _) = getOccString (ipNameName ip)
getPredTyDescription (EqPred {})   = "Type equality"

