-----------------------------------------------------------------------------
--
-- Stg to C-- code generation:
-- 
-- The types   LambdaFormInfo
--             ClosureInfo
--
-- Nothing monadic in here!
--
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module StgCmmClosure (
        DynTag,  tagForCon, isSmallFamily,
	ConTagZ, dataConTagZ,

        idPrimRep, isVoidRep, isGcPtrRep, addIdReps, addArgReps,
	argPrimRep,

        -- * LambdaFormInfo
        LambdaFormInfo,         -- Abstract
	StandardFormInfo,	-- ...ditto...
	mkLFThunk, mkLFReEntrant, mkConLFInfo, mkSelectorLFInfo,
	mkApLFInfo, mkLFImported, mkLFArgument, mkLFLetNoEscape,
        mkLFBlackHole,
        lfDynTag,
        maybeIsLFCon, isLFThunk, isLFReEntrant, lfUpdatable,

        nodeMustPointToIt,
        CallMethod(..), getCallMethod,

        isKnownFun, funTag, tagForArity,

        -- * ClosureInfo
	ClosureInfo,
        mkClosureInfo,
        mkCmmInfo,

        -- ** Inspection
        closureLFInfo, closureName,

        -- ** Labels
        -- These just need the info table label
        closureInfoLabel, staticClosureLabel,
        closureSlowEntryLabel, closureLocalEntryLabel,

        -- ** Predicates
        -- These are really just functions on LambdaFormInfo
        closureUpdReqd, closureSingleEntry,
        closureReEntrant, closureFunInfo,
        isToplevClosure,

        blackHoleOnEntry,  -- Needs LambdaFormInfo and SMRep
        isStaticClosure,   -- Needs SMPre

        -- * InfoTables
        mkDataConInfoTable,
        cafBlackHoleInfoTable,
        indStaticInfoTable,
        staticClosureNeedsLink,
    ) where

#include "../includes/MachDeps.h"

#define FAST_STRING_NOT_NEEDED
#include "HsVersions.h"

import StgSyn
import SMRep
import Cmm

import CLabel
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
import DynFlags
import Util

-----------------------------------------------------------------------------
--		Representations
-----------------------------------------------------------------------------

-- Why are these here?

-- NB: this is reliable because by StgCmm no Ids have unboxed tuple type
idPrimRep :: Id -> PrimRep
idPrimRep id = typePrimRep (idType id)

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
	!RepArity		-- Arity. Invariant: always > 0
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

                        -- XXX we can very nearly get rid of this, but
                        -- allocDynClosure needs a LambdaFormInfo


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
	RepArity		-- Arity, n


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
  | UnaryRep rep <- repType ty
  , Just tc <- tyConAppTyCon_maybe rep
  , isDataTyCon tc
  = False
  | otherwise
  = True

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
    arity = idRepArity id

------------
mkLFBlackHole :: LambdaFormInfo
mkLFBlackHole = LFBlackHole

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

isSmallFamily :: DynFlags -> Int -> Bool
isSmallFamily dflags fam_size = fam_size <= mAX_PTR_TAG dflags

-- We keep the *zero-indexed* tag in the srt_len field of the info
-- table of a data constructor.
dataConTagZ :: DataCon -> ConTagZ
dataConTagZ con = dataConTag con - fIRST_TAG

tagForCon :: DynFlags -> DataCon -> DynTag
tagForCon dflags con
  | isSmallFamily dflags fam_size = con_tag + 1
  | otherwise                     = 1
  where
    con_tag  = dataConTagZ con
    fam_size = tyConFamilySize (dataConTyCon con)

tagForArity :: DynFlags -> RepArity -> DynTag
tagForArity dflags arity
 | isSmallFamily dflags arity = arity
 | otherwise                  = 0

lfDynTag :: DynFlags -> LambdaFormInfo -> DynTag
-- Return the tag in the low order bits of a variable bound
-- to this LambdaForm
lfDynTag dflags (LFCon con)               = tagForCon dflags con
lfDynTag dflags (LFReEntrant _ arity _ _) = tagForArity dflags arity
lfDynTag _      _other                    = 0


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
lfClosureType (LFReEntrant _ arity _ argd) = Fun arity argd
lfClosureType (LFCon con)                  = Constr (dataConTagZ con)
                                                    (dataConIdentity con)
lfClosureType (LFThunk _ _ _ is_sel _)     = thunkClosureType is_sel
lfClosureType _                            = panic "lfClosureType"

thunkClosureType :: StandardFormInfo -> ClosureTypeInfo
thunkClosureType (SelectorThunk off) = ThunkSelector off
thunkClosureType _                   = Thunk

-- We *do* get non-updatable top-level thunks sometimes.  eg. f = g
-- gets compiled to a jump to g (if g has non-zero arity), instead of
-- messing around with update frames and PAPs.  We set the closure type
-- to FUN_STATIC in this case.

-----------------------------------------------------------------------------
--		nodeMustPointToIt
-----------------------------------------------------------------------------

nodeMustPointToIt :: DynFlags -> LambdaFormInfo -> Bool
nodeMustPointToIt _ (LFReEntrant top _ no_fvs _)
  = not no_fvs ||   -- Certainly if it has fvs we need to point to it
    isNotTopLevel top
		    -- If it is not top level we will point to it
		    --   We can have a \r closure with no_fvs which
		    --   is not top level as special case cgRhsClosure
		    --   has been dissabled in favour of let floating

		-- For lex_profiling we also access the cost centre for a
		-- non-inherited function i.e. not top level
		-- the  not top  case above ensures this is ok.

nodeMustPointToIt _ (LFCon _) = True

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

nodeMustPointToIt dflags (LFThunk _ no_fvs updatable NonStandardThunk _)
  = updatable || not no_fvs || gopt Opt_SccProfilingOn dflags
	  -- For the non-updatable (single-entry case):
	  --
	  -- True if has fvs (in which case we need access to them, and we
	  --		    should black-hole it)
	  -- or profiling (in which case we need to recover the cost centre
	  --		 from inside it)

nodeMustPointToIt _ (LFThunk {})	-- Node must point to a standard-form thunk
  = True 

nodeMustPointToIt _ (LFUnknown _)   = True
nodeMustPointToIt _ LFUnLifted      = False
nodeMustPointToIt _ LFBlackHole     = True    -- BH entry may require Node to point
nodeMustPointToIt _ LFLetNoEscape   = False 

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
	RepArity 		--   Its arity

getCallMethod :: DynFlags
              -> Name           -- Function being applied
              -> CafInfo        -- Can it refer to CAF's?
	      -> LambdaFormInfo	-- Its info
	      -> RepArity		-- Number of available arguments
	      -> CallMethod

getCallMethod dflags _name _ lf_info _n_args
  | nodeMustPointToIt dflags lf_info && gopt Opt_Parallel dflags
  =	-- If we're parallel, then we must always enter via node.  
	-- The reason is that the closure may have been 	
	-- fetched since we allocated it.
    EnterIt

getCallMethod dflags name caf (LFReEntrant _ arity _ _) n_args
  | n_args == 0    = ASSERT( arity /= 0 )
		     ReturnIt	-- No args at all
  | n_args < arity = SlowCall	-- Not enough args
  | otherwise      = DirectEntry (enterIdLabel dflags name caf) arity

getCallMethod _ _name _ LFUnLifted n_args
  = ASSERT( n_args == 0 ) ReturnIt

getCallMethod _ _name _ (LFCon _) n_args
  = ASSERT( n_args == 0 ) ReturnIt

getCallMethod dflags name caf (LFThunk _ _ updatable std_form_info is_fun) n_args
  | is_fun 	-- it *might* be a function, so we must "call" it (which is always safe)
  = SlowCall	-- We cannot just enter it [in eval/apply, the entry code
		-- is the fast-entry code]

  -- Since is_fun is False, we are *definitely* looking at a data value
  | updatable || gopt Opt_Ticky dflags -- to catch double entry
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
    DirectEntry (thunkEntryLabel dflags name caf std_form_info updatable) 0

getCallMethod _ _name _ (LFUnknown True) _n_args
  = SlowCall -- might be a function

getCallMethod _ name _ (LFUnknown False) n_args
  = ASSERT2( n_args == 0, ppr name <+> ppr n_args )
    EnterIt -- Not a function

getCallMethod _ _name _ LFBlackHole _n_args
  = SlowCall	-- Presumably the black hole has by now
		-- been updated, but we don't know with
		-- what, so we slow call it

getCallMethod _ _name _ LFLetNoEscape _n_args
  = JumpToIt

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
--              Data types for closure information
-----------------------------------------------------------------------------


{- ClosureInfo: information about a binding

   We make a ClosureInfo for each let binding (both top level and not),
   but not bindings for data constructors: for those we build a CmmInfoTable
   directly (see mkDataConInfoTable).

   To a first approximation:
       ClosureInfo = (LambdaFormInfo, CmmInfoTable)

   A ClosureInfo has enough information
     a) to construct the info table itself, and build other things
        related to the binding (e.g. slow entry points for a function)
     b) to allocate a closure containing that info pointer (i.e.
   	it knows the info table label)
-}

data ClosureInfo
  = ClosureInfo {
        closureName :: !Name,           -- The thing bound to this closure
           -- we don't really need this field: it's only used in generating
           -- code for ticky and profiling, and we could pass the information
           -- around separately, but it doesn't do much harm to keep it here.

        closureLFInfo :: !LambdaFormInfo, -- NOTE: not an LFCon
          -- this tells us about what the closure contains: it's right-hand-side.

          -- the rest is just an unpacked CmmInfoTable.
        closureInfoLabel :: !CLabel,
        closureSMRep     :: !SMRep,          -- representation used by storage mgr
        closureProf      :: !ProfilingInfo
    }

-- | Convert from 'ClosureInfo' to 'CmmInfoTable'.
mkCmmInfo :: ClosureInfo -> CmmInfoTable
mkCmmInfo ClosureInfo {..}
  = CmmInfoTable { cit_lbl  = closureInfoLabel
                 , cit_rep  = closureSMRep
                 , cit_prof = closureProf
                 , cit_srt  = NoC_SRT }


--------------------------------------
--	Building ClosureInfos
--------------------------------------

mkClosureInfo :: DynFlags
              -> Bool		-- Is static
	      -> Id
	      -> LambdaFormInfo 
	      -> Int -> Int	-- Total and pointer words
              -> String         -- String descriptor
	      -> ClosureInfo
mkClosureInfo dflags is_static id lf_info tot_wds ptr_wds val_descr
  = ClosureInfo { closureName      = name,
                  closureLFInfo    = lf_info,
                  closureInfoLabel = info_lbl,  -- These three fields are
                  closureSMRep     = sm_rep,    -- (almost) an info table
                  closureProf      = prof }     -- (we don't have an SRT yet)
  where
    name       = idName id
    sm_rep     = mkHeapRep dflags is_static ptr_wds nonptr_wds (lfClosureType lf_info)
    prof       = mkProfilingInfo dflags id val_descr
    nonptr_wds = tot_wds - ptr_wds

    info_lbl = mkClosureInfoTableLabel id lf_info

--------------------------------------
--   Other functions over ClosureInfo
--------------------------------------

-- Eager blackholing is normally disabled, but can be turned on with
-- -feager-blackholing.  When it is on, we replace the info pointer of
-- the thunk with stg_EAGER_BLACKHOLE_info on entry.

-- If we wanted to do eager blackholing with slop filling,
-- we'd need to do it at the *end* of a basic block, otherwise
-- we overwrite the free variables in the thunk that we still
-- need.  We have a patch for this from Andy Cheadle, but not
-- incorporated yet. --SDM [6/2004]
--
--
-- Previously, eager blackholing was enabled when ticky-ticky
-- was on. But it didn't work, and it wasn't strictly necessary 
-- to bring back minimal ticky-ticky, so now EAGER_BLACKHOLING 
-- is unconditionally disabled. -- krc 1/2007

-- Static closures are never themselves black-holed.

blackHoleOnEntry :: ClosureInfo -> Bool
blackHoleOnEntry cl_info
  | isStaticRep (closureSMRep cl_info)
  = False	-- Never black-hole a static closure

  | otherwise
  = case closureLFInfo cl_info of
	LFReEntrant _ _ _ _	  -> False
	LFLetNoEscape 		  -> False
        LFThunk _ _no_fvs _updatable _ _ -> True
        _other -> panic "blackHoleOnEntry"      -- Should never happen

isStaticClosure :: ClosureInfo -> Bool
isStaticClosure cl_info = isStaticRep (closureSMRep cl_info)

closureUpdReqd :: ClosureInfo -> Bool
closureUpdReqd ClosureInfo{ closureLFInfo = lf_info } = lfUpdatable lf_info

lfUpdatable :: LambdaFormInfo -> Bool
lfUpdatable (LFThunk _ _ upd _ _)  = upd
lfUpdatable LFBlackHole 	   = True
	-- Black-hole closures are allocated to receive the results of an
	-- alg case with a named default... so they need to be updated.
lfUpdatable _ = False

closureSingleEntry :: ClosureInfo -> Bool
closureSingleEntry (ClosureInfo { closureLFInfo = LFThunk _ _ upd _ _}) = not upd
closureSingleEntry _ = False

closureReEntrant :: ClosureInfo -> Bool
closureReEntrant (ClosureInfo { closureLFInfo = LFReEntrant _ _ _ _ }) = True
closureReEntrant _ = False

closureFunInfo :: ClosureInfo -> Maybe (RepArity, ArgDescr)
closureFunInfo (ClosureInfo { closureLFInfo = lf_info }) = lfFunInfo lf_info

lfFunInfo :: LambdaFormInfo ->  Maybe (RepArity, ArgDescr)
lfFunInfo (LFReEntrant _ arity _ arg_desc)  = Just (arity, arg_desc)
lfFunInfo _                                 = Nothing

funTag :: DynFlags -> ClosureInfo -> DynTag
funTag dflags (ClosureInfo { closureLFInfo = lf_info })
    = lfDynTag dflags lf_info

isToplevClosure :: ClosureInfo -> Bool
isToplevClosure (ClosureInfo { closureLFInfo = lf_info })
  = case lf_info of
      LFReEntrant TopLevel _ _ _ -> True
      LFThunk TopLevel _ _ _ _   -> True
      _other			 -> False

--------------------------------------
--   Label generation
--------------------------------------

staticClosureLabel :: ClosureInfo -> CLabel
staticClosureLabel = toClosureLbl .  closureInfoLabel

closureSlowEntryLabel :: ClosureInfo -> CLabel
closureSlowEntryLabel = toSlowEntryLbl . closureInfoLabel

closureLocalEntryLabel :: DynFlags -> ClosureInfo -> CLabel
closureLocalEntryLabel dflags
  | tablesNextToCode dflags = toInfoLbl  . closureInfoLabel
  | otherwise               = toEntryLbl . closureInfoLabel

mkClosureInfoTableLabel :: Id -> LambdaFormInfo -> CLabel
mkClosureInfoTableLabel id lf_info
  = case lf_info of
        LFBlackHole -> mkCAFBlackHoleInfoTableLabel

	LFThunk _ _ upd_flag (SelectorThunk offset) _ 
                      -> mkSelectorInfoLabel upd_flag offset

	LFThunk _ _ upd_flag (ApThunk arity) _ 
                      -> mkApInfoTableLabel upd_flag arity

        LFThunk{}     -> std_mk_lbl name cafs
        LFReEntrant{} -> std_mk_lbl name cafs
        _other        -> panic "closureInfoTableLabel"

  where 
    name = idName id

    std_mk_lbl | is_local  = mkLocalInfoTableLabel
               | otherwise = mkInfoTableLabel

    cafs     = idCafInfo id
    is_local = isDataConWorkId id
       -- Make the _info pointer for the implicit datacon worker
       -- binding local. The reason we can do this is that importing
       -- code always either uses the _closure or _con_info. By the
       -- invariants in CorePrep anything else gets eta expanded.


thunkEntryLabel :: DynFlags -> Name -> CafInfo -> StandardFormInfo -> Bool -> CLabel
-- thunkEntryLabel is a local help function, not exported.  It's used from
-- getCallMethod.
thunkEntryLabel dflags _thunk_id _ (ApThunk arity) upd_flag
  = enterApLabel dflags upd_flag arity
thunkEntryLabel dflags _thunk_id _ (SelectorThunk offset) upd_flag
  = enterSelectorLabel dflags upd_flag offset
thunkEntryLabel dflags thunk_id c _ _
  = enterIdLabel dflags thunk_id c

enterApLabel :: DynFlags -> Bool -> Arity -> CLabel
enterApLabel dflags is_updatable arity
  | tablesNextToCode dflags = mkApInfoTableLabel is_updatable arity
  | otherwise               = mkApEntryLabel is_updatable arity

enterSelectorLabel :: DynFlags -> Bool -> WordOff -> CLabel
enterSelectorLabel dflags upd_flag offset
  | tablesNextToCode dflags = mkSelectorInfoLabel upd_flag offset
  | otherwise               = mkSelectorEntryLabel upd_flag offset

enterIdLabel :: DynFlags -> Name -> CafInfo -> CLabel
enterIdLabel dflags id c
  | tablesNextToCode dflags = mkInfoTableLabel id c
  | otherwise               = mkEntryLabel id c


--------------------------------------
--   Profiling
--------------------------------------

-- Profiling requires two pieces of information to be determined for
-- each closure's info table --- description and type.

-- The description is stored directly in the @CClosureInfoTable@ when the
-- info table is built.

-- The type is determined from the type information stored with the @Id@
-- in the closure info using @closureTypeDescr@.

mkProfilingInfo :: DynFlags -> Id -> String -> ProfilingInfo
mkProfilingInfo dflags id val_descr
  | not (gopt Opt_SccProfilingOn dflags) = NoProfilingInfo
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
      ForAllTy _ ty          -> getTyDescription ty
      LitTy n                -> getTyLitDescription n
    }
  where
    fun_result (FunTy _ res) = '>' : fun_result res
    fun_result other	     = getTyDescription other

getTyLitDescription :: TyLit -> String
getTyLitDescription l =
  case l of
    NumTyLit n -> show n
    StrTyLit n -> show n

--------------------------------------
--   CmmInfoTable-related things
--------------------------------------

mkDataConInfoTable :: DynFlags -> DataCon -> Bool -> Int -> Int -> CmmInfoTable
mkDataConInfoTable dflags data_con is_static ptr_wds nonptr_wds
 = CmmInfoTable { cit_lbl  = info_lbl
                , cit_rep  = sm_rep
                , cit_prof = prof
                , cit_srt  = NoC_SRT }
 where
   name = dataConName data_con

   info_lbl | is_static = mkStaticInfoTableLabel name NoCafRefs
            | otherwise = mkConInfoTableLabel    name NoCafRefs

   sm_rep = mkHeapRep dflags is_static ptr_wds nonptr_wds cl_type

   cl_type = Constr (dataConTagZ data_con) (dataConIdentity data_con)

   prof | not (gopt Opt_SccProfilingOn dflags) = NoProfilingInfo
        | otherwise                            = ProfilingInfo ty_descr val_descr

   ty_descr  = stringToWord8s $ occNameString $ getOccName $ dataConTyCon data_con
   val_descr = stringToWord8s $ occNameString $ getOccName data_con

-- We need a black-hole closure info to pass to @allocDynClosure@ when we
-- want to allocate the black hole on entry to a CAF.

cafBlackHoleInfoTable :: CmmInfoTable
cafBlackHoleInfoTable
  = CmmInfoTable { cit_lbl  = mkCAFBlackHoleInfoTableLabel
                 , cit_rep  = blackHoleRep
                 , cit_prof = NoProfilingInfo
                 , cit_srt  = NoC_SRT }

indStaticInfoTable :: CmmInfoTable
indStaticInfoTable
  = CmmInfoTable { cit_lbl  = mkIndStaticInfoLabel
                 , cit_rep  = indStaticRep
                 , cit_prof = NoProfilingInfo
                 , cit_srt  = NoC_SRT }

staticClosureNeedsLink :: Bool -> CmmInfoTable -> Bool
-- A static closure needs a link field to aid the GC when traversing
-- the static closure graph.  But it only needs such a field if either
-- 	a) it has an SRT
--	b) it's a constructor with one or more pointer fields
-- In case (b), the constructor's fields themselves play the role
-- of the SRT.
--
-- At this point, the cit_srt field has not been calculated (that
-- happens right at the end of the Cmm pipeline), but we do have the
-- VarSet of CAFs that CoreToStg attached, and if that is empty there
-- will definitely not be an SRT.
--
staticClosureNeedsLink has_srt CmmInfoTable{ cit_rep = smrep }
  | isConRep smrep         = not (isStaticNoCafCon smrep)
  | otherwise              = has_srt -- needsSRT (cit_srt info_tbl)
