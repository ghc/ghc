%
% (c) The University of Glasgow 2006
% (c) The Univserity of Glasgow 1992-2004
%

	Data structures which describe closures, and
	operations over those data structures

		Nothing monadic in here

Much of the rationale for these things is in the ``details'' part of
the STG paper.

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module ClosureInfo (
        idRepArity,

	ClosureInfo(..), LambdaFormInfo(..),	-- would be abstract but
	StandardFormInfo(..),			-- mkCmmInfo looks inside
        SMRep,

	ArgDescr(..), Liveness, 
	C_SRT(..), needsSRT,

	mkLFThunk, mkLFReEntrant, mkConLFInfo, mkSelectorLFInfo,
	mkApLFInfo, mkLFImported, mkLFArgument, mkLFLetNoEscape,

	mkClosureInfo, mkConInfo, maybeIsLFCon,
        closureSize,

	ConTagZ, dataConTagZ,

	infoTableLabelFromCI, entryLabelFromCI,
	closureLabelFromCI,
	isLFThunk, closureUpdReqd,
	closureNeedsUpdSpace, closureIsThunk,
	closureSingleEntry, closureReEntrant, isConstrClosure_maybe,
        closureFunInfo, isKnownFun,
        funTag, funTagLFInfo, tagForArity, clHasCafRefs,

        enterIdLabel, enterReturnPtLabel,

	nodeMustPointToIt, 
	CallMethod(..), getCallMethod,

	blackHoleOnEntry,

	staticClosureRequired,

	isToplevClosure,
	closureValDescr, closureTypeDescr,	-- profiling

	isStaticClosure,
	cafBlackHoleClosureInfo,

	staticClosureNeedsLink,

	-- CgRep and its functions
	CgRep(..), nonVoidArg,
	argMachRep, primRepToCgRep, 
	isFollowableArg, isVoidArg, 
	isFloatingArg, is64BitArg,
	separateByPtrFollowness,
	cgRepSizeW, cgRepSizeB,
	retAddrSizeW,
	typeCgRep, idCgRep, tyConCgRep, 

    ) where

#include "../includes/MachDeps.h"
#include "HsVersions.h"

import StgSyn
import SMRep

import CLabel
import Cmm
import Unique
import StaticFlags
import Var
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
import FastString
import Constants
import DynFlags
import Util
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
        closureType   :: !Type,           -- Type of closure (ToDo: remove)
	closureDescr  :: !String,	  -- closure description (for profiling)
	closureInfLcl :: Bool             -- can the info pointer be a local symbol?
    }

  -- Constructor closures don't have a unique info table label (they use
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
	!RepArity	-- Arity. Invariant: always > 0
	!Bool		-- True <=> no fvs
	ArgDescr	-- Argument descriptor (should reall be in ClosureInfo)

  | LFCon		-- A saturated constructor application
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
	!RepArity	-- arity;

  | LFBlackHole		-- Used for the closures allocated to hold the result
			-- of a CAF.  We want the target of the update frame to
			-- be in the heap, so we make a black hole to hold it.



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
       WordOff             	-- 0-origin offset of ak within the "goods" of 
			-- constructor (Recall that the a1,...,an may be laid
			-- out in the heap in a non-obvious order.)

  | ApThunk 
	-- An ApThunk is of form
	--	x1 ... xn
	-- The code for the thunk just pushes x2..xn on the stack and enters x1.
	-- There are a few of these (for 1 <= n <= MAX_SPEC_AP_SIZE) pre-compiled
	-- in the RTS to save space.
	RepArity	-- Arity, n
\end{code}


%************************************************************************
%*									*
			CgRep
%*									*
%************************************************************************

An CgRep is an abstraction of a Type which tells the code generator
all it needs to know about the calling convention for arguments (and
results) of that type.  In particular, the ArgReps of a function's
arguments are used to decide which of the RTS's generic apply
functions to call when applying an unknown function.

It contains more information than the back-end data type MachRep,
so one can easily convert from CgRep -> MachRep.  (Except that
there's no MachRep for a VoidRep.)

It distinguishes 
	pointers from non-pointers (we sort the pointers together
	when building closures)

	void from other types: a void argument is different from no argument

All 64-bit types map to the same CgRep, because they're passed in the
same register, but a PtrArg is still different from an NonPtrArg
because the function's entry convention has to take into account the
pointer-hood of arguments for the purposes of describing the stack on
entry to the garbage collector.

\begin{code}
data CgRep 
  = VoidArg 	-- Void
  | PtrArg 	-- Word-sized heap pointer, followed
		-- by the garbage collector
  | NonPtrArg 	-- Word-sized non-pointer
		-- (including addresses not followed by GC)
  | LongArg	-- 64-bit non-pointer
  | FloatArg 	-- 32-bit float
  | DoubleArg 	-- 64-bit float
  deriving Eq

instance Outputable CgRep where
    ppr VoidArg   = ptext (sLit "V_")
    ppr PtrArg    = ptext (sLit "P_")
    ppr NonPtrArg = ptext (sLit "I_")
    ppr LongArg   = ptext (sLit "L_")
    ppr FloatArg  = ptext (sLit "F_")
    ppr DoubleArg = ptext (sLit "D_")

argMachRep :: CgRep -> CmmType
argMachRep PtrArg    = gcWord
argMachRep NonPtrArg = bWord
argMachRep LongArg   = b64
argMachRep FloatArg  = f32
argMachRep DoubleArg = f64
argMachRep VoidArg   = panic "argMachRep:VoidRep"

primRepToCgRep :: PrimRep -> CgRep
primRepToCgRep VoidRep    = VoidArg
primRepToCgRep PtrRep     = PtrArg
primRepToCgRep IntRep	  = NonPtrArg
primRepToCgRep WordRep	  = NonPtrArg
primRepToCgRep Int64Rep   = LongArg
primRepToCgRep Word64Rep  = LongArg
primRepToCgRep AddrRep    = NonPtrArg
primRepToCgRep FloatRep   = FloatArg
primRepToCgRep DoubleRep  = DoubleArg

idCgRep :: Id -> CgRep
idCgRep x = typeCgRep . idType $ x

tyConCgRep :: TyCon -> CgRep
tyConCgRep = primRepToCgRep . tyConPrimRep

typeCgRep :: UnaryType -> CgRep
typeCgRep = primRepToCgRep . typePrimRep 
\end{code}

Whether or not the thing is a pointer that the garbage-collector
should follow. Or, to put it another (less confusing) way, whether
the object in question is a heap object. 

Depending on the outcome, this predicate determines what stack
the pointer/object possibly will have to be saved onto, and the
computation of GC liveness info.

\begin{code}
isFollowableArg :: CgRep -> Bool  -- True <=> points to a heap object
isFollowableArg PtrArg  = True
isFollowableArg _       = False

isVoidArg :: CgRep -> Bool
isVoidArg VoidArg = True
isVoidArg _       = False

nonVoidArg :: CgRep -> Bool
nonVoidArg VoidArg = False
nonVoidArg _       = True

-- isFloatingArg is used to distinguish @Double@ and @Float@ which
-- cause inadvertent numeric conversions if you aren't jolly careful.
-- See codeGen/CgCon:cgTopRhsCon.

isFloatingArg :: CgRep -> Bool
isFloatingArg DoubleArg = True
isFloatingArg FloatArg  = True
isFloatingArg _         = False

is64BitArg :: CgRep -> Bool
is64BitArg LongArg = True
is64BitArg _       = False
\end{code}

\begin{code}
separateByPtrFollowness :: [(CgRep,a)] -> ([(CgRep,a)], [(CgRep,a)])
-- Returns (ptrs, non-ptrs)
separateByPtrFollowness things
  = sep_things things [] []
    -- accumulating params for follow-able and don't-follow things...
  where
    sep_things []    	       bs us = (reverse bs, reverse us)
    sep_things ((PtrArg,a):ts) bs us = sep_things ts ((PtrArg,a):bs) us
    sep_things (t         :ts) bs us = sep_things ts bs		     (t:us)
\end{code}

\begin{code}
cgRepSizeB :: CgRep -> ByteOff
cgRepSizeB DoubleArg = dOUBLE_SIZE
cgRepSizeB LongArg   = wORD64_SIZE
cgRepSizeB VoidArg   = 0
cgRepSizeB _         = wORD_SIZE

cgRepSizeW :: CgRep -> ByteOff
cgRepSizeW DoubleArg = dOUBLE_SIZE `quot` wORD_SIZE
cgRepSizeW LongArg   = wORD64_SIZE `quot` wORD_SIZE
cgRepSizeW VoidArg   = 0
cgRepSizeW _         = 1

retAddrSizeW :: WordOff
retAddrSizeW = 1	-- One word
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-construction]{Functions which build LFInfos}
%*									*
%************************************************************************

\begin{code}
mkLFReEntrant :: TopLevelFlag	-- True of top level
	      -> [Id]		-- Free vars
	      -> [Id] 		-- Args
	      -> ArgDescr	-- Argument descriptor
	      -> LambdaFormInfo

mkLFReEntrant top fvs args arg_descr 
  = LFReEntrant top (length args) (null fvs) arg_descr

mkLFThunk :: Type -> TopLevelFlag -> [Var] -> UpdateFlag -> LambdaFormInfo
mkLFThunk thunk_ty top fvs upd_flag
  = ASSERT2( not (isUpdatable upd_flag) || not (isUnLiftedType thunk_ty), ppr thunk_ty $$ ppr fvs )
    LFThunk top (null fvs) 
	    (isUpdatable upd_flag)
	    NonStandardThunk 
	    (might_be_a_function thunk_ty)

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
\end{code}

@mkConLFInfo@ is similar, for constructors.

\begin{code}
mkConLFInfo :: DataCon -> LambdaFormInfo
mkConLFInfo con = LFCon con

maybeIsLFCon :: LambdaFormInfo -> Maybe DataCon
maybeIsLFCon (LFCon con) = Just con
maybeIsLFCon _ = Nothing

mkSelectorLFInfo :: Id -> WordOff -> Bool -> LambdaFormInfo
mkSelectorLFInfo id offset updatable
  = LFThunk NotTopLevel False updatable (SelectorThunk offset) 
	(might_be_a_function (idType id))

mkApLFInfo :: Id -> UpdateFlag -> RepArity -> LambdaFormInfo
mkApLFInfo id upd_flag arity
  = LFThunk NotTopLevel (arity == 0) (isUpdatable upd_flag) (ApThunk arity)
	(might_be_a_function (idType id))
\end{code}

Miscellaneous LF-infos.

\begin{code}
mkLFArgument :: Id -> LambdaFormInfo
mkLFArgument id = LFUnknown (might_be_a_function (idType id))

mkLFLetNoEscape :: RepArity -> LambdaFormInfo
mkLFLetNoEscape = LFLetNoEscape

mkLFImported :: Id -> LambdaFormInfo
mkLFImported id
  = case idRepArity id of
      n | n > 0 -> LFReEntrant TopLevel n True (panic "arg_descr")  -- n > 0
      _ -> mkLFArgument id -- Not sure of exact arity
\end{code}

\begin{code}
isLFThunk :: LambdaFormInfo -> Bool
isLFThunk (LFThunk _ _ _ _ _)  = True
isLFThunk LFBlackHole          = True
	-- return True for a blackhole: this function is used to determine
	-- whether to use the thunk header in SMP mode, and a blackhole
	-- must have one.
isLFThunk _ = False
\end{code}

\begin{code}
-- We keep the *zero-indexed* tag in the srt_len field of the info
-- table of a data constructor.
type ConTagZ = Int	-- A *zero-indexed* contructor tag

dataConTagZ :: DataCon -> ConTagZ
dataConTagZ con = dataConTag con - fIRST_TAG
\end{code}


%************************************************************************
%*									*
	Building ClosureInfos
%*									*
%************************************************************************

\begin{code}
mkClosureInfo :: Bool		-- Is static
	      -> Id
	      -> LambdaFormInfo 
	      -> Int -> Int	-- Total and pointer words
	      -> C_SRT
	      -> String		-- String descriptor
	      -> ClosureInfo
mkClosureInfo is_static id lf_info tot_wds ptr_wds srt_info descr
  = ClosureInfo { closureName = name, 
		  closureLFInfo = lf_info,
		  closureSMRep = sm_rep, 
		  closureSRT = srt_info,
		  closureType = idType id,
		  closureDescr = descr,
		  closureInfLcl = isDataConWorkId id }
		    -- Make the _info pointer for the implicit datacon worker binding
		    -- local. The reason we can do this is that importing code always
		    -- either uses the _closure or _con_info. By the invariants in CorePrep
		    -- anything else gets eta expanded.
  where
    name   = idName id
    sm_rep = mkHeapRep is_static ptr_wds nonptr_wds (lfClosureType lf_info)
    nonptr_wds = tot_wds - ptr_wds

mkConInfo :: Bool	-- Is static
	  -> DataCon	
	  -> Int -> Int	-- Total and pointer words
	  -> ClosureInfo
mkConInfo is_static data_con tot_wds ptr_wds
   = ConInfo {	closureSMRep = sm_rep,
		closureCon = data_con }
  where
    sm_rep  = mkHeapRep is_static ptr_wds nonptr_wds (lfClosureType lf_info)
    lf_info = mkConLFInfo data_con
    nonptr_wds = tot_wds - ptr_wds
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-sizes]{Functions about closure {\em sizes}}
%*									*
%************************************************************************

\begin{code}
closureSize :: ClosureInfo -> WordOff
closureSize cl_info = heapClosureSize (closureSMRep cl_info)
\end{code}

\begin{code}
-- we leave space for an update if either (a) the closure is updatable
-- or (b) it is a static thunk.  This is because a static thunk needs
-- a static link field in a predictable place (after the slop), regardless
-- of whether it is updatable or not.
closureNeedsUpdSpace :: ClosureInfo -> Bool
closureNeedsUpdSpace (ClosureInfo { closureLFInfo = 
					LFThunk TopLevel _ _ _ _ }) = True
closureNeedsUpdSpace cl_info = closureUpdReqd cl_info
\end{code}

%************************************************************************
%*									*
\subsection[SMreps]{Choosing SM reps}
%*									*
%************************************************************************

\begin{code}
lfClosureType :: LambdaFormInfo -> ClosureTypeInfo
lfClosureType (LFReEntrant _ arity _ argd) = Fun (fromIntegral arity) argd
lfClosureType (LFCon con)                  = Constr (fromIntegral (dataConTagZ con))
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
\end{code}

%************************************************************************
%*									*
\subsection[ClosureInfo-4-questions]{Four major questions about @ClosureInfo@}
%*									*
%************************************************************************

Be sure to see the stg-details notes about these...

\begin{code}
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

nodeMustPointToIt (LFThunk _ _ _ _ _)
  = True  -- Node must point to any standard-form thunk

nodeMustPointToIt (LFUnknown _)     = True
nodeMustPointToIt LFBlackHole       = True    -- BH entry may require Node to point
nodeMustPointToIt (LFLetNoEscape _) = False 
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
data CallMethod
  = EnterIt				-- no args, not a function

  | JumpToIt CLabel			-- no args, not a function, but we
					-- know what its entry code is

  | ReturnIt				-- it's a function, but we have
					-- zero args to apply to it, so just
					-- return it.

  | ReturnCon DataCon			-- It's a data constructor, just return it

  | SlowCall				-- Unknown fun, or known fun with
					-- too few args.

  | DirectEntry 			-- Jump directly, with args in regs
	CLabel 				--   The code label
	RepArity			--   Its arity

getCallMethod :: DynFlags
              -> Name		-- Function being applied
              -> CafInfo        -- Can it refer to CAF's?
	      -> LambdaFormInfo	-- Its info
	      -> RepArity	-- Number of available arguments
	      -> CallMethod

getCallMethod dflags _ _ lf_info _
  | nodeMustPointToIt lf_info && dopt Opt_Parallel dflags
  =	-- If we're parallel, then we must always enter via node.  
	-- The reason is that the closure may have been 	
	-- fetched since we allocated it.
    EnterIt

getCallMethod _ name caf (LFReEntrant _ arity _ _) n_args
  | n_args == 0    = ASSERT( arity /= 0 )
		     ReturnIt	-- No args at all
  | n_args < arity = SlowCall	-- Not enough args
  | otherwise      = DirectEntry (enterIdLabel name caf) arity

getCallMethod _ _ _ (LFCon con) n_args
  | opt_SccProfilingOn     -- when profiling, we must always enter
  = EnterIt                -- a closure when we use it, so that the closure
                           -- can be recorded as used for LDV profiling.
  | otherwise
  = ASSERT( n_args == 0 )
    ReturnCon con

getCallMethod _dflags _name _caf (LFThunk _ _ _updatable _std_form_info is_fun) _n_args
  | is_fun 	-- it *might* be a function, so we must "call" it (which is
                -- always safe)
  = SlowCall	-- We cannot just enter it [in eval/apply, the entry code
		-- is the fast-entry code]

  -- Since is_fun is False, we are *definitely* looking at a data value
  | otherwise
  = EnterIt
    -- We used to have ASSERT( n_args == 0 ), but actually it is
    -- possible for the optimiser to generate
    --   let bot :: Int = error Int "urk"
    --   in (bot `cast` unsafeCoerce Int (Int -> Int)) 3
    -- This happens as a result of the case-of-error transformation
    -- So the right thing to do is just to enter the thing

-- Old version:
--  | updatable || doingTickyProfiling dflags -- to catch double entry
--  = EnterIt
--  | otherwise	-- Jump direct to code for single-entry thunks
--  = JumpToIt (thunkEntryLabel name caf std_form_info updatable)
--
-- Now we never use JumpToIt, even if the thunk is single-entry, since
-- the thunk may have already been entered and blackholed by another
-- processor.


getCallMethod _ _ _ (LFUnknown True) _
  = SlowCall -- Might be a function

getCallMethod _ name _ (LFUnknown False) n_args
  | n_args > 0 
  = WARN( True, ppr name <+> ppr n_args ) 
    SlowCall	-- Note [Unsafe coerce complications]

  | otherwise
  = EnterIt -- Not a function

getCallMethod _ _ _ LFBlackHole _
  = SlowCall	-- Presumably the black hole has by now
		-- been updated, but we don't know with
		-- what, so we slow call it

getCallMethod _ name _ (LFLetNoEscape 0) _
  = JumpToIt (enterReturnPtLabel (nameUnique name))

getCallMethod _ name _ (LFLetNoEscape arity) n_args
  | n_args == arity = DirectEntry (enterReturnPtLabel (nameUnique name)) arity
  | otherwise = pprPanic "let-no-escape: " (ppr name <+> ppr arity)


blackHoleOnEntry :: ClosureInfo -> Bool
blackHoleOnEntry ConInfo{} = False
blackHoleOnEntry cl_info
  | isStaticRep (closureSMRep cl_info)
  = False	-- Never black-hole a static closure

  | otherwise
  = case closureLFInfo cl_info of
	LFReEntrant _ _ _ _	  -> False
        LFLetNoEscape _           -> False
        LFThunk _ _no_fvs _updatable _ _ -> True
        _other -> panic "blackHoleOnEntry"      -- Should never happen

isKnownFun :: LambdaFormInfo -> Bool
isKnownFun (LFReEntrant _ _ _ _) = True
isKnownFun (LFLetNoEscape _) = True
isKnownFun _ = False
\end{code}

Note [Unsafe coerce complications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In some (badly-optimised) DPH code we see this
   Module X:    rr :: Int = error Int "Urk"
   Module Y:    ...((X.rr |> g) True) ...
     where g is an (unsafe) coercion of kind (Int ~ Bool->Bool), say

It's badly optimised, because knowing that 'X.rr' is bottom, we should
have dumped the application to True.  But it should still work. These
strange unsafe coercions arise from the case-of-error transformation:
	(case (error Int "foo") of { ... }) True
--->	(error Int "foo" |> g) True

Anyway, the net effect is that in STG-land, when casts are discarded,
we *can* see a value of type Int applied to an argument.  This only happens
if (a) the programmer made a mistake, or (b) the value of type Int is
actually bottom.

So it's wrong to trigger an ASSERT failure in this circumstance.  Instead
we now emit a WARN -- mainly to draw attention to a probably-badly-optimised
program fragment -- and do the conservative thing which is SlowCall.


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
staticClosureNeedsLink (ConInfo { closureSMRep = rep })
  = not (isStaticNoCafCon rep)
\end{code}

Note [Entering error thunks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this

	fail :: Int
	fail = error Int "Urk"

	foo :: Bool -> Bool 
	foo True  y = (fail `cast` Bool -> Bool) y
	foo False y = False

This looks silly, but it can arise from case-of-error.  Even if it
does, we'd usually see that 'fail' is a bottoming function and would
discard the extra argument 'y'.  But even if that does not occur,
this program is still OK.  We will enter 'fail', which never returns.

The WARN is just to alert me to the fact that we aren't spotting that
'fail' is bottoming.

(We are careful never to make a funtion value look like a data type,
because we can't enter a function closure -- but that is not the 
problem here.)


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
staticClosureRequired _ bndr_info
		      (LFReEntrant top_level _ _ _)	-- It's a function
  = ASSERT( isTopLevel top_level )
	-- Assumption: it's a top-level, no-free-var binding
	not (satCallsOnly bndr_info)

staticClosureRequired _ _ _ = True
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

closureFunInfo :: ClosureInfo -> Maybe (RepArity, ArgDescr)
closureFunInfo (ClosureInfo { closureLFInfo = lf_info }) = lfFunInfo lf_info
closureFunInfo _ = Nothing

lfFunInfo :: LambdaFormInfo ->  Maybe (RepArity, ArgDescr)
lfFunInfo (LFReEntrant _ arity _ arg_desc)  = Just (arity, arg_desc)
lfFunInfo _                                 = Nothing

funTag :: ClosureInfo -> Int
funTag (ClosureInfo { closureLFInfo = lf_info }) = funTagLFInfo lf_info
funTag _ = 0

-- maybe this should do constructor tags too?
funTagLFInfo :: LambdaFormInfo -> Int
funTagLFInfo lf
    -- A function is tagged with its arity
  | Just (arity,_) <- lfFunInfo lf,
    Just tag <- tagForArity arity
  = tag

    -- other closures (and unknown ones) are not tagged
  | otherwise
  = 0

tagForArity :: RepArity -> Maybe Int
tagForArity i | i <= mAX_PTR_TAG = Just i
              | otherwise        = Nothing

clHasCafRefs :: ClosureInfo -> CafInfo
clHasCafRefs (ClosureInfo {closureSRT = srt}) = 
  case srt of NoC_SRT -> NoCafRefs
              _       -> MayHaveCafRefs
clHasCafRefs (ConInfo {}) = NoCafRefs
\end{code}

\begin{code}
isToplevClosure :: ClosureInfo -> Bool
isToplevClosure (ClosureInfo { closureLFInfo = lf_info })
  = case lf_info of
      LFReEntrant TopLevel _ _ _ -> True
      LFThunk TopLevel _ _ _ _   -> True
      _ -> False
isToplevClosure _ = False
\end{code}

Label generation.

\begin{code}
infoTableLabelFromCI :: ClosureInfo -> CLabel
infoTableLabelFromCI = fst . labelsFromCI

entryLabelFromCI :: ClosureInfo -> CLabel
entryLabelFromCI ci
  | tablesNextToCode = info_lbl
  | otherwise        = entry_lbl
  where (info_lbl, entry_lbl) = labelsFromCI ci

labelsFromCI :: ClosureInfo -> (CLabel, CLabel) -- (Info, Entry)
labelsFromCI cl@(ClosureInfo { closureName = name,
			       closureLFInfo = lf_info,
			       closureInfLcl = is_lcl })
  = case lf_info of
	LFBlackHole -> (mkCAFBlackHoleInfoTableLabel, mkCAFBlackHoleEntryLabel)

	LFThunk _ _ upd_flag (SelectorThunk offset) _ -> 
		bothL (mkSelectorInfoLabel, mkSelectorEntryLabel) upd_flag offset

	LFThunk _ _ upd_flag (ApThunk arity) _ -> 
		bothL (mkApInfoTableLabel, mkApEntryLabel) upd_flag arity

	LFThunk{}      -> bothL std_mk_lbls name $ clHasCafRefs cl

	LFReEntrant _ _ _ _ -> bothL std_mk_lbls name $ clHasCafRefs cl

	_ -> panic "labelsFromCI"
  where std_mk_lbls = if is_lcl then (mkLocalInfoTableLabel, mkLocalEntryLabel) else (mkInfoTableLabel, mkEntryLabel)

labelsFromCI cl@(ConInfo { closureCon = con, 
			           closureSMRep = rep })
  | isStaticRep rep = bothL (mkStaticInfoTableLabel, mkStaticConEntryLabel)  name $ clHasCafRefs cl
  | otherwise	    = bothL (mkConInfoTableLabel,    mkConEntryLabel)        name $ clHasCafRefs cl
  where
    name = dataConName con

bothL :: (a -> b -> c, a -> b -> c) -> a -> b -> (c, c)
bothL (f, g) x y = (f x y, g x y)

-- ClosureInfo for a closure (as opposed to a constructor) is always local
closureLabelFromCI :: ClosureInfo -> CLabel
closureLabelFromCI cl@(ClosureInfo { closureName = nm }) = mkLocalClosureLabel nm $ clHasCafRefs cl
closureLabelFromCI _ = panic "closureLabelFromCI"

-- thunkEntryLabel is a local help function, not exported.  It's used from both
-- entryLabelFromCI and getCallMethod.

{- UNUSED:
thunkEntryLabel :: Name -> CafInfo -> StandardFormInfo -> Bool -> CLabel
thunkEntryLabel _thunk_id _ (ApThunk arity) is_updatable
  = enterApLabel is_updatable arity
thunkEntryLabel _thunk_id _ (SelectorThunk offset) upd_flag
  = enterSelectorLabel upd_flag offset
thunkEntryLabel thunk_id caf _ _is_updatable
  = enterIdLabel thunk_id caf
-}

{- UNUSED:
enterApLabel :: Bool -> Int -> CLabel
enterApLabel is_updatable arity
  | tablesNextToCode = mkApInfoTableLabel is_updatable arity
  | otherwise        = mkApEntryLabel is_updatable arity
-}

{- UNUSED:
enterSelectorLabel :: Bool -> Int -> CLabel
enterSelectorLabel upd_flag offset
  | tablesNextToCode = mkSelectorInfoLabel upd_flag offset
  | otherwise        = mkSelectorEntryLabel upd_flag offset
-}

enterIdLabel :: Name -> CafInfo -> CLabel
enterIdLabel id
  | tablesNextToCode = mkInfoTableLabel id
  | otherwise        = mkEntryLabel id

enterReturnPtLabel :: Unique -> CLabel
enterReturnPtLabel name
  | tablesNextToCode = mkReturnInfoLabel name
  | otherwise        = mkReturnPtLabel name
\end{code}


We need a black-hole closure info to pass to @allocDynClosure@ when we
want to allocate the black hole on entry to a CAF.  These are the only
ways to build an LFBlackHole, maintaining the invariant that it really
is a black hole and not something else.

\begin{code}
cafBlackHoleClosureInfo :: ClosureInfo -> ClosureInfo
cafBlackHoleClosureInfo (ClosureInfo { closureName = nm,
				       closureType = ty })
  = ClosureInfo { closureName   = nm,
		  closureLFInfo = LFBlackHole,
		  closureSMRep  = blackHoleRep,
		  closureSRT    = NoC_SRT,
		  closureType   = ty,
		  closureDescr  = "",
		  closureInfLcl = False }
cafBlackHoleClosureInfo _ = panic "cafBlackHoleClosureInfo"
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
closureValDescr, closureTypeDescr :: ClosureInfo -> String
closureValDescr (ClosureInfo {closureDescr = descr}) 
  = descr
closureValDescr (ConInfo {closureCon = con})
  = occNameString (getOccName con)

closureTypeDescr (ClosureInfo { closureType = ty })
  = getTyDescription ty
closureTypeDescr (ConInfo { closureCon = data_con })
  = occNameString (getOccName (dataConTyCon data_con))

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
\end{code}
