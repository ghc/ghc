%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CgBindery]{Utility functions related to doing @CgBindings@}

\begin{code}
module CgBindery (
	CgBindings, CgIdInfo(..){-dubiously concrete-},
	StableLoc, VolatileLoc,

	maybeStkLoc,

	stableAmodeIdInfo, heapIdInfo, newTempAmodeAndIdInfo,
	letNoEscapeIdInfo, idInfoToAmode,

	nukeVolatileBinds,
	nukeDeadBindings,

	bindNewToStack,  rebindToStack,
	bindNewToNode, bindNewToReg, bindArgsToRegs,
	bindNewToTemp, bindNewPrimToAmode,
	getArgAmode, getArgAmodes,
	getCAddrModeAndInfo, getCAddrMode,
	getCAddrModeIfVolatile, getVolatileRegs,

	buildLivenessMask, buildContLivenessMask
    ) where

#include "HsVersions.h"

import AbsCSyn
import CgMonad

import CgUsages		( getHpRelOffset, getSpRelOffset, getRealSp )
import CgStackery	( freeStackSlots, addFreeSlots )
import CLabel		( mkStaticClosureLabel, mkClosureLabel,
			  mkBitmapLabel )
import ClosureInfo	( mkLFImported, mkLFArgument, LambdaFormInfo )
import BitSet		( mkBS, emptyBS )
import PrimRep		( isFollowableRep, getPrimRepSize )
import DataCon		( DataCon, dataConName )
import Id		( Id, idPrimRep, idType )
import Type		( typePrimRep )
import VarEnv
import VarSet		( varSetElems )
import Const		( Con(..), Literal )
import Maybes		( catMaybes, maybeToBool )
import Name		( isLocallyDefined, isWiredInName, NamedThing(..) )
#ifdef DEBUG
import PprAbsC		( pprAmode )
#endif
import PrimRep          ( PrimRep(..) )
import StgSyn		( StgArg, StgLiveVars, GenStgArg(..) )
import Unique           ( Unique, Uniquable(..) )
import UniqSet		( elementOfUniqSet )
import Util		( zipWithEqual, sortLt )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection[Bindery-datatypes]{Data types}
%*									*
%************************************************************************

@(CgBinding a b)@ is a type of finite maps from a to b.

The assumption used to be that @lookupCgBind@ must get exactly one
match.  This is {\em completely wrong} in the case of compiling
letrecs (where knot-tying is used).  An initial binding is fed in (and
never evaluated); eventually, a correct binding is put into the
environment.  So there can be two bindings for a given name.

\begin{code}
type CgBindings = IdEnv CgIdInfo

data CgIdInfo
  = MkCgIdInfo	Id	-- Id that this is the info for
		VolatileLoc
		StableLoc
		LambdaFormInfo

data VolatileLoc
  = NoVolatileLoc
  | TempVarLoc	Unique

  | RegLoc	MagicId			-- in one of the magic registers
					-- (probably {Int,Float,Char,etc}Reg

  | VirHpLoc	VirtualHeapOffset	-- Hp+offset (address of closure)

  | VirNodeLoc	VirtualHeapOffset	-- Cts of offset indirect from Node
					-- ie *(Node+offset)
\end{code}

@StableLoc@ encodes where an Id can be found, used by
the @CgBindings@ environment in @CgBindery@.

\begin{code}
data StableLoc
  = NoStableLoc
  | VirStkLoc		VirtualSpOffset
  | LitLoc		Literal
  | StableAmodeLoc	CAddrMode

-- these are so StableLoc can be abstract:

maybeStkLoc (VirStkLoc offset) = Just offset
maybeStkLoc _		       = Nothing
\end{code}

%************************************************************************
%*									*
\subsection[Bindery-idInfo]{Manipulating IdInfo}
%*									*
%************************************************************************

\begin{code}
stableAmodeIdInfo i amode lf_info = MkCgIdInfo i NoVolatileLoc (StableAmodeLoc amode) lf_info
heapIdInfo i offset       lf_info = MkCgIdInfo i (VirHpLoc offset) NoStableLoc lf_info
tempIdInfo i uniq         lf_info = MkCgIdInfo i (TempVarLoc uniq) NoStableLoc lf_info

letNoEscapeIdInfo i sp lf_info
  = MkCgIdInfo i NoVolatileLoc (StableAmodeLoc (CJoinPoint sp)) lf_info

newTempAmodeAndIdInfo :: Id -> LambdaFormInfo -> (CAddrMode, CgIdInfo)

newTempAmodeAndIdInfo name lf_info
  = (temp_amode, temp_idinfo)
  where
    uniq       	= getUnique name
    temp_amode	= CTemp uniq (idPrimRep name)
    temp_idinfo = tempIdInfo name uniq lf_info

idInfoToAmode :: PrimRep -> CgIdInfo -> FCode CAddrMode
idInfoToAmode kind (MkCgIdInfo _ vol stab _) = idInfoPiecesToAmode kind vol stab

idInfoPiecesToAmode :: PrimRep -> VolatileLoc -> StableLoc -> FCode CAddrMode

idInfoPiecesToAmode kind (TempVarLoc uniq) stable_loc   = returnFC (CTemp uniq kind)
idInfoPiecesToAmode kind (RegLoc magic_id) stable_loc   = returnFC (CReg magic_id)

idInfoPiecesToAmode kind NoVolatileLoc (LitLoc lit)           = returnFC (CLit lit)
idInfoPiecesToAmode kind NoVolatileLoc (StableAmodeLoc amode) = returnFC amode

idInfoPiecesToAmode kind (VirNodeLoc nd_off) stable_loc
  = returnFC (CVal (nodeRel nd_off) kind)
    -- Virtual offsets from Node increase into the closures,
    -- and so do Node-relative offsets (which we want in the CVal),
    -- so there is no mucking about to do to the offset.

idInfoPiecesToAmode kind (VirHpLoc hp_off) stable_loc
  = getHpRelOffset hp_off `thenFC` \ rel_hp ->
    returnFC (CAddr rel_hp)

idInfoPiecesToAmode kind NoVolatileLoc (VirStkLoc i)
  = getSpRelOffset i `thenFC` \ rel_sp ->
    returnFC (CVal rel_sp kind)

#ifdef DEBUG
idInfoPiecesToAmode kind NoVolatileLoc NoStableLoc = panic "idInfoPiecesToAmode: no loc"
#endif
\end{code}

%************************************************************************
%*									*
\subsection[Bindery-nuke-volatile]{Nuking volatile bindings}
%*									*
%************************************************************************

We sometimes want to nuke all the volatile bindings; we must be sure
we don't leave any (NoVolatile, NoStable) binds around...

\begin{code}
nukeVolatileBinds :: CgBindings -> CgBindings
nukeVolatileBinds binds
  = mkVarEnv (foldr keep_if_stable [] (rngVarEnv binds))
  where
    keep_if_stable (MkCgIdInfo i _ NoStableLoc entry_info) acc = acc
    keep_if_stable (MkCgIdInfo i _ stable_loc  entry_info) acc
      = (i, MkCgIdInfo i NoVolatileLoc stable_loc entry_info) : acc
\end{code}


%************************************************************************
%*									*
\subsection[lookup-interface]{Interface functions to looking up bindings}
%*									*
%************************************************************************

I {\em think} all looking-up is done through @getCAddrMode(s)@.

\begin{code}
getCAddrModeAndInfo :: Id -> FCode (CAddrMode, LambdaFormInfo)

getCAddrModeAndInfo id
  | not (isLocallyDefined name) || isWiredInName name
    {- Why the "isWiredInName"?
	Imagine you are compiling PrelBase.hs (a module that
	supplies some of the wired-in values).  What can
	happen is that the compiler will inject calls to
	(e.g.) GHCbase.unpackPS, where-ever it likes -- it
	assumes those values are ubiquitously available.
	The main point is: it may inject calls to them earlier
	in GHCbase.hs than the actual definition...
    -}
  = returnFC (global_amode, mkLFImported id)

  | otherwise = -- *might* be a nested defn: in any case, it's something whose
		-- definition we will know about...
    lookupBindC id `thenFC` \ (MkCgIdInfo _ volatile_loc stable_loc lf_info) ->
    idInfoPiecesToAmode kind volatile_loc stable_loc `thenFC` \ amode ->
    returnFC (amode, lf_info)
  where
    name = getName id
    global_amode = CLbl (mkClosureLabel name) kind
    kind = idPrimRep id

getCAddrMode :: Id -> FCode CAddrMode
getCAddrMode name
  = getCAddrModeAndInfo name `thenFC` \ (amode, _) ->
    returnFC amode
\end{code}

\begin{code}
getCAddrModeIfVolatile :: Id -> FCode (Maybe CAddrMode)
getCAddrModeIfVolatile name
--  | toplevelishId name = returnFC Nothing
--  | otherwise
  = lookupBindC name `thenFC` \ ~(MkCgIdInfo _ volatile_loc stable_loc lf_info) ->
    case stable_loc of
	NoStableLoc ->	-- Aha!  So it is volatile!
	    idInfoPiecesToAmode (idPrimRep name) volatile_loc NoStableLoc `thenFC` \ amode ->
	    returnFC (Just amode)

	a_stable_loc -> returnFC Nothing
\end{code}

@getVolatileRegs@ gets a set of live variables, and returns a list of
all registers on which these variables depend.  These are the regs
which must be saved and restored across any C calls.  If a variable is
both in a volatile location (depending on a register) {\em and} a
stable one (notably, on the stack), we modify the current bindings to
forget the volatile one.

\begin{code}
getVolatileRegs :: StgLiveVars -> FCode [MagicId]

getVolatileRegs vars
  = mapFCs snaffle_it (varSetElems vars) `thenFC` \ stuff ->
    returnFC (catMaybes stuff)
  where
    snaffle_it var
      = lookupBindC var	`thenFC` \ (MkCgIdInfo _ volatile_loc stable_loc lf_info) ->
	let
	    -- commoned-up code...
	    consider_reg reg
	      =	if not (isVolatileReg reg) then
			-- Potentially dies across C calls
			-- For now, that's everything; we leave
			-- it to the save-macros to decide which
			-- regs *really* need to be saved.
		    returnFC Nothing
		else
		    case stable_loc of
		      NoStableLoc -> returnFC (Just reg) -- got one!
		      is_a_stable_loc ->
			-- has both volatile & stable locations;
			-- force it to rely on the stable location
			modifyBindC var nuke_vol_bind `thenC`
			returnFC Nothing
	in
	case volatile_loc of
	  RegLoc reg   -> consider_reg reg
    	  VirHpLoc _   -> consider_reg Hp
	  VirNodeLoc _ -> consider_reg node
	  non_reg_loc  -> returnFC Nothing

    nuke_vol_bind (MkCgIdInfo i _ stable_loc lf_info)
      = MkCgIdInfo i NoVolatileLoc stable_loc lf_info
\end{code}

\begin{code}
getArgAmodes :: [StgArg] -> FCode [CAddrMode]
getArgAmodes [] = returnFC []
getArgAmodes (atom:atoms)
  = getArgAmode  atom  `thenFC` \ amode ->
    getArgAmodes atoms `thenFC` \ amodes ->
    returnFC ( amode : amodes )

getArgAmode :: StgArg -> FCode CAddrMode

getArgAmode (StgVarArg var) = getCAddrMode var		-- The common case

getArgAmode (StgConArg (DataCon con))
     {- Why does this case differ from StgVarArg?
	Because the program might look like this:
		data Foo a = Empty | Baz a
		f a x = let c = Empty! a
			in h c
	Now, when we go Core->Stg, we drop the type applications, 
	so we can inline c, giving
		f x = h Empty
	Now we are referring to Empty as an argument (rather than in an STGCon), 
	so we'll look it up with getCAddrMode.  We want to return an amode for
	the static closure that we make for nullary constructors.  But if we blindly
	go ahead with getCAddrMode we end up looking in the environment, and it ain't there!

	This special case used to be in getCAddrModeAndInfo, but it doesn't work there.
	Consider:
		f a x = Baz a x
	If the constructor Baz isn't inlined we simply want to treat it like any other
	identifier, with a top level definition.  We don't want to spot that it's a constructor.

	In short 
		StgApp con args
	and
		StgCon con args
	are treated differently; the former is a call to a bog standard function while the
	latter uses the specially-labelled, pre-defined info tables etc for the constructor.

	The way to think of this case in getArgAmode is that
		SApp f Empty
	is really
		App f (StgCon Empty [])
     -}
  = returnFC (CLbl (mkStaticClosureLabel (dataConName con)) PtrRep)


getArgAmode (StgConArg (Literal lit)) = returnFC (CLit lit)
\end{code}

%************************************************************************
%*									*
\subsection[binding-and-rebinding-interface]{Interface functions for binding and re-binding names}
%*									*
%************************************************************************

\begin{code}
bindNewToStack :: (Id, VirtualSpOffset) -> Code
bindNewToStack (name, offset)
  = addBindC name info
  where
    info = MkCgIdInfo name NoVolatileLoc (VirStkLoc offset) mkLFArgument

bindNewToNode :: Id -> VirtualHeapOffset -> LambdaFormInfo -> Code
bindNewToNode name offset lf_info
  = addBindC name info
  where
    info = MkCgIdInfo name (VirNodeLoc offset) NoStableLoc lf_info

-- Create a new temporary whose unique is that in the id,
-- bind the id to it, and return the addressing mode for the
-- temporary.
bindNewToTemp :: Id -> FCode CAddrMode
bindNewToTemp name
  = let (temp_amode, id_info) = newTempAmodeAndIdInfo name mkLFArgument
		-- This is used only for things we don't know
		-- anything about; values returned by a case statement,
		-- for example.
    in
    addBindC name id_info	`thenC`
    returnFC temp_amode

bindNewToReg :: Id -> MagicId -> LambdaFormInfo -> Code
bindNewToReg name magic_id lf_info
  = addBindC name info
  where
    info = MkCgIdInfo name (RegLoc magic_id) NoStableLoc lf_info

bindNewToLit name lit
  = addBindC name info
  where
    info = MkCgIdInfo name NoVolatileLoc (LitLoc lit) (error "bindNewToLit")

bindArgsToRegs :: [Id] -> [MagicId] -> Code
bindArgsToRegs args regs
  = listCs (zipWithEqual "bindArgsToRegs" bind args regs)
  where
    arg `bind` reg = bindNewToReg arg reg mkLFArgument
\end{code}

@bindNewPrimToAmode@ works only for certain addressing modes.  Making
this work for stack offsets is non-trivial (virt vs. real stack offset
difficulties).

\begin{code}
bindNewPrimToAmode :: Id -> CAddrMode -> Code
bindNewPrimToAmode name (CReg reg) 
  = bindNewToReg name reg (panic "bindNewPrimToAmode")

bindNewPrimToAmode name (CTemp uniq kind)
  = addBindC name (tempIdInfo name uniq (panic "bindNewPrimToAmode"))

#ifdef DEBUG
bindNewPrimToAmode name amode
  = pprPanic "bindNew...:" (pprAmode amode)
#endif
\end{code}

\begin{code}
rebindToStack :: Id -> VirtualSpOffset -> Code
rebindToStack name offset
  = modifyBindC name replace_stable_fn
  where
    replace_stable_fn (MkCgIdInfo i vol stab einfo)
      = MkCgIdInfo i vol (VirStkLoc offset) einfo
\end{code}

%************************************************************************
%*									*
\subsection[CgBindery-liveness]{Build a liveness mask for the current stack}
%*									*
%************************************************************************

ToDo: remove the dependency on 32-bit words.

There are two ways to build a liveness mask, and both appear to have
problems.

  1) Find all the pointer words by searching through the binding list.
     Invert this to find the non-pointer words and build the bitmap.

  2) Find all the non-pointer words by search through the binding list.
     Merge this with the list of currently free slots.  Build the
     bitmap.

Method (1) conflicts with update frames - these contain pointers but
have no bindings in the environment.  We could bind the updatee to its
location in the update frame at the point when the update frame is
pushed, but this binding would be dropped by the first case expression
(nukeDeadBindings).

Method (2) causes problems because we must make sure that every
non-pointer word on the stack is either a free stack slot or has a
binding in the environment.  Things like cost centres break this (but
only for case-of-case expressions - because that's when there's a cost
centre on the stack from the outer case and we need to generate a
bitmap for the inner case's continuation).

This method also works "by accident" for update frames: since all
unaccounted for slots on the stack are assumed to be pointers, and an
update frame always occurs at virtual Sp offsets 0-3 (i.e. the bottom
of the stack frame), the bitmap will simply end at the start of the
update frame.

We use method (2) at the moment.

\begin{code}
buildLivenessMask 
	:: Unique		-- unique for for large bitmap label
	-> VirtualSpOffset	-- offset from which the bitmap should start
	-> FCode Liveness	-- mask for free/unlifted slots

buildLivenessMask uniq sp info_down
	state@(MkCgState abs_c binds ((vsp, free, _, _), heap_usage))
  = ASSERT(all (>=0) rel_slots) 
    livenessToAbsC uniq liveness_mask info_down state 
  where
	-- find all unboxed stack-resident ids
	unboxed_slots = 		   
	  [ (ofs, getPrimRepSize rep) | 
		     (MkCgIdInfo id _ (VirStkLoc ofs) _) <- rngVarEnv binds,
	        let rep = idPrimRep id,
		not (isFollowableRep rep)
	  ]

	-- flatten this list into a list of unboxed stack slots
	flatten_slots = sortLt (<) 
		(foldr (\(ofs,size) r -> [ofs-size+1 .. ofs] ++ r) []
		      unboxed_slots)

	-- merge in the free slots
	all_slots = addFreeSlots flatten_slots free ++ 
		    if vsp < sp then [vsp+1 .. sp] else []

        -- recalibrate the list to be sp-relative
	rel_slots = reverse (map (sp-) all_slots)

	-- build the bitmap
	liveness_mask = listToLivenessMask rel_slots

{- ALTERNATE version that doesn't work because update frames aren't
   recorded in the environment.

	-- find all boxed stack-resident ids
	boxed_slots = 		   
	  [ ofs | (MkCgIdInfo id _ (VirStkLoc ofs) _) <- rngVarEnv binds,
	        isFollowableRep (idPrimRep id)
	  ]
	all_slots = [1..vsp]

	-- invert to get unboxed slots
	unboxed_slots = filter (`notElem` boxed_slots) all_slots
-}

listToLivenessMask :: [Int] -> LivenessMask
listToLivenessMask []    = []
listToLivenessMask slots = 
   mkBS this : listToLivenessMask (map (\x -> x-32) rest)
   where (this,rest) = span (<32) slots

livenessToAbsC :: Unique -> LivenessMask -> FCode Liveness
livenessToAbsC uniq []    = returnFC (LvSmall emptyBS)
livenessToAbsC uniq [one] = returnFC (LvSmall one)
livenessToAbsC uniq many  = 
  	absC (CBitmap lbl many) `thenC`
  	returnFC (LvLarge lbl)
  where lbl = mkBitmapLabel uniq
\end{code}

In a continuation, we want a liveness mask that starts from just after
the return address, which is on the stack at realSp.

\begin{code}
buildContLivenessMask
	:: Unique
	-> FCode Liveness
buildContLivenessMask uniq
  = getRealSp  `thenFC` \ realSp ->
    buildLivenessMask uniq (realSp-1)
\end{code}

%************************************************************************
%*									*
\subsection[CgMonad-deadslots]{Finding dead stack slots}
%*									*
%************************************************************************

nukeDeadBindings does the following:

      -	Removes all bindings from the environment other than those
	for variables in the argument to nukeDeadBindings.
      -	Collects any stack slots so freed, and returns them to the  stack free
	list.
      -	Moves the virtual stack pointer to point to the topmost used
	stack locations.

You can have multi-word slots on the stack (where a Double# used to
be, for instance); if dead, such a slot will be reported as *several*
offsets (one per word).

Probably *naughty* to look inside monad...

\begin{code}
nukeDeadBindings :: StgLiveVars  -- All the *live* variables
		 -> Code

nukeDeadBindings live_vars info_down (MkCgState abs_c binds usage)
  = freeStackSlots extra_free info_down (MkCgState abs_c (mkVarEnv bs') usage)
  where
    (dead_stk_slots, bs')
      = dead_slots live_vars
		   [] []
		   [ (i, b) | b@(MkCgIdInfo i _ _ _) <- rngVarEnv binds ]

    extra_free = sortLt (<) dead_stk_slots
\end{code}

Several boring auxiliary functions to do the dirty work.

\begin{code}
dead_slots :: StgLiveVars
	   -> [(Id,CgIdInfo)]
	   -> [VirtualSpOffset]
	   -> [(Id,CgIdInfo)]
	   -> ([VirtualSpOffset], [(Id,CgIdInfo)])

-- dead_slots carries accumulating parameters for
--	filtered bindings, dead slots
dead_slots live_vars fbs ds []
  = (ds, reverse fbs) -- Finished; rm the dups, if any

dead_slots live_vars fbs ds ((v,i):bs)
  | v `elementOfUniqSet` live_vars
    = dead_slots live_vars ((v,i):fbs) ds bs
	  -- Live, so don't record it in dead slots
	  -- Instead keep it in the filtered bindings

  | otherwise
    = case i of
	MkCgIdInfo _ _ stable_loc _
	 | is_stk_loc ->
	   dead_slots live_vars fbs ([offset-size+1 .. offset] ++ ds) bs
 	 where
 	  maybe_stk_loc = maybeStkLoc stable_loc
 	  is_stk_loc	= maybeToBool maybe_stk_loc
 	  (Just offset) = maybe_stk_loc

	_ -> dead_slots live_vars fbs ds bs
  where

    size :: Int
    size = (getPrimRepSize . typePrimRep . idType) v

\end{code}
