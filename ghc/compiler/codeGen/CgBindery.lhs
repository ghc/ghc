%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CgBindery]{Utility functions related to doing @CgBindings@}

\begin{code}
module CgBindery (
	CgBindings, CgIdInfo,
	StableLoc, VolatileLoc,

	stableAmodeIdInfo, heapIdInfo, 
	letNoEscapeIdInfo, idInfoToAmode,

	addBindC, addBindsC,

	nukeVolatileBinds,
	nukeDeadBindings,

	bindNewToStack,  rebindToStack,
	bindNewToNode, bindNewToReg, bindArgsToRegs,
	bindNewToTemp, 
	getArgAmode, getArgAmodes,
	getCAddrModeAndInfo, getCAddrMode,
	getCAddrModeIfVolatile, getVolatileRegs,

	buildContLivenessMask
    ) where

#include "HsVersions.h"

import AbsCSyn
import CgMonad

import CgUsages		( getHpRelOffset, getSpRelOffset, getRealSp )
import CgStackery	( freeStackSlots, getStackFrame )
import CLabel		( mkClosureLabel,
			  mkBitmapLabel, pprCLabel )
import ClosureInfo	( mkLFImported, mkLFArgument, LambdaFormInfo )
import Bitmap
import PrimRep		( isFollowableRep, getPrimRepSize )
import Id		( Id, idPrimRep, idType )
import Type		( typePrimRep )
import VarEnv
import VarSet		( varSetElems )
import Literal		( Literal )
import Maybes		( catMaybes, maybeToBool, seqMaybe )
import Name		( isInternalName, NamedThing(..) )
import PprAbsC		( pprAmode, pprMagicId )
import PrimRep          ( PrimRep(..) )
import StgSyn		( StgArg, StgLiveVars, GenStgArg(..), isStgTypeArg )
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
					-- (probably {Int,Float,Char,etc}Reg)

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

\begin{code}
instance Outputable CgIdInfo where
  ppr (MkCgIdInfo id vol stb lf)
    = ppr id <+> ptext SLIT("-->") <+> vcat [ppr vol, ppr stb]

instance Outputable VolatileLoc where
  ppr NoVolatileLoc = empty
  ppr (TempVarLoc u) = ptext SLIT("tmp") <+> ppr u
  ppr (RegLoc r)     = ptext SLIT("reg") <+> pprMagicId r
  ppr (VirHpLoc v)   = ptext SLIT("vh") <+> ppr v
  ppr (VirNodeLoc v) = ptext SLIT("vn") <+> ppr v

instance Outputable StableLoc where
  ppr NoStableLoc 	 = empty
  ppr (VirStkLoc v) 	 = ptext SLIT("vs") <+> ppr v
  ppr (LitLoc l) 	 = ptext SLIT("lit") <+> ppr l
  ppr (StableAmodeLoc a) = ptext SLIT("amode") <+> pprAmode a
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
\subsection[CgMonad-bindery]{Monad things for fiddling with @CgBindings@}
%*									*
%************************************************************************

There are three basic routines, for adding (@addBindC@), modifying
(@modifyBindC@) and looking up (@lookupBindC@) bindings.

A @Id@ is bound to a @(VolatileLoc, StableLoc)@ triple.
The name should not already be bound. (nice ASSERT, eh?)

\begin{code}
addBindC :: Id -> CgIdInfo -> Code
addBindC name stuff_to_bind = do
	binds <- getBinds
	setBinds $ extendVarEnv binds name stuff_to_bind

addBindsC :: [(Id, CgIdInfo)] -> Code
addBindsC new_bindings = do
	binds <- getBinds
	let new_binds = foldl (\ binds (name,info) -> extendVarEnv binds name info)
		binds
		new_bindings
	setBinds new_binds

modifyBindC :: Id -> (CgIdInfo -> CgIdInfo) -> Code
modifyBindC name mangle_fn = do
	binds <- getBinds
	setBinds $ modifyVarEnv mangle_fn binds name

lookupBindC :: Id -> FCode CgIdInfo
lookupBindC id = do maybe_info <- lookupBindC_maybe id
		    case maybe_info of
		      Just info -> return info
		      Nothing   -> cgLookupPanic id

lookupBindC_maybe :: Id -> FCode (Maybe CgIdInfo)
lookupBindC_maybe id
  = do	static_binds <- getStaticBinds
	local_binds  <- getBinds
	return (lookupVarEnv local_binds id
			`seqMaybe`
		lookupVarEnv static_binds id)
			
cgLookupPanic :: Id -> FCode a
cgLookupPanic id
  = do	static_binds <- getStaticBinds
	local_binds <- getBinds
	srt <- getSRTLabel
	pprPanic "cgPanic"
		(vcat [ppr id,
		ptext SLIT("static binds for:"),
		vcat [ ppr i | (MkCgIdInfo i _ _ _) <- rngVarEnv static_binds ],
		ptext SLIT("local binds for:"),
		vcat [ ppr i | (MkCgIdInfo i _ _ _) <- rngVarEnv local_binds ],
	        ptext SLIT("SRT label") <+> pprCLabel srt
	      ])
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
getCAddrModeAndInfo :: Id -> FCode (Id, CAddrMode, LambdaFormInfo)

getCAddrModeAndInfo id
  = do
	maybe_cg_id_info <- lookupBindC_maybe id
	case maybe_cg_id_info of

		-- Nothing => not in the environment, so should be imported
	  Nothing | isInternalName name -> cgLookupPanic id
		  | otherwise	     -> returnFC (id, global_amode, mkLFImported id)

	  Just (MkCgIdInfo id' volatile_loc stable_loc lf_info)
		  -> do amode <- idInfoPiecesToAmode kind volatile_loc stable_loc
			return (id', amode, lf_info)
  where
    name = getName id
    global_amode = CLbl (mkClosureLabel name) kind
    kind = idPrimRep id

getCAddrMode :: Id -> FCode CAddrMode
getCAddrMode name = do
	(_, amode, _) <- getCAddrModeAndInfo name
	return amode
\end{code}

\begin{code}
getCAddrModeIfVolatile :: Id -> FCode (Maybe CAddrMode)
getCAddrModeIfVolatile name
--  | toplevelishId name = returnFC Nothing
--  | otherwise
	= do
	(MkCgIdInfo _ volatile_loc stable_loc lf_info) <- lookupBindC name
	case stable_loc of
		NoStableLoc -> do -- Aha!  So it is volatile!
			amode <- idInfoPiecesToAmode (idPrimRep name) volatile_loc NoStableLoc
			return $ Just amode
		a_stable_loc -> return Nothing
\end{code}

@getVolatileRegs@ gets a set of live variables, and returns a list of
all registers on which these variables depend.  These are the regs
which must be saved and restored across any C calls.  If a variable is
both in a volatile location (depending on a register) {\em and} a
stable one (notably, on the stack), we modify the current bindings to
forget the volatile one.

\begin{code}
getVolatileRegs :: StgLiveVars -> FCode [MagicId]

getVolatileRegs vars = do
	stuff <- mapFCs snaffle_it (varSetElems vars)
	returnFC $ catMaybes stuff
	where
	snaffle_it var = do
		(MkCgIdInfo _ volatile_loc stable_loc lf_info) <- lookupBindC var 
		let
		-- commoned-up code...
			consider_reg reg =
				if not (isVolatileReg reg) then
				-- Potentially dies across C calls
				-- For now, that's everything; we leave
				-- it to the save-macros to decide which
				-- regs *really* need to be saved.
					returnFC Nothing
				else
					case stable_loc of
						NoStableLoc -> returnFC (Just reg) -- got one!
						is_a_stable_loc -> do
							-- has both volatile & stable locations;
							-- force it to rely on the stable location
							modifyBindC var nuke_vol_bind 
							return Nothing
			in
			case volatile_loc of
				RegLoc reg   -> consider_reg reg
				VirNodeLoc _ -> consider_reg node
				non_reg_loc  -> returnFC Nothing

	nuke_vol_bind (MkCgIdInfo i _ stable_loc lf_info)
		= MkCgIdInfo i NoVolatileLoc stable_loc lf_info
\end{code}

\begin{code}
getArgAmodes :: [StgArg] -> FCode [CAddrMode]
getArgAmodes [] = returnFC []
getArgAmodes (atom:atoms)
	| isStgTypeArg atom 
	= getArgAmodes atoms
	| otherwise = do
		amode <- getArgAmode  atom 
		amodes <- getArgAmodes atoms
		return ( amode : amodes )

getArgAmode :: StgArg -> FCode CAddrMode

getArgAmode (StgVarArg var) = getCAddrMode var		-- The common case
getArgAmode (StgLitArg lit) = returnFC (CLit lit)
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
    info = MkCgIdInfo name NoVolatileLoc (VirStkLoc offset) (mkLFArgument name)

bindNewToNode :: Id -> VirtualHeapOffset -> LambdaFormInfo -> Code
bindNewToNode name offset lf_info
  = addBindC name info
  where
    info = MkCgIdInfo name (VirNodeLoc offset) NoStableLoc lf_info

-- Create a new temporary whose unique is that in the id,
-- bind the id to it, and return the addressing mode for the
-- temporary.
bindNewToTemp :: Id -> FCode CAddrMode
bindNewToTemp id
  = do	addBindC id id_info
	return temp_amode
  where
    uniq       = getUnique id
    temp_amode = CTemp uniq (idPrimRep id)
    id_info    = tempIdInfo id uniq lf_info
    lf_info    = mkLFArgument id	-- Always used of things we
					-- know nothing about

bindNewToReg :: Id -> MagicId -> LambdaFormInfo -> Code
bindNewToReg name magic_id lf_info
  = addBindC name info
  where
    info = MkCgIdInfo name (RegLoc magic_id) NoStableLoc lf_info

bindArgsToRegs :: [Id] -> [MagicId] -> Code
bindArgsToRegs args regs
  = listCs (zipWithEqual "bindArgsToRegs" bind args regs)
  where
    arg `bind` reg = bindNewToReg arg reg (mkLFArgument arg)
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

There are four kinds of things on the stack:

	- pointer variables (bound in the environment)
	- non-pointer variables (boudn in the environment)
	- free slots (recorded in the stack free list)
	- non-pointer data slots (recorded in the stack free list)

We build up a bitmap of non-pointer slots by searching the environment
for all the pointer variables, and subtracting these from a bitmap
with initially all bits set (up to the size of the stack frame).

\begin{code}
buildLivenessMask 
	:: VirtualSpOffset	-- size of the stack frame
	-> VirtualSpOffset	-- offset from which the bitmap should start
	-> FCode Bitmap		-- mask for free/unlifted slots

buildLivenessMask size sp = do {
    -- find all live stack-resident pointers
    binds <- getBinds;
    ((vsp, _, free, _, _), heap_usage) <- getUsage;

    let {
	rel_slots = sortLt (<) 
    	    [ sp - ofs  -- get slots relative to top of frame
    	    | (MkCgIdInfo id _ (VirStkLoc ofs) _) <- rngVarEnv binds,
    	      isFollowableRep (idPrimRep id)
    	    ];
    };

    WARN( not (all (>=0) rel_slots), ppr size $$ ppr sp $$ ppr rel_slots $$ ppr binds )
    return (intsToReverseBitmap size rel_slots)
  }

-- In a continuation, we want a liveness mask that starts from just after
-- the return address, which is on the stack at realSp.

buildContLivenessMask :: Id -> FCode Liveness
	-- The Id is used just for its unique to make a label
buildContLivenessMask id = do
	realSp <- getRealSp

	frame_sp <- getStackFrame
	-- realSp points to the frame-header for the current stack frame,
	-- and the end of this frame is frame_sp.  The size is therefore
	-- realSp - frame_sp - 1 (subtract one for the frame-header).
	let frame_size = realSp - frame_sp - 1

	mask <- buildLivenessMask frame_size (realSp-1)

        let liveness = Liveness (mkBitmapLabel (getName id)) frame_size mask
	absC (maybeLargeBitmap liveness)
	return liveness
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
nukeDeadBindings live_vars = do
	binds <- getBinds
	let (dead_stk_slots, bs') =
		dead_slots live_vars 
			[] []
			[ (i, b) | b@(MkCgIdInfo i _ _ _) <- rngVarEnv binds ]
	setBinds $ mkVarEnv bs'
	freeStackSlots dead_stk_slots
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
	 | is_stk_loc && size > 0 ->
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
