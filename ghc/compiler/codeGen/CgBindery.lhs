%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[CgBindery]{Utility functions related to doing @CgBindings@}

\begin{code}
#include "HsVersions.h"

module CgBindery (
	CgBindings(..), CgIdInfo(..){-dubiously concrete-},
	StableLoc, VolatileLoc, LambdaFormInfo{-re-exported-},

	maybeAStkLoc, maybeBStkLoc,

	stableAmodeIdInfo, heapIdInfo, newTempAmodeAndIdInfo,
	letNoEscapeIdInfo, idInfoToAmode,

	nukeVolatileBinds,

	bindNewToAStack, bindNewToBStack,
	bindNewToNode, bindNewToReg, bindArgsToRegs,
--UNUSED: bindNewToSameAsOther,
	bindNewToTemp, bindNewPrimToAmode,
	getAtomAmode, getAtomAmodes,
	getCAddrModeAndInfo, getCAddrMode,
	getCAddrModeIfVolatile, getVolatileRegs,
	rebindToAStack, rebindToBStack,
--UNUSED:	rebindToTemp,

	-- and to make a self-sufficient interface...
	AbstractC, CAddrMode, HeapOffset, MagicId, CLabel, CgState,
	BasicLit, IdEnv(..), UniqFM,
	Id, Maybe, Unique, StgAtom, UniqSet(..)
    ) where

IMPORT_Trace		-- ToDo: rm (debugging only)
import Outputable
import Unpretty
import PprAbsC

import AbsCSyn
import CgMonad

import CgUsages		( getHpRelOffset, getSpARelOffset, getSpBRelOffset )
import CLabelInfo	( mkClosureLabel, CLabel )
import ClosureInfo
import Id		( getIdKind, toplevelishId, isDataCon, Id )
import IdEnv		-- used to build CgBindings
import Maybes		( catMaybes, Maybe(..) )
import UniqSet		-- ( setToList )
import StgSyn
import Util
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

data StableLoc
  = NoStableLoc
  | VirAStkLoc		VirtualSpAOffset
  | VirBStkLoc		VirtualSpBOffset
  | LitLoc		BasicLit
  | StableAmodeLoc	CAddrMode

-- these are so StableLoc can be abstract:

maybeAStkLoc (VirAStkLoc offset) = Just offset
maybeAStkLoc _			 = Nothing

maybeBStkLoc (VirBStkLoc offset) = Just offset
maybeBStkLoc _			 = Nothing
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

letNoEscapeIdInfo i spa spb lf_info
  = MkCgIdInfo i NoVolatileLoc (StableAmodeLoc (CJoinPoint spa spb)) lf_info

newTempAmodeAndIdInfo :: Id -> LambdaFormInfo -> (CAddrMode, CgIdInfo)

newTempAmodeAndIdInfo name lf_info
  = (temp_amode, temp_idinfo)
  where
    uniq       	= getTheUnique name
    temp_amode	= CTemp uniq (getIdKind name)
    temp_idinfo = tempIdInfo name uniq lf_info

idInfoToAmode :: PrimKind -> CgIdInfo -> FCode CAddrMode
idInfoToAmode kind (MkCgIdInfo _ vol stab _) = idInfoPiecesToAmode kind vol stab

idInfoPiecesToAmode :: PrimKind -> VolatileLoc -> StableLoc -> FCode CAddrMode

idInfoPiecesToAmode kind (TempVarLoc uniq) stable_loc   = returnFC (CTemp uniq kind)
idInfoPiecesToAmode kind (RegLoc magic_id) stable_loc   = returnFC (CReg magic_id)

idInfoPiecesToAmode kind NoVolatileLoc (LitLoc lit)           = returnFC (CLit lit)
idInfoPiecesToAmode kind NoVolatileLoc (StableAmodeLoc amode) = returnFC amode

idInfoPiecesToAmode kind (VirNodeLoc nd_off) stable_loc
  = returnFC (CVal (NodeRel nd_off) kind)
    -- Virtual offsets from Node increase into the closures,
    -- and so do Node-relative offsets (which we want in the CVal),
    -- so there is no mucking about to do to the offset.

idInfoPiecesToAmode kind (VirHpLoc hp_off) stable_loc
  = getHpRelOffset hp_off `thenFC` \ rel_hp ->
    returnFC (CAddr rel_hp)

idInfoPiecesToAmode kind NoVolatileLoc (VirAStkLoc i)
  = getSpARelOffset i `thenFC` \ rel_spA ->
    returnFC (CVal rel_spA kind)

idInfoPiecesToAmode kind NoVolatileLoc (VirBStkLoc i)
  = getSpBRelOffset i `thenFC` \ rel_spB ->
    returnFC (CVal rel_spB kind)

idInfoPiecesToAmode kind NoVolatileLoc NoStableLoc = panic "idInfoPiecesToAmode: no loc"
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
  = mkIdEnv (foldr keep_if_stable [] (rngIdEnv binds))
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

getCAddrModeAndInfo name
  | not (isLocallyDefined name)
  = returnFC (global_amode, mkLFImported name)

  | isDataCon name
  = returnFC (global_amode, mkConLFInfo name)

  | otherwise = -- *might* be a nested defn: in any case, it's something whose
		-- definition we will know about...
    lookupBindC name `thenFC` \ (MkCgIdInfo _ volatile_loc stable_loc lf_info) ->
    idInfoPiecesToAmode kind volatile_loc stable_loc `thenFC` \ amode ->
    returnFC (amode, lf_info)
  where
    global_amode = CLbl (mkClosureLabel name) kind
    kind = getIdKind name

getCAddrMode :: Id -> FCode CAddrMode
getCAddrMode name
  = getCAddrModeAndInfo name `thenFC` \ (amode, _) ->
    returnFC amode
\end{code}

\begin{code}
getCAddrModeIfVolatile :: Id -> FCode (Maybe CAddrMode)
getCAddrModeIfVolatile name
  | toplevelishId name = returnFC Nothing
  | otherwise
  = lookupBindC name `thenFC` \ ~(MkCgIdInfo _ volatile_loc stable_loc lf_info) ->
    case stable_loc of
	NoStableLoc ->	-- Aha!  So it is volatile!
	    idInfoPiecesToAmode (getIdKind name) volatile_loc NoStableLoc `thenFC` \ amode ->
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
getVolatileRegs :: PlainStgLiveVars -> FCode [MagicId]

getVolatileRegs vars
  = mapFCs snaffle_it (uniqSetToList vars) `thenFC` \ stuff ->
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
getAtomAmodes :: [PlainStgAtom] -> FCode [CAddrMode]
getAtomAmodes [] = returnFC []
getAtomAmodes (atom:atoms)
  = getAtomAmode  atom  `thenFC` \ amode ->
    getAtomAmodes atoms `thenFC` \ amodes ->
    returnFC ( amode : amodes )

getAtomAmode :: PlainStgAtom -> FCode CAddrMode

getAtomAmode (StgVarAtom var) = getCAddrMode var
getAtomAmode (StgLitAtom lit) = returnFC (CLit lit)
\end{code}

%************************************************************************
%*									*
\subsection[binding-and-rebinding-interface]{Interface functions for binding and re-binding names}
%*									*
%************************************************************************

\begin{code}
bindNewToAStack :: (Id, VirtualSpAOffset) -> Code
bindNewToAStack (name, offset)
  = addBindC name info
  where
    info = MkCgIdInfo name NoVolatileLoc (VirAStkLoc offset) mkLFArgument

bindNewToBStack :: (Id, VirtualSpBOffset) -> Code
bindNewToBStack (name, offset)
  = addBindC name info
  where
    info = MkCgIdInfo name NoVolatileLoc (VirBStkLoc offset) (panic "bindNewToBStack")
	   -- B-stack things shouldn't need lambda-form info!

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
 = listCs (zipWith bind args regs)
 where
   arg `bind` reg = bindNewToReg arg reg mkLFArgument

{- UNUSED:
bindNewToSameAsOther :: Id -> PlainStgAtom -> Code
bindNewToSameAsOther name (StgVarAtom old_name)
#ifdef DEBUG
  | toplevelishId old_name = panic "bindNewToSameAsOther: global old name"
  | otherwise
#endif
  = lookupBindC old_name	`thenFC` \ old_stuff ->
    addBindC name old_stuff

bindNewToSameAsOther name (StgLitAtom lit)
  = addBindC name info
  where
    info = MkCgIdInfo name NoVolatileLoc (LitLoc lit) (panic "bindNewToSameAsOther")
-}
\end{code}

@bindNewPrimToAmode@ works only for certain addressing modes, because
those are the only ones we've needed so far!

\begin{code}
bindNewPrimToAmode :: Id -> CAddrMode -> Code
bindNewPrimToAmode name (CReg reg) = bindNewToReg name reg (panic "bindNewPrimToAmode")
						-- was: mkLFArgument
						-- LFinfo is irrelevant for primitives
bindNewPrimToAmode name (CTemp uniq kind)
  = addBindC name (tempIdInfo name uniq (panic "bindNewPrimToAmode"))
	-- LFinfo is irrelevant for primitives

bindNewPrimToAmode name (CLit lit) = bindNewToLit name lit

bindNewPrimToAmode name (CVal (SpBRel _ offset) _) 
  = bindNewToBStack (name, offset)

bindNewPrimToAmode name (CVal (NodeRel offset) _) 
  = bindNewToNode name offset (panic "bindNewPrimToAmode node")
  -- See comment on idInfoPiecesToAmode for VirNodeLoc

#ifdef DEBUG
bindNewPrimToAmode name amode
  = panic ("bindNew...:"++(uppShow 80 (pprAmode PprDebug  amode)))
#endif
\end{code}

\begin{code}
rebindToAStack :: Id -> VirtualSpAOffset -> Code
rebindToAStack name offset
  = modifyBindC name replace_stable_fn
  where
    replace_stable_fn (MkCgIdInfo i vol stab einfo)
      = MkCgIdInfo i vol (VirAStkLoc offset) einfo

rebindToBStack :: Id -> VirtualSpBOffset -> Code
rebindToBStack name offset
  = modifyBindC name replace_stable_fn
  where
    replace_stable_fn (MkCgIdInfo i vol stab einfo)
      = MkCgIdInfo i vol (VirBStkLoc offset) einfo

{- UNUSED:
rebindToTemp :: Id -> FCode CAddrMode
rebindToTemp name
  = let
	(temp_amode, MkCgIdInfo _ new_vol _ _ {-LF info discarded-})
	  = newTempAmodeAndIdInfo name (panic "rebindToTemp")
    in
    modifyBindC name (replace_volatile_fn new_vol) `thenC`
    returnFC temp_amode
  where
    replace_volatile_fn new_vol (MkCgIdInfo i vol stab einfo)
      = MkCgIdInfo i new_vol stab einfo
-}
\end{code}

