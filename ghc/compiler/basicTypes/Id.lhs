%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
module Id (
	Id, DictId,

	-- Simple construction
	mkGlobalId, mkLocalId, mkSpecPragmaId, mkLocalIdWithInfo, 
	mkSysLocal, mkSysLocalUnencoded, mkUserLocal, mkVanillaGlobal,
	mkTemplateLocals, mkTemplateLocalsNum, mkWildId, mkTemplateLocal,
	mkWorkerId, mkExportedLocalId,

	-- Taking an Id apart
	idName, idType, idUnique, idInfo,
	isId, globalIdDetails, idPrimRep,
	recordSelectorFieldLabel,

	-- Modifying an Id
	setIdName, setIdUnique, Id.setIdType, setIdExported, setIdNotExported, 
	setIdInfo, lazySetIdInfo, modifyIdInfo, maybeModifyIdInfo,
	zapLamIdInfo, zapDemandIdInfo, 

	-- Predicates
	isImplicitId, isDeadBinder,
	isSpecPragmaId,	isExportedId, isLocalId, isGlobalId,
	isRecordSelector,
	isPrimOpId, isPrimOpId_maybe, 
	isFCallId, isFCallId_maybe,
	isDataConWorkId, isDataConWorkId_maybe, 
	isBottomingId, idIsFrom,
	hasNoBinding, 

	-- Inline pragma stuff
	idInlinePragma, setInlinePragma, modifyInlinePragma, 


	-- One shot lambda stuff
	isOneShotBndr, isOneShotLambda, isStateHackType,
	setOneShotLambda, clearOneShotLambda,

	-- IdInfo stuff
	setIdUnfolding,
	setIdArity,
	setIdNewDemandInfo, 
	setIdNewStrictness, zapIdNewStrictness,
	setIdWorkerInfo,
	setIdSpecialisation,
	setIdCafInfo,
	setIdOccInfo,

#ifdef OLD_STRICTNESS
	idDemandInfo, 
	idStrictness, 
	idCprInfo,
	setIdStrictness, 
	setIdDemandInfo, 
	setIdCprInfo,
#endif

	idArity, 
	idNewDemandInfo, idNewDemandInfo_maybe,
	idNewStrictness, idNewStrictness_maybe, 
	idWorkerInfo,
	idUnfolding,
	idSpecialisation, idCoreRules,
	idCafInfo,
	idLBVarInfo,
	idOccInfo,

#ifdef OLD_STRICTNESS
	newStrictnessFromOld 	-- Temporary
#endif

    ) where

#include "HsVersions.h"


import CoreSyn		( Unfolding, CoreRules, IdCoreRule(..), rulesRules )
import BasicTypes	( Arity )
import Var		( Id, DictId,
			  isId, isExportedId, isSpecPragmaId, isLocalId,
			  idName, idType, idUnique, idInfo, isGlobalId,
			  setIdName, setIdType, setIdUnique, 
			  setIdExported, setIdNotExported,
			  setIdInfo, lazySetIdInfo, modifyIdInfo, 
			  maybeModifyIdInfo,
			  globalIdDetails
			)
import qualified Var	( mkLocalId, mkGlobalId, mkSpecPragmaId, mkExportedLocalId )
import TyCon		( FieldLabel, TyCon )
import Type		( Type, typePrimRep, addFreeTyVars, seqType, 
			  splitTyConApp_maybe, PrimRep )
import TysPrim		( statePrimTyCon )
import IdInfo 

#ifdef OLD_STRICTNESS
import qualified Demand	( Demand )
#endif
import DataCon		( isUnboxedTupleCon )
import NewDemand	( Demand, StrictSig, topDmd, topSig, isBottomingSig )
import Name	 	( Name, OccName, nameIsLocalOrFrom, 
			  mkSystemVarName, mkSystemVarNameEncoded, mkInternalName,
			  getOccName, getSrcLoc
			) 
import Module		( Module )
import OccName		( EncodedFS, mkWorkerOcc )
import Maybes		( orElse )
import SrcLoc		( SrcLoc )
import Outputable
import Unique		( Unique, mkBuiltinUnique )
import StaticFlags	( opt_NoStateHack )

-- infixl so you can say (id `set` a `set` b)
infixl 	1 `setIdUnfolding`,
	  `setIdArity`,
	  `setIdNewDemandInfo`,
	  `setIdNewStrictness`,
	  `setIdWorkerInfo`,
	  `setIdSpecialisation`,
	  `setInlinePragma`,
	  `idCafInfo`
#ifdef OLD_STRICTNESS
	  ,`idCprInfo`
	  ,`setIdStrictness`
	  ,`setIdDemandInfo`
#endif
\end{code}



%************************************************************************
%*									*
\subsection{Simple Id construction}
%*									*
%************************************************************************

Absolutely all Ids are made by mkId.  It is just like Var.mkId,
but in addition it pins free-tyvar-info onto the Id's type, 
where it can easily be found.

\begin{code}
mkLocalIdWithInfo :: Name -> Type -> IdInfo -> Id
mkLocalIdWithInfo name ty info = Var.mkLocalId name (addFreeTyVars ty) info

mkSpecPragmaId :: Name -> Type -> Id
mkSpecPragmaId name ty = Var.mkSpecPragmaId name (addFreeTyVars ty) vanillaIdInfo

mkExportedLocalId :: Name -> Type -> Id
mkExportedLocalId name ty = Var.mkExportedLocalId name (addFreeTyVars ty) vanillaIdInfo

mkGlobalId :: GlobalIdDetails -> Name -> Type -> IdInfo -> Id
mkGlobalId details name ty info = Var.mkGlobalId details name (addFreeTyVars ty) info
\end{code}

\begin{code}
mkLocalId :: Name -> Type -> Id
mkLocalId name ty = mkLocalIdWithInfo name ty vanillaIdInfo

-- SysLocal: for an Id being created by the compiler out of thin air...
-- UserLocal: an Id with a name the user might recognize...
mkUserLocal :: OccName -> Unique -> Type -> SrcLoc -> Id
mkSysLocal  :: EncodedFS  -> Unique -> Type -> Id
mkVanillaGlobal :: Name -> Type -> IdInfo -> Id

-- for SysLocal, we assume the base name is already encoded, to avoid
-- re-encoding the same string over and over again.
mkSysLocal fs uniq ty = mkLocalId (mkSystemVarNameEncoded uniq fs) ty

-- version to use when the faststring needs to be encoded
mkSysLocalUnencoded fs uniq ty = mkLocalId (mkSystemVarName uniq fs)  ty

mkUserLocal occ uniq ty loc = mkLocalId (mkInternalName    uniq occ loc) ty
mkVanillaGlobal 	    = mkGlobalId VanillaGlobal
\end{code}

Make some local @Ids@ for a template @CoreExpr@.  These have bogus
@Uniques@, but that's OK because the templates are supposed to be
instantiated before use.
 
\begin{code}
-- "Wild Id" typically used when you need a binder that you don't expect to use
mkWildId :: Type -> Id
mkWildId ty = mkSysLocal FSLIT("wild") (mkBuiltinUnique 1) ty

mkWorkerId :: Unique -> Id -> Type -> Id
-- A worker gets a local name.  CoreTidy will externalise it if necessary.
mkWorkerId uniq unwrkr ty
  = mkLocalId wkr_name ty
  where
    wkr_name = mkInternalName uniq (mkWorkerOcc (getOccName unwrkr)) (getSrcLoc unwrkr)

-- "Template locals" typically used in unfoldings
mkTemplateLocals :: [Type] -> [Id]
mkTemplateLocals tys = zipWith mkTemplateLocal [1..] tys

mkTemplateLocalsNum :: Int -> [Type] -> [Id]
-- The Int gives the starting point for unique allocation
mkTemplateLocalsNum n tys = zipWith mkTemplateLocal [n..] tys

mkTemplateLocal :: Int -> Type -> Id
mkTemplateLocal i ty = mkSysLocal FSLIT("tpl") (mkBuiltinUnique i) ty
\end{code}


%************************************************************************
%*									*
\subsection[Id-general-funs]{General @Id@-related functions}
%*									*
%************************************************************************

\begin{code}
setIdType :: Id -> Type -> Id
	-- Add free tyvar info to the type
setIdType id ty = seqType ty `seq` Var.setIdType id (addFreeTyVars ty)

idPrimRep :: Id -> PrimRep
idPrimRep id = typePrimRep (idType id)
\end{code}


%************************************************************************
%*									*
\subsection{Special Ids}
%*									*
%************************************************************************

The @SpecPragmaId@ exists only to make Ids that are
on the *LHS* of bindings created by SPECIALISE pragmas; 
eg:		s = f Int d
The SpecPragmaId is never itself mentioned; it
exists solely so that the specialiser will find
the call to f, and make specialised version of it.
The SpecPragmaId binding is discarded by the specialiser
when it gathers up overloaded calls.
Meanwhile, it is not discarded as dead code.


\begin{code}
recordSelectorFieldLabel :: Id -> (TyCon, FieldLabel)
recordSelectorFieldLabel id = case globalIdDetails id of
				 RecordSelId tycon lbl -> (tycon,lbl)
				 other -> panic "recordSelectorFieldLabel"

isRecordSelector id = case globalIdDetails id of
			RecordSelId _ _ -> True
			other	  	-> False

isPrimOpId id = case globalIdDetails id of
		    PrimOpId op -> True
		    other	-> False

isPrimOpId_maybe id = case globalIdDetails id of
			    PrimOpId op -> Just op
			    other	-> Nothing

isFCallId id = case globalIdDetails id of
		    FCallId call -> True
		    other	 -> False

isFCallId_maybe id = case globalIdDetails id of
			    FCallId call -> Just call
			    other	 -> Nothing

isDataConWorkId id = case globalIdDetails id of
			DataConWorkId _ -> True
			other	        -> False

isDataConWorkId_maybe id = case globalIdDetails id of
			  DataConWorkId con -> Just con
			  other	            -> Nothing

-- hasNoBinding returns True of an Id which may not have a
-- binding, even though it is defined in this module.  
-- Data constructor workers used to be things of this kind, but
-- they aren't any more.  Instead, we inject a binding for 
-- them at the CorePrep stage. 
-- EXCEPT: unboxed tuples, which definitely have no binding
hasNoBinding id = case globalIdDetails id of
			PrimOpId _  	 -> True
			FCallId _   	 -> True
			DataConWorkId dc -> isUnboxedTupleCon dc
			other	         -> False

isImplicitId :: Id -> Bool
	-- isImplicitId tells whether an Id's info is implied by other
	-- declarations, so we don't need to put its signature in an interface
	-- file, even if it's mentioned in some other interface unfolding.
isImplicitId id
  = case globalIdDetails id of
	RecordSelId _ _ -> True
        FCallId _       -> True
        PrimOpId _      -> True
	ClassOpId _	-> True
        DataConWorkId _ -> True
	DataConWrapId _ -> True
		-- These are are implied by their type or class decl;
		-- remember that all type and class decls appear in the interface file.
		-- The dfun id is not an implicit Id; it must *not* be omitted, because 
		-- it carries version info for the instance decl
	other		-> False

idIsFrom :: Module -> Id -> Bool
idIsFrom mod id = nameIsLocalOrFrom mod (idName id)
\end{code}

\begin{code}
isDeadBinder :: Id -> Bool
isDeadBinder bndr | isId bndr = isDeadOcc (idOccInfo bndr)
		  | otherwise = False	-- TyVars count as not dead
\end{code}


%************************************************************************
%*									*
\subsection{IdInfo stuff}
%*									*
%************************************************************************

\begin{code}
	---------------------------------
	-- ARITY
idArity :: Id -> Arity
idArity id = arityInfo (idInfo id)

setIdArity :: Id -> Arity -> Id
setIdArity id arity = modifyIdInfo (`setArityInfo` arity) id

#ifdef OLD_STRICTNESS
	---------------------------------
	-- (OLD) STRICTNESS 
idStrictness :: Id -> StrictnessInfo
idStrictness id = strictnessInfo (idInfo id)

setIdStrictness :: Id -> StrictnessInfo -> Id
setIdStrictness id strict_info = modifyIdInfo (`setStrictnessInfo` strict_info) id
#endif

-- isBottomingId returns true if an application to n args would diverge
isBottomingId :: Id -> Bool
isBottomingId id = isBottomingSig (idNewStrictness id)

idNewStrictness_maybe :: Id -> Maybe StrictSig
idNewStrictness :: Id -> StrictSig

idNewStrictness_maybe id = newStrictnessInfo (idInfo id)
idNewStrictness       id = idNewStrictness_maybe id `orElse` topSig

setIdNewStrictness :: Id -> StrictSig -> Id
setIdNewStrictness id sig = modifyIdInfo (`setNewStrictnessInfo` Just sig) id

zapIdNewStrictness :: Id -> Id
zapIdNewStrictness id = modifyIdInfo (`setNewStrictnessInfo` Nothing) id

	---------------------------------
	-- WORKER ID
idWorkerInfo :: Id -> WorkerInfo
idWorkerInfo id = workerInfo (idInfo id)

setIdWorkerInfo :: Id -> WorkerInfo -> Id
setIdWorkerInfo id work_info = modifyIdInfo (`setWorkerInfo` work_info) id

	---------------------------------
	-- UNFOLDING
idUnfolding :: Id -> Unfolding
idUnfolding id = unfoldingInfo (idInfo id)

setIdUnfolding :: Id -> Unfolding -> Id
setIdUnfolding id unfolding = modifyIdInfo (`setUnfoldingInfo` unfolding) id

#ifdef OLD_STRICTNESS
	---------------------------------
	-- (OLD) DEMAND
idDemandInfo :: Id -> Demand.Demand
idDemandInfo id = demandInfo (idInfo id)

setIdDemandInfo :: Id -> Demand.Demand -> Id
setIdDemandInfo id demand_info = modifyIdInfo (`setDemandInfo` demand_info) id
#endif

idNewDemandInfo_maybe :: Id -> Maybe NewDemand.Demand
idNewDemandInfo       :: Id -> NewDemand.Demand

idNewDemandInfo_maybe id = newDemandInfo (idInfo id)
idNewDemandInfo       id = newDemandInfo (idInfo id) `orElse` NewDemand.topDmd

setIdNewDemandInfo :: Id -> NewDemand.Demand -> Id
setIdNewDemandInfo id dmd = modifyIdInfo (`setNewDemandInfo` Just dmd) id

	---------------------------------
	-- SPECIALISATION
idSpecialisation :: Id -> CoreRules
idSpecialisation id = specInfo (idInfo id)

idCoreRules :: Id -> [IdCoreRule]
idCoreRules id = [IdCoreRule id False rule | rule <- rulesRules (idSpecialisation id)]

setIdSpecialisation :: Id -> CoreRules -> Id
setIdSpecialisation id spec_info = modifyIdInfo (`setSpecInfo` spec_info) id

	---------------------------------
	-- CAF INFO
idCafInfo :: Id -> CafInfo
#ifdef OLD_STRICTNESS
idCafInfo id = case cgInfo (idInfo id) of
		  NoCgInfo -> pprPanic "idCafInfo" (ppr id)
		  info     -> cgCafInfo info
#else
idCafInfo id = cafInfo (idInfo id)
#endif

setIdCafInfo :: Id -> CafInfo -> Id
setIdCafInfo id caf_info = modifyIdInfo (`setCafInfo` caf_info) id

	---------------------------------
	-- CPR INFO
#ifdef OLD_STRICTNESS
idCprInfo :: Id -> CprInfo
idCprInfo id = cprInfo (idInfo id)

setIdCprInfo :: Id -> CprInfo -> Id
setIdCprInfo id cpr_info = modifyIdInfo (`setCprInfo` cpr_info) id
#endif

	---------------------------------
	-- Occcurrence INFO
idOccInfo :: Id -> OccInfo
idOccInfo id = occInfo (idInfo id)

setIdOccInfo :: Id -> OccInfo -> Id
setIdOccInfo id occ_info = modifyIdInfo (`setOccInfo` occ_info) id
\end{code}


	---------------------------------
	-- INLINING
The inline pragma tells us to be very keen to inline this Id, but it's still
OK not to if optimisation is switched off.

\begin{code}
idInlinePragma :: Id -> InlinePragInfo
idInlinePragma id = inlinePragInfo (idInfo id)

setInlinePragma :: Id -> InlinePragInfo -> Id
setInlinePragma id prag = modifyIdInfo (`setInlinePragInfo` prag) id

modifyInlinePragma :: Id -> (InlinePragInfo -> InlinePragInfo) -> Id
modifyInlinePragma id fn = modifyIdInfo (\info -> info `setInlinePragInfo` (fn (inlinePragInfo info))) id
\end{code}


	---------------------------------
	-- ONE-SHOT LAMBDAS
\begin{code}
idLBVarInfo :: Id -> LBVarInfo
idLBVarInfo id = lbvarInfo (idInfo id)

isOneShotBndr :: Id -> Bool
-- This one is the "business end", called externally.
-- Its main purpose is to encapsulate the Horrible State Hack
isOneShotBndr id = isOneShotLambda id || (isStateHackType (idType id))

isStateHackType :: Type -> Bool
isStateHackType ty
  | opt_NoStateHack 
  = False
  | otherwise
  = case splitTyConApp_maybe ty of
	Just (tycon,_) -> tycon == statePrimTyCon
        other          -> False
	-- This is a gross hack.  It claims that 
	-- every function over realWorldStatePrimTy is a one-shot
	-- function.  This is pretty true in practice, and makes a big
	-- difference.  For example, consider
	--	a `thenST` \ r -> ...E...
	-- The early full laziness pass, if it doesn't know that r is one-shot
	-- will pull out E (let's say it doesn't mention r) to give
	--	let lvl = E in a `thenST` \ r -> ...lvl...
	-- When `thenST` gets inlined, we end up with
	--	let lvl = E in \s -> case a s of (r, s') -> ...lvl...
	-- and we don't re-inline E.
	--
	-- It would be better to spot that r was one-shot to start with, but
	-- I don't want to rely on that.
	--
	-- Another good example is in fill_in in PrelPack.lhs.  We should be able to
	-- spot that fill_in has arity 2 (and when Keith is done, we will) but we can't yet.


-- The OneShotLambda functions simply fiddle with the IdInfo flag
isOneShotLambda :: Id -> Bool
isOneShotLambda id = case idLBVarInfo id of
                       IsOneShotLambda  -> True
                       NoLBVarInfo      -> False

setOneShotLambda :: Id -> Id
setOneShotLambda id = modifyIdInfo (`setLBVarInfo` IsOneShotLambda) id

clearOneShotLambda :: Id -> Id
clearOneShotLambda id 
  | isOneShotLambda id = modifyIdInfo (`setLBVarInfo` NoLBVarInfo) id
  | otherwise	       = id			

-- But watch out: this may change the type of something else
--	f = \x -> e
-- If we change the one-shot-ness of x, f's type changes
\end{code}

\begin{code}
zapLamIdInfo :: Id -> Id
zapLamIdInfo id = maybeModifyIdInfo zapLamInfo id

zapDemandIdInfo id = maybeModifyIdInfo zapDemandInfo id
\end{code}

