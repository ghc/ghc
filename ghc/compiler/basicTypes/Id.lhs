%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
module Id (
	Id, DictId,

	-- Simple construction
	mkGlobalId, mkLocalId, mkSpecPragmaId, mkLocalIdWithInfo,
	mkSysLocal, mkUserLocal, mkVanillaGlobal,
	mkTemplateLocals, mkTemplateLocalsNum, mkWildId, mkTemplateLocal,
	mkWorkerId,

	-- Taking an Id apart
	idName, idType, idUnique, idInfo,
	idPrimRep, isId, globalIdDetails,
	recordSelectorFieldLabel,

	-- Modifying an Id
	setIdName, setIdUnique, setIdType, setIdNoDiscard, setGlobalIdDetails,
	setIdInfo, lazySetIdInfo, modifyIdInfo, maybeModifyIdInfo,
	zapLamIdInfo, zapDemandIdInfo, 

	-- Predicates
	isImplicitId, isDeadBinder,
	isSpecPragmaId,	isExportedId, isLocalId, isGlobalId,
	isRecordSelector,
	isPrimOpId, isPrimOpId_maybe, 
	isFCallId, isFCallId_maybe,
	isDataConId, isDataConId_maybe, 
	isDataConWrapId, isDataConWrapId_maybe,
	isBottomingId,
	hasNoBinding,

	-- Inline pragma stuff
	idInlinePragma, setInlinePragma, modifyInlinePragma, 


	-- One shot lambda stuff
	isOneShotLambda, setOneShotLambda, clearOneShotLambda,

	-- IdInfo stuff
	setIdUnfolding,
	setIdArityInfo,
	setIdDemandInfo, setIdNewDemandInfo, 
	setIdStrictness, setIdNewStrictness, zapIdNewStrictness,
        setIdTyGenInfo,
	setIdWorkerInfo,
	setIdSpecialisation,
	setIdCgInfo,
	setIdCprInfo,
	setIdOccInfo,

	idArity, idArityInfo, 
	idDemandInfo, idNewDemandInfo,
	idStrictness, idNewStrictness, idNewStrictness_maybe, getNewStrictness,
        idTyGenInfo,
	idWorkerInfo,
	idUnfolding,
	idSpecialisation,
	idCgInfo,
	idCafInfo,
	idCgArity,
	idCprInfo,
	idLBVarInfo,
	idOccInfo,

	newStrictnessFromOld 	-- Temporary

    ) where

#include "HsVersions.h"


import CoreSyn		( Unfolding, CoreRules )
import BasicTypes	( Arity )
import Var		( Id, DictId,
			  isId, isExportedId, isSpecPragmaId, isLocalId,
			  idName, idType, idUnique, idInfo, isGlobalId,
			  setIdName, setVarType, setIdUnique, setIdNoDiscard,
			  setIdInfo, lazySetIdInfo, modifyIdInfo, 
			  maybeModifyIdInfo,
			  globalIdDetails, setGlobalIdDetails
			)
import qualified Var	( mkLocalId, mkGlobalId, mkSpecPragmaId )
import Type		( Type, typePrimRep, addFreeTyVars, 
                          usOnce, eqUsage, seqType, splitTyConApp_maybe )

import IdInfo 

import qualified Demand	( Demand )
import NewDemand	( Demand, DmdResult(..), StrictSig, topSig, isBotRes,
			  isBottomingSig, splitStrictSig, strictSigResInfo
			)
import Name	 	( Name, OccName,
			  mkSysLocalName, mkLocalName,
			  getOccName, getSrcLoc
			) 
import OccName		( UserFS, mkWorkerOcc )
import PrimRep		( PrimRep )
import TysPrim		( statePrimTyCon )
import FieldLabel	( FieldLabel )
import Maybes		( orElse )
import SrcLoc		( SrcLoc )
import Outputable
import Unique		( Unique, mkBuiltinUnique )

infixl 	1 `setIdUnfolding`,
	  `setIdArityInfo`,
	  `setIdDemandInfo`,
	  `setIdStrictness`,
	  `setIdNewDemandInfo`,
	  `setIdNewStrictness`,
	  `setIdTyGenInfo`,
	  `setIdWorkerInfo`,
	  `setIdSpecialisation`,
	  `setInlinePragma`,
	  `idCafInfo`,
	  `idCprInfo`

	-- infixl so you can say (id `set` a `set` b)
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

mkSpecPragmaId :: OccName -> Unique -> Type -> SrcLoc -> Id
mkSpecPragmaId occ uniq ty loc = Var.mkSpecPragmaId (mkLocalName uniq occ loc)
						    (addFreeTyVars ty)
						    vanillaIdInfo

mkGlobalId :: GlobalIdDetails -> Name -> Type -> IdInfo -> Id
mkGlobalId details name ty info = Var.mkGlobalId details name (addFreeTyVars ty) info
\end{code}

\begin{code}
mkLocalId :: Name -> Type -> Id
mkLocalId name ty = mkLocalIdWithInfo name ty vanillaIdInfo

-- SysLocal: for an Id being created by the compiler out of thin air...
-- UserLocal: an Id with a name the user might recognize...
mkUserLocal :: OccName -> Unique -> Type -> SrcLoc -> Id
mkSysLocal  :: UserFS  -> Unique -> Type -> Id
mkVanillaGlobal :: Name -> Type -> IdInfo -> Id

mkSysLocal  fs uniq ty      = mkLocalId (mkSysLocalName uniq fs)      ty
mkUserLocal occ uniq ty loc = mkLocalId (mkLocalName    uniq occ loc) ty
mkVanillaGlobal 	    = mkGlobalId VanillaGlobal
\end{code}

Make some local @Ids@ for a template @CoreExpr@.  These have bogus
@Uniques@, but that's OK because the templates are supposed to be
instantiated before use.
 
\begin{code}
-- "Wild Id" typically used when you need a binder that you don't expect to use
mkWildId :: Type -> Id
mkWildId ty = mkSysLocal SLIT("wild") (mkBuiltinUnique 1) ty

mkWorkerId :: Unique -> Id -> Type -> Id
-- A worker gets a local name.  CoreTidy will globalise it if necessary.
mkWorkerId uniq unwrkr ty
  = mkLocalId wkr_name ty
  where
    wkr_name = mkLocalName uniq (mkWorkerOcc (getOccName unwrkr)) (getSrcLoc unwrkr)

-- "Template locals" typically used in unfoldings
mkTemplateLocals :: [Type] -> [Id]
mkTemplateLocals tys = zipWith mkTemplateLocal [1..] tys

mkTemplateLocalsNum :: Int -> [Type] -> [Id]
-- The Int gives the starting point for unique allocation
mkTemplateLocalsNum n tys = zipWith mkTemplateLocal [n..] tys

mkTemplateLocal :: Int -> Type -> Id
mkTemplateLocal i ty = mkSysLocal SLIT("tpl") (mkBuiltinUnique i) ty
\end{code}


%************************************************************************
%*									*
\subsection[Id-general-funs]{General @Id@-related functions}
%*									*
%************************************************************************

\begin{code}
setIdType :: Id -> Type -> Id
	-- Add free tyvar info to the type
setIdType id ty = seqType ty `seq` setVarType id (addFreeTyVars ty)

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
recordSelectorFieldLabel :: Id -> FieldLabel
recordSelectorFieldLabel id = case globalIdDetails id of
				 RecordSelId lbl -> lbl

isRecordSelector id = case globalIdDetails id of
			RecordSelId lbl -> True
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

isDataConId id = case globalIdDetails id of
			DataConId _ -> True
			other	    -> False

isDataConId_maybe id = case globalIdDetails id of
			  DataConId con -> Just con
			  other	        -> Nothing

isDataConWrapId_maybe id = case globalIdDetails id of
				  DataConWrapId con -> Just con
				  other	            -> Nothing

isDataConWrapId id = case globalIdDetails id of
			DataConWrapId con -> True
			other	          -> False

	-- hasNoBinding returns True of an Id which may not have a
	-- binding, even though it is defined in this module.  Notably,
	-- the constructors of a dictionary are in this situation.
hasNoBinding id = case globalIdDetails id of
			DataConId _ -> True
			PrimOpId _  -> True
			FCallId _   -> True
			other	    -> False

isImplicitId :: Id -> Bool
	-- isImplicitId tells whether an Id's info is implied by other
	-- declarations, so we don't need to put its signature in an interface
	-- file, even if it's mentioned in some other interface unfolding.
isImplicitId id
  = case globalIdDetails id of
	RecordSelId _   -> True	-- Includes dictionary selectors
        FCallId _       -> True
        PrimOpId _      -> True
        DataConId _     -> True
	DataConWrapId _ -> True
		-- These are are implied by their type or class decl;
		-- remember that all type and class decls appear in the interface file.
		-- The dfun id must *not* be omitted, because it carries version info for
		-- the instance decl
	other		-> False
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
idArityInfo :: Id -> ArityInfo
idArityInfo id = arityInfo (idInfo id)

idArity :: Id -> Arity
idArity id = arityLowerBound (idArityInfo id)

setIdArityInfo :: Id -> Arity -> Id
setIdArityInfo id arity = modifyIdInfo (`setArityInfo` arity) id

	---------------------------------
	-- STRICTNESS 
idStrictness :: Id -> StrictnessInfo
idStrictness id = case strictnessInfo (idInfo id) of
			NoStrictnessInfo -> case idNewStrictness_maybe id of
						Just sig -> oldStrictnessFromNew sig
						Nothing  -> NoStrictnessInfo
			strictness -> strictness

setIdStrictness :: Id -> StrictnessInfo -> Id
setIdStrictness id strict_info = modifyIdInfo (`setStrictnessInfo` strict_info) id

-- isBottomingId returns true if an application to n args would diverge
isBottomingId :: Id -> Bool
isBottomingId id = isBottomingSig (idNewStrictness id)

idNewStrictness_maybe :: Id -> Maybe StrictSig
idNewStrictness :: Id -> StrictSig

idNewStrictness_maybe id = newStrictnessInfo (idInfo id)
idNewStrictness       id = idNewStrictness_maybe id `orElse` topSig

getNewStrictness :: Id -> StrictSig
-- First tries the "new-strictness" field, and then
-- reverts to the old one. This is just until we have
-- cross-module info for new strictness
getNewStrictness id = idNewStrictness_maybe id `orElse` newStrictnessFromOld id
		      
newStrictnessFromOld :: Id -> StrictSig
newStrictnessFromOld id = mkNewStrictnessInfo id (idArity id) (idStrictness id) (idCprInfo id)

oldStrictnessFromNew :: StrictSig -> StrictnessInfo
oldStrictnessFromNew sig = mkStrictnessInfo (map oldDemand dmds, isBotRes res_info)
			 where
			   (dmds, res_info) = splitStrictSig sig

setIdNewStrictness :: Id -> StrictSig -> Id
setIdNewStrictness id sig = modifyIdInfo (`setNewStrictnessInfo` Just sig) id

zapIdNewStrictness :: Id -> Id
zapIdNewStrictness id = modifyIdInfo (`setNewStrictnessInfo` Nothing) id

	---------------------------------
	-- TYPE GENERALISATION
idTyGenInfo :: Id -> TyGenInfo
idTyGenInfo id = tyGenInfo (idInfo id)

setIdTyGenInfo :: Id -> TyGenInfo -> Id
setIdTyGenInfo id tygen_info = modifyIdInfo (`setTyGenInfo` tygen_info) id

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

	---------------------------------
	-- DEMAND
idDemandInfo :: Id -> Demand.Demand
idDemandInfo id = demandInfo (idInfo id)

setIdDemandInfo :: Id -> Demand.Demand -> Id
setIdDemandInfo id demand_info = modifyIdInfo (`setDemandInfo` demand_info) id

idNewDemandInfo :: Id -> NewDemand.Demand
idNewDemandInfo id = newDemandInfo (idInfo id)

setIdNewDemandInfo :: Id -> NewDemand.Demand -> Id
setIdNewDemandInfo id dmd = modifyIdInfo (`setNewDemandInfo` dmd) id

	---------------------------------
	-- SPECIALISATION
idSpecialisation :: Id -> CoreRules
idSpecialisation id = specInfo (idInfo id)

setIdSpecialisation :: Id -> CoreRules -> Id
setIdSpecialisation id spec_info = modifyIdInfo (`setSpecInfo` spec_info) id

	---------------------------------
	-- CG INFO
idCgInfo :: Id -> CgInfo
#ifdef DEBUG
idCgInfo id = case cgInfo (idInfo id) of
		  NoCgInfo -> pprPanic "idCgInfo" (ppr id)
		  info     -> info
#else
idCgInfo id = cgInfo (idInfo id)
#endif		

setIdCgInfo :: Id -> CgInfo -> Id
setIdCgInfo id cg_info = modifyIdInfo (`setCgInfo` cg_info) id

	---------------------------------
	-- CAF INFO
idCafInfo :: Id -> CafInfo
idCafInfo id = cgCafInfo (idCgInfo id)

	---------------------------------
	-- CG ARITY
idCgArity :: Id -> Arity
idCgArity id = cgArity (idCgInfo id)

	---------------------------------
	-- CPR INFO
idCprInfo :: Id -> CprInfo
idCprInfo id = case cprInfo (idInfo id) of
		 NoCPRInfo -> case strictSigResInfo (idNewStrictness id) of
				RetCPR -> ReturnsCPR
				other  -> NoCPRInfo
		 ReturnsCPR -> ReturnsCPR

setIdCprInfo :: Id -> CprInfo -> Id
setIdCprInfo id cpr_info = modifyIdInfo (`setCprInfo` cpr_info) id

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

isOneShotLambda :: Id -> Bool
isOneShotLambda id = analysis || hack
  where analysis = case idLBVarInfo id of
                     LBVarInfo u    | u `eqUsage` usOnce      -> True
                     other                                    -> False
        hack     = case splitTyConApp_maybe (idType id) of
                     Just (tycon,_) | tycon == statePrimTyCon -> True
                     other                                    -> False

	-- The last clause is a gross hack.  It claims that 
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

setOneShotLambda :: Id -> Id
setOneShotLambda id = modifyIdInfo (`setLBVarInfo` LBVarInfo usOnce) id

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
