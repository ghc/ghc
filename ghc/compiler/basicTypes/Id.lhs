%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
module Id (
	Id, DictId,

	-- Simple construction
	mkId, mkVanillaId, mkSysLocal, mkUserLocal,
	mkTemplateLocals, mkWildId, mkTemplateLocal,

	-- Taking an Id apart
	idName, idType, idUnique, idInfo,
	idPrimRep, isId,
	recordSelectorFieldLabel,

	-- Modifying an Id
	setIdName, setIdUnique, setIdType, setIdNoDiscard, 
	setIdInfo, modifyIdInfo, maybeModifyIdInfo,

	-- Predicates
	omitIfaceSigForId,
	externallyVisibleId,
	idFreeTyVars, 

	-- Inline pragma stuff
	getInlinePragma, setInlinePragma, modifyInlinePragma, 
	idMustBeINLINEd, idMustNotBeINLINEd,

	isSpecPragmaId,	isRecordSelector,
	isPrimitiveId_maybe, isDataConId_maybe,
	isConstantId, isBottomingId, idAppIsBottom,
	isExportedId, isUserExportedId,

	-- IdInfo stuff
	setIdUnfolding,
	setIdArity,
	setIdDemandInfo,
	setIdStrictness,
	setIdWorkerInfo,
	setIdSpecialisation,
	setIdUpdateInfo,
	setIdCafInfo,
	setIdCprInfo,

	getIdArity,
	getIdDemandInfo,
	getIdStrictness,
	getIdWorkerInfo,
	getIdUnfolding,
	getIdSpecialisation,
	getIdUpdateInfo,
	getIdCafInfo,
	getIdCprInfo

    ) where

#include "HsVersions.h"

import {-# SOURCE #-} CoreUnfold ( Unfolding )
import {-# SOURCE #-} CoreSyn    ( CoreRules )

import Var		( Id, DictId,
			  isId, mkIdVar,
			  idName, idType, idUnique, idInfo,
			  setIdName, setVarType, setIdUnique, 
			  setIdInfo, modifyIdInfo, maybeModifyIdInfo,
			  externallyVisibleId
			)
import VarSet
import Type		( Type, tyVarsOfType, typePrimRep, addFreeTyVars )
import IdInfo
import Demand		( Demand, isStrict, wwLazy )
import Name	 	( Name, OccName,
			  mkSysLocalName, mkLocalName,
			  isWiredInName, isUserExportedName
			) 
import Const		( Con(..) )
import PrimRep		( PrimRep )
import PrimOp		( PrimOp )
import FieldLabel	( FieldLabel(..) )
import SrcLoc		( SrcLoc )
import Unique		( Unique, mkBuiltinUnique, getBuiltinUniques )
import Outputable

infixl 	1 `setIdUnfolding`,
	  `setIdArity`,
	  `setIdDemandInfo`,
	  `setIdStrictness`,
	  `setIdWorkerInfo`,
	  `setIdSpecialisation`,
	  `setIdUpdateInfo`,
	  `setInlinePragma`,
	  `getIdCafInfo`,
	  `getIdCprInfo`

	-- infixl so you can say (id `set` a `set` b)
\end{code}



%************************************************************************
%*									*
\subsection{Simple Id construction}
%*									*
%************************************************************************

Absolutely all Ids are made by mkId.  It 
	a) Pins free-tyvar-info onto the Id's type, 
	   where it can easily be found.
	b) Ensures that exported Ids are 

\begin{code}
mkId :: Name -> Type -> IdInfo -> Id
mkId name ty info = mkIdVar name (addFreeTyVars ty) info'
		  where
		    info' | isUserExportedName name = setNoDiscardInfo info
			  | otherwise		    = info
\end{code}

\begin{code}
mkVanillaId :: Name -> Type -> Id
mkVanillaId name ty = mkId name ty vanillaIdInfo

-- SysLocal: for an Id being created by the compiler out of thin air...
-- UserLocal: an Id with a name the user might recognize...
mkUserLocal :: OccName     -> Unique -> Type -> SrcLoc -> Id
mkSysLocal  :: FAST_STRING -> Unique -> Type -> Id

mkSysLocal  fs uniq ty      = mkVanillaId (mkSysLocalName uniq fs)      ty
mkUserLocal occ uniq ty loc = mkVanillaId (mkLocalName    uniq occ loc) ty
\end{code}

Make some local @Ids@ for a template @CoreExpr@.  These have bogus
@Uniques@, but that's OK because the templates are supposed to be
instantiated before use.

\begin{code}
-- "Wild Id" typically used when you need a binder that you don't expect to use
mkWildId :: Type -> Id
mkWildId ty = mkSysLocal SLIT("wild") (mkBuiltinUnique 1) ty

-- "Template locals" typically used in unfoldings
mkTemplateLocals :: [Type] -> [Id]
mkTemplateLocals tys = zipWith (mkSysLocal SLIT("tpl"))
			       (getBuiltinUniques (length tys))
			       tys

mkTemplateLocal :: Int -> Type -> Id
mkTemplateLocal i ty = mkSysLocal SLIT("tpl") (mkBuiltinUnique i) ty
\end{code}


%************************************************************************
%*									*
\subsection[Id-general-funs]{General @Id@-related functions}
%*									*
%************************************************************************

\begin{code}
idFreeTyVars :: Id -> TyVarSet
idFreeTyVars id = tyVarsOfType (idType id)

setIdType :: Id -> Type -> Id
	-- Add free tyvar info to the type
setIdType id ty = setVarType id (addFreeTyVars ty)

idPrimRep :: Id -> PrimRep
idPrimRep id = typePrimRep (idType id)
\end{code}


%************************************************************************
%*									*
\subsection{Special Ids}
%*									*
%************************************************************************

\begin{code}
idFlavour :: Id -> IdFlavour
idFlavour id = flavourInfo (idInfo id)

setIdNoDiscard :: Id -> Id
setIdNoDiscard id	-- Make an Id into a NoDiscardId, unless it is already
  = modifyIdInfo setNoDiscardInfo id

recordSelectorFieldLabel :: Id -> FieldLabel
recordSelectorFieldLabel id = case idFlavour id of
				RecordSelId lbl -> lbl

isRecordSelector id = case idFlavour id of
			RecordSelId lbl -> True
			other	  	-> False

isPrimitiveId_maybe id = case idFlavour id of
			    ConstantId (PrimOp op) -> Just op
			    other		   -> Nothing

isDataConId_maybe id = case idFlavour id of
			  ConstantId (DataCon con) -> Just con
			  other		           -> Nothing

isConstantId id = case idFlavour id of
		    ConstantId _ -> True
		    other	 -> False

isSpecPragmaId id = case idFlavour id of
			SpecPragmaId -> True
			other	     -> False

-- Don't drop a binding for an exported Id,
-- if it otherwise looks dead.  
isExportedId :: Id -> Bool
isExportedId id = case idFlavour id of
			VanillaId -> False
			other	  -> True	-- All the others are no-discard

-- Say if an Id was exported by the user
-- Implies isExportedId (see mkId above)
isUserExportedId :: Id -> Bool
isUserExportedId id = isUserExportedName (idName id)
\end{code}


omitIfaceSigForId tells whether an Id's info is implied by other declarations,
so we don't need to put its signature in an interface file, even if it's mentioned
in some other interface unfolding.

\begin{code}
omitIfaceSigForId :: Id -> Bool
omitIfaceSigForId id
  | isWiredInName (idName id)
  = True

  | otherwise
  = case idFlavour id of
	RecordSelId _  -> True	-- Includes dictionary selectors
        ConstantId _   -> True
		-- ConstantIds are implied by their type or class decl;
		-- remember that all type and class decls appear in the interface file.
		-- The dfun id must *not* be omitted, because it carries version info for
		-- the instance decl

	other	       -> False	-- Don't omit!
\end{code}



%************************************************************************
%*									*
\subsection{IdInfo stuff}
%*									*
%************************************************************************

\begin{code}
	---------------------------------
	-- ARITY
getIdArity :: Id -> ArityInfo
getIdArity id = arityInfo (idInfo id)

setIdArity :: Id -> ArityInfo -> Id
setIdArity id arity = modifyIdInfo (`setArityInfo` arity) id

	---------------------------------
	-- STRICTNESS
getIdStrictness :: Id -> StrictnessInfo
getIdStrictness id = strictnessInfo (idInfo id)

setIdStrictness :: Id -> StrictnessInfo -> Id
setIdStrictness id strict_info = modifyIdInfo (`setStrictnessInfo` strict_info) id

-- isBottomingId returns true if an application to n args would diverge
isBottomingId :: Id -> Bool
isBottomingId id = isBottomingStrictness (strictnessInfo (idInfo id))

idAppIsBottom :: Id -> Int -> Bool
idAppIsBottom id n = appIsBottom (strictnessInfo (idInfo id)) n

	---------------------------------
	-- WORKER ID
getIdWorkerInfo :: Id -> WorkerInfo
getIdWorkerInfo id = workerInfo (idInfo id)

setIdWorkerInfo :: Id -> WorkerInfo -> Id
setIdWorkerInfo id work_info = modifyIdInfo (`setWorkerInfo` work_info) id

	---------------------------------
	-- UNFOLDING
getIdUnfolding :: Id -> Unfolding
getIdUnfolding id = unfoldingInfo (idInfo id)

setIdUnfolding :: Id -> Unfolding -> Id
setIdUnfolding id unfolding = modifyIdInfo (`setUnfoldingInfo` unfolding) id

	---------------------------------
	-- DEMAND
getIdDemandInfo :: Id -> Demand
getIdDemandInfo id = demandInfo (idInfo id)

setIdDemandInfo :: Id -> Demand -> Id
setIdDemandInfo id demand_info = modifyIdInfo (`setDemandInfo` demand_info) id

	---------------------------------
	-- UPDATE INFO
getIdUpdateInfo :: Id -> UpdateInfo
getIdUpdateInfo id = updateInfo (idInfo id)

setIdUpdateInfo :: Id -> UpdateInfo -> Id
setIdUpdateInfo id upd_info = modifyIdInfo (`setUpdateInfo` upd_info) id

	---------------------------------
	-- SPECIALISATION
getIdSpecialisation :: Id -> CoreRules
getIdSpecialisation id = specInfo (idInfo id)

setIdSpecialisation :: Id -> CoreRules -> Id
setIdSpecialisation id spec_info = modifyIdInfo (`setSpecInfo` spec_info) id

	---------------------------------
	-- CAF INFO
getIdCafInfo :: Id -> CafInfo
getIdCafInfo id = cafInfo (idInfo id)

setIdCafInfo :: Id -> CafInfo -> Id
setIdCafInfo id caf_info = modifyIdInfo (`setCafInfo` caf_info) id

	---------------------------------
	-- CPR INFO
getIdCprInfo :: Id -> CprInfo
getIdCprInfo id = cprInfo (idInfo id)

setIdCprInfo :: Id -> CprInfo -> Id
setIdCprInfo id cpr_info = modifyIdInfo (`setCprInfo` cpr_info) id
\end{code}


	---------------------------------
	-- INLINING
The inline pragma tells us to be very keen to inline this Id, but it's still
OK not to if optimisation is switched off.

\begin{code}
getInlinePragma :: Id -> InlinePragInfo
getInlinePragma id = inlinePragInfo (idInfo id)

setInlinePragma :: Id -> InlinePragInfo -> Id
setInlinePragma id prag = modifyIdInfo (`setInlinePragInfo` prag) id

modifyInlinePragma :: Id -> (InlinePragInfo -> InlinePragInfo) -> Id
modifyInlinePragma id fn = modifyIdInfo (\info -> info `setInlinePragInfo` (fn (inlinePragInfo info))) id

idMustNotBeINLINEd id = case getInlinePragma id of
			  IMustNotBeINLINEd -> True
			  IAmALoopBreaker   -> True
			  other		    -> False

idMustBeINLINEd id =  case getInlinePragma id of
			IMustBeINLINEd -> True
			other	       -> False
\end{code}
