%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
module Id (
	Id, DictId,

	-- Simple construction
	mkVanillaId, mkImportedId, mkSysLocal, mkUserLocal,
	mkTemplateLocals, mkWildId, mkUserId,

	-- Taking an Id apart
	idName, idType, idUnique, idInfo, idDetails,
	idPrimRep, isId,
	recordSelectorFieldLabel,

	-- Modifying an Id
	setIdName, setIdUnique, setIdType, setIdInfo,

	-- Predicates
	omitIfaceSigForId,
	externallyVisibleId,
	idFreeTyVars, 

	-- Inline pragma stuff
	getInlinePragma, setInlinePragma, modifyInlinePragma, 
	idWantsToBeINLINEd, idMustBeINLINEd, idMustNotBeINLINEd,
	isSpecPragmaId,
	

	isRecordSelector,
	isPrimitiveId_maybe, isDataConId_maybe,
	isConstantId,
	isBottomingId, idAppIsBottom,

	-- IdInfo stuff
	setIdUnfolding,
	setIdArity,
	setIdDemandInfo,
	setIdStrictness,
	setIdSpecialisation,
	setIdUpdateInfo,
	setIdCafInfo,

	getIdArity,
	getIdDemandInfo,
	getIdStrictness,
	getIdUnfolding,
	getIdSpecialisation,
	getIdUpdateInfo,
	getIdCafInfo

    ) where

#include "HsVersions.h"

import {-# SOURCE #-} CoreUnfold ( Unfolding )

import Var		( Id, DictId, VarDetails(..), 
			  isId, mkId, 
			  idName, idType, idUnique, idInfo, idDetails,
			  setIdName, setVarType, setIdUnique, setIdInfo, modifyIdInfo,
			  externallyVisibleId
			)
import VarSet
import Type		( Type, tyVarsOfType, typePrimRep, addFreeTyVars )
import IdInfo
import Demand		( Demand )
import Name	 	( Name, OccName, Module,
			  mkSysLocalName, mkLocalName,
			  isWiredInName
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
	  `setIdSpecialisation`,
	  `setIdUpdateInfo`,
	  `setInlinePragma`
	-- infixl so you can say (id `set` a `set` b)
\end{code}



%************************************************************************
%*									*
\subsection{Simple Id construction}
%*									*
%************************************************************************

\begin{code}
mkVanillaId :: Name -> Type -> Id
mkVanillaId name ty = mkId name (addFreeTyVars ty) VanillaId noIdInfo

mkImportedId :: Name -> Type -> IdInfo -> Id
mkImportedId name ty info = mkId name (addFreeTyVars ty) VanillaId info

mkUserId :: Name -> Type -> Id
mkUserId name ty = mkVanillaId name ty

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

omitIfaceSigForId tells whether an Id's info is implied by other declarations,
so we don't need to put its signature in an interface file, even if it's mentioned
in some other interface unfolding.

\begin{code}
omitIfaceSigForId :: Id -> Bool
omitIfaceSigForId id
  | isWiredInName (idName id)
  = True

  | otherwise
  = case idDetails id of
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
\subsection{Special Ids}
%*									*
%************************************************************************

\begin{code}
recordSelectorFieldLabel :: Id -> FieldLabel
recordSelectorFieldLabel id = case idDetails id of
				RecordSelId lbl -> lbl

isRecordSelector id = case idDetails id of
			RecordSelId lbl -> True
			other	  	-> False

isPrimitiveId_maybe id = case idDetails id of
			    ConstantId (PrimOp op) -> Just op
			    other		   -> Nothing

isDataConId_maybe id = case idDetails id of
			  ConstantId (DataCon con) -> Just con
			  other		           -> Nothing

isConstantId id = case idDetails id of
		    ConstantId _ -> True
		    other	 -> False
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
setIdArity id arity = modifyIdInfo id (arity `setArityInfo`)

	---------------------------------
	-- STRICTNESS
getIdStrictness :: Id -> StrictnessInfo
getIdStrictness id = strictnessInfo (idInfo id)

setIdStrictness :: Id -> StrictnessInfo -> Id
setIdStrictness id strict_info = modifyIdInfo id (strict_info `setStrictnessInfo`)

-- isBottomingId returns true if an application to n args would diverge
isBottomingId :: Id -> Bool
isBottomingId id = isBottomingStrictness (strictnessInfo (idInfo id))

idAppIsBottom :: Id -> Int -> Bool
idAppIsBottom id n = appIsBottom (strictnessInfo (idInfo id)) n

	---------------------------------
	-- UNFOLDING
getIdUnfolding :: Id -> Unfolding
getIdUnfolding id = unfoldingInfo (idInfo id)

setIdUnfolding :: Id -> Unfolding -> Id
setIdUnfolding id unfolding = modifyIdInfo id (unfolding `setUnfoldingInfo`)

	---------------------------------
	-- DEMAND
getIdDemandInfo :: Id -> Demand
getIdDemandInfo id = demandInfo (idInfo id)

setIdDemandInfo :: Id -> Demand -> Id
setIdDemandInfo id demand_info = modifyIdInfo id (demand_info `setDemandInfo`)

	---------------------------------
	-- UPDATE INFO
getIdUpdateInfo :: Id -> UpdateInfo
getIdUpdateInfo id = updateInfo (idInfo id)

setIdUpdateInfo :: Id -> UpdateInfo -> Id
setIdUpdateInfo id upd_info = modifyIdInfo id (upd_info `setUpdateInfo`)

	---------------------------------
	-- SPECIALISATION
getIdSpecialisation :: Id -> IdSpecEnv
getIdSpecialisation id = specInfo (idInfo id)

setIdSpecialisation :: Id -> IdSpecEnv -> Id
setIdSpecialisation id spec_info = modifyIdInfo id (spec_info `setSpecInfo`)

	---------------------------------
	-- CAF INFO
getIdCafInfo :: Id -> CafInfo
getIdCafInfo id = cafInfo (idInfo id)

setIdCafInfo :: Id -> CafInfo -> Id
setIdCafInfo id caf_info = modifyIdInfo id (caf_info `setCafInfo`)
\end{code}


	---------------------------------
	-- INLINING
The inline pragma tells us to be very keen to inline this Id, but it's still
OK not to if optimisation is switched off.

\begin{code}
getInlinePragma :: Id -> InlinePragInfo
getInlinePragma id = inlinePragInfo (idInfo id)

setInlinePragma :: Id -> InlinePragInfo -> Id
setInlinePragma id prag = modifyIdInfo id (setInlinePragInfo prag)

modifyInlinePragma :: Id -> (InlinePragInfo -> InlinePragInfo) -> Id
modifyInlinePragma id fn = modifyIdInfo id (\info -> setInlinePragInfo (fn (inlinePragInfo info)) info)

idWantsToBeINLINEd :: Id -> Bool
idWantsToBeINLINEd id = case getInlinePragma id of
			  IWantToBeINLINEd -> True
			  IMustBeINLINEd   -> True
			  other		   -> False

idMustNotBeINLINEd id = case getInlinePragma id of
			  IMustNotBeINLINEd -> True
			  IAmASpecPragmaId  -> True
			  IAmALoopBreaker   -> True
			  other		    -> False

idMustBeINLINEd id =  case getInlinePragma id of
			IMustBeINLINEd -> True
			other	       -> False

isSpecPragmaId id = case getInlinePragma id of
			IAmASpecPragmaId -> True
			other		 -> False
\end{code}
