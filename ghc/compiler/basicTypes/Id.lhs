%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
module Id (
	Id, DictId, GenId,

	-- Simple construction
	mkVanillaId, mkImportedId, mkSysLocal, mkUserLocal,
	mkTemplateLocals, mkWildId, mkUserId,

	-- Taking an Id apart
	idName, idType, idUnique, idInfo,
	idPrimRep, isId,
	recordSelectorFieldLabel,

	-- Modifying an Id
	setIdName, setIdUnique, setIdType, setIdInfo,
	setIdVisibility, mkIdVisible,

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
	isBottomingId, 

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

import Var		( Id, GenId, DictId, VarDetails(..), 
			  isId, mkId, 
			  idName, idType, idUnique, idInfo, varDetails,
			  setIdName, setVarType, setIdUnique, setIdInfo, modifyIdInfo,
			  externallyVisibleId
			)
import VarSet
import Type		( GenType, Type, tyVarsOfType, typePrimRep, addFreeTyVars )
import IdInfo
import Demand		( Demand )
import Name	 	( Name, OccName, 
			  mkSysLocalName, mkLocalName,
			  isWiredInName, setNameVisibility, mkNameVisible
			) 
import Const		( Con(..) )
import PrimRep		( PrimRep )
import PrimOp		( PrimOp )
import FieldLabel	( FieldLabel(..) )
import BasicTypes	( Module )
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
mkVanillaId :: Name -> (GenType flexi) -> GenId flexi
mkVanillaId name ty = mkId name ty VanillaId noIdInfo

mkImportedId :: Name -> Type -> IdInfo -> Id
mkImportedId name ty info = mkId name ty VanillaId info

mkUserId :: Name -> GenType flexi -> GenId flexi
mkUserId name ty = mkVanillaId name ty

-- SysLocal: for an Id being created by the compiler out of thin air...
-- UserLocal: an Id with a name the user might recognize...
mkUserLocal :: OccName -> Unique -> GenType flexi -> GenId flexi
mkSysLocal  ::            Unique -> GenType flexi -> GenId flexi

mkSysLocal  uniq ty     = mkVanillaId (mkSysLocalName uniq)  ty
mkUserLocal occ uniq ty = mkVanillaId (mkLocalName uniq occ) ty
\end{code}

Make some local @Ids@ for a template @CoreExpr@.  These have bogus
@Uniques@, but that's OK because the templates are supposed to be
instantiated before use.

\begin{code}
-- "Wild Id" typically used when you need a binder that you don't expect to use
mkWildId :: Type -> Id
mkWildId ty = mkSysLocal (mkBuiltinUnique 1) ty

-- "Template locals" typically used in unfoldings
mkTemplateLocals :: [Type] -> [Id]
mkTemplateLocals tys = zipWith mkSysLocal
			       (getBuiltinUniques (length tys))
			       tys
\end{code}


%************************************************************************
%*									*
\subsection[Id-general-funs]{General @Id@-related functions}
%*									*
%************************************************************************

\begin{code}
idFreeTyVars :: (GenId flexi) -> (GenTyVarSet flexi)
idFreeTyVars id = tyVarsOfType (idType id)

setIdType :: GenId flexi1 -> GenType flexi2 -> GenId flexi2
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
  = case varDetails id of
	RecordSelId _  -> True	-- Includes dictionary selectors
        ConstantId _   -> True
		-- ConstantIds are implied by their type or class decl;
		-- remember that all type and class decls appear in the interface file.
		-- The dfun id must *not* be omitted, because it carries version info for
		-- the instance decl

	other	       -> False	-- Don't omit!
\end{code}

See notes with setNameVisibility (Name.lhs)

\begin{code}
setIdVisibility :: Maybe Module -> Unique -> Id -> Id
setIdVisibility maybe_mod u id
  = setIdName id (setNameVisibility maybe_mod u (idName id))

mkIdVisible :: Module -> Unique -> Id -> Id
mkIdVisible mod u id 
  = setIdName id (mkNameVisible mod u (idName id))
\end{code}

%************************************************************************
%*									*
\subsection{Special Ids}
%*									*
%************************************************************************

\begin{code}
recordSelectorFieldLabel :: Id -> FieldLabel
recordSelectorFieldLabel id = case varDetails id of
				RecordSelId lbl -> lbl

isRecordSelector id = case varDetails id of
			RecordSelId lbl -> True
			other	  	-> False

isPrimitiveId_maybe id = case varDetails id of
			    ConstantId (PrimOp op) -> Just op
			    other		   -> Nothing

isDataConId_maybe id = case varDetails id of
			  ConstantId (DataCon con) -> Just con
			  other		           -> Nothing

isConstantId id = case varDetails id of
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
getIdArity :: GenId flexi -> ArityInfo
getIdArity id = arityInfo (idInfo id)

setIdArity :: GenId flexi -> ArityInfo -> GenId flexi
setIdArity id arity = modifyIdInfo id (arity `setArityInfo`)

	---------------------------------
	-- STRICTNESS
getIdStrictness :: GenId flexi -> StrictnessInfo
getIdStrictness id = strictnessInfo (idInfo id)

setIdStrictness :: GenId flexi -> StrictnessInfo -> GenId flexi
setIdStrictness id strict_info = modifyIdInfo id (strict_info `setStrictnessInfo`)

isBottomingId :: GenId flexi -> Bool
isBottomingId id = bottomIsGuaranteed (strictnessInfo (idInfo id))

	---------------------------------
	-- UNFOLDING
getIdUnfolding :: GenId flexi -> Unfolding
getIdUnfolding id = unfoldingInfo (idInfo id)

setIdUnfolding :: GenId flexi -> Unfolding -> GenId flexi
setIdUnfolding id unfolding = modifyIdInfo id (unfolding `setUnfoldingInfo`)

	---------------------------------
	-- DEMAND
getIdDemandInfo :: GenId flexi -> Demand
getIdDemandInfo id = demandInfo (idInfo id)

setIdDemandInfo :: GenId flexi -> Demand -> GenId flexi
setIdDemandInfo id demand_info = modifyIdInfo id (demand_info `setDemandInfo`)

	---------------------------------
	-- UPDATE INFO
getIdUpdateInfo :: GenId flexi -> UpdateInfo
getIdUpdateInfo id = updateInfo (idInfo id)

setIdUpdateInfo :: GenId flexi -> UpdateInfo -> GenId flexi
setIdUpdateInfo id upd_info = modifyIdInfo id (upd_info `setUpdateInfo`)

	---------------------------------
	-- SPECIALISATION
getIdSpecialisation :: GenId flexi -> IdSpecEnv
getIdSpecialisation id = specInfo (idInfo id)

setIdSpecialisation :: GenId flexi -> IdSpecEnv -> GenId flexi
setIdSpecialisation id spec_info = modifyIdInfo id (spec_info `setSpecInfo`)

	---------------------------------
	-- CAF INFO
getIdCafInfo :: GenId flexi -> CafInfo
getIdCafInfo id = cafInfo (idInfo id)

setIdCafInfo :: GenId flexi -> CafInfo -> GenId flexi
setIdCafInfo id caf_info = modifyIdInfo id (caf_info `setCafInfo`)
\end{code}


	---------------------------------
	-- INLINING
The inline pragma tells us to be very keen to inline this Id, but it's still
OK not to if optimisation is switched off.

\begin{code}
getInlinePragma :: GenId flexi -> InlinePragInfo
getInlinePragma id = inlinePragInfo (idInfo id)

setInlinePragma :: GenId flexi -> InlinePragInfo -> GenId flexi
setInlinePragma id prag = modifyIdInfo id (setInlinePragInfo prag)

modifyInlinePragma :: GenId flexi -> (InlinePragInfo -> InlinePragInfo) -> GenId flexi
modifyInlinePragma id fn = modifyIdInfo id (\info -> setInlinePragInfo (fn (inlinePragInfo info)) info)

idWantsToBeINLINEd :: GenId flexi -> Bool
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
