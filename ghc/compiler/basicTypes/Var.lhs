%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@Vars@: Variables}

\begin{code}
module Var (
	Var, VarDetails,		-- Abstract
	varName, varUnique, varInfo, varType,
	setVarName, setVarUnique, setVarType, setVarOcc,

	-- TyVars
	TyVar,
	tyVarName, tyVarKind,
	setTyVarName, setTyVarUnique,
	mkTyVar, mkSysTyVar, 
	mkMutTyVar, mutTyVarRef, makeTyVarImmutable, 

	-- Ids
	Id, DictId,
	idName, idType, idUnique, idInfo, modifyIdInfo, maybeModifyIdInfo,
	setIdName, setIdUnique, setIdInfo, lazySetIdInfo, 
	setIdLocalExported, zapSpecPragmaId,

	globalIdDetails, setGlobalIdDetails, 

	mkLocalId, mkGlobalId, mkSpecPragmaId,

	isTyVar, isMutTyVar, mutTyVarDetails,
	isId, isLocalVar, isLocalId,
	isGlobalId, isExportedId, isSpecPragmaId,
	mustHaveLocalBinding
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TypeRep( Type, Kind )
import {-# SOURCE #-}	TcType( TyVarDetails )
import {-# SOURCE #-}	IdInfo( GlobalIdDetails, notGlobalId,
				IdInfo, seqIdInfo )

import Name		( Name, OccName, NamedThing(..),
			  setNameUnique, setNameOcc, nameUnique, 
			  mkSystemTvNameEncoded,
			)
import Unique		( Unique, Uniquable(..), mkUniqueGrimily, getKey )
import FastTypes
import Outputable

import DATA_IOREF	( IORef )
\end{code}


%************************************************************************
%*									*
\subsection{The main data type declarations}
%*									*
%************************************************************************


Every @Var@ has a @Unique@, to uniquify it and for fast comparison, a
@Type@, and an @IdInfo@ (non-essential info about it, e.g.,
strictness).  The essential info about different kinds of @Vars@ is
in its @VarDetails@.

\begin{code}
data Var
  = Var {
	varName    :: !Name,
	realUnique :: FastInt,		-- Key for fast comparison
					-- Identical to the Unique in the name,
					-- cached here for speed
	varType    :: Type,
	varDetails :: VarDetails,
	varInfo    :: IdInfo		-- Only used for Ids at the moment
    }

data VarDetails
  = LocalId 		-- Used for locally-defined Ids (see NOTE below)
	LocalIdDetails

  | GlobalId 		-- Used for imported Ids, dict selectors etc
	GlobalIdDetails

  | TyVar
  | MutTyVar (IORef (Maybe Type)) 	-- Used during unification;
	     TyVarDetails
	-- TODO: the IORef should be unboxed here, but we don't want to unbox
  	-- the Name above.

	-- For a long time I tried to keep mutable Vars statically
	-- type-distinct from immutable Vars, but I've finally given
	-- up.  It's just too painful.  After type checking there are
	-- no MutTyVars left, but there's no static check of that
	-- fact.

data LocalIdDetails 
  = NotExported	-- Not exported
  | Exported	-- Exported
  | SpecPragma	-- Not exported, but not to be discarded either
		-- It's unclean that this is so deeply built in
\end{code}

LocalId and GlobalId
~~~~~~~~~~~~~~~~~~~~
A GlobalId is
  * always a constant (top-level)
  * imported, or data constructor, or primop, or record selector
  * has a Unique that is globally unique across the whole
    GHC invocation (a single invocation may compile multiple modules)

A LocalId is 
  * bound within an expression (lambda, case, local let(rec))
  * or defined at top level in the module being compiled

After CoreTidy, top-level LocalIds are turned into GlobalIds
 

\begin{code}
instance Outputable Var where
  ppr var = ppr (varName var)

instance Show Var where
  showsPrec p var = showsPrecSDoc p (ppr var)

instance NamedThing Var where
  getName = varName

instance Uniquable Var where
  getUnique = varUnique

instance Eq Var where
    a == b = realUnique a ==# realUnique b

instance Ord Var where
    a <= b = realUnique a <=# realUnique b
    a <	 b = realUnique a <#  realUnique b
    a >= b = realUnique a >=# realUnique b
    a >	 b = realUnique a >#  realUnique b
    a `compare` b = varUnique a `compare` varUnique b
\end{code}


\begin{code}
varUnique :: Var -> Unique
varUnique (Var {realUnique = uniq}) = mkUniqueGrimily uniq

setVarUnique :: Var -> Unique -> Var
setVarUnique var@(Var {varName = name}) uniq 
  = var {realUnique = getKey uniq, 
	 varName = setNameUnique name uniq}

setVarName :: Var -> Name -> Var
setVarName var new_name
  = var { realUnique = getKey (getUnique new_name), varName = new_name }

setVarOcc :: Var -> OccName -> Var
setVarOcc var new_occ
  = var { varName = setNameOcc (varName var) new_occ }

setVarType :: Var -> Type -> Var
setVarType var ty = var {varType = ty}
\end{code}


%************************************************************************
%*									*
\subsection{Type variables}
%*									*
%************************************************************************

\begin{code}
type TyVar = Var
\end{code}

\begin{code}
tyVarName = varName
tyVarKind = varType

setTyVarUnique = setVarUnique
setTyVarName   = setVarName
\end{code}

\begin{code}
mkTyVar :: Name -> Kind -> TyVar
mkTyVar name kind = Var { varName    = name
			, realUnique = getKey (nameUnique name)
			, varType    = kind
			, varDetails = TyVar
			, varInfo    = pprPanic "mkTyVar" (ppr name)
			}

mkSysTyVar :: Unique -> Kind -> TyVar
mkSysTyVar uniq kind = Var { varName    = name
			   , realUnique = getKey uniq
			   , varType    = kind
			   , varDetails = TyVar
			   , varInfo    = pprPanic "mkSysTyVar" (ppr name)
			   }
		     where
		       name = mkSystemTvNameEncoded uniq FSLIT("t")

mkMutTyVar :: Name -> Kind -> TyVarDetails -> IORef (Maybe Type) -> TyVar
mkMutTyVar name kind details ref
  = Var { varName    = name
	, realUnique = getKey (nameUnique name)
	, varType    = kind
	, varDetails = MutTyVar ref details
	, varInfo    = pprPanic "newMutTyVar" (ppr name)
	}

mutTyVarRef :: TyVar -> IORef (Maybe Type)
mutTyVarRef (Var {varDetails = MutTyVar loc _}) = loc

makeTyVarImmutable :: TyVar -> TyVar
makeTyVarImmutable tyvar = tyvar { varDetails = TyVar}

mutTyVarDetails :: TyVar -> TyVarDetails
mutTyVarDetails (Var {varDetails = MutTyVar _ details}) = details
\end{code}


%************************************************************************
%*									*
\subsection{Id Construction}
%*									*
%************************************************************************

Most Id-related functions are in Id.lhs and MkId.lhs

\begin{code}
type Id     = Var
type DictId = Id
\end{code}

\begin{code}
idName    = varName
idType    = varType
idUnique  = varUnique
idInfo	  = varInfo

setIdUnique :: Id -> Unique -> Id
setIdUnique = setVarUnique

setIdName :: Id -> Name -> Id
setIdName = setVarName

setIdLocalExported :: Id -> Id
setIdLocalExported id = id { varDetails = LocalId Exported }

zapSpecPragmaId :: Id -> Id
zapSpecPragmaId id 
  = case varDetails id of
	LocalId SpecPragma -> id { varDetails = LocalId NotExported }
	other		   -> id

lazySetIdInfo :: Id -> IdInfo -> Id
lazySetIdInfo var info = var {varInfo = info}

setIdInfo :: Id -> IdInfo -> Id
setIdInfo var info = seqIdInfo info `seq` var {varInfo = info}
	-- Try to avoid spack leaks by seq'ing

modifyIdInfo :: (IdInfo -> IdInfo) -> Id -> Id
modifyIdInfo fn var@(Var {varInfo = info})
  = seqIdInfo new_info `seq` var {varInfo = new_info}
  where
    new_info = fn info

-- maybeModifyIdInfo tries to avoid unnecesary thrashing
maybeModifyIdInfo :: (IdInfo -> Maybe IdInfo) -> Id -> Id
maybeModifyIdInfo fn var@(Var {varInfo = info}) = case fn info of
							Nothing       -> var
							Just new_info -> var {varInfo = new_info}
\end{code}

%************************************************************************
%*									*
\subsection{Predicates over variables
%*									*
%************************************************************************

\begin{code}
mkId :: Name -> Type -> VarDetails -> IdInfo -> Id
mkId name ty details info
  = Var { varName    = name, 
	  realUnique = getKey (nameUnique name), 	-- Cache the unique
	  varType    = ty,	
	  varDetails = details,
	  varInfo    = info }

mkLocalId :: Name -> Type -> IdInfo -> Id
mkLocalId name ty info = mkId name ty (LocalId NotExported) info

mkSpecPragmaId :: Name -> Type -> IdInfo -> Id
mkSpecPragmaId name ty info = mkId name ty (LocalId SpecPragma) info

mkGlobalId :: GlobalIdDetails -> Name -> Type -> IdInfo -> Id
mkGlobalId details name ty info = mkId name ty (GlobalId details) info
\end{code}

\begin{code}
isTyVar, isMutTyVar			 :: Var -> Bool
isId, isLocalVar, isLocalId    		 :: Var -> Bool
isGlobalId, isExportedId, isSpecPragmaId :: Var -> Bool
mustHaveLocalBinding			 :: Var -> Bool

isTyVar var = case varDetails var of
		TyVar        -> True
		MutTyVar _ _ -> True
		other	     -> False

isMutTyVar (Var {varDetails = MutTyVar _ _}) = True
isMutTyVar other			     = False


isId var = case varDetails var of
		LocalId _  -> True
		GlobalId _ -> True
		other	   -> False

isLocalId var = case varDetails var of
		  LocalId _  -> True
		  other	     -> False

-- isLocalVar returns True for type variables as well as local Ids
-- These are the variables that we need to pay attention to when finding free
-- variables, or doing dependency analysis.
isLocalVar var = case varDetails var of
		    LocalId _  	 -> True
		    TyVar      	 -> True
		    MutTyVar _ _ -> True
		    other	 -> False

-- mustHaveLocalBinding returns True of Ids and TyVars
-- that must have a binding in this module.  The converse
-- is not quite right: there are some GlobalIds that must have
-- bindings, such as record selectors.  But that doesn't matter,
-- because it's only used for assertions
mustHaveLocalBinding var = isLocalVar var

isGlobalId var = case varDetails var of
		   GlobalId _ -> True
		   other      -> False

-- isExportedId means "don't throw this away"
isExportedId var = case varDetails var of
			LocalId Exported   -> True
			LocalId SpecPragma -> True
			GlobalId _	   -> True
			other		   -> False

isSpecPragmaId var = case varDetails var of
			LocalId SpecPragma -> True
			other		   -> False
\end{code}

\begin{code}
globalIdDetails :: Var -> GlobalIdDetails
-- Works OK on local Ids too, returning notGlobalId
globalIdDetails var = case varDetails var of
			  GlobalId details -> details
			  other		   -> notGlobalId
setGlobalIdDetails :: Id -> GlobalIdDetails -> Id
setGlobalIdDetails id details = id { varDetails = GlobalId details }
\end{code}

