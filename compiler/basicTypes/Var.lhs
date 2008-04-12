%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@Vars@: Variables}

\begin{code}
module Var (
	Var, 
	varName, varUnique, varType,
	setVarName, setVarUnique, 

	-- TyVars
	TyVar, mkTyVar, mkTcTyVar, mkWildCoVar,
	tyVarName, tyVarKind,
	setTyVarName, setTyVarUnique, setTyVarKind,
	tcTyVarDetails,

        -- CoVars
        CoVar, coVarName, setCoVarUnique, setCoVarName, mkCoVar, isCoVar,

	-- Ids
	Id, DictId,
	idName, idType, idUnique, idInfo, modifyIdInfo, maybeModifyIdInfo,
	setIdName, setIdUnique, setIdType, setIdInfo, lazySetIdInfo, 
	setIdExported, setIdNotExported,

	globalIdDetails, globaliseId, 

	mkLocalId, mkExportedLocalId, mkGlobalId, 

	isTyVar, isTcTyVar, isId, isLocalVar, isLocalId,
	isGlobalId, isExportedId, 
	mustHaveLocalBinding
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TypeRep( Type, Kind )
import {-# SOURCE #-}	TcType( TcTyVarDetails, pprTcTyVarDetails )
import {-# SOURCE #-}	IdInfo( GlobalIdDetails, notGlobalId, 
                                IdInfo, seqIdInfo )
import {-# SOURCE #-}	TypeRep( isCoercionKind )

import Name hiding (varName)
import Unique
import FastTypes
import FastString
import Outputable       
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
  = TyVar {
	varName    :: !Name,
	realUnique :: FastInt,		-- Key for fast comparison
					-- Identical to the Unique in the name,
					-- cached here for speed
	varType       :: Kind,
        isCoercionVar :: Bool
 }

  | TcTyVar { 				-- Used only during type inference
					-- Used for kind variables during 
					-- inference, as well
	varName        :: !Name,
	realUnique     :: FastInt,
	varType        :: Kind,
	tcTyVarDetails :: TcTyVarDetails }

  | GlobalId { 			-- Used for imported Ids, dict selectors etc
 				-- See Note [GlobalId/LocalId] below
	varName    :: !Name,	-- Always an External or WiredIn Name
	realUnique :: FastInt,
   	varType    :: Type,
	idInfo_    :: IdInfo,
	gblDetails :: GlobalIdDetails }

  | LocalId { 			-- Used for locally-defined Ids 
				-- See Note [GlobalId/LocalId] below
	varName    :: !Name,
	realUnique :: FastInt,
   	varType    :: Type,
	idInfo_    :: IdInfo,
	lclDetails :: LocalIdDetails }

data LocalIdDetails 
  = NotExported	-- Not exported
  | Exported	-- Exported
  -- Exported Ids are kept alive; 
  -- NotExported things may be discarded as dead code.
\end{code}

Note [GlobalId/LocalId]
~~~~~~~~~~~~~~~~~~~~~~~
A GlobalId is
  * always a constant (top-level)
  * imported, or data constructor, or primop, or record selector
  * has a Unique that is globally unique across the whole
    GHC invocation (a single invocation may compile multiple modules)
  * never treated as a candidate by the free-variable finder;
	it's a constant!

A LocalId is 
  * bound within an expression (lambda, case, local let(rec))
  * or defined at top level in the module being compiled
  * always treated as a candidate by the free-variable finder

After CoreTidy, top-level LocalIds are turned into GlobalIds
 

\begin{code}
instance Outputable Var where
  ppr var = ppr (varName var) <+> ifPprDebug (brackets extra)
	where
	  extra = case var of
			GlobalId {} -> ptext (sLit "gid")
			LocalId  {} -> ptext (sLit "lid")
			TyVar    {} -> ptext (sLit "tv")
			TcTyVar {tcTyVarDetails = details} -> pprTcTyVarDetails details

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
varUnique var = mkUniqueGrimily (iBox (realUnique var))

setVarUnique :: Var -> Unique -> Var
setVarUnique var uniq 
  = var { realUnique = getKeyFastInt uniq, 
	  varName = setNameUnique (varName var) uniq }

setVarName :: Var -> Name -> Var
setVarName var new_name
  = var { realUnique = getKeyFastInt (getUnique new_name), 
   	  varName = new_name }
\end{code}


%************************************************************************
%*									*
\subsection{Type variables}
%*									*
%************************************************************************

\begin{code}
type TyVar = Var

tyVarName :: TyVar -> Name
tyVarName = varName

tyVarKind :: TyVar -> Kind
tyVarKind = varType

setTyVarUnique :: TyVar -> Unique -> TyVar
setTyVarUnique = setVarUnique

setTyVarName :: TyVar -> Name -> TyVar
setTyVarName   = setVarName

setTyVarKind :: TyVar -> Kind -> TyVar
setTyVarKind tv k = tv {varType = k}
\end{code}

\begin{code}
mkTyVar :: Name -> Kind -> TyVar
mkTyVar name kind = ASSERT( not (isCoercionKind kind ) )
		    TyVar { varName    = name
			  , realUnique = getKeyFastInt (nameUnique name)
			  , varType  = kind
                          , isCoercionVar    = False
			}

mkTcTyVar :: Name -> Kind -> TcTyVarDetails -> TyVar
mkTcTyVar name kind details
  = -- TOM: no longer valid assertion? 
    -- ASSERT( not (isCoercionKind kind) )
    TcTyVar {	varName    = name,
		realUnique = getKeyFastInt (nameUnique name),
		varType  = kind,
		tcTyVarDetails = details
	}
\end{code}

%************************************************************************
%*									*
\subsection{Coercion variables}
%*									*
%************************************************************************

\begin{code}
type CoVar = Var	-- A coercion variable is simply a type 
			-- variable of kind (ty1 :=: ty2)

coVarName :: CoVar -> Name
coVarName = varName

setCoVarUnique :: CoVar -> Unique -> CoVar
setCoVarUnique = setVarUnique

setCoVarName :: CoVar -> Name -> CoVar
setCoVarName   = setVarName

mkCoVar :: Name -> Kind -> CoVar
mkCoVar name kind = ASSERT( isCoercionKind kind )
		    TyVar { varName    	  = name
			  , realUnique 	  = getKeyFastInt (nameUnique name)
			  , varType    	  = kind	
				-- varType is always PredTy (EqPred t1 t2)
                          , isCoercionVar = True
			}

mkWildCoVar :: Kind -> TyVar
-- A type variable that is never referred to,
-- so its unique doesn't matter
mkWildCoVar kind 
  = ASSERT( isCoercionKind kind )
    TyVar { varName = mkSysTvName wild_uniq (fsLit "co_wild"),
            realUnique = _ILIT(1),
            varType = kind,
            isCoercionVar = True }
  where
    wild_uniq = mkBuiltinUnique 1
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
idName   :: Id -> Name
idUnique :: Id -> Unique
idType   :: Id -> Kind

idName    = varName
idUnique  = varUnique
idType    = varType

setIdUnique :: Id -> Unique -> Id
setIdUnique = setVarUnique

setIdName :: Id -> Name -> Id
setIdName = setVarName

setIdType :: Id -> Type -> Id
setIdType id ty = id {varType = ty}

setIdExported :: Id -> Id
-- Can be called on GlobalIds, such as data cons and class ops,
-- which are "born" as GlobalIds and automatically exported
setIdExported id@(LocalId {}) = id { lclDetails = Exported }
setIdExported other_id	      = ASSERT( isId other_id ) other_id

setIdNotExported :: Id -> Id
-- We can only do this to LocalIds
setIdNotExported id = ASSERT( isLocalId id ) id { lclDetails = NotExported }

globaliseId :: GlobalIdDetails -> Id -> Id
-- If it's a local, make it global
globaliseId details id = GlobalId { varName    = varName id,
				    realUnique = realUnique id,
			   	    varType    = varType id,
				    idInfo_    = idInfo id,
				    gblDetails = details }

idInfo :: Id -> IdInfo
idInfo (GlobalId {idInfo_ = info}) = info
idInfo (LocalId  {idInfo_ = info}) = info
idInfo other_var		   = pprPanic "idInfo" (ppr other_var)

lazySetIdInfo :: Id -> IdInfo -> Id
lazySetIdInfo id info = id {idInfo_ = info}

setIdInfo :: Id -> IdInfo -> Id
setIdInfo id info = seqIdInfo info `seq` id {idInfo_ = info}
	-- Try to avoid spack leaks by seq'ing

modifyIdInfo :: (IdInfo -> IdInfo) -> Id -> Id
modifyIdInfo fn id
  = seqIdInfo new_info `seq` id {idInfo_ = new_info}
  where
    new_info = fn (idInfo id)

-- maybeModifyIdInfo tries to avoid unnecesary thrashing
maybeModifyIdInfo :: Maybe IdInfo -> Id -> Id
maybeModifyIdInfo (Just new_info) id = id {idInfo_ = new_info}
maybeModifyIdInfo Nothing	  id = id
\end{code}

%************************************************************************
%*									*
\subsection{Predicates over variables
%*									*
%************************************************************************

\begin{code}
mkGlobalId :: GlobalIdDetails -> Name -> Type -> IdInfo -> Id
mkGlobalId details name ty info 
  = GlobalId {	varName    = name, 
		realUnique = getKeyFastInt (nameUnique name), 	-- Cache the unique
		varType     = ty,	
		gblDetails = details,
		idInfo_    = info }

mk_local_id :: Name -> Type -> LocalIdDetails -> IdInfo -> Id
mk_local_id name ty details info
  = LocalId {	varName    = name, 
		realUnique = getKeyFastInt (nameUnique name), 	-- Cache the unique
		varType     = ty,	
		lclDetails = details,
		idInfo_    = info }

mkLocalId :: Name -> Type -> IdInfo -> Id
mkLocalId name ty info = mk_local_id name ty NotExported info

mkExportedLocalId :: Name -> Type -> IdInfo -> Id
mkExportedLocalId name ty info = mk_local_id name ty Exported info
\end{code}

\begin{code}
isTyVar, isTcTyVar	    :: Var -> Bool
isId, isLocalVar, isLocalId :: Var -> Bool
isGlobalId, isExportedId    :: Var -> Bool
mustHaveLocalBinding	    :: Var -> Bool
isCoVar                     :: Var -> Bool

isTyVar (TyVar {})   = True
isTyVar (TcTyVar {}) = True
isTyVar _            = False

isTcTyVar (TcTyVar {}) = True
isTcTyVar _            = False

isId (LocalId {})  = True
isId (GlobalId {}) = True
isId _             = False

isLocalId (LocalId {}) = True
isLocalId _            = False

isCoVar (v@(TyVar {})) = isCoercionVar v
isCoVar _              = False

-- isLocalVar returns True for type variables as well as local Ids
-- These are the variables that we need to pay attention to when finding free
-- variables, or doing dependency analysis.
isLocalVar (GlobalId {}) = False 
isLocalVar _             = True

-- mustHaveLocalBinding returns True of Ids and TyVars
-- that must have a binding in this module.  The converse
-- is not quite right: there are some GlobalIds that must have
-- bindings, such as record selectors.  But that doesn't matter,
-- because it's only used for assertions
mustHaveLocalBinding var = isLocalVar var

isGlobalId (GlobalId {}) = True
isGlobalId _             = False

-- isExportedId means "don't throw this away"
isExportedId (GlobalId {}) = True
isExportedId (LocalId {lclDetails = details}) 
  = case details of
	Exported   -> True
	_          -> False
isExportedId _ = False
\end{code}

\begin{code}
globalIdDetails :: Var -> GlobalIdDetails
-- Works OK on local Ids too, returning notGlobalId
globalIdDetails (GlobalId {gblDetails = details}) = details
globalIdDetails _                                 = notGlobalId
\end{code}

