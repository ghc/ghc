
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@Vars@: Variables}

\begin{code}
module Var (
	Var, IdOrTyVar,		-- Abstract
	VarDetails(..), 	-- Concrete
	varName, varUnique, varDetails, varInfo, varType,
	setVarName, setVarUnique, setVarType,  setVarOcc,


	-- TyVars
	TyVar,
	tyVarName, tyVarKind,
	setTyVarName, setTyVarUnique,
	mkTyVar, mkSysTyVar, isTyVar,
	newMutTyVar, readMutTyVar, writeMutTyVar, isMutTyVar, makeTyVarImmutable,

	-- Ids
	Id, DictId,
	idDetails, idName, idType, idUnique, idInfo, modifyIdInfo,
	setIdName, setIdUnique, setIdInfo,
	mkId, isId, externallyVisibleId
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	Type( Type, Kind )
import {-# SOURCE #-}	IdInfo( IdInfo )
import {-# SOURCE #-}	Const( Con )

import FieldLabel	( FieldLabel )
import Unique		( Unique, Uniquable(..), mkUniqueGrimily, getKey )
import Name		( Name, OccName, NamedThing(..),
			  setNameUnique, setNameOcc, nameUnique, 
			  mkSysLocalName, isExternallyVisibleName
			)
import BasicTypes	( Unused )
import Outputable

import IOExts		( IORef, newIORef, readIORef, writeIORef )
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
type IdOrTyVar = Var

data Var
  = Var {
	varName    :: Name,
	realUnique :: Int#,		-- Key for fast comparison
					-- Identical to the Unique in the name,
					-- cached here for speed
	varType    :: Type,
	varDetails :: VarDetails,
	varInfo    :: IdInfo		-- Only used for Ids at the moment
    }

data VarDetails
  = VanillaId 				-- Most Ids are like this
  | ConstantId Con			-- The Id for a constant (data constructor or primop)
  | RecordSelId FieldLabel		-- The Id for a record selector
  | TyVar
  | MutTyVar (IORef (Maybe Type))	-- Used during unification

-- For a long time I tried to keep mutable Vars statically type-distinct
-- from immutable Vars, but I've finally given up.   It's just too painful.
-- After type checking there are no MutTyVars left, but there's no static check
-- of that fact.
\end{code}

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
setVarUnique var uniq = var {realUnique = getKey uniq, 
			     varName = setNameUnique (varName var) uniq}

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
#ifdef DEBUG
			, varInfo = pprPanic "mkTyVar" (ppr name)
#endif
			}

mkSysTyVar :: Unique -> Kind -> TyVar
mkSysTyVar uniq kind = Var { varName    = name
			   , realUnique = getKey uniq
			   , varType    = kind
			   , varDetails = TyVar
#ifdef DEBUG
			   , varInfo = pprPanic "mkSysTyVar" (ppr name)
#endif
			   }
		     where
		       name = mkSysLocalName uniq SLIT("t")

newMutTyVar :: Name -> Kind -> IO TyVar
newMutTyVar name kind = 
  do loc <- newIORef Nothing
     return (Var { varName = name, 
		   realUnique = getKey (nameUnique name),
		   varType = kind, 
		   varDetails = MutTyVar loc })

readMutTyVar :: TyVar -> IO (Maybe Type)
readMutTyVar (Var {varDetails = MutTyVar loc}) = readIORef loc

writeMutTyVar :: TyVar -> Maybe Type -> IO ()
writeMutTyVar (Var {varDetails = MutTyVar loc}) val = writeIORef loc val

makeTyVarImmutable :: TyVar -> TyVar
makeTyVarImmutable tyvar = tyvar { varDetails = TyVar}
\end{code}

\begin{code}
isTyVar :: Var -> Bool
isTyVar (Var {varDetails = details}) = case details of
					TyVar      -> True
					MutTyVar _ -> True
					other	   -> False

isMutTyVar :: Var -> Bool
isMutTyVar (Var {varDetails = MutTyVar _}) = True
isMutTyVar other			       = False
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
idDetails = varDetails

setIdUnique :: Id -> Unique -> Id
setIdUnique = setVarUnique

setIdName :: Id -> Name -> Id
setIdName = setVarName

setIdInfo :: Id -> IdInfo -> Id
setIdInfo var info = var {varInfo = info}

modifyIdInfo :: Id -> (IdInfo -> IdInfo) -> Id
modifyIdInfo var@(Var {varInfo = info}) fn = var {varInfo = fn info}
\end{code}

\begin{code}
mkId :: Name -> Type -> VarDetails -> IdInfo -> Id
mkId name ty details info
  = Var {varName = name, realUnique = getKey (nameUnique name), varType = ty, 
	 varDetails = details, varInfo = info}
\end{code}

\begin{code}
isId :: Var -> Bool
isId (Var {varDetails = details}) = case details of
					VanillaId     -> True
					ConstantId _  -> True
					RecordSelId _ -> True
					other	      -> False
\end{code}

@externallyVisibleId@: is it true that another module might be
able to ``see'' this Id in a code generation sense. That
is, another .o file might refer to this Id.

In tidyCorePgm (SimplCore.lhs) we carefully set each top level thing's
local-ness precisely so that the test here would be easy

This defn appears here (rather than, say, in Id.lhs) because
CostCentre.lhs uses it (CostCentre feeds PprType feeds Id.lhs)

\end{code}
\begin{code}
externallyVisibleId :: Id -> Bool
externallyVisibleId var = isExternallyVisibleName (varName var)
\end{code}
