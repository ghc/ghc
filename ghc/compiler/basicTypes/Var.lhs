
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@Vars@: Variables}

\begin{code}
module Var (
	Var, IdOrTyVar,		-- Abstract
	VarDetails(..), 	-- Concrete
	varName, varUnique, varDetails, varInfo, varType,
	setVarName, setVarUnique, setVarType,


	-- TyVars
	TyVar, GenTyVar,
	tyVarName, tyVarKind,
	tyVarFlexi, setTyVarFlexi, removeTyVarFlexi, setTyVarName, setTyVarUnique,
	mkFlexiTyVar, mkTyVar, mkSysTyVar, isTyVar, isFlexiTyVar,

	-- Ids
	Id, DictId, GenId,
	idName, idType, idUnique, idInfo, modifyIdInfo,
	setIdName, setIdUnique, setIdInfo,
	mkId, isId, externallyVisibleId
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	Type( GenType, Kind )
import {-# SOURCE #-}	IdInfo( IdInfo )
import {-# SOURCE #-}	Const( Con )

import FieldLabel	( FieldLabel )
import Unique		( Unique, Uniquable(..), mkUniqueGrimily, getKey )
import Name		( Name, NamedThing(..),
			  changeUnique, nameUnique, 
			  mkSysLocalName, isExternallyVisibleName
			)
import BasicTypes	( Unused )
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
type IdOrTyVar = Var Unused Unused 

data Var flex_self flex_ty 
  = Var {
	varName    :: Name,
	realUnique :: Int#,		-- Key for fast comparison
					-- Identical to the Unique in the name,
					-- cached here for speed
	varType    :: GenType flex_ty,
	varDetails :: VarDetails flex_self,
	varInfo    :: IdInfo		-- Only used for Ids at the moment
    }

varUnique Var{realUnique = uniq} = mkUniqueGrimily uniq

data VarDetails flex_self
  = TyVar
  | FlexiTyVar flex_self	-- Used during unification
  | VanillaId 			-- Most Ids are like this
  | ConstantId Con		-- The Id for a constant (data constructor or primop)
  | RecordSelId FieldLabel	-- The Id for a record selector
\end{code}

\begin{code}
instance Outputable (Var fs ft) where
  ppr var = ppr (varName var)

instance Show (Var fs ft) where
  showsPrec p var = showsPrecSDoc p (ppr var)

instance NamedThing (Var fs ft) where
  getName = varName

instance Uniquable (Var fs ft) where
  getUnique = varUnique

instance Eq (Var fs ft) where
    a == b = realUnique a ==# realUnique b

instance Ord (Var fs ft) where
    a <= b = realUnique a <=# realUnique b
    a <	 b = realUnique a <#  realUnique b
    a >= b = realUnique a >=# realUnique b
    a >	 b = realUnique a >#  realUnique b
    a `compare` b = varUnique a `compare` varUnique b
\end{code}


\begin{code}
setVarUnique :: Var fs ft -> Unique -> Var fs ft
setVarUnique var uniq = var {realUnique = getKey uniq, 
			     varName = changeUnique (varName var) uniq}

setVarName :: Var fs ft -> Name -> Var fs ft
setVarName var new_name
  = var { realUnique = getKey (getUnique new_name), varName = new_name }

setVarType :: Var flex_self flex_ty1 -> GenType flex_ty2 -> Var flex_self flex_ty2
setVarType var ty = var {varType = ty}
\end{code}


%************************************************************************
%*									*
\subsection{Type variables}
%*									*
%************************************************************************

\begin{code}
type GenTyVar flex_self = Var flex_self Unused		-- Perhaps a mutable tyvar, but 
							-- with a fixed Kind

type TyVar		= GenTyVar Unused		-- NOt even mutable
\end{code}

\begin{code}
tyVarName = varName
tyVarKind = varType

setTyVarUnique = setVarUnique
setTyVarName   = setVarName

tyVarFlexi :: GenTyVar flexi -> flexi
tyVarFlexi (Var {varDetails = FlexiTyVar flex}) = flex
tyVarFlexi other_var        = pprPanic "tyVarFlexi" (ppr other_var)

setTyVarFlexi :: GenTyVar flexi1 -> flexi2 -> GenTyVar flexi2
setTyVarFlexi var flex = var {varDetails = FlexiTyVar flex}

removeTyVarFlexi :: GenTyVar flexi1 -> GenTyVar flexi2
removeTyVarFlexi var = var {varDetails = TyVar}
\end{code}

\begin{code}
mkTyVar :: Name -> Kind -> GenTyVar flexi
mkTyVar name kind = Var { varName = name, realUnique = getKey (nameUnique name),
			  varType = kind, varDetails = TyVar }

mkSysTyVar :: Unique -> Kind -> GenTyVar flexi
mkSysTyVar uniq kind = Var { varName = name, realUnique = getKey uniq,
			     varType = kind, varDetails = TyVar }
		     where
		       name = mkSysLocalName uniq

mkFlexiTyVar :: Name -> Kind -> flexi -> GenTyVar flexi
mkFlexiTyVar name kind flex = Var { varName = name, 
				    realUnique = getKey (nameUnique name),
			            varType = kind, 
				    varDetails = FlexiTyVar flex }
\end{code}

\begin{code}
isTyVar :: Var fs ft -> Bool
isTyVar (Var {varDetails = details}) = case details of
					TyVar        -> True
					FlexiTyVar _ -> True
					other	     -> False

isFlexiTyVar :: Var fs ft -> Bool
isFlexiTyVar (Var {varDetails = FlexiTyVar _}) = True
isFlexiTyVar other			       = False
\end{code}


%************************************************************************
%*									*
\subsection{Id Construction}
%*									*
%************************************************************************

	Most Id-related functions are in Id.lhs and MkId.lhs

\begin{code}
type GenId flex_ty = Var Unused flex_ty
type Id 	   = GenId Unused
type DictId	   = Id
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

setIdInfo :: GenId flexi -> IdInfo -> GenId flexi
setIdInfo var info = var {varInfo = info}

modifyIdInfo :: GenId flexi -> (IdInfo -> IdInfo) -> GenId flexi
modifyIdInfo var@(Var {varInfo = info}) fn = var {varInfo = fn info}
\end{code}

\begin{code}
mkId :: Name -> GenType flex_ty  -> VarDetails Unused -> IdInfo -> GenId flex_ty
mkId name ty details info
  = Var {varName = name, realUnique = getKey (nameUnique name), varType = ty, 
	 varDetails = details, varInfo = info}
\end{code}

\begin{code}
isId :: Var fs ft -> Bool
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
