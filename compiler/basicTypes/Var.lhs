%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{@Vars@: Variables}

\begin{code}
-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'OccName.OccName': see "OccName#name_types"
--
-- * 'RdrName.RdrName': see "RdrName#name_types"
--
-- * 'Name.Name': see "Name#name_types"
--
-- * 'Id.Id': see "Id#name_types"
--
-- * 'Var.Var' is a synonym for the 'Id.Id' type but it may additionally potentially contain type variables, 
--   which have a 'TypeRep.Kind' rather than a 'TypeRep.Type' and only contain some extra details during typechecking.
--   These 'Var.Var' names may either be global or local, see "Var#globalvslocal"
--
-- #globalvslocal#
-- Global 'Id's and 'Var's are those that are imported or correspond to a data constructor, primitive operation, or record selectors.
-- Local 'Id's and 'Var's are those bound within an expression (e.g. by a lambda) or at the top level of the module being compiled.
module Var (
        -- * The main data type
	Var,

	-- ** Constructing 'Var's
	mkLocalIdVar, mkExportedLocalIdVar, mkGlobalIdVar, 

	-- ** Taking 'Var's apart
	varName, varUnique, varType, varIdInfo, globalIdVarDetails,

	-- ** Modifying 'Var's
	setVarName, setVarUnique, setVarType,
	setIdVarExported, setIdVarNotExported, 
	globaliseIdVar, lazySetVarIdInfo,

        -- ** Predicates
        isCoVar, isIdVar, isTyVar, isTcTyVar,
        isLocalVar, isLocalIdVar,
	isGlobalIdVar, isExportedIdVar,
	mustHaveLocalBinding,

	-- * Type variable data type
	TyVar,

	-- ** Constructing 'TyVar's
	mkTyVar, mkTcTyVar, mkWildCoVar,

	-- ** Taking 'TyVar's apart
	tyVarName, tyVarKind, tcTyVarDetails,

	-- ** Modifying 'TyVar's
	setTyVarName, setTyVarUnique, setTyVarKind,

        -- * Coercion variable data type
        CoVar,

        -- ** Constructing 'CoVar's
        mkCoVar,

        -- ** Taking 'CoVar's apart
        coVarName,

        -- ** Modifying 'CoVar's
        setCoVarUnique, setCoVarName,

	-- * 'Var' type synonyms
	Id, DictId
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TypeRep( Type, Kind )
import {-# SOURCE #-}	TcType( TcTyVarDetails, pprTcTyVarDetails )
import {-# SOURCE #-}	IdInfo( GlobalIdDetails, notGlobalId, 
                                IdInfo )
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
-- | Essentially a typed 'Name', that may also contain some additional information
-- about the 'Var' and it's use sites.
data Var
  = TyVar {
	varName    :: !Name,
	realUnique :: FastInt,		-- Key for fast comparison
					-- Identical to the Unique in the name,
					-- cached here for speed
	varType       :: Kind,          -- ^ The type or kind of the 'Var' in question
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
  = NotExported	-- ^ Not exported: may be discarded as dead code.
  | Exported	-- ^ Exported: kept alive
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

setVarType :: Id -> Type -> Id
setVarType id ty = id { varType = ty }

setIdVarExported :: Var -> Var
-- ^ Exports the given local 'Id'. Can also be called on global 'Id's, such as data constructors
-- and class operations, which are born as global 'Id's and automatically exported
setIdVarExported id@(LocalId {}) = id { lclDetails = Exported }
setIdVarExported other_id	      = ASSERT( isIdVar other_id ) other_id

setIdVarNotExported :: Id -> Id
-- ^ We can only do this to LocalIds
setIdVarNotExported id = ASSERT( isLocalIdVar id ) id { lclDetails = NotExported }

globaliseIdVar :: GlobalIdDetails -> Var -> Var
-- ^ If it's a local, make it global
globaliseIdVar details id = GlobalId { varName    = varName id,
				    realUnique = realUnique id,
			   	    varType    = varType id,
				    idInfo_    = varIdInfo id,
				    gblDetails = details }

-- | Extract 'Id' information from the 'Var' if it represents a global or local 'Id', otherwise panic
varIdInfo :: Var -> IdInfo
varIdInfo (GlobalId {idInfo_ = info}) = info
varIdInfo (LocalId  {idInfo_ = info}) = info
varIdInfo other_var		   = pprPanic "idInfo" (ppr other_var)

lazySetVarIdInfo :: Var -> IdInfo -> Var
lazySetVarIdInfo id info = id { idInfo_ = info }
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
  = -- NB: 'kind' may be a coercion kind; cf, 'TcMType.newMetaCoVar'
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
type CoVar = TyVar -- A coercion variable is simply a type 
			-- variable of kind @ty1 ~ ty2@. Hence its
			-- 'varType' is always @PredTy (EqPred t1 t2)@

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
                          , isCoercionVar = True
			}

mkWildCoVar :: Kind -> TyVar
-- ^ Create a type variable that is never referred to, so its unique doesn't 
-- matter
mkWildCoVar = mkCoVar (mkSysTvName (mkBuiltinUnique 1) (fsLit "co_wild"))
\end{code}

%************************************************************************
%*									*
\subsection{Ids}
%*									*
%************************************************************************

\begin{code}

-- These synonyms are here and not in Id because otherwise we need a very
-- large number of SOURCE imports of Id.hs :-(
type Id = Var
type DictId = Var

\end{code}

%************************************************************************
%*									*
\subsection{Predicates over variables}
%*									*
%************************************************************************

\begin{code}
-- | For an explanation of global vs. local 'Var's, see "Var#globalvslocal"
mkGlobalIdVar :: GlobalIdDetails -> Name -> Type -> IdInfo -> Var
mkGlobalIdVar details name ty info 
  = GlobalId {	varName    = name, 
		realUnique = getKeyFastInt (nameUnique name), 	-- Cache the unique
		varType     = ty,	
		gblDetails = details,
		idInfo_    = info }

mkLocalIdVar' :: Name -> Type -> LocalIdDetails -> IdInfo -> Var
mkLocalIdVar' name ty details info
  = LocalId {	varName    = name, 
		realUnique = getKeyFastInt (nameUnique name), 	-- Cache the unique
		varType     = ty,	
		lclDetails = details,
		idInfo_    = info }

-- | For an explanation of global vs. local 'Var's, see "Var#globalvslocal"
mkLocalIdVar :: Name -> Type -> IdInfo -> Var
mkLocalIdVar name ty info = mkLocalIdVar' name ty NotExported info

-- | Exported 'Var's will not be removed as dead code
mkExportedLocalIdVar :: Name -> Type -> IdInfo -> Var
mkExportedLocalIdVar name ty info = mkLocalIdVar' name ty Exported info
\end{code}

\begin{code}
isTyVar :: Var -> Bool
isTyVar (TyVar {})   = True
isTyVar (TcTyVar {}) = True
isTyVar _            = False

isTcTyVar :: Var -> Bool
isTcTyVar (TcTyVar {}) = True
isTcTyVar _            = False

isIdVar :: Var -> Bool
isIdVar (LocalId {})  = True
isIdVar (GlobalId {}) = True
isIdVar _             = False

isLocalIdVar :: Var -> Bool
isLocalIdVar (LocalId {}) = True
isLocalIdVar _            = False

isCoVar :: Var -> Bool
isCoVar (v@(TyVar {}))             = isCoercionVar v
isCoVar (TcTyVar {varType = kind}) = isCoercionKind kind  -- used during solving
isCoVar _                          = False

-- | 'isLocalVar' returns @True@ for type variables as well as local 'Id's
-- These are the variables that we need to pay attention to when finding free
-- variables, or doing dependency analysis.
isLocalVar :: Var -> Bool
isLocalVar (GlobalId {}) = False 
isLocalVar _             = True

-- | 'mustHaveLocalBinding' returns @True@ of 'Id's and 'TyVar's
-- that must have a binding in this module.  The converse
-- is not quite right: there are some global 'Id's that must have
-- bindings, such as record selectors.  But that doesn't matter,
-- because it's only used for assertions
mustHaveLocalBinding	    :: Var -> Bool
mustHaveLocalBinding var = isLocalVar var

isGlobalIdVar :: Var -> Bool
isGlobalIdVar (GlobalId {}) = True
isGlobalIdVar _             = False

-- | 'isExportedIdVar' means \"don't throw this away\"
isExportedIdVar :: Var -> Bool
isExportedIdVar (GlobalId {}) = True
isExportedIdVar (LocalId {lclDetails = details}) 
  = case details of
	Exported   -> True
	_          -> False
isExportedIdVar _ = False
\end{code}

\begin{code}
globalIdVarDetails :: Var -> GlobalIdDetails
-- ^ Find the global 'Id' information if the 'Var' is a global 'Id', otherwise returns 'notGlobalId'
globalIdVarDetails (GlobalId {gblDetails = details}) = details
globalIdVarDetails _                                 = notGlobalId
\end{code}
