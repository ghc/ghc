
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
module Id (
	-- TYPES
	GenId,		 	-- Abstract
	Id,
	IdDetails(..),		-- Exposed only to MkId
	StrictnessMark(..),
	ConTag, fIRST_TAG,
	DataCon, DictFun, DictVar,

	-- Construction and modification
	mkId, mkIdWithNewUniq, mkIdWithNewName, mkIdWithNewType,
	mkTemplateLocals, 
	setIdVisibility, mkVanillaId,

	-- DESTRUCTION (excluding pragmatic info)
	idPrimRep,
	idType,
	idUnique,
	idName,

	-- Extracting pieces of particular sorts of Ids
	dataConRepType,
	dataConArgTys,
	dataConNumFields,
	dataConFieldLabels,
	dataConRawArgTys,
	dataConSig,
	dataConStrictMarks,
	dataConTag,
	dataConTyCon,

	recordSelectorFieldLabel,

	-- PREDICATES
	omitIfaceSigForId,
	cmpId,
	externallyVisibleId,
	idHasNoFreeTyVars,
	idWantsToBeINLINEd, getInlinePragma, 
	idMustBeINLINEd, idMustNotBeINLINEd,
	isBottomingId,
	
	isDataCon, isAlgCon, isNewCon, isTupleCon,
	isNullaryDataCon,

	isRecordSelector, isSpecPragmaId,
	isPrimitiveId_maybe,

	-- PRINTING and RENUMBERING
	pprId,
	showId,

	-- UNFOLDING, ARITY, UPDATE, AND STRICTNESS STUFF (etc)
	idInfo,
	addIdUnfolding,
	addIdArity,
	addIdDemandInfo,
	addIdStrictness,
	addIdUpdateInfo,
	getIdArity,
	getIdDemandInfo,
	getIdStrictness,
	getIdUnfolding,
	getIdUpdateInfo,
	replaceIdInfo,
	addInlinePragma, nukeNoInlinePragma, addNoInlinePragma,
	getIdSpecialisation,
	setIdSpecialisation,

	-- IdEnvs AND IdSets
	IdEnv, GenIdSet, IdSet,
	addOneToIdEnv,
	addOneToIdSet,
	combineIdEnvs,
	delManyFromIdEnv,
	delOneFromIdEnv,
	elementOfIdSet,
	emptyIdSet,
	growIdEnv,
	growIdEnvList,
	idSetToList,
	intersectIdSets,
	isEmptyIdSet,
	isNullIdEnv,
	lookupIdEnv, lookupIdSubst,
	lookupNoFailIdEnv,
	mapIdEnv,
	minusIdSet,
	mkIdEnv, elemIdEnv,
	mkIdSet,
	modifyIdEnv,
	modifyIdEnv_Directly,
	nullIdEnv,
	rngIdEnv,
	unionIdSets,
	unionManyIdSets,
	unitIdEnv,
	unitIdSet
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} CoreUnfold ( Unfolding )

import CmdLineOpts      ( opt_PprStyle_All )
import Bag
import IdInfo
import Name	 	( nameUnique, isLocalName, mkSysLocalName,
			  isWiredInName, setNameVisibility,
			  ExportFlag(..), Provenance,
			  OccName(..), Name, Module,
			  NamedThing(..)
			) 
import PrimOp		( PrimOp )
import PrelMods		( pREL_TUP, pREL_BASE )
import FieldLabel	( fieldLabelName, FieldLabel(..) )
import SrcLoc		( mkBuiltinSrcLoc )
import TysWiredIn	( tupleTyCon )
import TyCon		( TyCon, isDataTyCon, isNewTyCon )
import Type		( mkSigmaTy, mkTyVarTys, mkFunTys,
			  mkTyConApp, instantiateTy, mkForAllTys,
			  tyVarsOfType, instantiateTy, typePrimRep,
			  instantiateTauTy,
			  ThetaType, TauType, Type, GenType
			)
import TyVar		( TyVar, alphaTyVars, isEmptyTyVarSet, 
			  TyVarEnv, zipTyVarEnv, mkTyVarEnv
			)
import UniqFM
import UniqSet		-- practically all of it
import Unique		( Unique, Uniquable(..), getBuiltinUniques )
import Outputable
import SrcLoc		( SrcLoc )
import Util		( nOfThem, assoc )
import GlaExts		( Int# )
\end{code}

Here are the @Id@ and @IdDetails@ datatypes; also see the notes that
follow.

Every @Id@ has a @Unique@, to uniquify it and for fast comparison, a
@Type@, and an @IdInfo@ (non-essential info about it, e.g.,
strictness).  The essential info about different kinds of @Ids@ is
in its @IdDetails@.

ToDo: possibly cache other stuff in the single-constructor @Id@ type.

\begin{code}
data GenId ty = Id {
	idUnique  :: Unique,		-- Key for fast comparison
	idName    :: Name,
	idType    :: ty,		-- Id's type; used all the time;
	idDetails :: IdDetails,		-- Stuff about individual kinds of Ids.
	idInfo    :: IdInfo		-- Properties of this Id deduced by compiler
	}
				   
type Id	           = GenId Type

data StrictnessMark = MarkedStrict | NotMarkedStrict

data IdDetails

  ---------------- Local values

  = VanillaId	Bool		-- Ordinary Id
				-- True <=> no free type vars

  | PrimitiveId PrimOp		-- The Id for a primitive operation
                                

  ---------------- Data constructors

  | AlgConId			-- Used for both data and newtype constructors.
				-- You can tell the difference by looking at the TyCon
		ConTag
		[StrictnessMark] -- Strict args; length = arity
		[FieldLabel]	-- Field labels for this constructor; 
				--length = 0 (not a record) or arity

		[TyVar] ThetaType 	-- Type vars and context for the data type decl
		[TyVar] ThetaType 	-- Ditto for the context of the constructor, 
					-- the existentially quantified stuff
		[Type] TyCon		-- Args and result tycon
				-- the type is:
				-- forall tyvars1 ++ tyvars2. theta1 ++ theta2 =>
				--    unitype_1 -> ... -> unitype_n -> tycon tyvars

  | TupleConId	Int		-- Its arity

  | RecordSelId FieldLabel

  | SpecPragmaId		-- This guy exists only to make Ids that are
				-- on the *LHS* of bindings created by SPECIALISE
				-- pragmas; eg:		s = f Int d
				-- The SpecPragmaId is never itself mentioned; it
				-- exists solely so that the specialiser will find
				-- the call to f, and make specialised version of it.
				-- The SpecPragmaId binding is discarded by the specialiser
				-- when it gathers up overloaded calls.
				-- Meanwhile, it is not discarded as dead code.



type ConTag	= Int
type DictVar	= Id
type DictFun	= Id
type DataCon	= Id
\end{code}


%************************************************************************
%*									*
\subsection{Construction}
%*									*
%************************************************************************

\begin{code}
mkId :: Name -> ty -> IdDetails -> IdInfo -> GenId ty
mkId name ty details info
  = Id {idName = name, idUnique = nameUnique name, idType = ty, 
	idDetails = details, idInfo = info}

mkVanillaId :: Name -> (GenType flexi) -> IdInfo -> GenId (GenType flexi)
mkVanillaId name ty info
  = Id {idName = name, idUnique = nameUnique name, idType = ty, 
	idDetails = VanillaId (isEmptyTyVarSet (tyVarsOfType ty)),
	idInfo = info}

mkIdWithNewUniq :: Id -> Unique -> Id
mkIdWithNewUniq id uniq = id {idUnique = uniq, idName = changeUnique (idName id) uniq}

mkIdWithNewName :: Id -> Name -> Id
mkIdWithNewName id new_name
  = id {idUnique = uniqueOf new_name, idName = new_name}

mkIdWithNewType :: GenId ty1 -> ty2 -> GenId ty2
mkIdWithNewType id ty = id {idType = ty}
\end{code}


Make some local @Ids@ for a template @CoreExpr@.  These have bogus
@Uniques@, but that's OK because the templates are supposed to be
instantiated before use.

\begin{code}
mkTemplateLocals :: [Type] -> [Id]
mkTemplateLocals tys
  = zipWith mk (getBuiltinUniques (length tys)) tys
  where
    mk uniq ty = mkVanillaId (mkSysLocalName uniq SLIT("tpl") mkBuiltinSrcLoc)
			     ty noIdInfo
\end{code}


\begin{code}
-- See notes with setNameVisibility (Name.lhs)
setIdVisibility :: Maybe Module -> Unique -> Id -> Id
setIdVisibility maybe_mod u id 
  = id {idName = setNameVisibility maybe_mod u (idName id)}

replaceIdInfo :: GenId ty -> IdInfo -> GenId ty
replaceIdInfo id info = id {idInfo = info}
\end{code}

%************************************************************************
%*									*
\subsection[Id-general-funs]{General @Id@-related functions}
%*									*
%************************************************************************

\begin{code}
fIRST_TAG :: ConTag
fIRST_TAG =  1	-- Tags allocated from here for real constructors

-- isDataCon returns False for @newtype@ constructors
isDataCon (Id {idDetails = AlgConId _ _ _ _ _ _ _ _ tc}) = isDataTyCon tc
isDataCon (Id {idDetails = TupleConId _})		 = True
isDataCon other					         = False

isNewCon (Id {idDetails = AlgConId _ _ _ _ _ _ _ _ tc}) = isNewTyCon tc
isNewCon other					        = False

-- isAlgCon returns True for @data@ or @newtype@ constructors
isAlgCon (Id {idDetails = AlgConId _ _ _ _ _ _ _ _ _}) = True
isAlgCon (Id {idDetails = TupleConId _})	       = True
isAlgCon other					       = False

isTupleCon (Id {idDetails = TupleConId _}) = True
isTupleCon other			   = False
\end{code}

\begin{code}
idHasNoFreeTyVars :: Id -> Bool

idHasNoFreeTyVars (Id {idDetails = details})
  = chk details
  where
    chk (AlgConId _ _ _ _ _ _ _ _ _) = True
    chk (TupleConId _)    	   = True
    chk (RecordSelId _)   	   = True
    chk (VanillaId    no_free_tvs) = no_free_tvs
    chk (PrimitiveId _)		   = True
    chk SpecPragmaId		   = False	-- Play safe

-- omitIfaceSigForId tells whether an Id's info is implied by other declarations,
-- so we don't need to put its signature in an interface file, even if it's mentioned
-- in some other interface unfolding.

omitIfaceSigForId
	:: Id
	-> Bool

omitIfaceSigForId (Id {idName = name, idDetails = details})
  | isWiredInName name
  = True

  | otherwise
  = case details of
        (PrimitiveId _)	  -> True		-- Ditto, for primitives

	-- This group is Ids that are implied by their type or class decl;
	-- remember that all type and class decls appear in the interface file.
	-- The dfun id must *not* be omitted, because it carries version info for
	-- the instance decl
        (AlgConId _ _ _ _ _ _ _ _ _) -> True
        (TupleConId _)    	     -> True
        (RecordSelId _)   	     -> True

	other			     -> False	-- Don't omit!
		-- NB DefaultMethodIds are not omitted
\end{code}

\begin{code}
isBottomingId id = bottomIsGuaranteed (strictnessInfo (idInfo id))

isPrimitiveId_maybe (Id {idDetails = PrimitiveId primop}) = Just primop
isPrimitiveId_maybe other				  = Nothing

isSpecPragmaId (Id {idDetails = SpecPragmaId}) = True
isSpecPragmaId _			       = False
\end{code}

@externallyVisibleId@: is it true that another module might be
able to ``see'' this Id in a code generation sense. That
is, another .o file might refer to this Id.

In tidyCorePgm (SimplCore.lhs) we carefully set each top level thing's
local-ness precisely so that the test here would be easy

\begin{code}
externallyVisibleId :: Id -> Bool
externallyVisibleId id = not (isLocalName (idName id))
		     -- not local => global => externally visible
\end{code}


\begin{code}
idPrimRep id = typePrimRep (idType id)
\end{code}


%************************************************************************
%*									*
\subsection[Id-arities]{Arity-related functions}
%*									*
%************************************************************************

For locally-defined Ids, the code generator maintains its own notion
of their arities; so it should not be asking...	 (but other things
besides the code-generator need arity info!)

\begin{code}
getIdArity :: Id -> ArityInfo
getIdArity id = arityInfo (idInfo id)

addIdArity :: Id -> ArityInfo -> Id
addIdArity id@(Id {idInfo = info}) arity
  = id {idInfo = arity `setArityInfo` info}
\end{code}

%************************************************************************
%*									*
\subsection[constructor-funs]{@DataCon@-related functions (incl.~tuples)}
%*									*
%************************************************************************


dataConNumFields gives the number of actual fields in the
{\em representation} of the data constructor.  This may be more than appear
in the source code; the extra ones are the existentially quantified
dictionaries

\begin{code}
dataConNumFields id
  = ASSERT( if (isDataCon id) then True else
	    pprTrace "dataConNumFields" (ppr id) False )
    case (dataConSig id) of { (_, _, _, con_theta, arg_tys, _) ->
    length con_theta + length arg_tys }

isNullaryDataCon con = dataConNumFields con == 0 -- function of convenience

\end{code}


\begin{code}
dataConTag :: DataCon -> ConTag	-- will panic if not a DataCon
dataConTag (Id {idDetails = AlgConId tag _ _ _ _ _ _ _ _}) = tag
dataConTag (Id {idDetails = TupleConId _})	           = fIRST_TAG

dataConTyCon :: DataCon -> TyCon	-- will panic if not a DataCon
dataConTyCon (Id {idDetails = AlgConId _ _ _ _ _ _ _ _ tycon}) = tycon
dataConTyCon (Id {idDetails = TupleConId a})	 	       = tupleTyCon a

dataConSig :: DataCon -> ([TyVar], ThetaType, [TyVar], ThetaType, [TauType], TyCon)
					-- will panic if not a DataCon

dataConSig (Id {idDetails = AlgConId _ _ _ tyvars theta con_tyvars con_theta arg_tys tycon})
  = (tyvars, theta, con_tyvars, con_theta, arg_tys, tycon)

dataConSig (Id {idDetails = TupleConId arity})
  = (tyvars, [], [], [], tyvar_tys, tupleTyCon arity)
  where
    tyvars	= take arity alphaTyVars
    tyvar_tys	= mkTyVarTys tyvars


-- dataConRepType returns the type of the representation of a contructor
-- This may differ from the type of the contructor Id itself for two reasons:
--	a) the constructor Id may be overloaded, but the dictionary isn't stored
--	   e.g.    data Eq a => T a = MkT a a
--
--	b) the constructor may store an unboxed version of a strict field.
--
-- Here's an example illustrating both:
--	data Ord a => T a = MkT Int! a
-- Here
--	T :: Ord a => Int -> a -> T a
-- but the rep type is
--	Trep :: Int# -> a -> T a
-- Actually, the unboxed part isn't implemented yet!

dataConRepType :: Id -> Type
dataConRepType (Id {idDetails = AlgConId _ _ _ tyvars theta con_tyvars con_theta arg_tys tycon})
  = mkForAllTys (tyvars++con_tyvars) 
		(mkFunTys arg_tys (mkTyConApp tycon (mkTyVarTys tyvars)))
dataConRepType other_id
  = ASSERT( isDataCon other_id )
    idType other_id

dataConFieldLabels :: DataCon -> [FieldLabel]
dataConFieldLabels (Id {idDetails = AlgConId _ _ fields _ _ _ _ _ _}) = fields
dataConFieldLabels (Id {idDetails = TupleConId _})		      = []
#ifdef DEBUG
dataConFieldLabels x@(Id {idDetails = idt}) = 
  panic ("dataConFieldLabel: " ++
    (case idt of
      VanillaId _   -> "l"
      PrimitiveId _ -> "p"
      RecordSelId _ -> "r"))
#endif

dataConStrictMarks :: DataCon -> [StrictnessMark]
dataConStrictMarks (Id {idDetails = AlgConId _ stricts _ _ _ _ _ _ _}) = stricts
dataConStrictMarks (Id {idDetails = TupleConId arity})		       = nOfThem arity NotMarkedStrict

dataConRawArgTys :: DataCon -> [TauType] -- a function of convenience
dataConRawArgTys con = case (dataConSig con) of { (_,_, _, _, arg_tys,_) -> arg_tys }

dataConArgTys :: DataCon 
	      -> [Type] 	-- Instantiated at these types
	      -> [Type]		-- Needs arguments of these types
dataConArgTys con_id inst_tys
 = map (instantiateTy tenv) arg_tys
 where
    (tyvars, _, _, _, arg_tys, _) = dataConSig con_id
    tenv 		          = zipTyVarEnv tyvars inst_tys
\end{code}

\begin{code}
recordSelectorFieldLabel :: Id -> FieldLabel
recordSelectorFieldLabel (Id {idDetails = RecordSelId lbl}) = lbl

isRecordSelector (Id {idDetails = RecordSelId lbl}) = True
isRecordSelector other				    = False
\end{code}


%************************************************************************
%*									*
\subsection[unfolding-Ids]{Functions related to @Ids@' unfoldings}
%*									*
%************************************************************************

\begin{code}
getIdUnfolding :: Id -> Unfolding

getIdUnfolding id = unfoldingInfo (idInfo id)

addIdUnfolding :: Id -> Unfolding -> Id
addIdUnfolding id@(Id {idInfo = info}) unfolding
  = id {idInfo = unfolding `setUnfoldingInfo` info}
\end{code}

The inline pragma tells us to be very keen to inline this Id, but it's still
OK not to if optimisation is switched off.

\begin{code}
getInlinePragma :: Id -> InlinePragInfo
getInlinePragma id = inlinePragInfo (idInfo id)

idWantsToBeINLINEd :: Id -> Bool

idWantsToBeINLINEd id = case getInlinePragma id of
			  IWantToBeINLINEd -> True
			  IMustBeINLINEd   -> True
			  other		   -> False

idMustNotBeINLINEd id = case getInlinePragma id of
			  IMustNotBeINLINEd -> True
			  other		    -> False

idMustBeINLINEd id =  case getInlinePragma id of
			IMustBeINLINEd -> True
			other	       -> False

addInlinePragma :: Id -> Id
addInlinePragma id@(Id {idInfo = info})
  = id {idInfo = setInlinePragInfo IWantToBeINLINEd info}

nukeNoInlinePragma :: Id -> Id
nukeNoInlinePragma id@(Id {idInfo = info})
  = case inlinePragInfo info of
	IMustNotBeINLINEd -> id {idInfo = setInlinePragInfo NoPragmaInfo info}
	other		  -> id

addNoInlinePragma :: Id -> Id
addNoInlinePragma id@(Id {idInfo = info})
  = id {idInfo = IMustNotBeINLINEd `setInlinePragInfo` info}

mustInlineInfo   = IMustBeINLINEd   `setInlinePragInfo` noIdInfo
wantToInlineInfo = IWantToBeINLINEd `setInlinePragInfo` noIdInfo
\end{code}



%************************************************************************
%*									*
\subsection[IdInfo-funs]{Functions related to @Ids@' @IdInfos@}
%*									*
%************************************************************************

\begin{code}
getIdDemandInfo :: Id -> DemandInfo
getIdDemandInfo id = demandInfo (idInfo id)

addIdDemandInfo :: Id -> DemandInfo -> Id
addIdDemandInfo id@(Id {idInfo = info}) demand_info
  = id {idInfo = demand_info `setDemandInfo` info}
\end{code}p

\begin{code}
getIdUpdateInfo :: Id -> UpdateInfo
getIdUpdateInfo id = updateInfo (idInfo id)

addIdUpdateInfo :: Id -> UpdateInfo -> Id
addIdUpdateInfo id@(Id {idInfo = info}) upd_info
  = id {idInfo = upd_info `setUpdateInfo` info}
\end{code}

\begin{code}
getIdSpecialisation :: Id -> IdSpecEnv
getIdSpecialisation id = specInfo (idInfo id)

setIdSpecialisation :: Id -> IdSpecEnv -> Id
setIdSpecialisation id@(Id {idInfo = info}) spec_info
  = id {idInfo = spec_info `setSpecInfo` info}
\end{code}

\begin{code}
getIdStrictness :: Id -> StrictnessInfo
getIdStrictness id = strictnessInfo (idInfo id)

addIdStrictness :: Id -> StrictnessInfo -> Id
addIdStrictness id@(Id {idInfo = info}) strict_info
  = id {idInfo = strict_info `setStrictnessInfo` info}
\end{code}

%************************************************************************
%*									*
\subsection[Id-comparison]{Comparison functions for @Id@s}
%*									*
%************************************************************************

Comparison: equality and ordering---this stuff gets {\em hammered}.

\begin{code}
cmpId (Id {idUnique = u1}) (Id {idUnique = u2}) = compare u1 u2
\end{code}

\begin{code}
instance Eq (GenId ty) where
    a == b = case (a `compare` b) of { EQ -> True;  _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False; _ -> True  }

instance Ord (GenId ty) where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = cmpId a b
\end{code}

%************************************************************************
%*									*
\subsection[Id-other-instances]{Other instance declarations for @Id@s}
%*									*
%************************************************************************

\begin{code}
instance Outputable ty => Outputable (GenId ty) where
    ppr id = pprId id

showId :: Id -> String
showId id = showSDoc (pprId id)
\end{code}

Default printing code (not used for interfaces):
\begin{code}
pprId :: Outputable ty => GenId ty -> SDoc

pprId Id {idUnique = u, idName = n, idInfo = info}
  = hcat [ppr n, pp_prags]
  where
    pp_prags | opt_PprStyle_All = case inlinePragInfo info of
				     IMustNotBeINLINEd -> text "{n}"
				     IWantToBeINLINEd  -> text "{i}"
				     IMustBeINLINEd    -> text "{I}"
				     other	       -> empty
	     | otherwise        = empty
\end{code}

\begin{code}
instance Uniquable (GenId ty) where
    uniqueOf = idUnique

instance NamedThing (GenId ty) where
    getName = idName
\end{code}

Note: The code generator doesn't carry a @UniqueSupply@, so it uses
the @Uniques@ out of local @Ids@ given to it.

%************************************************************************
%*									*
\subsection{@IdEnv@s and @IdSet@s}
%*									*
%************************************************************************

\begin{code}
type IdEnv elt = UniqFM elt

nullIdEnv	  :: IdEnv a
		  
mkIdEnv		  :: [(GenId ty, a)] -> IdEnv a
unitIdEnv	  :: GenId ty -> a -> IdEnv a
addOneToIdEnv	  :: IdEnv a -> GenId ty -> a -> IdEnv a
growIdEnv	  :: IdEnv a -> IdEnv a -> IdEnv a
growIdEnvList	  :: IdEnv a -> [(GenId ty, a)] -> IdEnv a
		  
delManyFromIdEnv  :: IdEnv a -> [GenId ty] -> IdEnv a
delOneFromIdEnv	  :: IdEnv a -> GenId ty -> IdEnv a
combineIdEnvs	  :: (a -> a -> a) -> IdEnv a -> IdEnv a -> IdEnv a
mapIdEnv	  :: (a -> b) -> IdEnv a -> IdEnv b
modifyIdEnv	  :: (a -> a) -> IdEnv a -> GenId ty -> IdEnv a
rngIdEnv	  :: IdEnv a -> [a]
		  
isNullIdEnv	  :: IdEnv a -> Bool
lookupIdEnv	  :: IdEnv a -> GenId ty -> Maybe a
lookupNoFailIdEnv :: IdEnv a -> GenId ty -> a
elemIdEnv	  :: Id -> IdEnv a -> Bool
\end{code}

\begin{code}
elemIdEnv        = elemUFM
addOneToIdEnv	 = addToUFM
combineIdEnvs	 = plusUFM_C
delManyFromIdEnv = delListFromUFM
delOneFromIdEnv	 = delFromUFM
growIdEnv	 = plusUFM
lookupIdEnv	 = lookupUFM
mapIdEnv	 = mapUFM
mkIdEnv		 = listToUFM
nullIdEnv	 = emptyUFM
rngIdEnv	 = eltsUFM
unitIdEnv	 = unitUFM
isNullIdEnv	 = isNullUFM

growIdEnvList	  env pairs = plusUFM env (listToUFM pairs)
lookupNoFailIdEnv env id    = case (lookupIdEnv env id) of { Just xx -> xx }

lookupIdSubst :: IdEnv Id -> Id -> Id
lookupIdSubst env id = case lookupIdEnv env id of
			 Just id' -> id'	-- Return original if 
			 Nothing  -> id		-- it isn't in subst

-- modifyIdEnv: Look up a thing in the IdEnv, then mash it with the
-- modify function, and put it back.

modifyIdEnv mangle_fn env key
  = case (lookupIdEnv env key) of
      Nothing -> env
      Just xx -> addOneToIdEnv env key (mangle_fn xx)

modifyIdEnv_Directly mangle_fn env key
  = case (lookupUFM_Directly env key) of
      Nothing -> env
      Just xx -> addToUFM_Directly env key (mangle_fn xx)
\end{code}

\begin{code}
type GenIdSet ty = UniqSet (GenId ty)
type IdSet 	 = UniqSet (GenId Type)

emptyIdSet	:: GenIdSet ty
intersectIdSets	:: GenIdSet ty -> GenIdSet ty -> GenIdSet ty
unionIdSets	:: GenIdSet ty -> GenIdSet ty -> GenIdSet ty
unionManyIdSets	:: [GenIdSet ty] -> GenIdSet ty
idSetToList	:: GenIdSet ty -> [GenId ty]
unitIdSet	:: GenId ty -> GenIdSet ty
addOneToIdSet	:: GenIdSet ty -> GenId ty -> GenIdSet ty
elementOfIdSet	:: GenId ty -> GenIdSet ty -> Bool
minusIdSet	:: GenIdSet ty -> GenIdSet ty -> GenIdSet ty
isEmptyIdSet	:: GenIdSet ty -> Bool
mkIdSet		:: [GenId ty] -> GenIdSet ty

emptyIdSet	= emptyUniqSet
unitIdSet	= unitUniqSet
addOneToIdSet	= addOneToUniqSet
intersectIdSets	= intersectUniqSets
unionIdSets	= unionUniqSets
unionManyIdSets	= unionManyUniqSets
idSetToList	= uniqSetToList
elementOfIdSet	= elementOfUniqSet
minusIdSet	= minusUniqSet
isEmptyIdSet	= isEmptyUniqSet
mkIdSet		= mkUniqSet
\end{code}
