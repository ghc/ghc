
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
module Id (
	-- TYPES
	GenId(..), -- *naughtily* used in some places (e.g., TcHsSyn)
	Id, IdDetails,
	StrictnessMark(..),
	ConTag, fIRST_TAG,
	DataCon, DictFun, DictVar,

	-- CONSTRUCTION
	mkDataCon,
	mkDefaultMethodId,
	mkDictFunId,
	mkIdWithNewUniq, mkIdWithNewName, mkIdWithNewType,
	mkImported,
	mkMethodSelId,
	mkRecordSelId,
	mkSuperDictSelId,
	mkSysLocal,
	mkTemplateLocals,
	mkTupleCon,
	mkUserId,
	mkUserLocal,
	mkPrimitiveId, 
	mkWorkerId,
	setIdVisibility,

	-- DESTRUCTION (excluding pragmatic info)
	idPrimRep,
	idType,
	idUnique,
	idName,

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
	cmpEqDataCon,
	cmpId,
	cmpId_withSpecDataCon,
	externallyVisibleId,
	idHasNoFreeTyVars,
	idWantsToBeINLINEd, getInlinePragma, 
	idMustBeINLINEd, idMustNotBeINLINEd,
	isBottomingId,
	isDataCon, isAlgCon, isNewCon,
	isDefaultMethodId,
	isDefaultMethodId_maybe,
	isDictFunId,
	isImportedId,
	isRecordSelector,
	isDictSelId_maybe,
	isNullaryDataCon,
	isSpecPragmaId,
	isPrimitiveId_maybe,
	isSysLocalId,
	isTupleCon,
	isWrapperId,
	toplevelishId,
	unfoldingUnfriendlyId,

	-- SUBSTITUTION
	applyTypeEnvToId,
	apply_to_Id,
	
	-- PRINTING and RENUMBERING
	pprId,
--	pprIdInUnfolding,
	showId,

	-- Specialialisation
	getIdSpecialisation,
	addIdSpecialisation,

	-- UNFOLDING, ARITY, UPDATE, AND STRICTNESS STUFF (etc)
	addIdUnfolding,
	addIdArity,
	addIdDemandInfo,
	addIdStrictness,
	addIdUpdateInfo,
	getIdArity,
	getIdDemandInfo,
	getIdInfo,
	getIdStrictness,
	getIdUnfolding,
	getIdUpdateInfo,
	getPragmaInfo,
	replaceIdInfo, replacePragmaInfo,
	addInlinePragma, nukeNoInlinePragma, addNoInlinePragma,

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
	lookupIdEnv,
	lookupNoFailIdEnv,
	mapIdEnv,
	minusIdSet,
	mkIdEnv,
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
import {-# SOURCE #-} StdIdInfo  ( addStandardIdInfo )

import CmdLineOpts      ( opt_PprStyle_All )
import SpecEnv	        ( SpecEnv   )
import Bag
import Class		( Class )
import BasicTypes	( Arity )
import IdInfo
import Maybes		( maybeToBool )
import Name	 	( nameUnique, mkLocalName, mkSysLocalName, isLocalName,
			  mkCompoundName, occNameString, modAndOcc,
			  changeUnique, isWiredInName, setNameVisibility,
			  ExportFlag(..), Provenance,
			  OccName(..), Name, Module,
			  NamedThing(..)
			) 
import PrimOp		( PrimOp )
import PrelMods		( pREL_TUP, pREL_BASE )
import FieldLabel	( fieldLabelName, FieldLabel(..){-instances-} )
import PragmaInfo	( PragmaInfo(..) )
import SrcLoc		( mkBuiltinSrcLoc )
import TysWiredIn	( tupleTyCon )
import TyCon		( TyCon, tyConDataCons, isDataTyCon, isNewTyCon, mkSpecTyCon )
import Type		( mkSigmaTy, mkTyVarTys, mkFunTys,
			  mkTyConApp, instantiateTy, mkForAllTys,
			  tyVarsOfType, instantiateTy, typePrimRep,
			  instantiateTauTy,
			  GenType, ThetaType, TauType, Type
			)
import TyVar		( TyVar, alphaTyVars, isEmptyTyVarSet, 
			  TyVarEnv, zipTyVarEnv, mkTyVarEnv
			)
import UniqFM
import UniqSet		-- practically all of it
import Unique		( getBuiltinUniques, Unique, Uniquable(..) )
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
data GenId ty = Id
	Unique		-- Key for fast comparison
	Name
	ty		-- Id's type; used all the time;
	IdDetails	-- Stuff about individual kinds of Ids.
	PragmaInfo	-- Properties of this Id requested by programmer
			-- eg specialise-me, inline-me
	IdInfo		-- Properties of this Id deduced by compiler
				   
type Id	           = GenId Type

data StrictnessMark = MarkedStrict | NotMarkedStrict

data IdDetails

  ---------------- Local values

  = LocalId	Bool		-- Local name; mentioned by the user
				-- True <=> no free type vars

  | SysLocalId	Bool	        -- Local name; made up by the compiler
				-- as for LocalId

  | PrimitiveId PrimOp		-- The Id for a primitive operation

  | SpecPragmaId 		-- Local name; introduced by the compiler
		 (Maybe Id)	-- for explicit specid in pragma
		 Bool		-- as for LocalId

  ---------------- Global values

  | ImportedId			-- Global name (Imported or Implicit); Id imported from an interface

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

  ---------------- Things to do with overloading

  | DictSelId			-- Selector that extracts a method or superclass from a dictionary
		Class		-- The class

  | DefaultMethodId		-- Default method for a particular class op
		Class		-- same class, <blah-blah> info as MethodSelId

				-- see below
  | DictFunId	Class		-- A DictFun is uniquely identified
		[Type]		-- by its class and type; this type has free type vars,
				-- whose identity is irrelevant.  Eg Class = Eq
				--				     Type  = Tree a
				-- The "a" is irrelevant.  As it is too painful to
				-- actually do comparisons that way, we kindly supply
				-- a Unique for that purpose.

  | SpecId			-- A specialisation of another Id
		Id		-- Id of which this is a specialisation
		[Maybe Type]	-- Types at which it is specialised;
				-- A "Nothing" says this type ain't relevant.
		Bool		-- True <=> no free type vars; it's not enough
				-- to know about the unspec version, because
				-- we may specialise to a type w/ free tyvars
				-- (i.e., in one of the "Maybe Type" dudes).

type ConTag	= Int
type DictVar	= Id
type DictFun	= Id
type DataCon	= Id
\end{code}

DictFunIds are generated from instance decls.
\begin{verbatim}
	class Foo a where
	  op :: a -> a -> Bool

	instance Foo a => Foo [a] where
	  op = ...
\end{verbatim}
generates the dict fun id decl
\begin{verbatim}
	dfun.Foo.[*] = \d -> ...
\end{verbatim}
The dfun id is uniquely named by the (class, type) pair.  Notice, it
isn't a (class,tycon) pair any more, because we may get manually or
automatically generated specialisations of the instance decl:
\begin{verbatim}
	instance Foo [Int] where
	  op = ...
\end{verbatim}
generates
\begin{verbatim}
	dfun.Foo.[Int] = ...
\end{verbatim}
The type variables in the name are irrelevant; we print them as stars.


Constant method ids are generated from instance decls where
there is no context; that is, no dictionaries are needed to
construct the method.  Example
\begin{verbatim}
	instance Foo Int where
	  op = ...
\end{verbatim}
Then we get a constant method
\begin{verbatim}
	Foo.op.Int = ...
\end{verbatim}

It is possible, albeit unusual, to have a constant method
for an instance decl which has type vars:
\begin{verbatim}
	instance Foo [a] where
	  op []     ys = True
	  op (x:xs) ys = False
\end{verbatim}
We get the constant method
\begin{verbatim}
	Foo.op.[*] = ...
\end{verbatim}
So a constant method is identified by a class/op/type triple.
The type variables in the type are irrelevant.


For Ids whose names must be known/deducible in other modules, we have
to conjure up their worker's names (and their worker's worker's
names... etc) in a known systematic way.


%************************************************************************
%*									*
\subsection[Id-documentation]{Documentation}
%*									*
%************************************************************************

[A BIT DATED [WDP]]

The @Id@ datatype describes {\em values}.  The basic things we want to
know: (1)~a value's {\em type} (@idType@ is a very common
operation in the compiler); and (2)~what ``flavour'' of value it might
be---for example, it can be terribly useful to know that a value is a
class method.

\begin{description}
%----------------------------------------------------------------------
\item[@AlgConId@:] For the data constructors declared by a @data@
declaration.  Their type is kept in {\em two} forms---as a regular
@Type@ (in the usual place), and also in its constituent pieces (in
the ``details''). We are frequently interested in those pieces.

%----------------------------------------------------------------------
\item[@TupleConId@:] This is just a special shorthand for @DataCons@ for
the infinite family of tuples.

%----------------------------------------------------------------------
\item[@ImportedId@:] These are values defined outside this module.
{\em Everything} we want to know about them must be stored here (or in
their @IdInfo@).

%----------------------------------------------------------------------
\item[@MethodSelId@:] A selector from a dictionary; it may select either
a method or a dictionary for one of the class's superclasses.

%----------------------------------------------------------------------
\item[@DictFunId@:]

@mkDictFunId [a,b..] theta C T@ is the function derived from the
instance declaration

	instance theta => C (T a b ..) where
		...

It builds function @Id@ which maps dictionaries for theta,
to a dictionary for C (T a b ..).

*Note* that with the ``Mark Jones optimisation'', the theta may
include dictionaries for the immediate superclasses of C at the type
(T a b ..).

%----------------------------------------------------------------------
\item[@SpecId@:]

%----------------------------------------------------------------------
\item[@LocalId@:] A purely-local value, e.g., a function argument,
something defined in a @where@ clauses, ... --- but which appears in
the original program text.

%----------------------------------------------------------------------
\item[@SysLocalId@:] Same as a @LocalId@, except does {\em not} appear in
the original program text; these are introduced by the compiler in
doing its thing.

%----------------------------------------------------------------------
\item[@SpecPragmaId@:] Introduced by the compiler to record
Specialisation pragmas. It is dead code which MUST NOT be removed
before specialisation.
\end{description}

Further remarks:
\begin{enumerate}
%----------------------------------------------------------------------
\item

@DataCons@ @TupleCons@, @Importeds@, @SuperDictSelIds@,
@MethodSelIds@, @DictFunIds@, and @DefaultMethodIds@ have the following
properties:
\begin{itemize}
\item
They have no free type variables, so if you are making a
type-variable substitution you don't need to look inside them.
\item
They are constants, so they are not free variables.  (When the STG
machine makes a closure, it puts all the free variables in the
closure; the above are not required.)
\end{itemize}
Note that @Locals@ and @SysLocals@ {\em may} have the above
properties, but they may not.
\end{enumerate}

%************************************************************************
%*									*
\subsection[Id-general-funs]{General @Id@-related functions}
%*									*
%************************************************************************

\begin{code}
-- isDataCon returns False for @newtype@ constructors
isDataCon (Id _ _ _ (AlgConId _ _ _ _ _ _ _ _ tc) _ _) = isDataTyCon tc
isDataCon (Id _ _ _ (TupleConId _) _ _)		        = True
isDataCon (Id _ _ _ (SpecId unspec _ _) _ _)	        = isDataCon unspec
isDataCon other					        = False

isNewCon (Id _ _ _ (AlgConId _ _ _ _ _ _ _ _ tc) _ _) = isNewTyCon tc
isNewCon other					       = False

-- isAlgCon returns True for @data@ or @newtype@ constructors
isAlgCon (Id _ _ _ (AlgConId _ _ _ _ _ _ _ _ _) _ _) = True
isAlgCon (Id _ _ _ (TupleConId _) _ _)		      = True
isAlgCon (Id _ _ _ (SpecId unspec _ _) _ _)	      = isAlgCon unspec
isAlgCon other					      = False

isTupleCon (Id _ _ _ (TupleConId _) _ _)	 = True
isTupleCon (Id _ _ _ (SpecId unspec _ _) _ _)	 = isTupleCon unspec
isTupleCon other				 = False
\end{code}

@toplevelishId@ tells whether an @Id@ {\em may} be defined in a nested
@let(rec)@ (returns @False@), or whether it is {\em sure} to be
defined at top level (returns @True@). This is used to decide whether
the @Id@ is a candidate free variable. NB: you are only {\em sure}
about something if it returns @True@!

\begin{code}
toplevelishId	  :: Id -> Bool
idHasNoFreeTyVars :: Id -> Bool

toplevelishId (Id _ _ _ details _ _)
  = chk details
  where
    chk (AlgConId _ __ _ _ _ _ _ _)   = True
    chk (TupleConId _)    	    = True
    chk (RecordSelId _)   	    = True
    chk ImportedId	    	    = True
    chk (DictSelId _)		    = True
    chk (DefaultMethodId _)         = True
    chk (DictFunId     _ _)	    = True
    chk (SpecId unspec _ _)	    = toplevelishId unspec
				    -- depends what the unspecialised thing is
    chk (LocalId      _)	    = False
    chk (SysLocalId   _)	    = False
    chk (SpecPragmaId _ _)	    = False
    chk (PrimitiveId _)		    = True

idHasNoFreeTyVars (Id _ _ _ details _ info)
  = chk details
  where
    chk (AlgConId _ _ _ _ _ _ _ _ _) = True
    chk (TupleConId _)    	  = True
    chk (RecordSelId _)   	  = True
    chk ImportedId	    	  = True
    chk (DictSelId _)		  = True
    chk (DefaultMethodId _)       = True
    chk (DictFunId     _ _)	  = True
    chk (SpecId _     _   no_free_tvs) = no_free_tvs
    chk (LocalId        no_free_tvs) = no_free_tvs
    chk (SysLocalId     no_free_tvs) = no_free_tvs
    chk (SpecPragmaId _ no_free_tvs) = no_free_tvs
    chk (PrimitiveId _)		    = True

-- omitIfaceSigForId tells whether an Id's info is implied by other declarations,
-- so we don't need to put its signature in an interface file, even if it's mentioned
-- in some other interface unfolding.

omitIfaceSigForId
	:: Id
	-> Bool

omitIfaceSigForId (Id _ name _ details _ _)
  | isWiredInName name
  = True

  | otherwise
  = case details of
        ImportedId	  -> True		-- Never put imports in interface file
        (PrimitiveId _)	  -> True		-- Ditto, for primitives

	-- This group is Ids that are implied by their type or class decl;
	-- remember that all type and class decls appear in the interface file.
	-- The dfun id must *not* be omitted, because it carries version info for
	-- the instance decl
        (AlgConId _ _ _ _ _ _ _ _ _) -> True
        (TupleConId _)    	  -> True
        (RecordSelId _)   	  -> True
        (DictSelId _)		  -> True

	other			  -> False	-- Don't omit!
		-- NB DefaultMethodIds are not omitted
\end{code}

\begin{code}
isImportedId (Id _ _ _ ImportedId _ _) = True
isImportedId other		       = False

isBottomingId (Id _ _ _ _ _ info) = bottomIsGuaranteed (strictnessInfo info)

isSysLocalId (Id _ _ _ (SysLocalId _) _ _) = True
isSysLocalId other			   = False

isSpecPragmaId (Id _ _ _ (SpecPragmaId _ _) _ _) = True
isSpecPragmaId other			         = False

isSpecId_maybe (Id _ _ _ (SpecId unspec ty_maybes _) _ _)
  = ASSERT(not (maybeToBool (isSpecId_maybe unspec)))
    Just (unspec, ty_maybes)
isSpecId_maybe other_id
  = Nothing

isDictSelId_maybe (Id _ _ _ (DictSelId cls) _ _) = Just cls
isDictSelId_maybe _				 = Nothing

isDefaultMethodId (Id _ _ _ (DefaultMethodId _) _ _) = True
isDefaultMethodId other				     = False

isDefaultMethodId_maybe (Id _ _ _ (DefaultMethodId cls) _ _)
  = Just cls
isDefaultMethodId_maybe other = Nothing

isDictFunId (Id _ _ _ (DictFunId _ _) _ _) = True
isDictFunId other		    	   = False

isWrapperId id = workerExists (getIdStrictness id)

isPrimitiveId_maybe (Id _ _ _ (PrimitiveId primop) _ _) = Just primop
isPrimitiveId_maybe other				= Nothing
\end{code}

\begin{code}
unfoldingUnfriendlyId	-- return True iff it is definitely a bad
	:: Id		-- idea to export an unfolding that
	-> Bool		-- mentions this Id.  Reason: it cannot
			-- possibly be seen in another module.

unfoldingUnfriendlyId id = not (externallyVisibleId id)
\end{code}

@externallyVisibleId@: is it true that another module might be
able to ``see'' this Id in a code generation sense. That
is, another .o file might refer to this Id.

In tidyCorePgm (SimplCore.lhs) we carefully set each top level thing's
local-ness precisely so that the test here would be easy

\begin{code}
externallyVisibleId :: Id -> Bool
externallyVisibleId id@(Id _ name _ _ _ _) = not (isLocalName name)
		     -- not local => global => externally visible
\end{code}

CLAIM (not ASSERTed) for @applyTypeEnvToId@ and @applySubstToId@:
`Top-levelish Ids'' cannot have any free type variables, so applying
the type-env cannot have any effect.  (NB: checked in CoreLint?)

\begin{code}
type TypeEnv = TyVarEnv Type

applyTypeEnvToId :: TypeEnv -> Id -> Id
applyTypeEnvToId type_env id@(Id _ _ ty _ _ _)
  = apply_to_Id ( \ ty ->
	instantiateTy type_env ty
    ) id
\end{code}

\begin{code}
apply_to_Id :: (Type -> Type) -> Id -> Id

apply_to_Id ty_fn id@(Id u n ty details prag info)
  | idHasNoFreeTyVars id
  = id
  | otherwise
  = Id u n (ty_fn ty) (apply_to_details details) prag (apply_to_IdInfo ty_fn info)
  where
    apply_to_details (SpecId unspec ty_maybes no_ftvs)
      = let
	    new_unspec = apply_to_Id ty_fn unspec
	    new_maybes = map apply_to_maybe ty_maybes
	in
	SpecId new_unspec new_maybes (no_free_tvs ty)
	-- ToDo: gratuitous recalc no_ftvs????
      where
	apply_to_maybe Nothing   = Nothing
	apply_to_maybe (Just ty) = Just (ty_fn ty)

    apply_to_details other = other
\end{code}


%************************************************************************
%*									*
\subsection[Id-type-funs]{Type-related @Id@ functions}
%*									*
%************************************************************************

\begin{code}
idName :: GenId ty -> Name
idName (Id _ n _ _ _ _) = n

idType :: GenId ty -> ty
idType (Id _ _ ty _ _ _) = ty

idPrimRep i = typePrimRep (idType i)
\end{code}

%************************************************************************
%*									*
\subsection[Id-overloading]{Functions related to overloading}
%*									*
%************************************************************************

\begin{code}
mkSuperDictSelId :: Unique -> Class -> Int -> Type -> Id
	-- The Int is an arbitrary tag to say which superclass is selected
	-- So, for 
	--	class (C a, C b) => Foo a b where ...
	-- we get superclass selectors
	--	Foo_sc1, Foo_sc2

mkSuperDictSelId u clas index ty
  = addStandardIdInfo $
    Id u name ty details NoPragmaInfo noIdInfo
  where
    name    = mkCompoundName name_fn u (getName clas)
    details = DictSelId clas
    name_fn clas_str = clas_str _APPEND_ SLIT("_sc") _APPEND_ (_PK_ (show index))

	-- For method selectors the clean thing to do is
	-- to give the method selector the same name as the class op itself.
mkMethodSelId op_name clas ty
  = addStandardIdInfo $
    Id (uniqueOf op_name) op_name ty (DictSelId clas) NoPragmaInfo noIdInfo

mkDefaultMethodId dm_name rec_c ty
  = Id (uniqueOf dm_name) dm_name ty (DefaultMethodId rec_c) NoPragmaInfo noIdInfo

mkDictFunId dfun_name full_ty clas itys
  = Id (nameUnique dfun_name) dfun_name full_ty details NoPragmaInfo noIdInfo
  where
    details  = DictFunId clas itys

mkWorkerId u unwrkr ty info
  = Id u name ty details NoPragmaInfo info
  where
    details = LocalId (no_free_tvs ty)
    name    = mkCompoundName name_fn u (getName unwrkr)
    name_fn wkr_str = SLIT("$w") _APPEND_ wkr_str
\end{code}

%************************************************************************
%*									*
\subsection[local-funs]{@LocalId@-related functions}
%*									*
%************************************************************************

\begin{code}
mkImported  n ty info = Id (nameUnique n) n ty ImportedId NoPragmaInfo info

mkPrimitiveId n ty primop 
  = addStandardIdInfo $
    Id (nameUnique n) n ty (PrimitiveId primop) IMustBeINLINEd noIdInfo
	-- The pragma @IMustBeINLINEd@ says that this Id absolutely must be inlined.
	-- It's only true for primitives, because we don't want to make a closure for each of them.

\end{code}

\begin{code}
no_free_tvs ty = isEmptyTyVarSet (tyVarsOfType ty)

-- SysLocal: for an Id being created by the compiler out of thin air...
-- UserLocal: an Id with a name the user might recognize...
mkSysLocal  :: FAST_STRING -> Unique -> GenType flexi -> SrcLoc -> GenId (GenType flexi)
mkUserLocal :: OccName     -> Unique -> GenType flexi -> SrcLoc -> GenId (GenType flexi)

mkSysLocal str uniq ty loc
  = Id uniq (mkSysLocalName uniq str loc) ty (SysLocalId (no_free_tvs ty)) NoPragmaInfo noIdInfo

mkUserLocal occ uniq ty loc
  = Id uniq (mkLocalName uniq occ loc) ty (LocalId (no_free_tvs ty)) NoPragmaInfo noIdInfo

mkUserId :: Name -> GenType flexi -> PragmaInfo -> GenId (GenType flexi)
mkUserId name ty pragma_info
  = Id (nameUnique name) name ty (LocalId (no_free_tvs ty)) pragma_info noIdInfo
\end{code}

\begin{code}
-- See notes with setNameVisibility (Name.lhs)
setIdVisibility :: Maybe Module -> Unique -> Id -> Id
setIdVisibility maybe_mod u (Id uniq name ty details prag info)
  = Id uniq (setNameVisibility maybe_mod u name) ty details prag info

mkIdWithNewUniq :: Id -> Unique -> Id
mkIdWithNewUniq (Id _ n ty details prag info) u
  = Id u (changeUnique n u) ty details prag info

mkIdWithNewName :: Id -> Name -> Id
mkIdWithNewName (Id _ _ ty details prag info) new_name
  = Id (uniqueOf new_name) new_name ty details prag info

mkIdWithNewType :: Id -> Type -> Id
mkIdWithNewType (Id u name _ details pragma info) ty 
  = Id u name ty details pragma info

{-
-- Specialised version of constructor: only used in STG and code generation
-- Note: The specialsied Id has the same unique as the unspeced Id

mkSameSpecCon ty_maybes unspec@(Id u name ty details pragma info)
  = ASSERT(isDataCon unspec)
    ASSERT(not (maybeToBool (isSpecId_maybe unspec)))
    Id u name new_ty (SpecId unspec ty_maybes (no_free_tvs new_ty)) pragma info
  where
    new_ty = specialiseTy ty ty_maybes 0

    -- pprTrace "SameSpecCon:Unique:"
    --	        (ppSep (ppr unspec: [pprMaybeTy ty | ty <- ty_maybes]))
-}
\end{code}

Make some local @Ids@ for a template @CoreExpr@.  These have bogus
@Uniques@, but that's OK because the templates are supposed to be
instantiated before use.
\begin{code}
mkTemplateLocals :: [Type] -> [Id]
mkTemplateLocals tys
  = zipWith (\ u -> \ ty -> mkSysLocal SLIT("tpl") u ty mkBuiltinSrcLoc)
	    (getBuiltinUniques (length tys))
	    tys
\end{code}

\begin{code}
getIdInfo     :: GenId ty -> IdInfo
getPragmaInfo :: GenId ty -> PragmaInfo

getIdInfo     (Id _ _ _ _ _ info) = info
getPragmaInfo (Id _ _ _ _ info _) = info

replaceIdInfo :: Id -> IdInfo -> Id
replaceIdInfo (Id u n ty details pinfo _) info = Id u n ty details pinfo info

replacePragmaInfo :: GenId ty -> PragmaInfo -> GenId ty
replacePragmaInfo (Id u sn ty details _ info) prag = Id u sn ty details prag info
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
getIdArity id@(Id _ _ _ _ _ id_info)
  = arityInfo id_info

addIdArity :: Id -> ArityInfo -> Id
addIdArity (Id u n ty details pinfo info) arity
  = Id u n ty details pinfo (info `addArityInfo` arity)
\end{code}

%************************************************************************
%*									*
\subsection[constructor-funs]{@DataCon@-related functions (incl.~tuples)}
%*									*
%************************************************************************

\begin{code}
mkDataCon :: Name
	  -> [StrictnessMark] -> [FieldLabel]
	  -> [TyVar] -> ThetaType
	  -> [TyVar] -> ThetaType
	  -> [TauType] -> TyCon
	  -> Id
  -- can get the tag and all the pieces of the type from the Type

mkDataCon n stricts fields tvs ctxt con_tvs con_ctxt args_tys tycon
  = ASSERT(length stricts == length args_tys)
    addStandardIdInfo data_con
  where
    -- NB: data_con self-recursion; should be OK as tags are not
    -- looked at until late in the game.
    data_con
      = Id (nameUnique n)
	   n
	   data_con_ty
	   (AlgConId data_con_tag stricts fields tvs ctxt con_tvs con_ctxt args_tys tycon)
	   IWantToBeINLINEd	-- Always inline constructors if possible
	   noIdInfo

    data_con_tag    = assoc "mkDataCon" (data_con_family `zip` [fIRST_TAG..]) data_con
    data_con_family = tyConDataCons tycon

    data_con_ty
      = mkSigmaTy (tvs++con_tvs) (ctxt++con_ctxt)
	(mkFunTys args_tys (mkTyConApp tycon (mkTyVarTys tvs)))


mkTupleCon :: Arity -> Name -> Type -> Id
mkTupleCon arity name ty 
  = addStandardIdInfo tuple_id
  where
    tuple_id = Id (nameUnique name) name ty 
	  	  (TupleConId arity) 
		  IWantToBeINLINEd		-- Always inline constructors if possible
	  	  noIdInfo

fIRST_TAG :: ConTag
fIRST_TAG =  1	-- Tags allocated from here for real constructors
\end{code}

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
dataConTag (Id _ _ _ (AlgConId tag _ _ _ _ _ _ _ _) _ _) = tag
dataConTag (Id _ _ _ (TupleConId _) _ _)	      = fIRST_TAG
dataConTag (Id _ _ _ (SpecId unspec _ _) _ _)	      = dataConTag unspec

dataConTyCon :: DataCon -> TyCon	-- will panic if not a DataCon
dataConTyCon (Id _ _ _ (AlgConId _ _ _ _ _ _ _ _ tycon) _ _) = tycon
dataConTyCon (Id _ _ _ (TupleConId a) _ _)	          = tupleTyCon a

dataConSig :: DataCon -> ([TyVar], ThetaType, [TyVar], ThetaType, [TauType], TyCon)
					-- will panic if not a DataCon

dataConSig (Id _ _ _ (AlgConId _ _ _ tyvars theta con_tyvars con_theta arg_tys tycon) _ _)
  = (tyvars, theta, con_tyvars, con_theta, arg_tys, tycon)

dataConSig (Id _ _ _ (TupleConId arity) _ _)
  = (tyvars, [], [], [], tyvar_tys, tupleTyCon arity)
  where
    tyvars	= take arity alphaTyVars
    tyvar_tys	= mkTyVarTys tyvars

dataConSig (Id _ _ _ (SpecId unspec ty_maybes _) _ _)
  = (spec_tyvars, spec_theta_ty, spec_con_tyvars, spec_con_theta, spec_arg_tys, spec_tycon)
  where
    (tyvars, theta_ty, con_tyvars, con_theta, arg_tys, tycon) = dataConSig unspec

    ty_env = tyvars `zip` ty_maybes

    spec_tyvars     = [tyvar | (tyvar, Nothing) <- ty_env]
    spec_con_tyvars = [tyvar | (tyvar, Nothing) <- con_tyvars `zip` ty_maybes] -- Hmm..

    spec_env = mkTyVarEnv [(tyvar, ty) | (tyvar, Just ty) <- ty_env]
    spec_arg_tys = map (instantiateTauTy spec_env) arg_tys

    spec_theta_ty  = if null theta_ty then []
		     else panic "dataConSig:ThetaTy:SpecDataCon1"
    spec_con_theta = if null con_theta then []
		     else panic "dataConSig:ThetaTy:SpecDataCon2"
    spec_tycon     = mkSpecTyCon tycon ty_maybes


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
dataConRepType (Id _ _ _ (AlgConId _ _ _ tyvars theta con_tyvars con_theta arg_tys tycon) _ _)
  = mkForAllTys (tyvars++con_tyvars) 
		(mkFunTys arg_tys (mkTyConApp tycon (mkTyVarTys tyvars)))
dataConRepType other_id
  = ASSERT( isDataCon other_id )
    idType other_id

dataConFieldLabels :: DataCon -> [FieldLabel]
dataConFieldLabels (Id _ _ _ (AlgConId _ _ fields _ _ _ _ _ _) _ _) = fields
dataConFieldLabels (Id _ _ _ (TupleConId _)		    _ _) = []
#ifdef DEBUG
dataConFieldLabels x@(Id _ _ _ idt _ _) = 
  panic ("dataConFieldLabel: " ++
    (case idt of
      LocalId _    -> "l"
      SysLocalId _ -> "sl"
      PrimitiveId _ -> "p"
      SpecPragmaId _  _ -> "sp"
      ImportedId -> "i"
      RecordSelId _ -> "r"
      DictSelId _ -> "m"
      DefaultMethodId _ -> "d"
      DictFunId _ _ -> "di"
      SpecId _ _ _ -> "spec"))
#endif

dataConStrictMarks :: DataCon -> [StrictnessMark]
dataConStrictMarks (Id _ _ _ (AlgConId _ stricts _ _ _ _ _ _ _) _ _) = stricts
dataConStrictMarks (Id _ _ _ (TupleConId arity)		     _ _) 
  = nOfThem arity NotMarkedStrict

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
mkRecordSelId field_label selector_ty
  = addStandardIdInfo $		-- Record selectors have a standard unfolding
    Id (nameUnique name)
       name
       selector_ty
       (RecordSelId field_label)
       NoPragmaInfo
       noIdInfo
  where
    name = fieldLabelName field_label

recordSelectorFieldLabel :: Id -> FieldLabel
recordSelectorFieldLabel (Id _ _ _ (RecordSelId lbl) _ _) = lbl

isRecordSelector (Id _ _ _ (RecordSelId lbl) _ _) = True
isRecordSelector other				  = False
\end{code}


Data type declarations are of the form:
\begin{verbatim}
data Foo a b = C1 ... | C2 ... | ... | Cn ...
\end{verbatim}
For each constructor @Ci@, we want to generate a curried function; so, e.g., for
@C1 x y z@, we want a function binding:
\begin{verbatim}
fun_C1 = /\ a -> /\ b -> \ [x, y, z] -> Con C1 [a, b] [x, y, z]
\end{verbatim}
Notice the ``big lambdas'' and type arguments to @Con@---we are producing
2nd-order polymorphic lambda calculus with explicit types.

%************************************************************************
%*									*
\subsection[unfolding-Ids]{Functions related to @Ids@' unfoldings}
%*									*
%************************************************************************

\begin{code}
getIdUnfolding :: Id -> Unfolding

getIdUnfolding (Id _ _ _ _ _ info) = unfoldInfo info

addIdUnfolding :: Id -> Unfolding -> Id
addIdUnfolding id@(Id u n ty details prag info) unfolding
  = Id u n ty details prag (info `addUnfoldInfo` unfolding)
\end{code}

The inline pragma tells us to be very keen to inline this Id, but it's still
OK not to if optimisation is switched off.

\begin{code}
getInlinePragma :: Id -> PragmaInfo
getInlinePragma (Id _ _ _ _ prag _) = prag

idWantsToBeINLINEd :: Id -> Bool

idWantsToBeINLINEd (Id _ _ _ _ IWantToBeINLINEd _) = True
idWantsToBeINLINEd (Id _ _ _ _ IMustBeINLINEd   _) = True
idWantsToBeINLINEd _				   = False

idMustNotBeINLINEd (Id _ _ _ _ IMustNotBeINLINEd _) = True
idMustNotBeINLINEd _				    = False

idMustBeINLINEd (Id _ _ _ _ IMustBeINLINEd _) = True
idMustBeINLINEd _			      = False

addInlinePragma :: Id -> Id
addInlinePragma (Id u sn ty details _ info)
  = Id u sn ty details IWantToBeINLINEd info

nukeNoInlinePragma :: Id -> Id
nukeNoInlinePragma id@(Id u sn ty details IMustNotBeINLINEd info)
  = Id u sn ty details NoPragmaInfo info
nukeNoInlinePragma id@(Id u sn ty details _ info) = id		-- Otherwise no-op

addNoInlinePragma :: Id -> Id
addNoInlinePragma id@(Id u sn ty details _ info)
  = Id u sn ty details IMustNotBeINLINEd info
\end{code}



%************************************************************************
%*									*
\subsection[IdInfo-funs]{Functions related to @Ids@' @IdInfos@}
%*									*
%************************************************************************

\begin{code}
getIdDemandInfo :: Id -> DemandInfo
getIdDemandInfo (Id _ _ _ _ _ info) = demandInfo info

addIdDemandInfo :: Id -> DemandInfo -> Id
addIdDemandInfo (Id u n ty details prags info) demand_info
  = Id u n ty details prags (info `addDemandInfo` demand_info)
\end{code}

\begin{code}
getIdUpdateInfo :: Id -> UpdateInfo
getIdUpdateInfo (Id _ _ _ _ _ info) = updateInfo info

addIdUpdateInfo :: Id -> UpdateInfo -> Id
addIdUpdateInfo (Id u n ty details prags info) upd_info
  = Id u n ty details prags (info `addUpdateInfo` upd_info)
\end{code}

\begin{code}
{- LATER:
getIdArgUsageInfo :: Id -> ArgUsageInfo
getIdArgUsageInfo (Id u n ty info details) = argUsageInfo info

addIdArgUsageInfo :: Id -> ArgUsageInfo -> Id
addIdArgUsageInfo (Id u n ty info details) au_info
  = Id u n ty (info `addArgusageInfo` au_info) details
-}
\end{code}

\begin{code}
{- LATER:
getIdFBTypeInfo :: Id -> FBTypeInfo
getIdFBTypeInfo (Id u n ty info details) = fbTypeInfo info

addIdFBTypeInfo :: Id -> FBTypeInfo -> Id
addIdFBTypeInfo (Id u n ty info details) upd_info
  = Id u n ty (info `addFBTypeInfo` upd_info) details
-}
\end{code}

\begin{code}
getIdSpecialisation :: Id -> IdSpecEnv
getIdSpecialisation (Id _ _ _ _ _ info) = specInfo info

addIdSpecialisation :: Id -> IdSpecEnv -> Id
addIdSpecialisation (Id u n ty details prags info) spec_info
  = Id u n ty details prags (info `addSpecInfo` spec_info)
\end{code}

Strictness: we snaffle the info out of the IdInfo.

\begin{code}
getIdStrictness :: Id -> StrictnessInfo

getIdStrictness (Id _ _ _ _ _ info) = strictnessInfo info

addIdStrictness :: Id -> StrictnessInfo -> Id
addIdStrictness (Id u n ty details prags info) strict_info
  = Id u n ty details prags (info `addStrictnessInfo` strict_info)
\end{code}

%************************************************************************
%*									*
\subsection[Id-comparison]{Comparison functions for @Id@s}
%*									*
%************************************************************************

Comparison: equality and ordering---this stuff gets {\em hammered}.

\begin{code}
cmpId (Id u1 _ _ _ _ _) (Id u2 _ _ _ _ _) = compare u1 u2
-- short and very sweet
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

@cmpId_withSpecDataCon@ ensures that any spectys are taken into
account when comparing two data constructors. We need to do this
because a specialised data constructor has the same Unique as its
unspecialised counterpart.

\begin{code}
cmpId_withSpecDataCon :: Id -> Id -> Ordering

cmpId_withSpecDataCon id1 id2
  | eq_ids && isDataCon id1 && isDataCon id2
  = cmpEqDataCon id1 id2

  | otherwise
  = cmp_ids
  where
    cmp_ids = cmpId id1 id2
    eq_ids  = case cmp_ids of { EQ -> True; other -> False }

cmpEqDataCon (Id _ _ _ (SpecId _ mtys1 _) _ _) (Id _ _ _ (SpecId _ mtys2 _) _ _)
  = panic "Id.cmpEqDataCon:cmpUniTypeMaybeList mtys1 mtys2"

cmpEqDataCon _ (Id _ _ _ (SpecId _ _ _) _ _) = LT
cmpEqDataCon (Id _ _ _ (SpecId _ _ _) _ _) _ = GT
cmpEqDataCon _				   _ = EQ
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

pprId (Id u n _ _ prags _)
  = hcat [ppr n, pp_prags]
  where
    pp_prags | opt_PprStyle_All = case prags of
				     IMustNotBeINLINEd -> text "{n}"
				     IWantToBeINLINEd  -> text "{i}"
				     IMustBeINLINEd    -> text "{I}"
				     other	       -> empty
	     | otherwise        = empty

  -- WDP 96/05/06: We can re-elaborate this as we go along...
\end{code}

\begin{code}
idUnique (Id u _ _ _ _ _) = u

instance Uniquable (GenId ty) where
    uniqueOf = idUnique

instance NamedThing (GenId ty) where
    getName this_id@(Id u n _ details _ _) = n
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
\end{code}

\begin{code}
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

growIdEnvList	  env pairs = plusUFM env (listToUFM pairs)
isNullIdEnv	  env	    = sizeUFM env == 0
lookupNoFailIdEnv env id    = case (lookupIdEnv env id) of { Just xx -> xx }

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
