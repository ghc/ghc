%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
#include "HsVersions.h"

module Id (
	Id, -- abstract
	IdInfo,	-- re-exporting
	ConTag(..), DictVar(..), DictFun(..), DataCon(..),

	-- CONSTRUCTION
	mkSysLocal, mkUserLocal,
	mkSpecPragmaId,
	mkSpecId, mkSameSpecCon,
	selectIdInfoForSpecId,
	mkTemplateLocals,
	mkImported, mkPreludeId,
	mkDataCon, mkTupleCon,
	mkIdWithNewUniq,
	mkClassOpId, mkSuperDictSelId, mkDefaultMethodId,
	mkConstMethodId, mkInstId,
#ifdef DPH
	mkProcessorCon,
	mkPodId,
#endif {- Data Parallel Haskell -}

	updateIdType,
	mkId, mkDictFunId,
	mkWorkerId,
	localiseId,

	-- DESTRUCTION
	getIdUniType,
	getInstNamePieces, getIdInfo, replaceIdInfo,
	getIdKind, getInstIdModule,
	getMentionedTyConsAndClassesFromId,
	getDataConTag,
	getDataConSig, getInstantiatedDataConSig,
	getDataConTyCon, -- UNUSED: getDataConFamily,
#ifdef USE_SEMANTIQUE_STRANAL
	getDataConDeps,
#endif

	-- PREDICATES
	isDataCon, isTupleCon, isNullaryDataCon,
	isSpecId_maybe, isSpecPragmaId_maybe,
	toplevelishId, externallyVisibleId,
	isTopLevId, isWorkerId, isWrapperId,
	isImportedId, isSysLocalId,
	isBottomingId,
	isClassOpId, isDefaultMethodId_maybe, isSuperDictSelId_maybe,
	isDictFunId, isInstId_maybe, isConstMethodId_maybe, 
#ifdef DPH
	isInventedTopLevId,
	isProcessorCon,
#endif {- Data Parallel Haskell -}
	eqId, cmpId,
	cmpId_withSpecDataCon,
	myWrapperMaybe,
	whatsMentionedInId,
	unfoldingUnfriendlyId,	-- ToDo: rm, eventually
	idWantsToBeINLINEd,
--	dataConMentionsNonPreludeTyCon,

	-- SUBSTITUTION
	applySubstToId, applyTypeEnvToId,
-- not exported:	apply_to_Id, -- please don't use this, generally

	-- UNFOLDING, ARITY, UPDATE, AND STRICTNESS STUFF (etc)
	getIdArity, getDataConArity, addIdArity,
	getIdDemandInfo, addIdDemandInfo,
	getIdSpecialisation, addIdSpecialisation,
	getIdStrictness, addIdStrictness,
	getIdUnfolding, addIdUnfolding, -- UNUSED? clearIdUnfolding,
	getIdUpdateInfo, addIdUpdateInfo,
	getIdArgUsageInfo, addIdArgUsageInfo,
	getIdFBTypeInfo, addIdFBTypeInfo,
	-- don't export the types, lest OptIdInfo be dragged in!

	-- MISCELLANEOUS
	unlocaliseId,
	fIRST_TAG,
	showId,
	pprIdInUnfolding,

	-- and to make the interface self-sufficient...
	Class, ClassOp, GlobalSwitch, Inst, Maybe, Name,
	FullName, PprStyle, PrettyRep,
	PrimKind, SrcLoc, Pretty(..), Subst, UnfoldingDetails,
	TyCon, TyVar, TyVarTemplate, TauType(..), UniType, Unique,
	UniqueSupply, Arity(..), ThetaType(..),
	TypeEnv(..), UniqFM, InstTemplate, Bag,
	SpecEnv, nullSpecEnv, SpecInfo,

	-- and to make sure pragmas work...
	IdDetails  -- from this module, abstract
	IF_ATTACK_PRAGMAS(COMMA getMentionedTyConsAndClassesFromUniType)
	IF_ATTACK_PRAGMAS(COMMA bottomIsGuaranteed)
	IF_ATTACK_PRAGMAS(COMMA getInfo_UF)

#ifndef __GLASGOW_HASKELL__
	, TAG_
#endif
    ) where

IMPORT_Trace		-- ToDo: rm (debugging only)

import AbsPrel		( PrimOp, PrimKind, mkFunTy, nilDataCon, pRELUDE_BUILTIN
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
#ifdef DPH
			  , mkPodNTy, mkPodizedPodNTy
#endif {- Data Parallel Haskell -}
			)

import AbsUniType
import Bag
import CLabelInfo	( identToC, cSEP )
import CmdLineOpts	( GlobalSwitch(..) )
import IdEnv		-- ( nullIdEnv, IdEnv )
import IdInfo		-- piles of it
import Inst		-- lots of things
import Maybes		( maybeToBool, Maybe(..) )
import Name		( Name(..) )
import NameTypes
import Outputable
import Pretty		-- for pretty-printing
import SrcLoc
import Subst		( applySubstToTy )       -- PRETTY GRIMY TO LOOK IN HERE
import PlainCore
import PrelFuns		( pcGenerateTupleSpecs ) -- PRETTY GRIMY TO LOOK IN HERE
import UniqFM
import UniqSet
import Unique
import Util
#ifdef DPH
IMPORT_Trace
import PodizeCore	( podizeTemplateExpr )
import PodInfoTree	( infoTypeNumToMask )
#endif {- Data Parallel Haskell -}
\end{code}

Here are the @Id@ and @IdDetails@ datatypes; also see the notes that
follow.

Every @Id@ has a @Unique@, to uniquify it and for fast comparison, a
@UniType@, and an @IdInfo@ (non-essential info about it, e.g.,
strictness).  The essential info about different kinds of @Ids@ is
in its @IdDetails@.

ToDo: possibly cache other stuff in the single-constructor @Id@ type.

\begin{code}
data Id = Id	Unique		-- key for fast comparison
		UniType		-- Id's type; used all the time;
		IdInfo		-- non-essential info about this Id;
		IdDetails	-- stuff about individual kinds of Ids.

data IdDetails

  ---------------- Local values

  = LocalId	ShortName	-- mentioned by the user
		Bool		-- True <=> no free type vars

  | SysLocalId	ShortName	-- made up by the compiler
		Bool		-- as for LocalId

  | SpecPragmaId ShortName	-- introduced by the compiler
		(Maybe SpecInfo)-- for explicit specid in pragma 
		Bool		-- as for LocalId

  ---------------- Global values

  | ImportedId	FullName	-- Id imported from an interface

  | PreludeId	FullName	-- things < Prelude that compiler "knows" about

  | TopLevId	FullName	-- Top-level in the orig source pgm
				-- (not moved there by transformations).

	-- a TopLevId's type may contain free type variables, if
	-- the monomorphism restriction applies.

  ---------------- Data constructors

  | DataConId	FullName
		ConTag
		-- cached pieces of the type:
		[TyVarTemplate] [(Class,UniType)] [UniType] TyCon
		-- the type is:
		-- forall tyvars . theta_ty =>
		--    unitype_1 -> ... -> unitype_n -> tycon tyvars
		--
		-- "type ThetaType  = [(Class, UniType)]"

		-- The [TyVarTemplate] is in the same order as the args of the
		-- TyCon for the constructor

  | TupleConId	Int		-- Its arity

#ifdef DPH
  | ProcessorCon Int		-- Its arity
#endif {- Data Parallel Haskell -}

  ---------------- Things to do with overloading

  | SuperDictSelId		-- Selector for superclass dictionary
		Class		-- The class (input dict)
		Class		-- The superclass (result dict)

  | ClassOpId	Class		-- An overloaded class operation, with
				-- a fully polymorphic type.  Its code
				-- just selects a method from the
				-- dictionary.  The class.
		ClassOp		-- The operation

	-- NB: The IdInfo for a ClassOpId has all the info about its
	-- related "constant method Ids", which are just
	-- specialisations of this general one.

  | DefaultMethodId		-- Default method for a particular class op
		Class		-- same class, <blah-blah> info as ClassOpId
		ClassOp		-- (surprise, surprise)
		Bool		-- True <=> I *know* this default method Id
				-- is a generated one that just says
				-- `error "No default method for <op>"'.
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

\begin{code}
  | DictFunId	Class		-- A DictFun is uniquely identified
		UniType		-- by its class and type; this type has free type vars,
				-- whose identity is irrelevant.  Eg Class = Eq
				--				     Type  = Tree a
				-- The "a" is irrelevant.  As it is too painful to
				-- actually do comparisons that way, we kindly supply
				-- a Unique for that purpose.
		Bool		-- True <=> from an instance decl in this mod
		FAST_STRING	-- module where instance came from
\end{code}

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

\begin{code}
  | ConstMethodId		-- A method which depends only on the type of the
				-- instance, and not on any further dictionaries etc.
		Class		-- Uniquely identified by:
		UniType		-- (class, type, classop) triple
		ClassOp
		Bool		-- True <=> from an instance decl in this mod
		FAST_STRING	-- module where instance came from

  | InstId	Inst		-- An instance of a dictionary, class operation,
				-- or overloaded value

  | SpecId			-- A specialisation of another Id
		Id		-- Id of which this is a specialisation
		[Maybe UniType]	-- Types at which it is specialised;
				-- A "Nothing" says this type ain't relevant.
		Bool		-- True <=> no free type vars; it's not enough
				-- to know about the unspec version, because
				-- we may specialise to a type w/ free tyvars
				-- (i.e., in one of the "Maybe UniType" dudes).

  | WorkerId			-- A "worker" for some other Id
		Id		-- Id for which this is a worker

#ifdef DPH
  | PodId	Int		-- The dimension of the PODs context
		Int		-- Which specialisation of InfoType is
				-- bind. ToDo(hilly): Int is a little messy
				-- and has a restricted range---change.
		Id		-- One of the aboves Ids.
#endif {- Data Parallel Haskell -}

type ConTag	= Int
type DictVar	= Id
type DictFun	= Id
type DataCon	= Id
\end{code}

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
know: (1)~a value's {\em type} (@getIdUniType@ is a very common
operation in the compiler); and (2)~what ``flavour'' of value it might
be---for example, it can be terribly useful to know that a value is a
class method.

\begin{description}
%----------------------------------------------------------------------
\item[@DataConId@:] For the data constructors declared by a @data@
declaration.  Their type is kept in {\em two} forms---as a regular
@UniType@ (in the usual place), and also in its constituent pieces (in
the ``details''). We are frequently interested in those pieces.

%----------------------------------------------------------------------
\item[@TupleConId@:] This is just a special shorthand for @DataCons@ for
the infinite family of tuples.

%----------------------------------------------------------------------
\item[@ImportedId@:] These are values defined outside this module.
{\em Everything} we want to know about them must be stored here (or in
their @IdInfo@).

%----------------------------------------------------------------------
\item[@PreludeId@:] ToDo

%----------------------------------------------------------------------
\item[@TopLevId@:] These are values defined at the top-level in this
module; i.e., those which {\em might} be exported (hence, a
@FullName@).  It does {\em not} include those which are moved to the
top-level through program transformations.

We also guarantee that @TopLevIds@ will {\em stay} at top-level.
Theoretically, they could be floated inwards, but there's no known
advantage in doing so.	This way, we can keep them with the same
@Unique@ throughout (no cloning), and, in general, we don't have to be
so paranoid about them.

In particular, we had the following problem generating an interface:
We have to ``stitch together'' info (1)~from the typechecker-produced
global-values list (GVE) and (2)~from the STG code [which @Ids@ have
what arities].	If the @Uniques@ on the @TopLevIds@ can {\em change}
between (1) and (2), you're sunk!

%----------------------------------------------------------------------
\item[@ClassOpId@:] A selector from a dictionary; it may select either
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
\item[@InstId@:]

%----------------------------------------------------------------------
\item[@SpecId@:]

%----------------------------------------------------------------------
\item[@WorkerId@:]

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

@DataCons@ @TupleCons@, @Importeds@, @TopLevIds@, @SuperDictSelIds@,
@ClassOpIds@, @DictFunIds@, and @DefaultMethodIds@ have the following
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
Note that @InstIds@, @Locals@ and @SysLocals@ {\em may} have the above
properties, but they may not.
\end{enumerate}


%************************************************************************
%*									*
\subsection[Id-general-funs]{General @Id@-related functions}
%*									*
%************************************************************************

\begin{code}
isDataCon (Id _ _ _ (DataConId _ _ _ _ _ _)) = True
isDataCon (Id _ _ _ (TupleConId _))	     = True
isDataCon (Id _ _ _ (SpecId unspec _ _))     = isDataCon unspec
#ifdef DPH
isDataCon (ProcessorCon _ _)	      = True
isDataCon (PodId _ _ id )	      = isDataCon id
#endif {- Data Parallel Haskell -}
isDataCon other			      = False

isTupleCon (Id _ _ _ (TupleConId _))	    = True
isTupleCon (Id _ _ _ (SpecId unspec _ _))   = isTupleCon unspec
#ifdef DPH
isTupleCon (PodId _ _ id)	= isTupleCon id
#endif {- Data Parallel Haskell -}
isTupleCon other		= False

isNullaryDataCon data_con
  =  isDataCon data_con
  && (case arityMaybe (getIdArity data_con) of
	Just a -> a == 0
	_      -> panic "isNullaryDataCon")

isSpecId_maybe (Id _ _ _ (SpecId unspec ty_maybes _))
  = ASSERT(not (maybeToBool (isSpecId_maybe unspec)))
    Just (unspec, ty_maybes)
isSpecId_maybe other_id
  = Nothing

isSpecPragmaId_maybe (Id _ _ _ (SpecPragmaId _ specinfo _))
  = Just specinfo
isSpecPragmaId_maybe other_id
  = Nothing

#ifdef DPH
isProcessorCon (ProcessorCon _ _) = True
isProcessorCon (PodId _ _ id)	  = isProcessorCon id
isProcessorCon other		  = False
#endif {- Data Parallel Haskell -}
\end{code}

@toplevelishId@ tells whether an @Id@ {\em may} be defined in a
nested @let(rec)@ (returns @False@), or whether it is {\em sure} to be
defined at top level (returns @True@).	This is used to decide whether
the @Id@ is a candidate free variable.	NB: you are only {\em sure}
about something if it returns @True@!

\begin{code}
toplevelishId	    :: Id -> Bool
idHasNoFreeTyVars   :: Id -> Bool

toplevelishId (Id _ _ _ details)
  = chk details
  where
    chk (DataConId _ _ _ _ _ _) = True
    chk (TupleConId _)	    	= True
    chk (ImportedId _)	    	= True
    chk (PreludeId  _)	    	= True
    chk (TopLevId   _)	    	= True	-- NB: see notes
    chk (SuperDictSelId _ _)	= True
    chk (ClassOpId _ _)		= True
    chk (DefaultMethodId _ _ _) = True
    chk (DictFunId     _ _ _ _)	= True
    chk (ConstMethodId _ _ _ _ _) = True
    chk (SpecId unspec _ _)	= toplevelishId unspec
				  -- depends what the unspecialised thing is
    chk (WorkerId unwrkr)	= toplevelishId unwrkr
    chk (InstId _)		= False	-- these are local
    chk (LocalId      _ _)	= False
    chk (SysLocalId   _ _)	= False
    chk (SpecPragmaId _ _ _)	= False
#ifdef DPH
    chk (ProcessorCon _ _)	= True
    chk (PodId _ _ id)		= toplevelishId id
#endif {- Data Parallel Haskell -}

idHasNoFreeTyVars (Id _ _ info details)
  = chk details
  where
    chk (DataConId _ _ _ _ _ _) = True
    chk (TupleConId _)	    	= True
    chk (ImportedId _)	    	= True
    chk (PreludeId  _)	    	= True
    chk (TopLevId   _)	    	= True
    chk (SuperDictSelId _ _)	= True
    chk (ClassOpId _ _)		= True
    chk (DefaultMethodId _ _ _) = True
    chk (DictFunId     _ _ _ _)	= True
    chk (ConstMethodId _ _ _ _ _) = True
    chk (WorkerId unwrkr)	= idHasNoFreeTyVars unwrkr
    chk (InstId _)		      = False	-- these are local
    chk (SpecId _     _   no_free_tvs) = no_free_tvs
    chk (LocalId      _   no_free_tvs) = no_free_tvs
    chk (SysLocalId   _   no_free_tvs) = no_free_tvs
    chk (SpecPragmaId _ _ no_free_tvs) = no_free_tvs
#ifdef DPH
    chk (ProcessorCon _ _)	= True
    chk (PodId _ _ id)		= idHasNoFreeTyVars id
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
isTopLevId (Id _ _ _ (TopLevId _)) = True
#ifdef DPH
isTopLevId (PodId _ _ id)	= isTopLevId id
#endif {- Data Parallel Haskell -}
isTopLevId other		= False

-- an "invented" one is a top-level Id, must be globally visible, etc.,
-- but it's slightly different in that it was "conjured up".
-- This handles workers fine, but may need refinement for other
-- conjured-up things (e.g., specializations)
-- NB: Only used in DPH now (93/08/20)

#ifdef DPH
ToDo: DPH
isInventedTopLevId (TopLevId _ n _ _)	= isInventedFullName n
isInventedTopLevId (SpecId _ _ _)	= True
isInventedTopLevId (WorkerId _)		= True
isInventedTopLevId (PodId _ _ id)	= isInventedTopLevId id
isInventedTopLevId other		= False
#endif {- Data Parallel Haskell -}

isImportedId (Id _ _ _ (ImportedId _))   = True
#ifdef DPH
isImportedId (PodId _ _ id)	  = isImportedId id
#endif {- Data Parallel Haskell -}
isImportedId other		  = False

isBottomingId (Id _ _ info _) = bottomIsGuaranteed (getInfo info)
#ifdef DPH
isBottomingId (PodId _ _ id)	  = isBottomingId id
#endif {- Data Parallel Haskell -}
--isBottomingId other		  = False

isSysLocalId (Id _ _ _ (SysLocalId _ _)) = True
#ifdef DPH
isSysLocalId (PodId _ _ id)	= isSysLocalId id
#endif {- Data Parallel Haskell -}
isSysLocalId other		= False

isSpecPragmaId (Id _ _ _ (SpecPragmaId _ _ _)) = True
#ifdef DPH
isSpecPragmaId (PodId _ _ id)	= isSpecPragmaId id
#endif {- Data Parallel Haskell -}
isSpecPragmaId other		= False

isClassOpId (Id _ _ _ (ClassOpId _ _)) = True
isClassOpId _ = False

isDefaultMethodId_maybe (Id _ _ _ (DefaultMethodId cls clsop err)) = Just (cls, clsop, err)
#ifdef DPH
isDefaultMethodId_maybe (PodId _ _ id)	= isDefaultMethodId_maybe id
#endif {- Data Parallel Haskell -}
isDefaultMethodId_maybe other		= Nothing

isDictFunId (Id _ _ _ (DictFunId _ _ _ _)) = True
#ifdef DPH
isDictFunId (PodId _ _ id)	    	= isDictFunId id
#endif {- Data Parallel Haskell -}
isDictFunId other		    	= False

isConstMethodId_maybe (Id _ _ _ (ConstMethodId cls ty clsop _ _)) = Just (cls, ty, clsop)
#ifdef DPH
isConstMethodId_maybe (PodId _ _ id)	= isConstMethodId_maybe id
#endif {- Data Parallel Haskell -}
isConstMethodId_maybe other		= Nothing

isInstId_maybe (Id _ _ _ (InstId inst)) = Just inst
#ifdef DPH
isInstId_maybe (PodId _ _ id)	        = isInstId_maybe id
#endif {- Data Parallel Haskell -}
isInstId_maybe other_id			= Nothing

isSuperDictSelId_maybe (Id _ _ _ (SuperDictSelId c sc)) = Just (c, sc)
#ifdef DPH
isSuperDictSelId_maybe (PodId _ _ id)	= isSuperDictSelId_maybe id
#endif {- Data Parallel Haskell -}
isSuperDictSelId_maybe other_id		= Nothing

isWorkerId (Id _ _ _ (WorkerId _)) = True
#ifdef DPH
isWorkerId (PodId _ _ id)	    = isWorkerId id
#endif {- Data Parallel Haskell -}
isWorkerId other		    = False

isWrapperId id = workerExists (getIdStrictness id)
\end{code}

\begin{code}
pprIdInUnfolding :: IdSet -> Id -> Pretty

pprIdInUnfolding in_scopes v
  = let
	v_ty = getIdUniType v
    in
    -- local vars first:
    if v `elementOfUniqSet` in_scopes then
	pprUnique (getTheUnique v)

    -- ubiquitous Ids with special syntax:
    else if v == nilDataCon then
	ppPStr SLIT("_NIL_")
    else if isTupleCon v then
	ppBeside (ppPStr SLIT("_TUP_")) (ppInt (getDataConArity v))

    -- ones to think about:
    else
	let
	    (Id _ _ _ v_details) = v
	in
    	case v_details of
	    -- these ones must have been exported by their original module
	  ImportedId   _ -> pp_full_name
	  PreludeId    _ -> pp_full_name

	    -- these ones' exportedness checked later...
	  TopLevId  _ -> pp_full_name
	  DataConId _ _ _ _ _ _ -> pp_full_name

	    -- class-ish things: class already recorded as "mentioned"
	  SuperDictSelId c sc
	    -> ppCat [ppPStr SLIT("_SDSEL_"), pp_class c, pp_class sc]
	  ClassOpId c o
	    -> ppCat [ppPStr SLIT("_METH_"), pp_class c, pp_class_op o]
	  DefaultMethodId c o _
	    -> ppCat [ppPStr SLIT("_DEFM_"), pp_class c, pp_class_op o]

	    -- instance-ish things: should we try to figure out
	    -- *exactly* which extra instances have to be exported? (ToDo)
	  DictFunId  c t _ _
	    -> ppCat [ppPStr SLIT("_DFUN_"), pp_class c, pp_type t]
	  ConstMethodId c t o _ _
	    -> ppCat [ppPStr SLIT("_CONSTM_"), pp_class c, pp_class_op o, pp_type t]

	  -- specialisations and workers
	  SpecId unspec ty_maybes _
	    -> let
		  pp = pprIdInUnfolding in_scopes unspec
	       in
	       ppCat [ppPStr SLIT("_SPEC_"), pp, ppLbrack,
			ppIntersperse pp'SP{-'-} (map pp_ty_maybe ty_maybes),
			ppRbrack]

	  WorkerId unwrkr
	    -> let
		  pp = pprIdInUnfolding in_scopes unwrkr
	       in
	       ppBeside (ppPStr SLIT("_WRKR_ ")) pp

	  -- anything else? we're nae interested
	  other_id -> panic "pprIdInUnfolding:mystery Id"
  where
    ppr_Unfolding = PprUnfolding (panic "Id:ppr_Unfolding")

    pp_full_name
      = let
	    (m_str, n_str) = getOrigName v

	    pp_n =
	      if isAvarop n_str || isAconop n_str then
		  ppBesides [ppLparen, ppPStr n_str, ppRparen]
	      else
		  ppPStr n_str
	in
	if fromPreludeCore v then
	    pp_n
	else
	    ppCat [ppPStr SLIT("_ORIG_"), ppPStr m_str, pp_n]

    pp_class :: Class -> Pretty
    pp_class_op :: ClassOp -> Pretty
    pp_type :: UniType -> Pretty
    pp_ty_maybe :: Maybe UniType -> Pretty

    pp_class    clas = ppr ppr_Unfolding clas
    pp_class_op op   = ppr ppr_Unfolding op

    pp_type t = ppBesides [ppLparen, ppr ppr_Unfolding t, ppRparen]

    pp_ty_maybe Nothing  = ppPStr SLIT("_N_")
    pp_ty_maybe (Just t) = pp_type t
\end{code}

@whatsMentionedInId@ ferrets out the types/classes/instances on which
this @Id@ depends.  If this Id is to appear in an interface, then
those entities had Jolly Well be in scope.  Someone else up the
call-tree decides that.

\begin{code}
whatsMentionedInId
	:: IdSet			    -- Ids known to be in scope
	-> Id				    -- Id being processed
	-> (Bag Id, Bag TyCon, Bag Class)   -- mentioned Ids/TyCons/etc.

whatsMentionedInId in_scopes v
  = let
	v_ty = getIdUniType v

    	(tycons, clss)
	  = getMentionedTyConsAndClassesFromUniType v_ty

	result0 id_bag = (id_bag, tycons, clss)

	result1 ids tcs cs
	  = (ids `unionBags` unitBag v,	-- we add v to "mentioned"...
	     tcs `unionBags` tycons,
	     cs  `unionBags` clss)
    in
    -- local vars first:
    if v `elementOfUniqSet` in_scopes then
	result0 emptyBag    -- v not added to "mentioned"

    -- ones to think about:
    else
	let
	    (Id _ _ _ v_details) = v
	in
    	case v_details of
	  -- specialisations and workers
	  SpecId unspec ty_maybes _
	    -> let
		  (ids2, tcs2, cs2) = whatsMentionedInId in_scopes unspec
	       in
	       result1 ids2 tcs2 cs2

	  WorkerId unwrkr
	    -> let
		  (ids2, tcs2, cs2) = whatsMentionedInId in_scopes unwrkr
	       in
	       result1 ids2 tcs2 cs2

	  anything_else -> result0 (unitBag v) -- v is  added to "mentioned"
\end{code}

Tell them who my wrapper function is.
\begin{code}
myWrapperMaybe :: Id -> Maybe Id

myWrapperMaybe (Id _ _ _ (WorkerId my_wrapper)) = Just my_wrapper
myWrapperMaybe other_id			    	= Nothing
\end{code}

\begin{code}
unfoldingUnfriendlyId	-- return True iff it is definitely a bad
	:: Id		-- idea to export an unfolding that
	-> Bool		-- mentions this Id.  Reason: it cannot
			-- possibly be seen in another module.

unfoldingUnfriendlyId id
  | not (externallyVisibleId id) -- that settles that...
  = True

unfoldingUnfriendlyId (Id _ _ _ (WorkerId wrapper))
  = class_thing wrapper
  where
    -- "class thing": If we're going to use this worker Id in
    -- an interface, we *have* to be able to untangle the wrapper's
    -- strictness when reading it back in.  At the moment, this
    -- is not always possible: in precisely those cases where
    -- we pass tcGenPragmas a "Nothing" for its "ty_maybe".

    class_thing (Id _ _ _ (SuperDictSelId _ _))    = True
    class_thing (Id _ _ _ (ClassOpId _ _))  	   = True
    class_thing (Id _ _ _ (DefaultMethodId _ _ _)) = True
    class_thing other				   = False

unfoldingUnfriendlyId (Id _ _ _ (SpecId d@(Id _ _ _ dfun@(DictFunId _ t _ _)) _ _))
    -- a SPEC of a DictFunId can end up w/ gratuitous
    -- TyVar(Templates) in the i/face; only a problem
    -- if -fshow-pragma-name-errs; but we can do without the pain.
    -- A HACK in any case (WDP 94/05/02)
  = --pprTrace "unfriendly1:" (ppCat [ppr PprDebug d, ppr PprDebug t]) (
    naughty_DictFunId dfun
    --)

unfoldingUnfriendlyId d@(Id _ _ _ dfun@(DictFunId _ t _ _))
  = --pprTrace "unfriendly2:" (ppCat [ppr PprDebug d, ppr PprDebug t]) (
    naughty_DictFunId dfun -- similar deal...
    --)

unfoldingUnfriendlyId other_id   = False -- is friendly in all other cases

naughty_DictFunId :: IdDetails -> Bool
    -- True <=> has a TyVar(Template) in the "type" part of its "name"

naughty_DictFunId (DictFunId _ _ False _) = False -- came from outside; must be OK
naughty_DictFunId (DictFunId _ ty _ _)
  = not (isGroundTy ty)
\end{code}

@externallyVisibleId@: is it true that another module might be
able to ``see'' this Id?

We need the @toplevelishId@ check as well as @isExported@ for when we
compile instance declarations in the prelude.  @DictFunIds@ are
``exported'' if either their class or tycon is exported, but, in
compiling the prelude, the compiler may not recognise that as true.

\begin{code}
externallyVisibleId :: Id -> Bool

externallyVisibleId id@(Id _ _ _ details)
  = if isLocallyDefined id then
	toplevelishId id && isExported id && not (weird_datacon details)
    else
	not (weird_tuplecon details)
	-- if visible here, it must be visible elsewhere, too.
  where
    -- If it's a DataCon, it's not enough to know it (meaning
    -- its TyCon) is exported; we need to know that it might
    -- be visible outside.  Consider:
    --
    --	data Foo a = Mumble | BigFoo a WeirdLocalType
    --
    -- We can't tell the outside world *anything* about Foo, because
    -- of WeirdLocalType; but we need to know this when asked if
    -- "Mumble" is externally visible...

    weird_datacon (DataConId _ _ _ _ _ tycon)
      = maybeToBool (maybePurelyLocalTyCon tycon)
    weird_datacon not_a_datacon_therefore_not_weird = False

    weird_tuplecon (TupleConId arity)
      = arity > 32 -- sigh || isBigTupleTyCon tycon -- generated *purely* for local use
    weird_tuplecon _ = False
\end{code}

\begin{code}
idWantsToBeINLINEd :: Id -> Bool

idWantsToBeINLINEd id
  = case (getIdUnfolding id) of
      IWantToBeINLINEd _ -> True
      _ -> False
\end{code}

For @unlocaliseId@: See the brief commentary in
\tr{simplStg/SimplStg.lhs}.

\begin{code}
unlocaliseId :: FAST_STRING{-modulename-} -> Id -> Maybe Id

unlocaliseId mod (Id u ty info (TopLevId fn))
  = Just (Id u ty info (TopLevId (unlocaliseFullName fn)))

unlocaliseId mod (Id u ty info (LocalId sn no_ftvs))
  = --false?: ASSERT(no_ftvs)
    let
	full_name = unlocaliseShortName mod u sn
    in
    Just (Id u ty info (TopLevId full_name))

unlocaliseId mod (Id u ty info (SysLocalId sn no_ftvs))
  = --false?: on PreludeGlaST: ASSERT(no_ftvs)
    let
	full_name = unlocaliseShortName mod u sn
    in
    Just (Id u ty info (TopLevId full_name))

unlocaliseId mod (Id u ty info (SpecId unspec ty_maybes no_ftvs))
  = case unlocalise_parent mod u unspec of
      Nothing -> Nothing
      Just xx -> Just (Id u ty info (SpecId xx ty_maybes no_ftvs))

unlocaliseId mod (Id u ty info (WorkerId unwrkr))
  = case unlocalise_parent mod u unwrkr of
      Nothing -> Nothing
      Just xx -> Just (Id u ty info (WorkerId xx))

unlocaliseId mod (Id u ty info (InstId inst))
  = Just (Id u ty info (TopLevId full_name))
	-- type might be wrong, but it hardly matters
	-- at this stage (just before printing C)  ToDo
  where
    name = let	(bit1:bits) = getInstNamePieces True inst  in
	   _CONCAT_ (bit1 : [ _CONS_ '.'  b | b <- bits ])

    full_name = mkFullName mod (mod _APPEND_ name) InventedInThisModule ExportAll mkGeneratedSrcLoc

#ifdef DPH
unlocaliseId mod (PodId dim ity id)
  = case (unlocaliseId mod id) of
      Just id' -> Just (PodId dim ity id')
      Nothing  -> Nothing
#endif {- Data Parallel Haskell -}

unlocaliseId mod other_id = Nothing

--------------------
-- we have to be Very Careful for workers/specs of
-- local functions!

unlocalise_parent mod uniq (Id _ ty info (LocalId sn no_ftvs))
  = --false?: ASSERT(no_ftvs)
    let
	full_name = unlocaliseShortName mod uniq sn
    in
    Just (Id uniq ty info (TopLevId full_name))

unlocalise_parent mod uniq (Id _ ty info (SysLocalId sn no_ftvs))
  = --false?: ASSERT(no_ftvs)
    let
	full_name = unlocaliseShortName mod uniq sn
    in
    Just (Id uniq ty info (TopLevId full_name))

unlocalise_parent mod uniq other_id = unlocaliseId mod other_id
  -- we're OK otherwise
\end{code}

CLAIM (not ASSERTed) for @applyTypeEnvToId@ and @applySubstToId@:
`Top-levelish Ids'' cannot have any free type variables, so applying
the type-env cannot have any effect.  (NB: checked in CoreLint?)

The special casing is in @applyTypeEnvToId@, not @apply_to_Id@, as the
former ``should be'' the usual crunch point.

\begin{code}
applyTypeEnvToId :: TypeEnv -> Id -> Id

applyTypeEnvToId type_env id@(Id u ty info details)
  | idHasNoFreeTyVars id
  = id
  | otherwise
  = apply_to_Id ( \ ty ->
	applyTypeEnvToTy type_env ty
    ) id
\end{code}

\begin{code}
apply_to_Id :: (UniType -> UniType)
	    -> Id
	    -> Id

apply_to_Id ty_fn (Id u ty info details)
  = Id u (ty_fn ty) (apply_to_IdInfo ty_fn info) (apply_to_details details)
  where
    apply_to_details (InstId inst)
      = let
	    new_inst = apply_to_Inst ty_fn inst
	in
	InstId new_inst

    apply_to_details (SpecId unspec ty_maybes no_ftvs)
      = let
	    new_unspec = apply_to_Id ty_fn unspec
	    new_maybes = map apply_to_maybe ty_maybes
	in
	SpecId new_unspec new_maybes no_ftvs
	-- ToDo: recalc no_ftvs????
      where
	apply_to_maybe Nothing   = Nothing
	apply_to_maybe (Just ty) = Just (ty_fn ty)

    apply_to_details (WorkerId unwrkr)
      = let
	    new_unwrkr = apply_to_Id ty_fn unwrkr
	in
	WorkerId new_unwrkr

#ifdef DPH
    apply_to_details (PodId d ity id )
      = PodId d ity (apply_to_Id ty_fn id)
#endif {- Data Parallel Haskell -}

    apply_to_details other = other
\end{code}

Sadly, I don't think the one using the magic typechecker substitution
can be done with @apply_to_Id@.  Here we go....

Strictness is very important here.  We can't leave behind thunks
with pointers to the substitution: it {\em must} be single-threaded.

\begin{code}
applySubstToId :: Subst -> Id -> (Subst, Id)

applySubstToId subst id@(Id u ty info details)
  -- *cannot* have a "idHasNoFreeTyVars" get-out clause
  -- because, in the typechecker, we are still
  -- *concocting* the types.
  = case (applySubstToTy     subst ty)		of { (s2, new_ty)      ->
    case (applySubstToIdInfo s2    info)	of { (s3, new_info)    ->
    case (apply_to_details   s3 new_ty details) of { (s4, new_details) ->
    (s4, Id u new_ty new_info new_details) }}}
  where
    apply_to_details subst _ (InstId inst)
      = case (applySubstToInst subst inst) of { (s2, new_inst) ->
	(s2, InstId new_inst) }

    apply_to_details subst new_ty (SpecId unspec ty_maybes _)
      = case (applySubstToId subst unspec)  	     of { (s2, new_unspec) ->
        case (mapAccumL apply_to_maybe s2 ty_maybes) of { (s3, new_maybes) ->
	(s3, SpecId new_unspec new_maybes (no_free_tvs new_ty)) }}
	-- NB: recalc no_ftvs (I think it's necessary (?) WDP 95/04)
      where
	apply_to_maybe subst Nothing   = (subst, Nothing)
	apply_to_maybe subst (Just ty)
	  = case (applySubstToTy subst ty) of { (s2, new_ty) ->
	    (s2, Just new_ty) }

    apply_to_details subst _ (WorkerId unwrkr)
      = case (applySubstToId subst unwrkr) of { (s2, new_unwrkr) ->
        (s2, WorkerId new_unwrkr) }

    apply_to_details subst _ other = (subst, other)

#ifdef DPH
applySubstToId (PodId d ity id )
  = ???? ToDo:DPH; not sure what! returnLft (PodId d ity (applySubstToId id))
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
getIdNamePieces :: Bool {-show Uniques-} -> Id -> [FAST_STRING]

getIdNamePieces show_uniqs (Id u ty info details)
  = case details of
      DataConId n _ _ _ _ _ ->
	case (getOrigName n) of { (mod, name) ->
	if fromPrelude mod then [name] else [mod, name] }

      TupleConId a -> [SLIT("Tup") _APPEND_ (_PK_ (show a))]

      ImportedId  n -> get_fullname_pieces n
      PreludeId   n -> get_fullname_pieces n
      TopLevId    n -> get_fullname_pieces n

      SuperDictSelId c sc ->
	case (getOrigName c)	of { (c_mod, c_name) ->
	case (getOrigName sc)	of { (sc_mod, sc_name) ->
	let
	    c_bits = if fromPreludeCore c
		     then [c_name]
		     else [c_mod, c_name]

	    sc_bits= if fromPreludeCore sc
		     then [sc_name]
		     else [sc_mod, sc_name]
	in
	[SLIT("sdsel")] ++ c_bits ++ sc_bits  }}

      ClassOpId clas op ->
	case (getOrigName clas)	of { (c_mod, c_name) ->
	case (getClassOpString op)	of { op_name ->
	if fromPreludeCore clas then [op_name] else [c_mod, c_name, op_name]
	} }

      DefaultMethodId clas op _ ->
	case (getOrigName clas)		of { (c_mod, c_name) ->
	case (getClassOpString op)	of { op_name ->
	if fromPreludeCore clas
	then [SLIT("defm"), op_name]
	else [SLIT("defm"), c_mod, c_name, op_name] }}

      DictFunId c ty _ _ ->
	case (getOrigName c)	    of { (c_mod, c_name) ->
	let
	    c_bits = if fromPreludeCore c
		     then [c_name]
		     else [c_mod, c_name]
	
	    ty_bits = getTypeString ty
	in
	[SLIT("dfun")] ++ c_bits ++ ty_bits }


      ConstMethodId c ty o _ _ ->
	case (getOrigName c)	    of { (c_mod, c_name) ->
	case (getTypeString ty)	    of { ty_bits ->
	case (getClassOpString o)   of { o_name ->
	case (if fromPreludeCore c
		then []
		else [c_mod, c_name])	of { c_bits ->
	[SLIT("const")] ++ c_bits ++ ty_bits ++ [o_name] }}}}

      -- if the unspecialised equiv is "top-level",
      -- the name must be concocted from its name and the
      -- names of the types to which specialised...

      SpecId unspec ty_maybes _ ->
	getIdNamePieces show_uniqs unspec ++ (
	if not (toplevelishId unspec)
	then [showUnique u]
	else concat (map typeMaybeString ty_maybes)
	)

      WorkerId unwrkr ->
	getIdNamePieces show_uniqs unwrkr ++ (
	if not (toplevelishId unwrkr)
	then [showUnique u]
	else [SLIT("wrk")]    -- show u
	)

      InstId    inst     -> getInstNamePieces show_uniqs inst
      LocalId      n _   -> let local = getLocalName n in
		            if show_uniqs then [local, showUnique u] else [local]
      SysLocalId   n _   -> [getLocalName n, showUnique u]
      SpecPragmaId n _ _ -> [getLocalName n, showUnique u]

#ifdef DPH
      ProcessorCon a _ -> ["MkProcessor" ++ (show a)]
      PodId n ity id   -> getIdNamePieces show_uniqs id ++
			   ["mapped", "POD" ++ (show n), show ity]
#endif {- Data Parallel Haskell -}

get_fullname_pieces :: FullName -> [FAST_STRING]
get_fullname_pieces n
  = BIND (getOrigName n) _TO_ (mod, name) ->
    if fromPrelude mod
    then [name]
    else [mod, name]
    BEND
\end{code}

Really Inst-ish, but only used in this module...
\begin{code}
getInstNamePieces :: Bool -> Inst -> [FAST_STRING]

getInstNamePieces show_uniqs (Dict   u clas ty _)
  = let  (mod, nm) = getOrigName clas  in
    if fromPreludeCore clas
    then [SLIT("d"), nm, showUnique u]
    else [SLIT("d"), mod, nm, showUnique u]
    
getInstNamePieces show_uniqs (Method u id tys  _)
  = let local = getIdNamePieces show_uniqs id in
    if show_uniqs then local ++ [showUnique u] else local

getInstNamePieces show_uniqs (LitInst u _ _ _) = [SLIT("lit"), showUnique u]
\end{code}

%************************************************************************
%*									*
\subsection[Id-type-funs]{Type-related @Id@ functions}
%*									*
%************************************************************************

\begin{code}
getIdUniType :: Id -> UniType

getIdUniType (Id _ ty _ _) = ty

#ifdef DPH
-- ToDo: DPH
getIdUniType (ProcessorCon _ ty)	= ty
getIdUniType (PodId d ity id)
  = let (foralls,rho) = splitForalls (getIdUniType id)		in
    let tys	      = get_args rho				in
    let itys_mask     = infoTypeNumToMask ity			in
    let tys'	      = zipWith convert tys itys_mask		in
    mkForallTy foralls (foldr1 mkFunTy tys')
  where	-- ToDo(hilly) change to use getSourceType etc...

    get_args ty = case (maybeUnpackFunTy ty) of
		    Nothing	   -> [ty]
		    Just (arg,res) -> arg:get_args res

    convert ty cond = if cond
		      then ty
		      else (coerce ty)

    coerce ty = case (maybeUnpackFunTy ty) of
		  Nothing	 ->mkPodizedPodNTy d ty
		  Just (arg,res) ->mkFunTy (coerce arg) (coerce res)
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
getMentionedTyConsAndClassesFromId :: Id -> (Bag TyCon, Bag Class)

getMentionedTyConsAndClassesFromId id
 = getMentionedTyConsAndClassesFromUniType (getIdUniType id)
\end{code}

\begin{code}
getIdKind i = kindFromType (getIdUniType i)
\end{code}

\begin{code}
getInstIdModule (Id _ _ _ (DictFunId _ _ _ mod)) = mod
getInstIdModule (Id _ _ _ (ConstMethodId _ _ _ _ mod)) = mod
getInstIdModule other = panic "Id:getInstIdModule"
\end{code}


\begin{code}
{- NOT USED 
getIdTauType :: Id -> TauType
getIdTauType i = expandTySyn (getTauType (getIdUniType i))

getIdSourceTypes :: Id -> [TauType]
getIdSourceTypes i = map expandTySyn (sourceTypes (getTauType (getIdUniType i)))

getIdTargetType :: Id -> TauType
getIdTargetType i = expandTySyn (targetType (getTauType (getIdUniType i)))
-}
\end{code}

%************************************************************************
%*									*
\subsection[Id-overloading]{Functions related to overloading}
%*									*
%************************************************************************

\begin{code}
mkSuperDictSelId  u c sc     ty info = Id u ty info (SuperDictSelId c sc)
mkClassOpId       u c op     ty info = Id u ty info (ClassOpId c op)
mkDefaultMethodId u c op gen ty info = Id u ty info (DefaultMethodId c op gen)

mkDictFunId u c ity full_ty from_here modname info
  = Id u full_ty info (DictFunId c ity from_here modname)

mkConstMethodId	u c op ity full_ty from_here modname info
  = Id u full_ty info (ConstMethodId c ity op from_here modname)

mkWorkerId u unwrkr ty info = Id u ty info (WorkerId unwrkr)

mkInstId inst
  = Id u (getInstUniType inst) noIdInfo (InstId inst)
  where
    u = case inst of
	  Dict	  u c t o   -> u
	  Method  u i ts o  -> u
	  LitInst u l ty o  -> u

{- UNUSED:
getSuperDictSelIdSig (Id _ _ _ (SuperDictSelId input_class result_class))
  = (input_class, result_class)
-}
\end{code}

%************************************************************************
%*									*
\subsection[local-funs]{@LocalId@-related functions}
%*									*
%************************************************************************

\begin{code}
mkImported    u n ty info = Id u ty info (ImportedId   n)
mkPreludeId   u n ty info = Id u ty info (PreludeId    n)

#ifdef DPH
mkPodId d i = PodId d i
#endif

updateIdType :: Id -> UniType -> Id
updateIdType (Id u _ info details) ty = Id u ty info details
\end{code}

\begin{code}
no_free_tvs ty = null (extractTyVarsFromTy ty)

-- SysLocal: for an Id being created by the compiler out of thin air...
-- UserLocal: an Id with a name the user might recognize...
mkSysLocal, mkUserLocal :: FAST_STRING -> Unique -> UniType -> SrcLoc -> Id

mkSysLocal str uniq ty loc
  = Id uniq ty noIdInfo (SysLocalId (mkShortName str loc) (no_free_tvs ty))

mkUserLocal str uniq ty loc
  = Id uniq ty noIdInfo (LocalId (mkShortName str loc) (no_free_tvs ty))

-- for a SpecPragmaId being created by the compiler out of thin air...
mkSpecPragmaId :: FAST_STRING -> Unique -> UniType -> Maybe SpecInfo -> SrcLoc -> Id
mkSpecPragmaId str uniq ty specinfo loc
  = Id uniq ty noIdInfo (SpecPragmaId (mkShortName str loc) specinfo (no_free_tvs ty))

-- for new SpecId 
mkSpecId u unspec ty_maybes ty info
  = ASSERT(not (maybeToBool (isSpecId_maybe unspec)))
    Id u ty info (SpecId unspec ty_maybes (no_free_tvs ty))

-- Specialised version of constructor: only used in STG and code generation
-- Note: The specialsied Id has the same unique as the unspeced Id

mkSameSpecCon ty_maybes unspec@(Id u ty info details)
  = ASSERT(isDataCon unspec)
    ASSERT(not (maybeToBool (isSpecId_maybe unspec)))
    Id u new_ty info (SpecId unspec ty_maybes (no_free_tvs new_ty))
  where
    new_ty = specialiseTy ty ty_maybes 0

    -- pprTrace "SameSpecCon:Unique:"
    --	        (ppSep (ppr PprDebug unspec: [pprMaybeTy PprDebug ty | ty <- ty_maybes]))

-- mkId builds a local or top-level Id, depending on the name given
mkId :: Name -> UniType -> IdInfo -> Id
mkId (Short uniq short) ty info	= Id uniq ty info (LocalId short (no_free_tvs ty))
mkId (OtherTopId uniq full) ty info
  = Id uniq ty info
	(if isLocallyDefined full then TopLevId full else ImportedId full)

localiseId :: Id -> Id
localiseId id@(Id u ty info details)
  = Id u ty info (LocalId (mkShortName name loc) (no_free_tvs ty))
  where
    name = getOccurrenceName id
    loc  = getSrcLoc id

-- this has to be one of the "local" flavours (LocalId, SysLocalId, InstId)
-- ToDo: it does??? WDP
mkIdWithNewUniq :: Id -> Unique -> Id

mkIdWithNewUniq (Id _ ty info details) uniq
  = let
	new_details
	  = case details of
	      InstId (Dict _ c t o)	-> InstId (Dict uniq c t o)
	      InstId (Method _ i ts o)  -> InstId (Method uniq i ts o)
	      InstId (LitInst _ l ty o) -> InstId (LitInst uniq l ty o)
	      old_details		-> old_details
    in
    Id uniq ty info new_details

#ifdef DPH
mkIdWithNewUniq (PodId d t id) uniq = PodId d t (mkIdWithNewUniq id uniq)
#endif {- Data Parallel Haskell -}
\end{code}

Make some local @Ids@ for a template @CoreExpr@.  These have bogus
@Uniques@, but that's OK because the templates are supposed to be
instantiated before use.
\begin{code}
mkTemplateLocals :: [UniType] -> [Id]
mkTemplateLocals tys
  = zipWith (\ u -> \ ty -> mkSysLocal SLIT("tpl") u ty mkUnknownSrcLoc)
	    (getBuiltinUniques (length tys))
	    tys
\end{code}

\begin{code}
getIdInfo :: Id -> IdInfo

getIdInfo (Id _ _ info _) = info

#ifdef DPH
getIdInfo (PodId _ _ id)		= getIdInfo id
#endif {- Data Parallel Haskell -}

replaceIdInfo :: Id -> IdInfo -> Id

replaceIdInfo (Id u ty _ details) info = Id u ty info details

#ifdef DPH
replaceIdInfo (PodId dim ity id) info = PodId dim ity (replaceIdInfo id info)
#endif {- Data Parallel Haskell -}

selectIdInfoForSpecId :: Id -> IdInfo
selectIdInfoForSpecId unspec
  = ASSERT(not (maybeToBool (isSpecId_maybe unspec)))
    noIdInfo `addInfo_UF` getIdUnfolding unspec
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
getIdArity  	:: Id -> ArityInfo
getDataConArity :: DataCon -> Int -- a simpler i/face; they always have arities

#ifdef DPH
getIdArity (ProcessorCon n _)		= mkArityInfo n
getIdArity (PodId _ _ id)		= getIdArity id
#endif {- Data Parallel Haskell -}

getIdArity (Id _ _ id_info _)  = getInfo id_info

getDataConArity id@(Id _ _ id_info _)
  = ASSERT(isDataCon id)
    case (arityMaybe (getInfo id_info)) of
      Nothing -> pprPanic "getDataConArity:Nothing:" (ppr PprDebug id)
      Just  i -> i

addIdArity :: Id -> Int -> Id
addIdArity (Id u ty info details) arity
  = Id u ty (info `addInfo` (mkArityInfo arity)) details
\end{code}

%************************************************************************
%*									*
\subsection[constructor-funs]{@DataCon@-related functions (incl.~tuples)}
%*									*
%************************************************************************

\begin{code}
mkDataCon :: Unique{-DataConKey-} -> FullName -> [TyVarTemplate] -> ThetaType -> [TauType] -> TyCon -> SpecEnv -> Id
  -- can get the tag and all the pieces of the type from the UniType

mkDataCon k n tyvar_tmpls context args_tys tycon specenv = data_con
  where
    data_con  = Id k type_of_constructor datacon_info
		    (DataConId n
			(position_within fIRST_TAG data_con_family data_con)
			tyvar_tmpls context args_tys tycon)

    -- Note data_con self-recursion;
    -- should be OK as tags are not looked at until
    -- late in the game.

    data_con_family		= getTyConDataCons tycon

    position_within :: Int -> [Id] -> Id -> Int
    position_within acc [] con
      = panic "mkDataCon: con not found in family"

    position_within acc (c:cs) con
      = if c `eqId` con then acc else position_within (acc+(1::Int)) cs con

    type_of_constructor = mkSigmaTy tyvar_tmpls context
				(glueTyArgs
					args_tys
					(applyTyCon tycon (map mkTyVarTemplateTy tyvar_tmpls)))

    datacon_info = noIdInfo `addInfo_UF` unfolding
			    `addInfo` mkArityInfo arity
			    `addInfo` specenv

    arity = length args_tys

    unfolding
      = -- if arity == 0
    	-- then noIdInfo
	-- else -- do some business...
	let
	    (tyvars, dict_vars, vars) = mk_uf_bits tyvar_tmpls context args_tys tycon
	    tyvar_tys = map mkTyVarTy tyvars
	in
	BIND (CoCon data_con tyvar_tys [CoVarAtom v | v <- vars]) _TO_ plain_CoCon ->

	BIND (mkCoLam (dict_vars ++ vars) plain_CoCon) _TO_ lambdized_CoCon ->

	mkUnfolding EssentialUnfolding -- for data constructors
		    (foldr CoTyLam lambdized_CoCon tyvars)
	BEND BEND

    mk_uf_bits tyvar_tmpls context arg_tys tycon
      = let
	    (inst_env, tyvars, tyvar_tys)
	      = instantiateTyVarTemplates tyvar_tmpls 
					  (map getTheUnique tyvar_tmpls)
	in
	    -- the "context" and "arg_tys" have TyVarTemplates in them, so
	    -- we instantiate those types to have the right TyVars in them
	    -- instead.
	BIND (map (instantiateTauTy inst_env) (map ctxt_ty context))
						       	_TO_ inst_dict_tys ->
	BIND (map (instantiateTauTy inst_env) arg_tys) 	_TO_ inst_arg_tys ->

	    -- We can only have **ONE** call to mkTemplateLocals here;
	    -- otherwise, we get two blobs of locals w/ mixed-up Uniques
	    -- (Mega-Sigh) [ToDo]
	BIND (mkTemplateLocals (inst_dict_tys ++ inst_arg_tys)) _TO_ all_vars ->

	BIND (splitAt (length context) all_vars)	_TO_ (dict_vars, vars) ->

	(tyvars, dict_vars, vars)
	BEND BEND BEND BEND
      where
	-- these are really dubious UniTypes, but they are only to make the
	-- binders for the lambdas for tossed-away dicts.
	ctxt_ty (clas, ty) = mkDictTy clas ty
\end{code}

\begin{code}
mkTupleCon :: Arity -> Id

mkTupleCon arity = data_con
  where
    data_con	= Id unique ty tuplecon_info (TupleConId arity)
    unique      = mkTupleDataConUnique arity
    ty 		= mkSigmaTy tyvars [] (glueTyArgs tyvar_tys (applyTyCon tycon tyvar_tys))
    tycon	= mkTupleTyCon arity
    tyvars	= take arity alphaTyVars
    tyvar_tys	= map mkTyVarTemplateTy tyvars

    tuplecon_info
      = noIdInfo `addInfo_UF` unfolding
		 `addInfo` mkArityInfo arity
		 `addInfo` pcGenerateTupleSpecs arity ty

    unfolding
      = -- if arity == 0
    	-- then noIdInfo
	-- else -- do some business...
	let
	    (tyvars, dict_vars, vars) = mk_uf_bits arity
	    tyvar_tys = map mkTyVarTy tyvars
	in
	BIND (CoCon data_con tyvar_tys [CoVarAtom v | v <- vars]) _TO_ plain_CoCon ->

	BIND (mkCoLam (dict_vars ++ vars) plain_CoCon) _TO_ lambdized_CoCon ->

	mkUnfolding
	    EssentialUnfolding    -- data constructors
	    (foldr CoTyLam lambdized_CoCon tyvars)
	BEND BEND

    mk_uf_bits arity
      = BIND (mkTemplateLocals tyvar_tys)		 _TO_ vars ->
	(tyvars, [], vars)
	BEND
      where
	tyvar_tmpls	= take arity alphaTyVars
	(_, tyvars, tyvar_tys) = instantiateTyVarTemplates tyvar_tmpls (map getTheUnique tyvar_tmpls)


#ifdef DPH
mkProcessorCon :: Arity -> Id
mkProcessorCon arity
  = ProcessorCon arity ty
  where
    ty = mkSigmaTy tyvars [] (glueTyArgs tyvar_tys (applyTyCon tycon tyvar_tys))
    tycon	= mkProcessorTyCon arity
    tyvars	= take arity alphaTyVars
    tyvar_tys	= map mkTyVarTemplateTy tyvars
#endif {- Data Parallel Haskell -}

fIRST_TAG :: ConTag
fIRST_TAG =  1	-- Tags allocated from here for real constructors

-- given one data constructor in a family, return a list
-- of all the data constructors in that family.

#ifdef DPH
getDataConFamily :: DataCon -> [DataCon]

getDataConFamily data_con
  = ASSERT(isDataCon data_con)
    getTyConDataCons (getDataConTyCon data_con)
#endif
\end{code}

\begin{code}
getDataConTag :: DataCon -> ConTag	-- will panic if not a DataCon

getDataConTag	(Id _ _ _ (DataConId _ tag _ _ _ _)) = tag
getDataConTag	(Id _ _ _ (TupleConId _))	     = fIRST_TAG
getDataConTag	(Id _ _ _ (SpecId unspec _ _))	     = getDataConTag unspec
#ifdef DPH
getDataConTag	(ProcessorCon _ _) = fIRST_TAG
#endif {- Data Parallel Haskell -}

getDataConTyCon :: DataCon -> TyCon	-- will panic if not a DataCon

getDataConTyCon (Id _ _ _ (DataConId _ _ _ _ _ tycon)) = tycon
getDataConTyCon (Id _ _ _ (TupleConId a))	       = mkTupleTyCon a
getDataConTyCon	(Id _ _ _ (SpecId unspec tys _))       = mkSpecTyCon (getDataConTyCon unspec) tys
#ifdef DPH
getDataConTyCon (ProcessorCon a _) = mkProcessorTyCon a
#endif {- Data Parallel Haskell -}

getDataConSig :: DataCon -> ([TyVarTemplate], ThetaType, [TauType], TyCon)
					-- will panic if not a DataCon

getDataConSig (Id _ _ _ (DataConId _ _ tyvars theta_ty arg_tys tycon))
  = (tyvars, theta_ty, arg_tys, tycon)

getDataConSig (Id _ _ _ (TupleConId arity))
  = (tyvars, [], tyvar_tys, mkTupleTyCon arity)
  where
    tyvars	= take arity alphaTyVars
    tyvar_tys	= map mkTyVarTemplateTy tyvars

getDataConSig (Id _ _ _ (SpecId unspec ty_maybes _))
  = (spec_tyvars, spec_theta_ty, spec_arg_tys, spec_tycon)
  where
    (tyvars, theta_ty, arg_tys, tycon) = getDataConSig unspec

    ty_env = tyvars `zip` ty_maybes

    spec_tyvars = foldr nothing_tyvars [] ty_env
    nothing_tyvars (tyvar, Nothing) l = tyvar : l
    nothing_tyvars (tyvar, Just ty) l = l

    spec_env = foldr just_env [] ty_env
    just_env (tyvar, Nothing) l = l
    just_env (tyvar, Just ty) l = (tyvar, ty) : l
    spec_arg_tys = map (instantiateTauTy spec_env) arg_tys

    spec_theta_ty = if null theta_ty then []
		    else panic "getDataConSig:ThetaTy:SpecDataCon"
    spec_tycon    = mkSpecTyCon tycon ty_maybes

#ifdef DPH
getDataConSig (ProcessorCon arity _)
  = (tyvars, [], tyvar_tys, mkProcessorTyCon arity)
  where
    tyvars	= take arity alphaTyVars
    tyvar_tys	= map mkTyVarTemplateTy tyvars
#endif {- Data Parallel Haskell -}
\end{code}

@getInstantiatedDataConSig@ takes a constructor and some types to which
it is applied; it returns its signature instantiated to these types.

\begin{code}
getInstantiatedDataConSig :: 
	   DataCon	-- The data constructor
			--   Not a specialised data constructor
	-> [TauType]	-- Types to which applied
			--   Must be fully applied i.e. contain all types of tycon
	-> ([TauType],	-- Types of dict args
	    [TauType],	-- Types of regular args
	    TauType	-- Type of result
	   )

getInstantiatedDataConSig data_con tycon_arg_tys
  = ASSERT(isDataCon data_con)
    --false?? WDP 95/06: ASSERT(not (maybeToBool (isSpecId_maybe data_con)))
    let
	(tv_tmpls, theta, cmpnt_ty_tmpls, tycon) = getDataConSig data_con

	inst_env = --ASSERT(length tv_tmpls == length tycon_arg_tys)
{-		   if (length tv_tmpls /= length tycon_arg_tys) then
			pprPanic "Id:1666:" (ppCat [ppr PprShowAll data_con, ppr PprDebug tycon_arg_tys])
		   else
-}		   tv_tmpls `zip` tycon_arg_tys

	theta_tys = [ instantiateTauTy inst_env (mkDictTy c t) | (c,t) <- theta ]
	cmpnt_tys = map (instantiateTauTy inst_env) cmpnt_ty_tmpls
	result_ty = instantiateTauTy inst_env (applyTyCon tycon tycon_arg_tys)
    in
    -- Are the first/third results ever used?
    (theta_tys, cmpnt_tys, result_ty)

{- UNUSED: allows a specilaised constructor to be instantiated
	   (with all argument types of the unspecialsied tycon)

getInstantiatedDataConSig data_con tycon_arg_tys
  = ASSERT(isDataCon data_con)
    if is_speccon && arg_tys_match_error then
	pprPanic "getInstantiatedDataConSig:SpecId:"
		 (ppHang (ppr PprDebug data_con) 4 pp_match_error)
    else
	(theta_tys, cmpnt_tys, result_ty)  -- Are the first/third results ever used?
  where
    is_speccon       = maybeToBool is_speccon_maybe
    is_speccon_maybe = isSpecId_maybe data_con
    Just (unspec_con, spec_tys) = is_speccon_maybe

    arg_tys_match_error = maybeToBool match_error_maybe
    match_error_maybe   = ASSERT(length spec_tys == length tycon_arg_tys)
			  argTysMatchSpecTys spec_tys tycon_arg_tys
    (Just pp_match_error) = match_error_maybe

    (tv_tmpls, theta, cmpnt_ty_tmpls, tycon)
      = if is_speccon 
	then getDataConSig unspec_con
	else getDataConSig data_con

    inst_env = ASSERT(length tv_tmpls == length tycon_arg_tys)
	       tv_tmpls `zip` tycon_arg_tys

    theta_tys = [ instantiateTauTy inst_env (mkDictTy c t) | (c,t) <- theta ]
    cmpnt_tys = map (instantiateTauTy inst_env) cmpnt_ty_tmpls
    result_ty = instantiateTauTy inst_env (applyTyCon tycon tycon_arg_tys)
-}
\end{code}

The function @getDataConDeps@ is passed an @Id@ representing a data
constructor of some type. We look at the source types of the
constructor and create the set of all @TyCons@ referred to directly
from the source types.

\begin{code}
#ifdef USE_SEMANTIQUE_STRANAL
getDataConDeps :: Id -> [TyCon]

getDataConDeps (Id _ _ _ (DataConId _ _ _ _ arg_tys _))
  = concat (map getReferredToTyCons arg_tys)
getDataConDeps (Id _ _ _ (TupleConId _)) = []
getDataConDeps (Id _ _ _ (SpecId unspec ty_maybes _))
  = getDataConDeps unspec ++ concat (map getReferredToTyCons (catMaybes ty_maybes))
#ifdef DPH
getDataConDeps (ProcessorCon _ _) = []
#endif {- Data Parallel Haskell -}
#endif {- Semantique strictness analyser -}
\end{code}

Data type declarations are of the form:
\begin{verbatim}
data Foo a b = C1 ... | C2 ... | ... | Cn ...
\end{verbatim}
For each constructor @Ci@, we want to generate a curried function; so, e.g., for
@C1 x y z@, we want a function binding:
\begin{verbatim}
fun_C1 = /\ a -> /\ b -> \ [x, y, z] -> CoCon C1 [a, b] [x, y, z]
\end{verbatim}
Notice the ``big lambdas'' and type arguments to @CoCon@---we are producing
2nd-order polymorphic lambda calculus with explicit types.

%************************************************************************
%*									*
\subsection[unfolding-Ids]{Functions related to @Ids@' unfoldings}
%*									*
%************************************************************************

@getIdUnfolding@ takes a @Id@ (we are discussing the @DataCon@ case)
and generates an @UnfoldingDetails@ for its unfolding.  The @Ids@ and
@TyVars@ don't really have to be new, because we are only producing a
template.

ToDo: what if @DataConId@'s type has a context (haven't thought about it
--WDP)?

Note: @getDataConUnfolding@ is a ``poor man's'' version---it is NOT
EXPORTED.  It just returns the binders (@TyVars@ and @Ids@) [in the
example above: a, b, and x, y, z], which is enough (in the important
\tr{DsExpr} case).  (The middle set of @Ids@ is binders for any
dictionaries, in the even of an overloaded data-constructor---none at
present.)

\begin{code}
getIdUnfolding	    :: Id -> UnfoldingDetails

#ifdef DPH
getIdUnfolding dcon@(ProcessorCon arity _)
  = let
	(tyvars, dict_vars, vars) = getDataConUnfolding dcon
	tyvar_tys = map mkTyVarTy tyvars
    in
    BIND (CoCon dcon tyvar_tys [CoVarAtom v | v <- vars]) _TO_ plain_CoCon ->
    BIND (mkCoLam vars plain_CoCon)		 _TO_ lambdized_CoCon ->
    mkUnfoldTemplate (\x->False){-ToDo-} EssentialUnfolding{-ToDo???DPH-} (foldr CoTyLam lambdized_CoCon tyvars)
    BEND BEND

-- If we have a PodId whose ``id'' has an unfolding, then we need to
-- parallelize the unfolded expression for the d^th dimension.
{-
getIdUnfolding (PodId d _ id)
   = case (unfoldingMaybe (getIdUnfolding id)) of
	Nothing	  -> noInfo
	Just expr -> trace ("getIdUnfolding ("++
			    ppShow 80 (ppr PprDebug id) ++
			    ") for " ++ show d ++ "D pod")
			   (podizeTemplateExpr d expr)
-}
#endif {- Data Parallel Haskell -}

getIdUnfolding (Id _ _ id_info _) = getInfo_UF id_info

addIdUnfolding :: Id -> UnfoldingDetails -> Id
addIdUnfolding id@(Id u ty info details) unfold_details
  = ASSERT(
    	case (isLocallyDefined id, unfold_details) of
	(_,     NoUnfoldingDetails) -> True
	(True,  IWantToBeINLINEd _) -> True
	(False, IWantToBeINLINEd _) -> False -- v bad
	(False, _)  	    	    -> True
	_   	    	    	    -> False -- v bad
    )
    Id u ty (info `addInfo_UF` unfold_details) details

{- UNUSED:
clearIdUnfolding :: Id -> Id
clearIdUnfolding (Id u ty info details) = Id u ty (clearInfo_UF info) details
-}
\end{code}

In generating selector functions (take a dictionary, give back one
component...), we need to what out for the nothing-to-select cases (in
which case the ``selector'' is just an identity function):
\begin{verbatim}
class Eq a => Foo a { }	    # the superdict selector for "Eq"

class Foo a { op :: Complex b => c -> b -> a }
			    # the method selector for "op";
			    # note local polymorphism...
\end{verbatim}

For data constructors, we make an unfolding which has a bunch of
lambdas to bind the arguments, with a (saturated) @CoCon@ inside.  In
the case of overloaded constructors, the dictionaries are just thrown
away; they were only required in the first place to ensure that the
type was indeed an instance of the required class.
\begin{code}
#ifdef DPH
getDataConUnfolding :: Id -> ([TyVar], [Id], [Id])

getDataConUnfolding dcon@(ProcessorCon arity _)
  = BIND (mkTemplateLocals tyvar_tys)		 _TO_ vars ->
    (tyvars, [], vars)
    BEND
  where
    tyvar_tmpls = take arity alphaTyVars
    (_, tyvars, tyvar_tys) = instantiateTyVarTemplates tyvar_tmpls (map getTheUnique tyvar_tmpls)
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[IdInfo-funs]{Functions related to @Ids@' @IdInfos@}
%*									*
%************************************************************************

\begin{code}
getIdDemandInfo :: Id -> DemandInfo
getIdDemandInfo (Id _ _ info _) = getInfo info

addIdDemandInfo :: Id -> DemandInfo -> Id
addIdDemandInfo (Id u ty info details) demand_info
  = Id u ty (info `addInfo` demand_info) details
\end{code}

\begin{code}
getIdUpdateInfo :: Id -> UpdateInfo
getIdUpdateInfo (Id u ty info details) = getInfo info

addIdUpdateInfo :: Id -> UpdateInfo -> Id
addIdUpdateInfo (Id u ty info details) upd_info
  = Id u ty (info `addInfo` upd_info) details
\end{code}

\begin{code}
getIdArgUsageInfo :: Id -> ArgUsageInfo
getIdArgUsageInfo (Id u ty info details) = getInfo info

addIdArgUsageInfo :: Id -> ArgUsageInfo -> Id
addIdArgUsageInfo (Id u ty info details) au_info
  = Id u ty (info `addInfo` au_info) details
\end{code}

\begin{code}
getIdFBTypeInfo :: Id -> FBTypeInfo
getIdFBTypeInfo (Id u ty info details) = getInfo info

addIdFBTypeInfo :: Id -> FBTypeInfo -> Id
addIdFBTypeInfo (Id u ty info details) upd_info
  = Id u ty (info `addInfo` upd_info) details
\end{code}

\begin{code}
getIdSpecialisation :: Id -> SpecEnv
getIdSpecialisation (Id _ _ info _) = getInfo info

addIdSpecialisation :: Id -> SpecEnv -> Id
addIdSpecialisation (Id u ty info details) spec_info
  = Id u ty (info `addInfo` spec_info) details
\end{code}

Strictness: we snaffle the info out of the IdInfo.

\begin{code}
getIdStrictness :: Id -> StrictnessInfo

getIdStrictness (Id _ _ id_info _) = getInfo id_info

addIdStrictness :: Id -> StrictnessInfo -> Id

addIdStrictness (Id u ty info details) strict_info
  = Id u ty (info `addInfo` strict_info) details
\end{code}

%************************************************************************
%*									*
\subsection[Id-comparison]{Comparison functions for @Id@s}
%*									*
%************************************************************************

Comparison: equality and ordering---this stuff gets {\em hammered}.

\begin{code}
cmpId (Id u1 _ _ _) (Id u2 _ _ _) = cmpUnique u1 u2
-- short and very sweet
\end{code}

\begin{code}
eqId :: Id -> Id -> Bool

eqId a b = case cmpId a b of { EQ_ -> True;  _   -> False }

instance Eq Id where
    a == b = case cmpId a b of { EQ_ -> True;  _ -> False }
    a /= b = case cmpId a b of { EQ_ -> False; _ -> True  }

instance Ord Id where
    a <= b = case cmpId a b of { LT_ -> True;  EQ_ -> True;  GT__ -> False }
    a <	 b = case cmpId a b of { LT_ -> True;  EQ_ -> False; GT__ -> False }
    a >= b = case cmpId a b of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case cmpId a b of { LT_ -> False; EQ_ -> False; GT__ -> True  }
#ifdef __GLASGOW_HASKELL__
    _tagCmp a b = case cmpId a b of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
#endif
\end{code}

@cmpId_withSpecDataCon@ ensures that any spectys are taken into
account when comparing two data constructors. We need to do this
because a specialsied data constructor has the same unique as its
unspeciailsed counterpart.

\begin{code}
cmpId_withSpecDataCon :: Id -> Id -> TAG_

cmpId_withSpecDataCon id1 id2
  | eq_ids && isDataCon id1 && isDataCon id2
  = cmpEqDataCon id1 id2

  | otherwise
  = cmp_ids
  where
    cmp_ids = cmpId id1 id2
    eq_ids  = case cmp_ids of { EQ_ -> True; other -> False }

cmpEqDataCon (Id _ _ _ (SpecId _ mtys1 _)) (Id _ _ _ (SpecId _ mtys2 _))
  = cmpUniTypeMaybeList mtys1 mtys2

cmpEqDataCon unspec1 (Id _ _ _ (SpecId _ _ _))
  = LT_

cmpEqDataCon (Id _ _ _ (SpecId _ _ _)) unspec2
  = GT_

cmpEqDataCon unspec1 unspec2
  = EQ_

\end{code}

%************************************************************************
%*									*
\subsection[Id-other-instances]{Other instance declarations for @Id@s}
%*									*
%************************************************************************

\begin{code}
instance Outputable Id where
    ppr sty id = pprId sty id

showId :: PprStyle -> Id -> String
showId sty id = ppShow 80 (pprId sty id)

-- [used below]
-- for DictFuns (instances) and const methods (instance code bits we
-- can call directly): exported (a) if *either* the class or
-- ***OUTERMOST*** tycon [arbitrary...] is exported; or (b) *both*
-- class and tycon are from PreludeCore [non-std, but convenient]
-- *and* the thing was defined in this module.

instance_export_flag :: Class -> UniType -> Bool -> ExportFlag

instance_export_flag clas inst_ty from_here
  = if instanceIsExported clas inst_ty from_here
    then ExportAll
    else NotExported
\end{code}

Do we consider an ``instance type'' (as on a @DictFunId@) to be ``from
PreludeCore''?  True if the outermost TyCon is fromPreludeCore.
\begin{code}
is_prelude_core_ty :: UniType -> Bool

is_prelude_core_ty inst_ty
  = case getUniDataTyCon_maybe inst_ty of
      Just (tycon,_,_) -> fromPreludeCore tycon
      Nothing 	       -> panic "Id: is_prelude_core_ty"
\end{code}

Default printing code (not used for interfaces):
\begin{code}
pprId :: PprStyle -> Id -> Pretty

pprId other_sty id
  = let
	pieces = getIdNamePieces (case other_sty of {PprForUser -> False; _ -> True}) id

	for_code
	  = let
		pieces_to_print -- maybe use Unique only
		  = if isSysLocalId id then tail pieces else pieces
	    in
	    ppIntersperse (ppPStr cSEP) (map identToC pieces_to_print)
    in
    case other_sty of
      PprForC _	      -> for_code
      PprForAsm _ _ _ -> for_code
      PprInterface _  -> ppPStr occur_name
      PprForUser      -> ppPStr occur_name
      PprUnfolding _  -> qualified_name pieces
      PprDebug	      -> qualified_name pieces
      PprShowAll      -> ppBesides [qualified_name pieces,
			    (ppCat [pp_uniq id,
				    ppPStr SLIT("{-"),
				    ppr other_sty (getIdUniType id),
				    ppIdInfo other_sty id True (\x->x) nullIdEnv (getIdInfo id),
				    ppPStr SLIT("-}") ])]
  where
    occur_name = getOccurrenceName id _APPEND_
		 ( _PK_ (if not (isSysLocalId id)
			 then ""
			 else "." ++ (_UNPK_ (showUnique (getTheUnique id)))))

    qualified_name pieces
      = ppBeside (pp_ubxd (ppIntersperse (ppChar '.') (map ppPStr pieces))) (pp_uniq id)

    pp_uniq (Id _ _ _ (PreludeId _)) 	= ppNil -- No uniq to add
    pp_uniq (Id _ _ _ (DataConId _ _ _ _ _ _)) = ppNil	-- No uniq to add
    pp_uniq (Id _ _ _ (TupleConId _)) 	    = ppNil -- No uniq to add
    pp_uniq (Id _ _ _ (LocalId      _ _))   = ppNil -- uniq printed elsewhere
    pp_uniq (Id _ _ _ (SysLocalId   _ _))   = ppNil -- ditto
    pp_uniq (Id _ _ _ (SpecPragmaId _ _ _)) = ppNil -- ditto
    pp_uniq (Id _ _ _ (InstId _))   	    = ppNil -- ditto
    pp_uniq other_id = ppBesides [ppPStr SLIT("{-"), pprUnique (getTheUnique other_id),  ppPStr SLIT("-}")]

    -- For Robin Popplestone: print PprDebug Ids with # afterwards
    -- if they are of primitive type.
    pp_ubxd pretty = if isPrimType (getIdUniType id)
		     then ppBeside pretty (ppChar '#')
		     else pretty
\end{code}

\begin{code}
instance NamedThing Id where
    getExportFlag (Id _ _ _ details)
      = get details
      where
	get (DataConId _ _ _ _ _ tc)= getExportFlag tc -- NB: don't use the FullName
	get (TupleConId _)	    = NotExported
	get (ImportedId  n)         = getExportFlag n
	get (PreludeId   n)         = getExportFlag n
	get (TopLevId    n)         = getExportFlag n
	get (SuperDictSelId c _)    = getExportFlag c
	get (ClassOpId  c _)	    = getExportFlag c
	get (DefaultMethodId c _ _) = getExportFlag c
	get (DictFunId  c ty from_here _) = instance_export_flag c ty from_here
	get (ConstMethodId c ty _ from_here _) = instance_export_flag c ty from_here
	get (SpecId unspec _ _)     = getExportFlag unspec
	get (WorkerId unwrkr)	    = getExportFlag unwrkr
	get (InstId _)		    = NotExported
	get (LocalId      _ _)	    = NotExported
	get (SysLocalId   _ _)	    = NotExported
	get (SpecPragmaId _ _ _)    = NotExported
#ifdef DPH
	get (ProcessorCon _ _)	    = NotExported
	get (PodId _ _ i)	    = getExportFlag i
#endif {- Data Parallel Haskell -}

    isLocallyDefined this_id@(Id _ _ _ details)
      = get details
      where
	get (DataConId _ _ _ _ _ tc)= isLocallyDefined tc -- NB: don't use the FullName
	get (TupleConId _)	    = False
	get (ImportedId	_)    	    = False
	get (PreludeId  _)    	    = False
	get (TopLevId	n)	    = isLocallyDefined n
	get (SuperDictSelId c _)    = isLocallyDefined c
	get (ClassOpId c _) 	    = isLocallyDefined c
	get (DefaultMethodId c _ _) = isLocallyDefined c
	get (DictFunId c tyc from_here _) = from_here
	    -- For DictFunId and ConstMethodId things, you really have to
	    -- know whether it came from an imported instance or one
	    -- really here; no matter where the tycon and class came from.

	get (ConstMethodId c tyc _ from_here _) = from_here
	get (SpecId unspec _ _)	    = isLocallyDefined unspec
	get (WorkerId unwrkr) 	    = isLocallyDefined unwrkr
	get (InstId  _)		    = True
	get (LocalId      _ _)	    = True
	get (SysLocalId   _ _)	    = True
	get (SpecPragmaId _ _ _)    = True
#ifdef DPH
	get (ProcessorCon _ _)	    = False
	get (PodId _ _ i)	    = isLocallyDefined i
#endif {- Data Parallel Haskell -}

    getOrigName this_id@(Id u _ _ details)
      = get details
      where
	get (DataConId n _ _ _ _ _) = getOrigName n
	get (TupleConId a)	    = (pRELUDE_BUILTIN, SLIT("Tup") _APPEND_ _PK_ (show a))
	get (ImportedId   n)	    = getOrigName n
	get (PreludeId    n)	    = getOrigName n
	get (TopLevId     n)	    = getOrigName n

	get (ClassOpId c op)	    = case (getOrigName c) of -- ToDo; better ???
					(mod, _) -> (mod, getClassOpString op)

	get (SpecId unspec ty_maybes _)
	  = BIND getOrigName unspec	      _TO_ (mod, unspec_nm) ->
	    BIND specMaybeTysSuffix ty_maybes _TO_ tys_suffix ->
	    (mod,
	     unspec_nm _APPEND_
		(if not (toplevelishId unspec)
		 then showUnique u
		 else tys_suffix)
	    )
	    BEND BEND

	get (WorkerId unwrkr)
	  = BIND getOrigName unwrkr	_TO_ (mod, unwrkr_nm) ->
	    (mod,
	     unwrkr_nm _APPEND_
		(if not (toplevelishId unwrkr)
		 then showUnique u
		 else SLIT(".wrk"))
	    )
	    BEND

	get (InstId inst)
	  = (panic "NamedThing.Id.getOrigName (InstId)",
	     BIND (getInstNamePieces True inst)   _TO_ (piece1:pieces) ->
	     BIND [ _CONS_ '.' p | p <- pieces ]  _TO_ dotted_pieces ->
	     _CONCAT_ (piece1 : dotted_pieces)
	     BEND BEND )

	get (LocalId      n _) = (panic "NamedThing.Id.getOrigName (LocalId)",
				  getLocalName n)
	get (SysLocalId   n _) = (panic "NamedThing.Id.getOrigName (SysLocal)",
				  getLocalName n)
	get (SpecPragmaId n _ _)=(panic "NamedThing.Id.getOrigName (SpecPragmaId)",
				  getLocalName n)
#ifdef DPH
	get (ProcessorCon a _)	    = ("PreludeBuiltin",
						   "MkProcessor" ++ (show a))
	get (PodId d ity id)
	  = BIND (getOrigName id) _TO_ (m,n) ->
	    (m,n ++ ".mapped.POD"++ show d ++ "." ++ show ity)
	    BEND
    -- ToDo(hilly): should the above be using getIdNamePieces???
#endif {- Data Parallel Haskell -}

	get other_details
	    -- the remaining internally-generated flavours of
	    -- Ids really do not have meaningful "original name" stuff,
	    -- but we need to make up something (usually for debugging output)

	  = BIND (getIdNamePieces True this_id)  _TO_ (piece1:pieces) ->
	    BIND [ _CONS_ '.' p | p <- pieces ]  _TO_ dotted_pieces ->
	    (_NIL_, _CONCAT_ (piece1 : dotted_pieces))
	    BEND BEND

    getOccurrenceName this_id@(Id _ _ _ details)
      = get details
      where
	get (DataConId  n _ _ _ _ _) = getOccurrenceName n
	get (TupleConId a)	= SLIT("Tup") _APPEND_ (_PK_ (show a))
	get (ImportedId	 n)	= getOccurrenceName n
	get (PreludeId   n)	= getOccurrenceName n
	get (TopLevId	 n)	= getOccurrenceName n
	get (ClassOpId _ op)	= getClassOpString op
#ifdef DPH
	get (ProcessorCon a _)	= "MkProcessor" ++ (show a)
	get (PodId _ _ id)	= getOccurrenceName id
#endif {- Data Parallel Haskell -}
	get _			= snd (getOrigName this_id)

    getInformingModules id = panic "getInformingModule:Id"

    getSrcLoc (Id _ _ id_info details)
      = get details
      where
	get (DataConId  n _ _ _ _ _) = getSrcLoc n
	get (TupleConId _)	= mkBuiltinSrcLoc
	get (ImportedId	 n)	= getSrcLoc n
	get (PreludeId   n)	= getSrcLoc n
	get (TopLevId	 n)	= getSrcLoc n
	get (SuperDictSelId c _)= getSrcLoc c
	get (ClassOpId c _)	= getSrcLoc c
	get (SpecId unspec _ _)	= getSrcLoc unspec
	get (WorkerId unwrkr)	= getSrcLoc unwrkr
	get (InstId	i)	= let (loc,_) = getInstOrigin i
				  in  loc
	get (LocalId      n _)	= getSrcLoc n
	get (SysLocalId   n _)	= getSrcLoc n
	get (SpecPragmaId n _ _)= getSrcLoc n
#ifdef DPH
	get (ProcessorCon _ _)	= mkBuiltinSrcLoc
	get (PodId _ _ n)		= getSrcLoc n
#endif {- Data Parallel Haskell -}
	-- well, try the IdInfo
	get something_else = getSrcLocIdInfo id_info

    getTheUnique (Id u _ _ _) = u

    fromPreludeCore (Id _ _ _ details)
      = get details
      where
	get (DataConId _ _ _ _ _ tc)= fromPreludeCore tc -- NB: not from the FullName
	get (TupleConId _)	    = True
	get (ImportedId  n)	    = fromPreludeCore n
	get (PreludeId   n)	    = fromPreludeCore n
	get (TopLevId    n)	    = fromPreludeCore n
	get (SuperDictSelId c _)    = fromPreludeCore c
	get (ClassOpId c _)	    = fromPreludeCore c
	get (DefaultMethodId c _ _) = fromPreludeCore c
	get (DictFunId	c t _ _)    = fromPreludeCore c && is_prelude_core_ty t
	get (ConstMethodId c t _ _ _) = fromPreludeCore c && is_prelude_core_ty t
	get (SpecId unspec _ _)	    = fromPreludeCore unspec
	get (WorkerId unwrkr)	    = fromPreludeCore unwrkr
	get (InstId   _)	    = False
	get (LocalId      _ _)	    = False
	get (SysLocalId   _ _)	    = False
	get (SpecPragmaId _ _ _)    = False
#ifdef DPH			     
	get (ProcessorCon _ _)	    = True
	get (PodId _ _ id)	    = fromPreludeCore id
#endif {- Data Parallel Haskell -}

    hasType id	= True
    getType id	= getIdUniType id
\end{code}

Reason for @getTheUnique@: The code generator doesn't carry a
@UniqueSupply@, so it wants to use the @Uniques@ out of local @Ids@
given to it.
