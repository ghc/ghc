%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
#include "HsVersions.h"

module Id (
	-- TYPES
	GenId(..), -- *naughtily* used in some places (e.g., TcHsSyn)
	SYN_IE(Id), IdDetails,
	StrictnessMark(..),
	SYN_IE(ConTag), fIRST_TAG,
	SYN_IE(DataCon), SYN_IE(DictFun), SYN_IE(DictVar),

	-- CONSTRUCTION
	mkConstMethodId,
	mkDataCon,
	mkDefaultMethodId,
	mkDictFunId,
	mkIdWithNewUniq,
	mkImported,
	mkInstId,
	mkMethodSelId,
	mkRecordSelId,
	mkSuperDictSelId,
	mkSysLocal,
	mkTemplateLocals,
	mkTupleCon,
	mkUserId,
	mkUserLocal,
	mkWorkerId,

	-- MANGLING
	unsafeGenId2Id,

	-- DESTRUCTION (excluding pragmatic info)
	idPrimRep,
	idType,
	idUnique,

	dataConArgTys,
	dataConArity,
	dataConNumFields,
	dataConFieldLabels,
	dataConRawArgTys,
	dataConSig,
	dataConStrictMarks,
	dataConTag,
	dataConTyCon,

	recordSelectorFieldLabel,

	-- PREDICATES
	cmpEqDataCon,
	cmpId,
	cmpId_withSpecDataCon,
	externallyVisibleId,
	idHasNoFreeTyVars,
	idWantsToBeINLINEd,
	isBottomingId,
	isConstMethodId,
	isConstMethodId_maybe,
	isDataCon,
	isDefaultMethodId,
	isDefaultMethodId_maybe,
	isDictFunId,
	isImportedId,
	isMethodSelId,
	isNullaryDataCon,
	isSpecPragmaId,
	isSuperDictSelId_maybe,
	isSysLocalId,
	isTopLevId,
	isTupleCon,
	isWorkerId,
	isWrapperId,
	toplevelishId,
	unfoldingUnfriendlyId,

	-- SUBSTITUTION
	applyTypeEnvToId,
	apply_to_Id,
	
	-- PRINTING and RENUMBERING
	addId,
	nmbrDataCon,
	nmbrId,
	pprId,
	showId,

	-- UNFOLDING, ARITY, UPDATE, AND STRICTNESS STUFF (etc)
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
	replaceIdInfo,

	-- IdEnvs AND IdSets
	SYN_IE(IdEnv), SYN_IE(GenIdSet), SYN_IE(IdSet),
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
	nullIdEnv,
	rngIdEnv,
	unionIdSets,
	unionManyIdSets,
	unitIdEnv,
	unitIdSet
    ) where

IMP_Ubiq()
IMPORT_DELOOPER(IdLoop)   -- for paranoia checking
IMPORT_DELOOPER(TyLoop)   -- for paranoia checking

import Bag
import Class		( classOpString, SYN_IE(Class), GenClass, SYN_IE(ClassOp), GenClassOp )
import IdInfo
import Maybes		( maybeToBool )
import Name		( appendRdr, nameUnique, mkLocalName, isLocalName,
			  isLocallyDefinedName,
			  mkTupleDataConName, mkCompoundName, mkCompoundName2,
			  isLexSym, isLexSpecialSym,
			  isLocallyDefined, changeUnique,
			  getOccName, origName, moduleOf,
			  isExported, ExportFlag(..),
			  RdrName(..), Name
			)
import FieldLabel	( fieldLabelName, FieldLabel(..){-instances-} )
import PragmaInfo	( PragmaInfo(..) )
import PprEnv		-- ( SYN_IE(NmbrM), NmbrEnv(..) )
import PprType		( getTypeString, typeMaybeString, specMaybeTysSuffix,
			  nmbrType, nmbrTyVar,
			  GenType, GenTyVar
			)
import PprStyle
import Pretty
import SrcLoc		( mkBuiltinSrcLoc )
import TyCon		( TyCon, mkTupleTyCon, tyConDataCons )
import Type		( mkSigmaTy, mkTyVarTys, mkFunTys, mkDictTy,
			  applyTyCon, instantiateTy,
			  tyVarsOfType, applyTypeEnvToTy, typePrimRep,
			  GenType, SYN_IE(ThetaType), SYN_IE(TauType), SYN_IE(Type)
			)
import TyVar		( alphaTyVars, isEmptyTyVarSet, SYN_IE(TyVarEnv) )
import UniqFM
import UniqSet		-- practically all of it
import Unique		( getBuiltinUniques, pprUnique, showUnique,
			  incrUnique,
			  Unique{-instance Ord3-}
			)
import Util		( mapAccumL, nOfThem, zipEqual,
			  panic, panic#, pprPanic, assertPanic
			)
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
				   
type Id = GenId Type

data StrictnessMark = MarkedStrict | NotMarkedStrict

data IdDetails

  ---------------- Local values

  = LocalId	Bool		-- Local name; mentioned by the user
				-- True <=> no free type vars

  | SysLocalId	Bool	        -- Local name; made up by the compiler
				-- as for LocalId

  | SpecPragmaId 		-- Local name; introduced by the compiler
		 (Maybe Id)	-- for explicit specid in pragma
		 Bool		-- as for LocalId

  ---------------- Global values

  | ImportedId			-- Global name (Imported or Implicit); Id imported from an interface

  | TopLevId			-- Global name (LocalDef); Top-level in the orig source pgm
				-- (not moved there by transformations).

	-- a TopLevId's type may contain free type variables, if
	-- the monomorphism restriction applies.

  ---------------- Data constructors

  | DataConId	ConTag
		[StrictnessMark] -- Strict args; length = arity
		[FieldLabel]	-- Field labels for this constructor

		[TyVar] [(Class,Type)] [Type] TyCon
				-- the type is:
				-- forall tyvars . theta_ty =>
				--    unitype_1 -> ... -> unitype_n -> tycon tyvars

  | TupleConId	Int		-- Its arity

  | RecordSelId FieldLabel

  ---------------- Things to do with overloading

  | SuperDictSelId		-- Selector for superclass dictionary
		Class		-- The class (input dict)
		Class		-- The superclass (result dict)

  | MethodSelId	Class		-- An overloaded class operation, with
				-- a fully polymorphic type.  Its code
				-- just selects a method from the
				-- dictionary.  The class.
		ClassOp		-- The operation

	-- NB: The IdInfo for a MethodSelId has all the info about its
	-- related "constant method Ids", which are just
	-- specialisations of this general one.

  | DefaultMethodId		-- Default method for a particular class op
		Class		-- same class, <blah-blah> info as MethodSelId
		ClassOp		-- (surprise, surprise)
		Bool		-- True <=> I *know* this default method Id
				-- is a generated one that just says
				-- `error "No default method for <op>"'.

				-- see below
  | DictFunId	Class		-- A DictFun is uniquely identified
		Type		-- by its class and type; this type has free type vars,
				-- whose identity is irrelevant.  Eg Class = Eq
				--				     Type  = Tree a
				-- The "a" is irrelevant.  As it is too painful to
				-- actually do comparisons that way, we kindly supply
				-- a Unique for that purpose.
		Module		-- module where instance came from

				-- see below
  | ConstMethodId		-- A method which depends only on the type of the
				-- instance, and not on any further dictionaries etc.
		Class		-- Uniquely identified by:
		Type		-- (class, type, classop) triple
		ClassOp
		Module		-- module where instance came from

  | InstId			-- An instance of a dictionary, class operation,
				-- or overloaded value (Local name)
		Bool		-- as for LocalId

  | SpecId			-- A specialisation of another Id
		Id		-- Id of which this is a specialisation
		[Maybe Type]	-- Types at which it is specialised;
				-- A "Nothing" says this type ain't relevant.
		Bool		-- True <=> no free type vars; it's not enough
				-- to know about the unspec version, because
				-- we may specialise to a type w/ free tyvars
				-- (i.e., in one of the "Maybe Type" dudes).

  | WorkerId			-- A "worker" for some other Id
		Id		-- Id for which this is a worker

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
\item[@DataConId@:] For the data constructors declared by a @data@
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
\item[@TopLevId@:] These are values defined at the top-level in this
module; i.e., those which {\em might} be exported (hence, a
@Name@).  It does {\em not} include those which are moved to the
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
Note that @InstIds@, @Locals@ and @SysLocals@ {\em may} have the above
properties, but they may not.
\end{enumerate}

%************************************************************************
%*									*
\subsection[Id-general-funs]{General @Id@-related functions}
%*									*
%************************************************************************

\begin{code}
unsafeGenId2Id :: GenId ty -> Id
unsafeGenId2Id (Id u n ty d p i) = Id u n (panic "unsafeGenId2Id:ty") d p i

isDataCon id = is_data (unsafeGenId2Id id)
 where
  is_data (Id _ _ _ (DataConId _ _ _ _ _ _ _) _ _) = True
  is_data (Id _ _ _ (TupleConId _) _ _)		   = True
  is_data (Id _ _ _ (SpecId unspec _ _) _ _)	   = is_data unspec
  is_data other					   = False


isTupleCon id = is_tuple (unsafeGenId2Id id)
 where
  is_tuple (Id _ _ _ (TupleConId _) _ _)	 = True
  is_tuple (Id _ _ _ (SpecId unspec _ _) _ _)	 = is_tuple unspec
  is_tuple other				 = False

{-LATER:
isSpecId_maybe (Id _ _ _ (SpecId unspec ty_maybes _) _ _)
  = ASSERT(not (maybeToBool (isSpecId_maybe unspec)))
    Just (unspec, ty_maybes)
isSpecId_maybe other_id
  = Nothing

isSpecPragmaId_maybe (Id _ _ _ (SpecPragmaId specid _) _ _)
  = Just specid
isSpecPragmaId_maybe other_id
  = Nothing
-}
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
    chk (DataConId _ _ _ _ _ _ _)   = True
    chk (TupleConId _)    	    = True
    chk (RecordSelId _)   	    = True
    chk ImportedId	    	    = True
    chk TopLevId	    	    = True	-- NB: see notes
    chk (SuperDictSelId _ _)	    = True
    chk (MethodSelId _ _)	    = True
    chk (DefaultMethodId _ _ _)     = True
    chk (DictFunId     _ _ _)	    = True
    chk (ConstMethodId _ _ _ _)     = True
    chk (SpecId unspec _ _)	    = toplevelishId unspec
				    -- depends what the unspecialised thing is
    chk (WorkerId unwrkr)	    = toplevelishId unwrkr
    chk (InstId	      _)	    = False	-- these are local
    chk (LocalId      _)	    = False
    chk (SysLocalId   _)	    = False
    chk (SpecPragmaId _ _)	    = False

idHasNoFreeTyVars (Id _ _ _ details _ info)
  = chk details
  where
    chk (DataConId _ _ _ _ _ _ _) = True
    chk (TupleConId _)    	  = True
    chk (RecordSelId _)   	  = True
    chk ImportedId	    	  = True
    chk TopLevId	    	  = True
    chk (SuperDictSelId _ _)	  = True
    chk (MethodSelId _ _)	  = True
    chk (DefaultMethodId _ _ _)   = True
    chk (DictFunId     _ _ _)	  = True
    chk (ConstMethodId _ _ _ _)   = True
    chk (WorkerId unwrkr)	  = idHasNoFreeTyVars unwrkr
    chk (SpecId _     _   no_free_tvs) = no_free_tvs
    chk (InstId         no_free_tvs) = no_free_tvs
    chk (LocalId        no_free_tvs) = no_free_tvs
    chk (SysLocalId     no_free_tvs) = no_free_tvs
    chk (SpecPragmaId _ no_free_tvs) = no_free_tvs
\end{code}

\begin{code}
isTopLevId (Id _ _ _ TopLevId _ _) = True
isTopLevId other		   = False

isImportedId (Id _ _ _ ImportedId _ _) = True
isImportedId other		       = False

isBottomingId (Id _ _ _ _ _ info) = bottomIsGuaranteed (getInfo info)

isSysLocalId (Id _ _ _ (SysLocalId _) _ _) = True
isSysLocalId other			   = False

isSpecPragmaId (Id _ _ _ (SpecPragmaId _ _) _ _) = True
isSpecPragmaId other			         = False

isMethodSelId (Id _ _ _ (MethodSelId _ _) _ _) = True
isMethodSelId _				       = False

isDefaultMethodId (Id _ _ _ (DefaultMethodId _ _ _) _ _) = True
isDefaultMethodId other				         = False

isDefaultMethodId_maybe (Id _ _ _ (DefaultMethodId cls clsop err) _ _)
  = Just (cls, clsop, err)
isDefaultMethodId_maybe other = Nothing

isDictFunId (Id _ _ _ (DictFunId _ _ _) _ _) = True
isDictFunId other		    	     = False

isConstMethodId (Id _ _ _ (ConstMethodId _ _ _ _) _ _) = True
isConstMethodId other		    		       = False

isConstMethodId_maybe (Id _ _ _ (ConstMethodId cls ty clsop _) _ _)
  = Just (cls, ty, clsop)
isConstMethodId_maybe other = Nothing

isSuperDictSelId_maybe (Id _ _ _ (SuperDictSelId c sc) _ _) = Just (c, sc)
isSuperDictSelId_maybe other_id				  = Nothing

isWorkerId (Id _ _ _ (WorkerId _) _ _) = True
isWorkerId other		     = False

isWrapperId id = workerExists (getIdStrictness id)
\end{code}

\begin{code}
{-LATER:
pprIdInUnfolding :: IdSet -> Id -> Pretty

pprIdInUnfolding in_scopes v
  = let
	v_ty = idType v
    in
    -- local vars first:
    if v `elementOfUniqSet` in_scopes then
	pprUnique (idUnique v)

    -- ubiquitous Ids with special syntax:
    else if v == nilDataCon then
	ppPStr SLIT("_NIL_")
    else if isTupleCon v then
	ppBeside (ppPStr SLIT("_TUP_")) (ppInt (dataConArity v))

    -- ones to think about:
    else
	let
	    (Id _ _ _ v_details _ _) = v
	in
    	case v_details of
	    -- these ones must have been exported by their original module
	  ImportedId   -> pp_full_name

	    -- these ones' exportedness checked later...
	  TopLevId  -> pp_full_name
	  DataConId _ _ _ _ _ _ _ -> pp_full_name

	  RecordSelId lbl -> ppr sty lbl

	    -- class-ish things: class already recorded as "mentioned"
	  SuperDictSelId c sc
	    -> ppCat [ppPStr SLIT("_SDSEL_"), pp_class c, pp_class sc]
	  MethodSelId c o
	    -> ppCat [ppPStr SLIT("_METH_"), pp_class c, pp_class_op o]
	  DefaultMethodId c o _
	    -> ppCat [ppPStr SLIT("_DEFM_"), pp_class c, pp_class_op o]

	    -- instance-ish things: should we try to figure out
	    -- *exactly* which extra instances have to be exported? (ToDo)
	  DictFunId  c t _
	    -> ppCat [ppPStr SLIT("_DFUN_"), pp_class c, pp_type t]
	  ConstMethodId c t o _
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
	    (OrigName m_str n_str) = origName "Id:ppr_Unfolding" v

	    pp_n =
	      if isLexSym n_str && not (isLexSpecialSym n_str) then
		  ppBesides [ppLparen, ppPStr n_str, ppRparen]
	      else
		  ppPStr n_str
	in
	if isPreludeDefined v then
	    pp_n
	else
	    ppCat [ppPStr SLIT("_ORIG_"), ppPStr m_str, pp_n]

    pp_class :: Class -> Pretty
    pp_class_op :: ClassOp -> Pretty
    pp_type :: Type -> Pretty
    pp_ty_maybe :: Maybe Type -> Pretty

    pp_class    clas = ppr ppr_Unfolding clas
    pp_class_op op   = ppr ppr_Unfolding op

    pp_type t = ppBesides [ppLparen, ppr ppr_Unfolding t, ppRparen]

    pp_ty_maybe Nothing  = ppPStr SLIT("_N_")
    pp_ty_maybe (Just t) = pp_type t
-}
\end{code}

@whatsMentionedInId@ ferrets out the types/classes/instances on which
this @Id@ depends.  If this Id is to appear in an interface, then
those entities had Jolly Well be in scope.  Someone else up the
call-tree decides that.

\begin{code}
{-LATER:
whatsMentionedInId
	:: IdSet			    -- Ids known to be in scope
	-> Id				    -- Id being processed
	-> (Bag Id, Bag TyCon, Bag Class)   -- mentioned Ids/TyCons/etc.

whatsMentionedInId in_scopes v
  = let
	v_ty = idType v

    	(tycons, clss)
	  = getMentionedTyConsAndClassesFromType v_ty

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
	    (Id _ _ _ v_details _ _) = v
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
-}
\end{code}

Tell them who my wrapper function is.
\begin{code}
{-LATER:
myWrapperMaybe :: Id -> Maybe Id

myWrapperMaybe (Id _ _ _ (WorkerId my_wrapper) _ _) = Just my_wrapper
myWrapperMaybe other_id			    	  = Nothing
-}
\end{code}

\begin{code}
unfoldingUnfriendlyId	-- return True iff it is definitely a bad
	:: Id		-- idea to export an unfolding that
	-> Bool		-- mentions this Id.  Reason: it cannot
			-- possibly be seen in another module.

unfoldingUnfriendlyId id = True -- ToDo:panic "Id.unfoldingUnfriendlyId"
{-LATER:

unfoldingUnfriendlyId id
  | not (externallyVisibleId id) -- that settles that...
  = True

unfoldingUnfriendlyId (Id _ _ _ (WorkerId wrapper) _ _)
  = class_thing wrapper
  where
    -- "class thing": If we're going to use this worker Id in
    -- an interface, we *have* to be able to untangle the wrapper's
    -- strictness when reading it back in.  At the moment, this
    -- is not always possible: in precisely those cases where
    -- we pass tcGenPragmas a "Nothing" for its "ty_maybe".

    class_thing (Id _ _ _ (SuperDictSelId _ _) _ _)    = True
    class_thing (Id _ _ _ (MethodSelId _ _) _ _)  	   = True
    class_thing (Id _ _ _ (DefaultMethodId _ _ _) _ _) = True
    class_thing other				   = False

unfoldingUnfriendlyId (Id _ _ _ (SpecId d@(Id _ _ _ dfun@(DictFunId _ t _)) _ _) _ _)
    -- a SPEC of a DictFunId can end up w/ gratuitous
    -- TyVar(Templates) in the i/face; only a problem
    -- if -fshow-pragma-name-errs; but we can do without the pain.
    -- A HACK in any case (WDP 94/05/02)
  = naughty_DictFunId dfun

unfoldingUnfriendlyId d@(Id _ _ _ dfun@(DictFunId _ t _) _ _)
  = naughty_DictFunId dfun -- similar deal...

unfoldingUnfriendlyId other_id   = False -- is friendly in all other cases

naughty_DictFunId :: IdDetails -> Bool
    -- True <=> has a TyVar(Template) in the "type" part of its "name"

naughty_DictFunId (DictFunId _ _ _) = panic "False" -- came from outside; must be OK
naughty_DictFunId (DictFunId _ ty _)
  = not (isGroundTy ty)
-}
\end{code}

@externallyVisibleId@: is it true that another module might be
able to ``see'' this Id?

We need the @toplevelishId@ check as well as @isExported@ for when we
compile instance declarations in the prelude.  @DictFunIds@ are
``exported'' if either their class or tycon is exported, but, in
compiling the prelude, the compiler may not recognise that as true.

\begin{code}
externallyVisibleId :: Id -> Bool

externallyVisibleId id@(Id _ _ _ details _ _)
  = if isLocallyDefined id then
	toplevelishId id && (isExported id || isDataCon id)
	-- NB: the use of "isExported" is most dodgy;
	-- We may eventually move to a situation where
	-- every Id is "externallyVisible", even if the
	-- module system's namespace control renders it
	-- "not exported".
    else
	True
	-- if visible here, it must be visible elsewhere, too.
\end{code}

\begin{code}
idWantsToBeINLINEd :: Id -> Bool

idWantsToBeINLINEd (Id _ _ _ _ IWantToBeINLINEd _) = True
idWantsToBeINLINEd _				   = False
\end{code}

For @unlocaliseId@: See the brief commentary in
\tr{simplStg/SimplStg.lhs}.

\begin{code}
{-LATER:
unlocaliseId :: FAST_STRING{-modulename-} -> Id -> Maybe Id

unlocaliseId mod (Id u fn ty info TopLevId)
  = Just (Id u (unlocaliseFullName fn) ty info TopLevId)

unlocaliseId mod (Id u sn ty info (LocalId no_ftvs))
  = --false?: ASSERT(no_ftvs)
    let
	full_name = unlocaliseShortName mod u sn
    in
    Just (Id u full_name ty info TopLevId)

unlocaliseId mod (Id u sn ty info (SysLocalId no_ftvs))
  = --false?: on PreludeGlaST: ASSERT(no_ftvs)
    let
	full_name = unlocaliseShortName mod u sn
    in
    Just (Id u full_name ty info TopLevId)

unlocaliseId mod (Id u n ty info (SpecId unspec ty_maybes no_ftvs))
  = case unlocalise_parent mod u unspec of
      Nothing -> Nothing
      Just xx -> Just (Id u n ty info (SpecId xx ty_maybes no_ftvs))

unlocaliseId mod (Id u n ty info (WorkerId unwrkr))
  = case unlocalise_parent mod u unwrkr of
      Nothing -> Nothing
      Just xx -> Just (Id u n ty info (WorkerId xx))

unlocaliseId mod (Id u name ty info (InstId no_ftvs))
  = Just (Id u full_name ty info TopLevId)
	-- type might be wrong, but it hardly matters
	-- at this stage (just before printing C)  ToDo
  where
    name = nameOf (origName "Id.unlocaliseId" name)
    full_name = mkFullName mod name InventedInThisModule ExportAll mkGeneratedSrcLoc

unlocaliseId mod other_id = Nothing

--------------------
-- we have to be Very Careful for workers/specs of
-- local functions!

unlocalise_parent mod uniq (Id _ sn ty info (LocalId no_ftvs))
  = --false?: ASSERT(no_ftvs)
    let
	full_name = unlocaliseShortName mod uniq sn
    in
    Just (Id uniq full_name ty info TopLevId)

unlocalise_parent mod uniq (Id _ sn ty info (SysLocalId no_ftvs))
  = --false?: ASSERT(no_ftvs)
    let
	full_name = unlocaliseShortName mod uniq sn
    in
    Just (Id uniq full_name ty info TopLevId)

unlocalise_parent mod uniq other_id = unlocaliseId mod other_id
  -- we're OK otherwise
-}
\end{code}

CLAIM (not ASSERTed) for @applyTypeEnvToId@ and @applySubstToId@:
`Top-levelish Ids'' cannot have any free type variables, so applying
the type-env cannot have any effect.  (NB: checked in CoreLint?)

The special casing is in @applyTypeEnvToId@, not @apply_to_Id@, as the
former ``should be'' the usual crunch point.

\begin{code}
type TypeEnv = TyVarEnv Type

applyTypeEnvToId :: TypeEnv -> Id -> Id

applyTypeEnvToId type_env id@(Id _ _ ty _ _ _)
  | idHasNoFreeTyVars id
  = id
  | otherwise
  = apply_to_Id ( \ ty ->
	applyTypeEnvToTy type_env ty
    ) id
\end{code}

\begin{code}
apply_to_Id :: (Type -> Type) -> Id -> Id

apply_to_Id ty_fn (Id u n ty details prag info)
  = let
	new_ty = ty_fn ty
    in
    Id u n new_ty (apply_to_details details) prag (apply_to_IdInfo ty_fn info)
  where
    apply_to_details (SpecId unspec ty_maybes no_ftvs)
      = let
	    new_unspec = apply_to_Id ty_fn unspec
	    new_maybes = map apply_to_maybe ty_maybes
	in
	SpecId new_unspec new_maybes (no_free_tvs ty)
	-- ToDo: gratuitous recalc no_ftvs???? (also InstId)
      where
	apply_to_maybe Nothing   = Nothing
	apply_to_maybe (Just ty) = Just (ty_fn ty)

    apply_to_details (WorkerId unwrkr)
      = let
	    new_unwrkr = apply_to_Id ty_fn unwrkr
	in
	WorkerId new_unwrkr

    apply_to_details other = other
\end{code}

Sadly, I don't think the one using the magic typechecker substitution
can be done with @apply_to_Id@.  Here we go....

Strictness is very important here.  We can't leave behind thunks
with pointers to the substitution: it {\em must} be single-threaded.

\begin{code}
{-LATER:
applySubstToId :: Subst -> Id -> (Subst, Id)

applySubstToId subst id@(Id u n ty info details)
  -- *cannot* have a "idHasNoFreeTyVars" get-out clause
  -- because, in the typechecker, we are still
  -- *concocting* the types.
  = case (applySubstToTy     subst ty)		of { (s2, new_ty)      ->
    case (applySubstToIdInfo s2    info)	of { (s3, new_info)    ->
    case (apply_to_details   s3 new_ty details) of { (s4, new_details) ->
    (s4, Id u n new_ty new_info new_details) }}}
  where
    apply_to_details subst _ (InstId inst no_ftvs)
      = case (applySubstToInst subst inst) of { (s2, new_inst) ->
	(s2, InstId new_inst no_ftvs{-ToDo:right???-}) }

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
-}
\end{code}

%************************************************************************
%*									*
\subsection[Id-type-funs]{Type-related @Id@ functions}
%*									*
%************************************************************************

\begin{code}
idType :: GenId ty -> ty

idType (Id _ _ ty _ _ _) = ty
\end{code}

\begin{code}
{-LATER:
getMentionedTyConsAndClassesFromId :: Id -> (Bag TyCon, Bag Class)

getMentionedTyConsAndClassesFromId id
 = getMentionedTyConsAndClassesFromType (idType id)
-}
\end{code}

\begin{code}
idPrimRep i = typePrimRep (idType i)
\end{code}

\begin{code}
{-LATER:
getInstIdModule (Id _ _ _ (DictFunId _ _ mod)) = mod
getInstIdModule (Id _ _ _ (ConstMethodId _ _ _ mod)) = mod
getInstIdModule other = panic "Id:getInstIdModule"
-}
\end{code}

%************************************************************************
%*									*
\subsection[Id-overloading]{Functions related to overloading}
%*									*
%************************************************************************

\begin{code}
mkSuperDictSelId u c sc ty info
  = mk_classy_id (SuperDictSelId c sc) SLIT("sdsel") (Left (origName "mkSuperDictSelId" sc)) u c ty info

mkMethodSelId u rec_c op ty info
  = mk_classy_id (MethodSelId rec_c op) SLIT("meth") (Right (classOpString op)) u rec_c ty info

mkDefaultMethodId u rec_c op gen ty info
  = mk_classy_id (DefaultMethodId rec_c op gen) SLIT("defm") (Right (classOpString op)) u rec_c ty info

mk_classy_id details str op_str u rec_c ty info
  = Id u n ty details NoPragmaInfo info
  where
    cname = getName rec_c -- we get other info out of here
    cname_orig = origName "mk_classy_id" cname
    cmod = moduleOf cname_orig

    n = mkCompoundName u cmod str [Left cname_orig, op_str] cname

mkDictFunId u c ity full_ty from_here locn mod info
  = Id u n full_ty (DictFunId c ity mod) NoPragmaInfo info
  where
    n = mkCompoundName2 u mod SLIT("dfun") (Left (origName "mkDictFunId" c) : renum_type_string full_ty ity) from_here locn

mkConstMethodId	u c op ity full_ty from_here locn mod info
  = Id u n full_ty (ConstMethodId c ity op mod) NoPragmaInfo info
  where
    n = mkCompoundName2 u mod SLIT("const") (Left (origName "mkConstMethodId" c) : Right (classOpString op) : renum_type_string full_ty ity) from_here locn

renum_type_string full_ty ity
  = initNmbr (
	nmbrType full_ty    `thenNmbr` \ _ -> -- so all the tyvars get added to renumbering...
	nmbrType ity	    `thenNmbr` \ rn_ity ->
	returnNmbr (getTypeString rn_ity)
    )

mkWorkerId u unwrkr ty info
  = Id u n ty (WorkerId unwrkr) NoPragmaInfo info
  where
    unwrkr_name = getName unwrkr
    unwrkr_orig = trace "mkWorkerId:origName:" $ origName "mkWorkerId" unwrkr_name
    umod = moduleOf unwrkr_orig

    n = mkCompoundName u umod SLIT("wrk") [Left unwrkr_orig] unwrkr_name

mkInstId u ty name = Id u (changeUnique name u) ty (InstId (no_free_tvs ty)) NoPragmaInfo noIdInfo

{-LATER:
getConstMethodId clas op ty
  = -- constant-method info is hidden in the IdInfo of
    -- the class-op id (as mentioned up above).
    let
	sel_id = getMethodSelId clas op
    in
    case (lookupConstMethodId (getIdSpecialisation sel_id) ty) of
      Just xx -> xx
      Nothing -> pprError "ERROR: getConstMethodId:" (ppAboves [
	ppCat [ppr PprDebug ty, ppr PprDebug ops, ppr PprDebug op_ids,
	       ppr PprDebug sel_id],
	ppStr "(This can arise if an interface pragma refers to an instance",
	ppStr "but there is no imported interface which *defines* that instance.",
	ppStr "The info above, however ugly, should indicate what else you need to import."
	])
-}
\end{code}

%************************************************************************
%*									*
\subsection[local-funs]{@LocalId@-related functions}
%*									*
%************************************************************************

\begin{code}
mkImported  n ty info = Id (nameUnique n) n ty ImportedId NoPragmaInfo info

{-LATER:
updateIdType :: Id -> Type -> Id
updateIdType (Id u n _ info details) ty = Id u n ty info details
-}
\end{code}

\begin{code}
type MyTy a b = GenType (GenTyVar a) b
type MyId a b = GenId (MyTy a b)

no_free_tvs ty = isEmptyTyVarSet (tyVarsOfType ty)

-- SysLocal: for an Id being created by the compiler out of thin air...
-- UserLocal: an Id with a name the user might recognize...
mkSysLocal, mkUserLocal :: FAST_STRING -> Unique -> MyTy a b -> SrcLoc -> MyId a b

mkSysLocal str uniq ty loc
  = Id uniq (mkLocalName uniq str True{-emph uniq-} loc) ty (SysLocalId (no_free_tvs ty)) NoPragmaInfo noIdInfo

mkUserLocal str uniq ty loc
  = Id uniq (mkLocalName uniq str False{-emph name-} loc) ty (LocalId (no_free_tvs ty)) NoPragmaInfo noIdInfo

-- mkUserId builds a local or top-level Id, depending on the name given
mkUserId :: Name -> MyTy a b -> PragmaInfo -> MyId a b
mkUserId name ty pragma_info
  | isLocalName name
  = Id (nameUnique name) name ty (LocalId (no_free_tvs ty)) pragma_info noIdInfo
  | otherwise
  = Id (nameUnique name) name ty 
	(if isLocallyDefinedName name then TopLevId else ImportedId)
	pragma_info noIdInfo
\end{code}


\begin{code}
{-LATER:

-- for a SpecPragmaId being created by the compiler out of thin air...
mkSpecPragmaId :: FAST_STRING -> Unique -> Type -> Maybe Id -> SrcLoc -> Id
mkSpecPragmaId str uniq ty specid loc
  = Id uniq (mkShortName str loc) ty noIdInfo (SpecPragmaId specid (no_free_tvs ty))

-- for new SpecId
mkSpecId u unspec ty_maybes ty info
  = ASSERT(not (maybeToBool (isSpecId_maybe unspec)))
    Id u n ty info (SpecId unspec ty_maybes (no_free_tvs ty))

-- Specialised version of constructor: only used in STG and code generation
-- Note: The specialsied Id has the same unique as the unspeced Id

mkSameSpecCon ty_maybes unspec@(Id u n ty info details)
  = ASSERT(isDataCon unspec)
    ASSERT(not (maybeToBool (isSpecId_maybe unspec)))
    Id u n new_ty info (SpecId unspec ty_maybes (no_free_tvs new_ty))
  where
    new_ty = specialiseTy ty ty_maybes 0

localiseId :: Id -> Id
localiseId id@(Id u n ty info details)
  = Id u (mkShortName name loc) ty info (LocalId (no_free_tvs ty))
  where
    name = getOccName id
    loc  = getSrcLoc id
-}

mkIdWithNewUniq :: Id -> Unique -> Id

mkIdWithNewUniq (Id _ n ty details prag info) u
  = Id u (changeUnique n u) ty details prag info
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

{-LATER:
selectIdInfoForSpecId :: Id -> IdInfo
selectIdInfoForSpecId unspec
  = ASSERT(not (maybeToBool (isSpecId_maybe unspec)))
    noIdInfo `addInfo_UF` getIdUnfolding unspec
-}
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
  = --ASSERT( not (isDataCon id))
    getInfo id_info

dataConArity, dataConNumFields :: DataCon -> Int

dataConArity id@(Id _ _ _ _ _ id_info)
  = ASSERT(isDataCon id)
    case (arityMaybe (getInfo id_info)) of
      Just  i -> i
      Nothing -> pprPanic "dataConArity:Nothing:" (pprId PprDebug id)

dataConNumFields id
  = ASSERT(isDataCon id)
    case (dataConSig id) of { (_, _, arg_tys, _) ->
    length arg_tys }

isNullaryDataCon con = dataConNumFields con == 0 -- function of convenience

addIdArity :: Id -> Int -> Id
addIdArity (Id u n ty details pinfo info) arity
  = Id u n ty details pinfo (info `addInfo` (mkArityInfo arity))
\end{code}

%************************************************************************
%*									*
\subsection[constructor-funs]{@DataCon@-related functions (incl.~tuples)}
%*									*
%************************************************************************

\begin{code}
mkDataCon :: Name
	  -> [StrictnessMark] -> [FieldLabel]
	  -> [TyVar] -> ThetaType -> [TauType] -> TyCon
--ToDo:   -> SpecEnv
	  -> Id
  -- can get the tag and all the pieces of the type from the Type

mkDataCon n stricts fields tvs ctxt args_tys tycon
  = ASSERT(length stricts == length args_tys)
    data_con
  where
    -- NB: data_con self-recursion; should be OK as tags are not
    -- looked at until late in the game.
    data_con
      = Id (nameUnique n)
	   n
	   type_of_constructor
	   (DataConId data_con_tag stricts fields tvs ctxt args_tys tycon)
	   IWantToBeINLINEd	-- Always inline constructors if possible
	   datacon_info

    data_con_tag    = position_within fIRST_TAG data_con_family

    data_con_family = tyConDataCons tycon

    position_within :: Int -> [Id] -> Int

    position_within acc (c:cs)
      = if c == data_con then acc else position_within (acc+1) cs
#ifdef DEBUG
    position_within acc []
      = panic "mkDataCon: con not found in family"
#endif

    type_of_constructor
      = mkSigmaTy tvs ctxt
	(mkFunTys args_tys (applyTyCon tycon (mkTyVarTys tvs)))

    datacon_info = noIdInfo `addInfo_UF` unfolding
			    `addInfo` mkArityInfo arity
--ToDo: 		    `addInfo` specenv

    arity = length ctxt + length args_tys

    unfolding
      = noInfo_UF
{- LATER:
      = -- if arity == 0
    	-- then noIdInfo
	-- else -- do some business...
	let
	    (tyvars, dict_vars, vars) = mk_uf_bits tvs ctxt args_tys tycon
	    tyvar_tys = mkTyVarTys tyvars
	in
	case (Con data_con tyvar_tys [VarArg v | v <- vars]) of { plain_Con ->

	mkUnfolding EssentialUnfolding -- for data constructors
		    (mkLam tyvars (dict_vars ++ vars) plain_Con)
	}

    mk_uf_bits tvs ctxt arg_tys tycon
      = let
	    (inst_env, tyvars, tyvar_tys)
	      = instantiateTyVarTemplates tvs
					  (map uniqueOf tvs)
	in
	    -- the "context" and "arg_tys" have TyVarTemplates in them, so
	    -- we instantiate those types to have the right TyVars in them
	    -- instead.
	case (map (instantiateTauTy inst_env) (map ctxt_ty ctxt))
						       	of { inst_dict_tys ->
	case (map (instantiateTauTy inst_env) arg_tys) 	of { inst_arg_tys ->

	    -- We can only have **ONE** call to mkTemplateLocals here;
	    -- otherwise, we get two blobs of locals w/ mixed-up Uniques
	    -- (Mega-Sigh) [ToDo]
	case (mkTemplateLocals (inst_dict_tys ++ inst_arg_tys)) of { all_vars ->

	case (splitAt (length ctxt) all_vars)	of { (dict_vars, vars) ->

	(tyvars, dict_vars, vars)
	}}}}
      where
	-- these are really dubious Types, but they are only to make the
	-- binders for the lambdas for tossed-away dicts.
	ctxt_ty (clas, ty) = mkDictTy clas ty
-}
\end{code}

\begin{code}
mkTupleCon :: Arity -> Id

mkTupleCon arity
  = Id unique n ty (TupleConId arity) NoPragmaInfo tuplecon_info 
  where
    n		= mkTupleDataConName arity
    unique      = uniqueOf n
    ty 		= mkSigmaTy tyvars []
		   (mkFunTys tyvar_tys (applyTyCon tycon tyvar_tys))
    tycon	= mkTupleTyCon arity
    tyvars	= take arity alphaTyVars
    tyvar_tys	= mkTyVarTys tyvars

    tuplecon_info
      = noIdInfo `addInfo_UF` unfolding
		 `addInfo` mkArityInfo arity
--LATER:?	 `addInfo` panic "Id:mkTupleCon:pcGenerateTupleSpecs arity ty"

    unfolding
      = noInfo_UF
{- LATER:
      = -- if arity == 0
    	-- then noIdInfo
	-- else -- do some business...
	let
	    (tyvars, dict_vars, vars) = mk_uf_bits arity
	    tyvar_tys = mkTyVarTys tyvars
	in
	case (Con data_con tyvar_tys [VarArg v | v <- vars]) of { plain_Con ->
	mkUnfolding
	    EssentialUnfolding    -- data constructors
	    (mkLam tyvars (dict_vars ++ vars) plain_Con) }

    mk_uf_bits arity
      = case (mkTemplateLocals tyvar_tys) of { vars ->
	(tyvars, [], vars) }
      where
	tyvar_tmpls	= take arity alphaTyVars
	(_, tyvars, tyvar_tys) = instantiateTyVarTemplates tyvar_tmpls (map uniqueOf tyvar_tmpls)
-}

fIRST_TAG :: ConTag
fIRST_TAG =  1	-- Tags allocated from here for real constructors
\end{code}

\begin{code}
dataConTag :: DataCon -> ConTag	-- will panic if not a DataCon
dataConTag (Id _ _ _ (DataConId tag _ _ _ _ _ _) _ _) = tag
dataConTag (Id _ _ _ (TupleConId _) _ _)	      = fIRST_TAG
dataConTag (Id _ _ _ (SpecId unspec _ _) _ _)	      = dataConTag unspec

dataConTyCon :: DataCon -> TyCon	-- will panic if not a DataCon
dataConTyCon (Id _ _ _ (DataConId _ _ _ _ _ _ tycon) _ _) = tycon
dataConTyCon (Id _ _ _ (TupleConId a) _ _)	          = mkTupleTyCon a

dataConSig :: DataCon -> ([TyVar], ThetaType, [TauType], TyCon)
					-- will panic if not a DataCon

dataConSig (Id _ _ _ (DataConId _ _ _ tyvars theta_ty arg_tys tycon) _ _)
  = (tyvars, theta_ty, arg_tys, tycon)

dataConSig (Id _ _ _ (TupleConId arity) _ _)
  = (tyvars, [], tyvar_tys, mkTupleTyCon arity)
  where
    tyvars	= take arity alphaTyVars
    tyvar_tys	= mkTyVarTys tyvars

dataConFieldLabels :: DataCon -> [FieldLabel]
dataConFieldLabels (Id _ _ _ (DataConId _ _ fields _ _ _ _) _ _) = fields
dataConFieldLabels (Id _ _ _ (TupleConId _)		    _ _) = []

dataConStrictMarks :: DataCon -> [StrictnessMark]
dataConStrictMarks (Id _ _ _ (DataConId _ stricts _ _ _ _ _) _ _) = stricts
dataConStrictMarks (Id _ _ _ (TupleConId arity)		     _ _) 
  = nOfThem arity NotMarkedStrict

dataConRawArgTys :: DataCon -> [TauType] -- a function of convenience
dataConRawArgTys con = case (dataConSig con) of { (_,_, arg_tys,_) -> arg_tys }

dataConArgTys :: DataCon 
	      -> [Type] 	-- Instantiated at these types
	      -> [Type]		-- Needs arguments of these types
dataConArgTys con_id inst_tys
 = map (instantiateTy tenv) arg_tys
 where
    (tyvars, _, arg_tys, _) = dataConSig con_id
    tenv 		    = zipEqual "dataConArgTys" tyvars inst_tys
\end{code}

\begin{code}
mkRecordSelId field_label selector_ty
  = Id (nameUnique name)
       name
       selector_ty
       (RecordSelId field_label)
       NoPragmaInfo
       noIdInfo
  where
    name = fieldLabelName field_label

recordSelectorFieldLabel :: Id -> FieldLabel
recordSelectorFieldLabel (Id _ _ _ (RecordSelId lbl) _ _) = lbl
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
getIdUnfolding :: Id -> UnfoldingDetails

getIdUnfolding (Id _ _ _ _ _ info) = getInfo_UF info

{-LATER:
addIdUnfolding :: Id -> UnfoldingDetails -> Id
addIdUnfolding id@(Id u n ty info details) unfold_details
  = ASSERT(
    	case (isLocallyDefined id, unfold_details) of
	(_,     NoUnfoldingDetails) -> True
	(True,  IWantToBeINLINEd _) -> True
	(False, IWantToBeINLINEd _) -> False -- v bad
	(False, _)  	    	    -> True
	_   	    	    	    -> False -- v bad
    )
    Id u n ty (info `addInfo_UF` unfold_details) details
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

%************************************************************************
%*									*
\subsection[IdInfo-funs]{Functions related to @Ids@' @IdInfos@}
%*									*
%************************************************************************

\begin{code}
getIdDemandInfo :: Id -> DemandInfo
getIdDemandInfo (Id _ _ _ _ _ info) = getInfo info

addIdDemandInfo :: Id -> DemandInfo -> Id
addIdDemandInfo (Id u n ty details prags info) demand_info
  = Id u n ty details prags (info `addInfo` demand_info)
\end{code}

\begin{code}
getIdUpdateInfo :: Id -> UpdateInfo
getIdUpdateInfo (Id _ _ _ _ _ info) = getInfo info

addIdUpdateInfo :: Id -> UpdateInfo -> Id
addIdUpdateInfo (Id u n ty details prags info) upd_info
  = Id u n ty details prags (info `addInfo` upd_info)
\end{code}

\begin{code}
{- LATER:
getIdArgUsageInfo :: Id -> ArgUsageInfo
getIdArgUsageInfo (Id u n ty info details) = getInfo info

addIdArgUsageInfo :: Id -> ArgUsageInfo -> Id
addIdArgUsageInfo (Id u n ty info details) au_info
  = Id u n ty (info `addInfo` au_info) details
-}
\end{code}

\begin{code}
{- LATER:
getIdFBTypeInfo :: Id -> FBTypeInfo
getIdFBTypeInfo (Id u n ty info details) = getInfo info

addIdFBTypeInfo :: Id -> FBTypeInfo -> Id
addIdFBTypeInfo (Id u n ty info details) upd_info
  = Id u n ty (info `addInfo` upd_info) details
-}
\end{code}

\begin{code}
{- LATER:
getIdSpecialisation :: Id -> SpecEnv
getIdSpecialisation (Id _ _ _ _ _ info) = getInfo info

addIdSpecialisation :: Id -> SpecEnv -> Id
addIdSpecialisation (Id u n ty details prags info) spec_info
  = Id u n ty details prags (info `addInfo` spec_info)
-}
\end{code}

Strictness: we snaffle the info out of the IdInfo.

\begin{code}
getIdStrictness :: Id -> StrictnessInfo

getIdStrictness (Id _ _ _ _ _ info) = getInfo info

addIdStrictness :: Id -> StrictnessInfo -> Id

addIdStrictness (Id u n ty details prags info) strict_info
  = Id u n ty details prags (info `addInfo` strict_info)
\end{code}

%************************************************************************
%*									*
\subsection[Id-comparison]{Comparison functions for @Id@s}
%*									*
%************************************************************************

Comparison: equality and ordering---this stuff gets {\em hammered}.

\begin{code}
cmpId (Id u1 _ _ _ _ _) (Id u2 _ _ _ _ _) = cmp u1 u2
-- short and very sweet
\end{code}

\begin{code}
instance Ord3 (GenId ty) where
    cmp = cmpId

instance Eq (GenId ty) where
    a == b = case (a `cmp` b) of { EQ_ -> True;  _ -> False }
    a /= b = case (a `cmp` b) of { EQ_ -> False; _ -> True  }

instance Ord (GenId ty) where
    a <= b = case (a `cmp` b) of { LT_ -> True;  EQ_ -> True;  GT__ -> False }
    a <	 b = case (a `cmp` b) of { LT_ -> True;  EQ_ -> False; GT__ -> False }
    a >= b = case (a `cmp` b) of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case (a `cmp` b) of { LT_ -> False; EQ_ -> False; GT__ -> True  }
    _tagCmp a b = case (a `cmp` b) of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
\end{code}

@cmpId_withSpecDataCon@ ensures that any spectys are taken into
account when comparing two data constructors. We need to do this
because a specialised data constructor has the same Unique as its
unspecialised counterpart.

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

cmpEqDataCon (Id _ _ _ (SpecId _ mtys1 _) _ _) (Id _ _ _ (SpecId _ mtys2 _) _ _)
  = panic# "Id.cmpEqDataCon:cmpUniTypeMaybeList mtys1 mtys2"

cmpEqDataCon _ (Id _ _ _ (SpecId _ _ _) _ _) = LT_
cmpEqDataCon (Id _ _ _ (SpecId _ _ _) _ _) _ = GT_
cmpEqDataCon _				   _ = EQ_
\end{code}

%************************************************************************
%*									*
\subsection[Id-other-instances]{Other instance declarations for @Id@s}
%*									*
%************************************************************************

\begin{code}
instance Outputable ty => Outputable (GenId ty) where
    ppr sty id = pprId sty id

-- and a SPECIALIZEd one:
instance Outputable {-Id, i.e.:-}(GenId Type) where
    ppr sty id = pprId sty id

showId :: PprStyle -> Id -> String
showId sty id = ppShow 80 (pprId sty id)
\end{code}

Default printing code (not used for interfaces):
\begin{code}
pprId :: Outputable ty => PprStyle -> GenId ty -> Pretty

pprId sty (Id u n _ _ _ _) = ppr sty n
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
modifyIdEnv	  :: IdEnv a -> (a -> a) -> GenId ty -> IdEnv a
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

modifyIdEnv env mangle_fn key
  = case (lookupIdEnv env key) of
      Nothing -> env
      Just xx -> addOneToIdEnv env key (mangle_fn xx)
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

\begin{code}
addId, nmbrId, nmbrDataCon :: Id -> NmbrM Id

addId id@(Id u n ty det prag info) nenv@(NmbrEnv ui ut uu idenv tvenv uvenv)
  = case (lookupUFM_Directly idenv u) of
      Just xx -> trace "addId: already in map!" $
		 (nenv, xx)
      Nothing ->
	if toplevelishId id then
	    trace "addId: can't add toplevelish!" $
	    (nenv, id)
	else -- alloc a new unique for this guy
	     -- and add an entry in the idenv
	     -- NB: *** KNOT-TYING ***
	    let
		nenv_plus_id	= NmbrEnv (incrUnique ui) ut uu
					  (addToUFM_Directly idenv u new_id)
					  tvenv uvenv

		(nenv2, new_ty)  = nmbrType     ty  nenv_plus_id
		(nenv3, new_det) = nmbr_details det nenv2

		new_id = Id ui n new_ty new_det prag info
	    in
	    (nenv3, new_id)

nmbrId id@(Id u n ty det prag info) nenv@(NmbrEnv ui ut uu idenv tvenv uvenv)
  = case (lookupUFM_Directly idenv u) of
      Just xx -> (nenv, xx)
      Nothing ->
	if not (toplevelishId id) then
	    trace "nmbrId: lookup failed" $
	    (nenv, id)
	else
	    let
		(nenv2, new_ty)  = nmbrType     ty  nenv
		(nenv3, new_det) = nmbr_details det nenv2

		new_id = Id u n new_ty new_det prag info
	    in
	    (nenv3, new_id)

    -- used when renumbering TyCons to produce data decls...
nmbrDataCon id@(Id _ _ _ (TupleConId _) _ _) nenv
  = (nenv, id) -- nothing to do for tuples

nmbrDataCon id@(Id u n ty (DataConId tag marks fields tvs theta arg_tys tc) prag info) nenv@(NmbrEnv ui ut uu idenv tvenv uvenv)
  = case (lookupUFM_Directly idenv u) of
      Just xx -> trace "nmbrDataCon: in env???\n" (nenv, xx)
      Nothing ->
	let
	    (nenv2, new_fields)  = (mapNmbr nmbrField  fields)  nenv
	    (nenv3, new_arg_tys) = (mapNmbr nmbrType   arg_tys) nenv2

	    new_det = DataConId tag marks new_fields (bottom "tvs") (bottom "theta") new_arg_tys tc
	    new_id  = Id u n (bottom "ty") new_det prag info
	in
	(nenv3, new_id)
  where
    bottom msg = panic ("nmbrDataCon"++msg)

------------
nmbr_details :: IdDetails -> NmbrM IdDetails

nmbr_details (DataConId tag marks fields tvs theta arg_tys tc)
  = mapNmbr nmbrTyVar  tvs	`thenNmbr` \ new_tvs ->
    mapNmbr nmbrField  fields	`thenNmbr` \ new_fields ->
    mapNmbr nmbr_theta theta	`thenNmbr` \ new_theta ->
    mapNmbr nmbrType   arg_tys	`thenNmbr` \ new_arg_tys ->
    returnNmbr (DataConId tag marks new_fields new_tvs new_theta new_arg_tys tc)
  where
    nmbr_theta (c,t)
      = --nmbrClass c	`thenNmbr` \ new_c ->
        nmbrType  t	`thenNmbr` \ new_t ->
	returnNmbr (c, new_t)

    -- ToDo:add more cases as needed
nmbr_details other_details = returnNmbr other_details

------------
nmbrField (FieldLabel n ty tag)
  = nmbrType ty `thenNmbr` \ new_ty ->
    returnNmbr (FieldLabel n new_ty tag)
\end{code}
