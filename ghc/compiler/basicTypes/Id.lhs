%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Id]{@Ids@: Value and constructor identifiers}

\begin{code}
#include "HsVersions.h"

module Id {- (
	GenId, Id(..),		-- Abstract
	StrictnessMark(..),	-- An enumaration
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
	mkMethodSelId, mkSuperDictSelId, mkDefaultMethodId,
	mkConstMethodId, getConstMethodId,

	updateIdType,
	mkId, mkDictFunId, mkInstId,
	mkWorkerId,
	localiseId,

	-- DESTRUCTION
	idType,
	getIdInfo, replaceIdInfo,
	getPragmaInfo,
	getIdPrimRep, getInstIdModule,
	getMentionedTyConsAndClassesFromId,
	getDataConTag,
	getDataConSig, getInstantiatedDataConSig,

	getDataConTyCon,

	-- PREDICATES
	isDataCon, isTupleCon,
	isSpecId_maybe, isSpecPragmaId_maybe,
	toplevelishId, externallyVisibleId,
	isTopLevId, isWorkerId, isWrapperId,
	isImportedId, isSysLocalId,
	isBottomingId,
	isMethodSelId, isDefaultMethodId_maybe, isSuperDictSelId_maybe,
	isDictFunId,
--???	isInstId_maybe,
	isConstMethodId_maybe,
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
	getIdUnfolding, addIdUnfolding,
	getIdUpdateInfo, addIdUpdateInfo,
	getIdArgUsageInfo, addIdArgUsageInfo,
	getIdFBTypeInfo, addIdFBTypeInfo,
	-- don't export the types, lest OptIdInfo be dragged in!

	-- MISCELLANEOUS
	unlocaliseId,
	fIRST_TAG,
	showId,
	pprIdInUnfolding,

	-- "Environments" keyed off of Ids, and sets of Ids
	IdEnv(..),
	lookupIdEnv, lookupNoFailIdEnv, nullIdEnv, unitIdEnv, mkIdEnv,
	growIdEnv, growIdEnvList, isNullIdEnv, addOneToIdEnv,
	delOneFromIdEnv, delManyFromIdEnv, modifyIdEnv, combineIdEnvs,
	rngIdEnv, mapIdEnv,

	-- and to make the interface self-sufficient...
	GenIdSet(..), IdSet(..)
    )-} where

import Ubiq
import IdLoop   -- for paranoia checking
import TyLoop   -- for paranoia checking
import NameLoop -- for paranoia checking

import Bag
import Class		( getClassOpString, Class(..), GenClass, ClassOp(..), GenClassOp )
import IdInfo
import Maybes		( maybeToBool )
import NameTypes	( mkShortName, fromPrelude, FullName, ShortName )
import Name		( Name(..) )
import Outputable	( isAvarop, isAconop, getLocalName,
			  isExported, ExportFlag(..) )
import PragmaInfo	( PragmaInfo(..) )
import PrelMods		( pRELUDE_BUILTIN )
import PprType		( GenType, GenTyVar,
			  getTypeString, typeMaybeString, specMaybeTysSuffix )
import PprStyle
import Pretty
import SrcLoc		( mkBuiltinSrcLoc )
import TyCon		( TyCon, mkTupleTyCon, getTyConDataCons )
import Type		( mkSigmaTy, mkTyVarTys, mkFunTys, mkDictTy,
			  applyTyCon, isPrimType, instantiateTy,
			  tyVarsOfType,
			  GenType, ThetaType(..), TauType(..), Type(..)
			)
import TyVar		( GenTyVar, alphaTyVars, isEmptyTyVarSet )
import UniqFM
import UniqSet		-- practically all of it
import Unique		( Unique, mkTupleDataConUnique, pprUnique, showUnique )
import Util		( mapAccumL, nOfThem, panic, pprPanic, assertPanic )
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
	ty		-- Id's type; used all the time;
	IdDetails	-- Stuff about individual kinds of Ids.
	PragmaInfo	-- Properties of this Id requested by programmer
			-- eg specialise-me, inline-me
	IdInfo		-- Properties of this Id deduced by compiler
				   
type Id = GenId Type

data StrictnessMark = MarkedStrict | NotMarkedStrict

data IdDetails

  ---------------- Local values

  = LocalId	ShortName	-- mentioned by the user
		Bool		-- True <=> no free type vars

  | SysLocalId	ShortName	-- made up by the compiler
		Bool		-- as for LocalId

  | SpecPragmaId ShortName	-- introduced by the compiler
		 (Maybe Id)	-- for explicit specid in pragma
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
		[StrictnessMark] -- Strict args; length = arity

		[TyVar] [(Class,Type)] [Type] TyCon
				-- the type is:
				-- forall tyvars . theta_ty =>
				--    unitype_1 -> ... -> unitype_n -> tycon tyvars

  | TupleConId	Int		-- Its arity

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
		Bool		-- True <=> from an instance decl in this mod
		FAST_STRING	-- module where instance came from

				-- see below
  | ConstMethodId		-- A method which depends only on the type of the
				-- instance, and not on any further dictionaries etc.
		Class		-- Uniquely identified by:
		Type		-- (class, type, classop) triple
		ClassOp
		Bool		-- True <=> from an instance decl in this mod
		FAST_STRING	-- module where instance came from

  | InstId	ShortName	-- An instance of a dictionary, class operation,
				-- or overloaded value

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
unsafeGenId2Id (Id u ty d p i) = Id u (panic "unsafeGenId2Id:ty") d p i

isDataCon id = is_data (unsafeGenId2Id id)
 where
  is_data (Id _ _ (DataConId _ _ _ _ _ _ _) _ _) = True
  is_data (Id _ _ (TupleConId _) _ _)		 = True
  is_data (Id _ _ (SpecId unspec _ _) _ _)	 = is_data unspec
  is_data other					 = False


isTupleCon id = is_tuple (unsafeGenId2Id id)
 where
  is_tuple (Id _ _ (TupleConId _) _ _)		 = True
  is_tuple (Id _ _ (SpecId unspec _ _) _ _)	 = is_tuple unspec
  is_tuple other				 = False

{-LATER:
isSpecId_maybe (Id _ _ (SpecId unspec ty_maybes _) _ _)
  = ASSERT(not (maybeToBool (isSpecId_maybe unspec)))
    Just (unspec, ty_maybes)
isSpecId_maybe other_id
  = Nothing

isSpecPragmaId_maybe (Id _ _ (SpecPragmaId _ specid _) _ _)
  = Just specid
isSpecPragmaId_maybe other_id
  = Nothing
-}
\end{code}

@toplevelishId@ tells whether an @Id@ {\em may} be defined in a
nested @let(rec)@ (returns @False@), or whether it is {\em sure} to be
defined at top level (returns @True@).	This is used to decide whether
the @Id@ is a candidate free variable.	NB: you are only {\em sure}
about something if it returns @True@!

\begin{code}
toplevelishId	    :: Id -> Bool
idHasNoFreeTyVars   :: Id -> Bool

toplevelishId (Id _ _ details _ _)
  = chk details
  where
    chk (DataConId _ _ _ _ _ _ _) = True
    chk (TupleConId _)	    	  = True
    chk (ImportedId _)	    	  = True
    chk (PreludeId  _)	    	  = True
    chk (TopLevId   _)	    	  = True	-- NB: see notes
    chk (SuperDictSelId _ _)	  = True
    chk (MethodSelId _ _)	  = True
    chk (DefaultMethodId _ _ _)   = True
    chk (DictFunId     _ _ _ _)	  = True
    chk (ConstMethodId _ _ _ _ _) = True
    chk (SpecId unspec _ _)	  = toplevelishId unspec
				  -- depends what the unspecialised thing is
    chk (WorkerId unwrkr)	  = toplevelishId unwrkr
    chk (InstId _)		  = False	-- these are local
    chk (LocalId      _ _)	  = False
    chk (SysLocalId   _ _)	  = False
    chk (SpecPragmaId _ _ _)	  = False

idHasNoFreeTyVars (Id _ _ details _ info)
  = chk details
  where
    chk (DataConId _ _ _ _ _ _ _) = True
    chk (TupleConId _)	    	  = True
    chk (ImportedId _)	    	  = True
    chk (PreludeId  _)	    	  = True
    chk (TopLevId   _)	    	  = True
    chk (SuperDictSelId _ _)	  = True
    chk (MethodSelId _ _)	  = True
    chk (DefaultMethodId _ _ _)   = True
    chk (DictFunId     _ _ _ _)	  = True
    chk (ConstMethodId _ _ _ _ _) = True
    chk (WorkerId unwrkr)	  = idHasNoFreeTyVars unwrkr
    chk (InstId _)		  = False	-- these are local
    chk (SpecId _     _   no_free_tvs) = no_free_tvs
    chk (LocalId      _   no_free_tvs) = no_free_tvs
    chk (SysLocalId   _   no_free_tvs) = no_free_tvs
    chk (SpecPragmaId _ _ no_free_tvs) = no_free_tvs
\end{code}

\begin{code}
isTopLevId (Id _ _ (TopLevId _) _ _) = True
isTopLevId other		     = False

isImportedId (Id _ _ (ImportedId _) _ _) = True
isImportedId other		  	 = False

isBottomingId (Id _ _ _ _ info) = panic "isBottomingId not implemented"
			-- LATER: bottomIsGuaranteed (getInfo info)

isSysLocalId (Id _ _ (SysLocalId _ _) _ _) = True
isSysLocalId other			   = False

isSpecPragmaId (Id _ _ (SpecPragmaId _ _ _) _ _) = True
isSpecPragmaId other			         = False

isMethodSelId (Id _ _ (MethodSelId _ _) _ _) = True
isMethodSelId _				 = False

isDefaultMethodId (Id _ _ (DefaultMethodId _ _ _) _ _) = True
isDefaultMethodId other				       = False

isDefaultMethodId_maybe (Id _ _ (DefaultMethodId cls clsop err) _ _)
  = Just (cls, clsop, err)
isDefaultMethodId_maybe other = Nothing

isDictFunId (Id _ _ (DictFunId _ _ _ _) _ _) = True
isDictFunId other		    	     = False

isConstMethodId (Id _ _ (ConstMethodId _ _ _ _ _) _ _) = True
isConstMethodId other		    		       = False

isConstMethodId_maybe (Id _ _ (ConstMethodId cls ty clsop _ _) _ _)
  = Just (cls, ty, clsop)
isConstMethodId_maybe other = Nothing

isSuperDictSelId_maybe (Id _ _ (SuperDictSelId c sc) _ _) = Just (c, sc)
isSuperDictSelId_maybe other_id				  = Nothing

isWorkerId (Id _ _ (WorkerId _) _ _) = True
isWorkerId other		     = False

{-LATER:
isWrapperId id = workerExists (getIdStrictness id)
-}
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
	pprUnique (getItsUnique v)

    -- ubiquitous Ids with special syntax:
    else if v == nilDataCon then
	ppPStr SLIT("_NIL_")
    else if isTupleCon v then
	ppBeside (ppPStr SLIT("_TUP_")) (ppInt (getDataConArity v))

    -- ones to think about:
    else
	let
	    (Id _ _ v_details _ _) = v
	in
    	case v_details of
	    -- these ones must have been exported by their original module
	  ImportedId   _ -> pp_full_name
	  PreludeId    _ -> pp_full_name

	    -- these ones' exportedness checked later...
	  TopLevId  _ -> pp_full_name
	  DataConId _ _ _ _ _ _ _ -> pp_full_name

	    -- class-ish things: class already recorded as "mentioned"
	  SuperDictSelId c sc
	    -> ppCat [ppPStr SLIT("_SDSEL_"), pp_class c, pp_class sc]
	  MethodSelId c o
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
	    (Id _ _ v_details _ _) = v
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

myWrapperMaybe (Id _ _ (WorkerId my_wrapper) _ _) = Just my_wrapper
myWrapperMaybe other_id			    	  = Nothing
-}
\end{code}

\begin{code}
unfoldingUnfriendlyId	-- return True iff it is definitely a bad
	:: Id		-- idea to export an unfolding that
	-> Bool		-- mentions this Id.  Reason: it cannot
			-- possibly be seen in another module.

unfoldingUnfriendlyId id = panic "Id.unfoldingUnfriendlyId"
{-LATER:

unfoldingUnfriendlyId id
  | not (externallyVisibleId id) -- that settles that...
  = True

unfoldingUnfriendlyId (Id _ _ (WorkerId wrapper) _ _)
  = class_thing wrapper
  where
    -- "class thing": If we're going to use this worker Id in
    -- an interface, we *have* to be able to untangle the wrapper's
    -- strictness when reading it back in.  At the moment, this
    -- is not always possible: in precisely those cases where
    -- we pass tcGenPragmas a "Nothing" for its "ty_maybe".

    class_thing (Id _ _ (SuperDictSelId _ _) _ _)    = True
    class_thing (Id _ _ (MethodSelId _ _) _ _)  	   = True
    class_thing (Id _ _ (DefaultMethodId _ _ _) _ _) = True
    class_thing other				   = False

unfoldingUnfriendlyId (Id _ _ (SpecId d@(Id _ _ _ dfun@(DictFunId _ t _ _)) _ _) _ _)
    -- a SPEC of a DictFunId can end up w/ gratuitous
    -- TyVar(Templates) in the i/face; only a problem
    -- if -fshow-pragma-name-errs; but we can do without the pain.
    -- A HACK in any case (WDP 94/05/02)
  = --pprTrace "unfriendly1:" (ppCat [ppr PprDebug d, ppr PprDebug t]) (
    naughty_DictFunId dfun
    --)

unfoldingUnfriendlyId d@(Id _ _ dfun@(DictFunId _ t _ _) _ _)
  = --pprTrace "unfriendly2:" (ppCat [ppr PprDebug d, ppr PprDebug t]) (
    naughty_DictFunId dfun -- similar deal...
    --)

unfoldingUnfriendlyId other_id   = False -- is friendly in all other cases

naughty_DictFunId :: IdDetails -> Bool
    -- True <=> has a TyVar(Template) in the "type" part of its "name"

naughty_DictFunId (DictFunId _ _ False _) = False -- came from outside; must be OK
naughty_DictFunId (DictFunId _ ty _ _)
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

externallyVisibleId id = panic "Id.externallyVisibleId"
{-LATER:

externallyVisibleId id@(Id _ _ details _ _)
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

    weird_datacon (DataConId _ _ _ _ _ _ tycon)
      = maybeToBool (maybePurelyLocalTyCon tycon)
    weird_datacon not_a_datacon_therefore_not_weird = False

    weird_tuplecon (TupleConId arity)
      = arity > 32 -- sigh || isBigTupleTyCon tycon -- generated *purely* for local use
    weird_tuplecon _ = False
-}
\end{code}

\begin{code}
idWantsToBeINLINEd :: Id -> Bool

idWantsToBeINLINEd id
  = panic "Id.idWantsToBeINLINEd"
{- LATER:
  = case (getIdUnfolding id) of
      IWantToBeINLINEd _ -> True
      _ -> False
-}
\end{code}

For @unlocaliseId@: See the brief commentary in
\tr{simplStg/SimplStg.lhs}.

\begin{code}
{-LATER:
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

unlocaliseId mod (Id u ty info (InstId name))
  = Just (Id u ty info (TopLevId full_name))
	-- type might be wrong, but it hardly matters
	-- at this stage (just before printing C)  ToDo
  where
    name = getLocalName name
    full_name = mkFullName mod name InventedInThisModule ExportAll mkGeneratedSrcLoc

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
-}
\end{code}

CLAIM (not ASSERTed) for @applyTypeEnvToId@ and @applySubstToId@:
`Top-levelish Ids'' cannot have any free type variables, so applying
the type-env cannot have any effect.  (NB: checked in CoreLint?)

The special casing is in @applyTypeEnvToId@, not @apply_to_Id@, as the
former ``should be'' the usual crunch point.

\begin{code}
{-LATER:
applyTypeEnvToId :: TypeEnv -> Id -> Id

applyTypeEnvToId type_env id@(Id u ty info details)
  | idHasNoFreeTyVars id
  = id
  | otherwise
  = apply_to_Id ( \ ty ->
	applyTypeEnvToTy type_env ty
    ) id
-}
\end{code}

\begin{code}
{-LATER:
apply_to_Id :: (Type -> Type)
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

    apply_to_details other = other
-}
\end{code}

Sadly, I don't think the one using the magic typechecker substitution
can be done with @apply_to_Id@.  Here we go....

Strictness is very important here.  We can't leave behind thunks
with pointers to the substitution: it {\em must} be single-threaded.

\begin{code}
{-LATER:
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
-}
\end{code}

\begin{code}
getIdNamePieces :: Bool {-show Uniques-} -> GenId ty -> [FAST_STRING]
getIdNamePieces show_uniqs id
  = get (unsafeGenId2Id id)
  where
  get (Id u _ details _ _)
    = case details of
      DataConId n _ _ _ _ _ _ ->
	case (getOrigName n) of { (mod, name) ->
	if fromPrelude mod then [name] else [mod, name] }

      TupleConId 0 -> [SLIT("()")]
      TupleConId a -> [_PK_ ( "(" ++ nOfThem (a-1) ',' ++ ")" )]

      ImportedId n -> get_fullname_pieces n
      PreludeId  n -> get_fullname_pieces n
      TopLevId   n -> get_fullname_pieces n

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

      MethodSelId clas op ->
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
	get unspec ++ (if not (toplevelishId unspec)
		       then [showUnique u]
		       else concat (map typeMaybeString ty_maybes))

      WorkerId unwrkr ->
	get unwrkr ++ (if not (toplevelishId unwrkr)
		       then [showUnique u]
		       else [SLIT("wrk")])

      LocalId      n _   -> let local = getLocalName n in
			    if show_uniqs then [local, showUnique u] else [local]
      InstId       n     -> [getLocalName n, showUnique u]
      SysLocalId   n _   -> [getLocalName n, showUnique u]
      SpecPragmaId n _ _ -> [getLocalName n, showUnique u]

get_fullname_pieces :: FullName -> [FAST_STRING]
get_fullname_pieces n
  = BIND (getOrigName n) _TO_ (mod, name) ->
    if fromPrelude mod
    then [name]
    else [mod, name]
    BEND
\end{code}

%************************************************************************
%*									*
\subsection[Id-type-funs]{Type-related @Id@ functions}
%*									*
%************************************************************************

\begin{code}
idType :: GenId ty -> ty

idType (Id _ ty _ _ _) = ty
\end{code}

\begin{code}
{-LATER:
getMentionedTyConsAndClassesFromId :: Id -> (Bag TyCon, Bag Class)

getMentionedTyConsAndClassesFromId id
 = getMentionedTyConsAndClassesFromType (idType id)
-}
\end{code}

\begin{code}
--getIdPrimRep i = primRepFromType (idType i)
\end{code}

\begin{code}
{-LATER:
getInstIdModule (Id _ _ _ (DictFunId _ _ _ mod)) = mod
getInstIdModule (Id _ _ _ (ConstMethodId _ _ _ _ mod)) = mod
getInstIdModule other = panic "Id:getInstIdModule"
-}
\end{code}

%************************************************************************
%*									*
\subsection[Id-overloading]{Functions related to overloading}
%*									*
%************************************************************************

\begin{code}
mkSuperDictSelId  u c sc     ty info = Id u ty (SuperDictSelId c sc) NoPragmaInfo info
mkMethodSelId       u c op     ty info = Id u ty (MethodSelId c op) NoPragmaInfo info
mkDefaultMethodId u c op gen ty info = Id u ty (DefaultMethodId c op gen) NoPragmaInfo info

mkDictFunId u c ity full_ty from_here modname info
  = Id u full_ty (DictFunId c ity from_here modname) NoPragmaInfo info

mkConstMethodId	u c op ity full_ty from_here modname info
  = Id u full_ty (ConstMethodId c ity op from_here modname) NoPragmaInfo info

mkWorkerId u unwrkr ty info = Id u ty (WorkerId unwrkr) NoPragmaInfo info

mkInstId uniq ty name = Id uniq ty (InstId name) NoPragmaInfo noIdInfo

{-LATER:
getConstMethodId clas op ty
  = -- constant-method info is hidden in the IdInfo of
    -- the class-op id (as mentioned up above).
    let
	sel_id = getMethodSelId clas op
    in
    case (lookupConstMethodId (getIdSpecialisation sel_id) ty) of
      Just xx -> xx
      Nothing -> error (ppShow 80 (ppAboves [
	ppCat [ppStr "ERROR: getConstMethodId:", ppr PprDebug op,
	       ppr PprDebug ty, ppr PprDebug ops, ppr PprDebug op_ids,
	       ppr PprDebug sel_id],
	ppStr "(This can arise if an interface pragma refers to an instance",
	ppStr "but there is no imported interface which *defines* that instance.",
	ppStr "The info above, however ugly, should indicate what else you need to import."
	]))
-}
\end{code}

%************************************************************************
%*									*
\subsection[local-funs]{@LocalId@-related functions}
%*									*
%************************************************************************

\begin{code}
mkImported    u n ty info = Id u ty (ImportedId n) NoPragmaInfo info
mkPreludeId   u n ty info = Id u ty (PreludeId  n) NoPragmaInfo info

{-LATER:
updateIdType :: Id -> Type -> Id
updateIdType (Id u _ info details) ty = Id u ty info details
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
  = Id uniq ty (SysLocalId (mkShortName str loc) (no_free_tvs ty)) NoPragmaInfo noIdInfo

mkUserLocal str uniq ty loc
  = Id uniq ty (LocalId (mkShortName str loc) (no_free_tvs ty)) NoPragmaInfo noIdInfo

-- mkUserId builds a local or top-level Id, depending on the name given
mkUserId :: Name -> MyTy a b -> PragmaInfo -> MyId a b
mkUserId (Short uniq short) ty pragma_info
  = Id uniq ty (LocalId short (no_free_tvs ty)) pragma_info noIdInfo
mkUserId (ValName uniq full) ty pragma_info
  = Id uniq ty 
	(if isLocallyDefined full then TopLevId full else ImportedId full)
	pragma_info noIdInfo
\end{code}


\begin{code}
{-LATER:

-- for a SpecPragmaId being created by the compiler out of thin air...
mkSpecPragmaId :: FAST_STRING -> Unique -> Type -> Maybe Id -> SrcLoc -> Id
mkSpecPragmaId str uniq ty specid loc
  = Id uniq ty noIdInfo (SpecPragmaId (mkShortName str loc) specid (no_free_tvs ty))

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
  = Id uniq ty info new_details
-}
\end{code}

Make some local @Ids@ for a template @CoreExpr@.  These have bogus
@Uniques@, but that's OK because the templates are supposed to be
instantiated before use.
\begin{code}
{-LATER:
mkTemplateLocals :: [Type] -> [Id]
mkTemplateLocals tys
  = zipWith (\ u -> \ ty -> mkSysLocal SLIT("tpl") u ty mkUnknownSrcLoc)
	    (getBuiltinUniques (length tys))
	    tys
-}
\end{code}

\begin{code}
getIdInfo     :: GenId ty -> IdInfo
getPragmaInfo :: GenId ty -> PragmaInfo

getIdInfo     (Id _ _ _ _ info) = info
getPragmaInfo (Id _ _ _ info _) = info

{-LATER:
replaceIdInfo :: Id -> IdInfo -> Id

replaceIdInfo (Id u ty _ details) info = Id u ty info details

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
getIdArity (Id _ _ _ _ id_info)  = getInfo id_info

getDataConArity :: DataCon -> Int
getDataConArity id@(Id _ _ _ _ id_info)
  = ASSERT(isDataCon id)
    case (arityMaybe (getInfo id_info)) of
      Nothing -> pprPanic "getDataConArity:Nothing:" (ppr PprDebug id)
      Just  i -> i

addIdArity :: Id -> Int -> Id
addIdArity (Id u ty details pinfo info) arity
  = Id u ty details pinfo (info `addInfo` (mkArityInfo arity))
\end{code}

%************************************************************************
%*									*
\subsection[constructor-funs]{@DataCon@-related functions (incl.~tuples)}
%*									*
%************************************************************************

\begin{code}
mkDataCon :: Unique{-DataConKey-}
	  -> FullName
	  -> [StrictnessMark]
	  -> [TyVar] -> ThetaType -> [TauType] -> TyCon
--ToDo:   -> SpecEnv
	  -> Id
  -- can get the tag and all the pieces of the type from the Type

mkDataCon k n stricts tvs ctxt args_tys tycon
  = ASSERT(length stricts == length args_tys)
    data_con
  where
    -- NB: data_con self-recursion; should be OK as tags are not
    -- looked at until late in the game.
    data_con
      = Id k
	   type_of_constructor
	   (DataConId n data_con_tag stricts tvs ctxt args_tys tycon)
	   NoPragmaInfo
	   datacon_info

    data_con_tag    = position_within fIRST_TAG data_con_family

    data_con_family = getTyConDataCons tycon

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

    arity = length args_tys

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
	BIND (Con data_con tyvar_tys [VarArg v | v <- vars]) _TO_ plain_Con ->

	mkUnfolding EssentialUnfolding -- for data constructors
		    (mkLam tyvars (dict_vars ++ vars) plain_Con)
	BEND

    mk_uf_bits tvs ctxt arg_tys tycon
      = let
	    (inst_env, tyvars, tyvar_tys)
	      = instantiateTyVarTemplates tvs
					  (map getItsUnique tvs)
	in
	    -- the "context" and "arg_tys" have TyVarTemplates in them, so
	    -- we instantiate those types to have the right TyVars in them
	    -- instead.
	BIND (map (instantiateTauTy inst_env) (map ctxt_ty ctxt))
						       	_TO_ inst_dict_tys ->
	BIND (map (instantiateTauTy inst_env) arg_tys) 	_TO_ inst_arg_tys ->

	    -- We can only have **ONE** call to mkTemplateLocals here;
	    -- otherwise, we get two blobs of locals w/ mixed-up Uniques
	    -- (Mega-Sigh) [ToDo]
	BIND (mkTemplateLocals (inst_dict_tys ++ inst_arg_tys)) _TO_ all_vars ->

	BIND (splitAt (length ctxt) all_vars)	_TO_ (dict_vars, vars) ->

	(tyvars, dict_vars, vars)
	BEND BEND BEND BEND
      where
	-- these are really dubious Types, but they are only to make the
	-- binders for the lambdas for tossed-away dicts.
	ctxt_ty (clas, ty) = mkDictTy clas ty
-}
\end{code}

\begin{code}
mkTupleCon :: Arity -> Id

mkTupleCon arity
  = Id unique ty (TupleConId arity) NoPragmaInfo tuplecon_info 
  where
    unique      = mkTupleDataConUnique arity
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
	BIND (Con data_con tyvar_tys [VarArg v | v <- vars]) _TO_ plain_Con ->

	mkUnfolding
	    EssentialUnfolding    -- data constructors
	    (mkLam tyvars (dict_vars ++ vars) plain_Con)
	BEND

    mk_uf_bits arity
      = BIND (mkTemplateLocals tyvar_tys)		 _TO_ vars ->
	(tyvars, [], vars)
	BEND
      where
	tyvar_tmpls	= take arity alphaTyVars
	(_, tyvars, tyvar_tys) = instantiateTyVarTemplates tyvar_tmpls (map getItsUnique tyvar_tmpls)
-}

fIRST_TAG :: ConTag
fIRST_TAG =  1	-- Tags allocated from here for real constructors
\end{code}

\begin{code}
getDataConTag :: DataCon -> ConTag	-- will panic if not a DataCon
getDataConTag	(Id _ _ (DataConId _ tag _ _ _ _ _) _ _) = tag
getDataConTag	(Id _ _ (TupleConId _) _ _)	         = fIRST_TAG
getDataConTag	(Id _ _ (SpecId unspec _ _) _ _)	 = getDataConTag unspec

getDataConTyCon :: DataCon -> TyCon	-- will panic if not a DataCon
getDataConTyCon (Id _ _ (DataConId _ _ _ _ _ _ tycon) _ _) = tycon
getDataConTyCon (Id _ _ (TupleConId a) _ _)	           = mkTupleTyCon a

getDataConSig :: DataCon -> ([TyVar], ThetaType, [TauType], TyCon)
					-- will panic if not a DataCon

getDataConSig (Id _ _ (DataConId _ _ _ tyvars theta_ty arg_tys tycon) _ _)
  = (tyvars, theta_ty, arg_tys, tycon)

getDataConSig (Id _ _ (TupleConId arity) _ _)
  = (tyvars, [], tyvar_tys, mkTupleTyCon arity)
  where
    tyvars	= take arity alphaTyVars
    tyvar_tys	= mkTyVarTys tyvars
\end{code}

{- LATER
getDataConTyCon	(Id _ _ _ (SpecId unspec tys _))
  = mkSpecTyCon (getDataConTyCon unspec) tys

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
-}
\end{code}

\begin{pseudocode}
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

getInstantiatedDataConSig data_con inst_tys
  = ASSERT(isDataCon data_con)
    let
	(tvs, theta, arg_tys, tycon) = getDataConSig data_con

	inst_env = ASSERT(length tvs == length inst_tys)
		   tvs `zip` inst_tys

	theta_tys = [ instantiateTy inst_env (mkDictTy c t) | (c,t) <- theta ]
	cmpnt_tys = [ instantiateTy inst_env arg_ty | arg_ty <- arg_tys ]
	result_ty = instantiateTy inst_env (applyTyCon tycon inst_tys)
    in
    -- Are the first/third results ever used?
    (theta_tys, cmpnt_tys, result_ty)
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

getIdUnfolding (Id _ _ _ _ info) = getInfo_UF info

{-LATER:
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
getIdDemandInfo (Id _ _ _ _ info) = getInfo info

addIdDemandInfo :: Id -> DemandInfo -> Id
addIdDemandInfo (Id u ty details prags info) demand_info
  = Id u ty details prags (info `addInfo` demand_info)
\end{code}

\begin{code}
getIdUpdateInfo :: Id -> UpdateInfo
getIdUpdateInfo (Id _ _ _ _ info) = getInfo info

addIdUpdateInfo :: Id -> UpdateInfo -> Id
addIdUpdateInfo (Id u ty details prags info) upd_info
  = Id u ty details prags (info `addInfo` upd_info)
\end{code}

\begin{code}
{- LATER:
getIdArgUsageInfo :: Id -> ArgUsageInfo
getIdArgUsageInfo (Id u ty info details) = getInfo info

addIdArgUsageInfo :: Id -> ArgUsageInfo -> Id
addIdArgUsageInfo (Id u ty info details) au_info
  = Id u ty (info `addInfo` au_info) details
-}
\end{code}

\begin{code}
{- LATER:
getIdFBTypeInfo :: Id -> FBTypeInfo
getIdFBTypeInfo (Id u ty info details) = getInfo info

addIdFBTypeInfo :: Id -> FBTypeInfo -> Id
addIdFBTypeInfo (Id u ty info details) upd_info
  = Id u ty (info `addInfo` upd_info) details
-}
\end{code}

\begin{code}
{- LATER:
getIdSpecialisation :: Id -> SpecEnv
getIdSpecialisation (Id _ _ _ _ info) = getInfo info

addIdSpecialisation :: Id -> SpecEnv -> Id
addIdSpecialisation (Id u ty details prags info) spec_info
  = Id u ty details prags (info `addInfo` spec_info)
-}
\end{code}

Strictness: we snaffle the info out of the IdInfo.

\begin{code}
getIdStrictness :: Id -> StrictnessInfo

getIdStrictness (Id _ _ _ _ info) = getInfo info

addIdStrictness :: Id -> StrictnessInfo -> Id

addIdStrictness (Id u ty details prags info) strict_info
  = Id u ty details prags (info `addInfo` strict_info)
\end{code}

%************************************************************************
%*									*
\subsection[Id-comparison]{Comparison functions for @Id@s}
%*									*
%************************************************************************

Comparison: equality and ordering---this stuff gets {\em hammered}.

\begin{code}
cmpId (Id u1 _ _ _ _) (Id u2 _ _ _ _) = cmp u1 u2
-- short and very sweet
\end{code}

\begin{code}
instance Ord3 (GenId ty) where
    cmp = cmpId

instance Eq (GenId ty) where
    a == b = case cmpId a b of { EQ_ -> True;  _ -> False }
    a /= b = case cmpId a b of { EQ_ -> False; _ -> True  }

instance Ord (GenId ty) where
    a <= b = case cmpId a b of { LT_ -> True;  EQ_ -> True;  GT__ -> False }
    a <	 b = case cmpId a b of { LT_ -> True;  EQ_ -> False; GT__ -> False }
    a >= b = case cmpId a b of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case cmpId a b of { LT_ -> False; EQ_ -> False; GT__ -> True  }
    _tagCmp a b = case cmpId a b of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
\end{code}

@cmpId_withSpecDataCon@ ensures that any spectys are taken into
account when comparing two data constructors. We need to do this
because a specialised data constructor has the same Unique as its
unspecialised counterpart.

\begin{code}
{-LATER:
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
-}
\end{code}

%************************************************************************
%*									*
\subsection[Id-other-instances]{Other instance declarations for @Id@s}
%*									*
%************************************************************************

\begin{code}
instance Outputable ty => Outputable (GenId ty) where
    ppr sty id = pprId sty id

showId :: PprStyle -> Id -> String
showId sty id = ppShow 80 (pprId sty id)

-- [used below]
-- for DictFuns (instances) and const methods (instance code bits we
-- can call directly): exported (a) if *either* the class or
-- ***OUTERMOST*** tycon [arbitrary...] is exported; or (b) *both*
-- class and tycon are from PreludeCore [non-std, but convenient]
-- *and* the thing was defined in this module.

instance_export_flag :: Class -> Type -> Bool -> ExportFlag

instance_export_flag clas inst_ty from_here
  = panic "Id:instance_export_flag"
{-LATER
  = if instanceIsExported clas inst_ty from_here
    then ExportAll
    else NotExported
-}
\end{code}

Do we consider an ``instance type'' (as on a @DictFunId@) to be ``from
PreludeCore''?  True if the outermost TyCon is fromPreludeCore.
\begin{code}
is_prelude_core_ty :: Type -> Bool

is_prelude_core_ty inst_ty
  = panic "Id.is_prelude_core_ty"
{- LATER
  = case maybeAppDataTyCon inst_ty of
      Just (tycon,_,_) -> fromPreludeCore tycon
      Nothing 	       -> panic "Id: is_prelude_core_ty"
-}
\end{code}

Default printing code (not used for interfaces):
\begin{code}
pprId :: Outputable ty => PprStyle -> GenId ty -> Pretty

pprId other_sty id
  = let
	pieces = getIdNamePieces (case other_sty of {PprForUser -> False; _ -> True}) id

	for_code = panic "pprId: for code"
	{-  = let
		pieces_to_print -- maybe use Unique only
		  = if isSysLocalId id then tail pieces else pieces
	    in
	    ppIntersperse (ppPStr cSEP) (map identToC pieces_to_print)
	-}
    in
    case other_sty of
      PprForC	      -> for_code
      PprForAsm _ _   -> for_code
      PprInterface    -> ppPStr occur_name
      PprForUser      -> ppPStr occur_name
      PprUnfolding    -> qualified_name pieces
      PprDebug	      -> qualified_name pieces
      PprShowAll      -> ppBesides [qualified_name pieces,
			    (ppCat [pp_uniq id,
				    ppPStr SLIT("{-"),
				    ppr other_sty (idType id),
				    ppIdInfo other_sty (unsafeGenId2Id id) True
					     (\x->x) nullIdEnv (getIdInfo id),
				    ppPStr SLIT("-}") ])]
  where
    occur_name = getOccurrenceName id _APPEND_
		 ( _PK_ (if not (isSysLocalId id)
			 then ""
			 else "." ++ (_UNPK_ (showUnique (getItsUnique id)))))

    qualified_name pieces
      = ppBeside (pp_ubxd (ppIntersperse (ppChar '.') (map ppPStr pieces))) (pp_uniq id)

    pp_uniq (Id _ _ (PreludeId _) _ _) 	    	   = ppNil -- no uniq to add
    pp_uniq (Id _ _ (DataConId _ _ _ _ _ _ _) _ _) = ppNil
    pp_uniq (Id _ _ (TupleConId _) _ _) 	   = ppNil
    pp_uniq (Id _ _ (LocalId _ _) _ _)   	   = ppNil -- uniq printed elsewhere
    pp_uniq (Id _ _ (SysLocalId _ _) _ _)   	   = ppNil
    pp_uniq (Id _ _ (SpecPragmaId _ _ _) _ _) 	   = ppNil
    pp_uniq (Id _ _ (InstId _) _ _)   	   	   = ppNil
    pp_uniq other_id = ppBesides [ppPStr SLIT("{-"), pprUnique (getItsUnique other_id), ppPStr SLIT("-}")]

    -- print PprDebug Ids with # afterwards if they are of primitive type.
    pp_ubxd pretty = pretty

{- LATER: applying isPrimType restricts type
    pp_ubxd pretty = if isPrimType (idType id)
		     then ppBeside pretty (ppChar '#')
		     else pretty
-}

\end{code}

\begin{code}
instance NamedThing (GenId ty) where
    getExportFlag (Id _ _ details _ _)
      = get details
      where
	get (DataConId _ _ _ _ _ _ tc)= getExportFlag tc -- NB: don't use the FullName
	get (TupleConId _)	    = NotExported
	get (ImportedId  n)         = getExportFlag n
	get (PreludeId   n)         = getExportFlag n
	get (TopLevId    n)         = getExportFlag n
	get (SuperDictSelId c _)    = getExportFlag c
	get (MethodSelId  c _)	    = getExportFlag c
	get (DefaultMethodId c _ _) = getExportFlag c
	get (DictFunId  c ty from_here _) = instance_export_flag c ty from_here
	get (ConstMethodId c ty _ from_here _) = instance_export_flag c ty from_here
	get (SpecId unspec _ _)     = getExportFlag unspec
	get (WorkerId unwrkr)	    = getExportFlag unwrkr
	get (InstId _)		    = NotExported
	get (LocalId      _ _)	    = NotExported
	get (SysLocalId   _ _)	    = NotExported
	get (SpecPragmaId _ _ _)    = NotExported

    isLocallyDefined this_id@(Id _ _ details _ _)
      = get details
      where
	get (DataConId _ _ _ _ _ _ tc)= isLocallyDefined tc -- NB: don't use the FullName
	get (TupleConId _)	    = False
	get (ImportedId	_)    	    = False
	get (PreludeId  _)    	    = False
	get (TopLevId	n)	    = isLocallyDefined n
	get (SuperDictSelId c _)    = isLocallyDefined c
	get (MethodSelId c _) 	    = isLocallyDefined c
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

    getOrigName this_id@(Id u _ details _ _)
      = get details
      where
	get (DataConId n _ _ _ _ _ _) =	 getOrigName n
	get (TupleConId 0)	= (pRELUDE_BUILTIN, SLIT("()"))
	get (TupleConId a)	= (pRELUDE_BUILTIN, _PK_ ( "(" ++ nOfThem (a-1) ',' ++ ")" ))
	get (ImportedId   n)	= getOrigName n
	get (PreludeId    n)	= getOrigName n
	get (TopLevId     n)	= getOrigName n

	get (MethodSelId c op)	= case (getOrigName c) of -- ToDo; better ???
				    (mod, _) -> (mod, getClassOpString op)

{- LATER:
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
-}

	get (InstId       n)    = (panic "NamedThing.Id.getOrigName (LocalId)",
				   getLocalName n)
	get (LocalId      n _)  = (panic "NamedThing.Id.getOrigName (LocalId)",
				   getLocalName n)
	get (SysLocalId   n _)  = (panic "NamedThing.Id.getOrigName (SysLocal)",
				   getLocalName n)
	get (SpecPragmaId n _ _)= (panic "NamedThing.Id.getOrigName (SpecPragmaId)",
				   getLocalName n)

	get other_details
	    -- the remaining internally-generated flavours of
	    -- Ids really do not have meaningful "original name" stuff,
	    -- but we need to make up something (usually for debugging output)

	  = BIND (getIdNamePieces True this_id)  _TO_ (piece1:pieces) ->
	    BIND [ _CONS_ '.' p | p <- pieces ]  _TO_ dotted_pieces ->
	    (_NIL_, _CONCAT_ (piece1 : dotted_pieces))
	    BEND BEND

    getOccurrenceName this_id@(Id _ _ details _ _)
      = get details
      where
	get (DataConId  n _ _ _ _ _ _) = getOccurrenceName n
	get (TupleConId 0)	= SLIT("()")
	get (TupleConId a)	= _PK_ ( "(" ++ nOfThem (a-1) ',' ++ ")" )
	get (ImportedId	 n)	= getOccurrenceName n
	get (PreludeId   n)	= getOccurrenceName n
	get (TopLevId	 n)	= getOccurrenceName n
	get (MethodSelId _ op)	= getClassOpString op
	get _			= snd (getOrigName this_id)

    getInformingModules id = panic "getInformingModule:Id"

    getSrcLoc (Id _ _ details _ id_info)
      = get details
      where
	get (DataConId  n _ _ _ _ _ _) = getSrcLoc n
	get (TupleConId _)	= mkBuiltinSrcLoc
	get (ImportedId	 n)	= getSrcLoc n
	get (PreludeId   n)	= getSrcLoc n
	get (TopLevId	 n)	= getSrcLoc n
	get (SuperDictSelId c _)= getSrcLoc c
	get (MethodSelId c _)	= getSrcLoc c
	get (SpecId unspec _ _)	= getSrcLoc unspec
	get (WorkerId unwrkr)	= getSrcLoc unwrkr
	get (InstId	  n)	= getSrcLoc n
	get (LocalId      n _)	= getSrcLoc n
	get (SysLocalId   n _)	= getSrcLoc n
	get (SpecPragmaId n _ _)= getSrcLoc n
	-- well, try the IdInfo
	get something_else = getSrcLocIdInfo id_info

    getItsUnique (Id u _ _ _ _) = u

    fromPreludeCore (Id _ _ details _ _)
      = get details
      where
	get (DataConId _ _ _ _ _ _ tc)= fromPreludeCore tc -- NB: not from the FullName
	get (TupleConId _)	    = True
	get (ImportedId  n)	    = fromPreludeCore n
	get (PreludeId   n)	    = fromPreludeCore n
	get (TopLevId    n)	    = fromPreludeCore n
	get (SuperDictSelId c _)    = fromPreludeCore c
	get (MethodSelId c _)	    = fromPreludeCore c
	get (DefaultMethodId c _ _) = fromPreludeCore c
	get (DictFunId	c t _ _)    = fromPreludeCore c && is_prelude_core_ty t
	get (ConstMethodId c t _ _ _) = fromPreludeCore c && is_prelude_core_ty t
	get (SpecId unspec _ _)	    = fromPreludeCore unspec
	get (WorkerId unwrkr)	    = fromPreludeCore unwrkr
	get (InstId       _)	    = False
	get (LocalId      _ _)	    = False
	get (SysLocalId   _ _)	    = False
	get (SpecPragmaId _ _ _)    = False
\end{code}

Reason for @getItsUnique@: The code generator doesn't carry a
@UniqueSupply@, so it wants to use the @Uniques@ out of local @Ids@
given to it.

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
unitIdEnv	 = singletonUFM

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
singletonIdSet	:: GenId ty -> GenIdSet ty
elementOfIdSet	:: GenId ty -> GenIdSet ty -> Bool
minusIdSet	:: GenIdSet ty -> GenIdSet ty -> GenIdSet ty
isEmptyIdSet	:: GenIdSet ty -> Bool
mkIdSet		:: [GenId ty] -> GenIdSet ty

emptyIdSet	= emptyUniqSet
singletonIdSet	= singletonUniqSet
intersectIdSets	= intersectUniqSets
unionIdSets	= unionUniqSets
unionManyIdSets	= unionManyUniqSets
idSetToList	= uniqSetToList
elementOfIdSet	= elementOfUniqSet
minusIdSet	= minusUniqSet
isEmptyIdSet	= isEmptyUniqSet
mkIdSet		= mkUniqSet
\end{code}
