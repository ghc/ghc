%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[DataCon]{@DataCon@: Data Constructors}

\begin{code}
module DataCon (
	DataCon,
	ConTag, fIRST_TAG,
	mkDataCon,
	dataConRepType, dataConSig, dataConName, dataConTag, dataConTyCon,
	dataConArgTys, dataConOrigArgTys, dataConInstOrigArgTys,
	dataConRepArgTys, dataConTheta, 
	dataConFieldLabels, dataConStrictMarks,
	dataConSourceArity, dataConRepArity,
	dataConNumInstArgs, 
	dataConWorkId, dataConWrapId, dataConWrapId_maybe,
	dataConRepStrictness,
	isNullaryDataCon, isTupleCon, isUnboxedTupleCon,
	isExistentialDataCon, classDataCon, dataConExistentialTyVars,

	splitProductType_maybe, splitProductType,
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Subst( substTyWith )
import {-# SOURCE #-} PprType( pprType )

import Type		( Type, ThetaType, 
			  mkForAllTys, mkFunTys, mkTyConApp,
			  mkTyVarTys, splitTyConApp_maybe, repType, 
			  mkPredTys, isStrictType
			)
import TyCon		( TyCon, tyConDataCons, tyConDataCons, isProductTyCon,
			  isTupleTyCon, isUnboxedTupleTyCon, isRecursiveTyCon )
import Class		( Class, classTyCon )
import Name		( Name, NamedThing(..), nameUnique )
import Var		( TyVar, Id )
import FieldLabel	( FieldLabel )
import BasicTypes	( Arity, StrictnessMark(..) )
import Outputable
import Unique		( Unique, Uniquable(..) )
import Maybes		( orElse )
import ListSetOps	( assoc )
import Util		( zipEqual, zipWithEqual, notNull )
\end{code}


Data constructor representation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following Haskell data type declaration

	data T = T !Int ![Int]

Using the strictness annotations, GHC will represent this as

	data T = T Int# [Int]

That is, the Int has been unboxed.  Furthermore, the Haskell source construction

	T e1 e2

is translated to

	case e1 of { I# x -> 
	case e2 of { r ->
	T x r }}

That is, the first argument is unboxed, and the second is evaluated.  Finally,
pattern matching is translated too:

	case e of { T a b -> ... }

becomes

	case e of { T a' b -> let a = I# a' in ... }

To keep ourselves sane, we name the different versions of the data constructor
differently, as follows.


Note [Data Constructor Naming]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each data constructor C has two, and possibly three, Names associated with it:

			     OccName	Name space	Used for
  ---------------------------------------------------------------------------
  * The "source data con" 	C	DataName	The DataCon itself
  * The "real data con"		C	VarName		Its worker Id
  * The "wrapper data con"	$wC	VarName		Wrapper Id (optional)

Each of these three has a distinct Unique.  The "source data con" name
appears in the output of the renamer, and names the Haskell-source
data constructor.  The type checker translates it into either the wrapper Id
(if it exists) or worker Id (otherwise).

The data con has one or two Ids associated with it:

  The "worker Id", is the actual data constructor.
	Its type may be different to the Haskell source constructor
	because:
		- useless dict args are dropped
		- strict args may be flattened
	The worker is very like a primop, in that it has no binding.

	Newtypes currently do get a worker-Id, but it is never used.


  The "wrapper Id", $wC, whose type is exactly what it looks like
	in the source program. It is an ordinary function,
	and it gets a top-level binding like any other function.

	The wrapper Id isn't generated for a data type if the worker
	and wrapper are identical.  It's always generated for a newtype.



A note about the stupid context
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Data types can have a context:
	
	data (Eq a, Ord b) => T a b = T1 a b | T2 a

and that makes the constructors have a context too
(notice that T2's context is "thinned"):

	T1 :: (Eq a, Ord b) => a -> b -> T a b
	T2 :: (Eq a) => a -> T a b

Furthermore, this context pops up when pattern matching
(though GHC hasn't implemented this, but it is in H98, and
I've fixed GHC so that it now does):

	f (T2 x) = x
gets inferred type
	f :: Eq a => T a b -> a

I say the context is "stupid" because the dictionaries passed
are immediately discarded -- they do nothing and have no benefit.
It's a flaw in the language.

Up to now [March 2002] I have put this stupid context into the type of
the "wrapper" constructors functions, T1 and T2, but that turned out
to be jolly inconvenient for generics, and record update, and other
functions that build values of type T (because they don't have
suitable dictionaries available).

So now I've taken the stupid context out.  I simply deal with it
separately in the type checker on occurrences of a constructor, either
in an expression or in a pattern.

[May 2003: actually I think this decision could evasily be reversed now,
and probably should be.  Generics could be disabled for types with 
a stupid context; record updates now (H98) needs the context too; etc.
It's an unforced change, so I'm leaving it for now --- but it does seem
odd that the wrapper doesn't include the stupid context.]



%************************************************************************
%*									*
\subsection{Data constructors}
%*									*
%************************************************************************

\begin{code}
data DataCon
  = MkData {			-- Used for data constructors only;
				-- there *is* no constructor for a newtype

	dcName    :: Name,	-- This is the name of the *source data con*
				-- (see "Note [Data Constructor Naming]" above)

	dcUnique :: Unique, 		-- Cached from Name
	dcTag    :: ConTag,

	-- Running example:
	--
	--	data Eq a => T a = forall b. Ord b => MkT a [b]

	dcRepType   :: Type,	-- Type of the constructor
				-- 	forall a b . Ord b => a -> [b] -> MkT a
				-- (this is *not* of the constructor wrapper Id:
				--  see notes after this data type declaration)
				--
	-- Notice that the existential type parameters come *second*.  
	-- Reason: in a case expression we may find:
	--	case (e :: T t) of { MkT b (d:Ord b) (x:t) (xs:[b]) -> ... }
	-- It's convenient to apply the rep-type of MkT to 't', to get
	--	forall b. Ord b => ...
	-- and use that to check the pattern.  Mind you, this is really only
	-- use in CoreLint.


	-- The next six fields express the type of the constructor, in pieces
	-- e.g.
	--
	--	dcTyVars   = [a]
	-- 	dcTheta    = [Eq a]
	--	dcExTyVars = [b]
	--	dcExTheta  = [Ord b]
	--	dcOrigArgTys   = [a,List b]
	--	dcTyCon    = T

	dcTyVars :: [TyVar], 		-- Type vars for the data type decl
					-- These are ALWAYS THE SAME AS THE TYVARS
					-- FOR THE PARENT TyCon.  We occasionally rely on
					-- this just to avoid redundant instantiation

	dcStupidTheta  ::  ThetaType,	-- This is a "thinned" version of the context of 
					-- the data decl.  
		-- "Thinned", because the Report says
		-- to eliminate any constraints that don't mention
		-- tyvars free in the arg types for this constructor
		--
		-- "Stupid", because the dictionaries aren't used for anything.  
		-- 
		-- Indeed, [as of March 02] they are no 
		-- longer in the type of the dcWrapId, because
		-- that makes it harder to use the wrap-id to rebuild
		-- values after record selection or in generics.

	dcExTyVars :: [TyVar], 		-- Ditto for the context of the constructor,
	dcExTheta  :: ThetaType,	-- the existentially quantified stuff
					
	dcOrigArgTys :: [Type],		-- Original argument types
					-- (before unboxing and flattening of
					--  strict fields)

	dcRepArgTys :: [Type],		-- Final, representation argument types, after unboxing and flattening,
					-- and including existential dictionaries

	dcRepStrictness :: [StrictnessMark],	-- One for each representation argument	

	dcTyCon  :: TyCon,		-- Result tycon

	-- Now the strictness annotations and field labels of the constructor
	dcStrictMarks :: [StrictnessMark],
		-- Strictness annotations as deduced by the compiler.  
		-- Has no MarkedUserStrict; they have been changed to MarkedStrict
		-- or MarkedUnboxed by the compiler.
		-- *Includes the existential dictionaries*
		-- length = length dcExTheta + dataConSourceArity dataCon

	dcFields  :: [FieldLabel],
		-- Field labels for this constructor, in the
		-- same order as the argument types; 
		-- length = 0 (if not a record) or dataConSourceArity.

	-- Finally, the curried worker function that corresponds to the constructor
	-- It doesn't have an unfolding; the code generator saturates these Ids
	-- and allocates a real constructor when it finds one.
	--
	-- An entirely separate wrapper function is built in TcTyDecls

	dcWorkId :: Id,		-- The corresponding worker Id
				-- Takes dcRepArgTys as its arguments
				-- Perhaps this should be a 'Maybe'; not reqd for newtype constructors

	dcWrapId :: Maybe Id	-- The wrapper Id, if it's necessary
				-- It's deemed unnecessary if it performs the 
				-- identity function
  }

type ConTag = Int

fIRST_TAG :: ConTag
fIRST_TAG =  1	-- Tags allocated from here for real constructors
\end{code}

The dcRepType field contains the type of the representation of a contructor
This may differ from the type of the contructor *Id* (built
by MkId.mkDataConId) for two reasons:
	a) the constructor Id may be overloaded, but the dictionary isn't stored
	   e.g.    data Eq a => T a = MkT a a

	b) the constructor may store an unboxed version of a strict field.

Here's an example illustrating both:
	data Ord a => T a = MkT Int! a
Here
	T :: Ord a => Int -> a -> T a
but the rep type is
	Trep :: Int# -> a -> T a
Actually, the unboxed part isn't implemented yet!


%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************

\begin{code}
instance Eq DataCon where
    a == b = getUnique a == getUnique b
    a /= b = getUnique a /= getUnique b

instance Ord DataCon where
    a <= b = getUnique a <= getUnique b
    a <	 b = getUnique a <  getUnique b
    a >= b = getUnique a >= getUnique b
    a >	 b = getUnique a > getUnique b
    compare a b = getUnique a `compare` getUnique b

instance Uniquable DataCon where
    getUnique = dcUnique

instance NamedThing DataCon where
    getName = dcName

instance Outputable DataCon where
    ppr con = ppr (dataConName con)

instance Show DataCon where
    showsPrec p con = showsPrecSDoc p (ppr con)
\end{code}


%************************************************************************
%*									*
\subsection{Construction}
%*									*
%************************************************************************

\begin{code}
mkDataCon :: Name 
	  -> [StrictnessMark] -> [FieldLabel]
	  -> [TyVar] -> ThetaType
	  -> [TyVar] -> ThetaType
	  -> [Type] -> TyCon
	  -> Id -> Maybe Id	-- Worker and possible wrapper
	  -> DataCon
  -- Can get the tag from the TyCon

mkDataCon name 
	  arg_stricts 	-- Use [] to mean 'all non-strict'
	  fields
	  tyvars theta ex_tyvars ex_theta orig_arg_tys tycon
	  work_id wrap_id
  = con
  where
    con = MkData {dcName = name, 
		  dcUnique = nameUnique name,
	  	  dcTyVars = tyvars, dcStupidTheta = theta,
		  dcOrigArgTys = orig_arg_tys,
		  dcRepArgTys = rep_arg_tys,
	     	  dcExTyVars = ex_tyvars, dcExTheta = ex_theta,
		  dcStrictMarks = real_stricts, dcRepStrictness = rep_arg_stricts,
		  dcFields = fields, dcTag = tag, dcTyCon = tycon, dcRepType = ty,
		  dcWorkId = work_id, dcWrapId = wrap_id}

	-- Strictness marks for source-args
	--	*after unboxing choices*, 
	-- but  *including existential dictionaries*
	-- 
	-- The 'arg_stricts' passed to mkDataCon are simply those for the
	-- source-language arguments.  We add extra ones for the
	-- dictionary arguments right here.
    ex_dict_tys  = mkPredTys ex_theta
    real_stricts = map mk_dict_strict_mark ex_dict_tys ++
		   zipWith (chooseBoxingStrategy tycon) 
			   orig_arg_tys 
			   (arg_stricts ++ repeat NotMarkedStrict)
    real_arg_tys = ex_dict_tys ++ orig_arg_tys

	-- Representation arguments and demands
    (rep_arg_stricts, rep_arg_tys) = computeRep real_stricts real_arg_tys

    tag = assoc "mkDataCon" (tyConDataCons tycon `zip` [fIRST_TAG..]) con
    ty  = mkForAllTys (tyvars ++ ex_tyvars)
	              (mkFunTys rep_arg_tys result_ty)
		-- NB: the existential dict args are already in rep_arg_tys

    result_ty = mkTyConApp tycon (mkTyVarTys tyvars)

mk_dict_strict_mark ty | isStrictType ty = MarkedStrict
		       | otherwise	 = NotMarkedStrict
\end{code}

\begin{code}
dataConName :: DataCon -> Name
dataConName = dcName

dataConTag :: DataCon -> ConTag
dataConTag  = dcTag

dataConTyCon :: DataCon -> TyCon
dataConTyCon = dcTyCon

dataConRepType :: DataCon -> Type
dataConRepType = dcRepType

dataConWorkId :: DataCon -> Id
dataConWorkId = dcWorkId

dataConWrapId_maybe :: DataCon -> Maybe Id
dataConWrapId_maybe = dcWrapId

dataConWrapId :: DataCon -> Id
-- Returns an Id which looks like the Haskell-source constructor
-- If there is no dcWrapId it's because there is no need for a 
-- wrapper, so the worker is the Right Thing
dataConWrapId dc = dcWrapId dc `orElse` dcWorkId dc

dataConFieldLabels :: DataCon -> [FieldLabel]
dataConFieldLabels = dcFields

dataConStrictMarks :: DataCon -> [StrictnessMark]
dataConStrictMarks = dcStrictMarks

-- Number of type-instantiation arguments
-- All the remaining arguments of the DataCon are (notionally)
-- stored in the DataCon, and are matched in a case expression
dataConNumInstArgs (MkData {dcTyVars = tyvars}) = length tyvars

dataConSourceArity :: DataCon -> Arity
	-- Source-level arity of the data constructor
dataConSourceArity dc = length (dcOrigArgTys dc)

-- dataConRepArity gives the number of actual fields in the
-- {\em representation} of the data constructor.  This may be more than appear
-- in the source code; the extra ones are the existentially quantified
-- dictionaries
dataConRepArity (MkData {dcRepArgTys = arg_tys}) = length arg_tys

isNullaryDataCon con  = dataConRepArity con == 0

dataConRepStrictness :: DataCon -> [StrictnessMark]
	-- Give the demands on the arguments of a
	-- Core constructor application (Con dc args)
dataConRepStrictness dc = dcRepStrictness dc

dataConSig :: DataCon -> ([TyVar], ThetaType,
			  [TyVar], ThetaType,
			  [Type], TyCon)

dataConSig (MkData {dcTyVars = tyvars, dcStupidTheta = theta,
		     dcExTyVars = ex_tyvars, dcExTheta = ex_theta,
		     dcOrigArgTys = arg_tys, dcTyCon = tycon})
  = (tyvars, theta, ex_tyvars, ex_theta, arg_tys, tycon)

dataConArgTys :: DataCon
	      -> [Type] 	-- Instantiated at these types
				-- NB: these INCLUDE the existentially quantified arg types
	      -> [Type]		-- Needs arguments of these types
				-- NB: these INCLUDE the existentially quantified dict args
				--     but EXCLUDE the data-decl context which is discarded
				-- It's all post-flattening etc; this is a representation type

dataConArgTys (MkData {dcRepArgTys = arg_tys, dcTyVars = tyvars,
		       dcExTyVars = ex_tyvars}) inst_tys
 = map (substTyWith (tyvars ++ ex_tyvars) inst_tys) arg_tys

dataConTheta :: DataCon -> ThetaType
dataConTheta dc = dcStupidTheta dc

dataConExistentialTyVars :: DataCon -> [TyVar]
dataConExistentialTyVars dc = dcExTyVars dc

-- And the same deal for the original arg tys:

dataConInstOrigArgTys :: DataCon -> [Type] -> [Type]
dataConInstOrigArgTys (MkData {dcOrigArgTys = arg_tys, dcTyVars = tyvars,
		       dcExTyVars = ex_tyvars}) inst_tys
 = map (substTyWith (tyvars ++ ex_tyvars) inst_tys) arg_tys
\end{code}

These two functions get the real argument types of the constructor,
without substituting for any type variables.

dataConOrigArgTys returns the arg types of the wrapper, excluding all dictionary args.

dataConRepArgTys retuns the arg types of the worker, including all dictionaries, and
after any flattening has been done.

\begin{code}
dataConOrigArgTys :: DataCon -> [Type]
dataConOrigArgTys dc = dcOrigArgTys dc

dataConRepArgTys :: DataCon -> [Type]
dataConRepArgTys dc = dcRepArgTys dc
\end{code}


\begin{code}
isTupleCon :: DataCon -> Bool
isTupleCon (MkData {dcTyCon = tc}) = isTupleTyCon tc
	
isUnboxedTupleCon :: DataCon -> Bool
isUnboxedTupleCon (MkData {dcTyCon = tc}) = isUnboxedTupleTyCon tc

isExistentialDataCon :: DataCon -> Bool
isExistentialDataCon (MkData {dcExTyVars = tvs}) = notNull tvs
\end{code}


\begin{code}
classDataCon :: Class -> DataCon
classDataCon clas = case tyConDataCons (classTyCon clas) of
		      (dict_constr:no_more) -> ASSERT( null no_more ) dict_constr 
\end{code}

%************************************************************************
%*									*
\subsection{Splitting products}
%*									*
%************************************************************************

\begin{code}
splitProductType_maybe
	:: Type 			-- A product type, perhaps
	-> Maybe (TyCon, 		-- The type constructor
		  [Type],		-- Type args of the tycon
		  DataCon,		-- The data constructor
		  [Type])		-- Its *representation* arg types

	-- Returns (Just ...) for any
	--	concrete (i.e. constructors visible)
	--	single-constructor
	--	not existentially quantified
	-- type whether a data type or a new type
	--
	-- Rejecing existentials is conservative.  Maybe some things
	-- could be made to work with them, but I'm not going to sweat
	-- it through till someone finds it's important.

splitProductType_maybe ty
  = case splitTyConApp_maybe ty of
	Just (tycon,ty_args)
	   | isProductTyCon tycon  	-- Includes check for non-existential,
					-- and for constructors visible
	   -> Just (tycon, ty_args, data_con, dataConArgTys data_con ty_args)
	   where
	      data_con = head (tyConDataCons tycon)
	other -> Nothing

splitProductType str ty
  = case splitProductType_maybe ty of
	Just stuff -> stuff
	Nothing    -> pprPanic (str ++ ": not a product") (pprType ty)

-- We attempt to unbox/unpack a strict field when either:
--   (i)  The tycon is imported, and the field is marked '! !', or
--   (ii) The tycon is defined in this module, the field is marked '!',
--	  and the -funbox-strict-fields flag is on.
--
-- This ensures that if we compile some modules with -funbox-strict-fields and
-- some without, the compiler doesn't get confused about the constructor
-- representations.

chooseBoxingStrategy :: TyCon -> Type -> StrictnessMark -> StrictnessMark
	-- Transforms any MarkedUserStricts into MarkUnboxed or MarkedStrict
chooseBoxingStrategy tycon arg_ty strict
  = case strict of
	MarkedUserStrict -> MarkedStrict
	MarkedUserUnboxed
	  | can_unbox -> MarkedUnboxed
	  | otherwise -> MarkedStrict
	other -> strict
  where
    can_unbox = unbox arg_ty
	-- beware: repType will go into a loop if we try this on a recursive
	-- type (for reasons unknown...), hence the check for recursion below.
    unbox ty =  
	case splitTyConApp_maybe ty of
		Nothing	-> False
		Just (arg_tycon, _)
		  | isRecursiveTyCon arg_tycon -> False
		  | otherwise ->
			  case splitTyConApp_maybe (repType ty) of
		     		Nothing -> False
		     		Just (arg_tycon, _) -> isProductTyCon arg_tycon

computeRep :: [StrictnessMark]		-- Original arg strictness
					--   [after strategy choice; can't be MarkedUserStrict]
	   -> [Type]			-- and types
	   -> ([StrictnessMark],	-- Representation arg strictness
	       [Type])			-- And type

computeRep stricts tys
  = unzip $ concat $ zipWithEqual "computeRep" unbox stricts tys
  where
    unbox NotMarkedStrict ty = [(NotMarkedStrict, ty)]
    unbox MarkedStrict    ty = [(MarkedStrict,    ty)]
    unbox MarkedUnboxed   ty = zipEqual "computeRep" (dataConRepStrictness arg_dc) arg_tys
			     where
			       (_, _, arg_dc, arg_tys) = splitProductType "unbox_strict_arg_ty" (repType ty)
\end{code}
