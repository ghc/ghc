%
% (c) The GRASP Project, Glasgow University, 1994-1998
%
\section[TysWiredIn]{Wired-in knowledge about {\em non-primitive} types}

This module is about types that can be defined in Haskell, but which
must be wired into the compiler nonetheless.

This module tracks the ``state interface'' document, ``GHC prelude:
types and operations.''

\begin{code}
module TysWiredIn (
	wiredInTyCons, genericTyCons,

	addrDataCon,
	addrTy,
	addrTyCon,
	ptrDataCon,
	ptrTy,
	ptrTyCon,
	funPtrDataCon,
	funPtrTy,
	funPtrTyCon,
	boolTy,
	boolTyCon,
	charDataCon,
	charTy,
	charTyCon,
	consDataCon,
	doubleDataCon,
	doubleTy,
	isDoubleTy,
	doubleTyCon,
	falseDataCon, falseDataConId,
	floatDataCon,
	floatTy,
	isFloatTy,
	floatTyCon,

	intDataCon,
	intTy,
	intTyCon,
	isIntTy,

	integerTy,
	integerTyCon,
	smallIntegerDataCon,
	largeIntegerDataCon,
	isIntegerTy,

	listTyCon,

	mkListTy,
	nilDataCon,

	-- tuples
	mkTupleTy,
	tupleTyCon, tupleCon, 
	unitTyCon, unitDataConId, pairTyCon, 
	unboxedSingletonTyCon, unboxedSingletonDataCon,
	unboxedPairTyCon, unboxedPairDataCon,

	-- Generics
        genUnitTyCon, genUnitDataCon, 
	plusTyCon, inrDataCon, inlDataCon,
	crossTyCon, crossDataCon,

	stablePtrTyCon,
	stringTy,
	trueDataCon, trueDataConId,
	unitTy,
	voidTy,
	wordDataCon,
	wordTy,
	wordTyCon,

	isFFIArgumentTy,     -- :: DynFlags -> Safety -> Type -> Bool
	isFFIImportResultTy, -- :: DynFlags -> Type -> Bool
	isFFIExportResultTy, -- :: Type -> Bool
	isFFIExternalTy,     -- :: Type -> Bool
        isFFIDynArgumentTy,  -- :: Type -> Bool
 	isFFIDynResultTy,    -- :: Type -> Bool
 	isFFILabelTy,        -- :: Type -> Bool
	isAddrTy,	     -- :: Type -> Bool
	isForeignPtrTy       -- :: Type -> Bool

    ) where

#include "HsVersions.h"

import {-# SOURCE #-} MkId( mkDataConId, mkDataConWrapId )
import {-# SOURCE #-} Generics( mkTyConGenInfo )

-- friends:
import PrelNames
import TysPrim

-- others:
import ForeignCall	( Safety, playSafe )
import Constants	( mAX_TUPLE_SIZE )
import Module		( mkPrelModule )
import Name		( Name, nameRdrName, nameUnique, nameOccName, 
			  nameModule, mkWiredInName )
import OccName		( mkOccFS, tcName, dataName, mkWorkerOcc, mkGenOcc1, mkGenOcc2 )
import RdrName		( rdrNameOcc )
import DataCon		( DataCon, mkDataCon, dataConId )
import Demand		( StrictnessMark(..) )
import Var		( TyVar, tyVarKind )
import TyCon		( TyCon, AlgTyConFlavour(..), tyConDataCons,
			  mkTupleTyCon, isUnLiftedTyCon, mkAlgTyCon
			)

import BasicTypes	( Arity, RecFlag(..), Boxity(..), isBoxed )

import Type		( Type, mkTyConTy, mkTyConApp, mkTyVarTys, 
			  mkArrowKinds, liftedTypeKind, unliftedTypeKind,
			  splitTyConApp_maybe, repType,
			  TauType, ThetaType )
import Unique		( incrUnique, mkTupleTyConUnique, mkTupleDataConUnique )
import PrelNames
import CmdLineOpts
import Array

alpha_tyvar	  = [alphaTyVar]
alpha_ty	  = [alphaTy]
alpha_beta_tyvars = [alphaTyVar, betaTyVar]
\end{code}


%************************************************************************
%*									*
\subsection{Wired in type constructors}
%*									*
%************************************************************************

\begin{code}
wiredInTyCons :: [TyCon]
wiredInTyCons = data_tycons ++ tuple_tycons ++ unboxed_tuple_tycons

data_tycons = genericTyCons ++
	      [ addrTyCon
	      , ptrTyCon
	      , funPtrTyCon
    	      , boolTyCon
    	      , charTyCon
    	      , doubleTyCon
    	      , floatTyCon
    	      , intTyCon
    	      , integerTyCon
    	      , listTyCon
    	      , wordTyCon
    	      ]

genericTyCons :: [TyCon]
genericTyCons = [ plusTyCon, crossTyCon, genUnitTyCon ]


tuple_tycons = unitTyCon : [tupleTyCon Boxed   i | i <- [2..mAX_TUPLE_SIZE] ]
unboxed_tuple_tycons     = [tupleTyCon Unboxed i | i <- [1..mAX_TUPLE_SIZE] ]
\end{code}


%************************************************************************
%*                                                                      *
\subsection{mkWiredInTyCon}
%*                                                                      *
%************************************************************************

\begin{code}
pcNonRecDataTyCon = pcTyCon DataTyCon NonRecursive
pcRecDataTyCon = pcTyCon DataTyCon Recursive

pcTyCon new_or_data is_rec name tyvars argvrcs cons
  = tycon
  where
    tycon = mkAlgTyCon name kind
                tyvars
                []              -- No context
                argvrcs
                cons
                (length cons) 
		[]		-- No record selectors
                new_or_data
                is_rec
		gen_info

    mod      = nameModule name
    kind     = mkArrowKinds (map tyVarKind tyvars) liftedTypeKind
    gen_info = mk_tc_gen_info mod (nameUnique name) name tycon

-- We generate names for the generic to/from Ids by incrementing
-- the TyCon unique.  So each Prelude tycon needs 3 slots, one
-- for itself and two more for the generic Ids.
mk_tc_gen_info mod tc_uniq tc_name tycon
  = mkTyConGenInfo tycon [name1, name2]
  where
	tc_occ_name = nameOccName tc_name
	occ_name1   = mkGenOcc1 tc_occ_name
	occ_name2   = mkGenOcc2 tc_occ_name
	fn1_key     = incrUnique tc_uniq
	fn2_key     = incrUnique fn1_key
	name1	    = mkWiredInName  mod occ_name1 fn1_key
	name2	    = mkWiredInName  mod occ_name2 fn2_key

pcDataCon :: Name -> [TyVar] -> ThetaType -> [TauType] -> TyCon -> DataCon
-- The unique is the first of two free uniques;
-- the first is used for the datacon itself and the worker;
-- the second is used for the wrapper.

pcDataCon name tyvars context arg_tys tycon
  = data_con
  where
    data_con = mkDataCon name
                [ NotMarkedStrict | a <- arg_tys ]
                [ {- no labelled fields -} ]
                tyvars context [] [] arg_tys tycon work_id wrap_id

    wrap_rdr  = nameRdrName name
    wrap_occ  = rdrNameOcc wrap_rdr

    mod       = nameModule name
    wrap_id   = mkDataConWrapId data_con

    work_occ  = mkWorkerOcc wrap_occ
    work_key  = incrUnique (nameUnique name)
    work_name = mkWiredInName mod work_occ work_key
    work_id   = mkDataConId work_name data_con
\end{code}


%************************************************************************
%*									*
\subsection[TysWiredIn-tuples]{The tuple types}
%*									*
%************************************************************************

\begin{code}
tupleTyCon :: Boxity -> Arity -> TyCon
tupleTyCon boxity i | i > mAX_TUPLE_SIZE = fst (mk_tuple boxity i)	-- Build one specially
tupleTyCon Boxed   i = fst (boxedTupleArr   ! i)
tupleTyCon Unboxed i = fst (unboxedTupleArr ! i)

tupleCon :: Boxity -> Arity -> DataCon
tupleCon boxity i | i > mAX_TUPLE_SIZE = snd (mk_tuple boxity i)	-- Build one specially
tupleCon Boxed   i = snd (boxedTupleArr   ! i)
tupleCon Unboxed i = snd (unboxedTupleArr ! i)

boxedTupleArr, unboxedTupleArr :: Array Int (TyCon,DataCon)
boxedTupleArr   = array (0,mAX_TUPLE_SIZE) [(i,mk_tuple Boxed i)   | i <- [0..mAX_TUPLE_SIZE]]
unboxedTupleArr = array (0,mAX_TUPLE_SIZE) [(i,mk_tuple Unboxed i) | i <- [0..mAX_TUPLE_SIZE]]

mk_tuple :: Boxity -> Int -> (TyCon,DataCon)
mk_tuple boxity arity = (tycon, tuple_con)
  where
	tycon   = mkTupleTyCon tc_name tc_kind arity tyvars tuple_con boxity gen_info 
	tc_name = mkWiredInName mod (mkOccFS tcName name_str) tc_uniq
    	tc_kind = mkArrowKinds (map tyVarKind tyvars) res_kind
	res_kind | isBoxed boxity = liftedTypeKind
		 | otherwise	  = unliftedTypeKind

	tyvars   | isBoxed boxity = take arity alphaTyVars
		 | otherwise	  = take arity openAlphaTyVars

	tuple_con = pcDataCon name tyvars [] tyvar_tys tycon
	tyvar_tys = mkTyVarTys tyvars
	(mod_name, name_str) = mkTupNameStr boxity arity
	name      = mkWiredInName mod (mkOccFS dataName name_str) dc_uniq
 	tc_uniq   = mkTupleTyConUnique   boxity arity
	dc_uniq   = mkTupleDataConUnique boxity arity
	mod	  = mkPrelModule mod_name
	gen_info  = mk_tc_gen_info mod tc_uniq tc_name tycon

unitTyCon     = tupleTyCon Boxed 0
unitDataConId = dataConId (head (tyConDataCons unitTyCon))

pairTyCon = tupleTyCon Boxed 2

unboxedSingletonTyCon   = tupleTyCon Unboxed 1
unboxedSingletonDataCon = tupleCon   Unboxed 1

unboxedPairTyCon   = tupleTyCon Unboxed 2
unboxedPairDataCon = tupleCon   Unboxed 2
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-boxed-prim]{The ``boxed primitive'' types (@Char@, @Int@, etc)}
%*									*
%************************************************************************

\begin{code}
-- The Void type is represented as a data type with no constructors
-- It's a built in type (i.e. there's no way to define it in Haskell;
--	the nearest would be
--
--		data Void =		-- No constructors!
--
-- ) It's lifted; there is only one value of this
-- type, namely "void", whose semantics is just bottom.
--
-- Haskell 98 drops the definition of a Void type, so we just 'simulate'
-- voidTy using ().
voidTy = unitTy
\end{code}


\begin{code}
charTy = mkTyConTy charTyCon

charTyCon   = pcNonRecDataTyCon charTyConName [] [] [charDataCon]
charDataCon = pcDataCon charDataConName [] [] [charPrimTy] charTyCon

stringTy = mkListTy charTy -- convenience only
\end{code}

\begin{code}
intTy = mkTyConTy intTyCon 

intTyCon = pcNonRecDataTyCon intTyConName [] [] [intDataCon]
intDataCon = pcDataCon intDataConName [] [] [intPrimTy] intTyCon

isIntTy :: Type -> Bool
isIntTy = isTyCon intTyConKey
\end{code}

\begin{code}

wordTy = mkTyConTy wordTyCon

wordTyCon = pcNonRecDataTyCon wordTyConName [] [] [wordDataCon]
wordDataCon = pcDataCon wordDataConName [] [] [wordPrimTy] wordTyCon
\end{code}

\begin{code}
addrTy = mkTyConTy addrTyCon

addrTyCon = pcNonRecDataTyCon addrTyConName [] [] [addrDataCon]
addrDataCon = pcDataCon addrDataConName [] [] [addrPrimTy] addrTyCon

isAddrTy :: Type -> Bool
isAddrTy = isTyCon addrTyConKey
\end{code}

\begin{code}
ptrTy = mkTyConTy ptrTyCon

ptrTyCon = pcNonRecDataTyCon ptrTyConName alpha_tyvar [(True,False)] [ptrDataCon]
ptrDataCon = pcDataCon ptrDataConName alpha_tyvar [] [addrPrimTy] ptrTyCon
\end{code}

\begin{code}
funPtrTy = mkTyConTy funPtrTyCon

funPtrTyCon = pcNonRecDataTyCon funPtrTyConName alpha_tyvar [(True,False)] [funPtrDataCon]
funPtrDataCon = pcDataCon funPtrDataConName alpha_tyvar [] [addrPrimTy] funPtrTyCon
\end{code}

\begin{code}
floatTy	= mkTyConTy floatTyCon

floatTyCon   = pcNonRecDataTyCon floatTyConName   [] [] [floatDataCon]
floatDataCon = pcDataCon         floatDataConName [] [] [floatPrimTy] floatTyCon

isFloatTy :: Type -> Bool
isFloatTy = isTyCon floatTyConKey
\end{code}

\begin{code}
doubleTy = mkTyConTy doubleTyCon

isDoubleTy :: Type -> Bool
isDoubleTy = isTyCon doubleTyConKey

doubleTyCon   = pcNonRecDataTyCon doubleTyConName     [] [] [doubleDataCon]
doubleDataCon = pcDataCon	  doubleDataConName [] [] [doublePrimTy] doubleTyCon
\end{code}

\begin{code}
stablePtrTyCon
  = pcNonRecDataTyCon stablePtrTyConName
	alpha_tyvar [(True,False)] [stablePtrDataCon]
  where
    stablePtrDataCon
      = pcDataCon stablePtrDataConName
	    alpha_tyvar [] [mkStablePtrPrimTy alphaTy] stablePtrTyCon
\end{code}

\begin{code}
foreignObjTyCon
  = pcNonRecDataTyCon foreignObjTyConName
	[] [] [foreignObjDataCon]
  where
    foreignObjDataCon
      = pcDataCon foreignObjDataConName
	    [] [] [foreignObjPrimTy] foreignObjTyCon
\end{code}

\begin{code}
foreignPtrTyCon
  = pcNonRecDataTyCon foreignPtrTyConName
	alpha_tyvar  [(True,False)] [foreignPtrDataCon]
  where
    foreignPtrDataCon
      = pcDataCon foreignPtrDataConName
	    alpha_tyvar [] [foreignObjPrimTy] foreignPtrTyCon

isForeignPtrTy :: Type -> Bool
isForeignPtrTy = isTyCon foreignPtrTyConKey
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-Integer]{@Integer@ and its related ``pairing'' types}
%*									*
%************************************************************************

@Integer@ and its pals are not really primitive.  @Integer@ itself, first:
\begin{code}
integerTy :: Type
integerTy = mkTyConTy integerTyCon

integerTyCon = pcNonRecDataTyCon integerTyConName
                   [] [] [smallIntegerDataCon, largeIntegerDataCon]

smallIntegerDataCon = pcDataCon smallIntegerDataConName
		[] [] [intPrimTy] integerTyCon
largeIntegerDataCon = pcDataCon largeIntegerDataConName
		[] [] [intPrimTy, byteArrayPrimTy] integerTyCon


isIntegerTy :: Type -> Bool
isIntegerTy = isTyCon integerTyConKey
\end{code}


%************************************************************************
%*									*
\subsection[TysWiredIn-ext-type]{External types}
%*									*
%************************************************************************

The compiler's foreign function interface supports the passing of a
restricted set of types as arguments and results (the restricting factor
being the )

\begin{code}
isFFIArgumentTy :: DynFlags -> Safety -> Type -> Bool
-- Checks for valid argument type for a 'foreign import'
isFFIArgumentTy dflags safety ty 
   = checkRepTyCon (legalOutgoingTyCon dflags safety) ty

isFFIExternalTy :: Type -> Bool
-- Types that are allowed as arguments of a 'foreign export'
isFFIExternalTy ty = checkRepTyCon legalFEArgTyCon ty

isFFIImportResultTy :: DynFlags -> Type -> Bool
isFFIImportResultTy dflags ty 
  = checkRepTyCon (legalFIResultTyCon dflags) ty

isFFIExportResultTy :: Type -> Bool
isFFIExportResultTy ty = checkRepTyCon legalFEResultTyCon ty

isFFIDynArgumentTy :: Type -> Bool
-- The argument type of a foreign import dynamic must be Ptr, FunPtr, Addr,
-- or a newtype of either.
isFFIDynArgumentTy = checkRepTyCon (\tc -> tc == ptrTyCon || tc == funPtrTyCon || tc == addrTyCon)

isFFIDynResultTy :: Type -> Bool
-- The result type of a foreign export dynamic must be Ptr, FunPtr, Addr,
-- or a newtype of either.
isFFIDynResultTy = checkRepTyCon (\tc -> tc == ptrTyCon || tc == funPtrTyCon || tc == addrTyCon)

isFFILabelTy :: Type -> Bool
-- The type of a foreign label must be Ptr, FunPtr, Addr,
-- or a newtype of either.
isFFILabelTy = checkRepTyCon (\tc -> tc == ptrTyCon || tc == funPtrTyCon || tc == addrTyCon)

checkRepTyCon :: (TyCon -> Bool) -> Type -> Bool
	-- look through newtypes
checkRepTyCon check_tc ty = checkTyCon check_tc (repType ty)

checkTyCon :: (TyCon -> Bool) -> Type -> Bool
checkTyCon check_tc ty = case splitTyConApp_maybe ty of
				Just (tycon, _) -> check_tc tycon
				Nothing		-> False

isTyCon :: Unique -> Type -> Bool
isTyCon uniq ty = checkTyCon (\tc -> uniq == getUnique tc) ty
\end{code}

----------------------------------------------
These chaps do the work; they are not exported
----------------------------------------------

\begin{code}
legalFEArgTyCon :: TyCon -> Bool
-- It's illegal to return foreign objects and (mutable)
-- bytearrays from a _ccall_ / foreign declaration
-- (or be passed them as arguments in foreign exported functions).
legalFEArgTyCon tc
  | getUnique tc `elem` [ foreignObjTyConKey, foreignPtrTyConKey,
			  byteArrayTyConKey, mutableByteArrayTyConKey ] 
  = False
  -- It's also illegal to make foreign exports that take unboxed
  -- arguments.  The RTS API currently can't invoke such things.  --SDM 7/2000
  | otherwise
  = boxedMarshalableTyCon tc

legalFIResultTyCon :: DynFlags -> TyCon -> Bool
legalFIResultTyCon dflags tc
  | getUnique tc `elem`
	[ foreignObjTyConKey, foreignPtrTyConKey,
	  byteArrayTyConKey, mutableByteArrayTyConKey ]  = False
  | tc == unitTyCon = True
  | otherwise	    = marshalableTyCon dflags tc

legalFEResultTyCon :: TyCon -> Bool
legalFEResultTyCon tc
  | getUnique tc `elem` 
	[ foreignObjTyConKey, foreignPtrTyConKey,
	  byteArrayTyConKey, mutableByteArrayTyConKey ]  = False
  | tc == unitTyCon = True
  | otherwise       = boxedMarshalableTyCon tc

legalOutgoingTyCon :: DynFlags -> Safety -> TyCon -> Bool
-- Checks validity of types going from Haskell -> external world
legalOutgoingTyCon dflags safety tc
  | playSafe safety && getUnique tc `elem` [byteArrayTyConKey, mutableByteArrayTyConKey]
  = False
  | otherwise
  = marshalableTyCon dflags tc

marshalableTyCon dflags tc
  =  (dopt Opt_GlasgowExts dflags && isUnLiftedTyCon tc)
  || boxedMarshalableTyCon tc

boxedMarshalableTyCon tc
   = getUnique tc `elem` [ intTyConKey, int8TyConKey, int16TyConKey
			 , int32TyConKey, int64TyConKey
			 , wordTyConKey, word8TyConKey, word16TyConKey
			 , word32TyConKey, word64TyConKey
			 , floatTyConKey, doubleTyConKey
			 , addrTyConKey, ptrTyConKey, funPtrTyConKey
			 , charTyConKey, foreignObjTyConKey
			 , foreignPtrTyConKey
			 , stablePtrTyConKey
			 , byteArrayTyConKey, mutableByteArrayTyConKey
			 , boolTyConKey
			 ]
\end{code}


%************************************************************************
%*									*
\subsection[TysWiredIn-Bool]{The @Bool@ type}
%*									*
%************************************************************************

An ordinary enumeration type, but deeply wired in.  There are no
magical operations on @Bool@ (just the regular Prelude code).

{\em BEGIN IDLE SPECULATION BY SIMON}

This is not the only way to encode @Bool@.  A more obvious coding makes
@Bool@ just a boxed up version of @Bool#@, like this:
\begin{verbatim}
type Bool# = Int#
data Bool = MkBool Bool#
\end{verbatim}

Unfortunately, this doesn't correspond to what the Report says @Bool@
looks like!  Furthermore, we get slightly less efficient code (I
think) with this coding. @gtInt@ would look like this:

\begin{verbatim}
gtInt :: Int -> Int -> Bool
gtInt x y = case x of I# x# ->
	    case y of I# y# ->
	    case (gtIntPrim x# y#) of
		b# -> MkBool b#
\end{verbatim}

Notice that the result of the @gtIntPrim@ comparison has to be turned
into an integer (here called @b#@), and returned in a @MkBool@ box.

The @if@ expression would compile to this:
\begin{verbatim}
case (gtInt x y) of
  MkBool b# -> case b# of { 1# -> e1; 0# -> e2 }
\end{verbatim}

I think this code is a little less efficient than the previous code,
but I'm not certain.  At all events, corresponding with the Report is
important.  The interesting thing is that the language is expressive
enough to describe more than one alternative; and that a type doesn't
necessarily need to be a straightforwardly boxed version of its
primitive counterpart.

{\em END IDLE SPECULATION BY SIMON}

\begin{code}
boolTy = mkTyConTy boolTyCon

boolTyCon = pcTyCon EnumTyCon NonRecursive boolTyConName
		    [] [] [falseDataCon, trueDataCon]

falseDataCon = pcDataCon falseDataConName [] [] [] boolTyCon
trueDataCon  = pcDataCon trueDataConName  [] [] [] boolTyCon

falseDataConId = dataConId falseDataCon
trueDataConId  = dataConId trueDataCon
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-List]{The @List@ type (incl ``build'' magic)}
%*									*
%************************************************************************

Special syntax, deeply wired in, but otherwise an ordinary algebraic
data types:
\begin{verbatim}
data [] a = [] | a : (List a)
data () = ()
data (,) a b = (,,) a b
...
\end{verbatim}

\begin{code}
mkListTy :: Type -> Type
mkListTy ty = mkTyConApp listTyCon [ty]

listTyCon = pcRecDataTyCon listTyConName
			alpha_tyvar [(True,False)] [nilDataCon, consDataCon]

nilDataCon  = pcDataCon nilDataConName alpha_tyvar [] [] listTyCon
consDataCon = pcDataCon consDataConName
 	       alpha_tyvar [] [alphaTy, mkTyConApp listTyCon alpha_ty] listTyCon
-- Interesting: polymorphic recursion would help here.
-- We can't use (mkListTy alphaTy) in the defn of consDataCon, else mkListTy
-- gets the over-specific type (Type -> Type)
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-Tuples]{The @Tuple@ types}
%*									*
%************************************************************************

The tuple types are definitely magic, because they form an infinite
family.

\begin{itemize}
\item
They have a special family of type constructors, of type @TyCon@
These contain the tycon arity, but don't require a Unique.

\item
They have a special family of constructors, of type
@Id@. Again these contain their arity but don't need a Unique.

\item
There should be a magic way of generating the info tables and
entry code for all tuples.

But at the moment we just compile a Haskell source
file\srcloc{lib/prelude/...} containing declarations like:
\begin{verbatim}
data Tuple0		= Tup0
data Tuple2  a b	= Tup2	a b
data Tuple3  a b c	= Tup3	a b c
data Tuple4  a b c d	= Tup4	a b c d
...
\end{verbatim}
The print-names associated with the magic @Id@s for tuple constructors
``just happen'' to be the same as those generated by these
declarations.

\item
The instance environment should have a magic way to know
that each tuple type is an instances of classes @Eq@, @Ix@, @Ord@ and
so on. \ToDo{Not implemented yet.}

\item
There should also be a way to generate the appropriate code for each
of these instances, but (like the info tables and entry code) it is
done by enumeration\srcloc{lib/prelude/InTup?.hs}.
\end{itemize}

\begin{code}
mkTupleTy :: Boxity -> Int -> [Type] -> Type
mkTupleTy boxity arity tys = mkTyConApp (tupleTyCon boxity arity) tys

unitTy    = mkTupleTy Boxed 0 []
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Wired In Type Constructors for Representation Types}
%*                                                                      *
%************************************************************************

The following code defines the wired in datatypes cross, plus, unit
and c_of needed for the generic methods.

Ok, so the basic story is that for each type constructor I need to
create 2 things - a TyCon and a DataCon and then we are basically
ok. There are going to be no arguments passed to these functions
because -well- there is nothing to pass to these functions.

\begin{code}
crossTyCon :: TyCon
crossTyCon = pcNonRecDataTyCon crossTyConName alpha_beta_tyvars [] [crossDataCon]

crossDataCon :: DataCon
crossDataCon = pcDataCon crossDataConName alpha_beta_tyvars [] [alphaTy, betaTy] crossTyCon

plusTyCon :: TyCon
plusTyCon = pcNonRecDataTyCon plusTyConName alpha_beta_tyvars [] [inlDataCon, inrDataCon]

inlDataCon, inrDataCon :: DataCon
inlDataCon = pcDataCon inlDataConName alpha_beta_tyvars [] [alphaTy] plusTyCon
inrDataCon = pcDataCon inrDataConName alpha_beta_tyvars [] [betaTy]  plusTyCon

genUnitTyCon :: TyCon 	-- The "1" type constructor for generics
genUnitTyCon = pcNonRecDataTyCon genUnitTyConName [] [] [genUnitDataCon]

genUnitDataCon :: DataCon
genUnitDataCon = pcDataCon genUnitDataConName [] [] [] genUnitTyCon
\end{code}
