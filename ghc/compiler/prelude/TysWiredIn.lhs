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
	addrDataCon,
	addrTy,
	addrTyCon,
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

	stablePtrTyCon,
	stringTy,
	trueDataCon, trueDataConId,
	unitTy,
	voidTy,
	wordDataCon,
	wordTy,
	wordTyCon,

	isFFIArgumentTy,  -- :: Bool -> Type -> Bool
	isFFIResultTy,    -- :: Type -> Bool
	isFFIExternalTy,  -- :: Type -> Bool
	isAddrTy,	  -- :: Type -> Bool
	isForeignObjTy    -- :: Type -> Bool

    ) where

#include "HsVersions.h"

import {-# SOURCE #-} MkId( mkDataConId, mkDataConWrapId )

-- friends:
import PrelNames
import TysPrim

-- others:
import Constants	( mAX_TUPLE_SIZE )
import Module		( Module, mkPrelModule )
import Name		( mkWiredInTyConName, mkWiredInIdName, mkSrcOccFS, mkWorkerOcc, dataName )
import DataCon		( DataCon, StrictnessMark(..),  mkDataCon, dataConId )
import Var		( TyVar, tyVarKind )
import TyCon		( TyCon, AlgTyConFlavour(..), ArgVrcs, tyConDataCons,
			  mkAlgTyCon, mkSynTyCon, mkTupleTyCon, isUnLiftedTyCon
			)
import BasicTypes	( Arity, NewOrData(..), RecFlag(..), Boxity(..), isBoxed )
import Type		( Type, mkTyConTy, mkTyConApp, mkSigmaTy, mkTyVarTys, 
			  mkArrowKinds, boxedTypeKind, unboxedTypeKind,
			  mkFunTy, mkFunTys,
			  splitTyConApp_maybe, repType,
			  TauType, ClassContext )
import PrimRep		( PrimRep(..) )
import Unique
import CmdLineOpts      ( opt_GlasgowExts )
import Util		( assoc )
import Panic		( panic )
import Array

alpha_tyvar	  = [alphaTyVar]
alpha_ty	  = [alphaTy]
alpha_beta_tyvars = [alphaTyVar, betaTyVar]

pcRecDataTyCon, pcNonRecDataTyCon
	:: Unique{-TyConKey-} -> Module -> FAST_STRING
	-> [TyVar] -> ArgVrcs -> [DataCon] -> TyCon

pcRecDataTyCon    = pcTyCon DataTyCon Recursive
pcNonRecDataTyCon = pcTyCon DataTyCon NonRecursive

pcTyCon new_or_data is_rec key mod str tyvars argvrcs cons
  = tycon
  where
    tycon = mkAlgTyCon name kind 
		tyvars 
		[] 		-- No context
                argvrcs
		cons
		(length cons)
		[]		-- No derivings
		new_or_data
		is_rec

    name = mkWiredInTyConName key mod str tycon
    kind = mkArrowKinds (map tyVarKind tyvars) boxedTypeKind

pcSynTyCon key mod str kind arity tyvars expansion argvrcs  -- this fun never used!
  = tycon
  where
    tycon = mkSynTyCon name kind arity tyvars expansion argvrcs
    name  = mkWiredInTyConName key mod str tycon

pcDataCon :: Unique{-DataConKey-} -> Module -> FAST_STRING
	  -> [TyVar] -> ClassContext -> [TauType] -> TyCon -> DataCon
-- The unique is the first of two free uniques;
-- the first is used for the datacon itself and the worker; 
-- the second is used for the wrapper.
pcDataCon wrap_key mod str tyvars context arg_tys tycon
  = data_con
  where
    data_con = mkDataCon wrap_name 
		[ NotMarkedStrict | a <- arg_tys ]
		[ {- no labelled fields -} ]
		tyvars context [] [] arg_tys tycon work_id wrap_id

    work_occ  = mkWorkerOcc wrap_occ
    work_key  = incrUnique wrap_key
    work_name = mkWiredInIdName work_key mod work_occ work_id
    work_id   = mkDataConId work_name data_con
    
    wrap_occ  = mkSrcOccFS dataName str
    wrap_name = mkWiredInIdName wrap_key mod wrap_occ wrap_id
    wrap_id   = mkDataConWrapId data_con
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
	tycon   = mkTupleTyCon tc_name tc_kind arity tyvars tuple_con boxity
	tc_name = mkWiredInTyConName tc_uniq mod name_str tycon
    	tc_kind = mkArrowKinds (map tyVarKind tyvars) res_kind
	res_kind | isBoxed boxity = boxedTypeKind
		 | otherwise	  = unboxedTypeKind

	tyvars   | isBoxed boxity = take arity alphaTyVars
		 | otherwise	  = take arity openAlphaTyVars

	tuple_con = pcDataCon dc_uniq mod name_str tyvars [] tyvar_tys tycon
	tyvar_tys = mkTyVarTys tyvars
	(mod_name, name_str) = mkTupNameStr boxity arity
 	tc_uniq   = mkTupleTyConUnique   boxity arity
	dc_uniq   = mkTupleDataConUnique boxity arity
	mod	  = mkPrelModule mod_name

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
-- ) It's boxed; there is only one value of this
-- type, namely "void", whose semantics is just bottom.
--
-- Haskell 98 drops the definition of a Void type, so we just 'simulate'
-- voidTy using ().
voidTy = unitTy
\end{code}


\begin{code}
charTy = mkTyConTy charTyCon

charTyCon = pcNonRecDataTyCon charTyConKey  pREL_BASE  SLIT("Char") [] [] [charDataCon]
charDataCon = pcDataCon charDataConKey pREL_BASE SLIT("C#") [] [] [charPrimTy] charTyCon

stringTy = mkListTy charTy -- convenience only
\end{code}

\begin{code}
intTy = mkTyConTy intTyCon 

intTyCon = pcNonRecDataTyCon intTyConKey pREL_BASE SLIT("Int") [] [] [intDataCon]
intDataCon = pcDataCon intDataConKey pREL_BASE SLIT("I#") [] [] [intPrimTy] intTyCon

isIntTy :: Type -> Bool
isIntTy = isTyCon intTyConKey
\end{code}

\begin{code}

wordTy = mkTyConTy wordTyCon

wordTyCon = pcNonRecDataTyCon wordTyConKey   pREL_ADDR SLIT("Word") [] [] [wordDataCon]
wordDataCon = pcDataCon wordDataConKey pREL_ADDR SLIT("W#") [] [] [wordPrimTy] wordTyCon
\end{code}

\begin{code}
addrTy = mkTyConTy addrTyCon

addrTyCon = pcNonRecDataTyCon addrTyConKey   pREL_ADDR SLIT("Addr") [] [] [addrDataCon]
addrDataCon = pcDataCon addrDataConKey pREL_ADDR SLIT("A#") [] [] [addrPrimTy] addrTyCon

isAddrTy :: Type -> Bool
isAddrTy = isTyCon addrTyConKey
\end{code}

\begin{code}
floatTy	= mkTyConTy floatTyCon

floatTyCon = pcNonRecDataTyCon floatTyConKey pREL_FLOAT SLIT("Float") [] [] [floatDataCon]
floatDataCon = pcDataCon floatDataConKey pREL_FLOAT SLIT("F#") [] [] [floatPrimTy] floatTyCon

isFloatTy :: Type -> Bool
isFloatTy = isTyCon floatTyConKey
\end{code}

\begin{code}
doubleTy = mkTyConTy doubleTyCon

isDoubleTy :: Type -> Bool
isDoubleTy = isTyCon doubleTyConKey

doubleTyCon = pcNonRecDataTyCon doubleTyConKey pREL_FLOAT SLIT("Double") [] [] [doubleDataCon]
doubleDataCon = pcDataCon doubleDataConKey pREL_FLOAT SLIT("D#") [] [] [doublePrimTy] doubleTyCon
\end{code}

\begin{code}
stablePtrTyCon
  = pcNonRecDataTyCon stablePtrTyConKey pREL_STABLE SLIT("StablePtr")
	alpha_tyvar [(True,False)] [stablePtrDataCon]
  where
    stablePtrDataCon
      = pcDataCon stablePtrDataConKey pREL_STABLE SLIT("StablePtr")
	    alpha_tyvar [] [mkStablePtrPrimTy alphaTy] stablePtrTyCon
\end{code}

\begin{code}
foreignObjTyCon
  = pcNonRecDataTyCon foreignObjTyConKey pREL_IO_BASE SLIT("ForeignObj")
	[] [] [foreignObjDataCon]
  where
    foreignObjDataCon
      = pcDataCon foreignObjDataConKey pREL_IO_BASE SLIT("ForeignObj")
	    [] [] [foreignObjPrimTy] foreignObjTyCon

isForeignObjTy :: Type -> Bool
isForeignObjTy = isTyCon foreignObjTyConKey
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

integerTyCon = pcNonRecDataTyCon integerTyConKey pREL_NUM SLIT("Integer")
                   [] [] [smallIntegerDataCon, largeIntegerDataCon]

smallIntegerDataCon = pcDataCon smallIntegerDataConKey pREL_NUM SLIT("S#")
		[] [] [intPrimTy] integerTyCon
largeIntegerDataCon = pcDataCon largeIntegerDataConKey pREL_NUM SLIT("J#")
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
isFFIArgumentTy :: Bool -> Type -> Bool
-- Checks for valid argument type for a 'foreign import'
isFFIArgumentTy is_safe ty = checkTyCon (legalOutgoingTyCon is_safe) ty

isFFIExternalTy :: Type -> Bool
-- Types that are allowed as arguments of a 'foreign export'
isFFIExternalTy ty = checkTyCon legalIncomingTyCon ty

isFFIResultTy :: Type -> Bool
-- Types that are allowed as a result of a 'foreign import' or of a 'foreign export'
-- Maybe we should distinguish between import and export, but 
-- here we just choose the more restrictive 'incoming' predicate
-- But we allow () as well
isFFIResultTy ty = checkTyCon (\tc -> tc == unitTyCon || legalIncomingTyCon tc) ty

checkTyCon :: (TyCon -> Bool) -> Type -> Bool
checkTyCon check_tc ty = case splitTyConApp_maybe (repType ty) of
				Just (tycon, _) -> check_tc tycon
				Nothing		-> False

isTyCon :: Unique -> Type -> Bool
isTyCon uniq ty = checkTyCon (\tc -> uniq == getUnique tc) ty
\end{code}

----------------------------------------------
These chaps do the work; they are not exported
----------------------------------------------

\begin{code}
legalIncomingTyCon :: TyCon -> Bool
-- It's illegal to return foreign objects and (mutable)
-- bytearrays from a _ccall_ / foreign declaration
-- (or be passed them as arguments in foreign exported functions).
legalIncomingTyCon tc
  | getUnique tc `elem` [ foreignObjTyConKey, byteArrayTyConKey, mutableByteArrayTyConKey ] 
  = False
  | otherwise
  = marshalableTyCon tc

legalOutgoingTyCon :: Bool -> TyCon -> Bool
-- Checks validity of types going from Haskell -> external world
-- The boolean is true for a 'safe' call (when we don't want to
-- pass Haskell pointers to the world)
legalOutgoingTyCon be_safe tc
  | be_safe && getUnique tc `elem` [byteArrayTyConKey, mutableByteArrayTyConKey]
  = False
  | otherwise
  = marshalableTyCon tc

marshalableTyCon tc
  =  (opt_GlasgowExts && isUnLiftedTyCon tc)
  || getUnique tc `elem` [ intTyConKey, int8TyConKey, int16TyConKey, int32TyConKey, int64TyConKey
			 , wordTyConKey, word8TyConKey, word16TyConKey, word32TyConKey, word64TyConKey
			 , floatTyConKey, doubleTyConKey
			 , addrTyConKey, charTyConKey, foreignObjTyConKey
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

boolTyCon = pcTyCon EnumTyCon NonRecursive boolTyConKey 
		    pREL_BASE SLIT("Bool") [] [] [falseDataCon, trueDataCon]

falseDataCon = pcDataCon falseDataConKey pREL_BASE SLIT("False") [] [] [] boolTyCon
trueDataCon  = pcDataCon trueDataConKey	 pREL_BASE SLIT("True")  [] [] [] boolTyCon

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

alphaListTy = mkSigmaTy alpha_tyvar [] (mkTyConApp listTyCon alpha_ty)

listTyCon = pcRecDataTyCon listTyConKey pREL_BASE SLIT("[]") 
			alpha_tyvar [(True,False)] [nilDataCon, consDataCon]

nilDataCon  = pcDataCon nilDataConKey  pREL_BASE SLIT("[]") alpha_tyvar [] [] listTyCon
consDataCon = pcDataCon consDataConKey pREL_BASE SLIT(":")
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
