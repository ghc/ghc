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
	falseDataCon,
	floatDataCon,
	floatTy,
	isFloatTy,
	floatTyCon,

	voidTyCon, voidTy, 

	intDataCon,
	intTy,
	intTyCon,
	isIntTy,
	inIntRange,

	int8TyCon,
	int16TyCon,
	int32TyCon,

	int64TyCon,
	int64DataCon,
--	int64Ty,

	integerTy,
	integerTyCon,
	integerDataCon,
	isIntegerTy,

	listTyCon,

	mkListTy,
	nilDataCon,

	-- tuples
	mkTupleTy,
	tupleTyCon, tupleCon, unitTyCon, unitDataCon, pairTyCon, pairDataCon,

	-- unboxed tuples
	mkUnboxedTupleTy,
	unboxedTupleTyCon, unboxedTupleCon, 
	unboxedPairTyCon, unboxedPairDataCon,

	stateDataCon,
	stateTyCon,
	realWorldStateTy,

	stablePtrTyCon,
	stringTy,
	trueDataCon,
	unitTy,
	wordDataCon,
	wordTy,
	wordTyCon,

	word8TyCon,
	word16TyCon,
	word32TyCon,

	word64DataCon,
--	word64Ty,
	word64TyCon,
	
	isFFIArgumentTy,  -- :: Type -> Bool
	isFFIResultTy,    -- :: Type -> Bool
	isFFIExternalTy,  -- :: Type -> Bool
	isAddrTy,	  -- :: Type -> Bool

    ) where

#include "HsVersions.h"

import {-# SOURCE #-} MkId( mkDataConId )

-- friends:
import PrelMods
import TysPrim

-- others:
import Constants	( mAX_TUPLE_SIZE )
import Name		( mkWiredInTyConName, mkWiredInIdName, mkTupNameStr,
			  mkUbxTupNameStr )
import DataCon		( DataCon, mkDataCon )
import Var		( TyVar, tyVarKind )
import TyCon		( TyCon, mkAlgTyCon, mkSynTyCon, mkTupleTyCon )
import BasicTypes	( Module, Arity, NewOrData(..), 
			  RecFlag(..), StrictnessMark(..) )
import Type		( Type, mkTyConTy, mkTyConApp, mkSigmaTy, mkTyVarTys, 
			  mkArrowKinds, boxedTypeKind, unboxedTypeKind,
			  mkFunTy, mkFunTys, isUnLiftedType,
			  splitTyConApp_maybe, splitAlgTyConApp_maybe,
			  GenType(..), ThetaType, TauType )
import PrimRep		( PrimRep(..) )
import Unique
import CmdLineOpts      ( opt_GlasgowExts )
import Util		( assoc, panic )
import Array

alpha_tyvar	  = [alphaTyVar]
alpha_ty	  = [alphaTy]
alpha_beta_tyvars = [alphaTyVar, betaTyVar]

pcRecDataTyCon, pcNonRecDataTyCon, pcNonRecNewTyCon
	:: Unique{-TyConKey-} -> Module -> FAST_STRING
	-> [TyVar] -> [DataCon] -> TyCon

pcRecDataTyCon    = pcTyCon DataType Recursive
pcNonRecDataTyCon = pcTyCon DataType NonRecursive
pcNonRecNewTyCon  = pcTyCon NewType  NonRecursive

pcTyCon new_or_data is_rec key mod str tyvars cons
  = tycon
  where
    tycon = mkAlgTyCon name kind 
		tyvars 
		[] 		-- No context
		cons
		[]		-- No derivings
		Nothing		-- Not a dictionary
		new_or_data
		is_rec

    name = mkWiredInTyConName key mod str tycon
    kind = mkArrowKinds (map tyVarKind tyvars) boxedTypeKind

pcSynTyCon key mod str kind arity tyvars expansion
  = tycon
  where
    tycon = mkSynTyCon name kind arity tyvars expansion
    name  = mkWiredInTyConName key mod str tycon

pcDataCon :: Unique{-DataConKey-} -> Module -> FAST_STRING
	  -> [TyVar] -> ThetaType -> [TauType] -> TyCon -> DataCon
pcDataCon key mod str tyvars context arg_tys tycon
  = data_con
  where
    data_con = mkDataCon name 
		[ NotMarkedStrict | a <- arg_tys ]
		[ {- no labelled fields -} ]
		tyvars context [] [] arg_tys tycon id
    name = mkWiredInIdName key mod str id
    id   = mkDataConId data_con
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-tuples]{The tuple types}
%*									*
%************************************************************************

\begin{code}
tupleTyCon :: Arity -> TyCon
tupleTyCon i | i > mAX_TUPLE_SIZE = fst (mk_tuple i)	-- Build one specially
	     | otherwise	  = tupleTyConArr!i

tupleCon :: Arity -> DataCon
tupleCon i | i > mAX_TUPLE_SIZE = snd (mk_tuple i)	-- Build one specially
	   | otherwise	        = tupleConArr!i

tupleTyCons :: [TyCon]
tupleTyCons = elems tupleTyConArr

tupleTyConArr :: Array Int TyCon
tupleTyConArr = array (0,mAX_TUPLE_SIZE) ([0..] `zip` map fst tuples)

tupleConArr :: Array Int DataCon
tupleConArr = array (0,mAX_TUPLE_SIZE) ([0..] `zip` map snd tuples)

tuples :: [(TyCon,DataCon)]
tuples = [mk_tuple i | i <- [0..mAX_TUPLE_SIZE]]

mk_tuple :: Int -> (TyCon,DataCon)
mk_tuple arity = (tycon, tuple_con)
  where
	tycon   = mkTupleTyCon tc_name tc_kind arity tyvars tuple_con True
	tc_name = mkWiredInTyConName tc_uniq mod_name name_str tycon
    	tc_kind = mkArrowKinds (map tyVarKind tyvars) boxedTypeKind

	tuple_con = pcDataCon dc_uniq mod_name name_str tyvars [] tyvar_tys tycon
	tyvars    = take arity alphaTyVars
	tyvar_tys = mkTyVarTys tyvars
	(mod_name, name_str) = mkTupNameStr arity
 	tc_uniq   = mkTupleTyConUnique   arity
	dc_uniq   = mkTupleDataConUnique arity

unitTyCon = tupleTyCon 0
pairTyCon = tupleTyCon 2

unitDataCon = tupleCon 0
pairDataCon = tupleCon 2
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-ubx-tuples]{Unboxed Tuple Types}
%*									*
%************************************************************************

\begin{code}
unboxedTupleTyCon :: Arity -> TyCon
unboxedTupleTyCon i | i > mAX_TUPLE_SIZE = fst (mk_unboxed_tuple i)
	            | otherwise	         = unboxedTupleTyConArr!i

unboxedTupleCon :: Arity -> DataCon
unboxedTupleCon i | i > mAX_TUPLE_SIZE = snd (mk_unboxed_tuple i)
	          | otherwise	       = unboxedTupleConArr!i

unboxedTupleTyConArr :: Array Int TyCon
unboxedTupleTyConArr = array (0,mAX_TUPLE_SIZE) ([0..] `zip` map fst ubx_tuples)

unboxedTupleConArr :: Array Int DataCon
unboxedTupleConArr = array (0,mAX_TUPLE_SIZE) ([0..] `zip` map snd ubx_tuples)

ubx_tuples :: [(TyCon,DataCon)]
ubx_tuples = [mk_unboxed_tuple i | i <- [0..mAX_TUPLE_SIZE]]

mk_unboxed_tuple :: Int -> (TyCon,DataCon)
mk_unboxed_tuple arity = (tycon, tuple_con)
  where
	tycon   = mkTupleTyCon tc_name tc_kind arity tyvars tuple_con False
	tc_name = mkWiredInTyConName tc_uniq mod_name name_str tycon
    	tc_kind = mkArrowKinds (map tyVarKind tyvars) unboxedTypeKind

	tuple_con = pcDataCon dc_uniq mod_name name_str tyvars [] tyvar_tys tycon
	tyvars    = take arity openAlphaTyVars
	tyvar_tys = mkTyVarTys tyvars
	(mod_name, name_str) = mkUbxTupNameStr arity
 	tc_uniq   = mkUbxTupleTyConUnique   arity
	dc_uniq   = mkUbxTupleDataConUnique arity

unboxedPairTyCon   = unboxedTupleTyCon 2
unboxedPairDataCon = unboxedTupleCon 2
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
voidTy    = mkTyConTy voidTyCon
voidTyCon = pcNonRecDataTyCon voidTyConKey pREL_GHC SLIT("Void") [] [{-No data cons-}]
\end{code}

\begin{code}
charTy = mkTyConTy charTyCon

charTyCon = pcNonRecDataTyCon charTyConKey  pREL_BASE  SLIT("Char") [] [charDataCon]
charDataCon = pcDataCon charDataConKey pREL_BASE SLIT("C#") [] [] [charPrimTy] charTyCon

stringTy = mkListTy charTy -- convenience only
\end{code}

\begin{code}
intTy = mkTyConTy intTyCon 

intTyCon = pcNonRecDataTyCon intTyConKey pREL_BASE SLIT("Int") [] [intDataCon]
intDataCon = pcDataCon intDataConKey pREL_BASE SLIT("I#") [] [] [intPrimTy] intTyCon

isIntTy :: GenType flexi -> Bool
isIntTy ty
  = case (splitAlgTyConApp_maybe ty) of
	Just (tycon, [], _) -> getUnique tycon == intTyConKey
	_		    -> False

inIntRange :: Integer -> Bool	-- Tells if an integer lies in the legal range of Ints
inIntRange i = (min_int <= i) && (i <= max_int)

max_int, min_int :: Integer
max_int = toInteger maxInt  
min_int = toInteger minInt

int8TyCon = pcNonRecDataTyCon int8TyConKey iNT SLIT("Int8") [] [int8DataCon]
  where
   int8DataCon = pcDataCon int8DataConKey iNT SLIT("I8#") [] [] [intPrimTy] int8TyCon

int16TyCon = pcNonRecDataTyCon int16TyConKey iNT SLIT("Int16") [] [int16DataCon]
  where
   int16DataCon = pcDataCon int16DataConKey iNT SLIT("I16#") [] [] [intPrimTy] int16TyCon

int32TyCon = pcNonRecDataTyCon int32TyConKey iNT SLIT("Int32") [] [int32DataCon]
  where
   int32DataCon = pcDataCon int32DataConKey iNT SLIT("I32#") [] [] [intPrimTy] int32TyCon

int64Ty = mkTyConTy int64TyCon 

int64TyCon = pcNonRecDataTyCon int64TyConKey pREL_ADDR SLIT("Int64") [] [int64DataCon]
int64DataCon = pcDataCon int64DataConKey pREL_ADDR SLIT("I64#") [] [] [int64PrimTy] int64TyCon
\end{code}

\begin{code}

wordTy = mkTyConTy wordTyCon

wordTyCon = pcNonRecDataTyCon wordTyConKey   pREL_ADDR SLIT("Word") [] [wordDataCon]
wordDataCon = pcDataCon wordDataConKey pREL_ADDR SLIT("W#") [] [] [wordPrimTy] wordTyCon

word8TyCon = pcNonRecDataTyCon word8TyConKey   wORD SLIT("Word8") [] [word8DataCon]
  where
   word8DataCon = pcDataCon word8DataConKey wORD SLIT("W8#") [] [] [wordPrimTy] word8TyCon

word16TyCon = pcNonRecDataTyCon word16TyConKey   wORD SLIT("Word16") [] [word16DataCon]
  where
   word16DataCon = pcDataCon word16DataConKey wORD SLIT("W16#") [] [] [wordPrimTy] word16TyCon

word32TyCon = pcNonRecDataTyCon word32TyConKey   wORD SLIT("Word32") [] [word32DataCon]
  where
   word32DataCon = pcDataCon word32DataConKey wORD SLIT("W32#") [] [] [wordPrimTy] word32TyCon

word64Ty = mkTyConTy word64TyCon

word64TyCon = pcNonRecDataTyCon word64TyConKey   pREL_ADDR SLIT("Word64") [] [word64DataCon]
word64DataCon = pcDataCon word64DataConKey pREL_ADDR SLIT("W64#") [] [] [word64PrimTy] word64TyCon
\end{code}

\begin{code}
addrTy = mkTyConTy addrTyCon

addrTyCon = pcNonRecDataTyCon addrTyConKey   pREL_ADDR SLIT("Addr") [] [addrDataCon]
addrDataCon = pcDataCon addrDataConKey pREL_ADDR SLIT("A#") [] [] [addrPrimTy] addrTyCon

isAddrTy :: GenType flexi -> Bool
isAddrTy ty
  = case (splitAlgTyConApp_maybe ty) of
	Just (tycon, [], _) -> getUnique tycon == addrTyConKey
	_		    -> False

\end{code}

\begin{code}
floatTy	= mkTyConTy floatTyCon

floatTyCon = pcNonRecDataTyCon floatTyConKey pREL_BASE SLIT("Float") [] [floatDataCon]
floatDataCon = pcDataCon floatDataConKey pREL_BASE SLIT("F#") [] [] [floatPrimTy] floatTyCon

isFloatTy :: GenType flexi -> Bool
isFloatTy ty
  = case (splitAlgTyConApp_maybe ty) of
	Just (tycon, [], _) -> getUnique tycon == floatTyConKey
	_		    -> False

\end{code}

\begin{code}
doubleTy = mkTyConTy doubleTyCon

isDoubleTy :: GenType flexi -> Bool
isDoubleTy ty
  = case (splitAlgTyConApp_maybe ty) of
	Just (tycon, [], _) -> getUnique tycon == doubleTyConKey
	_		    -> False

doubleTyCon = pcNonRecDataTyCon doubleTyConKey pREL_BASE SLIT("Double") [] [doubleDataCon]
doubleDataCon = pcDataCon doubleDataConKey pREL_BASE SLIT("D#") [] [] [doublePrimTy] doubleTyCon
\end{code}

\begin{code}
mkStateTy ty	 = mkTyConApp stateTyCon [ty]
realWorldStateTy = mkStateTy realWorldTy -- a common use

stateTyCon = pcNonRecDataTyCon stateTyConKey pREL_ST SLIT("State") alpha_tyvar [stateDataCon]
stateDataCon
  = pcDataCon stateDataConKey pREL_ST SLIT("S#")
	alpha_tyvar [] [mkStatePrimTy alphaTy] stateTyCon
\end{code}

\begin{code}
stablePtrTyCon
  = pcNonRecDataTyCon stablePtrTyConKey pREL_FOREIGN SLIT("StablePtr")
	alpha_tyvar [stablePtrDataCon]
  where
    stablePtrDataCon
      = pcDataCon stablePtrDataConKey pREL_FOREIGN SLIT("StablePtr")
	    alpha_tyvar [] [mkStablePtrPrimTy alphaTy] stablePtrTyCon
\end{code}

\begin{code}
foreignObjTyCon
  = pcNonRecDataTyCon foreignObjTyConKey pREL_IO_BASE SLIT("ForeignObj")
	[] [foreignObjDataCon]
  where
    foreignObjDataCon
      = pcDataCon foreignObjDataConKey pREL_IO_BASE SLIT("ForeignObj")
	    [] [] [foreignObjPrimTy] foreignObjTyCon
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-Integer]{@Integer@ and its related ``pairing'' types}
%*									*
%************************************************************************

@Integer@ and its pals are not really primitive.  @Integer@ itself, first:
\begin{code}
integerTy :: GenType t
integerTy    = mkTyConTy integerTyCon

integerTyCon = pcNonRecDataTyCon integerTyConKey pREL_BASE SLIT("Integer") [] [integerDataCon]

integerDataCon = pcDataCon integerDataConKey pREL_BASE SLIT("J#")
		[] [] [intPrimTy, intPrimTy, byteArrayPrimTy] integerTyCon

isIntegerTy :: GenType flexi -> Bool
isIntegerTy ty
  = case (splitAlgTyConApp_maybe ty) of
	Just (tycon, [], _) -> getUnique tycon == integerTyConKey
	_		    -> False
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
isFFIArgumentTy :: Type -> Bool
isFFIArgumentTy ty =
  (opt_GlasgowExts && isUnLiftedType ty) || --leave out for now: maybeToBool (maybeBoxedPrimType ty))) ||
  case (splitAlgTyConApp_maybe ty) of
    Just (tycon, _, _) -> (getUnique tycon) `elem` primArgTyConKeys
    _		       -> False

-- types that can be passed as arguments to "foreign" functions
primArgTyConKeys 
  = [ intTyConKey, int8TyConKey, int16TyConKey, int32TyConKey, int64TyConKey
    , wordTyConKey, word8TyConKey, word16TyConKey, word32TyConKey, word64TyConKey
    , floatTyConKey, doubleTyConKey
    , addrTyConKey, charTyConKey, foreignObjTyConKey
    , stablePtrTyConKey, byteArrayTyConKey, mutableByteArrayTyConKey
    ]

-- types that can be passed from the outside world into Haskell.
-- excludes (mutable) byteArrays.
isFFIExternalTy :: Type -> Bool
isFFIExternalTy ty = 
  (opt_GlasgowExts && isUnLiftedType ty) || --leave out for now: maybeToBool (maybeBoxedPrimType ty))) ||
  case (splitAlgTyConApp_maybe ty) of
    Just (tycon, _, _) -> 
       let 
        u_tycon = getUnique tycon
       in  
       (u_tycon `elem` primArgTyConKeys) &&
       not (u_tycon `elem` notLegalExternalTyCons)
    _		       -> False


isFFIResultTy :: Type -> Bool
isFFIResultTy ty =
   not (isUnLiftedType ty) &&
   case (splitAlgTyConApp_maybe ty) of
    Just (tycon, _, _) -> 
	let
	 u_tycon = getUnique tycon
	in
	(u_tycon == getUnique unitTyCon) ||
        ((u_tycon `elem` primArgTyConKeys) && 
	 not (u_tycon `elem` notLegalExternalTyCons))
    _		       -> False

-- it's illegal to return foreign objects and (mutable)
-- bytearrays from a _ccall_ / foreign declaration
-- (or be passed them as arguments in foreign exported functions).
notLegalExternalTyCons =
  [ foreignObjTyConKey, byteArrayTyConKey, mutableByteArrayTyConKey ]
    
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

boolTyCon = pcTyCon EnumType NonRecursive boolTyConKey 
		    pREL_BASE SLIT("Bool") [] [falseDataCon, trueDataCon]

falseDataCon = pcDataCon falseDataConKey pREL_BASE SLIT("False") [] [] [] boolTyCon
trueDataCon  = pcDataCon trueDataConKey	 pREL_BASE SLIT("True")  [] [] [] boolTyCon
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
mkListTy :: GenType t -> GenType t
mkListTy ty = mkTyConApp listTyCon [ty]

alphaListTy = mkSigmaTy alpha_tyvar [] (mkTyConApp listTyCon alpha_ty)

listTyCon = pcRecDataTyCon listTyConKey pREL_BASE SLIT("[]") 
			alpha_tyvar [nilDataCon, consDataCon]

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
mkTupleTy :: Int -> [GenType t] -> GenType t
mkTupleTy arity tys = mkTyConApp (tupleTyCon arity) tys

mkUnboxedTupleTy :: Int -> [GenType t] -> GenType t
mkUnboxedTupleTy arity tys = mkTyConApp (unboxedTupleTyCon arity) tys

unitTy    = mkTupleTy 0 []
\end{code}
