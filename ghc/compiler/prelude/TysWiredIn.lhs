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

	boolTy,
	boolTyCon,
	charDataCon,
	charTy,
	charTyCon,
	consDataCon,
	doubleDataCon,
	doubleTy,
	doubleTyCon,
	falseDataCon, falseDataConId,
	floatDataCon,
	floatTy,
	floatTyCon,

	intDataCon,
	intTy,
	intTyCon,

	integerTy,
	integerTyCon,
	smallIntegerDataCon,
	largeIntegerDataCon,

	listTyCon,

	mkListTy,
	nilDataCon,

	-- tuples
	mkTupleTy,
	tupleTyCon, tupleCon, 
	unitTyCon, unitDataCon, unitDataConId, pairTyCon, 
	unboxedSingletonTyCon, unboxedSingletonDataCon,
	unboxedPairTyCon, unboxedPairDataCon,

	-- Generics
        genUnitTyCon, genUnitDataCon, 
	plusTyCon, inrDataCon, inlDataCon,
	crossTyCon, crossDataCon,

	stringTy,
	trueDataCon, trueDataConId,
	unitTy,
	voidTy,
	wordDataCon,
	wordTy,
	wordTyCon,

        -- parallel arrays
	mkPArrTy,
	parrTyCon, parrFakeCon, isPArrTyCon, isPArrFakeCon
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} MkId( mkDataConWorkId )
import {-# SOURCE #-} Generics( mkTyConGenInfo )

-- friends:
import PrelNames
import TysPrim

-- others:
import Constants	( mAX_TUPLE_SIZE )
import Module		( mkBasePkgModule )
import Name		( Name, nameUnique, nameOccName, 
			  nameModule, mkWiredInName )
import OccName		( mkOccFS, tcName, dataName, mkDataConWorkerOcc, mkGenOcc1, mkGenOcc2 )
import DataCon		( DataCon, mkDataCon, dataConWorkId, dataConSourceArity )
import Var		( TyVar, tyVarKind )
import TyCon		( TyCon, AlgTyConFlavour(..), DataConDetails(..), tyConDataCons,
			  mkTupleTyCon, mkAlgTyCon, tyConName
			)

import BasicTypes	( Arity, RecFlag(..), Boxity(..), isBoxed )

import Type		( Type, mkTyConTy, mkTyConApp, mkTyVarTy, mkTyVarTys, 
			  mkArrowKinds, liftedTypeKind, unliftedTypeKind,
			  ThetaType )
import Unique		( incrUnique, mkTupleTyConUnique,
			  mkTupleDataConUnique, mkPArrDataConUnique )
import PrelNames
import Array
import FastString

alpha_tyvar	  = [alphaTyVar]
alpha_ty	  = [alphaTy]
alpha_beta_tyvars = [alphaTyVar, betaTyVar]
\end{code}


%************************************************************************
%*									*
\subsection{Wired in type constructors}
%*									*
%************************************************************************

If you change which things are wired in, make sure you change their
names in PrelNames, so they use wTcQual, wDataQual, etc

\begin{code}
wiredInTyCons :: [TyCon]
wiredInTyCons = data_tycons ++ tuple_tycons ++ unboxed_tuple_tycons

data_tycons = genericTyCons ++
	      [ boolTyCon
    	      , charTyCon
    	      , doubleTyCon
    	      , floatTyCon
    	      , intTyCon
    	      , integerTyCon
    	      , listTyCon
	      , parrTyCon
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
                (DataCons cons)
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

pcDataCon :: Name -> [TyVar] -> ThetaType -> [Type] -> TyCon -> DataCon
-- The Name should be in the DataName name space; it's the name
-- of the DataCon itself.
--
-- The unique is the first of two free uniques;
-- the first is used for the datacon itself,
-- the second is used for the "worker name"

pcDataCon dc_name tyvars context arg_tys tycon
  = data_con
  where
    data_con = mkDataCon dc_name	
                [{- No strictness -}]
                [{- No labelled fields -}]
                tyvars context [] [] arg_tys tycon work_id 
		Nothing {- No wrapper for wired-in things
			   (they are too simple to need one) -}

    mod      = nameModule dc_name
    wrk_occ  = mkDataConWorkerOcc (nameOccName dc_name)
    wrk_key  = incrUnique (nameUnique dc_name)
    wrk_name = mkWiredInName mod wrk_occ wrk_key
    work_id  = mkDataConWorkId wrk_name data_con
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
boxedTupleArr   = listArray (0,mAX_TUPLE_SIZE) [mk_tuple Boxed i | i <- [0..mAX_TUPLE_SIZE]]
unboxedTupleArr = listArray (0,mAX_TUPLE_SIZE) [mk_tuple Unboxed i | i <- [0..mAX_TUPLE_SIZE]]

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
	mod	  = mkBasePkgModule mod_name
	gen_info  = mk_tc_gen_info mod tc_uniq tc_name tycon

unitTyCon     = tupleTyCon Boxed 0
unitDataCon   = head (tyConDataCons unitTyCon)
unitDataConId = dataConWorkId unitDataCon

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
\end{code}

\begin{code}
wordTy = mkTyConTy wordTyCon

wordTyCon = pcNonRecDataTyCon wordTyConName [] [] [wordDataCon]
wordDataCon = pcDataCon wordDataConName [] [] [wordPrimTy] wordTyCon
\end{code}

\begin{code}
floatTy	= mkTyConTy floatTyCon

floatTyCon   = pcNonRecDataTyCon floatTyConName   [] [] [floatDataCon]
floatDataCon = pcDataCon         floatDataConName [] [] [floatPrimTy] floatTyCon
\end{code}

\begin{code}
doubleTy = mkTyConTy doubleTyCon

doubleTyCon   = pcNonRecDataTyCon doubleTyConName   [] [] [doubleDataCon]
doubleDataCon = pcDataCon	  doubleDataConName [] [] [doublePrimTy] doubleTyCon
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

falseDataConId = dataConWorkId falseDataCon
trueDataConId  = dataConWorkId trueDataCon
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
%*									*
\subsection[TysWiredIn-PArr]{The @[::]@ type}
%*									*
%************************************************************************

Special syntax for parallel arrays needs some wired in definitions.

\begin{code}
-- construct a type representing the application of the parallel array
-- constructor 
--
mkPArrTy    :: Type -> Type
mkPArrTy ty  = mkTyConApp parrTyCon [ty]

-- represents the type constructor of parallel arrays
--
-- * this must match the definition in `PrelPArr'
--
-- NB: Although the constructor is given here, it will not be accessible in
--     user code as it is not in the environment of any compiled module except
--     `PrelPArr'.
--
parrTyCon :: TyCon
parrTyCon  = tycon
  where
    tycon   = mkAlgTyCon 
		parrTyConName 
		kind
		tyvars
		[]              	 -- No context
		[(True, False)]
		(DataCons [parrDataCon]) -- The constructor defined in `PrelPArr'
		[]			 -- No record selectors
		DataTyCon
		NonRecursive
		genInfo
    tyvars  = alpha_tyvar
    mod     = nameModule parrTyConName
    kind    = mkArrowKinds (map tyVarKind tyvars) liftedTypeKind
    genInfo = mk_tc_gen_info mod (nameUnique parrTyConName) parrTyConName tycon

parrDataCon :: DataCon
parrDataCon  = pcDataCon 
	         parrDataConName 
		 alpha_tyvar		-- forall'ed type variables
		 []			-- context
		 [intPrimTy,		-- 1st argument: Int#
		  mkTyConApp		-- 2nd argument: Array# a
		    arrayPrimTyCon 
		    alpha_ty] 
		 parrTyCon

-- check whether a type constructor is the constructor for parallel arrays
--
isPArrTyCon    :: TyCon -> Bool
isPArrTyCon tc  = tyConName tc == parrTyConName

-- fake array constructors
--
-- * these constructors are never really used to represent array values;
--   however, they are very convenient during desugaring (and, in particular,
--   in the pattern matching compiler) to treat array pattern just like
--   yet another constructor pattern
--
parrFakeCon                        :: Arity -> DataCon
parrFakeCon i | i > mAX_TUPLE_SIZE  = mkPArrFakeCon  i	-- build one specially
parrFakeCon i                       = parrFakeConArr!i

-- pre-defined set of constructors
--
parrFakeConArr :: Array Int DataCon
parrFakeConArr  = array (0, mAX_TUPLE_SIZE) [(i, mkPArrFakeCon i)   
					    | i <- [0..mAX_TUPLE_SIZE]]

-- build a fake parallel array constructor for the given arity
--
mkPArrFakeCon       :: Int -> DataCon
mkPArrFakeCon arity  = pcDataCon name [tyvar] [] tyvarTys parrTyCon
  where
	tyvar     = head alphaTyVars
	tyvarTys  = replicate arity $ mkTyVarTy tyvar
        nameStr   = mkFastString ("MkPArr" ++ show arity)
	name      = mkWiredInName mod (mkOccFS dataName nameStr) uniq
	uniq      = mkPArrDataConUnique arity
	mod	  = mkBasePkgModule pREL_PARR_Name

-- checks whether a data constructor is a fake constructor for parallel arrays
--
isPArrFakeCon      :: DataCon -> Bool
isPArrFakeCon dcon  = dcon == parrFakeCon (dataConSourceArity dcon)
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
