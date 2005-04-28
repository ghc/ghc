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
	wiredInTyCons, 

	boolTy,	boolTyCon, boolTyCon_RDR, boolTyConName,
	trueDataCon,  trueDataConId,  true_RDR,
	falseDataCon, falseDataConId, false_RDR,

	charTyCon, charDataCon, charTyCon_RDR,
	charTy, stringTy, charTyConName,

	
	doubleTyCon, doubleDataCon, doubleTy, doubleTyConName, 
	
	floatTyCon, floatDataCon, floatTy, floatTyConName,

	intTyCon, intDataCon, intTyCon_RDR, intDataCon_RDR, intTyConName,
	intTy,

	listTyCon, nilDataCon, consDataCon,
	listTyCon_RDR, consDataCon_RDR, listTyConName,
	mkListTy,

	-- tuples
	mkTupleTy,
	tupleTyCon, tupleCon, 
	unitTyCon, unitDataCon, unitDataConId, pairTyCon, 
	unboxedSingletonTyCon, unboxedSingletonDataCon,
	unboxedPairTyCon, unboxedPairDataCon,

	unitTy,
	voidTy,

        -- parallel arrays
	mkPArrTy,
	parrTyCon, parrFakeCon, isPArrTyCon, isPArrFakeCon,
	parrTyCon_RDR, parrTyConName
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} MkId( mkDataConIds )

-- friends:
import PrelNames
import TysPrim

-- others:
import Constants	( mAX_TUPLE_SIZE )
import Module		( Module )
import RdrName		( nameRdrName )
import Name		( Name, BuiltInSyntax(..), nameUnique, nameOccName, 
			  nameModule, mkWiredInName )
import OccName		( mkOccFS, tcName, dataName, mkTupleOcc, mkDataConWorkerOcc )
import DataCon		( DataCon, mkDataCon, dataConWorkId, dataConSourceArity )
import Var		( TyVar, tyVarKind )
import TyCon		( TyCon, AlgTyConRhs(DataTyCon), tyConDataCons,
			  mkTupleTyCon, mkAlgTyCon, tyConName
			)

import BasicTypes	( Arity, RecFlag(..), Boxity(..), isBoxed, StrictnessMark(..) )

import Type		( Type, mkTyConTy, mkTyConApp, mkTyVarTy, mkTyVarTys, TyThing(..) )
import Kind		( mkArrowKinds, liftedTypeKind, ubxTupleKind )
import Unique		( incrUnique, mkTupleTyConUnique,
			  mkTupleDataConUnique, mkPArrDataConUnique )
import PrelNames
import Array
import FastString
import Outputable

alpha_tyvar = [alphaTyVar]
alpha_ty    = [alphaTy]
\end{code}


%************************************************************************
%*									*
\subsection{Wired in type constructors}
%*									*
%************************************************************************

If you change which things are wired in, make sure you change their
names in PrelNames, so they use wTcQual, wDataQual, etc

\begin{code}
wiredInTyCons :: [TyCon]	-- Excludes tuples
wiredInTyCons = [ unitTyCon	-- Not treated like other tuples, because
				-- it's defined in GHC.Base, and there's only
				-- one of it.  We put it in wiredInTyCons so
				-- that it'll pre-populate the name cache, so
				-- the special case in lookupOrigNameCache 
				-- doesn't need to look out for it
	      , boolTyCon
    	      , charTyCon
    	      , doubleTyCon
    	      , floatTyCon
    	      , intTyCon
    	      , listTyCon
	      , parrTyCon
    	      ]
\end{code}

\begin{code}
mkWiredInTyConName :: BuiltInSyntax -> Module -> FastString -> Unique -> TyCon -> Name
mkWiredInTyConName built_in mod fs uniq tycon
  = mkWiredInName mod (mkOccFS tcName fs) uniq
		  Nothing 		-- No parent object
		  (ATyCon tycon)	-- Relevant TyCon
		  built_in

mkWiredInDataConName :: BuiltInSyntax -> Module -> FastString -> Unique -> DataCon -> Name -> Name
mkWiredInDataConName built_in mod fs uniq datacon parent
  = mkWiredInName mod (mkOccFS dataName fs) uniq
		  (Just parent) 	-- Name of parent TyCon
		  (ADataCon datacon)	-- Relevant DataCon
		  built_in

charTyConName	  = mkWiredInTyConName   UserSyntax pREL_BASE FSLIT("Char") charTyConKey charTyCon
charDataConName   = mkWiredInDataConName UserSyntax pREL_BASE FSLIT("C#") charDataConKey charDataCon charTyConName
intTyConName	  = mkWiredInTyConName   UserSyntax pREL_BASE FSLIT("Int") intTyConKey   intTyCon
intDataConName	  = mkWiredInDataConName UserSyntax pREL_BASE FSLIT("I#") intDataConKey  intDataCon intTyConName
						  
boolTyConName	  = mkWiredInTyConName   UserSyntax pREL_BASE FSLIT("Bool") boolTyConKey boolTyCon
falseDataConName  = mkWiredInDataConName UserSyntax pREL_BASE FSLIT("False") falseDataConKey falseDataCon boolTyConName
trueDataConName	  = mkWiredInDataConName UserSyntax pREL_BASE FSLIT("True")  trueDataConKey  trueDataCon  boolTyConName
listTyConName	  = mkWiredInTyConName   BuiltInSyntax pREL_BASE FSLIT("[]") listTyConKey listTyCon
nilDataConName 	  = mkWiredInDataConName BuiltInSyntax pREL_BASE FSLIT("[]") nilDataConKey nilDataCon  listTyConName
consDataConName	  = mkWiredInDataConName BuiltInSyntax pREL_BASE FSLIT(":") consDataConKey consDataCon listTyConName

floatTyConName	   = mkWiredInTyConName   UserSyntax pREL_FLOAT FSLIT("Float") floatTyConKey floatTyCon
floatDataConName   = mkWiredInDataConName UserSyntax pREL_FLOAT FSLIT("F#") floatDataConKey floatDataCon floatTyConName
doubleTyConName    = mkWiredInTyConName   UserSyntax pREL_FLOAT FSLIT("Double") doubleTyConKey doubleTyCon
doubleDataConName  = mkWiredInDataConName UserSyntax pREL_FLOAT FSLIT("D#") doubleDataConKey doubleDataCon doubleTyConName

parrTyConName	  = mkWiredInTyConName   BuiltInSyntax pREL_PARR FSLIT("[::]") parrTyConKey parrTyCon 
parrDataConName   = mkWiredInDataConName UserSyntax    pREL_PARR FSLIT("PArr") parrDataConKey parrDataCon parrTyConName

boolTyCon_RDR   = nameRdrName boolTyConName
false_RDR	= nameRdrName falseDataConName
true_RDR	= nameRdrName trueDataConName
intTyCon_RDR	= nameRdrName intTyConName
charTyCon_RDR	= nameRdrName charTyConName
intDataCon_RDR	= nameRdrName intDataConName
listTyCon_RDR	= nameRdrName listTyConName
consDataCon_RDR = nameRdrName consDataConName
parrTyCon_RDR	= nameRdrName parrTyConName
\end{code}


%************************************************************************
%*                                                                      *
\subsection{mkWiredInTyCon}
%*                                                                      *
%************************************************************************

\begin{code}
pcNonRecDataTyCon = pcTyCon False NonRecursive
pcRecDataTyCon    = pcTyCon False Recursive

pcTyCon is_enum is_rec name tyvars argvrcs cons
  = tycon
  where
    tycon = mkAlgTyCon name
		(mkArrowKinds (map tyVarKind tyvars) liftedTypeKind)
                tyvars
                argvrcs
                []		-- No stupid theta
		(DataTyCon cons is_enum)
		[] 		-- No record selectors
                is_rec
		True		-- All the wired-in tycons have generics

pcDataCon :: Name -> [TyVar] -> [Type] -> TyCon -> DataCon
pcDataCon = pcDataConWithFixity False

pcDataConWithFixity :: Bool -> Name -> [TyVar] -> [Type] -> TyCon -> DataCon
-- The Name should be in the DataName name space; it's the name
-- of the DataCon itself.
--
-- The unique is the first of two free uniques;
-- the first is used for the datacon itself,
-- the second is used for the "worker name"

pcDataConWithFixity declared_infix dc_name tyvars arg_tys tycon
  = data_con
  where
    data_con = mkDataCon dc_name declared_infix True {- Vanilla -}
                (map (const NotMarkedStrict) arg_tys)
                [{- No labelled fields -}]
                tyvars [] [] arg_tys tycon (mkTyVarTys tyvars)
		(mkDataConIds bogus_wrap_name wrk_name data_con)
		

    mod      = nameModule dc_name
    wrk_occ  = mkDataConWorkerOcc (nameOccName dc_name)
    wrk_key  = incrUnique (nameUnique dc_name)
    wrk_name = mkWiredInName mod wrk_occ wrk_key
			     (Just (tyConName tycon))
			     (AnId (dataConWorkId data_con)) UserSyntax
    bogus_wrap_name = pprPanic "Wired-in data wrapper id" (ppr dc_name)
	-- Wired-in types are too simple to need wrappers
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
	mod	= mkTupleModule boxity arity
	tc_name = mkWiredInName mod (mkTupleOcc tcName boxity arity) tc_uniq
				Nothing (ATyCon tycon) BuiltInSyntax
    	tc_kind = mkArrowKinds (map tyVarKind tyvars) res_kind
	res_kind | isBoxed boxity = liftedTypeKind
		 | otherwise	  = ubxTupleKind

	tyvars   | isBoxed boxity = take arity alphaTyVars
		 | otherwise	  = take arity openAlphaTyVars

	tuple_con = pcDataCon dc_name tyvars tyvar_tys tycon
	tyvar_tys = mkTyVarTys tyvars
	dc_name   = mkWiredInName mod (mkTupleOcc dataName boxity arity) dc_uniq
				  (Just tc_name) (ADataCon tuple_con) BuiltInSyntax
 	tc_uniq   = mkTupleTyConUnique   boxity arity
	dc_uniq   = mkTupleDataConUnique boxity arity
	gen_info  = True		-- Tuples all have generics..
					-- hmm: that's a *lot* of code

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
charDataCon = pcDataCon charDataConName [] [charPrimTy] charTyCon

stringTy = mkListTy charTy -- convenience only
\end{code}

\begin{code}
intTy = mkTyConTy intTyCon 

intTyCon = pcNonRecDataTyCon intTyConName [] [] [intDataCon]
intDataCon = pcDataCon intDataConName [] [intPrimTy] intTyCon
\end{code}

\begin{code}
floatTy	= mkTyConTy floatTyCon

floatTyCon   = pcNonRecDataTyCon floatTyConName   [] [] [floatDataCon]
floatDataCon = pcDataCon         floatDataConName [] [floatPrimTy] floatTyCon
\end{code}

\begin{code}
doubleTy = mkTyConTy doubleTyCon

doubleTyCon   = pcNonRecDataTyCon doubleTyConName   [] [] [doubleDataCon]
doubleDataCon = pcDataCon	  doubleDataConName [] [doublePrimTy] doubleTyCon
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

boolTyCon = pcTyCon True NonRecursive boolTyConName
		    [] [] [falseDataCon, trueDataCon]

falseDataCon = pcDataCon falseDataConName [] [] boolTyCon
trueDataCon  = pcDataCon trueDataConName  [] [] boolTyCon

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

nilDataCon  = pcDataCon nilDataConName alpha_tyvar [] listTyCon
consDataCon = pcDataConWithFixity True {- Declared infix -}
	       consDataConName
 	       alpha_tyvar [alphaTy, mkTyConApp listTyCon alpha_ty] listTyCon
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
--  * this must match the definition in `PrelPArr'
--
-- NB: Although the constructor is given here, it will not be accessible in
--     user code as it is not in the environment of any compiled module except
--     `PrelPArr'.
--
parrTyCon :: TyCon
parrTyCon  = pcNonRecDataTyCon parrTyConName alpha_tyvar [(True, False)] [parrDataCon]

parrDataCon :: DataCon
parrDataCon  = pcDataCon 
	         parrDataConName 
		 alpha_tyvar		-- forall'ed type variables
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
--  * these constructors are never really used to represent array values;
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
mkPArrFakeCon arity  = data_con
  where
	data_con  = pcDataCon name [tyvar] tyvarTys parrTyCon
	tyvar     = head alphaTyVars
	tyvarTys  = replicate arity $ mkTyVarTy tyvar
        nameStr   = mkFastString ("MkPArr" ++ show arity)
	name      = mkWiredInName pREL_PARR (mkOccFS dataName nameStr) uniq
				  Nothing (ADataCon data_con) UserSyntax
	uniq      = mkPArrDataConUnique arity

-- checks whether a data constructor is a fake constructor for parallel arrays
--
isPArrFakeCon      :: DataCon -> Bool
isPArrFakeCon dcon  = dcon == parrFakeCon (dataConSourceArity dcon)
\end{code}

