%
% (c) The GRASP Project, Glasgow University, 1994-1998
%
\section[TysWiredIn]{Wired-in knowledge about {\em non-primitive} types}

\begin{code}
-- | This module is about types that can be defined in Haskell, but which
--   must be wired into the compiler nonetheless.  C.f module TysPrim
module TysWiredIn (
        -- * All wired in things
	wiredInTyCons, 

        -- * Bool
	boolTy,	boolTyCon, boolTyCon_RDR, boolTyConName,
	trueDataCon,  trueDataConId,  true_RDR,
	falseDataCon, falseDataConId, false_RDR,

        -- * Char
	charTyCon, charDataCon, charTyCon_RDR,
	charTy, stringTy, charTyConName,

	-- * Double
	doubleTyCon, doubleDataCon, doubleTy, doubleTyConName, 
	
	-- * Float
	floatTyCon, floatDataCon, floatTy, floatTyConName,

        -- * Int
	intTyCon, intDataCon, intTyCon_RDR, intDataCon_RDR, intTyConName,
	intTy,

        -- * Word
	wordTyCon, wordDataCon, wordTyConName, wordTy,

        -- * List
	listTyCon, nilDataCon, consDataCon,
	listTyCon_RDR, consDataCon_RDR, listTyConName,
	mkListTy,

	-- * Tuples
	mkTupleTy, mkBoxedTupleTy,
	tupleTyCon, tupleCon, 
	unitTyCon, unitDataCon, unitDataConId, pairTyCon, 
	unboxedSingletonTyCon, unboxedSingletonDataCon,
	unboxedPairTyCon, unboxedPairDataCon,

        -- * Unit
	unitTy,

        -- * Parallel arrays
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
import RdrName
import Name
import DataCon		( DataCon, mkDataCon, dataConWorkId, dataConSourceArity )
import Var
import TyCon		( TyCon, AlgTyConRhs(DataTyCon), tyConDataCons,
			  mkTupleTyCon, mkAlgTyCon, tyConName,
			  TyConParent(NoParentTyCon) )

import BasicTypes	( Arity, RecFlag(..), Boxity(..), isBoxed, HsBang(..) )

import Type		( Type, mkTyConTy, mkTyConApp, mkTyVarTy, mkTyVarTys,
			  TyThing(..) )
import Coercion         ( unsafeCoercionTyCon, symCoercionTyCon,
                          transCoercionTyCon, leftCoercionTyCon, 
                          rightCoercionTyCon, instCoercionTyCon )
import TypeRep          ( mkArrowKinds, liftedTypeKind, ubxTupleKind )
import Unique		( incrUnique, mkTupleTyConUnique,
			  mkTupleDataConUnique, mkPArrDataConUnique )
import Data.Array
import FastString
import Outputable

alpha_tyvar :: [TyVar]
alpha_tyvar = [alphaTyVar]

alpha_ty :: [Type]
alpha_ty = [alphaTy]
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
-- This list is used only to define PrelInfo.wiredInThings

-- It does not need to include kind constructors, because
-- all that wiredInThings does is to initialise the Name table,
-- and kind constructors don't appear in source code.

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
              , unsafeCoercionTyCon
              , symCoercionTyCon
              , transCoercionTyCon
              , leftCoercionTyCon
              , rightCoercionTyCon
              , instCoercionTyCon
    	      ]
\end{code}

\begin{code}
mkWiredInTyConName :: BuiltInSyntax -> Module -> FastString -> Unique -> TyCon -> Name
mkWiredInTyConName built_in modu fs unique tycon
  = mkWiredInName modu (mkTcOccFS fs) unique
		  (ATyCon tycon)	-- Relevant TyCon
		  built_in

mkWiredInDataConName :: BuiltInSyntax -> Module -> FastString -> Unique -> DataCon -> Name
mkWiredInDataConName built_in modu fs unique datacon
  = mkWiredInName modu (mkDataOccFS fs) unique
		  (ADataCon datacon)	-- Relevant DataCon
		  built_in

charTyConName, charDataConName, intTyConName, intDataConName :: Name
charTyConName	  = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Char") charTyConKey charTyCon
charDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "C#") charDataConKey charDataCon
intTyConName	  = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Int") intTyConKey   intTyCon
intDataConName	  = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "I#") intDataConKey  intDataCon

boolTyConName, falseDataConName, trueDataConName :: Name
boolTyConName	  = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Bool") boolTyConKey boolTyCon
falseDataConName  = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "False") falseDataConKey falseDataCon
trueDataConName	  = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "True")  trueDataConKey  trueDataCon

listTyConName, nilDataConName, consDataConName :: Name
listTyConName	  = mkWiredInTyConName   BuiltInSyntax gHC_TYPES (fsLit "[]") listTyConKey listTyCon
nilDataConName 	  = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit "[]") nilDataConKey nilDataCon 
consDataConName	  = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit ":") consDataConKey consDataCon

floatTyConName, floatDataConName, doubleTyConName, doubleDataConName :: Name
floatTyConName	   = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Float") floatTyConKey floatTyCon
floatDataConName   = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "F#") floatDataConKey floatDataCon
doubleTyConName    = mkWiredInTyConName   UserSyntax gHC_TYPES (fsLit "Double") doubleTyConKey doubleTyCon
doubleDataConName  = mkWiredInDataConName UserSyntax gHC_TYPES (fsLit "D#") doubleDataConKey doubleDataCon

parrTyConName, parrDataConName :: Name
parrTyConName	  = mkWiredInTyConName   BuiltInSyntax gHC_PARR (fsLit "[::]") parrTyConKey parrTyCon 
parrDataConName   = mkWiredInDataConName UserSyntax    gHC_PARR (fsLit "PArr") parrDataConKey parrDataCon

boolTyCon_RDR, false_RDR, true_RDR, intTyCon_RDR, charTyCon_RDR,
    intDataCon_RDR, listTyCon_RDR, consDataCon_RDR, parrTyCon_RDR:: RdrName
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
pcNonRecDataTyCon :: Name -> [TyVar] -> [DataCon] -> TyCon
pcNonRecDataTyCon = pcTyCon False NonRecursive
pcRecDataTyCon :: Name -> [TyVar] -> [DataCon] -> TyCon
pcRecDataTyCon    = pcTyCon False Recursive

pcTyCon :: Bool -> RecFlag -> Name -> [TyVar] -> [DataCon] -> TyCon
pcTyCon is_enum is_rec name tyvars cons
  = tycon
  where
    tycon = mkAlgTyCon name
		(mkArrowKinds (map tyVarKind tyvars) liftedTypeKind)
                tyvars
                []		-- No stupid theta
		(DataTyCon cons is_enum)
		NoParentTyCon
                is_rec
		True		-- All the wired-in tycons have generics
		False		-- Not in GADT syntax

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
    data_con = mkDataCon dc_name declared_infix
                (map (const HsNoBang) arg_tys)
                [] 	-- No labelled fields
                tyvars
		[] 	-- No existential type variables
		[]	-- No equality spec
		[]	-- No theta
		arg_tys (mkTyConApp tycon (mkTyVarTys tyvars)) 
		tycon
		[]	-- No stupid theta
		(mkDataConIds bogus_wrap_name wrk_name data_con)
		

    modu     = ASSERT( isExternalName dc_name ) 
	       nameModule dc_name
    wrk_occ  = mkDataConWorkerOcc (nameOccName dc_name)
    wrk_key  = incrUnique (nameUnique dc_name)
    wrk_name = mkWiredInName modu wrk_occ wrk_key
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
	modu	= mkTupleModule boxity arity
	tc_name = mkWiredInName modu (mkTupleOcc tcName boxity arity) tc_uniq
				(ATyCon tycon) BuiltInSyntax
    	tc_kind = mkArrowKinds (map tyVarKind tyvars) res_kind
	res_kind | isBoxed boxity = liftedTypeKind
		 | otherwise	  = ubxTupleKind

	tyvars   | isBoxed boxity = take arity alphaTyVars
		 | otherwise	  = take arity openAlphaTyVars

	tuple_con = pcDataCon dc_name tyvars tyvar_tys tycon
	tyvar_tys = mkTyVarTys tyvars
	dc_name   = mkWiredInName modu (mkTupleOcc dataName boxity arity) dc_uniq
				  (ADataCon tuple_con) BuiltInSyntax
 	tc_uniq   = mkTupleTyConUnique   boxity arity
	dc_uniq   = mkTupleDataConUnique boxity arity
	gen_info  = True		-- Tuples all have generics..
					-- hmm: that's a *lot* of code

unitTyCon :: TyCon
unitTyCon     = tupleTyCon Boxed 0
unitDataCon :: DataCon
unitDataCon   = head (tyConDataCons unitTyCon)
unitDataConId :: Id
unitDataConId = dataConWorkId unitDataCon

pairTyCon :: TyCon
pairTyCon = tupleTyCon Boxed 2

unboxedSingletonTyCon :: TyCon
unboxedSingletonTyCon   = tupleTyCon Unboxed 1
unboxedSingletonDataCon :: DataCon
unboxedSingletonDataCon = tupleCon   Unboxed 1

unboxedPairTyCon :: TyCon
unboxedPairTyCon   = tupleTyCon Unboxed 2
unboxedPairDataCon :: DataCon
unboxedPairDataCon = tupleCon   Unboxed 2
\end{code}


%************************************************************************
%*									*
\subsection[TysWiredIn-boxed-prim]{The ``boxed primitive'' types (@Char@, @Int@, etc)}
%*									*
%************************************************************************

\begin{code}
charTy :: Type
charTy = mkTyConTy charTyCon

charTyCon :: TyCon
charTyCon   = pcNonRecDataTyCon charTyConName [] [charDataCon]
charDataCon :: DataCon
charDataCon = pcDataCon charDataConName [] [charPrimTy] charTyCon

stringTy :: Type
stringTy = mkListTy charTy -- convenience only
\end{code}

\begin{code}
intTy :: Type
intTy = mkTyConTy intTyCon 

intTyCon :: TyCon
intTyCon = pcNonRecDataTyCon intTyConName [] [intDataCon]
intDataCon :: DataCon
intDataCon = pcDataCon intDataConName [] [intPrimTy] intTyCon
\end{code}

\begin{code}
wordTy :: Type
wordTy = mkTyConTy wordTyCon 

wordTyCon :: TyCon
wordTyCon = pcNonRecDataTyCon wordTyConName [] [wordDataCon]
wordDataCon :: DataCon
wordDataCon = pcDataCon wordDataConName [] [wordPrimTy] wordTyCon
\end{code}

\begin{code}
floatTy :: Type
floatTy	= mkTyConTy floatTyCon

floatTyCon :: TyCon
floatTyCon   = pcNonRecDataTyCon floatTyConName   [] [floatDataCon]
floatDataCon :: DataCon
floatDataCon = pcDataCon         floatDataConName [] [floatPrimTy] floatTyCon
\end{code}

\begin{code}
doubleTy :: Type
doubleTy = mkTyConTy doubleTyCon

doubleTyCon :: TyCon
doubleTyCon = pcNonRecDataTyCon doubleTyConName [] [doubleDataCon]

doubleDataCon :: DataCon
doubleDataCon = pcDataCon doubleDataConName [] [doublePrimTy] doubleTyCon
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
boolTy :: Type
boolTy = mkTyConTy boolTyCon

boolTyCon :: TyCon
boolTyCon = pcTyCon True NonRecursive boolTyConName
		    [] [falseDataCon, trueDataCon]

falseDataCon, trueDataCon :: DataCon
falseDataCon = pcDataCon falseDataConName [] [] boolTyCon
trueDataCon  = pcDataCon trueDataConName  [] [] boolTyCon

falseDataConId, trueDataConId :: Id
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

listTyCon :: TyCon
listTyCon = pcRecDataTyCon listTyConName alpha_tyvar [nilDataCon, consDataCon]

nilDataCon :: DataCon
nilDataCon  = pcDataCon nilDataConName alpha_tyvar [] listTyCon

consDataCon :: DataCon
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
mkTupleTy :: Boxity -> [Type] -> Type
-- Special case for *boxed* 1-tuples, which are represented by the type itself
mkTupleTy boxity [ty] | Boxed <- boxity = ty
mkTupleTy boxity tys = mkTyConApp (tupleTyCon boxity (length tys)) tys

-- | Build the type of a small tuple that holds the specified type of thing
mkBoxedTupleTy :: [Type] -> Type
mkBoxedTupleTy tys = mkTupleTy Boxed tys

unitTy :: Type
unitTy = mkTupleTy Boxed []
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-PArr]{The @[::]@ type}
%*									*
%************************************************************************

Special syntax for parallel arrays needs some wired in definitions.

\begin{code}
-- | Construct a type representing the application of the parallel array constructor 
mkPArrTy    :: Type -> Type
mkPArrTy ty  = mkTyConApp parrTyCon [ty]

-- | Represents the type constructor of parallel arrays
--
--  * This must match the definition in @PrelPArr@
--
-- NB: Although the constructor is given here, it will not be accessible in
--     user code as it is not in the environment of any compiled module except
--     @PrelPArr@.
--
parrTyCon :: TyCon
parrTyCon  = pcNonRecDataTyCon parrTyConName alpha_tyvar [parrDataCon]

parrDataCon :: DataCon
parrDataCon  = pcDataCon 
	         parrDataConName 
		 alpha_tyvar		-- forall'ed type variables
		 [intPrimTy,		-- 1st argument: Int#
		  mkTyConApp		-- 2nd argument: Array# a
		    arrayPrimTyCon 
		    alpha_ty] 
		 parrTyCon

-- | Check whether a type constructor is the constructor for parallel arrays
isPArrTyCon    :: TyCon -> Bool
isPArrTyCon tc  = tyConName tc == parrTyConName

-- | Fake array constructors
--
-- * These constructors are never really used to represent array values;
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
	name      = mkWiredInName gHC_PARR (mkDataOccFS nameStr) unique
				  (ADataCon data_con) UserSyntax
	unique      = mkPArrDataConUnique arity

-- | Checks whether a data constructor is a fake constructor for parallel arrays
isPArrFakeCon      :: DataCon -> Bool
isPArrFakeCon dcon  = dcon == parrFakeCon (dataConSourceArity dcon)
\end{code}


