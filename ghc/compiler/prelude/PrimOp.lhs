%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrimOp]{Primitive operations (machine-level)}

\begin{code}
module PrimOp (
	PrimOp(..), allThePrimOps,
	primOpType, primOpSig, primOpUsg, primOpArity,
	mkPrimOpIdName, primOpRdrName, primOpTag, primOpOcc,

	commutableOp,

	primOpOutOfLine, primOpNeedsWrapper, 
	primOpOkForSpeculation, primOpIsCheap, primOpIsDupable,
	primOpHasSideEffects,

	getPrimOpResultInfo,  PrimOpResultInfo(..),

	pprPrimOp,

	CCall(..), CCallTarget(..), ccallMayGC, ccallIsCasm, pprCCallOp,
	isDynamicTarget, dynamicTarget, setCCallUnique
    ) where

#include "HsVersions.h"

import PrimRep		-- most of it
import TysPrim
import TysWiredIn

import Demand		( wwLazy, wwPrim, wwStrict, StrictnessInfo(..) )
import Var		( TyVar, Id )
import CallConv		( CallConv, pprCallConv )
import Name		( Name, mkWiredInIdName )
import RdrName		( RdrName, mkRdrQual )
import OccName		( OccName, pprOccName, mkSrcVarOcc )
import TyCon		( TyCon, tyConArity )
import Type		( Type, mkForAllTys, mkFunTy, mkFunTys, mkTyVarTys,
			  mkTyConApp, typePrimRep,
			  splitFunTy_maybe, splitAlgTyConApp_maybe, splitTyConApp_maybe,
                          UsageAnn(..), mkUsgTy
			)
import Unique		( Unique, mkPrimOpIdUnique )
import BasicTypes	( Arity, Boxity(..) )
import CStrings		( CLabelString, pprCLabelString )
import PrelNames	( pREL_GHC, pREL_GHC_Name )
import Outputable
import Util		( zipWithEqual )
import GlaExts		( Int(..), Int#, (==#) )
\end{code}

%************************************************************************
%*									*
\subsection[PrimOp-datatype]{Datatype for @PrimOp@ (an enumeration)}
%*									*
%************************************************************************

These are in \tr{state-interface.verb} order.

\begin{code}

-- supplies: 
-- data PrimOp = ...
#include "primop-data-decl.hs-incl"
    | CCallOp CCall          -- and don't forget to add CCall
\end{code}

Used for the Ord instance

\begin{code}
primOpTag :: PrimOp -> Int
primOpTag op = IBOX( tagOf_PrimOp op )

-- supplies   
-- tagOf_PrimOp :: PrimOp -> FAST_INT
#include "primop-tag.hs-incl"
tagOf_PrimOp op = pprPanic# "tagOf_PrimOp: pattern-match" (ppr op)


instance Eq PrimOp where
    op1 == op2 = tagOf_PrimOp op1 _EQ_ tagOf_PrimOp op2

instance Ord PrimOp where
    op1 <  op2 =  tagOf_PrimOp op1 _LT_ tagOf_PrimOp op2
    op1 <= op2 =  tagOf_PrimOp op1 _LE_ tagOf_PrimOp op2
    op1 >= op2 =  tagOf_PrimOp op1 _GE_ tagOf_PrimOp op2
    op1 >  op2 =  tagOf_PrimOp op1 _GT_ tagOf_PrimOp op2
    op1 `compare` op2 | op1 < op2  = LT
		      | op1 == op2 = EQ
		      | otherwise  = GT

instance Outputable PrimOp where
    ppr op = pprPrimOp op

instance Show PrimOp where
    showsPrec p op = showsPrecSDoc p (pprPrimOp op)
\end{code}

An @Enum@-derived list would be better; meanwhile... (ToDo)
\begin{code}
allThePrimOps :: [PrimOp]
allThePrimOps =
#include "primop-list.hs-incl"
-- Doesn't include CCall, which is really a family of primops
\end{code}

%************************************************************************
%*									*
\subsection[PrimOp-info]{The essential info about each @PrimOp@}
%*									*
%************************************************************************

The @String@ in the @PrimOpInfos@ is the ``base name'' by which the user may
refer to the primitive operation.  The conventional \tr{#}-for-
unboxed ops is added on later.

The reason for the funny characters in the names is so we do not
interfere with the programmer's Haskell name spaces.

We use @PrimKinds@ for the ``type'' information, because they're
(slightly) more convenient to use than @TyCons@.
\begin{code}
data PrimOpInfo
  = Dyadic	OccName		-- string :: T -> T -> T
		Type
  | Monadic	OccName		-- string :: T -> T
		Type
  | Compare	OccName		-- string :: T -> T -> Bool
		Type

  | GenPrimOp   OccName  	-- string :: \/a1..an . T1 -> .. -> Tk -> T
		[TyVar] 
		[Type] 
		Type 

mkDyadic str  ty = Dyadic  (mkSrcVarOcc str) ty
mkMonadic str ty = Monadic (mkSrcVarOcc str) ty
mkCompare str ty = Compare (mkSrcVarOcc str) ty
mkGenPrimOp str tvs tys ty = GenPrimOp (mkSrcVarOcc str) tvs tys ty
\end{code}

%************************************************************************
%*									*
\subsubsection{Strictness}
%*									*
%************************************************************************

Not all primops are strict!

\begin{code}
primOpStrictness :: PrimOp -> Arity -> StrictnessInfo
	-- See Demand.StrictnessInfo for discussion of what the results
	-- The arity should be the arity of the primop; that's why
	-- this function isn't exported.
#include "primop-strictness.hs-incl"
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-comparison]{PrimOpInfo basic comparison ops}
%*									*
%************************************************************************

@primOpInfo@ gives all essential information (from which everything
else, notably a type, can be constructed) for each @PrimOp@.

\begin{code}
primOpInfo :: PrimOp -> PrimOpInfo
#include "primop-primop-info.hs-incl"
\end{code}

Here are a load of comments from the old primOp info:

A @Word#@ is an unsigned @Int#@.

@decodeFloat#@ is given w/ Integer-stuff (it's similar).

@decodeDouble#@ is given w/ Integer-stuff (it's similar).

Decoding of floating-point numbers is sorta Integer-related.  Encoding
is done with plain ccalls now (see PrelNumExtra.lhs).

A @Weak@ Pointer is created by the @mkWeak#@ primitive:

	mkWeak# :: k -> v -> f -> State# RealWorld 
			-> (# State# RealWorld, Weak# v #)

In practice, you'll use the higher-level

	data Weak v = Weak# v
	mkWeak :: k -> v -> IO () -> IO (Weak v)

The following operation dereferences a weak pointer.  The weak pointer
may have been finalized, so the operation returns a result code which
must be inspected before looking at the dereferenced value.

	deRefWeak# :: Weak# v -> State# RealWorld ->
			(# State# RealWorld, v, Int# #)

Only look at v if the Int# returned is /= 0 !!

The higher-level op is

	deRefWeak :: Weak v -> IO (Maybe v)

Weak pointers can be finalized early by using the finalize# operation:
	
	finalizeWeak# :: Weak# v -> State# RealWorld -> 
	   		   (# State# RealWorld, Int#, IO () #)

The Int# returned is either

	0 if the weak pointer has already been finalized, or it has no
	  finalizer (the third component is then invalid).

	1 if the weak pointer is still alive, with the finalizer returned
	  as the third component.

A {\em stable name/pointer} is an index into a table of stable name
entries.  Since the garbage collector is told about stable pointers,
it is safe to pass a stable pointer to external systems such as C
routines.

\begin{verbatim}
makeStablePtr#  :: a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
freeStablePtr   :: StablePtr# a -> State# RealWorld -> State# RealWorld
deRefStablePtr# :: StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
eqStablePtr#    :: StablePtr# a -> StablePtr# a -> Int#
\end{verbatim}

It may seem a bit surprising that @makeStablePtr#@ is a @IO@
operation since it doesn't (directly) involve IO operations.  The
reason is that if some optimisation pass decided to duplicate calls to
@makeStablePtr#@ and we only pass one of the stable pointers over, a
massive space leak can result.  Putting it into the IO monad
prevents this.  (Another reason for putting them in a monad is to
ensure correct sequencing wrt the side-effecting @freeStablePtr@
operation.)

An important property of stable pointers is that if you call
makeStablePtr# twice on the same object you get the same stable
pointer back.

Note that we can implement @freeStablePtr#@ using @_ccall_@ (and,
besides, it's not likely to be used from Haskell) so it's not a
primop.

Question: Why @RealWorld@ - won't any instance of @_ST@ do the job? [ADR]

Stable Names
~~~~~~~~~~~~

A stable name is like a stable pointer, but with three important differences:

	(a) You can't deRef one to get back to the original object.
	(b) You can convert one to an Int.
	(c) You don't need to 'freeStableName'

The existence of a stable name doesn't guarantee to keep the object it
points to alive (unlike a stable pointer), hence (a).

Invariants:
	
	(a) makeStableName always returns the same value for a given
	    object (same as stable pointers).

	(b) if two stable names are equal, it implies that the objects
	    from which they were created were the same.

	(c) stableNameToInt always returns the same Int for a given
	    stable name.


[Alastair Reid is to blame for this!]

These days, (Glasgow) Haskell seems to have a bit of everything from
other languages: strict operations, mutable variables, sequencing,
pointers, etc.  About the only thing left is LISP's ability to test
for pointer equality.  So, let's add it in!

\begin{verbatim}
reallyUnsafePtrEquality :: a -> a -> Int#
\end{verbatim}

which tests any two closures (of the same type) to see if they're the
same.  (Returns $0$ for @False@, $\neq 0$ for @True@ - to avoid
difficulties of trying to box up the result.)

NB This is {\em really unsafe\/} because even something as trivial as
a garbage collection might change the answer by removing indirections.
Still, no-one's forcing you to use it.  If you're worried about little
things like loss of referential transparency, you might like to wrap
it all up in a monad-like thing as John O'Donnell and John Hughes did
for non-determinism (1989 (Fraserburgh) Glasgow FP Workshop
Proceedings?)

I'm thinking of using it to speed up a critical equality test in some
graphics stuff in a context where the possibility of saying that
denotationally equal things aren't isn't a problem (as long as it
doesn't happen too often.)  ADR

To Will: Jim said this was already in, but I can't see it so I'm
adding it.  Up to you whether you add it.  (Note that this could have
been readily implemented using a @veryDangerousCCall@ before they were
removed...)


-- HWL: The first 4 Int# in all par... annotations denote:
--   name, granularity info, size of result, degree of parallelism
--      Same  structure as _seq_ i.e. returns Int#
-- KSW: v, the second arg in parAt# and parAtForNow#, is used only to determine
--   `the processor containing the expression v'; it is not evaluated

These primops are pretty wierd.

	dataToTag# :: a -> Int    (arg must be an evaluated data type)
	tagToEnum# :: Int -> a    (result type must be an enumerated type)

The constraints aren't currently checked by the front end, but the
code generator will fall over if they aren't satisfied.

\begin{code}
#ifdef DEBUG
primOpInfo op = pprPanic "primOpInfo:" (ppr op)
#endif
\end{code}

%************************************************************************
%*									*
\subsubsection[PrimOp-ool]{Which PrimOps are out-of-line}
%*									*
%************************************************************************

Some PrimOps need to be called out-of-line because they either need to
perform a heap check or they block.

\begin{code}
primOpOutOfLine (CCallOp c_call) = ccallMayGC c_call
#include "primop-out-of-line.hs-incl"
\end{code}


primOpOkForSpeculation
~~~~~~~~~~~~~~~~~~~~~~
Sometimes we may choose to execute a PrimOp even though it isn't
certain that its result will be required; ie execute them
``speculatively''.  The same thing as ``cheap eagerness.'' Usually
this is OK, because PrimOps are usually cheap, but it isn't OK for
(a)~expensive PrimOps and (b)~PrimOps which can fail.

PrimOps that have side effects also should not be executed speculatively.

Ok-for-speculation also means that it's ok *not* to execute the
primop. For example
	case op a b of
	  r -> 3
Here the result is not used, so we can discard the primop.  Anything
that has side effects mustn't be dicarded in this way, of course!

See also @primOpIsCheap@ (below).


\begin{code}
primOpOkForSpeculation :: PrimOp -> Bool
	-- See comments with CoreUtils.exprOkForSpeculation
primOpOkForSpeculation op 
  = primOpIsCheap op && not (primOpCanFail op)
\end{code}


primOpIsCheap
~~~~~~~~~~~~~
@primOpIsCheap@, as used in \tr{SimplUtils.lhs}.  For now (HACK
WARNING), we just borrow some other predicates for a
what-should-be-good-enough test.  "Cheap" means willing to call it more
than once.  Evaluation order is unaffected.

\begin{code}
primOpIsCheap :: PrimOp -> Bool
	-- See comments with CoreUtils.exprOkForSpeculation
primOpIsCheap op = not (primOpHasSideEffects op || primOpOutOfLine op)
\end{code}

primOpIsDupable
~~~~~~~~~~~~~~~
primOpIsDupable means that the use of the primop is small enough to
duplicate into different case branches.  See CoreUtils.exprIsDupable.

\begin{code}
primOpIsDupable :: PrimOp -> Bool
	-- See comments with CoreUtils.exprIsDupable
	-- We say it's dupable it isn't implemented by a C call with a wrapper
primOpIsDupable op = not (primOpNeedsWrapper op)
\end{code}


\begin{code}
primOpCanFail :: PrimOp -> Bool
#include "primop-can-fail.hs-incl"
\end{code}

And some primops have side-effects and so, for example, must not be
duplicated.

\begin{code}
primOpHasSideEffects :: PrimOp -> Bool
primOpHasSideEffects (CCallOp _) 	= True
#include "primop-has-side-effects.hs-incl"
\end{code}

Inline primitive operations that perform calls need wrappers to save
any live variables that are stored in caller-saves registers.

\begin{code}
primOpNeedsWrapper :: PrimOp -> Bool
primOpNeedsWrapper (CCallOp _) 		= True
#include "primop-needs-wrapper.hs-incl"
\end{code}

\begin{code}
primOpArity :: PrimOp -> Arity
primOpArity op 
  = case (primOpInfo op) of
      Monadic occ ty			  -> 1
      Dyadic occ ty			  -> 2
      Compare occ ty 			  -> 2
      GenPrimOp occ tyvars arg_tys res_ty -> length arg_tys
		
primOpType :: PrimOp -> Type  -- you may want to use primOpSig instead
primOpType op
  = case (primOpInfo op) of
      Dyadic occ ty ->	    dyadic_fun_ty ty
      Monadic occ ty ->	    monadic_fun_ty ty
      Compare occ ty ->	    compare_fun_ty ty

      GenPrimOp occ tyvars arg_tys res_ty -> 
	mkForAllTys tyvars (mkFunTys arg_tys res_ty)

mkPrimOpIdName :: PrimOp -> Id -> Name
	-- Make the name for the PrimOp's Id
	-- We have to pass in the Id itself because it's a WiredInId
	-- and hence recursive
mkPrimOpIdName op id
  = mkWiredInIdName key pREL_GHC occ_name id
  where
    occ_name = primOpOcc op
    key	     = mkPrimOpIdUnique (primOpTag op)


primOpRdrName :: PrimOp -> RdrName 
primOpRdrName op = mkRdrQual pREL_GHC_Name (primOpOcc op)

primOpOcc :: PrimOp -> OccName
primOpOcc op = case (primOpInfo op) of
			      Dyadic    occ _	  -> occ
			      Monadic   occ _	  -> occ
			      Compare   occ _	  -> occ
			      GenPrimOp occ _ _ _ -> occ

-- primOpSig is like primOpType but gives the result split apart:
-- (type variables, argument types, result type)
-- It also gives arity, strictness info

primOpSig :: PrimOp -> ([TyVar], [Type], Type, Arity, StrictnessInfo)
primOpSig op
  = (tyvars, arg_tys, res_ty, arity, primOpStrictness op arity)
  where
    arity = length arg_tys
    (tyvars, arg_tys, res_ty)
      = case (primOpInfo op) of
	  Monadic   occ ty -> ([],     [ty],    ty    )
	  Dyadic    occ ty -> ([],     [ty,ty], ty    )
	  Compare   occ ty -> ([],     [ty,ty], boolTy)
	  GenPrimOp occ tyvars arg_tys res_ty
                           -> (tyvars, arg_tys, res_ty)

-- primOpUsg is like primOpSig but the types it yields are the
-- appropriate sigma (i.e., usage-annotated) types,
-- as required by the UsageSP inference.

primOpUsg :: PrimOp -> ([TyVar],[Type],Type)
primOpUsg p@(CCallOp _) = mangle p [] mkM
#include "primop-usage.hs-incl"

-- Things with no Haskell pointers inside: in actuality, usages are
-- irrelevant here (hence it doesn't matter that some of these
-- apparently permit duplication; since such arguments are never 
-- ENTERed anyway, the usage annotation they get is entirely irrelevant
-- except insofar as it propagates to infect other values that *are*
-- pointed.


-- Helper bits & pieces for usage info.
                                    
mkZ          = mkUsgTy UsOnce  -- pointed argument used zero
mkO          = mkUsgTy UsOnce  -- pointed argument used once
mkM          = mkUsgTy UsMany  -- pointed argument used multiply
mkP          = mkUsgTy UsOnce  -- unpointed argument
mkR          = mkUsgTy UsMany  -- unpointed result

nomangle op
   = case primOpSig op of
        (tyvars, arg_tys, res_ty, _, _)
           -> (tyvars, map mkP arg_tys, mkR res_ty)

mangle op fs g  
   = case primOpSig op of
        (tyvars, arg_tys, res_ty, _, _)
           -> (tyvars, zipWithEqual "primOpUsg" ($) fs arg_tys, g res_ty)

inFun op f g ty 
   = case splitFunTy_maybe ty of
        Just (a,b) -> mkFunTy (f a) (g b)
        Nothing    -> pprPanic "primOpUsg:inFun" (ppr op <+> ppr ty)

inUB op fs ty
   = case splitTyConApp_maybe ty of
        Just (tc,tys) -> ASSERT( tc == tupleTyCon Unboxed (length fs) )
                         mkTupleTy Unboxed (length fs) (zipWithEqual "primOpUsg"
                                                                     ($) fs tys)
        Nothing       -> pprPanic "primOpUsg:inUB" (ppr op <+> ppr ty)
\end{code}

\begin{code}
data PrimOpResultInfo
  = ReturnsPrim	    PrimRep
  | ReturnsAlg	    TyCon

-- Some PrimOps need not return a manifest primitive or algebraic value
-- (i.e. they might return a polymorphic value).  These PrimOps *must*
-- be out of line, or the code generator won't work.

getPrimOpResultInfo :: PrimOp -> PrimOpResultInfo
getPrimOpResultInfo (CCallOp _)
  = ReturnsAlg unboxedPairTyCon
getPrimOpResultInfo op
  = case (primOpInfo op) of
      Dyadic  _ ty		 -> ReturnsPrim (typePrimRep ty)
      Monadic _ ty		 -> ReturnsPrim (typePrimRep ty)
      Compare _ ty		 -> ReturnsAlg boolTyCon
      GenPrimOp _ _ _ ty	 -> 
	let rep = typePrimRep ty in
	case rep of
	   PtrRep -> case splitAlgTyConApp_maybe ty of
			Nothing -> panic "getPrimOpResultInfo"
			Just (tc,_,_) -> ReturnsAlg tc
	   other -> ReturnsPrim other
\end{code}

The commutable ops are those for which we will try to move constants
to the right hand side for strength reduction.

\begin{code}
commutableOp :: PrimOp -> Bool
#include "primop-commutable.hs-incl"
\end{code}

Utils:
\begin{code}
mkPrimTyApp :: [TyVar] -> PrimRep -> ([TyVar], Type)
	-- CharRep       -->  ([],  Char#)
	-- StablePtrRep  -->  ([a], StablePtr# a)
mkPrimTyApp tvs kind
  = (forall_tvs, mkTyConApp tycon (mkTyVarTys forall_tvs))
  where
    tycon      = primRepTyCon kind
    forall_tvs = take (tyConArity tycon) tvs

dyadic_fun_ty  ty = mkFunTys [ty, ty] ty
monadic_fun_ty ty = mkFunTy  ty ty
compare_fun_ty ty = mkFunTys [ty, ty] boolTy
\end{code}

Output stuff:
\begin{code}
pprPrimOp  :: PrimOp -> SDoc

pprPrimOp (CCallOp c_call) = pprCCallOp c_call
pprPrimOp other_op
  = getPprStyle $ \ sty ->
    if ifaceStyle sty then	-- For interfaces Print it qualified with PrelGHC.
	ptext SLIT("PrelGHC.") <> pprOccName occ
    else
	pprOccName occ
  where
    occ = primOpOcc other_op
\end{code}


%************************************************************************
%*									*
\subsubsection{CCalls}
%*									*
%************************************************************************

A special ``trap-door'' to use in making calls direct to C functions:
\begin{code}
data CCall
  =  CCall	CCallTarget
		Bool		-- True <=> really a "casm"
		Bool		-- True <=> might invoke Haskell GC
		CallConv	-- calling convention to use.
  deriving( Eq )

data CCallTarget
  = StaticTarget  CLabelString  -- An "unboxed" ccall# to `fn'.
  | DynamicTarget Unique	-- First argument (an Addr#) is the function pointer
				--   (unique is used to generate a 'typedef' to cast
				--    the function pointer if compiling the ccall# down to
				--    .hc code - can't do this inline for tedious reasons.)

instance Eq CCallTarget where
  (StaticTarget l1) == (StaticTarget l2) = l1 == l2
  (DynamicTarget _) == (DynamicTarget _) = True	
	-- Ignore the arbitrary unique; this is important when comparing
	-- a dynamic ccall read from an interface file A.hi with the
	-- one constructed from A.hs, when deciding whether the interface
	-- has changed
  t1 == t2 = False

ccallMayGC :: CCall -> Bool
ccallMayGC (CCall _ _ may_gc _) = may_gc

ccallIsCasm :: CCall -> Bool
ccallIsCasm (CCall _ c_asm _ _) = c_asm

isDynamicTarget (DynamicTarget _) = True
isDynamicTarget (StaticTarget _)  = False

dynamicTarget :: CCallTarget
dynamicTarget = DynamicTarget (panic "Unique in DynamicTarget not yet set")
	-- The unique is really only to do with code generation, so it
	-- is only set in CoreToStg; before then it's just an error message

setCCallUnique :: CCall -> Unique -> CCall
setCCallUnique (CCall (DynamicTarget _) is_asm may_gc cconv) uniq
  = CCall (DynamicTarget uniq) is_asm may_gc cconv
setCCallUnique ccall uniq = ccall
\end{code}

\begin{code}
pprCCallOp (CCall fun is_casm may_gc cconv)
  = hcat [ ifPprDebug callconv
	 , text "__", ppr_dyn
         , text before , ppr_fun , after]
  where
        callconv = text "{-" <> pprCallConv cconv <> text "-}"

	before
	  | is_casm && may_gc = "casm_GC ``"
	  | is_casm	      = "casm ``"
	  | may_gc	      = "ccall_GC "
	  | otherwise	      = "ccall "

	after
	  | is_casm   = text "''"
	  | otherwise = empty
	  
	ppr_dyn = case fun of
		    DynamicTarget _ -> text "dyn_"
		    _	   	    -> empty

	ppr_fun = case fun of
		     DynamicTarget _ -> text "\"\""
		     StaticTarget fn -> pprCLabelString fn
\end{code}
