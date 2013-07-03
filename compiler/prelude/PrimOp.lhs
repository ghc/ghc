%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrimOp]{Primitive operations (machine-level)}

\begin{code}
module PrimOp (
        PrimOp(..), allThePrimOps,
        primOpType, primOpSig,
        primOpTag, maxPrimOpTag, primOpOcc,

        tagToEnumKey,

        primOpOutOfLine, primOpCodeSize,
        primOpOkForSpeculation, primOpOkForSideEffects,
        primOpIsCheap, primOpFixity,

        getPrimOpResultInfo,  PrimOpResultInfo(..),

        PrimCall(..)
    ) where

#include "HsVersions.h"

import TysPrim
import TysWiredIn

import Demand
import Var              ( TyVar )
import OccName          ( OccName, pprOccName, mkVarOccFS )
import TyCon            ( TyCon, isPrimTyCon, tyConPrimRep, PrimRep(..) )
import Type             ( Type, mkForAllTys, mkFunTy, mkFunTys, tyConAppTyCon,
                          typePrimRep )
import BasicTypes       ( Arity, Fixity(..), FixityDirection(..), TupleSort(..) )
import ForeignCall      ( CLabelString )
import Unique           ( Unique, mkPrimOpIdUnique )
import Outputable
import FastTypes
import FastString
import Module           ( PackageId )
\end{code}

%************************************************************************
%*                                                                      *
\subsection[PrimOp-datatype]{Datatype for @PrimOp@ (an enumeration)}
%*                                                                      *
%************************************************************************

These are in \tr{state-interface.verb} order.

\begin{code}

-- supplies:
-- data PrimOp = ...
#include "primop-data-decl.hs-incl"
\end{code}

Used for the Ord instance

\begin{code}
primOpTag :: PrimOp -> Int
primOpTag op = iBox (tagOf_PrimOp op)

-- supplies
-- tagOf_PrimOp :: PrimOp -> FastInt
#include "primop-tag.hs-incl"


instance Eq PrimOp where
    op1 == op2 = tagOf_PrimOp op1 ==# tagOf_PrimOp op2

instance Ord PrimOp where
    op1 <  op2 =  tagOf_PrimOp op1 <# tagOf_PrimOp op2
    op1 <= op2 =  tagOf_PrimOp op1 <=# tagOf_PrimOp op2
    op1 >= op2 =  tagOf_PrimOp op1 >=# tagOf_PrimOp op2
    op1 >  op2 =  tagOf_PrimOp op1 ># tagOf_PrimOp op2
    op1 `compare` op2 | op1 < op2  = LT
                      | op1 == op2 = EQ
                      | otherwise  = GT

instance Outputable PrimOp where
    ppr op = pprPrimOp op
\end{code}

An @Enum@-derived list would be better; meanwhile... (ToDo)

\begin{code}
allThePrimOps :: [PrimOp]
allThePrimOps =
#include "primop-list.hs-incl"
\end{code}

\begin{code}
tagToEnumKey :: Unique
tagToEnumKey = mkPrimOpIdUnique (primOpTag TagToEnumOp)
\end{code}



%************************************************************************
%*                                                                      *
\subsection[PrimOp-info]{The essential info about each @PrimOp@}
%*                                                                      *
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
  = Dyadic      OccName         -- string :: T -> T -> T
                Type
  | Monadic     OccName         -- string :: T -> T
                Type
  | Compare     OccName         -- string :: T -> T -> Int#
                Type
  | GenPrimOp   OccName         -- string :: \/a1..an . T1 -> .. -> Tk -> T
                [TyVar]
                [Type]
                Type

mkDyadic, mkMonadic, mkCompare :: FastString -> Type -> PrimOpInfo
mkDyadic str  ty = Dyadic  (mkVarOccFS str) ty
mkMonadic str ty = Monadic (mkVarOccFS str) ty
mkCompare str ty = Compare (mkVarOccFS str) ty

mkGenPrimOp :: FastString -> [TyVar] -> [Type] -> Type -> PrimOpInfo
mkGenPrimOp str tvs tys ty = GenPrimOp (mkVarOccFS str) tvs tys ty
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection{Strictness}
%*                                                                      *
%************************************************************************

Not all primops are strict!

\begin{code}
primOpStrictness :: PrimOp -> Arity -> StrictSig
        -- See Demand.StrictnessInfo for discussion of what the results
        -- The arity should be the arity of the primop; that's why
        -- this function isn't exported.
#include "primop-strictness.hs-incl"
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection{Fixity}
%*                                                                      *
%************************************************************************

\begin{code}
primOpFixity :: PrimOp -> Maybe Fixity
#include "primop-fixity.hs-incl"
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection[PrimOp-comparison]{PrimOpInfo basic comparison ops}
%*                                                                      *
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

%************************************************************************
%*                                                                      *
            Which PrimOps are out-of-line
%*                                                                      *
%************************************************************************

Some PrimOps need to be called out-of-line because they either need to
perform a heap check or they block.


\begin{code}
primOpOutOfLine :: PrimOp -> Bool
#include "primop-out-of-line.hs-incl"
\end{code}


%************************************************************************
%*                                                                      *
            Failure and side effects
%*                                                                      *
%************************************************************************

Note [PrimOp can_fail and has_side_effects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Both can_fail and has_side_effects mean that the primop has
some effect that is not captured entirely by its result value.

   ----------  has_side_effects ---------------------
   Has some imperative side effect, perhaps on the world (I/O),
   or perhaps on some mutable data structure (writeIORef).
   Generally speaking all such primops have a type like
      State -> input -> (State, output)
   so the state token guarantees ordering, and also ensures
   that the primop is executed even if 'output' is discarded.

   ----------  can_fail ----------------------------
   Can fail with a seg-fault or divide-by-zero error on some elements
   of its input domain.  Main examples:
      division (fails on zero demoninator
      array indexing (fails if the index is out of bounds)
   However (ASSUMPTION), these can_fail primops are ALWAYS surrounded
   with a test that checks for the bad cases.  

Consequences:

* You can discard a can_fail primop, or float it _inwards_.
  But you cannot float it _outwards_, lest you escape the
  dynamic scope of the test.  Example:
      case d ># 0# of
        True  -> case x /# d of r -> r +# 1
        False -> 0
  Here we must not float the case outwards to give
      case x/# d of r ->
      case d ># 0# of
        True  -> r +# 1
        False -> 0

* I believe that exactly the same rules apply to a has_side_effects
  primop; you can discard it (remember, the state token will keep
  it alive if necessary), or float it in, but not float it out.

  Example of the latter
       if blah then let! s1 = writeMutVar s0 v True in s1
               else s0
  Notice that s0 is mentioned in both branches of the 'if', but 
  only one of these two will actually be consumed.  But if we
  float out to
      let! s1 = writeMutVar s0 v True
      in if blah then s1 else s0
  the writeMutVar will be performed in both branches, which is
  utterly wrong.

* You cannot duplicate a has_side_effect primop.  You might wonder
  how this can occur given the state token threading, but just look
  at Control.Monad.ST.Lazy.Imp.strictToLazy!  We get something like
  this
        p = case readMutVar# s v of
              (# s', r #) -> (S# s', r)
        s' = case p of (s', r) -> s'
        r  = case p of (s', r) -> r

  (All these bindings are boxed.)  If we inline p at its two call
  sites, we get a catastrophe: because the read is performed once when
  s' is demanded, and once when 'r' is demanded, which may be much 
  later.  Utterly wrong.  Trac #3207 is real example of this happening.

  However, it's fine to duplicate a can_fail primop.  That is
  the difference between can_fail and has_side_effects.

            can_fail     has_side_effects
Discard        YES           YES
Float in       YES           YES
Float out      NO            NO
Duplicate      YES           NO

How do we achieve these effects?

Note [primOpOkForSpeculation]
  * The "no-float-out" thing is achieved by ensuring that we never
    let-bind a can_fail or has_side_effects primop.  The RHS of a
    let-binding (which can float in and out freely) satisfies
    exprOkForSpeculation.  And exprOkForSpeculation is false of
    can_fail and no_side_effect.

  * So can_fail and no_side_effect primops will appear only as the
    scrutinees of cases, and that's why the FloatIn pass is capable
    of floating case bindings inwards.

  * The no-duplicate thing is done via primOpIsCheap, by making
    has_side_effects things (very very very) not-cheap!


\begin{code}
primOpHasSideEffects :: PrimOp -> Bool
#include "primop-has-side-effects.hs-incl"

primOpCanFail :: PrimOp -> Bool
#include "primop-can-fail.hs-incl"

primOpOkForSpeculation :: PrimOp -> Bool
  -- See Note [primOpOkForSpeculation and primOpOkForFloatOut]
  -- See comments with CoreUtils.exprOkForSpeculation
primOpOkForSpeculation op
  = not (primOpHasSideEffects op || primOpOutOfLine op || primOpCanFail op)

primOpOkForSideEffects :: PrimOp -> Bool
primOpOkForSideEffects op
  = not (primOpHasSideEffects op)
\end{code}


Note [primOpIsCheap]
~~~~~~~~~~~~~~~~~~~~
@primOpIsCheap@, as used in \tr{SimplUtils.lhs}.  For now (HACK
WARNING), we just borrow some other predicates for a
what-should-be-good-enough test.  "Cheap" means willing to call it more
than once, and/or push it inside a lambda.  The latter could change the
behaviour of 'seq' for primops that can fail, so we don't treat them as cheap.

\begin{code}
primOpIsCheap :: PrimOp -> Bool
primOpIsCheap op = primOpOkForSpeculation op
-- In March 2001, we changed this to
--      primOpIsCheap op = False
-- thereby making *no* primops seem cheap.  But this killed eta
-- expansion on case (x ==# y) of True -> \s -> ...
-- which is bad.  In particular a loop like
--      doLoop n = loop 0
--     where
--         loop i | i == n    = return ()
--                | otherwise = bar i >> loop (i+1)
-- allocated a closure every time round because it doesn't eta expand.
--
-- The problem that originally gave rise to the change was
--      let x = a +# b *# c in x +# x
-- were we don't want to inline x. But primopIsCheap doesn't control
-- that (it's exprIsDupable that does) so the problem doesn't occur
-- even if primOpIsCheap sometimes says 'True'.
\end{code}


%************************************************************************
%*                                                                      *
               PrimOp code size
%*                                                                      *
%************************************************************************

primOpCodeSize
~~~~~~~~~~~~~~
Gives an indication of the code size of a primop, for the purposes of
calculating unfolding sizes; see CoreUnfold.sizeExpr.

\begin{code}
primOpCodeSize :: PrimOp -> Int
#include "primop-code-size.hs-incl"

primOpCodeSizeDefault :: Int
primOpCodeSizeDefault = 1
  -- CoreUnfold.primOpSize already takes into account primOpOutOfLine
  -- and adds some further costs for the args in that case.

primOpCodeSizeForeignCall :: Int
primOpCodeSizeForeignCall = 4
\end{code}


%************************************************************************
%*                                                                      *
               PrimOp types
%*                                                                      *
%************************************************************************

\begin{code}
primOpType :: PrimOp -> Type  -- you may want to use primOpSig instead
primOpType op
  = case primOpInfo op of
    Dyadic  _occ ty -> dyadic_fun_ty ty
    Monadic _occ ty -> monadic_fun_ty ty
    Compare _occ ty -> compare_fun_ty ty

    GenPrimOp _occ tyvars arg_tys res_ty ->
        mkForAllTys tyvars (mkFunTys arg_tys res_ty)

primOpOcc :: PrimOp -> OccName
primOpOcc op = case primOpInfo op of
               Dyadic    occ _     -> occ
               Monadic   occ _     -> occ
               Compare   occ _     -> occ
               GenPrimOp occ _ _ _ -> occ

-- primOpSig is like primOpType but gives the result split apart:
-- (type variables, argument types, result type)
-- It also gives arity, strictness info

primOpSig :: PrimOp -> ([TyVar], [Type], Type, Arity, StrictSig)
primOpSig op
  = (tyvars, arg_tys, res_ty, arity, primOpStrictness op arity)
  where
    arity = length arg_tys
    (tyvars, arg_tys, res_ty)
      = case (primOpInfo op) of
        Monadic   _occ ty                    -> ([],     [ty],    ty       )
        Dyadic    _occ ty                    -> ([],     [ty,ty], ty       )
        Compare   _occ ty                    -> ([],     [ty,ty], intPrimTy)
        GenPrimOp _occ tyvars arg_tys res_ty -> (tyvars, arg_tys, res_ty   )
\end{code}

\begin{code}
data PrimOpResultInfo
  = ReturnsPrim     PrimRep
  | ReturnsAlg      TyCon

-- Some PrimOps need not return a manifest primitive or algebraic value
-- (i.e. they might return a polymorphic value).  These PrimOps *must*
-- be out of line, or the code generator won't work.

getPrimOpResultInfo :: PrimOp -> PrimOpResultInfo
getPrimOpResultInfo op
  = case (primOpInfo op) of
      Dyadic  _ ty                        -> ReturnsPrim (typePrimRep ty)
      Monadic _ ty                        -> ReturnsPrim (typePrimRep ty)
      Compare _ _                         -> ReturnsPrim (tyConPrimRep intPrimTyCon)
      GenPrimOp _ _ _ ty | isPrimTyCon tc -> ReturnsPrim (tyConPrimRep tc)
                         | otherwise      -> ReturnsAlg tc
                         where
                           tc = tyConAppTyCon ty
                        -- All primops return a tycon-app result
                        -- The tycon can be an unboxed tuple, though, which
                        -- gives rise to a ReturnAlg
\end{code}

We do not currently make use of whether primops are commutable.

We used to try to move constants to the right hand side for strength
reduction.

\begin{code}
{-
commutableOp :: PrimOp -> Bool
#include "primop-commutable.hs-incl"
-}
\end{code}

Utils:
\begin{code}
dyadic_fun_ty, monadic_fun_ty, compare_fun_ty :: Type -> Type
dyadic_fun_ty  ty = mkFunTys [ty, ty] ty
monadic_fun_ty ty = mkFunTy  ty ty
compare_fun_ty ty = mkFunTys [ty, ty] intPrimTy
\end{code}

Output stuff:
\begin{code}
pprPrimOp  :: PrimOp -> SDoc
pprPrimOp other_op = pprOccName (primOpOcc other_op)
\end{code}


%************************************************************************
%*                                                                      *
\subsubsection[PrimCall]{User-imported primitive calls}
%*                                                                      *
%************************************************************************

\begin{code}
data PrimCall = PrimCall CLabelString PackageId

instance Outputable PrimCall where
  ppr (PrimCall lbl pkgId)
        = text "__primcall" <+> ppr pkgId <+> ppr lbl

\end{code}
