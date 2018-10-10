{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PrimOp]{Primitive operations (machine-level)}
-}

{-# LANGUAGE CPP #-}

-- The default is a bit too low for the quite large primOpInfo definition
{-# OPTIONS_GHC -fmax-pmcheck-iterations=10000000 #-}

module PrimOp (
        PrimOp(..), PrimOpVecCat(..), allThePrimOps,
        primOpType, primOpSig,
        primOpTag, maxPrimOpTag, primOpOcc,

        tagToEnumKey,

        primOpOutOfLine, primOpCodeSize,
        primOpOkForSpeculation, primOpOkForSideEffects,
        primOpIsCheap, primOpFixity,

        getPrimOpResultInfo,  isComparisonPrimOp, PrimOpResultInfo(..),

        PrimCall(..)
    ) where

#include "HsVersions.h"

import GhcPrelude

import TysPrim
import TysWiredIn

import CmmType
import Demand
import OccName          ( OccName, pprOccName, mkVarOccFS )
import TyCon            ( TyCon, isPrimTyCon, PrimRep(..) )
import Type
import RepType          ( typePrimRep1, tyConPrimRep1 )
import BasicTypes       ( Arity, Fixity(..), FixityDirection(..), Boxity(..),
                          SourceText(..) )
import ForeignCall      ( CLabelString )
import Unique           ( Unique, mkPrimOpIdUnique )
import Outputable
import FastString
import Module           ( UnitId )

{-
************************************************************************
*                                                                      *
\subsection[PrimOp-datatype]{Datatype for @PrimOp@ (an enumeration)}
*                                                                      *
************************************************************************

These are in \tr{state-interface.verb} order.
-}

-- supplies:
-- data PrimOp = ...
#include "primop-data-decl.hs-incl"

-- supplies
-- primOpTag :: PrimOp -> Int
#include "primop-tag.hs-incl"
primOpTag _ = error "primOpTag: unknown primop"


instance Eq PrimOp where
    op1 == op2 = primOpTag op1 == primOpTag op2

instance Ord PrimOp where
    op1 <  op2 =  primOpTag op1 < primOpTag op2
    op1 <= op2 =  primOpTag op1 <= primOpTag op2
    op1 >= op2 =  primOpTag op1 >= primOpTag op2
    op1 >  op2 =  primOpTag op1 > primOpTag op2
    op1 `compare` op2 | op1 < op2  = LT
                      | op1 == op2 = EQ
                      | otherwise  = GT

instance Outputable PrimOp where
    ppr op = pprPrimOp op

data PrimOpVecCat = IntVec
                  | WordVec
                  | FloatVec

-- An @Enum@-derived list would be better; meanwhile... (ToDo)

allThePrimOps :: [PrimOp]
allThePrimOps =
#include "primop-list.hs-incl"

tagToEnumKey :: Unique
tagToEnumKey = mkPrimOpIdUnique (primOpTag TagToEnumOp)

{-
************************************************************************
*                                                                      *
\subsection[PrimOp-info]{The essential info about each @PrimOp@}
*                                                                      *
************************************************************************

The @String@ in the @PrimOpInfos@ is the ``base name'' by which the user may
refer to the primitive operation.  The conventional \tr{#}-for-
unboxed ops is added on later.

The reason for the funny characters in the names is so we do not
interfere with the programmer's Haskell name spaces.

We use @PrimKinds@ for the ``type'' information, because they're
(slightly) more convenient to use than @TyCons@.
-}

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

{-
************************************************************************
*                                                                      *
\subsubsection{Strictness}
*                                                                      *
************************************************************************

Not all primops are strict!
-}

primOpStrictness :: PrimOp -> Arity -> StrictSig
        -- See Demand.StrictnessInfo for discussion of what the results
        -- The arity should be the arity of the primop; that's why
        -- this function isn't exported.
#include "primop-strictness.hs-incl"

{-
************************************************************************
*                                                                      *
\subsubsection{Fixity}
*                                                                      *
************************************************************************
-}

primOpFixity :: PrimOp -> Maybe Fixity
#include "primop-fixity.hs-incl"

{-
************************************************************************
*                                                                      *
\subsubsection[PrimOp-comparison]{PrimOpInfo basic comparison ops}
*                                                                      *
************************************************************************

@primOpInfo@ gives all essential information (from which everything
else, notably a type, can be constructed) for each @PrimOp@.
-}

primOpInfo :: PrimOp -> PrimOpInfo
#include "primop-primop-info.hs-incl"
primOpInfo _ = error "primOpInfo: unknown primop"

{-
Here are a load of comments from the old primOp info:

A @Word#@ is an unsigned @Int#@.

@decodeFloat#@ is given w/ Integer-stuff (it's similar).

@decodeDouble#@ is given w/ Integer-stuff (it's similar).

Decoding of floating-point numbers is sorta Integer-related.  Encoding
is done with plain ccalls now (see PrelNumExtra.hs).

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


These primops are pretty weird.

        tagToEnum# :: Int -> a    (result type must be an enumerated type)

The constraints aren't currently checked by the front end, but the
code generator will fall over if they aren't satisfied.

************************************************************************
*                                                                      *
            Which PrimOps are out-of-line
*                                                                      *
************************************************************************

Some PrimOps need to be called out-of-line because they either need to
perform a heap check or they block.
-}

primOpOutOfLine :: PrimOp -> Bool
#include "primop-out-of-line.hs-incl"

{-
************************************************************************
*                                                                      *
            Failure and side effects
*                                                                      *
************************************************************************

Note [PrimOp can_fail and has_side_effects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Both can_fail and has_side_effects mean that the primop has
some effect that is not captured entirely by its result value.

----------  has_side_effects ---------------------
A primop "has_side_effects" if it has some *write* effect, visible
elsewhere
    - writing to the world (I/O)
    - writing to a mutable data structure (writeIORef)
    - throwing a synchronous Haskell exception

Often such primops have a type like
   State -> input -> (State, output)
so the state token guarantees ordering.  In general we rely *only* on
data dependencies of the state token to enforce write-effect ordering

 * NB1: if you inline unsafePerformIO, you may end up with
   side-effecting ops whose 'state' output is discarded.
   And programmers may do that by hand; see Trac #9390.
   That is why we (conservatively) do not discard write-effecting
   primops even if both their state and result is discarded.

 * NB2: We consider primops, such as raiseIO#, that can raise a
   (Haskell) synchronous exception to "have_side_effects" but not
   "can_fail".  We must be careful about not discarding such things;
   see the paper "A semantics for imprecise exceptions".

 * NB3: *Read* effects (like reading an IORef) don't count here,
   because it doesn't matter if we don't do them, or do them more than
   once.  *Sequencing* is maintained by the data dependency of the state
   token.

----------  can_fail ----------------------------
A primop "can_fail" if it can fail with an *unchecked* exception on
some elements of its input domain. Main examples:
   division (fails on zero demoninator)
   array indexing (fails if the index is out of bounds)

An "unchecked exception" is one that is an outright error, (not
turned into a Haskell exception,) such as seg-fault or
divide-by-zero error.  Such can_fail primops are ALWAYS surrounded
with a test that checks for the bad cases, but we need to be
very careful about code motion that might move it out of
the scope of the test.

Note [Transformations affected by can_fail and has_side_effects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The can_fail and has_side_effects properties have the following effect
on program transformations.  Summary table is followed by details.

            can_fail     has_side_effects
Discard        YES           NO
Float in       YES           YES
Float out      NO            NO
Duplicate      YES           NO

* Discarding.   case (a `op` b) of _ -> rhs  ===>   rhs
  You should not discard a has_side_effects primop; e.g.
     case (writeIntArray# a i v s of (# _, _ #) -> True
  Arguably you should be able to discard this, since the
  returned stat token is not used, but that relies on NEVER
  inlining unsafePerformIO, and programmers sometimes write
  this kind of stuff by hand (Trac #9390).  So we (conservatively)
  never discard a has_side_effects primop.

  However, it's fine to discard a can_fail primop.  For example
     case (indexIntArray# a i) of _ -> True
  We can discard indexIntArray#; it has can_fail, but not
  has_side_effects; see Trac #5658 which was all about this.
  Notice that indexIntArray# is (in a more general handling of
  effects) read effect, but we don't care about that here, and
  treat read effects as *not* has_side_effects.

  Similarly (a `/#` b) can be discarded.  It can seg-fault or
  cause a hardware exception, but not a synchronous Haskell
  exception.



  Synchronous Haskell exceptions, e.g. from raiseIO#, are treated
  as has_side_effects and hence are not discarded.

* Float in.  You can float a can_fail or has_side_effects primop
  *inwards*, but not inside a lambda (see Duplication below).

* Float out.  You must not float a can_fail primop *outwards* lest
  you escape the dynamic scope of the test.  Example:
      case d ># 0# of
        True  -> case x /# d of r -> r +# 1
        False -> 0
  Here we must not float the case outwards to give
      case x/# d of r ->
      case d ># 0# of
        True  -> r +# 1
        False -> 0

  Nor can you float out a has_side_effects primop.  For example:
       if blah then case writeMutVar# v True s0 of (# s1 #) -> s1
               else s0
  Notice that s0 is mentioned in both branches of the 'if', but
  only one of these two will actually be consumed.  But if we
  float out to
      case writeMutVar# v True s0 of (# s1 #) ->
      if blah then s1 else s0
  the writeMutVar will be performed in both branches, which is
  utterly wrong.

* Duplication.  You cannot duplicate a has_side_effect primop.  You
  might wonder how this can occur given the state token threading, but
  just look at Control.Monad.ST.Lazy.Imp.strictToLazy!  We get
  something like this
        p = case readMutVar# s v of
              (# s', r #) -> (S# s', r)
        s' = case p of (s', r) -> s'
        r  = case p of (s', r) -> r

  (All these bindings are boxed.)  If we inline p at its two call
  sites, we get a catastrophe: because the read is performed once when
  s' is demanded, and once when 'r' is demanded, which may be much
  later.  Utterly wrong.  Trac #3207 is real example of this happening.

  However, it's fine to duplicate a can_fail primop.  That is really
  the only difference between can_fail and has_side_effects.

Note [Implementation: how can_fail/has_side_effects affect transformations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How do we ensure that that floating/duplication/discarding are done right
in the simplifier?

Two main predicates on primpops test these flags:
  primOpOkForSideEffects <=> not has_side_effects
  primOpOkForSpeculation <=> not (has_side_effects || can_fail)

  * The "no-float-out" thing is achieved by ensuring that we never
    let-bind a can_fail or has_side_effects primop.  The RHS of a
    let-binding (which can float in and out freely) satisfies
    exprOkForSpeculation; this is the let/app invariant.  And
    exprOkForSpeculation is false of can_fail and has_side_effects.

  * So can_fail and has_side_effects primops will appear only as the
    scrutinees of cases, and that's why the FloatIn pass is capable
    of floating case bindings inwards.

  * The no-duplicate thing is done via primOpIsCheap, by making
    has_side_effects things (very very very) not-cheap!
-}

primOpHasSideEffects :: PrimOp -> Bool
#include "primop-has-side-effects.hs-incl"

primOpCanFail :: PrimOp -> Bool
#include "primop-can-fail.hs-incl"

primOpOkForSpeculation :: PrimOp -> Bool
  -- See Note [PrimOp can_fail and has_side_effects]
  -- See comments with CoreUtils.exprOkForSpeculation
  -- primOpOkForSpeculation => primOpOkForSideEffects
primOpOkForSpeculation op
  =  primOpOkForSideEffects op
  && not (primOpOutOfLine op || primOpCanFail op)
    -- I think the "out of line" test is because out of line things can
    -- be expensive (eg sine, cosine), and so we may not want to speculate them

primOpOkForSideEffects :: PrimOp -> Bool
primOpOkForSideEffects op
  = not (primOpHasSideEffects op)

{-
Note [primOpIsCheap]
~~~~~~~~~~~~~~~~~~~~
@primOpIsCheap@, as used in \tr{SimplUtils.hs}.  For now (HACK
WARNING), we just borrow some other predicates for a
what-should-be-good-enough test.  "Cheap" means willing to call it more
than once, and/or push it inside a lambda.  The latter could change the
behaviour of 'seq' for primops that can fail, so we don't treat them as cheap.
-}

primOpIsCheap :: PrimOp -> Bool
-- See Note [PrimOp can_fail and has_side_effects]
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

{-
************************************************************************
*                                                                      *
               PrimOp code size
*                                                                      *
************************************************************************

primOpCodeSize
~~~~~~~~~~~~~~
Gives an indication of the code size of a primop, for the purposes of
calculating unfolding sizes; see CoreUnfold.sizeExpr.
-}

primOpCodeSize :: PrimOp -> Int
#include "primop-code-size.hs-incl"

primOpCodeSizeDefault :: Int
primOpCodeSizeDefault = 1
  -- CoreUnfold.primOpSize already takes into account primOpOutOfLine
  -- and adds some further costs for the args in that case.

primOpCodeSizeForeignCall :: Int
primOpCodeSizeForeignCall = 4

{-
************************************************************************
*                                                                      *
               PrimOp types
*                                                                      *
************************************************************************
-}

primOpType :: PrimOp -> Type  -- you may want to use primOpSig instead
primOpType op
  = case primOpInfo op of
    Dyadic  _occ ty -> dyadic_fun_ty ty
    Monadic _occ ty -> monadic_fun_ty ty
    Compare _occ ty -> compare_fun_ty ty

    GenPrimOp _occ tyvars arg_tys res_ty ->
        mkSpecForAllTys tyvars (mkFunTys arg_tys res_ty)

primOpOcc :: PrimOp -> OccName
primOpOcc op = case primOpInfo op of
               Dyadic    occ _     -> occ
               Monadic   occ _     -> occ
               Compare   occ _     -> occ
               GenPrimOp occ _ _ _ -> occ

isComparisonPrimOp :: PrimOp -> Bool
isComparisonPrimOp op = case primOpInfo op of
                          Compare {} -> True
                          _          -> False

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

data PrimOpResultInfo
  = ReturnsPrim     PrimRep
  | ReturnsAlg      TyCon

-- Some PrimOps need not return a manifest primitive or algebraic value
-- (i.e. they might return a polymorphic value).  These PrimOps *must*
-- be out of line, or the code generator won't work.

getPrimOpResultInfo :: PrimOp -> PrimOpResultInfo
getPrimOpResultInfo op
  = case (primOpInfo op) of
      Dyadic  _ ty                        -> ReturnsPrim (typePrimRep1 ty)
      Monadic _ ty                        -> ReturnsPrim (typePrimRep1 ty)
      Compare _ _                         -> ReturnsPrim (tyConPrimRep1 intPrimTyCon)
      GenPrimOp _ _ _ ty | isPrimTyCon tc -> ReturnsPrim (tyConPrimRep1 tc)
                         | otherwise      -> ReturnsAlg tc
                         where
                           tc = tyConAppTyCon ty
                        -- All primops return a tycon-app result
                        -- The tycon can be an unboxed tuple or sum, though,
                        -- which gives rise to a ReturnAlg

{-
We do not currently make use of whether primops are commutable.

We used to try to move constants to the right hand side for strength
reduction.
-}

{-
commutableOp :: PrimOp -> Bool
#include "primop-commutable.hs-incl"
-}

-- Utils:

dyadic_fun_ty, monadic_fun_ty, compare_fun_ty :: Type -> Type
dyadic_fun_ty  ty = mkFunTys [ty, ty] ty
monadic_fun_ty ty = mkFunTy  ty ty
compare_fun_ty ty = mkFunTys [ty, ty] intPrimTy

-- Output stuff:

pprPrimOp  :: PrimOp -> SDoc
pprPrimOp other_op = pprOccName (primOpOcc other_op)

{-
************************************************************************
*                                                                      *
\subsubsection[PrimCall]{User-imported primitive calls}
*                                                                      *
************************************************************************
-}

data PrimCall = PrimCall CLabelString UnitId

instance Outputable PrimCall where
  ppr (PrimCall lbl pkgId)
        = text "__primcall" <+> ppr pkgId <+> ppr lbl
