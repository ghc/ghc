{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PrimOp]{Primitive operations (machine-level)}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Builtin.PrimOps (
        PrimOp(..), PrimOpVecCat(..), allThePrimOps,
        primOpType, primOpSig, primOpResultType,
        primOpTag, maxPrimOpTag, primOpOcc,
        primOpWrapperId,
        pprPrimOp,

        tagToEnumKey,

        primOpOutOfLine, primOpCodeSize,
        primOpOkForSpeculation, primOpOkToDiscard,
        primOpIsWorkFree, primOpIsCheap, primOpFixity, primOpDocs,
        primOpIsDiv, primOpIsReallyInline,

        PrimOpEffect(..), primOpEffect,

        getPrimOpResultInfo,  isComparisonPrimOp, PrimOpResultInfo(..),

        PrimCall(..)
    ) where

import GHC.Prelude

import GHC.Builtin.Types.Prim
import GHC.Builtin.Types
import GHC.Builtin.Uniques (mkPrimOpIdUnique, mkPrimOpWrapperUnique )
import GHC.Builtin.Names ( gHC_PRIMOPWRAPPERS )

import GHC.Core.TyCon    ( isPrimTyCon, isUnboxedTupleTyCon, PrimRep(..) )
import GHC.Core.Type

import GHC.Cmm.Type

import GHC.Types.Demand
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Name
import GHC.Types.RepType ( tyConPrimRep )
import GHC.Types.Basic
import GHC.Types.Fixity  ( Fixity(..), FixityDirection(..) )
import GHC.Types.SrcLoc  ( wiredInSrcSpan )
import GHC.Types.ForeignCall ( CLabelString )
import GHC.Types.Unique  ( Unique )

import GHC.Unit.Types    ( Unit )

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Data.FastString

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
-}

data PrimOpInfo
  = Compare     OccName         -- string :: T -> T -> Int#
                Type
  | GenPrimOp   OccName         -- string :: \/a1..an . T1 -> .. -> Tk -> T
                [TyVarBinder]
                [Type]
                Type

mkCompare :: FastString -> Type -> PrimOpInfo
mkCompare str ty = Compare (mkVarOccFS str) ty

mkGenPrimOp :: FastString -> [TyVarBinder] -> [Type] -> Type -> PrimOpInfo
mkGenPrimOp str tvs tys ty = GenPrimOp (mkVarOccFS str) tvs tys ty

{-
************************************************************************
*                                                                      *
\subsubsection{Strictness}
*                                                                      *
************************************************************************

Not all primops are strict!
-}

primOpStrictness :: PrimOp -> Arity -> DmdSig
        -- See Demand.DmdSig for discussion of what the results
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
\subsubsection{Docs}
*                                                                      *
************************************************************************

See Note [GHC.Prim Docs]
-}

primOpDocs :: [(FastString, String)]
#include "primop-docs.hs-incl"

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


Note [Exceptions: asynchronous, synchronous, and unchecked]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are three very different sorts of things in GHC-Haskell that are
sometimes called exceptions:

* Haskell exceptions:

  These are ordinary exceptions that users can raise with the likes
  of 'throw' and handle with the likes of 'catch'.  They come in two
  very different flavors:

  * Asynchronous exceptions:
    * These can arise at nearly any time, and may have nothing to do
      with the code being executed.
    * The compiler itself mostly doesn't need to care about them.
    * Examples: a signal from another process, running out of heap or stack
    * Even pure code can receive asynchronous exceptions; in this
      case, executing the same code again may lead to different
      results, because the exception may not happen next time.
    * See rts/RaiseAsync.c for the gory details of how they work.

  * Synchronous exceptions:
    * These are produced by the code being executed, most commonly via
      a call to the `raise#` or `raiseIO#` primops.
    * At run-time, if a piece of pure code raises a synchronous
      exception, it will always raise the same synchronous exception
      if it is run again (and not interrupted by an asynchronous
      exception).
    * In particular, if an updatable thunk does some work and then
      raises a synchronous exception, it is safe to overwrite it with
      a thunk that /immediately/ raises the same exception.
    * Although we are careful not to discard synchronous exceptions, we
      are very liberal about re-ordering them with respect to most other
      operations.  See the paper "A semantics for imprecise exceptions"
      as well as Note [Precise exceptions and strictness analysis] in
      GHC.Types.Demand.

* Unchecked exceptions:

  * These are nasty failures like seg-faults or primitive Int# division
    by zero.  They differ from Haskell exceptions in that they are
    un-recoverable and typically bring execution to an immediate halt.
  * We generally treat unchecked exceptions as undefined behavior, on
    the assumption that the programmer never intends to crash the
    program in this way.  Thus we have no qualms about replacing a
    division-by-zero with a recoverable Haskell exception or
    discarding an indexArray# operation whose result is unused.


Note [Classifying primop effects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each primop has an associated 'PrimOpEffect', based on what that
primop can or cannot do at runtime.  This classification is

* Recorded in the 'effect' field in primops.txt.pp, and
* Exposed to the compiler via the 'primOpEffect' function in this module.

See Note [Transformations affected by primop effects] for how we make
use of this categorisation.

The meanings of the four constructors of 'PrimOpEffect' are as
follows, in decreasing order of permissiveness:

* ReadWriteEffect
    A primop is marked ReadWriteEffect if it can
    - read or write to the world (I/O), or
    - read or write to a mutable data structure (e.g. readMutVar#).

    Every such primop uses State# tokens for sequencing, with a type like:
      Inputs -> State# s -> (# State# s, Outputs #)
    The state token threading expresses ordering, but duplicating even
    a read-only effect would defeat this.  (See "duplication" under
    Note [Transformations affected by primop effects] for details.)

    Note that operations like `indexArray#` that read *immutable*
    data structures do not need such special sequencing-related care,
    and are therefore not marked ReadWriteEffect.

* ThrowsException
    A primop is marked ThrowsException if
    - it is not marked ReadWriteEffect, and
    - it may diverge or throw a synchronous Haskell exception
      even when used in a "correct" and well-specified way.

    See also Note [Exceptions: asynchronous, synchronous, and unchecked].
    Examples include raise#, raiseIO#, dataToTagLarge#, and seq#.

    Note that whether an exception is considered precise or imprecise
    does not matter for the purposes of the PrimOpEffect flag.

* CanFail
    A primop is marked CanFail if
    - it is not marked ReadWriteEffect or ThrowsException, and
    - it can trigger a (potentially-unchecked) exception when used incorrectly.

    See Note [Exceptions: asynchronous, synchronous, and unchecked].
    Examples include quotWord# and indexIntArray#, which can fail with
    division-by-zero and a segfault respectively.

    A correct use of a CanFail primop is usually surrounded by a test
    that screens out the bad cases such as a zero divisor or an
    out-of-bounds array index.  We must take care never to move a
    CanFail primop outside the scope of such a test.

* NoEffect
    A primop is marked NoEffect if it does not belong to any of the
    other three categories.  We can very aggressively shuffle these
    operations around without fear of changing a program's meaning.

    Perhaps surprisingly, this aggressive shuffling imposes another
    restriction: The tricky NoEffect primop uncheckedShiftLWord32# has
    an undefined result when the provided shift amount is not between
    0 and 31.  Thus, a call like `uncheckedShiftLWord32# x 95#` is
    obviously invalid.  But since uncheckedShiftLWord32# is marked
    NoEffect, we may float such an invalid call out of a dead branch
    and speculatively evaluate it.

    In particular, we cannot safely rewrite such an invalid call to a
    runtime error; we must emit code that produces a valid Word32#.
    (If we're lucky, Core Lint may complain that the result of such a
    rewrite violates the let-can-float invariant (#16742), but the
    rewrite is always wrong!)  See also Note [Guarding against silly shifts]
    in GHC.Core.Opt.ConstantFold.

    Marking uncheckedShiftLWord32# as CanFail instead of NoEffect
    would give us the freedom to rewrite such invalid calls to runtime
    errors, but would get in the way of optimization: When speculatively
    executing a bit-shift prevents the allocation of a thunk, that's a
    big win.


Note [Transformations affected by primop effects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The PrimOpEffect properties have the following effect on program
transformations.  The summary table is followed by details.  See also
Note [Classifying primop effects] for exactly what each column means.

                    NoEffect    CanFail    ThrowsException    ReadWriteEffect
Discard                YES        YES            NO                 NO
Defer (float in)       YES        YES           SAFE               SAFE
Speculate (float out)  YES        NO             NO                 NO
Duplicate              YES        YES            YES                NO

(SAFE means we could perform the transformation but do not.)

* Discarding:   case (a `op` b) of _ -> rhs  ===>   rhs
    You should not discard a ReadWriteEffect primop; e.g.
       case (writeIntArray# a i v s of (# _, _ #) -> True
    One could argue in favor of discarding this, since the returned
    State# token is not used.  But in practice unsafePerformIO can
    easily produce similar code, and programmers sometimes write this
    kind of stuff by hand (#9390).  So we (conservatively) never discard
    a ReadWriteEffect primop.

      Digression: We could try to track read-only effects separately
      from write effects to allow the former to be discarded.  But in
      fact we want a more general rewrite for read-only operations:
        case readOp# state# of (# newState#, _unused_result #) -> body
        ==> case state# of newState# -> body
      Such a rewrite is not yet implemented, but would have to be done
      in a different place anyway.

    Discarding a ThrowsException primop would also discard any exception
    it might have thrown.  For `raise#` or `raiseIO#` this would defeat
    the whole point of the primop, while for `dataToTagLarge#` or `seq#`
    this would make programs unexpectly lazier.

    However, it's fine to discard a CanFail primop.  For example
       case (indexIntArray# a i) of _ -> True
    We can discard indexIntArray# here; this came up in #5658.  Notice
    that CanFail primops like indexIntArray# can only trigger an
    exception when used incorrectly, i.e. a call that might not succeed
    is undefined behavior anyway.

* Deferring (float-in):
    See Note [Floating primops] in GHC.Core.Opt.FloatIn.

    In the absence of data dependencies (including state token threading),
    we reserve the right to re-order the following things arbitrarily:
      * Side effects
      * Imprecise exceptions
      * Divergent computations (infinite loops)
    This lets us safely float almost any primop *inwards*, but not
    inside a (multi-shot) lambda.  (See "Duplication" below.)

    However, the main reason to float-in a primop application would be
    to discard it (by floating it into some but not all branches of a
    case), so we actually only float-in NoEffect and CanFail operations.
    See also Note [Floating primops] in GHC.Core.Opt.FloatIn.

    (This automatically side-steps the question of precise exceptions, which
    mustn't be re-ordered arbitrarily but need at least ThrowsException.)

* Speculation (strict float-out):
    You must not float a CanFail primop *outwards* lest it escape the
    dynamic scope of a run-time validity test.  Example:
      case d ># 0# of
        True  -> case x /# d of r -> r +# 1
        False -> 0
    Here we must not float the case outwards to give
      case x/# d of r ->
      case d ># 0# of
        True  -> r +# 1
        False -> 0
    Otherwise, if this block is reached when d is zero, it will crash.
    Exactly the same reasoning applies to ThrowsException primops.

    Nor can you float out a ReadWriteEffect primop.  For example:
       if blah then case writeMutVar# v True s0 of (# s1 #) -> s1
               else s0
    Notice that s0 is mentioned in both branches of the 'if', but
    only one of these two will actually be consumed.  But if we
    float out to
      case writeMutVar# v True s0 of (# s1 #) ->
      if blah then s1 else s0
    the writeMutVar will be performed in both branches, which is
    utterly wrong.

    What about a read-only operation that cannot fail, like
    readMutVar#?  In principle we could safely float these out.  But
    there are not very many such operations and it's not clear if
    there are real-world programs that would benefit from this.

* Duplication:
    You cannot duplicate a ReadWriteEffect primop.  You might wonder
    how this can occur given the state token threading, but just look
    at Control.Monad.ST.Lazy.Imp.strictToLazy!  We get something like this
        p = case readMutVar# s v of
              (# s', r #) -> (State# s', r)
        s' = case p of (s', r) -> s'
        r  = case p of (s', r) -> r

    (All these bindings are boxed.)  If we inline p at its two call
    sites, we get a catastrophe: because the read is performed once when
    s' is demanded, and once when 'r' is demanded, which may be much
    later.  Utterly wrong.  #3207 is real example of this happening.
    Floating p into a multi-shot lambda would be wrong for the same reason.

    However, it's fine to duplicate a CanFail or ThrowsException primop.



Note [Implementation: how PrimOpEffect affects transformations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How do we ensure that floating/duplication/discarding are done right
in the simplifier?

Several predicates on primops test this flag:
  primOpOkToDiscard      <=> effect < ThrowsException
  primOpOkForSpeculation <=> effect == NoEffect && not (out_of_line)
  primOpIsCheap          <=> cheap  -- ...defaults to primOpOkForSpeculation
    [[But note that the raise# family and seq# are also considered cheap in
      GHC.Core.Utils.exprIsCheap by way of being work-free]]

  * The discarding mentioned above happens in
    GHC.Core.Opt.Simplify.Iteration, specifically in rebuildCase,
    where it is guarded by exprOkToDiscard, which in turn checks
    primOpOkToDiscard.

  * The "no-float-out" thing is achieved by ensuring that we never
    let-bind a saturated primop application unless it has NoEffect.
    The RHS of a let-binding (which can float in and out freely)
    satisfies exprOkForSpeculation; this is the let-can-float
    invariant.  And exprOkForSpeculation is false of a saturated
    primop application unless it has NoEffect.

  * So primops that aren't NoEffect will appear only as the
    scrutinees of cases, and that's why the FloatIn pass is capable
    of floating case bindings inwards.

  * Duplication via inlining and float-in of (lifted) let-binders is
    controlled via primOpIsWorkFree and primOpIsCheap, by making
    ReadWriteEffect things (among others) not-cheap!  (The test
    PrimOpEffect_Sanity will complain if any ReadWriteEffect primop
    is considered either work-free or cheap.)  Additionally, a
    case binding is only floated inwards if its scrutinee is ok-to-discard.
-}

primOpEffect :: PrimOp -> PrimOpEffect
#include "primop-effects.hs-incl"

data PrimOpEffect
  -- See Note [Classifying primop effects]
  = NoEffect
  | CanFail
  | ThrowsException
  | ReadWriteEffect
  deriving (Eq, Ord)

primOpOkForSpeculation :: PrimOp -> Bool
  -- See Note [Classifying primop effects]
  -- See comments with GHC.Core.Utils.exprOkForSpeculation
  -- primOpOkForSpeculation => primOpOkToDiscard
primOpOkForSpeculation op
  = primOpEffect op == NoEffect && not (primOpOutOfLine op)
    -- I think the "out of line" test is because out of line things can
    -- be expensive (eg sine, cosine), and so we may not want to speculate them

primOpOkToDiscard :: PrimOp -> Bool
primOpOkToDiscard op
  = primOpEffect op < ThrowsException

primOpIsWorkFree :: PrimOp -> Bool
#include "primop-is-work-free.hs-incl"

primOpIsCheap :: PrimOp -> Bool
-- See Note [Classifying primop effects]
#include "primop-is-cheap.hs-incl"
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
-- that (it's primOpIsWorkFree that does) so the problem doesn't occur
-- even if primOpIsCheap sometimes says 'True'.


-- | True of dyadic operators that can fail only if the second arg is zero!
--
-- This function probably belongs in an automagically generated file.. but it's
-- such a special case I thought I'd leave it here for now.
primOpIsDiv :: PrimOp -> Bool
primOpIsDiv op = case op of

  IntQuotOp       -> True
  Int8QuotOp      -> True
  Int16QuotOp     -> True
  Int32QuotOp     -> True
  Int64QuotOp     -> True

  IntRemOp        -> True
  Int8RemOp       -> True
  Int16RemOp      -> True
  Int32RemOp      -> True
  Int64RemOp      -> True

  IntQuotRemOp    -> True
  Int8QuotRemOp   -> True
  Int16QuotRemOp  -> True
  Int32QuotRemOp  -> True
  -- Int64QuotRemOp doesn't exist (yet)

  WordQuotOp      -> True
  Word8QuotOp     -> True
  Word16QuotOp    -> True
  Word32QuotOp    -> True
  Word64QuotOp    -> True

  WordRemOp       -> True
  Word8RemOp      -> True
  Word16RemOp     -> True
  Word32RemOp     -> True
  Word64RemOp     -> True

  WordQuotRemOp   -> True
  Word8QuotRemOp  -> True
  Word16QuotRemOp -> True
  Word32QuotRemOp -> True
  -- Word64QuotRemOp doesn't exist (yet)

  WordQuotRem2Op  -> True

  FloatDivOp      -> True
  DoubleDivOp     -> True
  _               -> False



{-
************************************************************************
*                                                                      *
               PrimOp code size
*                                                                      *
************************************************************************

primOpCodeSize
~~~~~~~~~~~~~~
Gives an indication of the code size of a primop, for the purposes of
calculating unfolding sizes; see GHC.Core.Unfold.sizeExpr.
-}

primOpCodeSize :: PrimOp -> Int
#include "primop-code-size.hs-incl"

primOpCodeSizeDefault :: Int
primOpCodeSizeDefault = 1
  -- GHC.Core.Unfold.primOpSize already takes into account primOpOutOfLine
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
    Compare _occ ty -> compare_fun_ty ty

    GenPrimOp _occ tyvars arg_tys res_ty ->
        mkForAllTys tyvars (mkVisFunTysMany arg_tys res_ty)

primOpResultType :: PrimOp -> Type
primOpResultType op
  = case primOpInfo op of
    Compare _occ _ty -> intPrimTy
    GenPrimOp _occ _tyvars _arg_tys res_ty -> res_ty

primOpOcc :: PrimOp -> OccName
primOpOcc op = case primOpInfo op of
               Compare   occ _     -> occ
               GenPrimOp occ _ _ _ -> occ

{- Note [Primop wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~

To support (limited) use of primops in GHCi genprimopcode generates the
GHC.PrimopWrappers module. This module contains a "primop wrapper"
binding for each primop. These are standard Haskell functions mirroring the
types of the primops they wrap. For instance, in the case of plusInt# we would
have:

    module GHC.PrimopWrappers where
    import GHC.Prim as P

    plusInt# :: Int# -> Int# -> Int#
    plusInt# a b = P.plusInt# a b

The Id for the wrapper of a primop can be found using
'GHC.Builtin.PrimOps.primOpWrapperId'. However, GHCi does not use this mechanism
to link primops; it rather does a rather hacky symbol lookup (see
GHC.ByteCode.Linker.primopToCLabel). TODO: Perhaps this should be changed?

Note that these wrappers aren't *quite* as expressive as their unwrapped
brethren, in that they may exhibit less representation polymorphism.
For instance, consider the case of mkWeakNoFinalizer#, which has type:

    mkWeakNoFinalizer# :: forall (r :: RuntimeRep) (k :: TYPE r) (v :: Type).
                          k -> v
                       -> State# RealWorld
                       -> (# State# RealWorld, Weak# v #)

Naively we could generate a wrapper of the form,


    mkWeakNoFinalizer# k v s = GHC.Prim.mkWeakNoFinalizer# k v s

However, this would require that 'k' bind the representation-polymorphic key,
which is disallowed by our representation polymorphism validity checks
(see Note [Representation polymorphism invariants] in GHC.Core).
Consequently, we give the wrapper the simpler, less polymorphic type

    mkWeakNoFinalizer# :: forall (k :: Type) (v :: Type).
                          k -> v
                       -> State# RealWorld
                       -> (# State# RealWorld, Weak# v #)

This simplification tends to be good enough for GHCi uses given that there are
few representation-polymorphic primops, and we do little simplification
on interpreted code anyways.

TODO: This behavior is actually wrong; a program becomes ill-typed upon
replacing a real primop occurrence with one of its wrapper due to the fact that
the former has an additional type binder. Hmmm....

Note [Eta expanding primops]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

STG requires that primop applications be saturated. This makes code generation
significantly simpler since otherwise we would need to define a calling
convention for curried applications that can accommodate representation
polymorphism.

To ensure saturation, CorePrep eta expands all primop applications as
described in Note [Eta expansion of hasNoBinding things in CorePrep] in
GHC.Core.Prep.

Historical Note:

For a short period around GHC 8.8 we rewrote unsaturated primop applications to
rather use the primop's wrapper (see Note [Primop wrappers] in
GHC.Builtin.PrimOps) instead of eta expansion. This was because at the time
CoreTidy would try to predict the CAFfyness of bindings that would be produced
by CorePrep for inclusion in interface files. Eta expanding during CorePrep
proved to be very difficult to predict, leading to nasty inconsistencies in
CAFfyness determinations (see #16846).

Thankfully, we now no longer try to predict CAFfyness but rather compute it on
GHC STG (see Note [SRTs] in GHC.Cmm.Info.Build) and inject it into the interface
file after code generation (see TODO: Refer to whatever falls out of #18096).
This is much simpler and avoids the potential for inconsistency, allowing us to
return to the somewhat simpler eta expansion approach for unsaturated primops.

See #18079.
-}

-- | Returns the 'Id' of the wrapper associated with the given 'PrimOp'.
-- See Note [Primop wrappers].
primOpWrapperId :: PrimOp -> Id
primOpWrapperId op = mkVanillaGlobalWithInfo name ty info
  where
    info = setCafInfo vanillaIdInfo NoCafRefs
    name = mkExternalName uniq gHC_PRIMOPWRAPPERS (primOpOcc op) wiredInSrcSpan
    uniq = mkPrimOpWrapperUnique (primOpTag op)
    ty   = primOpType op

isComparisonPrimOp :: PrimOp -> Bool
isComparisonPrimOp op = case primOpInfo op of
                          Compare {}   -> True
                          GenPrimOp {} -> False

-- primOpSig is like primOpType but gives the result split apart:
-- (type variables, argument types, result type)
-- It also gives arity, strictness info

primOpSig :: PrimOp -> ([TyVarBinder], [Type], Type, Arity, DmdSig)
primOpSig op
  = (tyvars, arg_tys, res_ty, arity, primOpStrictness op arity)
  where
    arity = length arg_tys
    (tyvars, arg_tys, res_ty)
      = case (primOpInfo op) of
        Compare   _occ ty                    -> ([],     [ty,ty], intPrimTy)
        GenPrimOp _occ tyvars arg_tys res_ty -> (tyvars, arg_tys, res_ty   )

data PrimOpResultInfo
  = ReturnsVoid
  | ReturnsPrim     PrimRep
  | ReturnsTuple

-- Some PrimOps need not return a manifest primitive or algebraic value
-- (i.e. they might return a polymorphic value).  These PrimOps *must*
-- be out of line, or the code generator won't work.

getPrimOpResultInfo :: PrimOp -> PrimOpResultInfo
getPrimOpResultInfo op
  = case (primOpInfo op) of
      Compare _ _                         -> ReturnsPrim IntRep
      GenPrimOp _ _ _ ty | isPrimTyCon tc -> case tyConPrimRep tc of
                                               [] -> ReturnsVoid
                                               [rep] -> ReturnsPrim rep
                                               _ -> pprPanic "getPrimOpResultInfo" (ppr op)
                         | isUnboxedTupleTyCon tc -> ReturnsTuple
                         | otherwise      -> pprPanic "getPrimOpResultInfo" (ppr op)
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

compare_fun_ty :: Type -> Type
compare_fun_ty ty = mkVisFunTysMany [ty, ty] intPrimTy

-- Output stuff:

pprPrimOp  :: IsLine doc => PrimOp -> doc
pprPrimOp other_op = pprOccName (primOpOcc other_op)
{-# SPECIALIZE pprPrimOp :: PrimOp -> SDoc #-}
{-# SPECIALIZE pprPrimOp :: PrimOp -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

{-
************************************************************************
*                                                                      *
\subsubsection[PrimCall]{User-imported primitive calls}
*                                                                      *
************************************************************************
-}

data PrimCall = PrimCall CLabelString Unit

instance Outputable PrimCall where
  ppr (PrimCall lbl pkgId)
        = text "__primcall" <+> ppr pkgId <+> ppr lbl

-- | Indicate if a primop is really inline: that is, it isn't out-of-line and it
-- isn't DataToTagOp which are two primops that evaluate their argument
-- hence induce thread/stack/heap changes.
primOpIsReallyInline :: PrimOp -> Bool
primOpIsReallyInline = \case
  DataToTagSmallOp -> False
  DataToTagLargeOp -> False
  p                -> not (primOpOutOfLine p)
