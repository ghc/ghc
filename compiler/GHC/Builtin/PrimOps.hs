{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PrimOp]{Primitive operations (machine-level)}
-}

{-# LANGUAGE CPP #-}

module GHC.Builtin.PrimOps (
        PrimOp(..), PrimOpVecCat(..), allThePrimOps,
        primOpType, primOpSig,
        primOpTag, maxPrimOpTag, primOpOcc,
        primOpWrapperId,

        tagToEnumKey,

        primOpOutOfLine, primOpCodeSize,
        primOpOkForSpeculation, primOpOkForSideEffects,
        primOpIsCheap, primOpFixity, primOpDocs,

        getPrimOpResultInfo,  isComparisonPrimOp, PrimOpResultInfo(..),

        PrimCall(..)
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Builtin.Types.Prim
import GHC.Builtin.Types

import GHC.Cmm.Type
import GHC.Types.Demand
import GHC.Types.Id      ( Id, mkVanillaGlobalWithInfo )
import GHC.Types.Id.Info ( vanillaIdInfo, setCafInfo, CafInfo(NoCafRefs) )
import GHC.Types.Name
import GHC.Builtin.Names ( gHC_PRIMOPWRAPPERS )
import GHC.Core.TyCon    ( TyCon, isPrimTyCon, PrimRep(..) )
import GHC.Core.Type
import GHC.Types.RepType ( typePrimRep1, tyConPrimRep1 )
import GHC.Types.Basic   ( Arity, Fixity(..), FixityDirection(..), Boxity(..),
                           SourceText(..) )
import GHC.Types.SrcLoc  ( wiredInSrcSpan )
import GHC.Types.ForeignCall ( CLabelString )
import GHC.Types.Unique  ( Unique, mkPrimOpIdUnique, mkPrimOpWrapperUnique )
import GHC.Unit          ( Unit )
import GHC.Utils.Outputable
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
\subsubsection{Docs}
*                                                                      *
************************************************************************

See Note [GHC.Prim Docs]
-}

primOpDocs :: [(String, String)]
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
-}

-- | A classification of primops by triggered side effects.
-- See Note [Classification by PrimOpEffect].
-- The total 'Ord' instance is significant (and hence the order of constructors
-- is). A "stronger" effect means less transformations are sound to apply to
-- them.
data PrimOpEffect
  = NoEffect
  | ThrowsImprecise
  | WriteEffect
  | ThrowsPrecise
  deriving (Eq, Ord)

-- | Can we discard a call to the primop, i.e. @case a `op` b of _ -> rhs@?
-- This is a question that i.e. the Simplifier asks before dropping the @case@.
-- See Note [Transformations affected by can_fail and has_side_effects].
isDiscardablePrimOpEffect :: PrimOpEffect -> Bool
isDiscardablePrimOpEffect eff = eff <= ThrowsImprecise

-- | Can we duplicate a call to the primop?
-- This is a question that i.e. the Simplifier asks when inlining definitions
-- involving primops with multiple syntactic occurrences.
-- See Note [Transformations affected by can_fail and has_side_effects].
isDupablePrimOpEffect :: PrimOpEffect -> Bool
-- isDupablePrimOpEffect eff = True -- #3207, see the Note
isDupablePrimOpEffect eff = eff <= ThrowsImprecise

-- | Can we perform other actions first before entering the primop?
-- This is the question that i.e. @FloatIn@ asks.
-- See Note [Transformations affected by can_fail and has_side_effects].
isDeferrablePrimOpEffect :: PrimOpEffect -> Bool
isDeferrablePrimOpEffect eff = eff <= WriteEffect

-- | Can we speculatively execute this primop, before performing other actions
-- that should come first according to evaluation strategy?
-- This is the question that i.e. @FloatOut@ (of a @case@) asks.
-- See Note [Transformations affected by can_fail and has_side_effects].
isSpeculatablePrimOpEffect :: PrimOpEffect -> Bool
isSpeculatablePrimOpEffect eff = eff <= NoEffect

{- Note [Classification by PrimOpEffect]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some primops have effects that are not captured entirely by their result value.
We distinguish these cases:

  * NoEffect: Pure primop, like `plusInt#`.
  * ThrowsImprecise: Possibly throws an *imprecise* exception, like
    division-by-zero or a segfault arising from an out of bounds array access.
    An imprecise exception is an outright error and transformations may play
    fast and loose by turning one imprecise exception into another, or bottom.
    See Note [Precise vs imprecise exceptions] in GHC.Types.Demand.
  * WriteEffect: A write side-effect, either writing to the RealWorld (IO) or
    to a mutable variable (`writeMutVar#`).
  * ThrowsPrecise: Possibly throws a *precise* exception. `raiseIO#` is the
    only primop that does that.
    See Note [Precise vs imprecise exceptions] in GHC.Types.Demand.

Why is this classification necessary? Because the kind of effect a primop
performs influences the transformations we are allowed to apply to it.
For example let binding a division-by-zero (which `ThrowsImprecise`) might
violate Core's let/app invariant (see Note [Core let/app invariant] in
GHC.Core) which is critical to the simplifier's ability to float without fear
of changing program meaning.

See Note [Transformations affected by PrimOpEffect].

Note [Transformations affected by PrimOpEffect]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The PrimOpEffect of a primop affects applicability of program transformations
in the following way.
Summary table is followed by details.

Tranform. | Example  | NoEffect | ThrowsImprecise | WriteEffect | ThrowsPrecise
----------+----------+----------+-----------------+-------------+--------------
Defer     | FloatIn  | YES      | YES             | YES         | NO
Discard   | DeadCode | YES      | YES             | NO          | NO
Dupe      | Inlining | YES      | YES             | NO          | NO
Speculate | FloatOut | YES      | NO              | NO          | NO

Note how there is a total order on effects in terms of which program
tranformations they inhibit. A "stronger" effect means less transformations
are sound to apply to it. NoEffect means any tranformation is sound;
ThrowsPrecise means none of the following is.
Whether or not a primop is cheap to evaluate is an orthogonal concern.

* Discarding.   case (a `op` b) of _ -> rhs  ===>   rhs
  You should not discard a WriteEffect primop; e.g.
    case (writeIntArray# a i v s of (# _, _ #) -> True
  Arguably you should be able to discard this, since the returned state token
  is not used, but that relies on NEVER inlining unsafePerformIO, and
  programmers sometimes write this kind of stuff by hand (#9390).  So we
  (conservatively) never discard such a primop.
  The situation with (stronger) ThrowsPrecise primops such as raiseIO# is even
  more restrictive: We may never discard a side effect throwing a precise
  exception.

  However, it's fine to discard a ThrowsImprecise primop.  For example
    case (indexIntArray# a i) of _ -> True
  We can discard indexIntArray#; it might throw an imprecise segmentation
  fault, but no precise exception, so we are OK with not observing it.
  See #5658 which was all about this.
  Similarly (a `/#` b) can be discarded.  It can seg-fault or cause a hardware
  exception, but not a precise Haskell exception.
  It's obviously fine to discard a NoEffect if its result aren't used.

* Duplication.  Example: The Simplifier inlines a (multi occ) binding.
  You cannot duplicate any effectful primop participating in state token
  threading. Not even what is actually a read-only effect like `readMutVar#`,
  see #3207.
  You might wonder how that can be problematic, but just look at
  Control.Monad.ST.Lazy.Imp.strictToLazy!  We get something like this
    p = case readMutVar# s v of
          (# s', r #) -> (S# s', r)
    s' = case p of (s', r) -> s'
    r  = case p of (s', r) -> r
  (All these bindings are boxed.)  If we inline p at its two call
  sites, we get a catastrophe: because the read is performed once when
  s' is demanded, and once when 'r' is demanded, which may be much
  later.  Utterly wrong.  #3207 is a real example of this happening.

  If it wasn't for working around state token threading
  (see https://gitlab.haskell.org/ghc/ghc/issues/3207#note_257470 for other
  approaches), then duplication wouldn't be an issue at all, soundness-wise.
  But for the time being, we mark primops that participate in state token
  threading such as `readMutVar#` (which has NoEffect at heart) and
  `readArray#` (ThrowsImprecise) as WriteEffect and say that we may not
  duplicate WriteEffect.

* Deferring (Float In).  Example, here inside a single-alt case:
    case (a `op` b) of (# s, x #) -> case e of p -> rhs
    ==>
    case e of p -> case (a `op` b) of (# s, x #) -> rhs
  Note that e might diverge (or throw an imprecise exception) and thus the
  side-effect we would observe by evaluating op might not happen if we defer it
  after e.

  That is a problem if op ThrowsPrecise: If e diverges, the user can catch
  the precise exception /before/ FloatIn, but not afterwards. Hence we may not
  float in a ThrowsPrecise primop like raiseIO#.

  But since e can never throw an imprecise exception, there is no
  non-imprecise-exceptional control flow in which it is possible to observe
  that a WriteEffect (and anything "weaker") didn't happen. So it's OK to
  defer (every weaker than or equal to) write effects. So you can float a
  WriteEffect *inwards*, but not inside a lambda (see Duplication below [SG: It
  isn't obvious to me how that explains why we shouldn't float inside a lambda
  at all]).

* Speculating (Float out).
  You must not float a ThrowsImprecise primop *outwards* lest you escape the
  dynamic scope of the test.  Example:
    case d ># 0# of
      True  -> case x /# d of r -> r +# 1
      False -> 0
  Here we must not float the division outwards to give
    case x/# d of r ->
    case d ># 0# of
      True  -> r +# 1
      False -> 0
  Now the potential division by zero will be performed in both branches.

  Similarly you can't float out a (stronger) WriteEffect primop.  For example:
    if blah then case writeMutVar# v True s0 of (# s1 #) -> s1
            else s0
  Notice that s0 is mentioned in both branches of the 'if', but only one of
  these two will actually be consumed.  But if we float out to
    case writeMutVar# v True s0 of (# s1 #) ->
      if blah then s1 else s0
  the writeMutVar will be performed in both branches, which is utterly wrong.

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
  -- See comments with GHC.Core.Utils.exprOkForSpeculation
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

@primOpIsCheap@, as used in GHC.Core.Opt.Simplify.Utils.  For now (HACK
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
    Dyadic  _occ ty -> dyadic_fun_ty ty
    Monadic _occ ty -> monadic_fun_ty ty
    Compare _occ ty -> compare_fun_ty ty

    GenPrimOp _occ tyvars arg_tys res_ty ->
        mkSpecForAllTys tyvars (mkVisFunTys arg_tys res_ty)

primOpOcc :: PrimOp -> OccName
primOpOcc op = case primOpInfo op of
               Dyadic    occ _     -> occ
               Monadic   occ _     -> occ
               Compare   occ _     -> occ
               GenPrimOp occ _ _ _ -> occ

{- Note [Primop wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~
Previously hasNoBinding would claim that PrimOpIds didn't have a curried
function definition. This caused quite some trouble as we would be forced to
eta expand unsaturated primop applications very late in the Core pipeline. Not
only would this produce unnecessary thunks, but it would also result in nasty
inconsistencies in CAFfy-ness determinations (see #16846 and
Note [CAFfyness inconsistencies due to late eta expansion] in GHC.Iface.Tidy).

However, it was quite unnecessary for hasNoBinding to claim this; primops in
fact *do* have curried definitions which are found in GHC.PrimopWrappers, which
is auto-generated by utils/genprimops from prelude/primops.txt.pp. These wrappers
are standard Haskell functions mirroring the types of the primops they wrap.
For instance, in the case of plusInt# we would have:

    module GHC.PrimopWrappers where
    import GHC.Prim as P
    plusInt# a b = P.plusInt# a b

We now take advantage of these curried definitions by letting hasNoBinding
claim that PrimOpIds have a curried definition and then rewrite any unsaturated
PrimOpId applications that we find during CoreToStg as applications of the
associated wrapper (e.g. `GHC.Prim.plusInt# 3#` will get rewritten to
`GHC.PrimopWrappers.plusInt# 3#`).` The Id of the wrapper for a primop can be
found using 'PrimOp.primOpWrapperId'.

Nota Bene: GHC.PrimopWrappers is needed *regardless*, because it's
used by GHCi, which does not implement primops direct at all.

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
dyadic_fun_ty  ty = mkVisFunTys [ty, ty] ty
monadic_fun_ty ty = mkVisFunTy  ty ty
compare_fun_ty ty = mkVisFunTys [ty, ty] intPrimTy

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

data PrimCall = PrimCall CLabelString Unit

instance Outputable PrimCall where
  ppr (PrimCall lbl pkgId)
        = text "__primcall" <+> ppr pkgId <+> ppr lbl
