-- We don't to strictness analysis on this file to avoid turning loopy unsafe
-- equality terms below to actual loops. Details in (U5) of
-- Note [Implementing unsafeCoerce]
{-# OPTIONS_GHC -fno-strictness #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Unsafe #-}

module Unsafe.Coerce
  ( unsafeCoerce, unsafeCoerceUnlifted, unsafeCoerceAddr
  , unsafeEqualityProof
  , UnsafeEquality (..)
  , unsafeCoerce#
  ) where

import GHC.Arr (amap) -- For amap/unsafeCoerce rule
import GHC.Base

{- Note [Implementing unsafeCoerce]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The implementation of unsafeCoerce is surprisingly subtle.
This Note describes the moving parts.  You will find more
background in MR !1869 and ticket #16893.

The key challenge is this.  Suppose we have
   case sameTypeRep t1 t2 of
      False -> blah2
      True  -> ...(case (x |> UnsafeCo @t1 @t2) of { K -> blah })...

The programmer thinks that the unsafeCoerce from 't1' to 't2' is safe,
because it is justified by a runtime test (sameTypeRep t1 t2).
It used to compile to a cast, with a magical 'UnsafeCo' coercion.

But alas, nothing then stops GHC floating that call to unsafeCoerce
outwards so we get
   case (x |> UnsafeCo @t1 @t2) of
     K -> case sameTypeRep t1 t2 of
             False -> blah2
             True  -> ...blah...

and this is utterly wrong, because the unsafeCoerce is being performed
before the dynamic test. This is exactly the setup in #16893.

The solution is this:

* In the library Unsafe.Coerce we define:

     unsafeEqualityProof :: forall k (a :: k) (b :: k).
                            UnsafeEquality a b

* It uses a GADT, Unsafe.Coerce.UnsafeEquality, that is exactly like :~:

    data UnsafeEquality (a :: k) (b :: k) where
      UnsafeRefl :: UnsafeEquality a a

* We can now define Unsafe.Coerce.unsafeCoerce very simply:

   unsafeCoerce :: forall (a :: Type) (b :: Type) . a -> b
   unsafeCoerce x = case unsafeEqualityProof @a @b of
                      UnsafeRefl -> x

  There is nothing special about unsafeCoerce; it is an
  ordinary library definition, and can be freely inlined.

Now our bad case can't happen.  We'll have
     case unsafeEqualityProof @t1 @t2 of
        UnsafeRefl (co :: t1 ~ t2) -> ....(x |> co)....

and the (x |> co) mentions the evidence 'co', which prevents it
floating.

But what stops the whole (case unsafeEqualityProof of ...) from
floating?  Answer: we never float a case on a redex that can fail
outside a conditional.  See Primop.hs,
Note [Transformations affected by can_fail and has_side_effects].
And unsafeEqualityProof (being opaque) is definitely treated as
can-fail.

While unsafeCoerce is a perfectly ordinary function that needs no
special treatment, Unsafe.Coerce.unsafeEqualityProof is magical, in
several ways

(U1) unsafeEqualityProof is /never/ inlined.

(U2) In CoreToStg.Prep, we transform
       case unsafeEqualityProof of UnsafeRefl g -> blah
      ==>
       blah[unsafe-co/g]

     This eliminates the overhead of evaluating the unsafe
     equality proof.

     Any /other/ occurrence of unsafeEqualityProof is left alone.
     For example you could write
         f :: UnsafeEquality a b -> blah
         f eq_proof = case eq_proof of UnsafeRefl -> ...
    (Nothing special about that.)  In a call, you might write
         f unsafeEqualityProof

    and we'll generate code simply by passing the top-level
    unsafeEqualityProof to f.  As (U5) says, it is implemented as
    UnsafeRefl so all is good.

    NB: Don't discard the case if the case-binder is used
           case unsafeEqualityProof of wild_xx { UnsafeRefl ->
           ...wild_xx...
        That rarely happens, but see #18227.

(U3) In GHC.CoreToStg.Prep.cpeRhsE, if we see
       let x = case unsafeEqualityProof ... of
                 UnsafeRefl -> K e
       in ...

     there is a danger that we'll go to
        let x = case unsafeEqualityProof ... of
                  UnsafeRefl -> let a = e in K a
        in ...

     and produce a thunk even after discarding the unsafeEqualityProof.
     So instead we float out the case to give
        case unsafeEqualityProof ... of { UnsafeRefl ->
        let a = e
            x = K a
        in ...  }
     Floating the case is OK here, even though it broadens the
     scope, because we are done with simplification.

(U4) Ditto GHC.Core.Unfold.inlineBoringOk we want to treat
     the RHS of unsafeCoerce as very small; see
     Note [Inline unsafeCoerce] in that module.

(U5) The definition of unsafeEqualityProof in Unsafe.Coerce
     looks very strange:
        unsafeEqualityProof = case unsafeEqualityProof @a @b of
                                 UnsafeRefl -> UnsafeRefl

     It looks recursive!  But the above-mentioned CoreToStg
     transform will change it to
        unsafeEqualityProof = UnsafeRefl
     And that is exactly the code we want!  For example, if we say
        f unsafeEqualityProof
     we want to pass an UnsafeRefl constructor to f.

     We turn off strictness analysis in this module, otherwise
     the strictness analyser will mark unsafeEqualityProof as
     bottom, which is utterly wrong.

(U6) The UnsafeEquality data type is also special in one way.
     Consider this piece of Core
        case unsafeEqualityProof @Int @Bool of
           UnsafeRefl (g :: Int ~# Bool) -> ...g...

     The simplifier normally eliminates case alternatives with
     contradicatory GADT data constructors; here we bring into
     scope evidence (g :: Int~Bool).  But we do not want to
     eliminate this particular alternative!  So we put a special
     case into DataCon.dataConCannotMatch to account for this.

(U7) We add a built-in RULE
       unsafeEqualityProof k t t  ==>  UnsafeRefl (Refl t)
     to simplify the case when the two types are equal.

(U8) The is a super-magic RULE in GHC.base
         map coerce = coerce
     (see Note [Getting the map/coerce RULE to work] in GHC.Core.SimpleOpt)
     But it's all about turning coerce into a cast, and unsafeCoerce
     no longer does that.  So we need a separate map/unsafeCoerce
     RULE, in this module.

     Adding these RULES means we must delay inlinine unsafeCoerce
     until the RULES have had a chance to fire; hence the INLINE[1]
     pragma on unsafeCoerce.  (Side note: this has the coincidental
     benefit of making the unsafeCoerce-based version of the `reflection`
     library work -- see #21575.)

There are yet more wrinkles

(U9) unsafeCoerce works only over types of kind `Type`.
     But what about other types?  In Unsafe.Coerce we also define

      unsafeCoerceUnlifted :: forall (a :: TYPE UnliftedRep)
                                     (b :: TYPE UnliftedRep).
                              a -> b
      unsafeCoerceUnlifted x
        = case unsafeEqualityProof @a @b of
              UnsafeRefl -> x

     and similarly for unsafeCoerceAddr, unsafeCoerceInt, etc.

(U10) We also want a representation-polymorphic unsafeCoerce#:

       unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                        (a :: TYPE r1) (b :: TYPE r2).
                        a -> b

      This is even more dangerous, because it converts between two types
      *with different runtime representations*!!  Our goal is to deprecate
      it entirely.  But for now we want it.

      But having it is hard!  It is defined by a kind of stub in Unsafe.Coerce,
      and overwritten by the desugarer.  See Note [Wiring in unsafeCoerce#]
      in Desugar.  Here's the code for it
        unsafeCoerce# x = case unsafeEqualityProof @r1 @r2 of UnsafeRefl ->
                          case unsafeEqualityProof @a  @b  of UnsafeRefl ->
                          x
      Notice that we can define this kind-/heterogeneous/ function by calling
      the kind-/homogeneous/ unsafeEqualityProof twice.

      See Note [Wiring in unsafeCoerce#] in Desugar.
-}

-- | This type is treated magically within GHC. Any pattern match of the
-- form @case unsafeEqualityProof of UnsafeRefl -> body@ gets transformed just into @body@.
-- This is ill-typed, but the transformation takes place after type-checking is
-- complete. It is used to implement 'unsafeCoerce'. You probably don't want to
-- use 'UnsafeRefl' in an expression, but you might conceivably want to pattern-match
-- on it. Use 'unsafeEqualityProof' to create one of these.
data UnsafeEquality a b where
  UnsafeRefl :: UnsafeEquality a a

{-# NOINLINE unsafeEqualityProof #-}
unsafeEqualityProof :: forall a b . UnsafeEquality a b
-- See (U5) of Note [Implementing unsafeCoerce]
unsafeEqualityProof = case unsafeEqualityProof @a @b of UnsafeRefl -> UnsafeRefl

{-# INLINE [1] unsafeCoerce #-}
-- The INLINE will almost certainly happen automatically, but it's almost
-- certain to generate (slightly) better code, so let's do it.  For example
--
--   case (unsafeCoerce blah) of ...
--
-- will turn into
--
--   case unsafeEqualityProof of UnsafeRefl -> case blah of ...
--
-- which is definitely better.
--
-- Why delay inlining to Phase 1?  Because of the RULES for map/unsafeCoerce;
-- see (U8) in Note [Implementing unsafeCoerce]

-- | Coerce a value from one type to another, bypassing the type-checker.
--
-- There are several legitimate ways to use 'unsafeCoerce':
--
--   1. To coerce e.g. @Int@ to @HValue@, put it in a list of @HValue@,
--      and then later coerce it back to @Int@ before using it.
--
--   2. To produce e.g. @(a+b) :~: (b+a)@ from @unsafeCoerce Refl@.
--      Here the two sides really are the same type -- so nothing unsafe is happening
--      -- but GHC is not clever enough to see it.
--
--   3. In @Data.Typeable@ we have
--
--      @
--        eqTypeRep :: forall k1 k2 (a :: k1) (b :: k2).
--                     TypeRep a -> TypeRep b -> Maybe (a :~~: b)
--        eqTypeRep a b
--          | sameTypeRep a b = Just (unsafeCoerce HRefl)
--          | otherwise       = Nothing
--      @
--
--      Here again, the @unsafeCoerce HRefl@ is safe, because the two types really
--      are the same  -- but the proof of that relies on the complex, trusted
--      implementation of @Typeable@.
--
--   4. The "reflection trick", which takes advantage of the fact that in
--      @class C a where { op :: ty }@, we can safely coerce between @C a@ and @ty@
--      (which have different kinds!) because it's really just a newtype.
--      Note: there is /no guarantee, at all/ that this behavior will be supported
--      into perpetuity.
--
--
--   For safe zero-cost coercions you can instead use the 'Data.Coerce.coerce' function from
--   "Data.Coerce".
unsafeCoerce :: forall (a :: Type) (b :: Type) . a -> b
unsafeCoerce x = case unsafeEqualityProof @a @b of UnsafeRefl -> x

unsafeCoerceUnlifted :: forall (a :: TYPE ('BoxedRep 'Unlifted)) (b :: TYPE ('BoxedRep 'Unlifted)) . a -> b
-- Kind-homogeneous, but representation-monomorphic (TYPE UnliftedRep)
unsafeCoerceUnlifted x = case unsafeEqualityProof @a @b of UnsafeRefl -> x

unsafeCoerceAddr :: forall (a :: TYPE 'AddrRep) (b :: TYPE 'AddrRep) . a -> b
-- Kind-homogeneous, but representation-monomorphic (TYPE AddrRep)
unsafeCoerceAddr x = case unsafeEqualityProof @a @b of UnsafeRefl -> x

-- | Highly, terribly dangerous coercion from one representation type
-- to another. Misuse of this function can invite the garbage collector
-- to trounce upon your data and then laugh in your face. You don't want
-- this function. Really.
unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                        (a :: TYPE r1) (b :: TYPE r2).
                 a -> b
unsafeCoerce# = error "GHC internal error: unsafeCoerce# not unfolded"
-- See (U10) of Note [Implementing unsafeCoerce]
-- The RHS is updated by Desugar.patchMagicDefns
-- See Desugar Note [Wiring in unsafeCoerce#]

{-# RULES
-- See (U8) in Note [Implementing unsafeCoerce]

-- unsafeCoerce version of the map/coerce rule defined in GHC.Base
"map/unsafeCoerce" map unsafeCoerce = unsafeCoerce

-- unsafeCoerce version of the amap/coerce rule defined in GHC.Arr
"amap/unsafeCoerce" amap unsafeCoerce = unsafeCoerce
 #-}
