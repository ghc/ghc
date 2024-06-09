{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
    -- The RULES for the methods of class Category may never fire
    -- e.g. identity/left, identity/right, association;  see #10528

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Control.Category
-- Copyright   :  (c) Ashley Yakeley 2007
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ashley@semantic.org
-- Stability   :  stable
-- Portability :  portable

-- https://gitlab.haskell.org/ghc/ghc/issues/1773

module GHC.Internal.Control.Category where

import qualified GHC.Internal.Base (id,(.))
import GHC.Internal.Data.Type.Coercion
import GHC.Internal.Data.Type.Equality
import GHC.Internal.Data.Coerce (coerce)

infixr 9 .
infixr 1 >>>, <<<

-- | A class for categories.
--
-- In mathematics, a /category/ is defined as a collection of /objects/ and a collection
-- of /morphisms/ between objects, together with an /identity morphism/ 'id' for every
-- object and an operation '(.)' that /composes/ compatible morphisms.
--
-- This class is defined in an analogous way. The collection of morphisms is represented
-- by a type parameter @cat@, which has kind @k -> k -> 'Data.Kind.Type'@ for some kind variable @k@
-- that represents the collection of objects; most of the time the choice of @k@ will be
-- 'Data.Kind.Type'.
--
-- ==== __Examples__
--
-- As the method names suggest, there's a category of functions:
--
-- @
-- instance Category '(->)' where
--   id = \\x -> x
--   f . g = \\x -> f (g x)
-- @
--
-- Using the `TypeData` language extension, we can also make a category where `k` isn't
-- `Type`, but a custom kind `Door` instead:
--
-- @
-- type data Door = DoorOpen | DoorClosed
--
-- data Action (before :: Door) (after :: Door) where
--   DoNothing :: Action door door
--   OpenDoor :: Action start DoorClosed -> Action start DoorOpen
--   CloseDoor :: Action start DoorOpen -> Action start DoorClosed
--
-- instance Category Action where
--   id = DoNothing
--
--   DoNothing . action = action
--   OpenDoor rest . action = OpenDoor (rest . action)
--   CloseDoor rest . action = CloseDoor (rest . action)
-- @
--
class Category cat where
    -- | The identity morphism. Implementations should satisfy two laws:
    --
    -- [Right identity] @f '.' 'id'  =  f@
    -- [Left identity]  @'id' '.' f  =  f@
    --
    -- These essentially state that 'id' should "do nothing".
    id :: cat a a

    -- | Morphism composition. Implementations should satisfy the law:
    --
    -- [Associativity]  @f '.' (g '.' h)  =  (f '.' g) '.' h@
    --
    -- This means that the way morphisms are grouped is irrelevant, so it is unambiguous
    -- to write a composition of morphisms as @f '.' g '.' h@, without parentheses.
    (.) :: cat b c -> cat a b -> cat a c

{-# RULES
"identity/left" forall p .
                id . p = p
"identity/right"        forall p .
                p . id = p
"association"   forall p q r .
                (p . q) . r = p . (q . r)
 #-}

-- | @since base-3.0
instance Category (->) where
    id = GHC.Internal.Base.id
    (.) = (GHC.Internal.Base..)

-- | @since base-4.7.0.0
instance Category (:~:) where
  id          = Refl
  Refl . Refl = Refl

-- | @since base-4.10.0.0
instance Category (:~~:) where
  id            = HRefl
  HRefl . HRefl = HRefl

-- | @since base-4.7.0.0
instance Category Coercion where
  id = Coercion
  (.) Coercion = coerce

-- | Right-to-left composition. This is a synonym for '(.)', but it can be useful to make
-- the order of composition more apparent.
(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(<<<) = (.)

-- | Left-to-right composition. This is useful if you want to write a morphism as a
-- pipeline going from left to right.
(>>>) :: Category cat => cat a b -> cat b c -> cat a c
f >>> g = g . f
{-# INLINE (>>>) #-} -- see Note [INLINE on >>>]

{- Note [INLINE on >>>]
~~~~~~~~~~~~~~~~~~~~~~~
It’s crucial that we include an INLINE pragma on >>>, which may be
surprising. After all, its unfolding is tiny, so GHC will be extremely
keen to inline it even without the pragma. Indeed, it is actually
/too/ keen: unintuitively, the pragma is needed to rein in inlining,
not to encourage it.

How is that possible? The difference lies entirely in whether GHC will
inline unsaturated calls. With no pragma at all, we get the following
unfolding guidance:
    ALWAYS_IF(arity=3,unsat_ok=True,boring_ok=True)
But with the pragma, we restrict inlining to saturated calls:
    ALWAYS_IF(arity=3,unsat_ok=False,boring_ok=True)
Why does this matter? Because the programmer may have put an INLINE
pragma on (.):

    instance Functor f => Category (Blah f) where
      id = ...
      Blah f . Blah g = buildBlah (\x -> ...)
      {-# INLINE (.) #-}

The intent here is to inline (.) at all saturated call sites. Perhaps
there is a RULE on buildBlah the programmer wants to fire, or maybe
they just expect the inlining to expose further simplifications.
Either way, code that uses >>> should not defeat this inlining, but if
we inline unsaturated calls, it might! Consider:

    let comp = (>>>) ($fCategoryBlah $dFunctor) in f `comp` (g `comp` h)

While simplifying this expression, we’ll start with the RHS of comp.
Without the INLINE pragma on >>>, we’ll inline it immediately, even
though it isn’t saturated:

    let comp = \f g -> $fCategoryBlah_$c. $dFunctor g f
     in f `comp` (g `comp` h)

Now `$fCategoryBlah_$c. $dFunctor g f` /is/ a fully-saturated call, so
it will get inlined immediately, too:

    let comp = \(Blah g) (Blah f) -> buildBlah (\x -> ...)
     in f `comp` (g `comp` h)

All okay so far. But if the RHS of (.) is large, comp won’t be inlined
at its use sites, and any RULEs on `buildBlah` will never fire. Bad!

What happens differently with the INLINE pragma on >>>? Well, we won’t
inline >>> immediately, since it isn’t saturated, which means comp’s
unfolding will be tiny. GHC will inline it at both use sites:

    (>>>) ($fCategoryBlah $dFunctor) f
          ((>>>) ($fCategoryBlah $dFunctor) g h)

And now the calls to >>> are saturated, so they’ll be inlined,
followed by (.), and any RULEs can fire as desired. Problem solved.

This situation might seem academic --- who would ever write a
definition like comp? Probably nobody, but GHC generates such
definitions when desugaring proc notation, which causes real problems
(see #18013). That could be fixed by changing the proc desugaring, but
fixing it this way is the Right Thing, it might benefit other programs
in more subtle ways too, and it’s easier to boot. -}
