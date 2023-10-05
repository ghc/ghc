{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
    -- The RULES for the methods of class Category may never fire
    -- e.g. identity/left, identity/right, association;  see #10528

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Category
-- Copyright   :  (c) Ashley Yakeley 2007
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ashley@semantic.org
-- Stability   :  stable
-- Portability :  portable

-- https://gitlab.haskell.org/ghc/ghc/issues/1773

module Control.Category where

import qualified GHC.Base (id,(.))
import Data.Type.Coercion
import Data.Type.Equality
import Data.Coerce (coerce)

infixr 9 .
infixr 1 >>>, <<<

-- | A class for categories. Instances should satisfy the laws
--
-- [Right identity] @f '.' 'id'  =  f@
-- [Left identity]  @'id' '.' f  =  f@
-- [Associativity]  @f '.' (g '.' h)  =  (f '.' g) '.' h@
--
class Category cat where
    -- | the identity morphism
    id :: cat a a

    -- | morphism composition
    (.) :: cat b c -> cat a b -> cat a c

{-# RULES
"identity/left" forall p .
                id . p = p
"identity/right"        forall p .
                p . id = p
"association"   forall p q r .
                (p . q) . r = p . (q . r)
 #-}

-- | @since 3.0
instance Category (->) where
    id = GHC.Base.id
    (.) = (GHC.Base..)

-- | @since 4.7.0.0
instance Category (:~:) where
  id          = Refl
  Refl . Refl = Refl

-- | @since 4.10.0.0
instance Category (:~~:) where
  id            = HRefl
  HRefl . HRefl = HRefl

-- | @since 4.7.0.0
instance Category Coercion where
  id = Coercion
  (.) Coercion = coerce

-- | Right-to-left composition
(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(<<<) = (.)

-- | Left-to-right composition
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
