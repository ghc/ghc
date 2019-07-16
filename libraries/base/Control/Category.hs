{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
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
-- Stability   :  experimental
-- Portability :  portable

-- https://gitlab.haskell.org/ghc/ghc/issues/1773

module Control.Category where

import qualified GHC.Base (id,(.))
import Data.Type.Coercion
import Data.Type.Equality
import Data.Coerce (coerce)

infixr 9 .
infixr 1 >>>, <<<

-- | A class for categories describing composition of typed
-- arrows. Has kind
--
-- @
-- Category :: (ob -> ob -> Type) -> Constraint
-- @
--
-- Category has been polykinded since base 4.7.0.0 (GHC 7.8.1, Apr
-- 2014) before which instances had kind @Type -> Type -> Type@.
--
-- Instances should satisfy the laws
--
-- [Right identity] @f '.' 'id'  =  f@
-- [Left identity]  @'id' '.' f  =  f@
-- [Associativity]  @f '.' (g '.' h)  =  (f '.' g) '.' h@
--
-- It lays the groundwork for abstract definitions that are 'Category'
-- polymorphic and apply to any instance. The central notion of an
-- /isomorphism/ which is an arrow @f :: cat a b@ that has an inverse
-- @inv :: cat b a@
--
-- * @inv '.' f = 'id' \@a@
-- * @f '.' inv = 'id' \@b@
--
-- and we say that @a@ and @b@ are isomorphic.
--
-- /See also/: 'Control.Arrow.Arrow', @Profunctor@.
class Category cat where
    -- | The identity arrow.
    id :: cat a a

    -- | A composition of two arrows.
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
  id :: a -> a
  id = GHC.Base.id

  (.) :: (b -> c) -> (a -> b) -> (a -> c)
  (.) = (GHC.Base..)

-- | @since 4.7.0.0
instance Category (:~:) where
  id :: a :~: a
  id = Refl

  (.) :: (b :~: c) -> (a :~: b) -> (a :~: c)
  Refl . Refl = Refl

-- | @since 4.10.0.0
instance Category (:~~:) where
  id :: a :~~:a
  id = HRefl

  (.) :: (b :~~: c) -> (a :~~: b) -> (a :~~: c)
  HRefl . HRefl = HRefl

-- | @since 4.7.0.0
instance Category Coercion where
  id :: Coercion a a
  id = Coercion

  (.) :: Coercion b c -> Coercion a b -> Coercion a c
  (.) Coercion = coerce

-- | Right-to-left composition
(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(<<<) = (.)

-- | Left-to-right composition
(>>>) :: Category cat => cat a b -> cat b c -> cat a c
f >>> g = g . f
