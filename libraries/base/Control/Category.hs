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

-- | A class for categories describing typed composition. Instances
-- should satisfy the laws
--
-- [Right identity] @f '.' 'id'  =  f@
-- [Left identity]  @'id' '.' f  =  f@
-- [Associativity]  @f '.' (g '.' h)  =  (f '.' g) '.' h@
--
-- @Category@ has been poly-kinded since /base-4.7.0.0/
--
-- @
-- Category :: (ob -> ob -> Type) -> Constraint
-- @
--
-- before which instances had kind @Type -> Type -> Type@.
--
-- It lays the groundwork for abstract definitions based on 'Category'
-- that work for instances of @Category@. An /isomorphism/ is a
-- morphism @f :: cat a b@ with an inverse @inv :: cat b a@
--
-- * @inv '.' f = 'id'@
-- * @f '.' inv = 'id'@
--
-- @a@ and @b@ are called isomorphic.
--
-- /See also/: 'Control.Arrow.Arrow'.
class Category cat where
    -- | The identity morphism.
    id :: cat a a

    -- | A composition of two morphisms.
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
