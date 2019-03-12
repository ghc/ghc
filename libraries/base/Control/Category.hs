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
-- Stability   :  experimental
-- Portability :  portable

-- https://gitlab.haskell.org/ghc/ghc/issues/1773

module Control.Category where

import qualified GHC.Base (id,(.))
import Data.Type.Coercion
import Data.Type.Equality
import GHC.Prim (coerce)

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
