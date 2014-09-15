{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Category
-- Copyright   :  (c) Ashley Yakeley 2007
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ashley@semantic.org
-- Stability   :  experimental
-- Portability :  portable

-- http://ghc.haskell.org/trac/ghc/ticket/1773

module Control.Category where

import qualified GHC.Base (id,(.))
import Data.Type.Coercion
import Data.Type.Equality
import GHC.Prim (coerce)

infixr 9 .
infixr 1 >>>, <<<

-- | A class for categories.
--   id and (.) must form a monoid.
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

instance Category (->) where
    id = GHC.Base.id
    (.) = (GHC.Base..)

instance Category (:~:) where
  id          = Refl
  Refl . Refl = Refl

instance Category Coercion where
  id = Coercion
  (.) Coercion = coerce

-- | Right-to-left composition
(<<<) :: Category cat => cat b c -> cat a b -> cat a c
(<<<) = (.)

-- | Left-to-right composition
(>>>) :: Category cat => cat a b -> cat b c -> cat a c
f >>> g = g . f
