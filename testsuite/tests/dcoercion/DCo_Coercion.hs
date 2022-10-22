{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}

module DCo_Coercion where

import Data.Type.Equality
  ( (:~:)(..), (:~~:)(..) )
import Data.Coerce
  ( Coercible, coerce )

data Coercion a b where
  Coercion :: Coercible a b => Coercion a b

class TestCoercion f where
  testCoercion :: f a -> f b -> Maybe (Coercion a b)

instance TestCoercion ((:~:) a) where
  testCoercion Refl Refl = Just Coercion

instance TestCoercion ((:~~:) a) where
  testCoercion HRefl HRefl = Just Coercion
