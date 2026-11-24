{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances #-}
module TypeLambdaSchema (forwardRowSchema, forwardSchemaTransformer) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import TypeLambda (Map, Prefixes)

-- | Apply the same error schema to an unknown list of column groups.
forwardRowSchema :: forall (e :: Type) (rows :: [[Type]]). Proxy e -> Proxy rows
                 -> Map (Prefixes ('Just e)) rows
                    :~: Map (\cells -> Prefixes ('Just e) cells) rows
forwardRowSchema _ _ = Refl

-- | Compare the two schema transformers before choosing the list of column groups.
forwardSchemaTransformer :: forall (e :: Type). Proxy e
                         -> Map (Prefixes ('Just e))
                            :~: Map (\cells -> Prefixes ('Just e) cells)
forwardSchemaTransformer _ = Refl
