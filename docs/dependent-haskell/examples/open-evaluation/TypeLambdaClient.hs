{-# LANGUAGE ExperimentalDependentTypes, PolyKinds #-}

module TypeLambdaClient (forwardErrors) where

import Data.Proxy (Proxy)
import TypeLambda (Fields, Map, withErrors)

-- | Give each field the same error type through an imported function.
forwardErrors :: Proxy e -> Fields xs -> Fields (Map (\field -> Either e field) xs)
forwardErrors p fields = withErrors p fields
