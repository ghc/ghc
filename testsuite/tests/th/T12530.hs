{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module T12530 where

import Language.Haskell.TH

$([d|   -- Test the Template Haskell pretty-printing for TypeApplications
        f :: Maybe Int -> Maybe Int
        f = id @(Maybe Int)

        -- Wildcards and scoped type variables too
        g :: forall a. a
        g = undefined @(_) @(a)
    |])
