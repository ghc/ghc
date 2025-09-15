{-# LANGUAGE RequiredTypeArguments, EmptyCase, LambdaCase #-}
{-# OPTIONS -Wincomplete-patterns #-}

module T25004k where

import Data.Kind

f :: ((forall k. forall (xs :: k) -> ()) -> r) -> r
f cont = cont (\case {})
