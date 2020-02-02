{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE RankNTypes, LiberalTypeSynonyms #-}

-- This test checks that deep skolemisation and deep
-- instantiation work right.  A buggy prototype
-- of GHC 7.0, where the type checker generated wrong
-- code, sent applyTypeToArgs into a loop.

module Twins where

import Data.Data

type GenericQ r = forall a. Data a => a -> r
type GenericM m = forall a. Data a => a -> m a

gzip :: GenericQ (GenericM Maybe) -> GenericQ (GenericM Maybe)
gzip f x y
  = f x y
     `orElse`
    if toConstr x == toConstr y
    then gzipWithM (gzip f) x y
    else Nothing

gzipWithM :: Monad m => GenericQ (GenericM m) -> GenericQ (GenericM m)
gzipWithM _ _ = error "urk"

orElse :: Maybe a -> Maybe a -> Maybe a
orElse = error "urk"
