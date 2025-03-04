{-# LANGUAGE RequiredTypeArguments, EmptyCase, LambdaCase #-}
{-# OPTIONS -Wincomplete-patterns #-}

module T25004 where

import Data.Kind

f :: forall (xs :: Type) -> ()
f = \case {}
