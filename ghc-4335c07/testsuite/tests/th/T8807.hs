{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE ConstraintKinds, RankNTypes #-}

module T8807 where

import Data.Proxy

foo :: $( [t| forall a b. a b => Proxy a -> b -> b |] )
foo = undefined
