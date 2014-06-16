{-# LANGUAGE ConstraintKinds #-}

module T8807 where

import Data.Proxy

foo :: $( [t| a b => Proxy a -> b -> b |] )
foo = undefined