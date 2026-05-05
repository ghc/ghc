{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fdefines-known-key-names #-}

module GHC.Internal.Exception.Context where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Internal.Types as Rebindable

data ExceptionContext

