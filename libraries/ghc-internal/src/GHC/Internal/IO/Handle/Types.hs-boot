{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.IO.Handle.Types ( Handle ) where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Internal.Types ()

data Handle
