{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.IO.Handle.Types ( Handle ) where

-- See Note [Depend on GHC.Num.Integer] in GHC.Internal.Base
import GHC.Types ()

data Handle
