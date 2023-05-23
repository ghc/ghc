{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO.Handle.Types ( Handle ) where

-- See Note [Depend on GHC.Num.Integer] in GHC.Base
import GHC.Types ()

data Handle
