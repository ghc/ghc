{-# OPTIONS_GHC -XNoImplicitPrelude #-}
module GHC.IO.Handle.FD where

import GHC.IO.Handle.Types

-- used in GHC.Conc, which is below GHC.IO.Handle.FD
stdout :: Handle
