{-# OPTIONS_GHC -fdefer-type-errors #-}

module RepPolyDeferred where

import GHC.Exts

foo :: forall rep (a :: TYPE rep). a -> a
foo bndr_a = bndr_a
