{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}

module T20330b where

import GHC.Exts

foo :: forall w r (z :: TYPE r). w -> z -> z
foo = raise# @w @(z -> z)
