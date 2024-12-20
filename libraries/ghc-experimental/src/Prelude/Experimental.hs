{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

{-
Module      :  Prelude.Experimental
Copyright   :  (c) The GHC Team
License     :  see libraries/ghc-experimental/LICENSE

Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  non-portable (GHC extensions)

The Prelude for ghc-experimental, re-exporting definitions peculiar to GHC that
are safe to use in ordinary code, but whose API may evolve rapidly over
subsequent releases.
-}
module Prelude.Experimental (
  List,

  -- * Tuple/sum syntax families and tycons
  module Data.Tuple.Experimental,
  module Data.Sum.Experimental,
) where

import GHC.Internal.Types (List)
import GHC.Internal.Classes

import Data.Tuple.Experimental
import Data.Sum.Experimental
