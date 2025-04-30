{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedSums, NoListTuplePuns #-}

{-
Module      :  Data.String.Interpolate.Experimental
Copyright   :  (c) The GHC Team
License     :  see libraries/ghc-experimental/LICENSE

Maintainer  :  ghc-devs@haskell.org
Stability   :  experimental
Portability :  non-portable (GHC extensions)

This module exports the machinery behind -XStringInterpolation
See the proposal for motivation and explanations:
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0570-string-interpolation.rst
-}
module Data.String.Interpolate.Experimental (
  Interpolate (..),
) where

import GHC.Internal.Data.String.Interpolate
