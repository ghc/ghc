{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Num
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Num' class and the 'Integer' type.
--

module GHC.Num
   ( Num(..)
   , subtract
   , quotRemInteger
   , module GHC.Internal.Bignum.Integer
   , module GHC.Internal.Bignum.Natural
   )
where

import GHC.Internal.Num
import GHC.Internal.Bignum.Integer
import GHC.Internal.Bignum.Natural
