{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Enum
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- The 'Enum' class.
--
-----------------------------------------------------------------------------

module Data.Enum
    ( Enum(..)
    , {-# DEPRECATED "Bounded should be imported from Data.Bounded" #-}
      Bounded(..)
    ) where

import GHC.Internal.Enum
