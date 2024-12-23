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
-- @since 4.20.0.0
--
-----------------------------------------------------------------------------

module Data.Enum
    ( Enum(..)
    , {-# DEPRECATED "Bounded should be imported from Data.Bounded" #-}
      Bounded(..)
    , enumerate
    ) where

import GHC.Internal.Enum

-- | Returns a list of all values of an enum type
--
-- 'enumerate' is often used to list all values of a custom enum data structure, such as a custom Color enum below:
--
-- @
-- data Color = Yellow | Red | Blue
--     deriving (Enum, Bounded, Show)
--
-- allColors :: [Color]
-- allColors = enumerate
-- -- Result: [Yellow, Red, Blue]
-- @
--
-- Note that you need to derive the 'Bounded' type class as well, only 'Enum' is not enough.
-- 'Enum' allows for sequential enumeration, while 'Bounded' provides the 'minBound' and 'maxBound' values.
--
-- 'enumerate' is commonly used together with the TypeApplications syntax. Here is an example of using 'enumerate' to retrieve all values of the 'Ordering' type:
--
-- >> enumerate @Ordering
-- [LT, EQ, GT]
--
-- The '@' symbol here is provided by the TypeApplications language extension.
--
-- @since base-4.22.0.0
enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]