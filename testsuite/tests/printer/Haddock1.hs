{-# OPTIONS_GHC -fno-warn-redundant-constraints -haddock #-}
-- | Haddock comment,
-- coming before the module
module Haddock1 (

        -- | This is some inline documentation in the export list
        --
        -- > a code block using bird-tracks
        -- > each line must begin with > (which isn't significant unless it
        -- > is at the beginning of the line).
        f

        {-| nested-style doc comments -}
        , g

        -- * A section
        -- and without an intervening comma:
        -- ** A subsection
   ) where

-- | Haddock before imports
import Data.List

-- | Haddock before decl
f = undefined
g = undefined

-- | This comment applies to the /following/ declaration
-- and it continues until the next non-comment line
data T a b
 = A Int (Maybe Float) -- ^ This comment describes the 'A' constructor
 | -- | This comment describes the 'B' constructor
   B (T a b, T Int Float) -- ^ abcd

-- | An abstract data declaration
data T2 a b = T2 a b
