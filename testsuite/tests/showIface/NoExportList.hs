-- | Module header
module NoExportList where

import Data.List

-- * Types
--
-- $types
--
-- Actually we have only one type.

data R = R
  { fα :: () -- ^ Documentation for 'R'\'s 'fα' field.
  , fβ :: ()
  }

-- | A very lazy Eq instance
instance Eq R where
  _r0 == _r1 = True

-- * Functions
--
-- $functions
--
-- We have them too.

add :: Int -> Int -> Int
add = (+)
