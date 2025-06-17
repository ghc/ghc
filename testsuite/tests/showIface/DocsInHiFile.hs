{-# LANGUAGE TypeFamilies, GHC2021 #-}
{-| `elem`, 'print',
`Unknown',
'<>', ':=:', 'Bool'
-}
module DocsInHiFile
  ( DocsInHiFile.elem
  , D(..)
  , add
  , P(..)
  , Show(..)
  ) where

-- | '()', 'elem'.
elem :: ()
elem = ()

-- | A datatype.
data D
  = D0 -- ^ A constructor for 'D'. '
  | D1 -- ^ Another constructor
  deriving ( Show -- ^ 'Show' instance
           )

add :: Int -- ^ First summand for 'add'
    -> Int -- ^ Second summand
    -> Int -- ^ Sum
add a b = a + b

-- | A class
class P f where
  -- | A class method
  p :: a -- ^ An argument
    -> f a

-- | Another datatype...
data D'
-- ^ ...with two docstrings.

-- | A type family
type family F a
-- | A type family instance
type instance F Int = Bool
