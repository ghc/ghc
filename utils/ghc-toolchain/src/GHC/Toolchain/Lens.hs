-- | A very simple Lens implementation
module GHC.Toolchain.Lens
    ( Lens(..)
    , (%)
    , over
    , (%++)
    , (&)
    ) where

import Prelude ((.), ($), (++))
import Data.Function ((&))

data Lens a b = Lens { view :: (a -> b), set :: (b -> a -> a) }

(%) :: Lens a b -> Lens b c -> Lens a c
a % b = Lens { view = view b . view a
             , set = \y x -> set a (set b y (view a x)) x
             }

over :: Lens a b -> (b -> b) -> a -> a
over l f x = set l (f $ view l x) x

-- | Append @b@ to @[b]@
--
-- Example usage:
-- @@
-- cc & _ccProgram % _prgFlags %++ "-U__i686"
-- @@
(%++) :: Lens a [b] -> b -> (a -> a)
(%++) l el = over l (++[el])

