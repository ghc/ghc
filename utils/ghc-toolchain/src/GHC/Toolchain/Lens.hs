-- | A very simple Lens implementation
module GHC.Toolchain.Lens
    ( Lens(..)
    , (%)
    , over
    ) where

import Prelude ((.), ($))

data Lens a b = Lens { view :: (a -> b), set :: (b -> a -> a) }

(%) :: Lens a b -> Lens b c -> Lens a c
a % b = Lens { view = view b . view a
             , set = \y x -> set a (set b y (view a x)) x
             }

over :: Lens a b -> (b -> b) -> a -> a
over l f x = set l (f $ view l x) x

