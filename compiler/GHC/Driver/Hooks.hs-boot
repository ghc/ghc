module GHC.Driver.Hooks where

import GHC.Prelude ()

data Hooks

emptyHooks :: Hooks

class HasHooks m where
    getHooks :: m Hooks

class ContainsHooks a where
    extractHooks :: a -> Hooks
