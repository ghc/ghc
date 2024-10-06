module T25325 where

import Control.Monad.State

data (f :+: g) a = Inl (f a) | Inr (g a)

newtype Buggy f m = Buggy { thing :: m Int }

class GhcBug f where
  demo :: MonadState (Buggy f m) m => f (m Int) -> m Int

instance (GhcBug f, GhcBug g) => GhcBug (f :+: g) where
    demo (Inl l) = demo l
    demo (Inr r) = demo r
