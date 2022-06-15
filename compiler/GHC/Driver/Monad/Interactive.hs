module GHC.Driver.Monad.Interactive where

import GHC.Prelude

import GHC.Driver.Monad    ( GhcMonad )
import GHC.Runtime.Context ( InteractiveContext )

class GhcMonad m => GhciMonad m where
  getInteractiveContext :: m InteractiveContext

  setInteractiveContext :: InteractiveContext -> m ()

  modifyInteractiveContext :: (InteractiveContext -> InteractiveContext) -> m ()
  modifyInteractiveContext f = do
    m <- getInteractiveContext
    setInteractiveContext $ f m

modifyInteractiveContextM :: GhciMonad m => (InteractiveContext -> m InteractiveContext) -> m ()
modifyInteractiveContextM f = do
  m <- getInteractiveContext
  setInteractiveContext =<< f m
