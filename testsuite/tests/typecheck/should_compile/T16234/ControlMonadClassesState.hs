{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module ControlMonadClassesState where

import qualified Control.Monad.Trans.State.Lazy as SL
import ControlMonadClassesCore
import ControlMonadClassesEffects

type instance CanDo (SL.StateT s m) eff = StateCanDo s eff

type family StateCanDo s eff where
  StateCanDo s (EffReader s) = 'True
  StateCanDo s eff = 'False
