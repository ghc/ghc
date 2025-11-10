{-# LANGUAGE DataKinds #-}

module Bug26246 ( Effect, Env, Eff, State(..) ) where

import Data.Kind

-- | @Effect@
type Effect = (Type -> Type) -> Type -> Type

-- @t'State' s :: 'Effect'@
data State s :: Effect where
  Get    :: State s m s
  Put    :: s -> State s m ()
  State  :: (s ->   (a, s)) -> State s m a
  StateM :: (s -> m (a, s)) -> State s m a

-- | @t'Env' es@
data Env (es :: [Effect]) = Env

-- | @t'Eff' es@
newtype Eff (es :: [Effect]) a = Eff (Env es -> IO a)
