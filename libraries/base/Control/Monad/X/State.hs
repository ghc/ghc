module Control.Monad.X.State (State, runState, runStateS, module T) where

import Control.Monad.X.Identity  
import qualified Control.Monad.X.StateT as S
import Control.Monad.X.Trans as T

type State s  = S.StateT s Identity

runState      :: s -> State s a -> a
runState s m  = runIdentity (S.runState s m)

runStateS     :: s -> State s a -> (a,s)
runStateS s m = runIdentity (S.runStateS s m)

