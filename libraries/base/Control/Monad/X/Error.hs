module Control.Monad.X.Error (Error, runError, module T) where

import Control.Monad.X.Identity  
import qualified Control.Monad.X.ErrorT as E
import Control.Monad.X.Trans as T

type Error e = E.ErrorT e Identity

runError    :: Error e a -> Either e a
runError m  = runIdentity (E.runError m)


