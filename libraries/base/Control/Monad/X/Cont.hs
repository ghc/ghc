module Control.Monad.X.Cont (Cont,runCont,module T) where

import Control.Monad.X.Identity  
import qualified Control.Monad.X.ContT as C
import Control.Monad.X.Trans as T

type Cont r   = C.ContT r Identity

runCont       :: Cont a a -> a
runCont m     = runIdentity (C.runCont m)


