module Control.Monad.X.Nondet (Nondet,runNondet,runNondets,module T) where

import Control.Monad.X.Identity  
import qualified Control.Monad.X.NondetT as N
import Control.Monad.X.Trans as T


-- this is simply list
type Nondet   = N.NondetT Identity

runNondet     :: Nondet a -> Maybe a
runNondet m   = runIdentity (N.runNondet m)

runNondets    :: Nondet a -> [a]
runNondets m  = runIdentity (N.runNondets m)

