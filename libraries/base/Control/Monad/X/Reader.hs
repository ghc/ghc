module Control.Monad.X.Reader (module T, Reader, runReader) where

import Control.Monad.X.Identity  
import qualified Control.Monad.X.ReaderT as R
import Control.Monad.X.Trans as T     

type Reader r = R.ReaderT r Identity

runReader     :: r -> Reader r a -> a
runReader r m = runIdentity (R.runReader r m)


