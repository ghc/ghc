module Control.Monad.X.Writer (Writer, runWriter, module T)where

import Control.Monad.X.Identity  
import qualified Control.Monad.X.WriterT as W
import Control.Monad.X.Trans as T

type Writer w = W.WriterT w Identity

runWriter     :: Writer w a -> (a,w)
runWriter m   = runIdentity (W.runWriter m)


