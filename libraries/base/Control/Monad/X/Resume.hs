module Control.Monad.X.Resume (Resume, hyper, module T) where

import Control.Monad.X.Identity  
import qualified Control.Monad.X.ResumeT as R
import Control.Monad.X.Trans as T     

type Resume   = R.ResumeT Identity

hyper         :: Resume a -> a
hyper m       = runIdentity (R.hyper m)


