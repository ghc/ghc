module T17801 where

import Control.Monad.Except
import T17810a

f :: ExceptT e (TCMT IO) ()
f = liftReduce
