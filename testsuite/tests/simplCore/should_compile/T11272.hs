module T11272 where

import T11272a as A
import Control.Monad.Trans.State

specialised :: Int -> Int -> ()
specialised x y = execState (A.overloaded x y) ()
