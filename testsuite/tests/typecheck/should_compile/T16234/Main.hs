{-# LANGUAGE TypeFamilies #-}
module Main where

import ControlMonadClasses (MonadReader)
--import ControlMonadPrimitive ()
import Control.Monad.Trans.State.Lazy (StateT)

main :: (n ~ StateT () IO, MonadReader () n) => IO ()
main = undefined
