{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Q where

import "p2" P
import Control.Monad.IO.Class
import System.IO

q = $(liftIO (print p >> hFlush stdout) >> [| () |])
