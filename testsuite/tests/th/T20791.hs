{-# LANGUAGE TemplateHaskell #-}
module T20791 where

import Control.Monad.IO.Class

main = $(liftIO $ (print ()) >> [| print () |])

