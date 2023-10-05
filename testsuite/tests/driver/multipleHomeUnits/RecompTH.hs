{-# LANGUAGE TemplateHaskell #-}
module RecompTH where

import Dep

qux = $(const [| () |] foo)
