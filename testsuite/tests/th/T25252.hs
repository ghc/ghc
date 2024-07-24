{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Word
import Foreign.StablePtr
import Control.Monad.IO.Class
import T25252B

main :: IO ()
main = pure $(liftIO foo >> [| () |])
