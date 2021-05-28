{-# LANGUAGE TemplateHaskell #-}
module OneShotTH where

main = $([| print () |])
