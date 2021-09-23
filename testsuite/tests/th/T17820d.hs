{-# LANGUAGE TemplateHaskell #-}

module Main where

decls = [d| data D = MkD { foo :: Int }
            blargh = $(const [| 0 |] foo) |]
