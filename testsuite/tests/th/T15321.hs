{-# LANGUAGE TemplateHaskell #-}

module T15321 where

foo :: String
foo = test

bar :: String
bar = $(_ "baz")
