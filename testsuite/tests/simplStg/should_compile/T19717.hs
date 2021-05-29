{-# LANGUAGE ScopedTypeVariables #-}

module Foo where


f x = x `seq` [Just x, Just x]
