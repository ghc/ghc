{-# LANGUAGE RoleAnnotations #-}

module Roles7 where

bar :: Int@P -> Int
bar = id