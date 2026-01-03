{-# LANGUAGE StaticPointers #-}

module Foo where

x = False

-- We should warn about the nested binding for y
f z = static (x && y)
 where
   y = True
