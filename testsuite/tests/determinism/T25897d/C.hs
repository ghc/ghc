module B
  ( module A
  ) where

-- The order of the imports is the order in the module re-exports
import A (a)
import A (c)
import A (b)

