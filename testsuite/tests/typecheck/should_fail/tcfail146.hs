module Foo1 where

-- Variant: class used as data
class SClass a where
 sFun :: a -> SData a
 
data SData a = SCon (SClass a)
