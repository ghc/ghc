module Foo1 where

-- Variant: ill-kinded. 
class XClass a where
 xFun :: a -> XData
 
data XData = XCon XClass
