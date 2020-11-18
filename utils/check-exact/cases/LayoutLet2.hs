module LayoutLet2 where

-- Simple let expression, rename xxx to something longer or shorter
-- and the let/in layout should adjust accordingly
-- In this case the tokens for xxx + a + b should also shift out

foo xxx = let a = 1
              b = 2 in xxx + a + b
  
