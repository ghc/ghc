module T14881Aux where

-- unambiguous function name.
adjust :: ()
adjust = undefined

-- ambiguous function name.
length :: ()
length = undefined

data L = Cons { x :: Int      -- unambiguous field selector
              , tail :: [Int] -- ambiguous field selector
              }
