module Callee where

import Mid

-- The real UC is a *unary* class: one method, no superclasses.  So a (UC a)
-- dictionary *is* the function ucm, and it can be bottom.
class UC a where
  ucm :: a -> a

instance UC Int where
  ucm = undefined

instance MC Int where
  mcm _ = 7
