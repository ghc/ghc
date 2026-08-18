module Callee where

import Mid ()

-- Really unary: the dictionary /is/ ucm, so it can be bottom.
class UC a where
  ucm :: a -> a

instance UC Int where
  ucm = id

instance UC a => UC [a] where
  ucm = error "ucm[] forced"
