module B where

import {-# SOURCE #-} A (HsExpr)

data HsOverLit id
  = OverLit (HsExpr id)
  deriving Eq
