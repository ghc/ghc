module A where

import B

data HsExpr id
  = HsOverLit (HsOverLit id)
  | HsBracketOut (HsExpr id)
  deriving Eq

