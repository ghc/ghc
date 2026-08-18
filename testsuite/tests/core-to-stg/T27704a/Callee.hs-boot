{-# LANGUAGE KindSignatures #-}
module Callee where

import Data.Kind (Type)

-- No context, no methods, no associated types, so buildAbstractClass makes an
-- AbstractTyCon rather than a UnaryClassTyCon: Mid cannot see that UC is unary.
class UC (a :: Type)

instance UC a => UC [a]
