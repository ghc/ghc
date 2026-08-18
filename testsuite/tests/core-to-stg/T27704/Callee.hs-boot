{-# LANGUAGE KindSignatures #-}
module Callee where

import Data.Kind (Type)

-- Abstract class declaration: no context, no methods, no associated types,
-- so GHC.Tc.TyCl builds an AbstractTyCon rather than a UnaryClassTyCon.
class UC (a :: Type)
