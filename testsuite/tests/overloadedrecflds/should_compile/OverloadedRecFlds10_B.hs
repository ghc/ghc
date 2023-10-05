{-# LANGUAGE TypeFamilies #-}
module OverloadedRecFlds10_B (F(..)) where

import OverloadedRecFlds10_A hiding (foo)

data instance F Bool = MkFBool { foo :: Int }
