{-# LANGUAGE TypeFamilies #-}
module OverloadedRecFldsFail10_B (F(..)) where

import OverloadedRecFldsFail10_A hiding (foo)

data instance F Bool = MkFBool { foo :: Int }
