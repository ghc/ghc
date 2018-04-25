{-# LANGUAGE DuplicateRecordFields, TypeFamilies #-}
module OverloadedRecFldsFail10_C (F(..)) where

import OverloadedRecFldsFail10_A

data instance F Char = MkFChar { foo :: Char }
