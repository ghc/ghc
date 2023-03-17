{-# LANGUAGE DuplicateRecordFields, TypeFamilies #-}
module OverloadedRecFlds10_C (F(..)) where

import OverloadedRecFlds10_A

data instance F Char = MkFChar { foo :: Char }
