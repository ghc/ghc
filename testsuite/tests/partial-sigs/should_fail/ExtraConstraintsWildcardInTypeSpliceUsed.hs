{-# LANGUAGE TemplateHaskell #-}
module ExtraConstraintsWildcardInTypeSpliceUsed where

import ExtraConstraintsWildcardInTypeSplice

-- An extra-constraints wild card is not supported in type splices
eq :: $(metaType)
eq x y = x == y
