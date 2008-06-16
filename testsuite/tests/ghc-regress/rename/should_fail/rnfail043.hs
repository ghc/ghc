{-# LANGUAGE TemplateHaskell #-}

-- Duplicate bindings introduced one at a time with TH
module ShouldFail where

f x = x

$([d| h x = x |])

f x = x
