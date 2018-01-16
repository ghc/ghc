{-# LANGUAGE ScopedTypeVariables #-}
module ExtraConstraintsWildcardInPatternSignature where

foo (x :: _ => _) y = x == y
