{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ExtraConstraintsWildcardInPatternSplice where

foo $( [p| (x :: _) |] ) = x
