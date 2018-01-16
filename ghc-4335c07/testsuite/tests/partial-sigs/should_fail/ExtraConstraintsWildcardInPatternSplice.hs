{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ExtraConstraintsWildcardInPatternSplice where

foo $( [p| (_ :: _) |] ) = ()
