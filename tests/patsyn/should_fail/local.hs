{-# LANGUAGE PatternSynonyms #-}
module ShouldFail where

varWithLocalPatSyn x = case x of
    P -> ()
  where
    pattern P = ()
