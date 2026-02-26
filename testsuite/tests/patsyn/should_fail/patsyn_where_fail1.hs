{-# LANGUAGE PatternSynonyms #-}

module PatSynWhereFail1 where

pattern P x <- x:xs where
  P { head = x } = [x]
