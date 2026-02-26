{-# LANGUAGE PatternSynonyms #-}

module PatSynWhereFail2 where

pattern P x <- x:xs where
  Just x = [x]
