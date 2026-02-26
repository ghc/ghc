{-# LANGUAGE PatternSynonyms #-}

module PatSynWhereFail4 where

pattern P x <- x:xs where
  P x = wrap x
  wrap x = [x]
