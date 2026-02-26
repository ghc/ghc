{-# LANGUAGE PatternSynonyms #-}

module PatSynWhereFail3 where

pattern P x <- x:xs where {}
