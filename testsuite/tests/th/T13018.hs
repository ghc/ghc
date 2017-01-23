{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module T13018 where

data T a where
  MkT :: Eq b => b -> T a

$([d| pattern P :: b -> T a
      pattern P x <- MkT x
    |])
