{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module T15324 where

$([d| f :: forall a. (Show a => a) -> a
      f _ = undefined
    |])
