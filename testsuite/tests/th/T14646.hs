{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module T14646 where

$([d| f :: (forall a. a) -> Int
      f _ = undefined |])
