{-# LANGUAGE PartialTypeSignatures, FlexibleContexts #-}

module InstanceGivenOverlap3 where

f :: Eq [a] => a -> _
f x = x
