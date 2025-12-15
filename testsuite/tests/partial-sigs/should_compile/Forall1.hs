{-# LANGUAGE PartialTypeSignatures #-}
module Forall1 where

fall :: forall a. _ -> a
fall v = v
