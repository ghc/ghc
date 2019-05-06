{-# LANGUAGE LinearTypes #-}
module Foo where

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
