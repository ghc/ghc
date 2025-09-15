{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module T15637 where

class C a where f :: String

instance C () where f = "foo"

newtype T = T () deriving C
