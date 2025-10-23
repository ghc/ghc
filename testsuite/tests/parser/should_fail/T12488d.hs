{-# LANGUAGE NamedDefaults #-}
module T12488d ( T (default C) ) where

class C a where

data T = A