{-# LANGUAGE PolyKinds #-}
module Tagged where

newtype Tagged s b = Tagged b deriving Eq
