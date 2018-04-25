{-# LANGUAGE DataKinds, PolyKinds #-}

module KindLevels where

data A
data B :: A -> *
data C :: B a -> *
data D :: C b -> *
data E :: D c -> *
