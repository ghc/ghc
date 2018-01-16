{-# LANGUAGE GADTs #-}

module T12468 where

data T a where
    I :: T Int

f :: T a -> a
f I = _
