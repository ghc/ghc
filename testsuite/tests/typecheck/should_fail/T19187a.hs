{-# LANGUAGE FlexibleContexts #-}

module T19187a where

data T

instance Eq Int => Eq T
