{-# LANGUAGE ConstraintKinds #-}

module A where

type K a = (Show a, Read a)
class K a => C a where
