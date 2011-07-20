{-# OPTIONS_GHC -dcore-lint #-}

-- Fails GHC 5.04.2 with -dcore-lint
-- The issue ariseswhen you have a method that
-- constrains a class variable

module Test where

class C a where
    f :: (Eq a) => a

instance C () where
    f = f

