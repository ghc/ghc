{-# LANGUAGE PartialTypeSignatures #-}
module ExtraConstraintsWildcardTwice where

foo :: ((_), _) => a -> a
foo = undefined
