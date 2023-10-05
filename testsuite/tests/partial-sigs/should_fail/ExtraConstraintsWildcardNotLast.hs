{-# LANGUAGE PartialTypeSignatures #-}
module ExtraConstraintsWildcardNotLast where

foo :: (_, Eq a) => a -> a
foo = undefined
