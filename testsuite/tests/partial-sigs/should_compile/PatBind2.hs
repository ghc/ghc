{-# LANGUAGE PartialTypeSignatures #-}
module PatBind2 where

foo :: Bool -> _
Just foo = Just id
