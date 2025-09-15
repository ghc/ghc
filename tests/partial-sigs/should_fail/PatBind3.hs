{-# LANGUAGE PartialTypeSignatures #-}
module PatBind3 where

-- Oddly GHC 8.0 accepted this, but it should obviously fail!
foo :: (Bool, _) -> Char
Just foo = Just id
