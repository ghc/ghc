{-# LANGUAGE PartialTypeSignatures, NamedWildCards #-}
module ShowNamed where

showTwo :: Show _a => _a -> String
showTwo x = show x
