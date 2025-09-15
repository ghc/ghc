{-# LANGUAGE PartialTypeSignatures, NamedWildCards #-}
module Either where

barry :: _a -> (_b _a, _b _)
barry x = (Left "x", Right x)
