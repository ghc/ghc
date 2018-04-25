{-# LANGUAGE NamedWildCards, ScopedTypeVariables #-}
module WildcardsInPatternAndExprSig where

bar (Just ([x :: _a] :: _) :: Maybe [_b]) (z :: _c) = [x, z] :: [_d]
