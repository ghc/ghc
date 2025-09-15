{-# LANGUAGE LinearTypes, TemplateHaskell, RankNTypes, NoMonomorphismRestriction #-}
module LinearTH2 where

x1 = [t|forall p. Int %p -> Int|]
