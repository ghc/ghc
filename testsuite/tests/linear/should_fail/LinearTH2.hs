{-# LANGUAGE LinearTypes, TemplateHaskell, RankNTypes #-}
module LinearTH2 where

x1 = [t|forall p. Int -->.(p) Int|]
