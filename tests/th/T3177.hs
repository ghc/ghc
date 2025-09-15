{- LANGUAGE TemplateHaskell #-}

-- Template Haskell type splices
module T3177 where

f :: $(id [t| Int |])
f = 3

class C a where
  op :: a -> a

instance C a => C ($([t| Maybe |]) a) where
  op x = fmap op x

