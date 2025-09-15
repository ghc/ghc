{-# LANGUAGE TemplateHaskell, ImpredicativeTypes #-}
module T21050 where

import Language.Haskell.TH.Syntax

data T = MkT (forall a. a)

f x = [|| MkT $$(x) ||]

g :: Code Q (forall a. a) -> Code Q T
g x = [|| MkT $$(x) ||]
