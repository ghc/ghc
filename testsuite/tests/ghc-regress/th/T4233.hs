
{-# LANGUAGE TypeFamilies, TypeOperators, TemplateHaskell #-}

module Q where

import Language.Haskell.TH
type family a :<=: b
w = varT (mkName "w")
f = [t|Maybe $w :<=: Int|]
