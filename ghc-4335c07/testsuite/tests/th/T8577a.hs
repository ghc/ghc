{-# LANGUAGE TemplateHaskell #-}
module T8577a where
import Language.Haskell.TH

data A a = A

x :: Q (TExp (A a))
x = [|| A ||]

y :: Q (TExp (A Int))
y = x
