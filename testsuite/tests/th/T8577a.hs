{-# LANGUAGE TemplateHaskell #-}
module T8577a where
import Language.Haskell.TH

data A a = A

x :: Code Q (A a)
x = [|| A ||]

y :: Code Q (A Int)
y = x
