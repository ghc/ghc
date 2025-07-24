{-# LANGUAGE ExplicitLevelImports, TemplateHaskell #-}
module T26090A where

import Language.Haskell.TH

a :: Q Exp
a = [| True |]

data T = T { t :: () }

data S = S { s :: () }

data R = R { r :: () }

