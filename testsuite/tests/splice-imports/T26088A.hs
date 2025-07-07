{-# LANGUAGE ExplicitLevelImports, TemplateHaskell #-}
module T26088A where

import splice T26088B
import Language.Haskell.TH.Syntax

x :: Q Exp
x = [| a |]
