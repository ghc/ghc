{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module B where

import splice A
import splice Language.Haskell.TH.Syntax (Lift(..))

main = print $(lift (A.foo 41))
