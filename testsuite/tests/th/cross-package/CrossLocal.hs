{-# language PackageImports #-}

module CrossLocal where

import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (lift)
-- just to be sure that the file isn't accidentally picked up locally
import "dep" CrossDep (dep)
import CrossNum (num)

splc :: ExpQ
splc = lift @_ @Int (num + dep)
