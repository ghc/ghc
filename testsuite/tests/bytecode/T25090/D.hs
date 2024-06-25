module D where

import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (lift)
import B
import C

splc :: ExpQ
splc =
  lift @_ @Int num
  where
    B (C num) = b
