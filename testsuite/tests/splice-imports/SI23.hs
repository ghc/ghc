{-# LANGUAGE ExplicitStageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI23 where

import splice SI23A
import splice Language.Haskell.TH.Syntax
import SI23A

main = print $(lift B)
