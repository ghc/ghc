{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ExplicitStageImports #-}
module SI14 where

import Language.Haskell.TH.Syntax (Lift)

data A = A deriving Lift
