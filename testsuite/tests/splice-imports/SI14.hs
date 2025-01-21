{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ExplicitLevelImports #-}
module SI14 where

import Language.Haskell.TH.Syntax (Lift)

data A = A deriving Lift
