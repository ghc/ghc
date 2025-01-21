{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ExplicitLevelImports #-}
module SI14 where

import Language.Haskell.TH.Syntax (Lift)

-- Deriving Lift doesn't work with ExplicitLevelImports

data A = A deriving Lift
