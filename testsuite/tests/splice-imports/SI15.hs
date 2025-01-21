{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NoImplicitStagePersistence #-}
module SI15 where

import Language.Haskell.TH.Syntax (Lift)

-- Deriving Lift doesn't work with NoImplicitStagePersistence

data A = A deriving Lift

