{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NoImplicitStagePersistence #-}
module SI15 where

import Language.Haskell.TH.Syntax (Lift)

data A = A deriving Lift

