{-# LANGUAGE DeriveLift #-}
module T1830 where

import Language.Haskell.TH.Syntax (Lift)

data Nothing deriving Lift
