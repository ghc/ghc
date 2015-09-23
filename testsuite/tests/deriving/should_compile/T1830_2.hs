{-# LANGUAGE DeriveLift #-}
module T1830_2 where

import Language.Haskell.TH.Syntax (Lift)

data Nothing deriving Lift
