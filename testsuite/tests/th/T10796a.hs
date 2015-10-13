{-# LANGUAGE TemplateHaskell #-}
module T10796a where

import Data.Ratio
import Data.Set (Set, fromList)
import Language.Haskell.TH.Syntax (liftData)

-- Data instance with toConstr implemented using a variable,
-- not a data constructor
splicedSet :: Set Char
splicedSet = $(liftData (fromList "test"))

-- Infix data constructor
splicedRatio :: Ratio Int
splicedRatio = $(liftData (1 % 2 :: Ratio Int))
