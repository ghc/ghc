-- | A module to define 'ImportLevel' so it can be given an Outputable instance
-- without introducing module loops.
module Language.Haskell.Syntax.ImpExp.ImportLevel ( ImportLevel(..) ) where


import Prelude (Eq, Ord, Show, Enum)
import Data.Data (Data)

data ImportLevel = NormalLevel | SpliceLevel | QuoteLevel deriving (Eq, Ord, Data, Show, Enum)

