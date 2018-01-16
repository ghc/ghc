module Distribution.Solver.Types.Flag
    ( FlagType(..)
    ) where

data FlagType = Manual | Automatic
  deriving (Eq, Show)
