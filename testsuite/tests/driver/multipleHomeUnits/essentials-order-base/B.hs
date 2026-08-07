-- A module in a unit that resolves known entities through base's
-- GHC.Essentials.  It is compiled before U (which imports it), so it is what
-- used to populate the compiler's known-entity cache first.
module B where

data T = A | C deriving (Eq, Show)

n :: Int
n = 42
