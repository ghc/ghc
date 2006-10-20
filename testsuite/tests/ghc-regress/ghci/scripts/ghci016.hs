
-- Test for trac #552

module Test where

default (T)

data T = T
    deriving (Eq, Show)

instance Num T where
    fromInteger _ = T

-- Typing 3 at the ghci prompt should print T

