-- !!! Infix record constructor.
module ShouldSucceed where

data Rec = (:<-:) { a :: Int, b :: Float }
