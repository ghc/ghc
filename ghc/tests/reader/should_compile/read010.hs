module ShouldSucceed where

--!!! Infix record constructor.

data Rec = (:<-:) { a :: Int, b :: Float }
