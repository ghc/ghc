-- !!! Infix record constructor.
module ShouldCompile where

data Rec = (:<-:) { a :: Int, b :: Float }
