
module M where

-- The array package used to have things in this sort of pattern, where
-- the "parse error" rule is needed to close the do block's layout

f :: [IO ()]
f = [do
   undefined
   undefined
   | _ <- undefined]

