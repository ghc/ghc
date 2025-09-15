module M where

-- There should be a cost center in the -ddump-simpl output
addStuff x y = do
    x + y :: Int
