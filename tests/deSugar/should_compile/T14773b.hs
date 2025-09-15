module T14773b where

b :: Bool
(Just b) | False = Nothing

c :: Bool
(Just c) | False = Nothing
         | True = Just True
