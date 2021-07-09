module T13157 where

f g x = (case g x of True  -> not
                     False -> id) `seq` True
