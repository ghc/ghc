module T16348 where

data V2 a = V2 !a !a

inv22 _ = case V2 (V2 1 2) (V2 3 4) of
             V2 _ (V2 _ z) -> z
