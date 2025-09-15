module ShouldFail where

(j,k) = case (if True then True else False) of
         True -> (True,1)
         False -> (1,True)
