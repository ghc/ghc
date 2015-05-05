module TH_bracket2 where

d_show = [d| data A = A

             instance Show A  where
                 show _ = "A"
         |]
