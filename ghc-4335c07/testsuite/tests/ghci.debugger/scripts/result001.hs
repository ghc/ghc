f xs = case map id xs of
         [] -> True
         x:xs -> False
