{-# LANGUAGE GHC2021 #-}
g i = let a = i + 1
          b = id
          c = ()
          d = (+)
      in (a,b,c,d)
