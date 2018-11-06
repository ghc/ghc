{-# LANGUAGE MagicHash #-}

import GHC.Exts
import GHC.Prim

main :: IO ()
main = print (cmpT a T2)
  where
    {-# NOINLINE f #-}
    f = T2
    {-# NOINLINE a #-}
    a = f

data T = T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9

cmpT a b
    = case dataToTag# a of
        a' -> case dataToTag# b of
                b' ->
                      if tagToEnum# (a' <# b') :: Bool then
                          LT -- used to return this
                      else
                          if tagToEnum# (a' ==# b') :: Bool then
                              EQ -- should return this
                          else
                              GT
