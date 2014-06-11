module Main where

newtype T a = MkT a

-- Trac #9128: we treated x as absent!!!!

f x = let {-# NOINLINE h #-}
          h = case x of MkT g -> g
      in 
      h (h (h (h (h (h True)))))

main = print (f (MkT id))
