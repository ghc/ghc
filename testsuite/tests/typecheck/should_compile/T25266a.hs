{-# LANGUAGE GADTs #-}

module T25266a where

data T a where { T1 :: T Int; T2 :: a -> T a }

-- Rejected, becuase there is no principal type,
-- and the function is top level
f x y t = (case t of
                      T1   -> length [x,y]
                      T2 _ -> 2)  :: Int


