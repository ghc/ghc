{-# LANGUAGE TypeOperators #-}

module T6027 where

data (?) a b = Q a b

infixr 2 ?

test :: Int ? String ? Bool
test = 0 `Q` ("foo" `Q` True)
