{-# LANGUAGE DeriveAnyClass #-}
module Bug where

class C p q

data D = D deriving (C (a :: k))
