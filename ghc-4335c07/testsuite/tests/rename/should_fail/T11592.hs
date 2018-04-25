{-# LANGUAGE TypeInType #-}

module Bug11592 where

data A (a :: a) = MkA String

data B b (a :: a b) = MkB String
data C b (a :: b a) = MkC String

data D b c (a :: c a b) = MkD String
