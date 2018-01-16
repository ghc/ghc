{-# OPTIONS_GHC -fdefer-typed-holes #-}

module T11274 where

data Asd = Asd

someHole = _asd

missingInstance :: Asd -> Asd -> Bool
missingInstance x y = x == y
