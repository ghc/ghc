module LinearGuards where

f :: a ->. a
f a | True = a
    | False = a
