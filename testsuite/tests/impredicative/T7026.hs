{-# LANGUAGE ImplicitParams, ImpredicativeTypes #-}

module Bug where

    f1 :: Maybe ((?a :: Bool) => Char)
    f1 = Just 'C'

    f2 :: Maybe ((?a :: Bool) => Bool)
    f2 = Just ?a
