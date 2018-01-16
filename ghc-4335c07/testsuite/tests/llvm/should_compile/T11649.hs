{-# LANGUAGE NoImplicitPrelude #-}
module Test where
import GHC.Base

data U1 p = U1

instance Functor U1 where
    fmap f U1 = U1

instance Applicative U1 where
    pure _ = U1
    U1 <*> U1 = U1

instance Alternative U1 where
    empty = U1
    U1 <|> U1 = U1
