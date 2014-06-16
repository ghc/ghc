{-# LANGUAGE FunctionalDependencies #-}  {-# LANGUAGE FlexibleInstances #-}

module T7171a where

import Data.ByteString as B
import Data.Word

class Foo a b | a -> b

class (Foo a b) => Bar a b | a -> b

instance Foo [a] a
instance Bar [a] a
instance Foo ByteString Word8
instance Bar ByteString Word8

test :: Bar full item => full -> full
test inp = inp
