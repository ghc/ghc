{-# LANGUAGE TypeOperators, TypeFamilies #-}

module X (type (X.*)) where

type family (*) a b where { (*) a b = Either b a }
