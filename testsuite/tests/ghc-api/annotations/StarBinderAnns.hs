{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# OPTIONS -Wno-star-is-type #-}

module X (type (X.*)) where

type family (*) a b where { (*) a b = Either b a }
