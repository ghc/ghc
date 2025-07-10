{-# LANGUAGE TypeFamilies #-}

module Other where

type family OtherF a b

type instance OtherF a b = b
