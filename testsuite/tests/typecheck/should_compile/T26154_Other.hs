{-# LANGUAGE TypeFamilies #-}

module T26154_Other where

type family OtherF a b

type instance OtherF a b = b