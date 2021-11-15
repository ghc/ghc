module OpaqueParseFail2 where

f = id
{-# OPAQUE CONLIKE f #-}
