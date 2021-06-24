module OpaqueParseFail1 where

f = id
{-# OPAQUE[1] f #-}
