{-# LANGUAGE ExplicitNamespaces #-}

module T14032f where

f $ a = f a

type f $$ a = f a

infix 0 type $  -- This should be `data` instead of `type`
infix 0 data $$ -- This should be `type` instead of `data`
