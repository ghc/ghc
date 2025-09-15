{-# LANGUAGE RebindableSyntax #-}
module Rebindable12 where

import Prelude

ifThenElse :: Char -> () -> () -> () -> ()
ifThenElse _ _ _ _ = ()

y :: ()
y = if 'a' then () else ()
