{-# LANGUAGE RebindableSyntax #-}

module Foo where

import Prelude( Bool(..) )

class Wombat a

ifThenElse :: Wombat a => Bool -> a -> a -> a
ifThenElse _ ok _ = ok

foo :: ()
foo = if True then () else ()
