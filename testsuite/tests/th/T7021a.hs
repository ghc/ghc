{-# LANGUAGE ConstraintKinds, TemplateHaskell, PolyKinds, TypeFamilies #-}

module T7021a where

import GHC.Prim
import Language.Haskell.TH

type IOable a = (Show a, Read a)
type family ALittleSilly :: Constraint

data Proxy a = Proxy

foo :: IOable a => a
foo = undefined

baz :: a b => Proxy a -> b
baz = undefined

bar :: ALittleSilly  => a
bar = undefined

test :: Q Exp
test = do
    Just fooName <- lookupValueName "foo"
    Just bazName <- lookupValueName "baz"
    Just barName <- lookupValueName "bar"
    reify fooName
    reify bazName
    reify barName
    [t| (Show a, (Read a, Num a)) => a -> a |]
    [| \_ -> 0 |]
