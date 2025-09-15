{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts  #-}

module T5236 where

data A
data B

class Id a b | a -> b, b -> a

instance Id A A
instance Id B B

-- The fundeps mean that this type signature
-- has a (derived) insoluble Given, A~B, but
-- we now ignore that (#12466)
loop :: Id A B => Bool
loop = True

-- f :: Bool
-- f = loop
