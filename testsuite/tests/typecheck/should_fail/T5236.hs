{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts  #-}

module T5236 where

data A
data B

class Id a b | a -> b, b -> a

instance Id A A
instance Id B B

loop :: Id A B => Bool
loop = True

-- f :: Bool
-- f = loop
