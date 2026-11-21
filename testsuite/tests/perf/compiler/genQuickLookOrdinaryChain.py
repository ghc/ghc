# A call with no retained index must not inspect every suffix of the chain.
depth = 480
print("""{-# LANGUAGE ImpredicativeTypes, RankNTypes #-}
module QuickLookOrdinaryChain where

choose :: Maybe a -> Maybe a
choose x = x

test :: Maybe (forall a. a -> a)""")
print("test = " + "choose (" * depth + "Just id" + ")" * depth)
