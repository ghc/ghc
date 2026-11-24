{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))

data Proof (index :: Bool) where
    Base :: Proof 'False

data Iso a b where
    MkIso :: foreach (encode :: a -> b) -> foreach (decode :: b -> a)
          -> (foreach (x :: a) -> decode (encode x) :~: x)
          -> (foreach (y :: b) -> encode (decode y) :~: y)
          -> Iso a b

-- CorePrep must preserve the index of the call to the lexical encoder.
check :: Iso (Proof 'False) Bool -> Bool
check (MkIso encode _ fromTo toFrom) = case fromTo Base of
    Refl -> case toFrom (encode Base) of Refl -> True

data Tag (b :: Bool) where
    Tag :: Tag b

{-# NOINLINE tag #-}
tag :: foreach (b :: Bool) -> Tag b
tag _ = Tag

#ifdef WRONG_INDEX
wrong :: foreach (encode :: Proof 'False -> Bool) -> Tag 'True
wrong encode = tag (encode Base)
#endif

{-# NOINLINE checkLazy #-}
checkLazy :: (Proof 'False -> Bool) -> Bool
checkLazy encode = case tag (encode Base) of
    Tag -> True

main :: IO ()
main = print (checkLazy (\_ -> error "forced the retained encoder"))
