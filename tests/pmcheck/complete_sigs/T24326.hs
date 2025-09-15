{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module T24326 where

data family Foo
data instance Foo = A | B
{-# COMPLETE A :: Foo #-}

class C a where
  matches :: a -> Bool

pattern P :: C a => a
pattern P <- (matches -> True)

data D = D Bool
instance C D where { matches (D b) = b }

data family B a
data instance B Bool = BBool Bool
instance C (B Bool) where { matches (BBool b) = b }
{-# COMPLETE P :: B #-}

f :: Foo -> Int
f A = 0 -- should not warn

f1 :: D -> ()
f1 P = () -- should warn, because COMPLETE doesn't apply at D

f2 :: B Bool -> ()
f2 P = () -- should not warn
