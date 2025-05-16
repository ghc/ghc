{-# LANGUAGE PatternSynonyms, ExplicitNamespaces, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
module T11959Lib (Vec2(Nil,(:>)), data (:>)) where

newtype Vec2 a = Vec2 {unvec2 :: [a]}

pattern Nil :: Vec2 a
pattern Nil = Vec2 []

pattern (:>) x xs <- ((\ys -> (head $ unvec2 ys,Vec2 . tail $ unvec2 ys)) -> (x,xs))
  where
    (:>) x xs = Vec2 (x:unvec2 xs)
