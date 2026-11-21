{-# LANGUAGE GHC2024, NoExperimentalDependentTypes #-}
module Main where

import Data.Type.Equality
import Provider

readBack :: Stored a b -> (a :~: Bool, b :~: Char, String)
readBack (Stored True n) = (Refl, Refl, show n)
readBack (Stored False c) = (Refl, Refl, [c])

ignorePayload :: Stored a b -> (a :~: Bool, b :~: Char)
ignorePayload (Stored _ _) = (Refl, Refl)

lazyStored :: Bool -> Stored Bool Char
lazyStored flag = Stored flag undefined

main :: IO ()
main = case (readBack (Stored True 71 :: Stored Bool Char),
             readBack (Stored False 'r' :: Stored Bool Char),
             ignorePayload (lazyStored undefined)) of
  ((Refl, Refl, a), (Refl, Refl, b), (Refl, Refl)) -> print (a, b)
