{-# LANGUAGE ExperimentalDependentTypes, RequiredTypeArguments, TypeApplications #-}
module Main where
import Data.Kind (Type)
import GHC.Exists

data Payload a = Payload a (a -> Int)
type Hidden = exists (a :: Type) | Payload a

hide :: Int -> Hidden
hide x = Pack (type Int) (Payload x id)

loop :: Int -> Maybe a -> (a -> Int) -> Int
loop 0 Nothing _ = 0
loop 0 (Just a) k = k a
loop n Nothing k = loop (n - 1) Nothing k
loop n (Just a) k = loop (n - 1) (Just a) k

readN :: Hidden -> Int -> Int
readN p n = case p of
  Pack (type t) (Payload x k) -> loop @t n (Just x) k


main :: IO ()
main = print (readN (hide 11) 8)
