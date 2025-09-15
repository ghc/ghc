{-# LANGUAGE UnliftedDatatypes #-}
import Data.Kind
import GHC.Exts

type StrictPair :: Type -> Type -> UnliftedType
data StrictPair a b = SP !a !b

f :: StrictPair Int Int -> StrictPair Int Int -> Int -> Bool
{-# OPAQUE f #-}
f (SP x _) (SP _ y) z = x < y + z

g :: Int -> [Int] -> Int
{-# OPAQUE g #-}
g x ys = h ys
  where
    h [] = 0
    h (y:ys) = case SP x 27 of
      u -> if f u u y then x else x + h ys

main :: IO ()
main = print (g undefined [])
