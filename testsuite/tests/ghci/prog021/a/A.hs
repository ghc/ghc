module A where

import B
import Data.Char

main :: IO ()
main = do
  let cl = newCharList "ABCDEF"
  let il = newIntList [1..5]
  print $ increment il
  print $ listOf ord cl

newCharList :: String -> CharList
newCharList = CharList

newtype CharList = CharList [Char]

instance ListLike CharList where
  type Elem CharList = Char
  listOf f (CharList xs) = map f xs
