{-# LANGUAGE TypeData, MagicHash #-}
module B where

import GHC.Exts

type data T a b where
  MkT :: T a a

f :: T Int Bool -> Char
f x = case dataToTag# x of
         0# -> 'a'
         _  -> 'b'
