{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -dcore-lint -O #-}
module T8196 where

data T a b = MkT (a b)
   deriving Show

foo = print (MkT (Just True))
