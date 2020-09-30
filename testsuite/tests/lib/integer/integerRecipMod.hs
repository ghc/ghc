{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples, TupleSections #-}

module Main (main) where

import Data.List (group)
import Data.Bits
import Data.Word
import Data.Maybe
import Control.Monad

import GHC.Word
import GHC.Base
import GHC.Num.Integer
import qualified GHC.Num.Integer as I

recipModInteger :: Integer -> Integer -> Maybe Integer
recipModInteger x m = case I.integerRecipMod# x m of
   (# y |    #) -> Just y
   (#   | () #) -> Nothing

main :: IO ()
main = do
   let
      f x = case recipModInteger x (2*3*11*11*17*17) of
               y -> fmap (x,) y

      g x = case recipModInteger x (-2*3*11*11*17*17) of
               y -> fmap (x,) y

   -- positive modulo
   print $ mapMaybe f [-7..71]

   -- negative modulo
   print $ mapMaybe g [-7..71]

   -- modulo == 1, -1 or 0
   print (recipModInteger 77 1)
   print (recipModInteger 77 (-1))
   print (recipModInteger 77 0)
