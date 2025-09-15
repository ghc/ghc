{-# LANGUAGE TypeFamilies #-}
module T18172 where

import Data.Word
import GHC.Exts

data Wombat = Wombat [Word8]
    deriving Show

instance IsList Wombat where
    type Item Wombat = Word8
    fromList xs = Wombat xs
    toList (Wombat xs)= xs
