{-# LANGUAGE MultiParamTypeClasses      #-}

module Basement.Alg.Class
    ( Indexable, index
    , RandomAccess, read, write
    ) where

import           Basement.Types.OffsetSize

class Indexable container ty where
    index :: container -> (Offset ty) -> ty

class RandomAccess container prim ty where
    read  :: container -> (Offset ty)       -> prim ty
    write :: container -> (Offset ty) -> ty -> prim ()
