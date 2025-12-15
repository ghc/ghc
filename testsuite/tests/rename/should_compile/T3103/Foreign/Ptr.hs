{-# LANGUAGE NoImplicitPrelude #-}

module Foreign.Ptr () where

import GHC.Show (Show(..))
import GHC.Word (Word)
import GHC.Base (Num)

newtype WordPtr = WordPtr Word
    deriving Num
