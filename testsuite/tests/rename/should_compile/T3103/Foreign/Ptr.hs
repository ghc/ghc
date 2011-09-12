{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving #-}

module Foreign.Ptr () where

import GHC.Classes (Eq)
import GHC.Show (Show(..))
import GHC.Num (Num)
import GHC.Word (Word)

newtype WordPtr = WordPtr Word
    deriving (Eq,Num)

instance Show WordPtr where

