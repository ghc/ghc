{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving #-}

module Foreign.Ptr () where

import GHC.Show (Show(..))
import GHC.Word (Word)
import GHC.Base (Num)
import qualified GHC.Internal.Base as Rebindable

newtype WordPtr = WordPtr Word
    deriving Num
